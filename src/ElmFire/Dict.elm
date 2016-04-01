module ElmFire.Dict
  ( Config, Delta (..)
  , getDict, getList, getKeys, getValues
  , subscribeDelta, update, integrate, mirror
  ) where


{-| Tasks to mirror a Firebase location in an Elm dictionary, either one-time or continuously.

The Firebase collection at a given location is treated like a key-value store, which is mapped into a `Dict String v`.

Keys are of type String. Values get converted from JSON to a user-defined type `v`.

This module is accompanied by `ElmFire.Op` for modifying the key-value store.

# Configuration
@docs Config

# One-time queries
@docs getDict

The lists returned by the following query tasks are ordered in respect to the configured `orderOptions`.

@docs getList, getKeys, getValues

# Subscribing to continuous mirroring
@docs Delta, subscribeDelta, update, integrate, mirror
-}

import Signal exposing (Mailbox, Address, mailbox, send)
import Task exposing (Task, andThen)
import Dict exposing (Dict)
import ElmFire exposing (Reference, Error)
import Json.Decode as JD
import Json.Encode as JE
import Debug

{- Notes

  - Poss. maintain a `Maybe (Dict (String v))` to deal with undefined states
  - Delta with undecodable but changed value should remove the key from the dict
  - Poss. emit a `Loaded` event
    (rf. to AngularFire:
     https://www.firebase.com/docs/web/libraries/angular/api.html#angularfire-firebaseobject-loaded)
  - for throttling the output Signal:
      - see Apanatshka/elm-signal-extra
      - poss. new throttle function that updates at least after every given time-period
-}

------------------------------------------------------------------------------


{-| Each query target is described by a configuration:

- `location`: Pointer to the Firebase path, e.g. `ElmFire.fromUrl "https://.../..."`.
- `orderOptions`: Filter and limit elements according to a given order. Use `ElmFire.noOrder` if not needed.
- `encoder`: Function to convert Elm values into JSON values according to the Firebase's schema.
  `encoder` is used only in module `ElmFire.Op`, but is present here because the type `Config` is shared between these two modules.
- `decoder`: A Json.Decoder used to convert a JSON value from the Firebase into the corresponding Elm value.

Decoding errors are reported as the special `Delta` value `Undecodable String String`.
They are silently ignored in all other functions of this module.
-}
type alias Config v =
  { location: ElmFire.Location
  , orderOptions: ElmFire.OrderOptions
  , encoder: v -> JD.Value
  , decoder: JD.Decoder v
  }

{-| Represents a single update reported from the Firebase. -}
type Delta v
  = Idem
  | Added String v
  | Changed String v
  | Removed String v
  | Undecodable String String
  | Unsubscribed
  | QueryError Error

------------------------------------------------------------------------------

{-| One-time query, mapping the Firebase store to a dictionary -}
getDict : Config v -> Task Error (Dict String v)
getDict config =
  getList config |> Task.map Dict.fromList

{-| One-time query, mapping the Firebase store to a list of key-value pairs -}
getList : Config v -> Task Error (List (String, v))
getList config =
  let
    decodePairList =
      List.filterMap
        ( \(key, jsonVal) ->
            case JD.decodeValue config.decoder jsonVal of
              Err _ -> Nothing
              Ok val -> Just (key, val)
        )
  in
    ElmFire.once (ElmFire.valueChanged config.orderOptions) config.location
    |> Task.map (ElmFire.toPairList >> decodePairList)

{-| One-time query, resulting in a list of keys (without values) -}
getKeys : Config v -> Task Error (List String)
getKeys config =
  ElmFire.once (ElmFire.valueChanged config.orderOptions) config.location
  |> Task.map ElmFire.toKeyList

{-| One-time query, resulting in a list of values (without keys) -}
getValues : Config v -> Task Error (List v)
getValues config =
  let
    decodeValueList =
      List.filterMap (JD.decodeValue config.decoder >> Result.toMaybe)
  in
    ElmFire.once (ElmFire.valueChanged config.orderOptions) config.location
    |> Task.map (ElmFire.toValueList >> decodeValueList)

{-| Get the initial state from the Firebase and subscribe to subsequent updates.
The resulting deltas are sent to a given mailbox.

The resulting task needs to be executed in order to initiate the subscription.

Its success-result is a task that may be executed later to unsubscribe again.
-}
subscribeDelta : Address (Delta v) ->  Config v -> Task Error (Task Error ())
subscribeDelta addressee config =
  let
    subscribeEvent event deltaOp =
      ElmFire.subscribe
        (\snapshot -> send addressee
          ( case JD.decodeValue config.decoder snapshot.value of
              Ok val -> deltaOp snapshot.key val
              Err description -> Undecodable snapshot.key description
          )
        )
        (\cancellation -> send addressee
          ( case cancellation of
              ElmFire.Unsubscribed _ -> Unsubscribed
              ElmFire.QueryError _ error -> QueryError error
          )
        )
        event
        config.location
  in
    subscribeEvent (ElmFire.childRemoved config.orderOptions) Removed
    `andThen` \s1 -> subscribeEvent (ElmFire.childChanged config.orderOptions) Changed
    `andThen` \s2 -> subscribeEvent (ElmFire.childAdded config.orderOptions) Added
    `andThen` \s3 ->
      Task.succeed
      ( ElmFire.unsubscribe s1
       `andThen` \_ -> ElmFire.unsubscribe s2
       `andThen` \_ -> ElmFire.unsubscribe s3
      )

{-| Update a dictionary by applying a single given delta. -}
update : Delta v -> Dict String v -> Dict String v
update delta dict =
  case delta of
    Idem -> dict
    Added key value -> Dict.insert key value dict
    Changed key value -> Dict.insert key value dict
    Removed key _ -> Dict.remove key dict
    Undecodable _ _ -> dict
    Unsubscribed -> dict
    QueryError _ -> dict

{-| Integrate a signal of deltas, returning a signal of dictionaries. -}
integrate : Signal (Delta v) -> Signal (Dict String v)
integrate deltas =
  Signal.foldp update Dict.empty deltas

{-| Convenience function that combines subscribing to and integrating of deltas.
It returns a 2-tuple:

- First element is a task to subscribe to the Firebase updates. It needs to be executed to initiate
  the subscritpion. The task's result is another task that can be uses for unsubscribing.
- Second element is a signal of the dictionary that consecutively mirrors the Firebase collection.
-}
mirror : Config v -> (Task Error (Task Error ()), Signal (Dict String v))
mirror config =
  let
    deltas : Mailbox (Delta v)
    deltas = mailbox Idem
    init = subscribeDelta deltas.address config
    sum = integrate deltas.signal
  in
    (init, sum)
