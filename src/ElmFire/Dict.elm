module ElmFire.Dict
  ( Config, Delta (..)
  , getDict, getList, getKeys, getValues
  , subscribeDelta, update, integrate, mirror
  ) where


{-| ...

...

# ...
@docs mirror
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
  - for throttling the output Signal:
      - see Apanatshka/elm-signal-extra
      - poss. new throttle function that updates at least after every given time-period
-}

------------------------------------------------------------------------------

type alias Config v =
  { location: ElmFire.Location
  , orderOptions: ElmFire.OrderOptions
  , encoder: v -> JD.Value
  , decoder: JD.Decoder v
  }

type Delta v
  = Idem
  | Added String v
  | Changed String v
  | Removed String v
  | Undecodable String String
  | Unsubscribed
  | QueryError Error

------------------------------------------------------------------------------

getDict : Config v -> Task Error (Dict String v)
getDict config =
  getList config |> Task.map Dict.fromList

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

getKeys : Config v -> Task Error (List String)
getKeys config =
  ElmFire.once (ElmFire.valueChanged config.orderOptions) config.location
  |> Task.map ElmFire.toKeyList

getValues : Config v -> Task Error (List v)
getValues config =
  let
    decodeValueList =
      List.filterMap (JD.decodeValue config.decoder >> Result.toMaybe)
  in
    ElmFire.once (ElmFire.valueChanged config.orderOptions) config.location
    |> Task.map (ElmFire.toValueList >> decodeValueList)

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

integrate : Signal (Delta v) -> Signal (Dict String v)
integrate deltas =
  Signal.foldp update Dict.empty deltas

mirror : Config v -> (Task Error (Task Error ()), Signal (Dict String v))
mirror config =
  let
    deltas : Mailbox (Delta v)
    deltas = mailbox Idem
    init = subscribeDelta deltas.address config
    sum = integrate deltas.signal
  in
    (init, sum)
