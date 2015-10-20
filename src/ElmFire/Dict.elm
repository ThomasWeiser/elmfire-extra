module ElmFire.Dict
  ( Config
  , Delta (..), subscribeDelta, update, integrate, mirror
  , Operation (..), operate, forwardOperation
  ) where


{-| ...

...

# ...
@docs mirror
-}

import Signal exposing (Mailbox, Address, mailbox, send)
import Task exposing (Task, andThen)
import Dict exposing (Dict)
import ElmFire exposing (Error)
import Json.Decode as JD
import Json.Encode as JE
import Debug

{- Notes

  - Poss. split module into subscription part and operation part (independently useful)
      - Notice that the Operations are not specific to a Dict-like interpretation.
      - Probably an extra part for Config
  - Provide functions for the Operations (e.g. insert = Insert). Export those functions.
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

------------------------------------------------------------------------------

type Delta v
  = Idem
  | Added String v
  | Changed String v
  | Removed String v
  | Undecodable String String
  | Unsubscribed
  | QueryError Error

subscribeDelta : Address (Delta v) ->  Config v ->  Task Error (Task Error ())
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

------------------------------------------------------------------------------

type Operation v
  = None
  | Empty
  | FromDict (Dict String v)
  | FromList (List (String, v))
  | Insert String v
  | Push v
  | Remove String
  | Update String (Maybe v -> Maybe v)
--| Map (String -> v -> v) -- One transaction for each mapped element -- TODO
  | MapT (String -> v -> v) -- One transaction for the whole mapping
--| Filter (String -> v -> Bool) -- TODO
  | FilterT (String -> v -> Bool) -- TODO
--| FilterMap (String -> v -> Maybe v) -- TODO
--| FilterMapT (String -> v -> Maybe v) -- TODO
-- poss. more:
--  - union, intersect, diff (?)
--  - list operations
--  - test/filter decodability
--  - getting as Dict or List or Pairs per once-query
--  - etc.

operate : Config v -> Operation v -> Task Error ()
operate config operation =
  let
    encodePairs pairs = JE.object <| List.map (\(k, v) -> (k, config.encoder v)) pairs
  in
    ( case operation of
        None
         -> ElmFire.open config.location
        Empty
         -> ElmFire.remove config.location
        FromDict dict
         -> ElmFire.set (encodePairs <| Dict.toList dict) config.location
        FromList pairs
         -> ElmFire.set (encodePairs pairs) config.location
        Insert key value
         -> ElmFire.set (config.encoder value) (ElmFire.sub key config.location)
        Push value
         -> ElmFire.set (config.encoder value) (ElmFire.push config.location)
        Remove key
         -> ElmFire.remove (ElmFire.sub key config.location)
        Update key alter
         -> ElmFire.transaction
              (operateUpdateFun config alter)
              (config.location |> ElmFire.sub key)
              True
            |> Task.map (snd >> .reference)
        MapT mapping
         -> ElmFire.transaction
              (operateMapTFun config mapping)
              config.location
              True
            |> Task.map (snd >> .reference)
        FilterT filter
         -> ElmFire.transaction
              (operateFilterTFun config filter)
              config.location
              True
            |> Task.map (snd >> .reference)
    )
    |> Task.map (always ())

operateUpdateFun : Config v -> (Maybe v -> Maybe v) -> Maybe JE.Value -> ElmFire.Action
operateUpdateFun config alter maybeJsonVal =
  case maybeJsonVal of
    Nothing -> case alter Nothing of
      Nothing -> ElmFire.Remove
      Just newVal -> ElmFire.Set (config.encoder newVal)
    Just jsonVal -> case JD.decodeValue config.decoder jsonVal of
      Err _ -> ElmFire.Abort
      Ok val ->
        case alter (Just val) of
          Nothing -> ElmFire.Remove
          Just newVal -> ElmFire.Set (config.encoder newVal)

operateMapTFun : Config v -> (String -> v -> v) -> Maybe JE.Value -> ElmFire.Action
operateMapTFun config mapping maybeJsonDict =
  case maybeJsonDict of
    Nothing -> ElmFire.Abort
    Just jsonDict ->
      case JD.decodeValue (JD.keyValuePairs JD.value) jsonDict of
        Err _ -> ElmFire.Abort
        Ok pairs ->
          let
            newPairs : List (String, JE.Value)
            newPairs = List.map
              (\(key, jsonVal) ->
                ( key
                , case JD.decodeValue config.decoder jsonVal of
                    Err _ -> jsonVal
                    Ok val -> config.encoder (mapping key val)
                )
              )
              pairs
          in
            ElmFire.Set <| JE.object newPairs

operateFilterTFun : Config v -> (String -> v -> Bool) -> Maybe JE.Value -> ElmFire.Action
operateFilterTFun config filter maybeJsonDict =
  case maybeJsonDict of
    Nothing -> ElmFire.Abort
    Just jsonDict ->
      case JD.decodeValue (JD.keyValuePairs JD.value) jsonDict of
        Err _ -> ElmFire.Abort
        Ok pairs ->
          let
            newPairs : List (String, JE.Value)
            newPairs = List.filter
              (\(key, jsonVal) ->
                case JD.decodeValue config.decoder jsonVal of
                  Err _ -> False
                  Ok val -> filter key val
              )
              pairs
          in
            ElmFire.Set <| JE.object newPairs

forwardOperation : Address (Task Error ()) -> Config v
                -> Address (Operation v)
forwardOperation taskAddressee config =
  Signal.forwardTo taskAddressee (operate config)
