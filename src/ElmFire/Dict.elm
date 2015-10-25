module ElmFire.Dict
  ( Config
  , Delta (..), subscribeDelta, update, integrate, mirror
  , Operation (..), Mode (..), operate, forwardOperation
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
  -- Operations on single elements of the dictionary
  | Insert String v
  | Push v
  | Update String (Maybe v -> Maybe v)
  | Remove String
  -- Operations on the whole dictionary
  | Empty
  | FromDict Mode (Dict String v)
  | FromList Mode (List (String, v))
--| InsertList Mode (List (String, v)) -- Todo
--| RemoveList Mode (List String) -- Todo
--| KeepList Mode (List String) -- Todo
  | Map Mode (String -> v -> v)
  | Filter Mode (String -> v -> Bool)
  | FilterMap Mode (String -> v -> Maybe v)
-- poss. more:
--  - filter out undecodable
-- poss. operations with a result (in separate operateX functions)
--  - getting as Dict or List or Pairs per once-query
--  - test/filter decodability

type Mode
  = AllAtOnce
  | Sequential
  | Parallel -- will be executed asynchronously (restriction due to Task module)

unitize : Task x a -> Task x ()
unitize = Task.map (always ())

operate : Config v -> Operation v -> Task Error ()
operate config operation =
  let
    -- encodePairs : List (String, v) -> JD.Value
    encodePairs pairs =
      JE.object <| List.map (\(key, value) -> (key, config.encoder value)) pairs
    getKeys : Task Error (List String)
    getKeys =
      ElmFire.once
        (ElmFire.valueChanged ElmFire.noOrder)
        config.location
      |> Task.map ElmFire.toKeyList
    disposeTaskList : Mode -> List (Task x a) -> Task x ()
    disposeTaskList mode taskList =
      if mode == Parallel
      then Task.sequence (List.map Task.spawn taskList) |> unitize
      else Task.sequence taskList |> unitize
  in
    case operation of
      None ->
        ElmFire.open config.location
        |> unitize
      Insert key value ->
        ElmFire.set (config.encoder value) (ElmFire.sub key config.location)
        |> unitize
      Push value ->
        ElmFire.set (config.encoder value) (ElmFire.push config.location)
        |> unitize
      Update key alter ->
        ElmFire.transaction
          (operateUpdateFun config alter)
          (config.location |> ElmFire.sub key)
          True
        |> unitize
      Remove key ->
        ElmFire.remove (ElmFire.sub key config.location)
        |> unitize
      Empty ->
        ElmFire.remove config.location
        |> unitize
      FromDict mode dict ->
        operate config (FromList mode (Dict.toList dict))
      FromList AllAtOnce pairs ->
        ElmFire.set (encodePairs pairs) config.location
        |> unitize
      FromList mode pairs ->
        ElmFire.remove config.location `andThen` \_ ->
        disposeTaskList mode
          ( List.map
              (\(key, value) ->
                ElmFire.set (config.encoder value) (ElmFire.sub key config.location)
              )
              pairs
          )
      Map AllAtOnce mapping ->
        ElmFire.transaction
          (operateMapTFun config mapping)
          config.location
          True
        |> unitize
      Map mode mapping ->
        getKeys `andThen` \keys ->
          disposeTaskList mode (List.map (operateMapElemT config mapping) keys)
      Filter AllAtOnce filter ->
        ElmFire.transaction
          (operateFilterTFun config filter)
          config.location
          True
        |> unitize
      Filter mode filter ->
        getKeys `andThen` \keys ->
          disposeTaskList mode (List.map (operateFilterElemT config filter) keys)
      FilterMap AllAtOnce filterMapping ->
        ElmFire.transaction
          (operateFilterMapTFun config filterMapping)
          config.location
          True
        |> unitize
      FilterMap mode filterMapping ->
        getKeys `andThen` \keys ->
          disposeTaskList mode (List.map (operateFilterMapElemT config filterMapping) keys)

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

operateMapElemT : Config v -> (String -> v -> v) -> String -> Task Error (Bool, ElmFire.Snapshot)
operateMapElemT config mapping key =
  ElmFire.transaction
    (\maybeJsonVal -> case maybeJsonVal of
      Nothing -> ElmFire.Abort
      Just jsonVal -> case JD.decodeValue config.decoder jsonVal of
        Err _ -> ElmFire.Abort
        Ok val -> ElmFire.Set (config.encoder (mapping key val))
    )
    (ElmFire.sub key config.location)
    True

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

operateFilterElemT : Config v -> (String -> v -> Bool) -> String -> Task Error (Bool, ElmFire.Snapshot)
operateFilterElemT config filter key =
  ElmFire.transaction
    (\maybeJsonVal -> case maybeJsonVal of
      Nothing -> ElmFire.Abort
      Just jsonVal -> case JD.decodeValue config.decoder jsonVal of
        Err _ -> ElmFire.Abort
        Ok val ->
          if filter key val
          then ElmFire.Abort
          else ElmFire.Remove
    )
    (ElmFire.sub key config.location)
    True

operateFilterMapTFun : Config v -> (String -> v -> Maybe v) -> Maybe JE.Value -> ElmFire.Action
operateFilterMapTFun config filterMapping maybeJsonDict =
  case maybeJsonDict of
    Nothing -> ElmFire.Abort
    Just jsonDict ->
      case JD.decodeValue (JD.keyValuePairs JD.value) jsonDict of
        Err _ -> ElmFire.Abort
        Ok pairs ->
          let
            newPairs : List (String, JE.Value)
            newPairs = List.filterMap
              (\(key, jsonVal) ->
                case JD.decodeValue config.decoder jsonVal of
                  Err _ -> Just (key, jsonVal)
                  Ok val ->
                    case filterMapping key val of
                      Nothing -> Nothing
                      Just newVal -> Just (key, config.encoder newVal)
              )
              pairs
          in
            ElmFire.Set <| JE.object newPairs

operateFilterMapElemT : Config v -> (String -> v -> Maybe v) -> String -> Task Error (Bool, ElmFire.Snapshot)
operateFilterMapElemT config filterMapping key =
  ElmFire.transaction
    (\maybeJsonVal -> case maybeJsonVal of
      Nothing -> ElmFire.Abort
      Just jsonVal -> case JD.decodeValue config.decoder jsonVal of
        Err _ -> ElmFire.Abort
        Ok val -> case filterMapping key val of
          Nothing -> ElmFire.Remove
          Just newVal -> ElmFire.Set (config.encoder newVal)
    )
    (ElmFire.sub key config.location)
    True

forwardOperation : Address (Task Error ()) -> Config v
                -> Address (Operation v)
forwardOperation taskAddressee config =
  Signal.forwardTo taskAddressee (operate config)
