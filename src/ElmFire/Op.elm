module ElmFire.Op
  ( Config, Operation, Dispatch
  , none, insert, push, update, remove
  , empty, fromDict, fromList, insertList, removeList
  , map, filter, filterMap
  , atomic, sequential, parallel
  , operate, forwardOperation
  ) where


{-| High-level interface for operations on a Firebase collection.

The Firebase collection at a given location is treated like a key-value store.

Keys are of type String. The value type is a configurable type `v`.
A mapping between Elm values and the Firebase's JSON values has to be provided.

This module is accompanied by `ElmFire.Dict` for querying and mirroring the key-value store.

# Configuration
@docs Config

# Performing operations
@docs Operation, operate, forwardOperation

# Initializing the key-value store
@docs empty, fromDict, fromList

# Modifying single values
@docs insert, push, update, remove

# Dispatch modes
Operations that touch the whole collection can be executed in different dispatch modes.
@docs Dispatch, atomic, sequential, parallel

# Inserting and deleting lists of key-value pairs
@docs insertList, removeList

# Updating the whole collection with higher-order functions
@docs map, filter, filterMap

# Miscellaneous operations
@docs none
-}

import Signal exposing (Mailbox, Address, mailbox, send)
import Task exposing (Task, andThen)
import Dict exposing (Dict)
import ElmFire exposing (Reference, Error)
import ElmFire.Dict
import Json.Decode as JD
import Json.Encode as JE
import Debug


{-| The target of all operations is described in a configuration record:

- `location`: Pointer to the Firebase path (e.g. `ElmFire.fromUrl "https://.../..."`).
- `orderOptions`: Filter and limit elements according to a given order. Use `ElmFire.noOrder` if not needed.
- `encoder`: Function to convert Elm values into JSON values according to the Firebase's schema.
- `decoder`: A Json.Decoder used to convert a JSON value from the Firebase into the corresponding Elm value.

Decoding errors are silently ignored in all operations (if they need the decoder at all).
-}
type alias Config v =
  { location: ElmFire.Location
  , orderOptions: ElmFire.OrderOptions
  , encoder: v -> JD.Value
  , decoder: JD.Decoder v
  }

{-| Opaque type representing an operation -}
type Operation v
  = None
  -- Operations on single elements of the collection
  | Insert String v
  | Push v
  | Update String (Maybe v -> Maybe v)
  | Remove String
  -- Operations on the whole collection
  | Empty
  | FromDict Dispatch (Dict String v)
  | FromList Dispatch (List (String, v))
  | InsertList Dispatch (List (String, v))
  | RemoveList Dispatch (List String)
  | Map Dispatch (String -> v -> v)
  | Filter Dispatch (String -> v -> Bool)
  | FilterMap Dispatch (String -> v -> Maybe v)

{-| Opaque type representing a dispatch mode -}
type Dispatch
  = Atomic
  | Sequential
  | Parallel -- will be executed asynchronously (restriction due to Task module)

------------------------------------------------------------------------------

{-| Do nothing. -}
none : Operation v
none = None

{-| Insert a single key-value pair, possible overwriting the previous value. -}
insert : String -> v -> Operation v
insert = Insert

{-| Insert a single key-value pair. This generates a new child location using a unique key. -}
push : v -> Operation v
push = Push

{-| Update a single value for a specific key with a given function. -}
update : String -> (Maybe v -> Maybe v) -> Operation v
update = Update

{-| Remove the element with the given key. -}
remove : String -> Operation v
remove = Remove

{-| Dlete the whole collection. -}
empty : Operation v
empty = Empty

{-| Initialize the collection from a dictionary. -}
fromDict : Dispatch -> (Dict String v) -> Operation v
fromDict = FromDict

{-| Initialize the collection from a list of key-value pairs. -}
fromList : Dispatch -> (List (String, v)) -> Operation v
fromList = FromList

{-| Insert a list of key-value pairs. -}
insertList : Dispatch -> (List (String, v)) -> Operation v
insertList = InsertList

{-| Remove selected elements by a list of keys. -}
removeList : Dispatch -> (List String) -> Operation v
removeList = RemoveList

{-| Apply a function to modify all values. -}
map : Dispatch -> (String -> v -> v) -> Operation v
map = Map

{-| Remove those elements that don't satisfy a predicate. -}
filter : Dispatch -> (String -> v -> Bool) -> Operation v
filter = Filter

{-| Apply a function to modify or remove values. -}
filterMap : Dispatch -> (String -> v -> Maybe v) -> Operation v
filterMap = FilterMap

{-| Perform the whole operation as a single transaction. -}
atomic : Dispatch
atomic = Atomic

{-| Perform the operation step-by-step for each affected element in a sequential manner. -}
sequential : Dispatch
sequential = Sequential

{-| Perform the operation for each affected element in parallel.
Will be executed asynchronously (due to a Task module restriction).
-}
parallel : Dispatch
parallel = Parallel

------------------------------------------------------------------------------

{-| Task to perform an operation with a given configuration. -}
operate : Config v -> Operation v -> Task Error Reference
operate config operation =
  let
    -- encodePairs : List (String, v) -> JD.Value
    encodePairs pairs =
      JE.object <| List.map (\(key, value) -> (key, config.encoder value)) pairs

    getKeys : Task Error (List String)
    getKeys = ElmFire.Dict.getKeys config

    dispatchTaskList : Dispatch -> List (Task Error a) -> Task Error Reference
    dispatchTaskList dispatch taskList =
      if dispatch == Parallel
      then Task.sequence (List.map Task.spawn taskList)
           `andThen` \_ -> ElmFire.open config.location
      else Task.sequence taskList
           `andThen` \_ -> ElmFire.open config.location

    transactionOp : (Maybe JE.Value -> ElmFire.Action) -> Task Error Reference
    transactionOp transactFun =
      ElmFire.transaction transactFun config.location True
      |> Task.map (snd >> .reference)

  in
    case operation of
      None ->
        ElmFire.open config.location
      Insert key value ->
        ElmFire.set (config.encoder value) (ElmFire.sub key config.location)
      Push value ->
        ElmFire.set (config.encoder value) (ElmFire.push config.location)
      Update key alter ->
        ElmFire.transaction
          (operateUpdateFun config alter)
          (config.location |> ElmFire.sub key)
          True
        |> Task.map (snd >> .reference)
      Remove key ->
        ElmFire.remove (ElmFire.sub key config.location)
      Empty ->
        ElmFire.remove config.location
      FromDict dispatch dict ->
        operate config (FromList dispatch (Dict.toList dict))
      FromList Atomic pairs ->
        ElmFire.set (encodePairs pairs) config.location
      FromList dispatch pairs ->
        ElmFire.remove config.location `andThen` \_ ->
        operate config (InsertList dispatch pairs)
      InsertList Atomic pairs ->
        ElmFire.update (encodePairs pairs) config.location
      InsertList dispatch pairs ->
        dispatchTaskList dispatch
          ( List.map
              (\(key, value) ->
                ElmFire.set (config.encoder value) (ElmFire.sub key config.location)
              )
              pairs
          )
      RemoveList Atomic keys ->
        ElmFire.update
          (JE.object <| List.map (\key -> (key, JE.null)) keys)
          config.location
      RemoveList dispatch keys ->
        dispatchTaskList dispatch
          (List.map (\key -> ElmFire.remove (ElmFire.sub key config.location)) keys)
      Map Atomic mapping ->
        transactionOp (operateMapTFun config mapping)
      Map dispatch mapping ->
        getKeys `andThen` \keys ->
          dispatchTaskList dispatch (List.map (operateMapElemT config mapping) keys)
      Filter Atomic filter ->
        transactionOp (operateFilterTFun config filter)
      Filter dispatch filter ->
        getKeys `andThen` \keys ->
          dispatchTaskList dispatch (List.map (operateFilterElemT config filter) keys)
      FilterMap Atomic filterMapping ->
        transactionOp (operateFilterMapTFun config filterMapping)
      FilterMap dispatch filterMapping ->
        getKeys `andThen` \keys ->
          dispatchTaskList dispatch (List.map (operateFilterMapElemT config filterMapping) keys)

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

{-| Creates a new address for receiving operations with a fixed configuration. -}
forwardOperation : Address (Task Error Reference) -> Config v
                -> Address (Operation v)
forwardOperation taskAddressee config =
  Signal.forwardTo taskAddressee (operate config)
