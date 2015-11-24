{- A ElmFire Demo App for Dict Syncing
-}


import ElmFire exposing (Error, Reference)
import ElmFire.Dict as EFD
import ElmFire.Op as EFO

import Dict exposing (Dict)
import Json.Encode as JE
import Json.Decode as JD
import Task exposing (Task, andThen, succeed, onError)
import Signal exposing (Mailbox, Address, mailbox, send)
import Time
import Html exposing (Html, text, ol, li, i, div, p)

import Debug

-------------------------------------------------------------------------------

url : String
url = "https://elmfiretest.firebaseio.com/dict"

config : EFD.Config Int
config =
  { location = ElmFire.fromUrl url
  -- , orderOptions = ElmFire.noOrder
  , orderOptions = ElmFire.orderByValue ElmFire.noRange ElmFire.noLimit
  , encoder = JE.int
  , decoder = JD.int
  }

-------------------------------------------------------------------------------

type alias State = Dict String Int
type alias Delta = EFD.Delta Int

changes : Mailbox Delta
changes = mailbox EFD.Idem

initTask : Task Error ()
initTask =
  EFD.subscribeDelta
    changes.address
    config
  -- Unsubscribe after some time
  `andThen` \unsubTask ->
    Task.sleep 30000
  `andThen` \_ ->
    unsubTask

states : Signal State
states =
  EFD.integrate changes.signal

port initSyncing : Task Error ()
port initSyncing = initTask

type alias History = List Delta

histories : Signal History
histories = Signal.foldp (::) [] changes.signal

main : Signal Html
main = Signal.map2 view states histories

view : State -> History -> Html
view dict history =
  div []
    [ p [] [text "State:"]
    , viewState dict
    , p [] [text "History:"]
    , viewHistory history
    ]

viewState : State -> Html
viewState dict =
  ol [] ( Dict.foldr
      (\key val list ->
            li [] (viewItem key (toString val))
         :: list
      )
      []
      dict
    )

viewHistory : History -> Html
viewHistory history =
  ol [] ( List.foldl
    (\delta list ->
          li [] (viewDelta delta)
       :: list
    )
    []
    history
  )

viewDelta : Delta -> List Html
viewDelta delta =
  case delta of
    EFD.Idem -> [text "idem"]
    EFD.Added key val -> text "added " :: viewItem key (toString val)
    EFD.Changed key val -> text "changed " :: viewItem key (toString val)
    EFD.Removed key val -> text "removed " :: viewItem key (toString val)
    EFD.Undecodable key descr -> text "undecodable " :: viewItem key descr
    EFD.Unsubscribed -> [text "unsubscribed"]
    EFD.QueryError error -> [text <| "query error: " ++ toString error]

viewItem : String -> String -> List Html
viewItem key val =
  [ i [] [text key]
  , text ": "
  , text val
  ]

-----------------------------------------------------------------------

gatherOperationTasks : Mailbox (Task Error ())
gatherOperationTasks = mailbox (succeed ())

port runOperationTasks : Signal (Task Error ())
port runOperationTasks = gatherOperationTasks.signal

operationAddressee : Address (EFO.Operation Int)
operationAddressee =
  EFO.forwardOperation
    ( Signal.forwardTo
        gatherOperationTasks.address
        (Task.map (always ()))
    )
    config

infixl 1 =>
(=>) : Task x a -> Task x b -> Task x b
(=>) taskL taskR =
  taskL `andThen` \_ -> taskR

port testOperations : Task () ()
port testOperations =
  let
    op : EFO.Operation Int -> Task y ()
    op operation =
      Task.sleep (1 * Time.second) => send operationAddressee operation
    increment : Maybe Int -> Maybe Int
    increment mN =
      case mN of
        Nothing -> Just 0
        Just n -> if n > 3 then Nothing else Just (n + 1)
  in
      op (EFO.none)
   => op (EFO.empty)
   => op (EFO.fromList EFO.sequential [("b", 4), ("c", 5)])
   => op (EFO.fromDict EFO.sequential <| Dict.fromList [("d", 6), ("e", 7), ("f", 8)])
   => op (EFO.push 1)
   => op (EFO.insert "a" 2)
   => op (EFO.insert "a" 3)
   => op (EFO.insert "g" 33)
   => op (EFO.remove "d")
   => op (EFO.update "a" increment)
   => op (EFO.update "a" increment)
   => op (EFO.update "a" increment)
   => op (EFO.update "a" increment)
   => op (EFO.map EFO.parallel (\k n -> n + 1))
   => op (EFO.map EFO.atomic (\k n -> n * 2))
   => op (EFO.map EFO.sequential (\k n -> n - 1))
   => op (EFO.filter EFO.atomic (\k n -> n < 20))
   => op (EFO.insertList EFO.atomic [("b", 4), ("c", 5)])
   => op (EFO.insertList EFO.parallel [("x", 10), ("y", 11), ("z", 12)])
   => op (EFO.filterMap EFO.atomic (\k n -> if k /= "y" then Just (n + 100) else Nothing))
   => op (EFO.filterMap EFO.sequential (\k n -> if k /= "z" then Just (n - 100) else Nothing))
   => op (EFO.removeList EFO.sequential ["c", "e", "a"])

port runGetDictTest : Task Error (Dict String Int, List (String, Int), List String, List Int)
port runGetDictTest =
  EFD.getDict   config `andThen` \dict ->
  EFD.getList   config `andThen` \list ->
  EFD.getKeys   config `andThen` \keys ->
  EFD.getValues config `andThen` \values ->
  succeed <|
    ( Debug.log "getDict" dict
    , Debug.log "getList" list
    , Debug.log "getKeys" keys
    , Debug.log "getValues" values
    )
