{- A ElmFire Demo App for Dict Syncing
-}


import ElmFire exposing (Error)
import ElmFire.Dict as EFD

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
  , orderOptions = ElmFire.noOrder
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

operationAddressee : Address (EFD.Operation Int)
operationAddressee =
  EFD.forwardOperation
    gatherOperationTasks.address
    config

infixl 1 =>
(=>) : Task x a -> Task x b -> Task x b
(=>) taskL taskR =
  taskL `andThen` \_ -> taskR

port testOperations : Task () ()
port testOperations =
  let
    sleep = Task.sleep (1 * Time.second)
    op operation = sleep => send operationAddressee operation
  in
      op (EFD.Empty)
   => op (EFD.FromList [("b", 4), ("c", 5)])
   => op (EFD.FromDict <| Dict.fromList [("d", 6), ("e", 7)])
   => op (EFD.Push 1)
   => op (EFD.Insert "a" 2)
   => op (EFD.Insert "a" 3)
   => op (EFD.Insert "f" 33)
   => op (EFD.Remove "d")
   => op (EFD.Update "a" increment)
   => op (EFD.Update "a" increment)
   => op (EFD.Update "a" increment)
   => op (EFD.Update "a" increment)
   => op (EFD.MapT (\k n -> n * 2))
   => op (EFD.FilterT (\k n -> n < 20))

increment : Maybe Int -> Maybe Int
increment mN = case Debug.log "mN" mN of
  Nothing -> Just 0
  Just n -> if n > 3 then Nothing else Just (n + 1)
