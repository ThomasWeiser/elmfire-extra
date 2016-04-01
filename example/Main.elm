import Dict exposing (Dict)
import Task exposing (Task)
import Json.Encode
import Json.Decode
import Html exposing (Html)
import ElmFire
import ElmFire.Dict
import ElmFire.Op

url = "https://elmfiretest.firebaseio.com/dict"

config : ElmFire.Dict.Config Int
config =
  { location = ElmFire.fromUrl url
  , orderOptions = ElmFire.noOrder
  , encoder = Json.Encode.int
  , decoder = Json.Decode.int
  }

-- Start mirroring
mirror = ElmFire.Dict.mirror config

port initSubscription : Task ElmFire.Error (Task ElmFire.Error ())
port initSubscription = fst mirror

model : Signal (Dict String Int)
model = snd mirror

main : Signal Html
main = Signal.map view model

view : Dict String Int -> Html
view dict =
  Html.ul [] <|
    List.map
      (\(s, i) ->
        Html.li []
          [ Html.text (s ++ ": " ++ toString i ) ]
      )
      (Dict.toList dict)

-- Initialize the store  (run the tasks via a port)
opInit : ElmFire.Op.Operation Int
opInit =
  ElmFire.Op.fromList
    ElmFire.Op.sequential
    [("foo",1), ("bar",2)]

-- Double each value
opMap : ElmFire.Op.Operation Int
opMap =
  ElmFire.Op.map
    ElmFire.Op.sequential
    (\key val -> val * 2)

port runOperation : Task ElmFire.Error (List ElmFire.Reference)
port runOperation =
  Task.sequence <|
    List.map (ElmFire.Op.operate config) [opInit, opMap, opMap]
