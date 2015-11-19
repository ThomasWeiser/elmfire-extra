# High-level API for ElmFire

## Treat your Firebase data like a local Dict

This package provides an API layer on top of the basic [ElmFire](http://package.elm-lang.org/packages/ThomasWeiser/elmfire/latest) API,
that treats a Firebase collection as a key-value store and
makes it available basically as an Elm dictionary with corresponding operations on it.

The package consists of two modules, for reading and writing respectively:

- `ElmFire.Dict`
  - Mirroring a Firebase location as an Elm `Dict`
  - Getting a signal of all updates
  - One-time retrieval


- `ElmFire.Op`
  - Inserting, updating and deleting of single key-value pairs
  - Inserting and deleting lists of key-value pairs
  - Updating the whole collection via the higher-order functions `map`, `filter` and `filterMap`
  - Operations on the whole store can selectively be run sequentially, in parallel or as a single transaction.

## General Usage Pattern

These two modules are intended to be used together.

- The state of the mirrored store will be held as a dictionary as part of the application's model.
- Operations on the store are performed by the appropriate actions of the application.

Local modifications will be reflected immediately in addition to be sent to the Firebase server and to other clients subscribed to the same Firebase location.
Likewise, remote modifications will be reflected in the local mirror.

## Configuration

All functionality of the package is guided by a configuration record that defines type mappings and other specifics of the Firebase collection.

```elm
type alias Config v =
  { location: ElmFire.Location
  , orderOptions: ElmFire.OrderOptions
  , encoder: v -> JD.Value
  , decoder: JD.Decoder v
  }

```

`location` specifies the Firebase and sub-path where the store is hosted.

`orderOptions` can be used to filter and limit the elements, that should be included in the local mirror. Use `ElmFire.noOrder` to access the whole collection.

The API is parameterized on the store's value type `v`. This can be any Elm type, as long as suitable conversion functions are provided.
Note that the keys are always of type String.

`encoder` and `decoder` are the functions used to convert between the value type in Elm code and the JSON schema in the Firebase.

## Example Code

We setup a simple store with values of type `Int`.
The signal `model` is the local mirror of the store.
`taskInit` and `taskMap` are sketches of operations on the store, that populate it and perform a mapping over it.

```elm
url = "https://myfirebase.firebaseio.com/sub/path"

config =
  { location = ElmFire.fromUrl url
  , orderOptions = ElmFire.noOrder
  , encoder = Json.Encode.int
  , decoder = Json.Decode.int
  }

-- Start mirroring
--   (run subscriptionTask via a port)
--   model : Signal (Dict String Int)
(subscriptionTask, model) = ElmFire.Dict.mirror config

-- Initialize the store  (run the tasks via a port)
taskInit : ElmFire.Op.Operation Int
taskInit =
  ElmFire.Op.fromList
    ElmFire.Op.parallel
    [("foo",1), ("bar",2)]

-- Double each value
taskMap : ElmFire.Op.Operation Int
taskMap =
  ElmFire.Op.map
    ElmFire.Op.sequential
    (\key val -> val * 2)
```

## Usage in TodoMVC

An example usage of this package is [this fork of TodoMVC](https://github.com/ThomasWeiser/todomvc-elmfire). It uses Firebase to store and share the todo items.

It utilizes `ElmFire.Dict` and `ElmFire.Op` in the aforementioned [usage pattern](#general-usage-pattern).
