# High-level API for ElmFire

This package provides an API layer on top of the basic [ElmFire](http://package.elm-lang.org/packages/ThomasWeiser/elmfire/latest) API,
that treats a Firebase collection as a key-value store and
makes it available basically as an Elm dictionary with similar operations on it.

The package contains two modules, for reading and writing respectively:

- `ElmFire.Dict`
  - Mirroring a Firebase location as an Elm `Dict`
  - Getting a signal of all updates
  - One-time retrieval


- `ElmFire.Op`
  - Inserting, updating and deleting of single key-value pairs
  - Inserting and deleting lists of key-value pairs
  - Updating the whole collection via the higher-order functions `map`, `filter` and `filterMap`
  - Operations on the whole store can be run sequentially, in parallel or as a single transaction.

## General Usage Pattern

These two modules are intended to be used together.

- The applications's model will comprise the mirrored dictionary as part of its state.
- Actions of the application may result in operations on the store.

Local modifications will be reflected immediately in addition to be sent to the Firebase and other clients connected to the same Firebase.
Likewise, remote modifications will be reflected in the locally mirrored dictionary.

## Configuration

All functionality of the package is guided by a configuration record that determines type mappings and other specifics of the Firebase collection.

```elm
type alias Config v =
  { location: ElmFire.Location
  , orderOptions: ElmFire.OrderOptions
  , encoder: v -> JD.Value
  , decoder: JD.Decoder v
  }

```

`location` specifies the Firebase and sub-path where the store is hosted.

`orderOptions` can be used to filter and limit the elements in the Firebase collection, that should be included in the local mirror. Use `ElmFire.noOrder` to access the whole collection.

Whereas the keys of the store are always of type String, the API is parameterized on the value type `v` of the store.

The user provides `encoder` and `decoder` functions to convert between the value type in Elm code and the JSON in the Firebase.

## Example Code

We setup a simple store with values of type `Int`.
`model` is the local mirror of the store.
`taskInit` and `taskMap` are sketches of operations on the store, to populate it and to perform a mapping over it.

```elm
config =
  { location = ElmFire.fromUrl (url)
  , orderOptions = ElmFire.noOrder
  , encoder = Json.Encode.int
  , decoder = Json.Decode.int
  }

-- Start mirroring
--   (run initialTask via a port)
--   model : Signal (Dict String Int)
(initialTask, model) = ElmFire.Dict.mirror config

-- Initialize the store  (run the tasks via a port)
taskInit : ElmFire.Op.Operation Int
taskInit =
  ElmFire.Op.fromList
    ElmFire.Op.parallel
    [("a",1), ("b",2)]

-- Double each value
taskMap : ElmFire.Op.Operation Int
taskMap =
  ElmFire.Op.map
    ElmFire.Op.sequential
    (\key val -> val * 2)
```
