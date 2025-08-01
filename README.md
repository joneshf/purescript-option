# purescript-option

A data type for optional values.

## Table of Contents

- [Explanation: Motivation for `Option _`](#explanation-motivation-for-option-_)
- [How To: Make a function with optional values](#how-to-make-a-function-with-optional-values)
- [How To: Make a function with optional values from a record](#how-to-make-a-function-with-optional-values-from-a-record)
- [How To: Make a function with required and optional values](#how-to-make-a-function-with-required-and-optional-values)
- [How To: Make a function with required and optional values from a record](#how-to-make-a-function-with-required-and-optional-values-from-a-record)
- [How To: Decode and Encode JSON with optional values in `purescript-argonaut`](#how-to-decode-and-encode-json-with-optional-values-in-purescript-argonaut)
- [How To: Decode and Encode JSON with optional values in `purescript-codec-argonaut`](#how-to-decode-and-encode-json-with-optional-values-in-purescript-codec-argonaut)
- [How To: Decode and Encode JSON with optional values in `purescript-simple-json`](#how-to-decode-and-encode-json-with-optional-values-in-purescript-simple-json)
- [How To: Decode and Encode JSON with required and optional values in `purescript-argonaut`](#how-to-decode-and-encode-json-with-required-and-optional-values-in-purescript-argonaut)
- [How To: Decode and Encode JSON with required and optional values in `purescript-codec-argonaut`](#how-to-decode-and-encode-json-with-required-and-optional-values-in-purescript-codec-argonaut)
- [How To: Decode and Encode JSON with required and optional values in `purescript-simple-json`](#how-to-decode-and-encode-json-with-required-and-optional-values-in-purescript-simple-json)
- [How To: Provide an easier API for `DateTime`](#how-to-provide-an-easier-api-for-datetime)
- [Reference: `FromRecord _ _ _`](#reference-fromrecord-_-_-_)

## Explanation: Motivation for `Option _`

There are a few different data types that encapsulate ideas in programming.

Records capture the idea of a collection of key/value pairs where every key and value exist.
E.g. `Record (foo :: Boolean, bar :: Int)` means that both `foo` and `bar` exist and with values all of the time.

Variants capture the idea of a collection of key/value pairs where exactly one of the key/value pairs exist.
E.g. `Data.Variant.Variant (foo :: Boolean, bar :: Int)` means that either only `foo` exists with a value or only `bar` exists with a value, but not both at the same time.

Options capture the idea of a collection of key/value pairs where any key and value may or may not exist.
E.g. `Option.Option (foo :: Boolean, bar :: Int)` means that either only `foo` exists with a value, only `bar` exists with a value, both `foo` and `bar` exist with values, or neither `foo` nor `bar` exist.

The distinction between these data types means that we can describe problems more accurately.
Options are typically what you find in dynamic languages or in weakly-typed static languages.
Their use cases range from making APIs more flexible to interfacing with serialization formats to providing better ergonomics around data types.

These data types are all specific to the PureScript language.
Different data types exist in other languages that combine some of these ideas.
In many languages records are a combination of both PureScript-style records and PureScript-style options.
E.g. `Option.Record (foo :: Boolean) (bar :: Int)` means that `foo` exists with a value all of the time, and either `bar` exists with a value or `bar` doesn't exist with a value.

Other languages might signify optional fields with a question mark.
E.g. In TypeScript, the previous example would be `{ foo: boolean; bar?: number }`

This is different from a required field with an optional value.
In PureScript, we might signify that by using: `Record (foo :: Boolean, bar :: Data.Maybe.Maybe Int)`.
In TypeScript, we might signify that by using: `{ foo: boolean; bar: number | null }`

## How To: Make a function with optional values

Let's say we want to make a `greeting` function where people can pass in an `Option ( name :: String, title :: String )` to override the default behavior.
I.e. we want something like: `greeting :: Option.Option ( name :: String, title :: String ) -> String`.
The implementation should be fairly straight forward:

```PureScript
greeting :: Option.Option ( name :: String, title :: String ) -> String
greeting option = "Hello, " <> title' <> name'
  where
  name' :: String
  name' = case Option.get (Type.Proxy.Proxy :: _ "name") option of
    Data.Maybe.Just name -> name
    Data.Maybe.Nothing -> "World"

  title' :: String
  title' = case Option.get (Type.Proxy.Proxy :: _ "title") option of
    Data.Maybe.Just title -> title <> " "
    Data.Maybe.Nothing -> ""
```

We look up each key in the given `Option _`, and decide what to do with it.
In the case of the `"title"`, we append a space so the output is still legible.
With the `greeting` function, we can pass in an option and alter the behavior:

```PureScript
> greeting (Option.fromRecord {})
"Hello, World"

> greeting (Option.fromRecord { title: "wonderful" })
"Hello, wonderful World"

> greeting (Option.fromRecord { name: "Pat" })
"Hello, Pat"

> greeting (Option.fromRecord { name: "Pat", title: "Dr." })
"Hello, Dr. Pat"
```

We've allowed people to override the behavior of the function with optional values!

It might be instructive to compare how we might write a similar function using a `Record _` instead of `Option _`:

```PureScript
greeting' ::
  Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ) ->
  String
greeting' option = "Hello, " <> title' <> name'
  where
  name' :: String
  name' = case option.name of
    Data.Maybe.Just name -> name
    Data.Maybe.Nothing -> "World"

  title' :: String
  title' = case option.title of
    Data.Maybe.Just title -> title <> " "
    Data.Maybe.Nothing -> ""
```

To implement `greeting'`, nothing really changed.
We used the built-in dot operator to fetch the keys out of the record, but we could have just as easily used `Record.get` (which would have highlighted the similarlities even more).

To use `greeting'`, we force the users of `greeting'` to do always give us a value in the record:

```PureScript
> User.greeting' { name: Data.Maybe.Nothing, title: Data.Maybe.Nothing }
"Hello, World"

> User.greeting' { name: Data.Maybe.Nothing, title: Data.Maybe.Just "wonderful" }
"Hello, wonderful World"

> User.greeting' { name: Data.Maybe.Just "Pat", title: Data.Maybe.Nothing }
"Hello, Pat"

> User.greeting' { name: Data.Maybe.Just "Pat", title: Data.Maybe.Just "Dr." }
"Hello, Dr. Pat"
```

## How To: Make a function with optional values from a record

Let's say we want to solve a similar problem as before, but we don't want to force people to create the `Option _` themselves.
We want to allow people to pass in a record that may be missing some fields.
To write this version of `greeting`, we'll need to use the `FromRecord` typeclass (see the section on `FromRecord` for more information).
I.e. we want something like: `greeting :: forall record. Option.FromRecord record ( name :: String, title :: String ) => Record record -> String`.

The implementation moves the work of constructing the `Option _`, but should also be straight forward:

```PureScript
greeting ::
  forall record.
  Option.FromRecord record () ( name :: String, title :: String ) =>
  Record record ->
  String
greeting record = "Hello, " <> title' <> name'
  where
  name' :: String
  name' = case Option.get (Type.Proxy.Proxy :: _ "name") option of
    Data.Maybe.Just name -> name
    Data.Maybe.Nothing -> "World"

  option :: Option.Option ( name :: String, title :: String )
  option = Option.fromRecord record

  title' :: String
  title' = case Option.get (Type.Proxy.Proxy :: _ "title") option of
    Data.Maybe.Just title -> title <> " "
    Data.Maybe.Nothing -> ""
```

We can use this similar to how we used the previous implementation of `greeting`.
Instead of needing to construct an `Option _` and pass it in, we give the `Record _` directly:

```PureScript
> greeting {}
"Hello, World"

> greeting { title: "wonderful" }
"Hello, wonderful World"

> greeting { name: "Pat" }
"Hello, Pat"

> greeting { name: "Pat", title: "Dr." }
"Hello, Dr. Pat"
```

We've allowed people to override the behavior of the function with optional values using a record!

## How To: Make a function with required and optional values

Let's say we want to make a `greeting` function where people can pass in an `Option.Record ( name :: String ) ( title :: String )`.
The `"name"` is required, but a `"title"` can be given to override the default behavior.
I.e. we want something like: `greeting :: Option.Record ( name :: String ) ( title :: String ) -> String`.
The implementation should be fairly straight forward:

```PureScript
import Prelude
import Data.Maybe as Data.Maybe
import Option as Option

greeting ::
  Option.Record ( name :: String ) ( title :: String ) ->
  String
greeting record' = "Hello, " <> title' <> record.name
  where
  record :: Record ( name :: String, title :: Data.Maybe.Maybe String )
  record = Option.recordToRecord record'

  title' :: String
  title' = case record.title of
    Data.Maybe.Just title -> title <> " "
    Data.Maybe.Nothing -> ""
```

We look up the `"title"` in the given `Option.Record _ _`, and decide what to do with it.
In the case of the `"title"`, we append a space so the output is still legible.
With the `greeting` function, we can pass in an option and alter the behavior:

```PureScript
> greeting (Option.recordFromRecord { name: "Pat" })
"Hello, Pat"

> greeting (Option.recordFromRecord { name: "Pat", title: "Dr." })
"Hello, Dr. Pat"
```

We've allowed people to override the behavior of the function with optional values!

It might be instructive to compare how we might write a similar function using a `Record _` instead of `Option _`:

```PureScript
greeting' ::
  Record ( name :: String, title :: Data.Maybe.Maybe String ) ->
  String
greeting' record = "Hello, " <> title' <> record.name
  where
  title' :: String
  title' = case record.title of
    Data.Maybe.Just title -> title <> " "
    Data.Maybe.Nothing -> ""
```

To implement `greeting'`, nothing really changed.
We don't have to convert down to a language-level Record because the argument is already a language-level Record.
That's the only difference as far as implementing `greeting'`.

To use `greeting'`, we force the users of `greeting'` to do always give us a value in the record:

```PureScript
> User.greeting' { name: "Pat", title: Data.Maybe.Nothing }
"Hello, Pat"

> User.greeting' { name: "Pat", title: Data.Maybe.Just "Dr." }
"Hello, Dr. Pat"
```

## How To: Make a function with required and optional values from a record

Let's say we want to solve a similar problem as before, but we don't want to force people to create the `Option.Record _ _` themselves.
We want to allow people to pass in a record that may be missing some fields.
To write this version of `greeting`, we'll need to use the `FromRecord` typeclass (see the section on `FromRecord` for more information).
I.e. we want something like: `greeting :: forall record. Option.FromRecord record ( name :: String ) ( title :: String ) => Record record -> String`.

The implementation moves the work of constructing the `Option.Record _ _`, but should also be straight forward:

```PureScript
import Prelude
import Data.Maybe as Data.Maybe
import Option as Option

greeting ::
  forall record.
  Option.FromRecord record ( name :: String ) ( title :: String ) =>
  Record record ->
  String
greeting record'' = "Hello, " <> title' <> record.name
  where
  record :: Record ( name :: String, title :: Data.Maybe.Maybe String )
  record = Option.recordToRecord record'

  record' :: Option.Record ( name :: String ) ( title :: String )
  record' = Option.recordFromRecord record''

  title' :: String
  title' = case record.title of
    Data.Maybe.Just title -> title <> " "
    Data.Maybe.Nothing -> ""
```

We can use this similar to how we used the previous implementation of `greeting`.
Instead of needing to construct an `Option.Record _ _` and pass it in, we give the `Record _` directly:

```PureScript
> greeting { name: "Pat" }
"Hello, Pat"

> greeting { name: "Pat", title: "Dr." }
"Hello, Dr. Pat"
```

We've allowed people to override the behavior of the function with optional values using a record!

## How To: Decode and Encode JSON with optional values in `purescript-argonaut`

A common pattern with JSON objects is that keys do not always have to be present.
Some APIs make the distinction between a JSON object like `{ "name": "Pat" }` and one like `{ "name": "Pat", "title": null }`.
In the first case, it might recognize that the `"title"` key does not exist, and behave in a different way from the `"title"` key having a value of `null`.
In the second case, it might notice that the `"title"` key exists and work with the value assuming it's good to go; the `null` might eventually cause a failure later.

In many cases, what we want is to not generate any fields that do not exist.
Using `purescript-argonaut`, `Option _` can help with that idea:

```PureScript
decode ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either String (Option.Option ( name :: String, title :: String ))
decode = Data.Argonaut.Decode.Class.decodeJson

encode ::
  Option.Option ( name :: String, title :: String ) ->
  Data.Argonaut.Core.Json
encode = Data.Argonaut.Encode.Class.encodeJson

parse ::
  String ->
  Data.Either.Either String (Option.Option (name :: String, title :: String))
parse string = case Data.Argonaut.Parser.jsonParser string of
  Data.Either.Left error -> Data.Either.Left error
  Data.Either.Right json -> case decode json of
    Data.Either.Left error -> Data.Either.Left (Data.Argonaut.Decode.Error.printJsonDecodeError error)
    Data.Either.Right option -> Data.Either.Right option
```

We can give that a spin with some different JSON values:

```PureScript
> parse """{}"""
(Right (Option.fromRecord {}))

> parse """{"title": "wonderful"}"""
(Right (Option.fromRecord { title: "wonderful" }))

> parse """{"name": "Pat"}"""
(Right (Option.fromRecord { name: "Pat" }))

> parse """{"name": "Pat", "title": "Dr."}"""
(Right (Option.fromRecord { name: "Pat", title: "Dr." }))

> parse """{ "name": null }"""
Right (Option.fromRecord {})

> parse """{ "title": null }"""
Right (Option.fromRecord {})

> parse """{ "name": null, "title": null }"""
Right (Option.fromRecord {})
```

We can also produce some different JSON values:

```PureScript
> Data.Argonaut.Core.stringify (encode (Option.fromRecord {}))
"{}"

> Data.Argonaut.Core.stringify (encode (Option.fromRecord { title: "wonderful" }))
"{\"title\":\"wonderful\"}"

> Data.Argonaut.Core.stringify (encode (Option.fromRecord { name: "Pat" }))
"{\"name\":\"Pat\"}"

> Data.Argonaut.Core.stringify (encode (Option.fromRecord { name: "Pat", title: "Dr." }))
"{\"title\":\"Dr.\",\"name\":\"Pat\"}"
```

Notice that we don't end up with a `"title"` field in the JSON output unless we have a `title` field in our record.

It might be instructive to compare how we might write a similar functions using a `Record _` instead of `Option _`:
With `purescript-argonaut`, the instances for decoding and encoding on records expect the field to always exist no matter its value.
If we attempt to go directly to `Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String )`:

```PureScript
decode' ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either String (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
decode' = Data.Argonaut.Decode.Class.decodeJson

encode' ::
  Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ) ->
  Data.Argonaut.Core.Json
encode' = Data.Argonaut.Encode.Class.encodeJson

parse' ::
  String ->
  Data.Either.Either String (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
parse' string = case Data.Argonaut.Parser.jsonParser string of
  Data.Either.Left error -> Data.Either.Left error
  Data.Either.Right json -> case decode json of
    Data.Either.Left error -> Data.Either.Left (Data.Argonaut.Decode.Error.printJsonDecodeError error)
    Data.Either.Right record -> Data.Either.Right record
```

We won't get the behavior we expect:

```PureScript
> parse' """{}"""
(Left "An error occurred while decoding a JSON value:\n  At object key 'title':\n  No value was found.")

> parse' """{"title": "wonderful"}"""
(Left "An error occurred while decoding a JSON value:\n  At object key 'name':\n  No value was found.")

> parse' """{"name": "Pat"}"""
(Left "An error occurred while decoding a JSON value:\n  At object key 'title':\n  No value was found.")

> parse' """{"name": "Pat", "title": "Dr."}"""
(Right { name: (Just "Pat"), title: (Just "Dr.") })

> Data.Argonaut.Core.stringify (encode' { name: Data.Maybe.Nothing, title: Data.Maybe.Nothing })
"{\"title\":null,\"name\":null}"

> Data.Argonaut.Core.stringify (encode' { name: Data.Maybe.Nothing, title: Data.Maybe.Just "wonderful" })
"{\"title\":\"wonderful\",\"name\":null}"

> Data.Argonaut.Core.stringify (encode' { name: Data.Maybe.Just "Pat", title: Data.Maybe.Nothing })
"{\"title\":null,\"name\":\"Pat\"}"

> Data.Argonaut.Core.stringify (encode' { name: Data.Maybe.Just "Pat", title: Data.Maybe.Just "Dr." })
"{\"title\":\"Dr.\",\"name\":\"Pat\"}"
```

Unless both fields exist, we cannot decode the JSON object.
Similarly, no matter what the values are, we always encode them into a JSON object.

In order to emulate the behavior of an optional field, we have to name the record, and write our own instances:

```PureScript
newtype Greeting
  = Greeting
  ( Record
      ( name :: Data.Maybe.Maybe String
      , title :: Data.Maybe.Maybe String
      )
  )

derive instance genericGreeting :: Data.Generic.Rep.Generic Greeting _

instance showGreeting :: Show Greeting where
  show = Data.Generic.Rep.Show.genericShow

instance decodeJsonGreeting :: Data.Argonaut.Decode.Class.DecodeJson Greeting where
  decodeJson json = do
    object <- Data.Argonaut.Decode.Class.decodeJson json
    name <- Data.Argonaut.Decode.Combinators.getFieldOptional object "name"
    title <- Data.Argonaut.Decode.Combinators.getFieldOptional object "title"
    pure (Greeting { name, title })

instance encodeJsonGreeting :: Data.Argonaut.Encode.Class.EncodeJson Greeting where
  encodeJson (Greeting { name, title }) =
    Data.Argonaut.Encode.Combinators.extendOptional
      (Data.Argonaut.Encode.Combinators.assocOptional "name" name)
      ( Data.Argonaut.Encode.Combinators.extendOptional
          (Data.Argonaut.Encode.Combinators.assocOptional "title" title)
          (Data.Argonaut.Core.jsonEmptyObject)
      )

parse'' ::
  String ->
  Data.Either.Either String Greeting
parse'' string = case Data.Argonaut.Parser.jsonParser string of
  Data.Either.Left error -> Data.Either.Left error
  Data.Either.Right json -> case decode json of
    Data.Either.Left error -> Data.Either.Left (Data.Argonaut.Decode.Error.printJsonDecodeError error)
    Data.Either.Right greeting -> Data.Either.Right greeting
```

If we try decoding and encoding now, we get something closer to what we wanted:

```PureScript
> parse'' """{}"""
(Right (Greeting { name: Nothing, title: Nothing }))

> parse'' """{"title": "wonderful"}"""
(Right (Greeting { name: Nothing, title: (Just "wonderful") }))

> parse'' """{"name": "Pat"}"""
(Right (Greeting { name: (Just "Pat"), title: Nothing }))

> parse'' """{"name": "Pat", "title": "Dr."}"""
(Right (Greeting { name: (Just "Pat"), title: (Just "Dr.") }))

> Data.Argonaut.Core.stringify (Data.Argonaut.Encode.Class.encodeJson (Greeting { name: Data.Maybe.Nothing, title: Data.Maybe.Nothing }))
"{}"

> Data.Argonaut.Core.stringify (Data.Argonaut.Encode.Class.encodeJson (Greeting { name: Data.Maybe.Nothing, title: Data.Maybe.Just "wonderful" }))
"{\"title\":\"wonderful\"}"

> Data.Argonaut.Core.stringify (Data.Argonaut.Encode.Class.encodeJson (Greeting { name: Data.Maybe.Just "Pat", title: Data.Maybe.Nothing }))
"{\"name\":\"Pat\"}"

> Data.Argonaut.Core.stringify (Data.Argonaut.Encode.Class.encodeJson (Greeting { name: Data.Maybe.Just "Pat", title: Data.Maybe.Just "Dr." }))
"{\"title\":\"Dr.\",\"name\":\"Pat\"}"
```

## How To: Decode and Encode JSON with optional values in `purescript-codec-argonaut`

A common pattern with JSON objects is that keys do not always have to be present.
Some APIs make the distinction between a JSON object like `{ "name": "Pat" }` and one like `{ "name": "Pat", "title": null }`.
In the first case, it might recognize that the `"title"` key does not exist, and behave in a different way from the `"title"` key having a value of `null`.
In the second case, it might notice that the `"title"` key exists and work with the value assuming it's good to go; the `null` might eventually cause a failure later.

In many cases, what we want is to not generate any fields that do not exist.
Using `purescript-codec-argonaut`, `Option _` can help with that idea:

```PureScript
jsonCodec :: Data.Codec.Argonaut.JsonCodec (Option.Option ( name :: String, title :: String ))
jsonCodec =
  Option.jsonCodec
    { name: Data.Codec.Argonaut.string
    , title: Data.Codec.Argonaut.string
    }
```

We can add a couple of helpers to make decoding/encoding easier in the REPL:

```PureScript
decode ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Option.Option ( name :: String, title :: String ))
decode = Data.Codec.Argonaut.decode jsonCodec

encode ::
  Option.Option ( name :: String, title :: String ) ->
  Data.Argonaut.Core.Json
encode = Data.Codec.Argonaut.encode jsonCodec

parse ::
  String ->
  Data.Either.Either String (Option.Option ( name :: String, title :: String ))
parse string = do
  json <- Data.Argonaut.Parser.jsonParser string
  case decode json of
    Data.Either.Left err -> Data.Either.Left (Data.Codec.Argonaut.printJsonDecodeError err)
    Data.Either.Right option -> Data.Either.Right option
```

We can give that a spin with some different JSON values:

```PureScript
> parse """{}"""
(Right (Option.fromRecord {}))

> parse """{"title": "wonderful"}"""
(Right (Option.fromRecord { title: "wonderful" }))

> parse """{"name": "Pat"}"""
(Right (Option.fromRecord { name: "Pat" }))

> parse """{"name": "Pat", "title": "Dr."}"""
(Right (Option.fromRecord { name: "Pat", title: "Dr." }))

> parse """{ "name": null }"""
Right (Option.fromRecord {})

> parse """{ "title": null }"""
Right (Option.fromRecord {})

> parse """{ "name": null, "title": null }"""
Right (Option.fromRecord {})
```

We can also produce some different JSON values:

```PureScript
> Data.Argonaut.Core.stringify (encode (Option.fromRecord {}))
"{}"

> Data.Argonaut.Core.stringify (encode (Option.fromRecord { title: "wonderful" }))
"{\"title\":\"wonderful\"}"

> Data.Argonaut.Core.stringify (encode (Option.fromRecord { name: "Pat" }))
"{\"name\":\"Pat\"}"

> Data.Argonaut.Core.stringify (encode (Option.fromRecord { name: "Pat", title: "Dr." }))
"{\"name\":\"Pat\",\"title\":\"Dr.\"}"
```

Notice that we don't end up with a `"title"` field in the JSON output unless we have a `title` field in our record.

It might be instructive to compare how we might write a similar functions using a `Record _` instead of `Option _`:
With `purescript-codec-argonaut`, there are a couple of codecs that ship with the package for records: `Data.Codec.Argonaut.recordProp` and `Data.Codec.Argonaut.Record.record`.
Each of those codecs expect the field to always exist no matter its value.
The difference between those codecs is not very relevant except to say that the latter requires less characters to use.
There are also a couple of codecs that ship with the package for `Data.Maybe.Maybe _`: `Data.Codec.Argonaut.Common.maybe` and `Data.Codec.Argonaut.Compat.maybe`.
The former decodes/encodes with tagged values, the latter with `null`s.
If we attempt to go directly to `Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String )` and `Data.Codec.Argonaut.Common.maybe`:

```PureScript
decode' ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
decode' = Data.Codec.Argonaut.decode jsonCodec'

encode' ::
  Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ) ->
  Data.Argonaut.Core.Json
encode' = Data.Codec.Argonaut.encode jsonCodec'

jsonCodec' :: Data.Codec.Argonaut.JsonCodec (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
jsonCodec' =
  Data.Codec.Argonaut.Record.object
    "Greeting"
    { name: Data.Codec.Argonaut.Common.maybe Data.Codec.Argonaut.string
    , title: Data.Codec.Argonaut.Common.maybe Data.Codec.Argonaut.string
    }

parse' ::
  String ->
  Data.Either.Either String (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
parse' string = do
  json <- Data.Argonaut.Parser.jsonParser string
  case decode' json of
    Data.Either.Left err -> Data.Either.Left (Data.Codec.Argonaut.printJsonDecodeError err)
    Data.Either.Right option -> Data.Either.Right option
```

We won't get the behavior we expect:

```PureScript
> parse' """{}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  No value was found.")

> parse' """{"title": "wonderful"}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  Under 'Maybe':\n  Expected value of type 'Object'.")

> parse' """{"name": "Pat"}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  No value was found.")

> parse' """{"name": "Pat", "title": "Dr."}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  Under 'Maybe':\n  Expected value of type 'Object'.")

> parse' """{"name": {"tag": "Just", "value": "Pat"}, "title": {"tag": "Just", "value": "Dr."}}"""
(Right { name: (Just "Pat"), title: (Just "Dr.") })

> Data.Argonaut.Core.stringify (encode' { name: Data.Maybe.Nothing, title: Data.Maybe.Nothing })
"{\"name\":{\"tag\":\"Nothing\"},\"title\":{\"tag\":\"Nothing\"}}"

> Data.Argonaut.Core.stringify (encode' { name: Data.Maybe.Nothing, title: Data.Maybe.Just "wonderful" })
"{\"name\":{\"tag\":\"Nothing\"},\"title\":{\"tag\":\"Just\",\"value\":\"wonderful\"}}"

> Data.Argonaut.Core.stringify (encode' { name: Data.Maybe.Just "Pat", title: Data.Maybe.Nothing })
"{\"name\":{\"tag\":\"Just\",\"value\":\"Pat\"},\"title\":{\"tag\":\"Nothing\"}}"

> Data.Argonaut.Core.stringify (encode' { name: Data.Maybe.Just "Pat", title: Data.Maybe.Just "Dr." })
"{\"name\":{\"tag\":\"Just\",\"value\":\"Pat\"},\"title\":{\"tag\":\"Just\",\"value\":\"Dr.\"}}"
```

Unless both fields exist, we cannot decode the JSON object.
Not only is every field required, they're serialized as tagged values.
Similarly, no matter what the values are, we always encode them into a JSON object.

If we try with `Data.Codec.Argonaut.Compat.maybe`:

```PureScript
decode'' ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
decode'' = Data.Codec.Argonaut.decode jsonCodec''

encode'' ::
  Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ) ->
  Data.Argonaut.Core.Json
encode'' = Data.Codec.Argonaut.encode jsonCodec''

jsonCodec'' :: Data.Codec.Argonaut.JsonCodec (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
jsonCodec'' =
  Data.Codec.Argonaut.Record.object
    "Greeting"
    { name: Data.Codec.Argonaut.Compat.maybe Data.Codec.Argonaut.string
    , title: Data.Codec.Argonaut.Compat.maybe Data.Codec.Argonaut.string
    }

parse'' ::
  String ->
  Data.Either.Either String (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
parse'' string = do
  json <- Data.Argonaut.Parser.jsonParser string
  case decode'' json of
    Data.Either.Left err -> Data.Either.Left (Data.Codec.Argonaut.printJsonDecodeError err)
    Data.Either.Right option -> Data.Either.Right option
```

We also don't get the behavior we expect:

```PureScript
> parse'' """{}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  No value was found.")

> parse'' """{"title": "wonderful"}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key name:\n  No value was found.")

> parse'' """{"name": "Pat"}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  No value was found.")

> parse'' """{"name": "Pat", "title": "Dr."}"""
(Right { name: (Just "Pat"), title: (Just "Dr.") })

> Data.Argonaut.Core.stringify (encode'' { name: Data.Maybe.Nothing, title: Data.Maybe.Nothing })
"{\"name\":null,\"title\":null}"

> Data.Argonaut.Core.stringify (encode'' { name: Data.Maybe.Nothing, title: Data.Maybe.Just "wonderful" })
"{\"name\":null,\"title\":\"wonderful\"}"

> Data.Argonaut.Core.stringify (encode'' { name: Data.Maybe.Just "Pat", title: Data.Maybe.Nothing })
"{\"name\":\"Pat\",\"title\":null}"

> Data.Argonaut.Core.stringify (encode'' { name: Data.Maybe.Just "Pat", title: Data.Maybe.Just "Dr." })
"{\"name\":\"Pat\",\"title\":\"Dr.\"}"
```

Unless both fields exist, we cannot decode the JSON object.
Similarly, no matter what the values are, we always encode them into a JSON object.

In order to emulate the behavior of an optional field, we have to use a different codec:

```PureScript
optionalField ::
  forall label record record' value.
  Data.Symbol.IsSymbol label =>
  Prim.Row.Cons label (Data.Maybe.Maybe value) record' record =>
  Prim.Row.Lacks label record' =>
  Type.Proxy.Proxy label ->
  Data.Codec.Argonaut.JsonCodec value ->
  Data.Codec.Argonaut.JPropCodec (Record record') ->
  Data.Codec.Argonaut.JPropCodec (Record record)
optionalField label codecValue codecRecord =
  Data.Codec.GCodec
    (Control.Monad.Reader.Trans.ReaderT decodeField)
    (Data.Profunctor.Star.Star encodeField)
  where
  decodeField ::
    Foreign.Object.Object Data.Argonaut.Core.Json ->
    Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Record record)
  decodeField object' = do
    record <- Data.Codec.Argonaut.decode codecRecord object'
    case Foreign.Object.lookup key object' of
      Data.Maybe.Just json -> case Data.Codec.Argonaut.decode codecValue json of
        Data.Either.Left error -> Data.Either.Left (Data.Codec.Argonaut.AtKey key error)
        Data.Either.Right value -> Data.Either.Right (Record.insert label (Data.Maybe.Just value) record)
      Data.Maybe.Nothing -> Data.Either.Right (Record.insert label Data.Maybe.Nothing record)

  encodeField ::
    Record record ->
    Control.Monad.Writer.Writer (Data.List.List (Data.Tuple.Tuple String Data.Argonaut.Core.Json)) (Record record)
  encodeField record = do
    case Record.get label record of
      Data.Maybe.Just value ->
        Control.Monad.Writer.Class.tell
          ( Data.List.Cons
              (Data.Tuple.Tuple key (Data.Codec.Argonaut.encode codecValue value))
              Data.List.Nil
          )
      Data.Maybe.Nothing -> pure unit
    Control.Monad.Writer.Class.tell
      (Data.Codec.Argonaut.encode codecRecord (Record.delete label record))
    pure record

  key :: String
  key = Data.Symbol.reflectSymbol label
```

With this codec defined, we can implement a codec for the record with optional fields:

```PureScript
decode''' ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
decode''' = Data.Codec.Argonaut.decode jsonCodec'''

encode''' ::
  Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ) ->
  Data.Argonaut.Core.Json
encode''' = Data.Codec.Argonaut.encode jsonCodec'''

jsonCodec''' :: Data.Codec.Argonaut.JsonCodec (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
jsonCodec''' =
  Data.Codec.Argonaut.object
    "Greeting"
    ( optionalField (Type.Proxy.Proxy :: _ "name") Data.Codec.Argonaut.string
        $ optionalField (Type.Proxy.Proxy :: _ "title") Data.Codec.Argonaut.string
        $ Data.Codec.Argonaut.record
    )

parse''' ::
  String ->
  Data.Either.Either String (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
parse''' string = do
  json <- Data.Argonaut.Parser.jsonParser string
  case decode''' json of
    Data.Either.Left err -> Data.Either.Left (Data.Codec.Argonaut.printJsonDecodeError err)
    Data.Either.Right option -> Data.Either.Right option
```

If we try decoding and encoding now, we get something closer to what we wanted:

```PureScript
> parse''' """{}"""
(Right { name: Nothing, title: Nothing })

> parse''' """{"title": "wonderful"}"""
(Right { name: Nothing, title: (Just "wonderful") })

> parse''' """{"name": "Pat"}"""
(Right { name: (Just "Pat"), title: Nothing })

> parse''' """{"name": "Pat", "title": "wonderful"}"""
(Right { name: (Just "Pat"), title: (Just "wonderful") })

> Data.Argonaut.Core.stringify (encode''' { name: Data.Maybe.Nothing, title: Data.Maybe.Nothing })
"{}"

> Data.Argonaut.Core.stringify (encode''' { name: Data.Maybe.Nothing, title: Data.Maybe.Just "wonderful" })
"{\"title\":\"wonderful\"}"

> Data.Argonaut.Core.stringify (encode''' { name: Data.Maybe.Just "Pat", title: Data.Maybe.Nothing })
"{\"name\":\"Pat\"}"

> Data.Argonaut.Core.stringify (encode''' { name: Data.Maybe.Just "Pat", title: Data.Maybe.Just "Dr." })
"{\"name\":\"Pat\",\"title\":\"Dr.\"}"
```

## How To: Decode and Encode JSON with optional values in `purescript-simple-json`

A common pattern with JSON objects is that keys do not always have to be present.
Some APIs make the distinction between a JSON object like `{ "name": "Pat" }` and one like `{ "name": "Pat", "title": null }`.
In the first case, it might recognize that the `"title"` key does not exist, and behave in a different way from the `"title"` key having a value of `null`.
In the second case, it might notice that the `"title"` key exists and work with the value assuming it's good to go; the `null` might eventually cause a failure later.

In many cases, what we want is to not generate any fields that do not exist.
Using `purescript-simple-json`, `Option _` can help with that idea:

```PureScript
readJSON ::
  String ->
  Simple.JSON.E (Option.Option ( name :: String, title :: String ))
readJSON = Simple.JSON.readJSON

writeJSON ::
  Option.Option ( name :: String, title :: String ) ->
  String
writeJSON = Simple.JSON.writeJSON
```

We can give that a spin with some different JSON values:

```PureScript
> readJSON """{}"""
(Right (Option.fromRecord {}))

> readJSON """{"title": "wonderful"}"""
(Right (Option.fromRecord { title: "wonderful" }))

> readJSON """{"name": "Pat"}"""
(Right (Option.fromRecord { name: "Pat" }))

> readJSON """{"name": "Pat", "title": "Dr."}"""
(Right (Option.fromRecord { name: "Pat", title: "Dr." }))

> readJSON """{ "name": null }"""
Right (Option.fromRecord {})

> readJSON """{ "title": null }"""
Right (Option.fromRecord {})

> readJSON """{ "name": null, "title": null }"""
Right (Option.fromRecord {})
```

We can also produce some different JSON values:

```PureScript
> writeJSON (Option.fromRecord {})
"{}"

> writeJSON (Option.fromRecord {title: "wonderful"})
"{\"title\":\"wonderful\"}"

> writeJSON (Option.fromRecord {name: "Pat"})
"{\"name\":\"Pat\"}"

> writeJSON (Option.fromRecord {name: "Pat", title: "Dr."})
"{\"title\":\"Dr.\",\"name\":\"Pat\"}"
```

Notice that we don't end up with a `"title"` field in the JSON output unless we have a `title` field in our record.

It might be instructive to compare how we might write a similar functions using a `Record _` instead of `Option _`:
With `purescript-simple-json`, the instances for decoding and encoding on records handle `Data.Maybe.Maybe _` values like they are optional.
If we attempt to go directly to `Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String )`:

```PureScript
readJSON' ::
  String ->
  Simple.JSON.E (Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ))
readJSON' = Simple.JSON.readJSON

writeJSON' ::
  Record ( name :: Data.Maybe.Maybe String, title :: Data.Maybe.Maybe String ) ->
  String
writeJSON' = Simple.JSON.writeJSON
```

We get the behavior we expect:

```PureScript
> readJSON' """{}"""
(Right { name: Nothing, title: Nothing })

> readJSON' """{"title": "wonderful"}"""
(Right { name: Nothing, title: (Just "wonderful") })

> readJSON' """{"name": "Pat"}"""
(Right { name: (Just "Pat"), title: Nothing })

> readJSON' """{"name": "Pat", "title": "Dr."}"""
(Right { name: (Just "Pat"), title: (Just "Dr.") })
> decode' =<< Data.Argonaut.Parser.jsonParser """{}"""
(Left "JSON was missing expected field: title")

> writeJSON' {name: Data.Maybe.Nothing, title: Data.Maybe.Nothing}
"{}"

> writeJSON' {name: Data.Maybe.Nothing, title: Data.Maybe.Just "wonderful"}
"{\"title\":\"wonderful\"}"

> writeJSON' {name: Data.Maybe.Just "Pat", title: Data.Maybe.Nothing}
"{\"name\":\"Pat\"}"

> writeJSON' {name: Data.Maybe.Just "Pat", title: Data.Maybe.Just "Dr."}
"{\"title\":\"Dr.\",\"name\":\"Pat\"}"
```

## How To: Decode and Encode JSON with required and optional values in `purescript-argonaut`

Another common pattern with JSON objects is that some keys always have to be present while others do not.
Some APIs make the distinction between a JSON object like `{ "name": "Pat" }` and one like `{ "name": "Pat", "title": null }`.
In the first case, it might recognize that the `"title"` key does not exist, and behave in a different way from the `"title"` key having a value of `null`.
In the second case, it might notice that the `"title"` key exists and work with the value assuming it's good to go; the `null` might eventually cause a failure later.

In many cases, what we want is to not generate any fields that do not exist.
Using `purescript-argonaut`, `Option.Record _ _` can help with that idea:

```PureScript
import Prelude
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Argonaut.Decode.Class as Data.Argonaut.Decode.Class
import Data.Argonaut.Decode.Error as Data.Argonaut.Decode.Error
import Data.Argonaut.Encode.Class as Data.Argonaut.Encode.Class
import Data.Argonaut.Parser as Data.Argonaut.Parser
import Data.Either as Data.Either
import Option as Option

decode ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either String (Option.Record ( name :: String ) ( title :: String ))
decode = Data.Argonaut.Decode.Class.decodeJson

encode ::
  Option.Record ( name :: String ) ( title :: String ) ->
  Data.Argonaut.Core.Json
encode = Data.Argonaut.Encode.Class.encodeJson

parse ::
  String ->
  Data.Either.Either String (Option.Record ( name :: String ) ( title :: String ))
parse string = case Data.Argonaut.Parser.jsonParser string of
  Data.Either.Left error -> Data.Either.Left error
  Data.Either.Right json -> case decode json of
    Data.Either.Left error -> Data.Either.Left (Data.Argonaut.Decode.Error.printJsonDecodeError error)
    Data.Either.Right record -> Data.Either.Right record
```

We can give that a spin with some different JSON values:

```PureScript
> parse """{}"""
(Left "An error occurred while decoding a JSON value:\n  At object key 'name':\n  No value was found.")

> parse """{"title": "wonderful"}"""
(Left "An error occurred while decoding a JSON value:\n  At object key 'name':\n  No value was found.")

> parse """{"name": "Pat"}"""
(Right (Option.recordFromRecord { name: "Pat" }))

> parse """{"name": "Pat", "title": "Dr."}"""
(Right (Option.recordFromRecord { name: "Pat", title: "Dr." }))

> parse """{ "name": "Pat", "title": null }"""
Right (Option.recordFromRecord { name: "Pat" })
```

We can also produce some different JSON values:

```PureScript
> Data.Argonaut.Core.stringify (encode (Option.recordFromRecord { name: "Pat" }))
"{\"name\":\"Pat\"}"

> Data.Argonaut.Core.stringify (encode (Option.recordFromRecord { name: "Pat", title: "Dr." }))
"{\"title\":\"Dr.\",\"name\":\"Pat\"}"
```

Notice that we don't end up with a `"title"` field in the JSON output unless we have a `title` field in our record.

It might be instructive to compare how we might write a similar functions using a `Record _` instead of `Option.Record _ _`:
With `purescript-argonaut`, the instances for decoding and encoding on records expect the field to always exist no matter its value.
If we attempt to go directly to `Record ( name :: String, title :: Data.Maybe.Maybe String )`:

```PureScript
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Argonaut.Decode.Class as Data.Argonaut.Decode.Class
import Data.Argonaut.Decode.Error as Data.Argonaut.Decode.Error
import Data.Argonaut.Encode.Class as Data.Argonaut.Encode.Class
import Data.Argonaut.Parser as Data.Argonaut.Parser
import Data.Either as Data.Either
import Data.Maybe as Data.Maybe

decode' ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either String (Record ( name :: String, title :: Data.Maybe.Maybe String ))
decode' = Data.Argonaut.Decode.Class.decodeJson

encode' ::
  Record ( name :: String, title :: Data.Maybe.Maybe String ) ->
  Data.Argonaut.Core.Json
encode' = Data.Argonaut.Encode.Class.encodeJson

parse' ::
  String ->
  Data.Either.Either String (Record ( name :: String, title :: Data.Maybe.Maybe String ))
parse' string = case Data.Argonaut.Parser.jsonParser string of
  Data.Either.Left error -> Data.Either.Left error
  Data.Either.Right json -> case decode json of
    Data.Either.Left error -> Data.Either.Left (Data.Argonaut.Decode.Error.printJsonDecodeError error)
    Data.Either.Right record -> Data.Either.Right record
```

We won't get the behavior we expect:

```PureScript
> parse' """{}"""
(Left "An error occurred while decoding a JSON value:\n  At object key 'title':\n  No value was found.")

> parse' """{"title": "wonderful"}"""
(Left "An error occurred while decoding a JSON value:\n  At object key 'name':\n  No value was found.")

> parse' """{"name": "Pat"}"""
(Left "An error occurred while decoding a JSON value:\n  At object key 'title':\n  No value was found.")

> parse' """{"name": "Pat", "title": "Dr."}"""
(Right { name: "Pat", title: (Just "Dr.") })

> Data.Argonaut.Core.stringify (encode' { name: "Pat", title: Data.Maybe.Nothing })
"{\"title\":null,\"name\":\"Pat\"}"

> Data.Argonaut.Core.stringify (encode' { name: "Pat", title: Data.Maybe.Just "Dr." })
"{\"title\":\"Dr.\",\"name\":\"Pat\"}"
```

Unless both fields exist, we cannot decode the JSON object.
Similarly, no matter what the values are, we always encode them into a JSON object.

In order to emulate the behavior of an optional field, we have to name the record, and write our own instances:

```PureScript
import Prelude
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Argonaut.Decode.Class as Data.Argonaut.Decode.Class
import Data.Argonaut.Decode.Combinators as Data.Argonaut.Decode.Combinators
import Data.Argonaut.Decode.Error as Data.Argonaut.Decode.Error
import Data.Argonaut.Encode.Class as Data.Argonaut.Encode.Class
import Data.Argonaut.Encode.Combinators as Data.Argonaut.Encode.Combinators
import Data.Argonaut.Parser as Data.Argonaut.Parser
import Data.Either as Data.Either
import Data.Generic.Rep as Data.Generic.Rep
import Data.Generic.Rep.Show as Data.Generic.Rep.Show
import Data.Maybe as Data.Maybe

newtype Greeting
  = Greeting
  ( Record
      ( name :: String
      , title :: Data.Maybe.Maybe String
      )
  )

derive instance genericGreeting :: Data.Generic.Rep.Generic Greeting _

instance showGreeting :: Show Greeting where
  show = Data.Generic.Rep.Show.genericShow

instance decodeJsonGreeting :: Data.Argonaut.Decode.Class.DecodeJson Greeting where
  decodeJson json = do
    object <- Data.Argonaut.Decode.Class.decodeJson json
    name <- Data.Argonaut.Decode.Combinators.getField object "name"
    title <- Data.Argonaut.Decode.Combinators.getFieldOptional object "title"
    pure (Greeting { name, title })

instance encodeJsonGreeting :: Data.Argonaut.Encode.Class.EncodeJson Greeting where
  encodeJson (Greeting { name, title }) =
    Data.Argonaut.Encode.Combinators.extend
      (Data.Argonaut.Encode.Combinators.assoc "name" name)
      ( Data.Argonaut.Encode.Combinators.extendOptional
          (Data.Argonaut.Encode.Combinators.assocOptional "title" title)
          (Data.Argonaut.Core.jsonEmptyObject)
      )

parse'' ::
  String ->
  Data.Either.Either String Greeting
parse'' string = case Data.Argonaut.Parser.jsonParser string of
  Data.Either.Left error -> Data.Either.Left error
  Data.Either.Right json -> case decode json of
    Data.Either.Left error -> Data.Either.Left (Data.Argonaut.Decode.Error.printJsonDecodeError error)
    Data.Either.Right greeting -> Data.Either.Right greeting
```

If we try decoding and encoding now, we get something closer to what we wanted:

```PureScript
> parse'' """{}"""
(Left "An error occurred while decoding a JSON value:\n  At object key 'name':\n  No value was found.")

> parse'' """{"title": "wonderful"}"""
(Left "An error occurred while decoding a JSON value:\n  At object key 'name':\n  No value was found.")

> parse'' """{"name": "Pat"}"""
(Right (Greeting { name: "Pat", title: Nothing }))

> parse'' """{"name": "Pat", "title": "Dr."}"""
(Right (Greeting { name: "Pat", title: (Just "Dr.") }))

> Data.Argonaut.Core.stringify (Data.Argonaut.Encode.Class.encodeJson (Greeting { name: "Pat", title: Data.Maybe.Nothing }))
"{\"name\":\"Pat\"}"

> Data.Argonaut.Core.stringify (Data.Argonaut.Encode.Class.encodeJson (Greeting { name: "Pat", title: Data.Maybe.Just "Dr." }))
"{\"title\":\"Dr.\",\"name\":\"Pat\"}"
```

## How To: Decode and Encode JSON with required and optional values in `purescript-codec-argonaut`

Another common pattern with JSON objects is that some keys always have to be present while others do not.
Some APIs make the distinction between a JSON object like `{ "name": "Pat" }` and one like `{ "name": "Pat", "title": null }`.
In the first case, it might recognize that the `"title"` key does not exist, and behave in a different way from the `"title"` key having a value of `null`.
In the second case, it might notice that the `"title"` key exists and work with the value assuming it's good to go; the `null` might eventually cause a failure later.

In many cases, what we want is to not generate any fields that do not exist.
Using `purescript-codec-argonaut`, `Option.Record _ _` can help with that idea:

```PureScript
import Data.Codec.Argonaut as Data.Codec.Argonaut
import Option as Option

jsonCodec :: Data.Codec.Argonaut.JsonCodec (Option.Record ( name :: String ) ( title :: String ))
jsonCodec =
  Option.jsonCodecRecord
    "Greeting"
    { name: Data.Codec.Argonaut.string
    , title: Data.Codec.Argonaut.string
    }
```

We can add a couple of helpers to make decoding/encoding easier in the REPL:

```PureScript
import Prelude
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Argonaut.Parser as Data.Argonaut.Parser
import Data.Codec.Argonaut as Data.Codec.Argonaut
import Data.Either as Data.Either
import Option as Option

decode ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Option.Record ( name :: String ) ( title :: String ))
decode = Data.Codec.Argonaut.decode jsonCodec

encode ::
  Option.Record ( name :: String ) ( title :: String ) ->
  Data.Argonaut.Core.Json
encode = Data.Codec.Argonaut.encode jsonCodec

parse ::
  String ->
  Data.Either.Either String (Option.Record ( name :: String ) ( title :: String ))
parse string = do
  json <- Data.Argonaut.Parser.jsonParser string
  case decode json of
    Data.Either.Left err -> Data.Either.Left (Data.Codec.Argonaut.printJsonDecodeError err)
    Data.Either.Right record -> Data.Either.Right record
```

We can give that a spin with some different JSON values:

```PureScript
> parse """{}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key name:\n  No value was found.")

> parse """{"title": "wonderful"}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key name:\n  No value was found.")

> parse """{"name": "Pat"}"""
(Right (Option.recordFromRecord { name: "Pat" }))

> parse """{"name": "Pat", "title": "Dr."}"""
(Right (Option.recordFromRecord { name: "Pat", title: "Dr." }))

> parse """{ "name": "Pat", "title": null }"""
Right (Option.recordFromRecord { name: "Pat" })
```

Notice that we have to supply a `"name"` field in the JSON input otherwise it will not parse.

We can also produce some different JSON values:

```PureScript
> Data.Argonaut.Core.stringify (encode (Option.recordFromRecord { name: "Pat" }))
"{\"name\":\"Pat\"}"

> Data.Argonaut.Core.stringify (encode (Option.recordFromRecord { name: "Pat", title: "Dr." }))
"{\"name\":\"Pat\",\"title\":\"Dr.\"}"
```

Notice that we don't end up with a `"title"` field in the JSON output unless we have a `title` field in our record.

It might be instructive to compare how we might write a similar functions using a `Record _` instead of `Option.Record _`:
With `purescript-codec-argonaut`, there are a couple of codecs that ship with the package for records: `Data.Codec.Argonaut.recordProp` and `Data.Codec.Argonaut.Record.record`.
Each of those codecs expect the field to always exist no matter its value.
The difference between those codecs is not very relevant except to say that the latter requires less characters to use.
There are also a couple of codecs that ship with the package for `Data.Maybe.Maybe _`: `Data.Codec.Argonaut.Common.maybe` and `Data.Codec.Argonaut.Compat.maybe`.
The former decodes/encodes with tagged values, the latter with `null`s.
If we attempt to go directly to `Record ( name :: String, title :: Data.Maybe.Maybe String )` and `Data.Codec.Argonaut.Common.maybe`:

```PureScript
import Prelude
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Argonaut.Parser as Data.Argonaut.Parser
import Data.Codec.Argonaut as Data.Codec.Argonaut
import Data.Codec.Argonaut.Common as Data.Codec.Argonaut.Common
import Data.Codec.Argonaut.Compat as Data.Codec.Argonaut.Compat
import Data.Codec.Argonaut.Record as Data.Codec.Argonaut.Record
import Data.Either as Data.Either
import Data.Maybe as Data.Maybe

decode' ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Record ( name :: String, title :: Data.Maybe.Maybe String ))
decode' = Data.Codec.Argonaut.decode jsonCodec'

encode' ::
  Record ( name :: String, title :: Data.Maybe.Maybe String ) ->
  Data.Argonaut.Core.Json
encode' = Data.Codec.Argonaut.encode jsonCodec'

jsonCodec' :: Data.Codec.Argonaut.JsonCodec (Record ( name :: String, title :: Data.Maybe.Maybe String ))
jsonCodec' =
  Data.Codec.Argonaut.Record.object
    "Greeting"
    { name: Data.Codec.Argonaut.string
    , title: Data.Codec.Argonaut.Common.maybe Data.Codec.Argonaut.string
    }

parse' ::
  String ->
  Data.Either.Either String (Record ( name :: String, title :: Data.Maybe.Maybe String ))
parse' string = do
  json <- Data.Argonaut.Parser.jsonParser string
  case decode' json of
    Data.Either.Left err -> Data.Either.Left (Data.Codec.Argonaut.printJsonDecodeError err)
    Data.Either.Right option -> Data.Either.Right option
```

We won't get the behavior we expect:

```PureScript
> parse' """{}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  No value was found.")

> parse' """{"title": "wonderful"}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  Under 'Maybe':\n  Expected value of type 'Object'.")

> parse' """{"name": "Pat"}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  No value was found.")

> parse' """{"name": "Pat", "title": "Dr."}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  Under 'Maybe':\n  Expected value of type 'Object'.")

> parse' """{"name": "Pat", "title": {"tag": "Just", "value": "Dr."}}"""
(Right { name: "Pat", title: (Just "Dr.") })

> Data.Argonaut.Core.stringify (encode' { name: "Pat", title: Data.Maybe.Nothing })
"{\"name\":\"Pat\",\"title\":{\"tag\":\"Nothing\"}}"

> Data.Argonaut.Core.stringify (encode' { name: "Pat", title: Data.Maybe.Just "Dr." })
"{\"name\":\"Pat\",\"title\":{\"tag\":\"Just\",\"value\":\"Dr.\"}}"
```

Unless both fields exist, we cannot decode the JSON object.
Not only is every field required, they're serialized as tagged values.
Similarly, no matter what the optional values are, we always encode them into a JSON object.

If we try with `Data.Codec.Argonaut.Compat.maybe`:

```PureScript
decode'' ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Record ( name :: String, title :: Data.Maybe.Maybe String ))
decode'' = Data.Codec.Argonaut.decode jsonCodec''

encode'' ::
  Record ( name :: String, title :: Data.Maybe.Maybe String ) ->
  Data.Argonaut.Core.Json
encode'' = Data.Codec.Argonaut.encode jsonCodec''

jsonCodec'' :: Data.Codec.Argonaut.JsonCodec (Record ( name :: String, title :: Data.Maybe.Maybe String ))
jsonCodec'' =
  Data.Codec.Argonaut.Record.object
    "Greeting"
    { name: Data.Codec.Argonaut.string
    , title: Data.Codec.Argonaut.Compat.maybe Data.Codec.Argonaut.string
    }

parse'' ::
  String ->
  Data.Either.Either String (Record ( name :: String, title :: Data.Maybe.Maybe String ))
parse'' string = do
  json <- Data.Argonaut.Parser.jsonParser string
  case decode'' json of
    Data.Either.Left err -> Data.Either.Left (Data.Codec.Argonaut.printJsonDecodeError err)
    Data.Either.Right option -> Data.Either.Right option
```

We also don't get the behavior we expect:

```PureScript
> parse'' """{}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  No value was found.")

> parse'' """{"title": "wonderful"}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key name:\n  No value was found.")

> parse'' """{"name": "Pat"}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key title:\n  No value was found.")

> parse'' """{"name": "Pat", "title": "Dr."}"""
(Right { name: "Pat", title: (Just "Dr.") })

> Data.Argonaut.Core.stringify (encode'' { name: "Pat", title: Data.Maybe.Nothing })
"{\"name\":\"Pat\",\"title\":null}"

> Data.Argonaut.Core.stringify (encode'' { name: "Pat", title: Data.Maybe.Just "Dr." })
"{\"name\":\"Pat\",\"title\":\"Dr.\"}"
```

Unless both fields exist, we cannot decode the JSON object.
Similarly, no matter what the optional values are, we always encode them into a JSON object.

In order to emulate the behavior of an optional field, we have to use a different codec:

```PureScript
import Prelude
import Control.Monad.Reader.Trans as Control.Monad.Reader.Trans
import Control.Monad.Writer as Control.Monad.Writer
import Control.Monad.Writer.Class as Control.Monad.Writer.Class
import Data.Codec as Data.Codec
import Data.List as Data.List
import Data.Profunctor.Star as Data.Profunctor.Star
import Data.Tuple as Data.Tuple
import Foreign.Object as Foreign.Object
import Prim.Row as Prim.Row
import Record as Record
import Type.Proxy as Type.Proxy

optionalField ::
  forall label record record' value.
  Data.Symbol.IsSymbol label =>
  Prim.Row.Cons label (Data.Maybe.Maybe value) record' record =>
  Prim.Row.Lacks label record' =>
  Type.Proxy.Proxy label ->
  Data.Codec.Argonaut.JsonCodec value ->
  Data.Codec.Argonaut.JPropCodec (Record record') ->
  Data.Codec.Argonaut.JPropCodec (Record record)
optionalField label codecValue codecRecord =
  Data.Codec.GCodec
    (Control.Monad.Reader.Trans.ReaderT decodeField)
    (Data.Profunctor.Star.Star encodeField)
  where
  decodeField ::
    Foreign.Object.Object Data.Argonaut.Core.Json ->
    Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Record record)
  decodeField object' = do
    record <- Data.Codec.Argonaut.decode codecRecord object'
    case Foreign.Object.lookup key object' of
      Data.Maybe.Just json -> case Data.Codec.Argonaut.decode codecValue json of
        Data.Either.Left error -> Data.Either.Left (Data.Codec.Argonaut.AtKey key error)
        Data.Either.Right value -> Data.Either.Right (Record.insert label (Data.Maybe.Just value) record)
      Data.Maybe.Nothing -> Data.Either.Right (Record.insert label Data.Maybe.Nothing record)

  encodeField ::
    Record record ->
    Control.Monad.Writer.Writer (Data.List.List (Data.Tuple.Tuple String Data.Argonaut.Core.Json)) (Record record)
  encodeField record = do
    case Record.get label record of
      Data.Maybe.Just value ->
        Control.Monad.Writer.Class.tell
          ( Data.List.Cons
              (Data.Tuple.Tuple key (Data.Codec.Argonaut.encode codecValue value))
              Data.List.Nil
          )
      Data.Maybe.Nothing -> pure unit
    Control.Monad.Writer.Class.tell
      (Data.Codec.Argonaut.encode codecRecord (Record.delete label record))
    pure record

  key :: String
  key = Data.Symbol.reflectSymbol label
```

With this codec defined, we can implement a codec for the record with required and optional fields:

```PureScript
decode''' ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Record ( name :: String, title :: Data.Maybe.Maybe String ))
decode''' = Data.Codec.Argonaut.decode jsonCodec'''

encode''' ::
  Record ( name :: String, title :: Data.Maybe.Maybe String ) ->
  Data.Argonaut.Core.Json
encode''' = Data.Codec.Argonaut.encode jsonCodec'''

jsonCodec''' :: Data.Codec.Argonaut.JsonCodec (Record ( name :: String, title :: Data.Maybe.Maybe String ))
jsonCodec''' =
  Data.Codec.Argonaut.object
    "Greeting"
    ( Data.Codec.Argonaut.recordProp (Type.Proxy.Proxy :: _ "name") Data.Codec.Argonaut.string
        $ optionalField (Type.Proxy.Proxy :: _ "title") Data.Codec.Argonaut.string
        $ Data.Codec.Argonaut.record
    )

parse''' ::
  String ->
  Data.Either.Either String (Record ( name :: String, title :: Data.Maybe.Maybe String ))
parse''' string = do
  json <- Data.Argonaut.Parser.jsonParser string
  case decode''' json of
    Data.Either.Left err -> Data.Either.Left (Data.Codec.Argonaut.printJsonDecodeError err)
    Data.Either.Right option -> Data.Either.Right option
```

If we try decoding and encoding now, we get something closer to what we wanted:

```PureScript
> parse''' """{}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key name:\n  No value was found.")

> parse''' """{"title": "wonderful"}"""
(Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key name:\n  No value was found.")

> parse''' """{"name": "Pat"}"""
(Right { name: "Pat", title: Nothing })

> parse''' """{"name": "Pat", "title": "wonderful"}"""
(Right { name: "Pat", title: (Just "wonderful") })

> Data.Argonaut.Core.stringify (encode''' { name: "Pat", title: Data.Maybe.Nothing })
"{\"name\":\"Pat\"}"

> Data.Argonaut.Core.stringify (encode''' { name: "Pat", title: Data.Maybe.Just "Dr." })
"{\"name\":\"Pat\",\"title\":\"Dr.\"}"
```

## How To: Decode and Encode JSON with required and optional values in `purescript-simple-json`

Another common pattern with JSON objects is that some keys always have to be present while others do not.
Some APIs make the distinction between a JSON object like `{ "name": "Pat" }` and one like `{ "name": "Pat", "title": null }`.
In the first case, it might recognize that the `"title"` key does not exist, and behave in a different way from the `"title"` key having a value of `null`.
In the second case, it might notice that the `"title"` key exists and work with the value assuming it's good to go; the `null` might eventually cause a failure later.

In many cases, what we want is to not generate any fields that do not exist.
Using `purescript-simple-json`, `Option.Record _ _` can help with that idea:

```PureScript
import Prelude
import Data.Either as Data.Either
import Data.Semigroup.Foldable as Data.Semigroup.Foldable
import Foreign as Foreign
import Option as Option
import Simple.JSON as Simple.JSON

parse ::
  String ->
  Data.Either.Either String (Option.Record ( name :: String ) ( title :: String ))
parse string = case readJSON string of
  Data.Either.Left errors -> Data.Either.Left (Data.Semigroup.Foldable.intercalateMap " " Foreign.renderForeignError errors)
  Data.Either.Right record -> Data.Either.Right record

readJSON ::
  String ->
  Simple.JSON.E (Option.Record ( name :: String ) ( title :: String ))
readJSON = Simple.JSON.readJSON

writeJSON ::
  Option.Record ( name :: String ) ( title :: String ) ->
  String
writeJSON = Simple.JSON.writeJSON
```

We can give that a spin with some different JSON values:

```PureScript
> parse """{}"""
(Left "Error at property \"name\": Type mismatch: expected String, found Undefined")

> parse """{"title": "wonderful"}"""
(Left "Error at property \"name\": Type mismatch: expected String, found Undefined")

> parse """{"name": "Pat"}"""
(Right (Option.recordFromRecord { name: "Pat" }))

> parse """{"name": "Pat", "title": "Dr."}"""
(Right (Option.recordFromRecord { name: "Pat", title: "Dr." }))

> parse """{ "name": "Pat", "title": null }"""
Right (Option.recordFromRecord { name: "Pat" })
```

We can also produce some different JSON values:

```PureScript
> writeJSON (Option.recordFromRecord {name: "Pat"})
"{\"name\":\"Pat\"}"

> writeJSON (Option.recordFromRecord {name: "Pat", title: "Dr."})
"{\"title\":\"Dr.\",\"name\":\"Pat\"}"
```

Notice that we don't end up with a `"title"` field in the JSON output unless we have a `title` field in our record.

It might be instructive to compare how we might write a similar functions using a `Record _` instead of `Option.Record _ _`:
With `purescript-simple-json`, the instances for decoding and encoding on records handle `Data.Maybe.Maybe _` values like they are optional.
If we attempt to go directly to `Record ( name :: String, title :: Data.Maybe.Maybe String )`:

```PureScript
import Prelude
import Data.Either as Data.Either
import Data.Maybe as Data.Maybe
import Data.Semigroup.Foldable as Data.Semigroup.Foldable
import Foreign as Foreign
import Simple.JSON as Simple.JSON

parse' ::
  String ->
  Data.Either.Either String (Record ( name :: String, title :: Data.Maybe.Maybe String ))
parse' string = case readJSON string of
  Data.Either.Left errors -> Data.Either.Left (Data.Semigroup.Foldable.intercalateMap " " Foreign.renderForeignError errors)
  Data.Either.Right record -> Data.Either.Right record

readJSON' ::
  String ->
  Simple.JSON.E (Record ( name :: String, title :: Data.Maybe.Maybe String ))
readJSON' = Simple.JSON.readJSON

writeJSON' ::
  Record ( name :: String, title :: Data.Maybe.Maybe String ) ->
  String
writeJSON' = Simple.JSON.writeJSON
```

We get the behavior we expect:

```PureScript
> parse' """{}"""
(Left "Error at property \"name\": Type mismatch: expected String, found Undefined")

> parse' """{"title": "wonderful"}"""
(Left "Error at property \"name\": Type mismatch: expected String, found Undefined")

> parse' """{"name": "Pat"}"""
(Right { name: "Pat", title: Nothing })

> parse' """{"name": "Pat", "title": "Dr."}"""
(Right { name: "Pat", title: (Just "Dr.") })

> writeJSON' {name: "Pat", title: Data.Maybe.Nothing}
"{\"name\":\"Pat\"}"

> writeJSON' {name: "Pat", title: Data.Maybe.Just "Dr."}
"{\"title\":\"Dr.\",\"name\":\"Pat\"}"
```

## How To: Provide an easier API for `DateTime`

The API for `Data.DateTime` is pretty nice because it means we cannot construct invalid dates.
What's not so nice about it is that it pushes all of the correctness onto us.
It might be a little easier to use if the API would allow optional values to be passed in and default to something sensible.
For instance, constructing a `Data.DateTime.DateTime` can be done by passing in both a `Data.Date.Date` and a `Data.Time.Time`:
The issue is, how do we construct a `Data.Date.Date` or `Data.Time.Time`.

One way to get construct these values is to use the `Data.Enum.Enum` instance for both of them:

```PureScript
> Data.DateTime.DateTime bottom bottom
(DateTime (Date (Year -271820) January (Day 1)) (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
```

This gives us a value, but some of its parts might not be what we really want; e.g. the year is `-271820`.

If we wanted to alter the year, we have to use `Data.Enum.toEnum` to construct a different year, then `Data.DateTime.modifyDate`, and `Data.Date.canonicalDate` to thread the year through:

```PureScript
> Data.DateTime.modifyDate (\date -> Data.Date.canonicalDate (Data.Maybe.fromMaybe bottom (Data.Enum.toEnum 2019)) (Data.Date.month date) (Data.Date.day date)) (Data.DateTime.DateTime bottom bottom)
(DateTime (Date (Year 2019) January (Day 1)) (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
```

Or create it with the correct year from the get-go:

```PureScript
> Data.DateTime.DateTime (Data.Date.canonicalDate (Data.Maybe.fromMaybe bottom (Data.Enum.toEnum 2019)) bottom bottom) bottom
(DateTime (Date (Year 2019) January (Day 1)) (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
```

That's a non-trivial amount of work in order to alter the year.
We can clean it up with a named function that takes in an `Int` for the year and does all the boilerplate (using `Data.Enum.toEnumWithDefaults` to handle bounds a bit better):

```PureScript
dateTimeFromYear :: Int -> Data.DateTime.DateTime
dateTimeFromYear year =
  Data.DateTime.DateTime
    ( Data.Date.canonicalDate
        (Data.Enum.toEnumWithDefaults bottom top year)
        bottom
        bottom
    )
    bottom
```

This works decently for the year alone.

```PureScript
> dateTimeFromYear 2019
(DateTime (Date (Year 2019) January (Day 1)) (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
```

Once we decide we are okay with the year, but want to alter the day instead, or that we want to alter both at the same time it becomes just as hard as if we hadn't done anything.
We either need to implement something similar for each part or change the arguments to `Data.Maybe.Maybe Int`s.

An alternative is to use an option for each part of the `Data.DateTime.DateTime`:

```PureScript
type Option
  = ( day :: Int
    , hour :: Int
    , millisecond :: Int
    , minute :: Int
    , month :: Data.Date.Component.Month
    , second :: Int
    , year :: Int
    )
```

Then we can build a `Data.DateTime.DateTime` from whatever happens to be passed in:

```PureScript
dateTime ::
  forall record.
  Option.FromRecord record Option =>
  Record record ->
  Data.DateTime.DateTime
dateTime record = Data.DateTime.DateTime date time
  where
  date :: Data.Date.Date
  date = Data.Date.canonicalDate year month day
    where
    day :: Data.Date.Component.Day
    day = get (Type.Proxy.Proxy :: _ "day")

    month :: Data.Date.Component.Month
    month = Option.getWithDefault bottom (Type.Proxy.Proxy :: _ "month") options

    year :: Data.Date.Component.Year
    year = get (Type.Proxy.Proxy :: _ "year")

  get ::
    forall label proxy record' value.
    Data.Enum.BoundedEnum value =>
    Data.Symbol.IsSymbol label =>
    Prim.Row.Cons label Int record' Option =>
    proxy label ->
    value
  get proxy = case Option.get proxy options of
    Data.Maybe.Just x -> Data.Enum.toEnumWithDefaults bottom top x
    Data.Maybe.Nothing -> bottom

  options :: Option.Option Option
  options = Option.fromRecord record

  time :: Data.Time.Time
  time = Data.Time.Time hour minute second millisecond
    where
    hour :: Data.Time.Component.Hour
    hour = get (Type.Proxy.Proxy :: _ "hour")

    minute :: Data.Time.Component.Minute
    minute = get (Type.Proxy.Proxy :: _ "minute")

    millisecond :: Data.Time.Component.Millisecond
    millisecond = get (Type.Proxy.Proxy :: _ "millisecond")

    second :: Data.Time.Component.Second
    second = get (Type.Proxy.Proxy :: _ "second")
```

Now, we can construct a `Data.DateTime.DateTime` fairly easily:

```PureScript
> dateTime {}
(DateTime (Date (Year -271820) January (Day 1)) (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
```

We can alter the year:

```PureScript
> dateTime {year: 2019}
(DateTime (Date (Year 2019) January (Day 1)) (Time (Hour 0) (Minute 0) (Second 0) (Millisecond 0)))
```

And, we can alter any of the components:

```PureScript
> dateTime {minute: 30, month: Data.Date.Component.April, year: 2019}
(DateTime (Date (Year 2019) April (Day 1)) (Time (Hour 0) (Minute 30) (Second 0) (Millisecond 0)))
```

## Reference: `FromRecord _ _ _`

A typeclass for converting a `Record _` into an `Option _`.

An instance `FromRecord record required optional` states that we can make a `Record required` and an `Option optional` from a `Record record` where every required field is in the record and the rest of the present fields in the record is present in the option.
E.g. `FromRecord () () ( name :: String )` says that the `Record ()` has no fields and the `Option ( name :: String )` will have no value;
`FromRecord ( name :: String ) () ( name :: String )` says that the `Record ()` has no fields and the `Option ( name :: String )` will have the given `name` value;
`FromRecord ( name :: String ) ( name :: String ) ()` says that the `Record ( name :: String )` has the given `name` value and the `Option ()` will have no value;
`FromRecord () ( name :: String) ()` is a type error since the `name` field is required but the given record lacks the field.

Since there is syntax for creating records, but no syntax for creating options, this typeclass can be useful for providing an easier to use interface to options.

E.g. Someone can say:

```PureScript
Option.fromRecord' { foo: true, bar: 31 }
```

Instead of having to say:

```PureScript
Option.insert
  (Type.Proxy.Proxy :: _ "foo")
  true
  ( Option.insert
      (Type.Proxy.Proxy :: _ "bar")
      31
      Option.empty
  )
```

Not only does it save a bunch of typing, it also mitigates the need for a direct dependency on `SProxy _`.
