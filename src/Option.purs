-- | There are a few different data types that encapsulate ideas in programming.
-- |
-- | Records capture the idea of a collection of key/value pairs where every key and value exist.
-- | E.g. `Record (foo :: Boolean, bar :: Int)` means that both `foo` and `bar` exist and with values all of the time.
-- |
-- | Variants capture the idea of a collection of key/value pairs where exactly one of the key/value pairs exist.
-- | E.g. `Variant (foo :: Boolean, bar :: Int)` means that either only `foo` exists with a value or only `bar` exists with a value, but not both at the same time.
-- |
-- | Options capture the idea of a collection of key/value pairs where any key and value may or may not exist.
-- | E.g. `Option (foo :: Boolean, bar :: Int)` means that either only `foo` exists with a value, only `bar` exists with a value, both `foo` and `bar` exist with values, or neither `foo` nor `bar` exist.
-- |
-- | The distinction between these data types means that we can describe problems more accurately.
-- | Options are typically what you find in dynamic languages or in weakly-typed static languages.
-- | Their use cases range from making APIs more flexible to interfacing with serialization formats to providing better ergonomics around data types.
module Option
  ( Option
  , fromRecord
  , fromRecordWithRequired
  , delete
  , delete'
  , empty
  , get
  , get'
  , getAll
  , getWithDefault
  , insert
  , insert'
  , jsonCodec
  , modify
  , modify'
  , set
  , set'
  , toRecord
  , class DecodeJsonOption
  , decodeJsonOption
  , class Delete
  , delete''
  , class DeleteOption
  , deleteOption
  , class EncodeJsonOption
  , encodeJsonOption
  , class EqOption
  , eqOption
  , class FromRecord
  , fromRecord'
  , class FromRecordOption
  , fromRecordOption
  , class FromRecordRequired
  , fromRecordRequired
  , class Get
  , get''
  , class GetOption
  , getOption
  , class GetAll
  , getAll'
  , class GetAllOption
  , getAllOption
  , class Insert
  , insert''
  , class InsertOption
  , insertOption
  , class JsonCodec
  , jsonCodec'
  , class JsonCodecOption
  , jsonCodecOption
  , class Modify
  , modify''
  , class ModifyOption
  , modifyOption
  , class OrdOption
  , compareOption
  , class ReadForeignOption
  , readImplOption
  , class Set
  , set''
  , class SetOption
  , setOption
  , class ShowOption
  , showOption
  , class ToRecord
  , toRecord'
  , class ToRecordOption
  , toRecordOption
  , class WriteForeignOption
  , writeForeignOption
  ) where

import Prelude
import Control.Monad.Except as Control.Monad.Except
import Control.Monad.Reader.Trans as Control.Monad.Reader.Trans
import Control.Monad.Writer as Control.Monad.Writer
import Control.Monad.Writer.Class as Control.Monad.Writer.Class
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Argonaut.Decode.Class as Data.Argonaut.Decode.Class
import Data.Argonaut.Encode.Class as Data.Argonaut.Encode.Class
import Data.Argonaut.Encode.Combinators as Data.Argonaut.Encode.Combinators
import Data.Codec as Data.Codec
import Data.Codec.Argonaut as Data.Codec.Argonaut
import Data.Either as Data.Either
import Data.List as Data.List
import Data.Maybe as Data.Maybe
import Data.Profunctor.Star as Data.Profunctor.Star
import Data.Symbol as Data.Symbol
import Data.Tuple as Data.Tuple
import Foreign as Foreign
import Foreign.Index as Foreign.Index
import Foreign.Object as Foreign.Object
import Prim.Row as Prim.Row
import Prim.RowList as Prim.RowList
import Record as Record
import Record.Builder as Record.Builder
import Simple.JSON as Simple.JSON
import Type.Equality as Type.Equality
import Unsafe.Coerce as Unsafe.Coerce

-- | A collection of key/value pairs where any key and value may or may not exist.
-- | E.g. `Option (foo :: Boolean, bar :: Int)` means that either only `foo` exists with a value, only `bar` exists with a value, both `foo` and `bar` exist with values, or neither `foo` nor `bar` exist.
newtype Option (row :: # Type)
  = Option (Foreign.Object.Object (forall a. a))

-- A local proxy for `Prim.RowList.RowList` so as not to impose a hard requirement on `Type.Data.RowList.RLProxy` in the typeclasses we define.
-- `Type.Data.RowList.RLProxy` can still be used by callers, but it's not a requirement.
data Proxy (list :: Prim.RowList.RowList)
  = Proxy

-- | This instance ignores keys that do not exist in the given JSON object.
-- |
-- | If a key does not exist in the JSON object, it will not be added to the `Option _`.
-- |
-- | If a key does exists in the JSON object but the value cannot be successfully decoded, it will fail with an error.
-- |
-- | If a key does exists in the JSON object and the value can be successfully decoded, it will be added to the `Option _`.
instance decodeJsonOptionOption ::
  ( DecodeJsonOption list option
  , Prim.RowList.RowToList option list
  ) =>
  Data.Argonaut.Decode.Class.DecodeJson (Option option) where
  decodeJson ::
    Data.Argonaut.Core.Json ->
    Data.Either.Either String (Option option)
  decodeJson json = case Data.Argonaut.Core.toObject json of
    Data.Maybe.Just object -> decodeJsonOption (Proxy :: Proxy list) object
    Data.Maybe.Nothing -> Data.Either.Left "Expected JSON object"

-- | This instance ignores keys that do not exist.
-- |
-- | If a key does not exist in the given `Option _`, it is not added to the JSON object.
-- |
-- | If a key does exists in the given `Option _`, it encodes it like normal and adds it to the JSON object.
instance encodeJsonOptionOption ::
  ( EncodeJsonOption list option
  , Prim.RowList.RowToList option list
  ) =>
  Data.Argonaut.Encode.Class.EncodeJson (Option option) where
  encodeJson ::
    Option option ->
    Data.Argonaut.Core.Json
  encodeJson = encodeJsonOption (Proxy :: Proxy list)

instance eqOptionOption ::
  ( EqOption list option
  , Prim.RowList.RowToList option list
  ) =>
  Eq (Option option) where
  eq = eqOption (Proxy :: Proxy list)

instance ordOptionOption ::
  ( OrdOption list option
  , Prim.RowList.RowToList option list
  ) =>
  Ord (Option option) where
  compare = compareOption (Proxy :: Proxy list)

-- | This instance ignores keys that do not exist in the given `Foreign`.
-- |
-- | If a key does not exist in the `Foreign`, it will not be added to the `Option _`.
-- |
-- | If a key does exists in the `Foreign` but the value cannot be successfully read, it will fail with an error.
-- |
-- | If a key does exists in the `Foreign` and the value can be successfully read, it will be added to the `Option _`.
instance readForeignOptionOption ::
  ( Prim.RowList.RowToList option list
  , ReadForeignOption list option
  ) =>
  Simple.JSON.ReadForeign (Option option) where
  readImpl ::
    Foreign.Foreign ->
    Foreign.F (Option option)
  readImpl = readImplOption (Proxy :: Proxy list)

instance showOptionOption ::
  ( Prim.RowList.RowToList option list
  , ShowOption list option
  ) =>
  Show (Option option) where
  show ::
    Option option ->
    String
  show option = "(Option.fromRecord {" <> go fields <> "})"
    where
    fields :: Data.List.List String
    fields = showOption proxy option

    go :: Data.List.List String -> String
    go x' = case x' of
      Data.List.Cons x Data.List.Nil -> " " <> x <> " "
      Data.List.Cons x y -> " " <> go' x y <> " "
      Data.List.Nil -> ""

    go' :: String -> Data.List.List String -> String
    go' acc x' = case x' of
      Data.List.Cons x y -> go' (acc <> ", " <> x) y
      Data.List.Nil -> acc

    proxy :: Proxy list
    proxy = Proxy

-- | This instance ignores keys that do not exist.
-- |
-- | If a key does not exist in the given `Option _`, it is not added to the `Foreign`.
-- |
-- | If a key does exists in the given `Option _`, it writes it like normal and adds it to the `Foreign`.
instance writeForeignOptionOption ::
  ( Prim.RowList.RowToList option list
  , WriteForeignOption list option
  ) =>
  Simple.JSON.WriteForeign (Option option) where
  writeImpl ::
    Option option ->
    Foreign.Foreign
  writeImpl = writeForeignOption (Proxy :: Proxy list)

-- | A typeclass that iterates a `RowList` decoding an `Object Json` to an `Option _`.
class DecodeJsonOption (list :: Prim.RowList.RowList) (option :: # Type) | list -> option where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  decodeJsonOption ::
    forall proxy.
    proxy list ->
    Foreign.Object.Object Data.Argonaut.Core.Json ->
    Data.Either.Either String (Option option)

instance decodeJsonOptionNil :: DecodeJsonOption Prim.RowList.Nil option where
  decodeJsonOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Foreign.Object.Object Data.Argonaut.Core.Json ->
    Data.Either.Either String (Option option)
  decodeJsonOption _ _ = Data.Either.Right empty
else instance decodeJsonOptionCons ::
  ( Data.Argonaut.Decode.Class.DecodeJson value
  , Data.Symbol.IsSymbol label
  , DecodeJsonOption list option
  , Prim.Row.Cons label value option' option
  ) =>
  DecodeJsonOption (Prim.RowList.Cons label value list) option where
  decodeJsonOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Foreign.Object.Object Data.Argonaut.Core.Json ->
    Data.Either.Either String (Option option)
  decodeJsonOption _ object' = case Foreign.Object.lookup key object' of
    Data.Maybe.Just json -> do
      value <- Data.Argonaut.Decode.Class.decodeJson json
      option <- option'
      Data.Either.Right (set label value option)
    Data.Maybe.Nothing -> do
      Option object <- option'
      Data.Either.Right (Option object)
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    key :: String
    key = Data.Symbol.reflectSymbol label

    option' :: Data.Either.Either String (Option option)
    option' = decodeJsonOption proxy object'

    proxy :: Proxy list
    proxy = Proxy

-- | A typeclass that removes keys from an option
-- |
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.fromRecord { foo: true, bar: 31 }
-- |
-- | anotherOption :: Option.Option ( bar :: Int )
-- | anotherOption = Option.delete'' { foo: unit } someOption
-- | ```
class Delete (record :: # Type) (option' :: # Type) (option :: # Type) | record option' -> option, record option -> option', option' option -> record where
  delete'' ::
    Record record ->
    Option option' ->
    Option option

-- | This instance removes keys from an `Option _`.
instance deleteAny ::
  ( DeleteOption list record option' option
  , Prim.RowList.RowToList record list
  ) =>
  Delete record option' option where
  delete'' ::
    Record record ->
    Option option' ->
    Option option
  delete'' = deleteOption (Proxy :: Proxy list)

-- | A typeclass that iterates a `Prim.RowList.RowList` removing keys from `Option _`.
class DeleteOption (list :: Prim.RowList.RowList) (record :: # Type) (option' :: # Type) (option :: # Type) | list option' -> option, list option -> option' where
  deleteOption ::
    forall proxy.
    proxy list ->
    Record record ->
    Option option' ->
    Option option

instance deleteOptionNil ::
  DeleteOption Prim.RowList.Nil record option option where
  deleteOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Record record ->
    Option option ->
    Option option
  deleteOption _ _ option = option
else instance deleteOptionCons ::
  ( Data.Symbol.IsSymbol label
  , DeleteOption list record oldOption' option
  , Prim.Row.Cons label value oldOption' oldOption
  , Prim.Row.Lacks label oldOption'
  ) =>
  DeleteOption (Prim.RowList.Cons label Unit list) record oldOption option where
  deleteOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label Unit list) ->
    Record record ->
    Option oldOption ->
    Option option
  deleteOption _ record option' = deleteOption proxy record option
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    option :: Option oldOption'
    option = delete label option'

    proxy :: Proxy list
    proxy = Proxy

-- | A typeclass that iterates a `RowList` encoding an `Option _` as `Json`.
class EncodeJsonOption (list :: Prim.RowList.RowList) (option :: # Type) | list -> option where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  encodeJsonOption ::
    forall proxy.
    proxy list ->
    Option option ->
    Data.Argonaut.Core.Json

instance encodeJsonOptionNil ::
  EncodeJsonOption Prim.RowList.Nil option where
  encodeJsonOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Option option ->
    Data.Argonaut.Core.Json
  encodeJsonOption _ _ = Data.Argonaut.Core.jsonEmptyObject
else instance encodeJsonOptionCons ::
  ( Data.Argonaut.Encode.Class.EncodeJson value
  , Data.Symbol.IsSymbol label
  , EncodeJsonOption list option
  , Prim.Row.Cons label value option' option
  ) =>
  EncodeJsonOption (Prim.RowList.Cons label value list) option where
  encodeJsonOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Option option ->
    Data.Argonaut.Core.Json
  encodeJsonOption _ option = case value' of
    Data.Maybe.Just value ->
      Data.Argonaut.Encode.Combinators.extend
        ( Data.Argonaut.Encode.Combinators.assoc
            key
            (Data.Argonaut.Encode.Class.encodeJson value)
        )
        json
    Data.Maybe.Nothing -> json
    where
    json :: Data.Argonaut.Core.Json
    json = encodeJsonOption proxy option

    key :: String
    key = Data.Symbol.reflectSymbol label

    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    proxy :: Proxy list
    proxy = Proxy

    value' :: Data.Maybe.Maybe value
    value' = get label option

-- | A typeclass that iterates a `RowList` converting an `Option _` to a `Boolean`.
class EqOption (list :: Prim.RowList.RowList) (option :: # Type) | list -> option where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  eqOption ::
    forall proxy.
    proxy list ->
    Option option ->
    Option option ->
    Boolean

instance eqOptionNil :: EqOption Prim.RowList.Nil option where
  eqOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Option option ->
    Option option ->
    Boolean
  eqOption _ _ _ = true
else instance eqOptionCons ::
  ( Data.Symbol.IsSymbol label
  , Eq value
  , EqOption list option
  , Prim.Row.Cons label value option' option
  ) =>
  EqOption (Prim.RowList.Cons label value list) option where
  eqOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Option option ->
    Option option ->
    Boolean
  eqOption _ left right = leftValue == rightValue && rest
    where
    key :: String
    key = Data.Symbol.reflectSymbol label

    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    leftValue :: Data.Maybe.Maybe value
    leftValue = get label left

    proxy :: Proxy list
    proxy = Proxy

    rest :: Boolean
    rest = eqOption proxy left right

    rightValue :: Data.Maybe.Maybe value
    rightValue = get label right

-- | A typeclass for converting a `Record _` into an `Option _`.
-- |
-- | An instance `FromRecord record required optional` states that we can make a `Record required` and an `Option optional` from a `Record record` where every required field is in the record and the rest of the present fields in the record is present in the option.
-- | E.g. `FromRecord () () ( name :: String )` says that the `Record ()` has no fields and the `Option ( name :: String )` will have no value;
-- | `FromRecord ( name :: String ) () ( name :: String )` says that the `Record ()` has no fields and the `Option ( name :: String )` will have the given `name` value;
-- | `FromRecord ( name :: String ) ( name :: String ) ()` says that the `Record ( name :: String )` has the given `name` value and the `Option ()` will have no value;
-- | `FromRecord () ( name :: String) ()` is a type error since the `name` field is required but the given record lacks the field.
-- |
-- | Since there is syntax for creating records, but no syntax for creating options, this typeclass can be useful for providing an easier to use interface to options.
-- |
-- | E.g. Someone can say:
-- | ```PureScript
-- | Option.fromRecord' { foo: true, bar: 31 }
-- | ```
-- | Instead of having to say:
-- | ```PureScript
-- | Option.insert
-- |   (Data.Symbol.SProxy :: _ "foo")
-- |   true
-- |   ( Option.insert
-- |       (Data.Symbol.SProxy :: _ "bar")
-- |       31
-- |       Option.empty
-- |   )
-- | ```
-- |
-- | Not only does it save a bunch of typing, it also mitigates the need for a direct dependency on `SProxy _`.
class FromRecord (record :: # Type) (required :: # Type) (optional :: # Type) where
  -- | The given `Record record` must have no more fields than expected.
  -- |
  -- | E.g. The following definitions are valid.
  -- | ```PureScript
  -- | option1 ::
  -- |   Record
  -- |     ( optional :: Option.Option ( foo :: Boolean, bar :: Int )
  -- |     , required :: Record ()
  -- |     )
  -- | option1 = Option.fromRecord' { foo: true, bar: 31 }
  -- |
  -- | option2 ::
  -- |   Record
  -- |     ( optional :: Option.Option ( foo :: Boolean, bar :: Int )
  -- |     , required :: Record ()
  -- |     )
  -- | option2 = Option.fromRecord' {}
  -- |
  -- | option3 ::
  -- |   Record
  -- |     ( optional :: Option.Option ( bar :: Int )
  -- |     , required :: Record ( foo :: Boolean )
  -- |     )
  -- | option3 = Option.fromRecord' { foo: true }
  -- | ```
  -- |
  -- | However, the following definitions are not valid as the given records have more fields than the expected `Option _`.
  -- | ```PureScript
  -- | -- This will not work as it has the extra field `baz`
  -- | option3 ::
  -- |   Record
  -- |     ( optional :: Option.Option ( foo :: Boolean, bar :: Int )
  -- |     , required :: Record ()
  -- |     )
  -- | option3 = Option.fromRecord' { foo: true, bar: 31, baz: "hi" }
  -- |
  -- | -- This will not work as it has the extra field `qux`
  -- | option4 ::
  -- |   Record
  -- |     ( optional :: Option.Option ( foo :: Boolean, bar :: Int )
  -- |     , required :: Record ()
  -- |     )
  -- | option4 = Option.fromRecord' { qux: [] }
  -- | ```
  -- |
  -- | And, this definition is not valid as the given record lacks the required fields.
  -- | ```PureScript
  -- | option5 ::
  -- |   Record
  -- |     ( optional :: Option.Option ( foo :: Boolean, bar :: Int )
  -- |     , required :: Record ( baz :: String )
  -- |     )
  -- | option5 = Option.fromRecord' { foo: true, bar: 31 }
  -- | ```
  fromRecord' ::
    Record record ->
    Record
      ( optional :: Option optional
      , required :: Record required
      )

-- | This instance converts a record into an option.
-- |
-- | Every field in the record is added to the option.
-- |
-- | Any fields in the expected option that do not exist in the record are not added.
instance fromRecordAny ::
  ( FromRecordOption optionalList record optional
  , FromRecordRequired requiredList record required
  , Prim.Row.Union required optional' record
  , Prim.RowList.RowToList optional' optionalList
  , Prim.RowList.RowToList required requiredList
  ) =>
  FromRecord record required optional where
  fromRecord' ::
    Record record ->
    Record
      ( optional :: Option optional
      , required :: Record required
      )
  fromRecord' record =
    { optional: fromRecordOption (Proxy :: Proxy optionalList) record
    , required: Record.Builder.build (fromRecordRequired (Proxy :: _ requiredList) record) {}
    }

-- | A typeclass that iterates a `RowList` converting a `Record _` into an `Option _`.
class FromRecordOption (list :: Prim.RowList.RowList) (record :: # Type) (option :: # Type) | list -> option record where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  fromRecordOption ::
    forall proxy.
    proxy list ->
    Record record ->
    Option option

instance fromRecordOptionNil :: FromRecordOption Prim.RowList.Nil record option where
  fromRecordOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Record record ->
    Option option
  fromRecordOption _ _ = empty
else instance fromRecordOptionCons ::
  ( Data.Symbol.IsSymbol label
  , FromRecordOption list record option
  , Prim.Row.Cons label value option' option
  , Prim.Row.Cons label value record' record
  ) =>
  FromRecordOption (Prim.RowList.Cons label value list) record option where
  fromRecordOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Record record ->
    Option option
  fromRecordOption _ record = set label value option
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    option :: Option option
    option = fromRecordOption proxy record

    proxy :: Proxy list
    proxy = Proxy

    value :: value
    value = Record.get label record

-- | A typeclass that iterates a `RowList` selecting the fields from a `Record _`.
class FromRecordRequired (list :: Prim.RowList.RowList) (record :: # Type) (required :: # Type) | list -> required record where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  fromRecordRequired ::
    forall proxy.
    proxy list ->
    Record record ->
    Record.Builder.Builder (Record ()) (Record required)

instance fromRecordRequiredNil :: FromRecordRequired Prim.RowList.Nil record () where
  fromRecordRequired ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Record record ->
    Record.Builder.Builder (Record ()) (Record ())
  fromRecordRequired _ _ = identity
else instance fromRecordRequiredCons ::
  ( Data.Symbol.IsSymbol label
  , FromRecordRequired list record required'
  , Prim.Row.Cons label value record' record
  , Prim.Row.Cons label value required' required
  , Prim.Row.Lacks label required'
  ) =>
  FromRecordRequired (Prim.RowList.Cons label value list) record required where
  fromRecordRequired ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Record record ->
    Record.Builder.Builder (Record ()) (Record required)
  fromRecordRequired _ record = first <<< rest
    where
    first :: Record.Builder.Builder (Record required') (Record required)
    first = Record.Builder.insert label value

    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    proxy :: Proxy list
    proxy = Proxy

    rest :: Record.Builder.Builder (Record ()) (Record required')
    rest = fromRecordRequired proxy record

    value :: value
    value = Record.get label record

-- | A typeclass that grabs the given fields of an `Option _`.
-- |
class Get (record' :: # Type) (option :: # Type) (record :: # Type) | option record' -> record, option record -> record', record record' -> option where
  -- | Attempts to fetch the values from the given option.
  -- |
  -- | The behavior of what's returned depends on what the value is for each field in the record.
  -- |
  -- | If the value in the record is of type `Maybe a -> b` ,
  -- | that function is run on the result of finding the field in the option.
  -- |
  -- | If the value in the record is of type `Maybe a` and the type of the field in the option is `a`,
  -- | the result is `Just _` if the value exists in the option and whatever the provided `Maybe a` was otherwise.
  -- |
  -- | If the value in the record is of type `a` and the type of the field in the option is `a`,
  -- | the result is whatever the value is in the option if it exists and whatever the provided `a` was otherwise.
  -- |
  -- | These behaviors allow handling different fields differently without jumping through hoops to get the values from an option.
  -- |
  -- | E.g.
  -- | ```PureScript
  -- | someOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
  -- | someOption = Option.empty
  -- |
  -- | -- Since `someOption` is empty,
  -- | -- this will have a shape like:
  -- | -- { foo: false, bar: "not set", qux: Data.Maybe.Nothing }
  -- | someRecord :: Record ( foo :: Boolean, bar :: String, qux :: Data.Maybe.Maybe String )
  -- | someRecord =
  -- |   Option.get''
  -- |     { foo: false
  -- |     , bar: \x -> case x of
  -- |         Data.Maybe.Just x -> if x > 0 then "positive" else "non-positive"
  -- |         Data.Maybe.Nothing -> "not set"
  -- |     , qux: Data.Maybe.Nothing
  -- |     }
  -- |     someOption
  -- | ```
  get'' ::
    Record record' ->
    Option option ->
    Record record

-- | This instance converts grabs the given fields of an `Option _`.
instance getAny ::
  ( GetOption list record' option record
  , Prim.RowList.RowToList record' list
  ) =>
  Get record' option record where
  get'' record option = getOption (Proxy :: Proxy list) record option

-- | A typeclass that iterates a `RowList` grabbing the given fields of an `Option _`.
class GetOption (list :: Prim.RowList.RowList) (record' :: # Type) (option :: # Type) (record :: # Type) | list -> record where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  getOption ::
    forall proxy.
    proxy list ->
    Record record' ->
    Option option ->
    Record record

instance getOptionNil ::
  GetOption Prim.RowList.Nil record' option () where
  getOption _ _ _ = {}
else instance getOptionConsFunction ::
  ( Data.Symbol.IsSymbol label
  , GetOption list givenRecord option record'
  , Prim.Row.Cons label (Data.Maybe.Maybe value -> result) givenRecord' givenRecord
  , Prim.Row.Cons label result record' record
  , Prim.Row.Cons label value option' option
  , Prim.Row.Lacks label record'
  ) =>
  GetOption (Prim.RowList.Cons label (Data.Maybe.Maybe value -> result) list) givenRecord option record where
  getOption _ record' option = Record.insert label value record
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    optionValue :: Data.Maybe.Maybe value
    optionValue = get label option

    proxy :: Proxy list
    proxy = Proxy

    record :: Record record'
    record = getOption proxy record' option

    recordValue ::
      Data.Maybe.Maybe value ->
      result
    recordValue = Record.get label record'

    value :: result
    value = recordValue optionValue
else instance getOptionConsMaybe ::
  ( Data.Symbol.IsSymbol label
  , GetOption list givenRecord option record'
  , Prim.Row.Cons label (Data.Maybe.Maybe value) givenRecord' givenRecord
  , Prim.Row.Cons label (Data.Maybe.Maybe value) record' record
  , Prim.Row.Cons label value option' option
  , Prim.Row.Lacks label record'
  ) =>
  GetOption (Prim.RowList.Cons label (Data.Maybe.Maybe value) list) givenRecord option record where
  getOption _ record' option = case optionValue of
    Data.Maybe.Just _ -> Record.insert label optionValue record
    Data.Maybe.Nothing -> Record.insert label recordValue record
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    optionValue :: Data.Maybe.Maybe value
    optionValue = get label option

    proxy :: Proxy list
    proxy = Proxy

    record :: Record record'
    record = getOption proxy record' option

    recordValue :: Data.Maybe.Maybe value
    recordValue = Record.get label record'
else instance getOptionConsValue ::
  ( Data.Symbol.IsSymbol label
  , GetOption list givenRecord option record'
  , Prim.Row.Cons label value givenRecord' givenRecord
  , Prim.Row.Cons label value option' option
  , Prim.Row.Cons label value record' record
  , Prim.Row.Lacks label record'
  ) =>
  GetOption (Prim.RowList.Cons label value list) givenRecord option record where
  getOption _ record' option = case optionValue of
    Data.Maybe.Just value -> Record.insert label value record
    Data.Maybe.Nothing -> Record.insert label recordValue record
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    optionValue :: Data.Maybe.Maybe value
    optionValue = get label option

    proxy :: Proxy list
    proxy = Proxy

    record :: Record record'
    record = getOption proxy record' option

    recordValue :: value
    recordValue = Record.get label record'

-- | A typeclass that converts an `Option _` to a `Maybe (Record _)`.
-- |
-- | If every key exists in the option, the record of values is returned in `Just _`.
-- |
-- | If any key does not exist, `Nothing` is returned.
-- |
-- | E.g. Someone can say:
-- | ```PureScript
-- | someRecord :: Data.Maybe.Maybe (Record ( foo :: Boolean, bar :: Int ))
-- | someRecord = Option.getAll' someOption
-- | ```
-- |
-- | This can also be roughtly thought of as a monomorphic `Data.Traversable.sequence`.
class GetAll (option :: # Type) (record :: # Type) | option -> record where
  -- | Attempts to fetch all of the values from all of the keys of an option.
  -- |
  -- | If every key exists in the option, the record of values is returned in `Just _`.
  -- |
  -- | If any key does not exist in the option, `Nothing` is returned.
  -- |
  -- | E.g.
  -- | ```PureScript
  -- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
  -- | someOption = Option.insert (Data.Symbol.SProxy :: _ "bar") 31 Option.empty
  -- |
  -- | -- This will be `Nothing` because the key `foo` does not exist in the option.
  -- | bar :: Data.Maybe.Maybe (Record ( foo :: Boolean, bar :: Int))
  -- | bar = Option.getAll' someOption
  -- |
  -- | -- This will be `Just { foo: true, bar: 31 }` because all keys exist in the option.
  -- | bar :: Data.Maybe.Maybe (Record ( foo :: Boolean, bar :: Int))
  -- | bar = Option.getAll' (Option.insert (Data.Symbol.SProxy :: _ "foo") true someOption)
  -- | ```
  getAll' ::
    Option option ->
    Data.Maybe.Maybe (Record record)

-- | This instancce converts an `Option _` to a `Maybe (Record _)`.
-- |
-- | If every key exists in the option, the record of values is returned in `Just _`.
-- |
-- | If any key does not exist, `Nothing` is returned.
instance getAllAny ::
  ( Prim.RowList.RowToList option list
  , GetAllOption list option record
  ) =>
  GetAll option record where
  getAll' = getAllOption (Proxy :: Proxy list)

-- | A typeclass that iterates a `RowList` converting an `Option _` into a `Maybe (Record _)`.
class GetAllOption (list :: Prim.RowList.RowList) (option :: # Type) (record :: # Type) | list -> option record where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  getAllOption ::
    forall proxy.
    proxy list ->
    Option option ->
    Data.Maybe.Maybe (Record record)

instance getAllOptionNil ::
  GetAllOption Prim.RowList.Nil option () where
  getAllOption _ _ = Data.Maybe.Just {}
else instance getAllOptionCons ::
  ( Data.Symbol.IsSymbol label
  , Prim.Row.Cons label value option' option
  , Prim.Row.Cons label value record' record
  , Prim.Row.Lacks label record'
  , GetAllOption list option record'
  ) =>
  GetAllOption (Prim.RowList.Cons label value list) option record where
  getAllOption _ option = case record' of
    Data.Maybe.Just record -> case value' of
      Data.Maybe.Just value -> Data.Maybe.Just (Record.insert label value record)
      Data.Maybe.Nothing -> Data.Maybe.Nothing
    Data.Maybe.Nothing -> Data.Maybe.Nothing
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    proxy :: Proxy list
    proxy = Proxy

    record' :: Data.Maybe.Maybe (Record record')
    record' = getAllOption proxy option

    value' :: Data.Maybe.Maybe value
    value' = get label option

-- | A typeclass that inserts values in an `Option _`.
-- |
-- | The keys must not already exist in the option.
-- | If any keys might already exist in the option,
-- | `set''` should be used instead.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.empty
-- |
-- | anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | anotherOption = Option.insert'' { bar: 31 } someOption
-- | ```
class Insert (record :: # Type) (option' :: # Type) (option :: # Type) where
  insert'' ::
    Record record ->
    Option option' ->
    Option option

-- | This instance inserts all values in an `Option _`.
instance insertAny ::
  ( Prim.RowList.RowToList record list
  , InsertOption list record option' option
  ) =>
  Insert record option' option where
  insert'' ::
    Record record ->
    Option option' ->
    Option option
  insert'' = insertOption (Proxy :: Proxy list)

-- | A typeclass that iterates a `Prim.RowList.RowList` inserting values in an `Option _`.
class InsertOption (list :: Prim.RowList.RowList) (record :: # Type) (option' :: # Type) (option :: # Type) | list -> record where
  insertOption ::
    forall proxy.
    proxy list ->
    Record record ->
    Option option' ->
    Option option

instance insertOptionNil ::
  InsertOption Prim.RowList.Nil record option option where
  insertOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Record record ->
    Option option ->
    Option option
  insertOption _ _ option = option
else instance insertOptionConsMaybe ::
  ( Data.Symbol.IsSymbol label
  , Prim.Row.Cons label (Data.Maybe.Maybe value) record' record
  , Prim.Row.Cons label value option' option
  , Prim.Row.Lacks label option'
  , InsertOption list record oldOption option'
  ) =>
  InsertOption (Prim.RowList.Cons label (Data.Maybe.Maybe value) list) record oldOption option where
  insertOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label (Data.Maybe.Maybe value) list) ->
    Record record ->
    Option oldOption ->
    Option option
  insertOption _ record oldOption = case value' of
    Data.Maybe.Just value -> insert label value option
    Data.Maybe.Nothing -> case option of
      Option object -> Option object
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    option :: Option option'
    option = insertOption proxy record oldOption

    proxy :: Proxy list
    proxy = Proxy

    value' :: Data.Maybe.Maybe value
    value' = Record.get label record
else instance insertOptionConsValue ::
  ( Data.Symbol.IsSymbol label
  , Prim.Row.Cons label value record' record
  , Prim.Row.Cons label value option' option
  , Prim.Row.Lacks label option'
  , InsertOption list record oldOption option'
  ) =>
  InsertOption (Prim.RowList.Cons label value list) record oldOption option where
  insertOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Record record ->
    Option oldOption ->
    Option option
  insertOption _ record oldOption = insert label value option
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    option :: Option option'
    option = insertOption proxy record oldOption

    proxy :: Proxy list
    proxy = Proxy

    value :: value
    value = Record.get label record

-- | A typeclass that converts a record of `JsonCodec`s into a `JsonCodec` for an option.
-- |
-- | This is useful to provide a straight-forward `JsonCodec` for an `Option _`.
class JsonCodec (record :: # Type) (option :: # Type) where
  -- | Creates a `JsonCodec` for an `Option _` given a `Record _` of `JsonCodec`s.
  -- |
  -- | E.g.
  -- | The `String` is used in errors when decoding fails.
  -- |
  -- | ```PureScript
  -- | type Example
  -- |   = Option.Option
  -- |       ( foo :: Boolean
  -- |       , bar :: Int
  -- |       )
  -- |
  -- | jsonCodec :: Data.Codec.Argonaut.JsonCodec Example
  -- | jsonCodec =
  -- |   Option.jsonCodec
  -- |     "Example"
  -- |     { foo: Data.Codec.Argonaut.boolean
  -- |     , bar: Data.Codec.Argonaut.int
  -- |     }
  -- | ```
  jsonCodec' ::
    String ->
    Record record ->
    Data.Codec.Argonaut.JsonCodec (Option option)

-- | This instance ignores keys that do not exist in the given JSON object and does not insert keys that do not exist in the given `Option _`.
-- |
-- | If a key does not exist in the JSON object, it will not be added to the `Option _`.
-- |
-- | If a key does exists in the JSON object but the value cannot be successfully decoded, it will fail with an error.
-- |
-- | If a key does exists in the JSON object and the value can be successfully decoded, it will be added to the `Option _`.
-- |
-- | If a key does not exist in the given `Option _`, it is not added to the JSON object.
-- |
-- | If a key does exists in the given `Option _`, it encodes it like normal and adds it to the JSON object.
instance jsonCodecOptionAny ::
  ( JsonCodecOption list record option
  , Prim.RowList.RowToList record list
  ) =>
  JsonCodec record option where
  jsonCodec' ::
    String ->
    Record record ->
    Data.Codec.Argonaut.JsonCodec (Option option)
  jsonCodec' name record =
    Data.Codec.Argonaut.object
      name
      (jsonCodecOption (Proxy :: Proxy list) record)

-- | A typeclass that iterates a `RowList` converting a record of `JsonCodec`s into a `JsonCodec` for an option.
class JsonCodecOption (list :: Prim.RowList.RowList) (record :: # Type) (option :: # Type) | list -> option record where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  jsonCodecOption ::
    forall proxy.
    proxy list ->
    Record record ->
    Data.Codec.Argonaut.JPropCodec (Option option)

instance jsonCodecOptionNil :: JsonCodecOption Prim.RowList.Nil record option where
  jsonCodecOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Record record ->
    Data.Codec.Argonaut.JPropCodec (Option option)
  jsonCodecOption _ _ =
    Data.Codec.mapCodec
      (\_ -> Data.Either.Right empty)
      (\_ -> {})
      Data.Codec.Argonaut.record
else instance jsonCodecOptionCons ::
  ( Data.Symbol.IsSymbol label
  , JsonCodecOption list record option
  , Prim.Row.Cons label value' option' option
  , Prim.Row.Cons label (Data.Codec.Argonaut.JsonCodec value') record' record
  , Type.Equality.TypeEquals value (Data.Codec.Argonaut.JsonCodec value')
  ) =>
  JsonCodecOption (Prim.RowList.Cons label value list) record option where
  jsonCodecOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Record record ->
    Data.Codec.Argonaut.JPropCodec (Option option)
  jsonCodecOption _ record =
    Data.Codec.GCodec
      (Control.Monad.Reader.Trans.ReaderT decode)
      (Data.Profunctor.Star.Star encode)
    where
    codec :: Data.Codec.Argonaut.JsonCodec value'
    codec = Record.get label record

    decode ::
      Foreign.Object.Object Data.Argonaut.Core.Json ->
      Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Option option)
    decode object' = do
      option@(Option object) <- Data.Codec.Argonaut.decode option' object'
      case Foreign.Object.lookup key object' of
        Data.Maybe.Just json -> case Data.Codec.Argonaut.decode codec json of
          Data.Either.Left error -> Data.Either.Left (Data.Codec.Argonaut.AtKey key error)
          Data.Either.Right value -> Data.Either.Right (set label value option)
        Data.Maybe.Nothing -> Data.Either.Right (Option object)

    encode ::
      Option option ->
      Control.Monad.Writer.Writer (Data.List.List (Data.Tuple.Tuple String Data.Argonaut.Core.Json)) (Option option)
    encode option = do
      case get label option of
        Data.Maybe.Just value ->
          Control.Monad.Writer.Class.tell
            ( Data.List.Cons
                (Data.Tuple.Tuple key (Data.Codec.Argonaut.encode codec value))
                Data.List.Nil
            )
        Data.Maybe.Nothing -> pure unit
      Control.Monad.Writer.Class.tell
        (Data.Codec.Argonaut.encode option' option)
      pure option

    key :: String
    key = Data.Symbol.reflectSymbol label

    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    option' :: Data.Codec.Argonaut.JPropCodec (Option option)
    option' = jsonCodecOption proxy record

    proxy :: Proxy list
    proxy = Proxy

-- | A typeclass that manipulates the values in an `Option _`.
-- |
-- | If the field exists in the `Option _`, the given function is applied to the value.
-- |
-- | If the field does not exist in the `Option _`, there is no change to the `Option _`.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.insert (Data.Symbol.SProxy :: _ "bar") 31 Option.empty
-- |
-- | anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | anotherOption = Option.modify'' { bar: \x -> x + 1 } someOption
-- | ```
class Modify (record :: # Type) (option' :: # Type) (option :: # Type) | record option -> option', record option' -> option where
  modify'' ::
    Record record ->
    Option option' ->
    Option option

-- | This instance manipulates the values in an `Option _`.
instance modifyAny ::
  ( ModifyOption list record option' option
  , Prim.RowList.RowToList record list
  ) =>
  Modify record option' option where
  modify'' ::
    Record record ->
    Option option' ->
    Option option
  modify'' record option = modifyOption (Proxy :: Proxy list) record option

-- | A typeclass that iterates a `Prim.RowList.RowList` manipulating values in an `Option _`.
class ModifyOption (list :: Prim.RowList.RowList) (record :: # Type) (option' :: # Type) (option :: # Type) | list option -> option', list option' -> option where
  modifyOption ::
    forall proxy.
    proxy list ->
    Record record ->
    Option option' ->
    Option option

instance modifyOptionNil ::
  ModifyOption Prim.RowList.Nil record option option where
  modifyOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Record record ->
    Option option ->
    Option option
  modifyOption _ _ option = option
else instance modifyOptionCons ::
  ( Data.Symbol.IsSymbol label
  , ModifyOption list record oldOption' option'
  , Prim.Row.Cons label (value' -> value) record' record
  , Prim.Row.Cons label value option' option
  , Prim.Row.Cons label value' oldOption' oldOption
  , Prim.Row.Lacks label oldOption'
  , Prim.Row.Lacks label option'
  ) =>
  ModifyOption (Prim.RowList.Cons label (value' -> value) list) record oldOption option where
  modifyOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label (value' -> value) list) ->
    Record record ->
    Option oldOption ->
    Option option
  modifyOption _ record oldOption = case optionValue of
    Data.Maybe.Just value -> insert label (recordValue value) option
    Data.Maybe.Nothing -> case option of
      Option object -> Option object
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    oldOption' :: Option oldOption'
    oldOption' = delete label oldOption

    option :: Option option'
    option = modifyOption proxy record oldOption'

    optionValue :: Data.Maybe.Maybe value'
    optionValue = get label oldOption

    proxy :: Proxy list
    proxy = Proxy

    recordValue ::
      value' ->
      value
    recordValue = Record.get label record

-- | A typeclass that iterates a `RowList` converting an `Option _` to a `Boolean`.
class
  (EqOption list option) <= OrdOption (list :: Prim.RowList.RowList) (option :: # Type) | list -> option where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  compareOption ::
    forall proxy.
    proxy list ->
    Option option ->
    Option option ->
    Ordering

instance ordOptionNil :: OrdOption Prim.RowList.Nil option where
  compareOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Option option ->
    Option option ->
    Ordering
  compareOption _ _ _ = EQ
else instance ordOptionCons ::
  ( Data.Symbol.IsSymbol label
  , Ord value
  , OrdOption list option
  , Prim.Row.Cons label value option' option
  ) =>
  OrdOption (Prim.RowList.Cons label value list) option where
  compareOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Option option ->
    Option option ->
    Ordering
  compareOption _ left right = case compare leftValue rightValue of
    EQ -> rest
    GT -> GT
    LT -> LT
    where
    key :: String
    key = Data.Symbol.reflectSymbol label

    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    leftValue :: Data.Maybe.Maybe value
    leftValue = get label left

    proxy :: Proxy list
    proxy = Proxy

    rest :: Ordering
    rest = compareOption proxy left right

    rightValue :: Data.Maybe.Maybe value
    rightValue = get label right

-- | A typeclass that iterates a `RowList` attempting to read a `Foreign` to an `Option _`.
class ReadForeignOption (list :: Prim.RowList.RowList) (option :: # Type) | list -> option where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  readImplOption ::
    forall proxy.
    proxy list ->
    Foreign.Foreign ->
    Foreign.F (Option option)

instance readForeignOptionNil :: ReadForeignOption Prim.RowList.Nil option where
  readImplOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Foreign.Foreign ->
    Foreign.F (Option option)
  readImplOption _ _ = pure empty
else instance readForeignOptionCons ::
  ( Data.Symbol.IsSymbol label
  , Prim.Row.Cons label value option' option
  , ReadForeignOption list option
  , Simple.JSON.ReadForeign value
  ) =>
  ReadForeignOption (Prim.RowList.Cons label value list) option where
  readImplOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Foreign.Foreign ->
    Foreign.F (Option option)
  readImplOption _ foreign' = do
    option@(Option object) <- option'
    case Foreign.Index.hasProperty key foreign' of
      true ->
        Control.Monad.Except.except case Control.Monad.Except.runExcept (Foreign.Index.readProp key foreign') of
          Data.Either.Left errors -> Data.Either.Left (map (Foreign.Index.errorAt key) errors)
          Data.Either.Right value' -> case Control.Monad.Except.runExcept (Simple.JSON.readImpl value') of
            Data.Either.Left errors -> Data.Either.Left (map (Foreign.Index.errorAt key) errors)
            Data.Either.Right value -> Data.Either.Right (set label value option)
      false -> pure (Option object)
    where
    key :: String
    key = Data.Symbol.reflectSymbol label

    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    option' :: Foreign.F (Option option)
    option' = readImplOption proxy foreign'

    proxy :: Proxy list
    proxy = Proxy

-- | A typeclass that sets values in an `Option _`.
-- |
-- | The keys must already exist in the option.
-- | If any keys might not already exist in the option,
-- | `insert''` should be used instead.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.empty
-- |
-- | anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | anotherOption = Option.set'' { bar: 31 } someOption
-- | ```
class Set (record :: # Type) (option' :: # Type) (option :: # Type) where
  set'' ::
    Record record ->
    Option option' ->
    Option option

-- | This instance sets all values in an `Option _`.
instance setAny ::
  ( Prim.RowList.RowToList record list
  , SetOption list record option' option
  ) =>
  Set record option' option where
  set'' ::
    Record record ->
    Option option' ->
    Option option
  set'' = setOption (Proxy :: Proxy list)

-- | A typeclass that iterates a `Prim.RowList.RowList` setting values in an `Option _`.
class SetOption (list :: Prim.RowList.RowList) (record :: # Type) (option' :: # Type) (option :: # Type) | list -> record where
  setOption ::
    forall proxy.
    proxy list ->
    Record record ->
    Option option' ->
    Option option

instance setOptionNil ::
  SetOption Prim.RowList.Nil record option option where
  setOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Record record ->
    Option option ->
    Option option
  setOption _ _ option = option
else instance setOptionConsMaybe ::
  ( Data.Symbol.IsSymbol label
  , Prim.Row.Cons label (Data.Maybe.Maybe value) record' record
  , Prim.Row.Cons label value option' option
  , Prim.Row.Cons label value oldOption' oldOption
  , Prim.Row.Lacks label oldOption'
  , Prim.Row.Lacks label option'
  , SetOption list record oldOption' option'
  ) =>
  SetOption (Prim.RowList.Cons label (Data.Maybe.Maybe value) list) record oldOption option where
  setOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label (Data.Maybe.Maybe value) list) ->
    Record record ->
    Option oldOption ->
    Option option
  setOption _ record oldOption = case value' of
    Data.Maybe.Just value -> insert label value option
    Data.Maybe.Nothing -> case get label oldOption of
      Data.Maybe.Just value -> insert label value option
      Data.Maybe.Nothing -> case option of
        Option object -> Option object
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    oldOption' :: Option oldOption'
    oldOption' = delete label oldOption

    option :: Option option'
    option = setOption proxy record oldOption'

    proxy :: Proxy list
    proxy = Proxy

    value' :: Data.Maybe.Maybe value
    value' = Record.get label record
else instance setOptionCons ::
  ( Data.Symbol.IsSymbol label
  , Prim.Row.Cons label value record' record
  , Prim.Row.Cons label value option' option
  , Prim.Row.Cons label value' oldOption' oldOption
  , Prim.Row.Lacks label oldOption'
  , Prim.Row.Lacks label option'
  , SetOption list record oldOption' option'
  ) =>
  SetOption (Prim.RowList.Cons label value list) record oldOption option where
  setOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Record record ->
    Option oldOption ->
    Option option
  setOption _ record oldOption = insert label value option
    where
    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    oldOption' :: Option oldOption'
    oldOption' = delete label oldOption

    option :: Option option'
    option = setOption proxy record oldOption'

    proxy :: Proxy list
    proxy = Proxy

    value :: value
    value = Record.get label record

-- | A typeclass that iterates a `RowList` converting an `Option _` to a `List String`.
-- | The `List String` should be processed into a single `String`.
class ShowOption (list :: Prim.RowList.RowList) (option :: # Type) | list -> option where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  showOption ::
    forall proxy.
    proxy list ->
    Option option ->
    Data.List.List String

instance showOptionNil :: ShowOption Prim.RowList.Nil option where
  showOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Option option ->
    Data.List.List String
  showOption _ _ = Data.List.Nil
else instance showOptionCons ::
  ( Data.Symbol.IsSymbol label
  , Show value
  , ShowOption list option
  , Prim.Row.Cons label value option' option
  ) =>
  ShowOption (Prim.RowList.Cons label value list) option where
  showOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Option option ->
    Data.List.List String
  showOption _ option = case value' of
    Data.Maybe.Just value -> Data.List.Cons (key <> ": " <> show value) rest
    Data.Maybe.Nothing -> rest
    where
    key :: String
    key = Data.Symbol.reflectSymbol label

    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    proxy :: Proxy list
    proxy = Proxy

    rest :: Data.List.List String
    rest = showOption proxy option

    value' :: Data.Maybe.Maybe value
    value' = get label option

-- | A typeclass for converting an `Option _` into a `Record _`.
-- |
-- | Since there is syntax for operating on records, but no syntax for operating on options, this typeclass can be useful for providing an easier to use interface to options.
-- |
-- | E.g. Someone can say:
-- | ```PureScript
-- | (Option.toRecord' someOption).foo
-- | ```
-- | Instead of having to say:
-- | ```PureScript
-- | Option.get (Data.Symbol.SProxy :: _ "foo") someOption
-- | ```
-- |
-- | Not only does it save a bunch of typing, it also mitigates the need for a direct dependency on `SProxy _`.
class ToRecord (option :: # Type) (record :: # Type) | option -> record where
  -- | The expected `Record record` will have the same fields as the given `Option _` where each type is wrapped in a `Maybe`.
  -- |
  -- | E.g.
  -- | ```PureScript
  -- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
  -- | someOption = Option.fromRecord' { foo: true, bar: 31 }
  -- |
  -- | someRecord :: Record ( foo :: Data.Maybe.Maybe Boolean, bar :: Data.Maybe.Maybe Int )
  -- | someRecord = Option.toRecord' someOption
  -- | ```
  toRecord' ::
    Option option ->
    Record record

-- | This instance converts an option into a record.
-- |
-- | Every field in the option is added to a record with a `Maybe _` type.
-- |
-- | All fields in the option that exist will have the value `Just _`.
-- | All fields in the option that do not exist will have the value `Nothing`.
instance toRecordAny ::
  ( ToRecordOption list option record
  , Prim.RowList.RowToList record list
  ) =>
  ToRecord option record where
  toRecord' ::
    Option option ->
    Record record
  toRecord' option = Record.Builder.build (toRecordOption (Proxy :: Proxy list) option) {}

-- | A typeclass that iterates a `RowList` converting an `Option _` into a `Record _`.
class ToRecordOption (list :: Prim.RowList.RowList) (option :: # Type) (record :: # Type) | list -> option record where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  toRecordOption ::
    forall proxy.
    proxy list ->
    Option option ->
    Record.Builder.Builder (Record ()) (Record record)

instance toRecordOptionNil ::
  ToRecordOption Prim.RowList.Nil option () where
  toRecordOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Option option ->
    Record.Builder.Builder (Record ()) (Record ())
  toRecordOption _ _ = identity
else instance toRecordOptionCons ::
  ( Data.Symbol.IsSymbol label
  , Prim.Row.Cons label value option' option
  , Prim.Row.Cons label (Data.Maybe.Maybe value) record' record
  , Prim.Row.Lacks label record'
  , ToRecordOption list option record'
  ) =>
  ToRecordOption (Prim.RowList.Cons label (Data.Maybe.Maybe value) list) option record where
  toRecordOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label (Data.Maybe.Maybe value) list) ->
    Option option ->
    Record.Builder.Builder (Record ()) (Record record)
  toRecordOption _ option = first <<< rest
    where
    first :: Record.Builder.Builder (Record record') (Record record)
    first = Record.Builder.insert label value

    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    proxy :: Proxy list
    proxy = Proxy

    rest :: Record.Builder.Builder (Record ()) (Record record')
    rest = toRecordOption proxy option

    value :: Data.Maybe.Maybe value
    value = get label option

-- | A typeclass that iterates a `RowList` writing an `Option _` to a `Foreign`.
class WriteForeignOption (list :: Prim.RowList.RowList) (option :: # Type) | list -> option where
  -- | The `proxy` can be anything so long as its type variable has kind `Prim.RowList.RowList`.
  -- |
  -- | It will commonly be `Type.Data.RowList.RLProxy`, but doesn't have to be.
  writeForeignOption ::
    forall proxy.
    proxy list ->
    Option option ->
    Foreign.Foreign

instance writeForeignOptionNil ::
  WriteForeignOption Prim.RowList.Nil option where
  writeForeignOption ::
    forall proxy.
    proxy Prim.RowList.Nil ->
    Option option ->
    Foreign.Foreign
  writeForeignOption _ _ = Foreign.unsafeToForeign {}
else instance writeForeignOptionCons ::
  ( Data.Symbol.IsSymbol label
  , Prim.Row.Cons label value option' option
  , Simple.JSON.WriteForeign value
  , WriteForeignOption list option
  ) =>
  WriteForeignOption (Prim.RowList.Cons label value list) option where
  writeForeignOption ::
    forall proxy.
    proxy (Prim.RowList.Cons label value list) ->
    Option option ->
    Foreign.Foreign
  writeForeignOption _ option = case value' of
    Data.Maybe.Just value ->
      Foreign.unsafeToForeign
        (Foreign.Object.insert key (Simple.JSON.writeImpl value) object)
    Data.Maybe.Nothing -> foreign'
    where
    foreign' :: Foreign.Foreign
    foreign' = writeForeignOption proxy option

    key :: String
    key = Data.Symbol.reflectSymbol label

    label :: Data.Symbol.SProxy label
    label = Data.Symbol.SProxy

    object :: Foreign.Object.Object Foreign.Foreign
    object = Foreign.unsafeFromForeign foreign'

    proxy :: Proxy list
    proxy = Proxy

    value' :: Data.Maybe.Maybe value
    value' = get label option

-- Do not export this value. It can be abused to invalidate invariants.
alter ::
  forall label option option' proxy value value'.
  Data.Symbol.IsSymbol label =>
  (Data.Maybe.Maybe value' -> Data.Maybe.Maybe value) ->
  proxy label ->
  Option option' ->
  { option :: Option option, value :: Data.Maybe.Maybe value }
alter f proxy (Option object) = { option, value }
  where
  from :: forall a. Data.Maybe.Maybe a -> Data.Maybe.Maybe value'
  from = Unsafe.Coerce.unsafeCoerce

  go :: forall a. Data.Maybe.Maybe a -> Data.Maybe.Maybe a
  go value' = to (f (from value'))

  key :: String
  key = Data.Symbol.reflectSymbol (Data.Symbol.SProxy :: Data.Symbol.SProxy label)

  option :: Option option
  option = Option (Foreign.Object.alter go key object)

  to :: forall a. Data.Maybe.Maybe value -> Data.Maybe.Maybe a
  to = Unsafe.Coerce.unsafeCoerce

  value :: Data.Maybe.Maybe value
  value = f (from (Foreign.Object.lookup key object))

-- | Removes a key from an option
-- |
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.fromRecord { foo: true, bar: 31 }
-- |
-- | anotherOption :: Option.Option ( bar :: Int )
-- | anotherOption = Option.delete (Data.Symbol.SProxy :: _ "foo") someOption
-- | ```
-- |
-- | The `proxy` can be anything so long as its type variable has kind `Symbol`.
-- |
-- | It will commonly be `Data.Symbol.SProxy`, but doesn't have to be.
delete ::
  forall label option option' proxy value.
  Data.Symbol.IsSymbol label =>
  Prim.Row.Cons label value option option' =>
  Prim.Row.Lacks label option =>
  proxy label ->
  Option option' ->
  Option option
delete proxy option = (alter go proxy option).option
  where
  go :: forall a. a -> Data.Maybe.Maybe value
  go _ = Data.Maybe.Nothing

-- | Removes the given key/values from an option
-- |
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.fromRecord { foo: true, bar: 31 }
-- |
-- | anotherOption :: Option.Option ( bar :: Int )
-- | anotherOption = Option.delete { foo: unit } someOption
-- | ```
delete' ::
  forall option option' record.
  Delete record option' option =>
  Record record ->
  Option option' ->
  Option option
delete' = delete''

-- | Creates an option with no key/values that matches any type of option.
-- |
-- | This can be useful as a starting point for an option that is later built up.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.empty
-- |
-- | anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | anotherOption = Option.set' { bar: 31 } Option.empty
-- | ```
empty :: forall option. Option option
empty = Option Foreign.Object.empty

-- | The given `Record record` must have no more fields than the expected `Option _`.
-- |
-- | E.g. The following definitions are valid.
-- | ```PureScript
-- | option1 :: Option.Option ( foo :: Boolean, bar :: Int )
-- | option1 = Option.fromRecord { foo: true, bar: 31 }
-- |
-- | option2 :: Option.Option ( foo :: Boolean, bar :: Int )
-- | option2 = Option.fromRecord {}
-- | ```
-- |
-- | However, the following definitions are not valid as the given records have more fields than the expected `Option _`.
-- | ```PureScript
-- | -- This will not work as it has the extra field `baz`
-- | option3 :: Option.Option ( foo :: Boolean, bar :: Int )
-- | option3 = Option.fromRecord { foo: true, bar: 31, baz: "hi" }
-- |
-- | -- This will not work as it has the extra field `qux`
-- | option4 :: Option.Option ( foo :: Boolean, bar :: Int )
-- | option4 = Option.fromRecord { qux: [] }
-- | ```
fromRecord ::
  forall option record.
  FromRecord record () option =>
  Record record ->
  Option option
fromRecord record = result.optional
  where
  result ::
    Record
      ( optional :: Option option
      , required :: Record ()
      )
  result = fromRecord' record

-- | The given `Record record` must have no more fields than expected.
-- |
-- | E.g. The following definitions are valid.
-- | ```PureScript
-- | option1 ::
-- |   Record
-- |     ( optional :: Option.Option ( foo :: Boolean, bar :: Int )
-- |     , required :: Record ()
-- |     )
-- | option1 = Option.fromRecord { foo: true, bar: 31 }
-- |
-- | option2 ::
-- |   Record
-- |     ( optional :: Option.Option ( foo :: Boolean, bar :: Int )
-- |     , required :: Record ()
-- |     )
-- | option2 = Option.fromRecord {}
-- |
-- | option3 ::
-- |   Record
-- |     ( optional :: Option.Option ( bar :: Int )
-- |     , required :: Record ( foo :: Boolean )
-- |     )
-- | option3 = Option.fromRecord { foo: true }
-- | ```
-- |
-- | However, the following definitions are not valid as the given records have more fields than the expected `Option _`.
-- | ```PureScript
-- | -- This will not work as it has the extra field `baz`
-- | option3 ::
-- |   Record
-- |     ( optional :: Option.Option ( foo :: Boolean, bar :: Int )
-- |     , required :: Record ()
-- |     )
-- | option3 = Option.fromRecord { foo: true, bar: 31, baz: "hi" }
-- |
-- | -- This will not work as it has the extra field `qux`
-- | option4 ::
-- |   Record
-- |     ( optional :: Option.Option ( foo :: Boolean, bar :: Int )
-- |     , required :: Record ()
-- |     )
-- | option4 = Option.fromRecord { qux: [] }
-- | ```
-- |
-- | And, this definition is not valid as the given record lacks the required fields.
-- | ```PureScript
-- | option5 ::
-- |   Record
-- |     ( optional :: Option.Option ( foo :: Boolean, bar :: Int )
-- |     , required :: Record ( baz :: String )
-- |     )
-- | option5 = Option.fromRecord { foo: true, bar: 31 }
-- | ```
-- |
-- | This is an alias for `fromRecord'` so the documentation is a bit clearer.
fromRecordWithRequired ::
  forall option required record.
  FromRecord record required option =>
  Record record ->
  Record
    ( optional :: Option option
    , required :: Record required
    )
fromRecordWithRequired = fromRecord'

-- | Attempts to fetch the value at the given key from an option.
-- |
-- | If the key exists in the option, `Just _` is returned.
-- |
-- | If the key does not exist in the option, `Nothing` is returned.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.insert (Data.Symbol.SProxy :: _ "bar") 31 Option.empty
-- |
-- | bar :: Data.Maybe.Maybe Int
-- | bar = Option.get (Data.Symbol.SProxy :: _ "bar") someOption
-- | ```
-- |
-- | The `proxy` can be anything so long as its type variable has kind `Symbol`.
-- |
-- | It will commonly be `Data.Symbol.SProxy`, but doesn't have to be.
get ::
  forall label option option' proxy value.
  Data.Symbol.IsSymbol label =>
  Prim.Row.Cons label value option' option =>
  proxy label ->
  Option option ->
  Data.Maybe.Maybe value
get proxy option = (alter go proxy option).value
  where
  go :: Data.Maybe.Maybe value -> Data.Maybe.Maybe value
  go value = value

-- | Attempts to fetch the values from the given option.
-- |
-- | The behavior of what's returned depends on what the value is for each field in the record.
-- |
-- | If the value in the record is of type `Maybe a -> b` ,
-- | that function is run on the result of finding the field in the option.
-- |
-- | If the value in the record is of type `Maybe a` and the type of the field in the option is `a`,
-- | the result is `Just _` if the value exists in the option and whatever the provided `Maybe a` was otherwise.
-- |
-- | If the value in the record is of type `a` and the type of the field in the option is `a`,
-- | the result is whatever the value is in the option if it exists and whatever the provided `a` was otherwise.
-- |
-- | These behaviors allow handling different fields differently without jumping through hoops to get the values from an option.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
-- | someOption = Option.empty
-- |
-- | -- Since `someOption` is empty,
-- | -- this will have a shape like:
-- | -- { foo: false, bar: "not set", qux: Data.Maybe.Nothing }
-- | someRecord :: Record ( foo :: Boolean, bar :: String, qux :: Data.Maybe.Maybe String )
-- | someRecord =
-- |   Option.get'
-- |     { foo: false
-- |     , bar: \x -> case x of
-- |         Data.Maybe.Just x -> if x > 0 then "positive" else "non-positive"
-- |         Data.Maybe.Nothing -> "not set"
-- |     , qux: Data.Maybe.Nothing
-- |     }
-- |     someOption
-- | ```
get' ::
  forall option record record'.
  Get record' option record =>
  Record record' ->
  Option option ->
  Record record
get' record option = get'' record option

-- | Attempts to fetch all of the values from all of the keys of an option.
-- |
-- | If every key exists in the option, the record of values is returned in `Just _`.
-- |
-- | If any key does not exist in the option, `Nothing` is returned.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.insert (Data.Symbol.SProxy :: _ "bar") 31 Option.empty
-- |
-- | -- This will be `Nothing` because the key `foo` does not exist in the option.
-- | bar :: Data.Maybe.Maybe (Record ( foo :: Boolean, bar :: Int))
-- | bar = Option.getAll someOption
-- |
-- | -- This will be `Just { foo: true, bar: 31 }` because all keys exist in the option.
-- | bar :: Data.Maybe.Maybe (Record ( foo :: Boolean, bar :: Int))
-- | bar = Option.getAll (Option.insert (Data.Symbol.SProxy :: _ "foo") true someOption)
-- | ```
getAll ::
  forall option record.
  GetAll option record =>
  Option option ->
  Data.Maybe.Maybe (Record record)
getAll = getAll'

-- | Attempts to fetch the value at the given key from an option falling back to the default.
-- |
-- | If the key exists in the option, `Just _` is returned.
-- |
-- | If the key does not exist in the option, `Nothing` is returned.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.insert (Data.Symbol.SProxy :: _ "bar") 31 Option.empty
-- |
-- | bar :: Int
-- | bar = Option.getWithDefault 13 (Data.Symbol.SProxy :: _ "bar") someOption
-- | ```
-- |
-- | The `proxy` can be anything so long as its type variable has kind `Symbol`.
-- |
-- | It will commonly be `Data.Symbol.SProxy`, but doesn't have to be.
getWithDefault ::
  forall label option option' proxy value.
  Data.Symbol.IsSymbol label =>
  Prim.Row.Cons label value option' option =>
  value ->
  proxy label ->
  Option option ->
  value
getWithDefault default proxy option = case get proxy option of
  Data.Maybe.Just value -> value
  Data.Maybe.Nothing -> default

-- | Gets a suboption of the given option, if it exists; otherwise,
-- | returns an empty option.
getOpt ::
  forall label option option' proxy suboption.
  Data.Symbol.IsSymbol label =>
  Prim.Row.Cons label (Option suboption) option' option =>
  proxy label ->
  Option option ->
  Option suboption
getOpt = getWithDefault empty

-- | Adds a new key with the given value to an option.
-- | The key must not already exist in the option.
-- | If the key might already exist in the option, `set` should be used instead.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean )
-- | someOption = Option.empty
-- |
-- | anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | anotherOption = Option.insert (Data.Symbol.SProxy :: _ "bar") 31 someOption
-- | ```
-- |
-- | The `proxy` can be anything so long as its type variable has kind `Symbol`.
-- |
-- | It will commonly be `Data.Symbol.SProxy`, but doesn't have to be.
insert ::
  forall label option option' proxy value.
  Data.Symbol.IsSymbol label =>
  Prim.Row.Cons label value option' option =>
  Prim.Row.Lacks label option' =>
  proxy label ->
  value ->
  Option option' ->
  Option option
insert proxy value option = (alter go proxy option).option
  where
  go :: forall a. a -> Data.Maybe.Maybe value
  go _ = Data.Maybe.Just value

insert' ::
  forall option option' record.
  Insert record option' option =>
  Record record ->
  Option option' ->
  Option option
insert' = insert''

-- | Creates a `JsonCodec` for an `Option _` given a `Record _` of `JsonCodec`s.
-- |
-- | The `String` is used in errors when decoding fails.
-- |
-- | E.g.
-- | ```PureScript
-- | type Example
-- |   = Option.Option
-- |       ( foo :: Boolean
-- |       , bar :: Int
-- |       )
-- |
-- | jsonCodec :: Data.Codec.Argonaut.JsonCodec Example
-- | jsonCodec =
-- |   Option.jsonCodec
-- |     "Example"
-- |     { foo: Data.Codec.Argonaut.boolean
-- |     , bar: Data.Codec.Argonaut.int
-- |     }
-- | ```
-- |
-- | This is an alias for `jsonCodec'` so the documentation is a bit clearer.
jsonCodec ::
  forall option record.
  JsonCodec record option =>
  String ->
  Record record ->
  Data.Codec.Argonaut.JsonCodec (Option option)
jsonCodec = jsonCodec'

-- | Manipulates the value of a key in an option.
-- |
-- | If the field exists in the option, the given function is applied to the value.
-- |
-- | If the field does not exist in the option, there is no change to the option.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.insert (Data.Symbol.SProxy :: _ "bar") 31 Option.empty
-- |
-- | anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | anotherOption = Option.modify (Data.Symbol.SProxy :: _ "bar") (_ + 1) someOption
-- | ```
-- |
-- | The `proxy` can be anything so long as its type variable has kind `Symbol`.
-- |
-- | It will commonly be `Data.Symbol.SProxy`, but doesn't have to be.
modify ::
  forall label option option' option'' proxy value value'.
  Data.Symbol.IsSymbol label =>
  Prim.Row.Cons label value' option'' option' =>
  Prim.Row.Cons label value option'' option =>
  proxy label ->
  (value' -> value) ->
  Option option' ->
  Option option
modify proxy f option = (alter go proxy option).option
  where
  go :: Data.Maybe.Maybe value' -> Data.Maybe.Maybe value
  go value' = case value' of
    Data.Maybe.Just value -> Data.Maybe.Just (f value)
    Data.Maybe.Nothing -> Data.Maybe.Nothing

-- | Manipulates the values of an option.
-- |
-- | If the field exists in the option, the given function is applied to the value.
-- |
-- | If the field does not exist in the option, there is no change to the option.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.insert (Data.Symbol.SProxy :: _ "bar") 31 Option.empty
-- |
-- | anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | anotherOption = Option.modify' { bar: \x -> x + 1 } someOption
-- | ```
modify' ::
  forall option option' record.
  Modify record option' option =>
  Record record ->
  Option option' ->
  Option option
modify' record option = modify'' record option

-- | Changes a key with the given value to an option.
-- | The key must already exist in the option.
-- | If the key might not already exist in the option, `insert` should be used instead.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.empty
-- |
-- | anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | anotherOption = Option.set (Data.Symbol.SProxy :: _ "bar") 31 someOption
-- | ```
-- |
-- | The `proxy` can be anything so long as its type variable has kind `Symbol`.
-- |
-- | It will commonly be `Data.Symbol.SProxy`, but doesn't have to be.
set ::
  forall label option option' option'' proxy value value'.
  Data.Symbol.IsSymbol label =>
  Prim.Row.Cons label value' option'' option' =>
  Prim.Row.Cons label value option'' option =>
  proxy label ->
  value ->
  Option option' ->
  Option option
set proxy value option = (alter go proxy option).option
  where
  go :: forall a. a -> Data.Maybe.Maybe value
  go _ = Data.Maybe.Just value

-- | Sets the given key/values in an option.
-- | The key must already exist in the option.
-- | If the key might not already exist in the option, `insert` should be used instead.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.empty
-- |
-- | anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | anotherOption = Option.set' { bar: 31 } someOption
-- | ```
set' ::
  forall option option' record.
  Set record option' option =>
  Record record ->
  Option option' ->
  Option option
set' = set''

-- | The expected `Record record` will have the same fields as the given `Option _` where each type is wrapped in a `Maybe`.
-- |
-- | E.g.
-- | ```PureScript
-- | someOption :: Option.Option ( foo :: Boolean, bar :: Int )
-- | someOption = Option.fromRecord { foo: true, bar: 31 }
-- |
-- | someRecord :: Record ( foo :: Data.Maybe.Maybe Boolean, bar :: Data.Maybe.Maybe Int )
-- | someRecord = Option.toRecord someOption
-- | ```
-- |
-- | This is an alias for `toRecord'` so the documentation is a bit clearer.
toRecord ::
  forall option record.
  ToRecord option record =>
  Option option ->
  Record record
toRecord = toRecord'

-- Sanity checks
-- These are in this module so things are always checked.
-- If a failure occurs in development, we can catch it early.
-- If a failure occurs in usage, it should be reported and addressed.
type User
  = Option ( username :: String, age :: Int )

-- does_not_type1 :: User
-- does_not_type1 = fromRecord { height: 10 }
-- does_not_type2 :: { age :: Data.Maybe.Maybe Int, username :: Data.Maybe.Maybe String }
-- does_not_type2 = toRecord empty
user :: User
user = empty

age :: Data.Maybe.Maybe Int
age = get (Data.Symbol.SProxy :: _ "age") user

user1 :: User
user1 = set (Data.Symbol.SProxy :: _ "age") 12 user

user2 :: Option ( username :: String, age :: Int, height :: Int )
user2 = insert (Data.Symbol.SProxy :: _ "height") 12 user

user3 :: Option ( username :: String, age :: Boolean )
user3 = set (Data.Symbol.SProxy :: _ "age") true user

user4 :: Option ( username :: String )
user4 = delete (Data.Symbol.SProxy :: _ "age") user

user5 :: Option ( username :: String, age :: Boolean )
user5 = modify (Data.Symbol.SProxy :: _ "age") (\_ -> true) user

user6 :: User
user6 = fromRecord {}

user7 :: User
user7 = fromRecord { age: 10 }

user8 :: { age :: Data.Maybe.Maybe Int, username :: Data.Maybe.Maybe String }
user8 = toRecord user

user9 :: Data.Maybe.Maybe { age :: Int, username :: String }
user9 = getAll user

user10 :: User
user10 = set' {} user

user11 :: User
user11 = set' { age: 31 } user

user12 :: User
user12 = set' { age: 31, username: "pat" } user

user13 :: Option ( username :: String, age :: Boolean )
user13 = set' { age: true } user

user14 :: User
user14 = set' { age: Data.Maybe.Just 31 } user

user15 :: User
user15 = set' { age: Data.Maybe.Just 31, username: "pat" } user

user16 :: User
user16 = delete' {} user

user17 :: Option ()
user17 = delete' { age: unit, username: unit } user

user18 :: Record ()
user18 = get' {} user

user19 :: Record ( age :: Int, username :: String )
user19 = get' { age: 0, username: "anonymous" } user

user20 :: Record ( age :: String, username :: Data.Maybe.Maybe String )
user20 = get' { age: Data.Maybe.maybe "unknown" show, username: Data.Maybe.Just "anonymous" } user

user21 :: Option ( age :: Boolean, username :: String )
user21 = modify' { age: \_ -> true } user

testing :: { optional :: Option ( title :: String ), required :: { name :: String } }
testing = fromRecordWithRequired { title: "Mr.", name: "" }
