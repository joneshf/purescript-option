module Test.Option
  ( spec
  ) where

import Prelude
import Data.Maybe as Data.Maybe
import Option as Option
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

data Proxy (symbol :: Symbol)
  = Proxy

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "Test.Option" do
    spec_alter
    spec_fromRecord
    spec_get'
    spec_modify'
    spec_recordFromRecord
    spec_recordToRecord
    spec_recordSet
    spec_set
    spec_set'

spec_alter :: Test.Spec.Spec Unit
spec_alter =
  Test.Spec.describe "alter" do
    Test.Spec.it "does nothing if values are not set" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
        someOption = Option.empty

        bar ::
          Data.Maybe.Maybe Int ->
          Data.Maybe.Maybe String
        bar value' = case value' of
          Data.Maybe.Just value -> if value > 0 then Data.Maybe.Just "positive" else Data.Maybe.Nothing
          Data.Maybe.Nothing -> Data.Maybe.Nothing
      Option.alter { bar } someOption `Test.Spec.Assertions.shouldEqual` Option.fromRecord {}
    Test.Spec.it "manipulates all fields it can" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
        someOption = Option.fromRecord { bar: 31, qux: "hi" }

        bar ::
          Data.Maybe.Maybe Int ->
          Data.Maybe.Maybe String
        bar value' = case value' of
          Data.Maybe.Just value -> if value > 0 then Data.Maybe.Just "positive" else Data.Maybe.Nothing
          Data.Maybe.Nothing -> Data.Maybe.Nothing

        qux ::
          Data.Maybe.Maybe String ->
          Data.Maybe.Maybe String
        qux _ = Data.Maybe.Nothing
      Option.alter { bar, qux } someOption `Test.Spec.Assertions.shouldEqual` Option.fromRecord { bar: "positive" }

spec_fromRecord :: Test.Spec.Spec Unit
spec_fromRecord =
  Test.Spec.describe "fromRecord" do
    Test.Spec.it "only sets the given values" do
      let
        option ::
          Option.Option
            ( age :: Int
            , name :: String
            )
        option =
          Option.fromRecord
            { name: "Pat"
            }
      Option.toRecord option `Test.Spec.Assertions.shouldEqual` { age: Data.Maybe.Nothing, name: Data.Maybe.Just "Pat" }
    Test.Spec.it "allows `Data.Maybe.Just _`" do
      let
        option ::
          Option.Option
            ( age :: Int
            , name :: String
            )
        option =
          Option.fromRecord
            { name: Data.Maybe.Just "Pat"
            }
      Option.toRecord option `Test.Spec.Assertions.shouldEqual` { age: Data.Maybe.Nothing, name: Data.Maybe.Just "Pat" }
    Test.Spec.it "allows `Data.Maybe.Nothing`" do
      let
        option ::
          Option.Option
            ( age :: Int
            , name :: String
            )
        option =
          Option.fromRecord
            { name: Data.Maybe.Nothing
            }
      Option.toRecord option `Test.Spec.Assertions.shouldEqual` { age: Data.Maybe.Nothing, name: Data.Maybe.Nothing }
    Test.Spec.it "allows `Data.Maybe.Just _` and `Data.Maybe.Nothing`" do
      let
        option ::
          Option.Option
            ( age :: Int
            , name :: String
            )
        option =
          Option.fromRecord
            { age: Data.Maybe.Just 31
            , name: Data.Maybe.Nothing
            }
      Option.toRecord option `Test.Spec.Assertions.shouldEqual` { age: Data.Maybe.Just 31, name: Data.Maybe.Nothing }
    Test.Spec.it "allows nested `Data.Maybe.Maybe _`s" do
      let
        option ::
          Option.Option
            ( active :: Data.Maybe.Maybe Boolean
            , age :: Data.Maybe.Maybe (Data.Maybe.Maybe Int)
            , name :: String
            )
        option =
          Option.fromRecord
            { active: Data.Maybe.Just (Data.Maybe.Just true)
            , age: Data.Maybe.Just (Data.Maybe.Just (Data.Maybe.Just 31))
            , name: Data.Maybe.Just "Pat"
            }
      Option.toRecord option `Test.Spec.Assertions.shouldEqual` { active: Data.Maybe.Just (Data.Maybe.Just true), age: Data.Maybe.Just (Data.Maybe.Just (Data.Maybe.Just 31)), name: Data.Maybe.Just "Pat" }

spec_get' :: Test.Spec.Spec Unit
spec_get' =
  Test.Spec.describe "get'" do
    Test.Spec.it "gets all fields it can" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
        someOption = Option.empty

        bar ::
          Data.Maybe.Maybe Int ->
          String
        bar value' = case value' of
          Data.Maybe.Just value -> if value > 0 then "positive" else "non-positive"
          Data.Maybe.Nothing -> "not set"
      Option.get' { foo: false, bar, qux: Data.Maybe.Nothing } someOption `Test.Spec.Assertions.shouldEqual` { foo: false, bar: "not set", qux: Data.Maybe.Nothing }

spec_modify' :: Test.Spec.Spec Unit
spec_modify' =
  Test.Spec.describe "modify'" do
    Test.Spec.it "does nothing if values are not set" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
        someOption = Option.empty

        bar ::
          Int ->
          String
        bar value = if value > 0 then "positive" else "non-positive"
      Option.modify' { bar } someOption `Test.Spec.Assertions.shouldEqual` Option.fromRecord {}
    Test.Spec.it "manipulates all fields it can" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
        someOption = Option.fromRecord { bar: 31 }

        bar ::
          Int ->
          String
        bar value = if value > 0 then "positive" else "non-positive"
      Option.modify' { bar } someOption `Test.Spec.Assertions.shouldEqual` Option.fromRecord { bar: "positive" }

spec_recordFromRecord :: Test.Spec.Spec Unit
spec_recordFromRecord =
  Test.Spec.describe "recordFromRecord" do
    Test.Spec.it "requires correct fields" do
      let
        record ::
          Option.Record
            ( name :: String
            )
            ( greeting :: String
            , title :: String
            )
        record =
          Option.recordFromRecord
            { name: "Pat"
            }
      Option.required record `Test.Spec.Assertions.shouldEqual` { name: "Pat" }
      Option.optional record `Test.Spec.Assertions.shouldEqual` Option.empty

spec_recordSet :: Test.Spec.Spec Unit
spec_recordSet =
  Test.Spec.describe "recordSet" do
    Test.Spec.it "sets a value when it doesn't exist" do
      let
        someRecord :: Option.Record ( foo :: Boolean ) ( bar :: Int )
        someRecord = Option.recordFromRecord { foo: false }

        anotherRecord :: Option.Record ( foo :: Boolean ) ( bar :: Int )
        anotherRecord = Option.recordSet { bar: 31 } someRecord
      Option.recordToRecord anotherRecord `Test.Spec.Assertions.shouldEqual` { bar: Data.Maybe.Just 31, foo: false }
    Test.Spec.it "can change the type" do
      let
        someRecord :: Option.Record ( foo :: Boolean ) ( bar :: Boolean )
        someRecord = Option.recordFromRecord { foo: false }

        anotherRecord :: Option.Record ( foo :: Boolean ) ( bar :: Int )
        anotherRecord = Option.recordSet { bar: 31 } someRecord
      Option.recordToRecord anotherRecord `Test.Spec.Assertions.shouldEqual` { bar: Data.Maybe.Just 31, foo: false }
    Test.Spec.it "can use a `Data.Maybe.Maybe _`" do
      let
        someRecord :: Option.Record ( foo :: Boolean ) ( bar :: Int )
        someRecord = Option.recordFromRecord { foo: false }

        anotherRecord :: Option.Record ( foo :: Boolean ) ( bar :: Int )
        anotherRecord = Option.recordSet { bar: Data.Maybe.Just 31 } someRecord
      Option.recordToRecord anotherRecord `Test.Spec.Assertions.shouldEqual` { bar: Data.Maybe.Just 31, foo: false }
    Test.Spec.it "`Data.Maybe.Nothing` keeps the previous value" do
      let
        someRecord :: Option.Record ( foo :: Boolean ) ( bar :: Int )
        someRecord = Option.recordFromRecord { bar: 31, foo: false }

        anotherRecord :: Option.Record ( foo :: Boolean ) ( bar :: Int )
        anotherRecord = Option.recordSet { bar: Data.Maybe.Nothing } someRecord
      Option.recordToRecord anotherRecord `Test.Spec.Assertions.shouldEqual` { bar: Data.Maybe.Just 31, foo: false }
    Test.Spec.it "can set both required and optional values" do
      let
        someRecord :: Option.Record ( foo :: Boolean ) ( bar :: Int )
        someRecord = Option.recordFromRecord { foo: false }

        anotherRecord :: Option.Record ( foo :: Boolean ) ( bar :: Int )
        anotherRecord = Option.recordSet { bar: 31 } someRecord
      Option.recordToRecord anotherRecord `Test.Spec.Assertions.shouldEqual` { bar: Data.Maybe.Just 31, foo: false }
    Test.Spec.it "can set any required and optional values" do
      let
        someRecord :: Option.Record ( foo :: Boolean, bar :: Int, baz :: String ) ( qux :: Boolean, cor :: Int, gar :: String )
        someRecord = Option.recordFromRecord { foo: false, bar: 31, baz: "hi" }

        anotherRecord :: Option.Record ( foo :: Boolean, bar :: Int, baz :: String ) ( qux :: Boolean, cor :: Int, gar :: String )
        anotherRecord = Option.recordSet { baz: "hello", qux: true } someRecord
      Option.recordToRecord anotherRecord `Test.Spec.Assertions.shouldEqual` { bar: 31, baz: "hello", cor: Data.Maybe.Nothing, foo: false, gar: Data.Maybe.Nothing, qux: Data.Maybe.Just true }
    Test.Spec.it "can set any required and optional values with any interleaving of names" do
      let
        someRecord :: Option.Record ( a :: Boolean, c :: Boolean, e :: Boolean ) ( b :: Boolean, d :: Boolean, f :: Boolean )
        someRecord = Option.recordFromRecord { a: false, c: false, e: false }

        anotherRecord :: Option.Record ( a :: Boolean, c :: Boolean, e :: Boolean ) ( b :: Boolean, d :: Boolean, f :: Boolean )
        anotherRecord = Option.recordSet { b: true, c: true, d: true } someRecord
      Option.recordToRecord anotherRecord `Test.Spec.Assertions.shouldEqual` { a: false, b: Data.Maybe.Just true, c: true, d: Data.Maybe.Just true, e: false, f: Data.Maybe.Nothing }
    Test.Spec.it "can change the type of both required and optional values" do
      let
        someRecord :: Option.Record ( foo :: Boolean ) ( bar :: Boolean )
        someRecord = Option.recordFromRecord { foo: false }

        anotherRecord :: Option.Record ( foo :: Int ) ( bar :: Int )
        anotherRecord = Option.recordSet { bar: 31, foo: 43 } someRecord
      Option.recordToRecord anotherRecord `Test.Spec.Assertions.shouldEqual` { bar: Data.Maybe.Just 31, foo: 43 }

spec_recordToRecord :: Test.Spec.Spec Unit
spec_recordToRecord =
  Test.Spec.describe "recordToRecord" do
    Test.Spec.it "requires correct fields" do
      let
        record ::
          Option.Record
            ( name :: String
            )
            ( greeting :: String
            , title :: String
            )
        record =
          Option.recordFromRecord
            { name: "Pat"
            , title: "Dr."
            }
      Option.recordToRecord record `Test.Spec.Assertions.shouldEqual` { greeting: Data.Maybe.Nothing, name: "Pat", title: Data.Maybe.Just "Dr." }

spec_set :: Test.Spec.Spec Unit
spec_set =
  Test.Spec.describe "set" do
    Test.Spec.it "sets a value when it doesn't exist" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Int )
        someOption = Option.empty

        anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
        anotherOption = Option.set (Proxy :: _ "bar") 31 someOption
      Option.get (Proxy :: _ "bar") anotherOption `Test.Spec.Assertions.shouldEqual` Data.Maybe.Just 31

spec_set' :: Test.Spec.Spec Unit
spec_set' =
  Test.Spec.describe "set'" do
    Test.Spec.it "sets a value when it doesn't exist" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Int )
        someOption = Option.empty

        anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
        anotherOption = Option.set' { bar: 31 } someOption
      Option.get (Proxy :: _ "bar") anotherOption `Test.Spec.Assertions.shouldEqual` Data.Maybe.Just 31
    Test.Spec.it "can work for any field" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
        someOption = Option.empty

        anotherOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
        anotherOption = Option.set' { qux: "hi" } someOption
      Option.get (Proxy :: _ "qux") anotherOption `Test.Spec.Assertions.shouldEqual` Data.Maybe.Just "hi"
    Test.Spec.it "can change the type" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Boolean )
        someOption = Option.empty

        anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
        anotherOption = Option.set' { bar: 31 } someOption
      Option.get (Proxy :: _ "bar") anotherOption `Test.Spec.Assertions.shouldEqual` Data.Maybe.Just 31
    Test.Spec.it "can use a `Data.Maybe.Maybe _`" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Int )
        someOption = Option.empty

        anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
        anotherOption = Option.set' { bar: Data.Maybe.Just 31 } someOption
      Option.get (Proxy :: _ "bar") anotherOption `Test.Spec.Assertions.shouldEqual` Data.Maybe.Just 31
    Test.Spec.it "`Data.Maybe.Nothing` keeps the previous value" do
      let
        someOption :: Option.Option ( foo :: Boolean, bar :: Int )
        someOption = Option.fromRecord { bar: 31 }

        anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
        anotherOption = Option.set' { bar: Data.Maybe.Nothing } someOption
      Option.get (Proxy :: _ "bar") anotherOption `Test.Spec.Assertions.shouldEqual` Data.Maybe.Just 31
