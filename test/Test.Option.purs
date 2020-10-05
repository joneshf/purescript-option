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
    spec_fromRecordWithRequired
    spec_get'
    spec_modify'
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

spec_fromRecordWithRequired :: Test.Spec.Spec Unit
spec_fromRecordWithRequired =
  Test.Spec.describe "fromRecordWithRequired" do
    Test.Spec.it "requires correct fields" do
      let
        option ::
          Record
            ( optional ::
                Option.Option
                  ( greeting :: String
                  , title :: String
                  )
            , required ::
                Record
                  ( name :: String
                  )
            )
        option =
          Option.fromRecordWithRequired
            { name: "Pat"
            }
      option.required `Test.Spec.Assertions.shouldEqual` { name: "Pat" }
      option.optional `Test.Spec.Assertions.shouldEqual` Option.empty

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
