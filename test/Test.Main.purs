module Test.Main (main) where

import Prelude
import Data.Maybe as Data.Maybe
import Effect as Effect
import Effect.Aff as Effect.Aff
import Option as Option
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assert
import Test.Spec.Reporter.Console as Test.Spec.Reporter.Console
import Test.Spec.Runner as Test.Spec.Runner

data Proxy (symbol :: Symbol)
  = Proxy

main :: Effect.Effect Unit
main = Effect.Aff.launchAff_ (Test.Spec.Runner.runSpec reporters spec)

reporters :: Array Test.Spec.Runner.Reporter
reporters =
  [ Test.Spec.Reporter.Console.consoleReporter
  ]

spec :: Test.Spec.Spec Unit
spec = do
  Test.Spec.describe "Option" do
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
        Option.alter { bar } someOption `Test.Spec.Assert.shouldEqual` Option.fromRecord {}
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
        Option.alter { bar, qux } someOption `Test.Spec.Assert.shouldEqual` Option.fromRecord { bar: "positive" }
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
        option.required `Test.Spec.Assert.shouldEqual` { name: "Pat" }
        option.optional `Test.Spec.Assert.shouldEqual` Option.empty
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
        Option.get' { foo: false, bar, qux: Data.Maybe.Nothing } someOption `Test.Spec.Assert.shouldEqual` { foo: false, bar: "not set", qux: Data.Maybe.Nothing }
    Test.Spec.describe "modify'" do
      Test.Spec.it "does nothing if values are not set" do
        let
          someOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
          someOption = Option.empty

          bar ::
            Int ->
            String
          bar value = if value > 0 then "positive" else "non-positive"
        Option.modify' { bar } someOption `Test.Spec.Assert.shouldEqual` Option.fromRecord {}
      Test.Spec.it "manipulates all fields it can" do
        let
          someOption :: Option.Option ( foo :: Boolean, bar :: Int, qux :: String )
          someOption = Option.fromRecord { bar: 31 }

          bar ::
            Int ->
            String
          bar value = if value > 0 then "positive" else "non-positive"
        Option.modify' { bar } someOption `Test.Spec.Assert.shouldEqual` Option.fromRecord { bar: "positive" }
    Test.Spec.describe "set" do
      Test.Spec.it "sets a value when it doesn't exist" do
        let
          someOption :: Option.Option ( foo :: Boolean, bar :: Int )
          someOption = Option.empty

          anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
          anotherOption = Option.set (Proxy :: _ "bar") 31 someOption
        Option.get (Proxy :: _ "bar") anotherOption `Test.Spec.Assert.shouldEqual` Data.Maybe.Just 31
    Test.Spec.describe "set'" do
      Test.Spec.it "sets a value when it doesn't exist" do
        let
          someOption :: Option.Option ( foo :: Boolean, bar :: Int )
          someOption = Option.empty

          anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
          anotherOption = Option.set' { bar: 31 } someOption
        Option.get (Proxy :: _ "bar") anotherOption `Test.Spec.Assert.shouldEqual` Data.Maybe.Just 31
      Test.Spec.it "can change the type" do
        let
          someOption :: Option.Option ( foo :: Boolean, bar :: Boolean )
          someOption = Option.empty

          anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
          anotherOption = Option.set' { bar: 31 } someOption
        Option.get (Proxy :: _ "bar") anotherOption `Test.Spec.Assert.shouldEqual` Data.Maybe.Just 31
      Test.Spec.it "can use a `Data.Maybe.Maybe _`" do
        let
          someOption :: Option.Option ( foo :: Boolean, bar :: Int )
          someOption = Option.empty

          anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
          anotherOption = Option.set' { bar: Data.Maybe.Just 31 } someOption
        Option.get (Proxy :: _ "bar") anotherOption `Test.Spec.Assert.shouldEqual` Data.Maybe.Just 31
      Test.Spec.it "`Data.Maybe.Nothing` keeps the previous value" do
        let
          someOption :: Option.Option ( foo :: Boolean, bar :: Int )
          someOption = Option.fromRecord { bar: 31 }

          anotherOption :: Option.Option ( foo :: Boolean, bar :: Int )
          anotherOption = Option.set' { bar: Data.Maybe.Nothing } someOption
        Option.get (Proxy :: _ "bar") anotherOption `Test.Spec.Assert.shouldEqual` Data.Maybe.Just 31
