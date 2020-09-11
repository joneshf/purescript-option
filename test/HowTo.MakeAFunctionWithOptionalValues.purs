-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.MakeAFunctionWithOptionalValues
  ( spec
  ) where

import Prelude
import Data.Maybe as Data.Maybe
import Data.Symbol as Data.Symbol
import Option as Option
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

greeting :: Option.Option ( name :: String, title :: String ) -> String
greeting option = "Hello, " <> title' <> name'
  where
  name' :: String
  name' = case Option.get (Data.Symbol.SProxy :: _ "name") option of
    Data.Maybe.Just name -> name
    Data.Maybe.Nothing -> "World"

  title' :: String
  title' = case Option.get (Data.Symbol.SProxy :: _ "title") option of
    Data.Maybe.Just title -> title <> " "
    Data.Maybe.Nothing -> ""

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.MakeAFunctionWithOptionalValues" do
    spec_greeting

spec_greeting :: Test.Spec.Spec Unit
spec_greeting =
  Test.Spec.describe "greeting" do
    Test.Spec.it "defaults with no values" do
      Test.Spec.Assertions.shouldEqual
        (greeting (Option.fromRecord {}))
        "Hello, World"
    Test.Spec.it "uses a name correctly" do
      Test.Spec.Assertions.shouldEqual
        (greeting (Option.fromRecord { name: "Pat" }))
        "Hello, Pat"
    Test.Spec.it "uses a title correctly" do
      Test.Spec.Assertions.shouldEqual
        (greeting (Option.fromRecord { title: "wonderful" }))
        "Hello, wonderful World"
    Test.Spec.it "uses both a name and a title correctly" do
      Test.Spec.Assertions.shouldEqual
        (greeting (Option.fromRecord { name: "Pat", title: "Dr." }))
        "Hello, Dr. Pat"
