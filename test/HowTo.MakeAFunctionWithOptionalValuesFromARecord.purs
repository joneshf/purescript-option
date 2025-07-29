-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.MakeAFunctionWithOptionalValuesFromARecord
  ( spec
  ) where

import Prelude
import Data.Maybe as Data.Maybe
import Option as Option
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions
import Type.Proxy as Type.Proxy

greeting ::
  forall record.
  Option.FromRecord record () (name :: String, title :: String) =>
  Record record ->
  String
greeting record = "Hello, " <> title' <> name'
  where
  name' :: String
  name' = case Option.get (Type.Proxy.Proxy :: _ "name") option of
    Data.Maybe.Just name -> name
    Data.Maybe.Nothing -> "World"

  option :: Option.Option (name :: String, title :: String)
  option = Option.fromRecord record

  title' :: String
  title' = case Option.get (Type.Proxy.Proxy :: _ "title") option of
    Data.Maybe.Just title -> title <> " "
    Data.Maybe.Nothing -> ""

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.MakeAFunctionWithOptionalValuesFromARecord" do
    spec_greeting

spec_greeting :: Test.Spec.Spec Unit
spec_greeting =
  Test.Spec.describe "greeting" do
    Test.Spec.it "defaults with no values" do
      Test.Spec.Assertions.shouldEqual
        (greeting {})
        "Hello, World"
    Test.Spec.it "uses a name correctly" do
      Test.Spec.Assertions.shouldEqual
        (greeting { name: "Pat" })
        "Hello, Pat"
    Test.Spec.it "uses a title correctly" do
      Test.Spec.Assertions.shouldEqual
        (greeting { title: "wonderful" })
        "Hello, wonderful World"
    Test.Spec.it "uses both a name and a title correctly" do
      Test.Spec.Assertions.shouldEqual
        (greeting { name: "Pat", title: "Dr." })
        "Hello, Dr. Pat"
