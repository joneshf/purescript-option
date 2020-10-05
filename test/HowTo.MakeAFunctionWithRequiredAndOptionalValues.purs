-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.MakeAFunctionWithRequiredAndOptionalValues
  ( spec
  ) where

import Prelude
import Data.Maybe as Data.Maybe
import Option as Option
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

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

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.MakeAFunctionWithOptionalValues" do
    spec_greeting

spec_greeting :: Test.Spec.Spec Unit
spec_greeting =
  Test.Spec.describe "greeting" do
    Test.Spec.it "uses a name correctly" do
      Test.Spec.Assertions.shouldEqual
        (greeting (Option.recordFromRecord { name: "Pat" }))
        "Hello, Pat"
    Test.Spec.it "uses both a name and a title correctly" do
      Test.Spec.Assertions.shouldEqual
        (greeting (Option.recordFromRecord { name: "Pat", title: "Dr." }))
        "Hello, Dr. Pat"
