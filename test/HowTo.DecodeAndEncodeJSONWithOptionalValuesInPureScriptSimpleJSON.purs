-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptSimpleJSON
  ( spec
  ) where

import Prelude
import Data.Either as Data.Either
import Option as Option
import Simple.JSON as Simple.JSON
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

readJSON ::
  String ->
  Simple.JSON.E (Option.Option ( name :: String, title :: String ))
readJSON = Simple.JSON.readJSON

writeJSON ::
  Option.Option ( name :: String, title :: String ) ->
  String
writeJSON = Simple.JSON.writeJSON

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptSimpleJSON" do
    spec_readJSON
    spec_writeJSON

spec_readJSON :: Test.Spec.Spec Unit
spec_readJSON =
  Test.Spec.describe "readJSON" do
    Test.Spec.it "doesn't require any fields" do
      Test.Spec.Assertions.shouldEqual
        (readJSON """{}""")
        (Data.Either.Right (Option.fromRecord {}))
    Test.Spec.it "accepts only a name" do
      Test.Spec.Assertions.shouldEqual
        (readJSON """{ "name": "Pat" }""")
        (Data.Either.Right (Option.fromRecord { name: "Pat" }))
    Test.Spec.it "accepts only a title" do
      Test.Spec.Assertions.shouldEqual
        (readJSON """{ "title": "wonderful" }""")
        (Data.Either.Right (Option.fromRecord { title: "wonderful" }))
    Test.Spec.it "accepts both a name and a title" do
      Test.Spec.Assertions.shouldEqual
        (readJSON """{ "name": "Pat", "title": "Dr." }""")
        (Data.Either.Right (Option.fromRecord { name: "Pat", title: "Dr." }))
    Test.Spec.it "doesn't fail a null name" do
      Test.Spec.Assertions.shouldEqual
        (readJSON """{ "name": null }""")
        (Data.Either.Right (Option.fromRecord {}))
    Test.Spec.it "doesn't fail for a null title" do
      Test.Spec.Assertions.shouldEqual
        (readJSON """{ "title": null }""")
        (Data.Either.Right (Option.fromRecord {}))
    Test.Spec.it "doesn't fail for a null name or title" do
      Test.Spec.Assertions.shouldEqual
        (readJSON """{ "name": null, "title": null }""")
        (Data.Either.Right (Option.fromRecord {}))

spec_writeJSON :: Test.Spec.Spec Unit
spec_writeJSON =
  Test.Spec.describe "writeJSON" do
    Test.Spec.it "inserts no fields if none exist" do
      Test.Spec.Assertions.shouldEqual
        (writeJSON (Option.fromRecord {}))
        "{}"
    Test.Spec.it "inserts a name if it exist" do
      Test.Spec.Assertions.shouldEqual
        (writeJSON (Option.fromRecord { name: "Pat" }))
        "{\"name\":\"Pat\"}"
    Test.Spec.it "inserts a title if it exist" do
      Test.Spec.Assertions.shouldEqual
        (writeJSON (Option.fromRecord { title: "wonderful" }))
        "{\"title\":\"wonderful\"}"
    Test.Spec.it "inserts both a name and a title if they exist" do
      Test.Spec.Assertions.shouldEqual
        (writeJSON (Option.fromRecord { name: "Pat", title: "Dr." }))
        "{\"title\":\"Dr.\",\"name\":\"Pat\"}"
