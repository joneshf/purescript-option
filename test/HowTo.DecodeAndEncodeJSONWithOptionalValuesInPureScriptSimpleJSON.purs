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
  Test.Spec.describe "HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptArgonaut" do
    spec_readJSON
    spec_writeJSON

spec_readJSON :: Test.Spec.Spec Unit
spec_readJSON =
  Test.Spec.describe "readJSON" do
    Test.Spec.it "doesn't require any fields" do
      let
        json :: String
        json = """{}"""
      Test.Spec.Assertions.shouldEqual
        (readJSON json)
        (Data.Either.Right (Option.fromRecord {}))
    Test.Spec.it "accepts only a name" do
      let
        json :: String
        json = """{ "name": "Pat" }"""
      Test.Spec.Assertions.shouldEqual
        (readJSON json)
        (Data.Either.Right (Option.fromRecord { name: "Pat" }))
    Test.Spec.it "accepts only a title" do
      let
        json :: String
        json = """{ "title": "wonderful" }"""
      Test.Spec.Assertions.shouldEqual
        (readJSON json)
        (Data.Either.Right (Option.fromRecord { title: "wonderful" }))
    Test.Spec.it "accepts both a name and a title" do
      let
        json :: String
        json = """{ "name": "Pat", "title": "Dr." }"""
      Test.Spec.Assertions.shouldEqual
        (readJSON json)
        (Data.Either.Right (Option.fromRecord { name: "Pat", title: "Dr." }))
    Test.Spec.it "doesn't fail a null name" do
      let
        json :: String
        json = """{ "name": null }"""
      Test.Spec.Assertions.shouldEqual
        (readJSON json)
        (Data.Either.Right (Option.fromRecord {}))
    Test.Spec.it "doesn't fail for a null title" do
      let
        json :: String
        json = """{ "title": null }"""
      Test.Spec.Assertions.shouldEqual
        (readJSON json)
        (Data.Either.Right (Option.fromRecord {}))
    Test.Spec.it "doesn't fail for a null name or title" do
      let
        json :: String
        json = """{ "name": null, "title": null }"""
      Test.Spec.Assertions.shouldEqual
        (readJSON json)
        (Data.Either.Right (Option.fromRecord {}))

spec_writeJSON :: Test.Spec.Spec Unit
spec_writeJSON =
  Test.Spec.describe "writeJSON" do
    Test.Spec.it "inserts no fields if none exist" do
      let
        option :: Option.Option ( name :: String, title :: String )
        option = Option.fromRecord {}
      Test.Spec.Assertions.shouldEqual
        (writeJSON option)
        "{}"
    Test.Spec.it "inserts a name if it exist" do
      let
        option :: Option.Option ( name :: String, title :: String )
        option = Option.fromRecord { name: "Pat" }
      Test.Spec.Assertions.shouldEqual
        (writeJSON option)
        "{\"name\":\"Pat\"}"
    Test.Spec.it "inserts a title if it exist" do
      let
        option :: Option.Option ( name :: String, title :: String )
        option = Option.fromRecord { title: "wonderful" }
      Test.Spec.Assertions.shouldEqual
        (writeJSON option)
        "{\"title\":\"wonderful\"}"
    Test.Spec.it "inserts both a name and a title if they exist" do
      let
        option :: Option.Option ( name :: String, title :: String )
        option = Option.fromRecord { name: "Pat", title: "Dr." }
      Test.Spec.Assertions.shouldEqual
        (writeJSON option)
        "{\"title\":\"Dr.\",\"name\":\"Pat\"}"
