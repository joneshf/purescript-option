-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptSimpleJSON
  ( spec
  ) where

import Prelude
import Data.Either as Data.Either
import Data.Semigroup.Foldable as Data.Semigroup.Foldable
import Foreign as Foreign
import Option as Option
import Simple.JSON as Simple.JSON
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

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

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptArgonaut" do
    spec_parse
    spec_writeJSON

spec_parse :: Test.Spec.Spec Unit
spec_parse =
  Test.Spec.describe "parse" do
    Test.Spec.it "fails if no fields are given" do
      Test.Spec.Assertions.shouldEqual
        (parse """{}""")
        (Data.Either.Left "Error at property \"name\": Type mismatch: expected String, found Undefined")
    Test.Spec.it "fails if only a title is given" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "title": "wonderful" }""")
        (Data.Either.Left "Error at property \"name\": Type mismatch: expected String, found Undefined")
    Test.Spec.it "requires a name" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "name": "Pat" }""")
        (Data.Either.Right (Option.recordFromRecord { name: "Pat" }))
    Test.Spec.it "requires a name and accepts a title" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "name": "Pat", "title": "Dr." }""")
        (Data.Either.Right (Option.recordFromRecord { name: "Pat", title: "Dr." }))
    Test.Spec.it "doesn't fail for null fields" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "name": "Pat", "title": null }""")
        (Data.Either.Right (Option.recordFromRecord { name: "Pat" }))

spec_writeJSON :: Test.Spec.Spec Unit
spec_writeJSON =
  Test.Spec.describe "writeJSON" do
    Test.Spec.it "inserts a name" do
      Test.Spec.Assertions.shouldEqual
        (writeJSON (Option.recordFromRecord { name: "Pat" }))
        "{\"name\":\"Pat\"}"
    Test.Spec.it "inserts both a name and a title if it exists" do
      Test.Spec.Assertions.shouldEqual
        (writeJSON (Option.recordFromRecord { name: "Pat", title: "Dr." }))
        "{\"title\":\"Dr.\",\"name\":\"Pat\"}"
