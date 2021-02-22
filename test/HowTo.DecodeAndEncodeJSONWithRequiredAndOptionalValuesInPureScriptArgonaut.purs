-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptArgonaut
  ( spec
  ) where

import Prelude
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Argonaut.Decode.Class as Data.Argonaut.Decode.Class
import Data.Argonaut.Decode.Error as Data.Argonaut.Decode.Error
import Data.Argonaut.Encode.Class as Data.Argonaut.Encode.Class
import Data.Argonaut.Parser as Data.Argonaut.Parser
import Data.Either as Data.Either
import Option as Option
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

decode ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either Data.Argonaut.Decode.Error.JsonDecodeError (Option.Record ( name :: String ) ( title :: String ))
decode = Data.Argonaut.Decode.Class.decodeJson

encode ::
  Option.Record ( name :: String ) ( title :: String ) ->
  Data.Argonaut.Core.Json
encode = Data.Argonaut.Encode.Class.encodeJson

parse ::
  String ->
  Data.Either.Either String (Option.Record ( name :: String ) ( title :: String ))
parse string = case Data.Argonaut.Parser.jsonParser string of
  Data.Either.Left error -> Data.Either.Left error
  Data.Either.Right json -> case decode json of
    Data.Either.Left error -> Data.Either.Left (Data.Argonaut.Decode.Error.printJsonDecodeError error)
    Data.Either.Right record -> Data.Either.Right record

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptArgonaut" do
    spec_parse
    spec_encode

spec_encode :: Test.Spec.Spec Unit
spec_encode =
  Test.Spec.describe "encode" do
    Test.Spec.it "inserts a name" do
      Test.Spec.Assertions.shouldEqual
        (Data.Argonaut.Core.stringify (encode (Option.recordFromRecord { name: "Pat" })))
        "{\"name\":\"Pat\"}"
    Test.Spec.it "inserts both a name and a title if it exists" do
      Test.Spec.Assertions.shouldEqual
        (Data.Argonaut.Core.stringify (encode (Option.recordFromRecord { name: "Pat", title: "Dr." })))
        "{\"title\":\"Dr.\",\"name\":\"Pat\"}"

spec_parse :: Test.Spec.Spec Unit
spec_parse =
  Test.Spec.describe "parse" do
    Test.Spec.it "fails if no fields are given" do
      Test.Spec.Assertions.shouldEqual
        (parse """{}""")
        (Data.Either.Left "An error occurred while decoding a JSON value:\n  At object key 'name':\n  No value was found.")
    Test.Spec.it "fails if only a title is given" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "title": "wonderful" }""")
        (Data.Either.Left "An error occurred while decoding a JSON value:\n  At object key 'name':\n  No value was found.")
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
