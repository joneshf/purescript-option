-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptCodecArgonaut
  ( spec
  ) where

import Prelude
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Argonaut.Parser as Data.Argonaut.Parser
import Data.Codec.Argonaut as Data.Codec.Argonaut
import Data.Either as Data.Either
import Option as Option
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

decode ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Option.Record ( name :: String ) ( title :: String ))
decode = Data.Codec.Argonaut.decode jsonCodec

encode ::
  Option.Record ( name :: String ) ( title :: String ) ->
  Data.Argonaut.Core.Json
encode = Data.Codec.Argonaut.encode jsonCodec

jsonCodec :: Data.Codec.Argonaut.JsonCodec (Option.Record ( name :: String ) ( title :: String ))
jsonCodec =
  Option.jsonCodecRecord
    "Greeting"
    { name: Data.Codec.Argonaut.string
    , title: Data.Codec.Argonaut.string
    }

parse ::
  String ->
  Data.Either.Either String (Option.Record ( name :: String ) ( title :: String ))
parse string = do
  json <- Data.Argonaut.Parser.jsonParser string
  case decode json of
    Data.Either.Left err -> Data.Either.Left (Data.Codec.Argonaut.printJsonDecodeError err)
    Data.Either.Right option -> Data.Either.Right option

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptCodecArgonaut" do
    spec_encode
    spec_parse

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
        "{\"name\":\"Pat\",\"title\":\"Dr.\"}"

spec_parse :: Test.Spec.Spec Unit
spec_parse =
  Test.Spec.describe "parse" do
    Test.Spec.it "fails if no fields are given" do
      Test.Spec.Assertions.shouldEqual
        (parse """{}""")
        (Data.Either.Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key name:\n  No value was found.")
    Test.Spec.it "fails if only a title is given" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "title": "wonderful" }""")
        (Data.Either.Left "An error occurred while decoding a JSON value:\n  Under 'Greeting':\n  At object key name:\n  No value was found.")
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
