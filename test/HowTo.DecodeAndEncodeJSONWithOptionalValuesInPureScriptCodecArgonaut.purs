-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptCodecArgonaut
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
  Data.Either.Either Data.Codec.Argonaut.JsonDecodeError (Option.Option ( name :: String, title :: String ))
decode = Data.Codec.Argonaut.decode jsonCodec

encode ::
  Option.Option ( name :: String, title :: String ) ->
  Data.Argonaut.Core.Json
encode = Data.Codec.Argonaut.encode jsonCodec

jsonCodec :: Data.Codec.Argonaut.JsonCodec (Option.Option ( name :: String, title :: String ))
jsonCodec =
  Option.jsonCodec
    "Greeting"
    { name: Data.Codec.Argonaut.string
    , title: Data.Codec.Argonaut.string
    }

parse ::
  String ->
  Data.Either.Either String (Option.Option ( name :: String, title :: String ))
parse string = do
  json <- Data.Argonaut.Parser.jsonParser string
  case decode json of
    Data.Either.Left err -> Data.Either.Left (Data.Codec.Argonaut.printJsonDecodeError err)
    Data.Either.Right option -> Data.Either.Right option

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptCodecArgonaut" do
    spec_encode
    spec_parse

spec_encode :: Test.Spec.Spec Unit
spec_encode =
  Test.Spec.describe "encode" do
    Test.Spec.it "inserts no fields if none exist" do
      Test.Spec.Assertions.shouldEqual
        (Data.Argonaut.Core.stringify (encode (Option.fromRecord {})))
        "{}"
    Test.Spec.it "inserts a name if it exist" do
      Test.Spec.Assertions.shouldEqual
        (Data.Argonaut.Core.stringify (encode (Option.fromRecord { name: "Pat" })))
        "{\"name\":\"Pat\"}"
    Test.Spec.it "inserts a title if it exist" do
      Test.Spec.Assertions.shouldEqual
        (Data.Argonaut.Core.stringify (encode (Option.fromRecord { title: "wonderful" })))
        "{\"title\":\"wonderful\"}"
    Test.Spec.it "inserts both a name and a title if they exist" do
      Test.Spec.Assertions.shouldEqual
        (Data.Argonaut.Core.stringify (encode (Option.fromRecord { name: "Pat", title: "Dr." })))
        "{\"name\":\"Pat\",\"title\":\"Dr.\"}"

spec_parse :: Test.Spec.Spec Unit
spec_parse =
  Test.Spec.describe "parse" do
    Test.Spec.it "doesn't require any fields" do
      Test.Spec.Assertions.shouldEqual
        (parse """{}""")
        (Data.Either.Right (Option.fromRecord {}))
    Test.Spec.it "accepts only a name" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "name": "Pat" }""")
        (Data.Either.Right (Option.fromRecord { name: "Pat" }))
    Test.Spec.it "accepts only a title" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "title": "wonderful" }""")
        (Data.Either.Right (Option.fromRecord { title: "wonderful" }))
    Test.Spec.it "accepts both a name and a title" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "name": "Pat", "title": "Dr." }""")
        (Data.Either.Right (Option.fromRecord { name: "Pat", title: "Dr." }))
    Test.Spec.it "doesn't fail a null name" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "name": null }""")
        (Data.Either.Right (Option.fromRecord {}))
    Test.Spec.it "doesn't fail for a null title" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "title": null }""")
        (Data.Either.Right (Option.fromRecord {}))
    Test.Spec.it "doesn't fail for a null name or title" do
      Test.Spec.Assertions.shouldEqual
        (parse """{ "name": null, "title": null }""")
        (Data.Either.Right (Option.fromRecord {}))
