-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptArgonaut
  ( spec
  ) where

import Prelude
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Argonaut.Decode.Class as Data.Argonaut.Decode.Class
import Data.Argonaut.Encode.Class as Data.Argonaut.Encode.Class
import Data.Either as Data.Either
import Option as Option
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

decode ::
  Data.Argonaut.Core.Json ->
  Data.Either.Either String (Option.Option ( name :: String, title :: String ))
decode = Data.Argonaut.Decode.Class.decodeJson

encode ::
  Option.Option ( name :: String, title :: String ) ->
  Data.Argonaut.Core.Json
encode = Data.Argonaut.Encode.Class.encodeJson

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptArgonaut" do
    spec_decode
    spec_encode

spec_decode :: Test.Spec.Spec Unit
spec_decode =
  Test.Spec.describe "decode" do
    Test.Spec.it "doesn't require any fields" do
      Test.Spec.Assertions.shouldEqual
        (decode (Data.Argonaut.Encode.Class.encodeJson {}))
        (Data.Either.Right (Option.fromRecord {}))
    Test.Spec.it "accepts only a name" do
      Test.Spec.Assertions.shouldEqual
        (decode (Data.Argonaut.Encode.Class.encodeJson { name: "Pat" }))
        (Data.Either.Right (Option.fromRecord { name: "Pat" }))
    Test.Spec.it "accepts only a title" do
      Test.Spec.Assertions.shouldEqual
        (decode (Data.Argonaut.Encode.Class.encodeJson { title: "wonderful" }))
        (Data.Either.Right (Option.fromRecord { title: "wonderful" }))
    Test.Spec.it "accepts both a name and a title" do
      Test.Spec.Assertions.shouldEqual
        (decode (Data.Argonaut.Encode.Class.encodeJson { name: "Pat", title: "Dr." }))
        (Data.Either.Right (Option.fromRecord { name: "Pat", title: "Dr." }))

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
        "{\"title\":\"Dr.\",\"name\":\"Pat\"}"
