-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptArgonaut
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
  Data.Either.Either String (Option.Record ( name :: String ) ( title :: String ))
decode = Data.Argonaut.Decode.Class.decodeJson

encode ::
  Option.Record ( name :: String ) ( title :: String ) ->
  Data.Argonaut.Core.Json
encode = Data.Argonaut.Encode.Class.encodeJson

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptArgonaut" do
    spec_decode
    spec_encode

spec_decode :: Test.Spec.Spec Unit
spec_decode =
  Test.Spec.describe "decode" do
    Test.Spec.it "fails if no fields are given" do
      Test.Spec.Assertions.shouldEqual
        (decode (Data.Argonaut.Encode.Class.encodeJson {}))
        (Data.Either.Left "JSON was missing expected field: name")
    Test.Spec.it "fails if only a title is given" do
      Test.Spec.Assertions.shouldEqual
        (decode (Data.Argonaut.Encode.Class.encodeJson { title: "wonderful" }))
        (Data.Either.Left "JSON was missing expected field: name")
    Test.Spec.it "requires a name" do
      Test.Spec.Assertions.shouldEqual
        (decode (Data.Argonaut.Encode.Class.encodeJson { name: "Pat" }))
        (Data.Either.Right (Option.recordFromRecord { name: "Pat" }))
    Test.Spec.it "requires a name and accepts a title" do
      Test.Spec.Assertions.shouldEqual
        (decode (Data.Argonaut.Encode.Class.encodeJson { name: "Pat", title: "Dr." }))
        (Data.Either.Right (Option.recordFromRecord { name: "Pat", title: "Dr." }))

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
