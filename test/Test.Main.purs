module Test.Main (main) where

import Prelude
import Effect as Effect
import Effect.Aff as Effect.Aff
import HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptArgonaut as HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptArgonaut
import HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptCodecArgonaut as HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptCodecArgonaut
import HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptSimpleJSON as HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptSimpleJSON
import HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptArgonaut as HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptArgonaut
import HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptCodecArgonaut as HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptCodecArgonaut
import HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptSimpleJSON as HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptSimpleJSON
import HowTo.MakeAFunctionWithOptionalValues as HowTo.MakeAFunctionWithOptionalValues
import HowTo.MakeAFunctionWithOptionalValuesFromARecord as HowTo.MakeAFunctionWithOptionalValuesFromARecord
import HowTo.MakeAFunctionWithRequiredAndOptionalValues as HowTo.MakeAFunctionWithRequiredAndOptionalValues
import HowTo.MakeAFunctionWithRequiredAndOptionalValuesFromARecord as HowTo.MakeAFunctionWithRequiredAndOptionalValuesFromARecord
import HowTo.ProvideAnEasierAPIForDateTime as HowTo.ProvideAnEasierAPIForDateTime
import Test.Option as Test.Option
import Test.Spec as Test.Spec
import Test.Spec.Reporter.Console as Test.Spec.Reporter.Console
import Test.Spec.Runner as Test.Spec.Runner

data Proxy (symbol :: Symbol) = Proxy

main :: Effect.Effect Unit
main = Effect.Aff.launchAff_ (Test.Spec.Runner.runSpec reporters spec)

reporters :: Array Test.Spec.Runner.Reporter
reporters =
  [ Test.Spec.Reporter.Console.consoleReporter
  ]

spec :: Test.Spec.Spec Unit
spec = do
  Test.Option.spec
  HowTo.MakeAFunctionWithOptionalValues.spec
  HowTo.MakeAFunctionWithOptionalValuesFromARecord.spec
  HowTo.MakeAFunctionWithRequiredAndOptionalValues.spec
  HowTo.MakeAFunctionWithRequiredAndOptionalValuesFromARecord.spec
  HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptArgonaut.spec
  HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptCodecArgonaut.spec
  HowTo.DecodeAndEncodeJSONWithOptionalValuesInPureScriptSimpleJSON.spec
  HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptArgonaut.spec
  HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptCodecArgonaut.spec
  HowTo.DecodeAndEncodeJSONWithRequiredAndOptionalValuesInPureScriptSimpleJSON.spec
  HowTo.ProvideAnEasierAPIForDateTime.spec
