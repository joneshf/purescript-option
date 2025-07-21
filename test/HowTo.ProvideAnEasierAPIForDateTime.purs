-- | If anything changes here,
-- | make sure the README is updated accordingly.
module HowTo.ProvideAnEasierAPIForDateTime
  ( spec
  ) where

import Prelude
import Data.Date as Data.Date
import Data.Date.Component as Data.Date.Component
import Data.DateTime as Data.DateTime
import Data.Enum as Data.Enum
import Data.Maybe as Data.Maybe
import Data.Symbol as Data.Symbol
import Data.Time as Data.Time
import Data.Time.Component as Data.Time.Component
import Option as Option
import Prim.Row as Prim.Row
import Test.Spec as Test.Spec
import Test.Spec.Assertions as Test.Spec.Assertions

type Option =
  ( day :: Int
  , hour :: Int
  , millisecond :: Int
  , minute :: Int
  , month :: Data.Date.Component.Month
  , second :: Int
  , year :: Int
  )

dateTime ::
  forall record.
  Option.FromRecord record () Option =>
  Record record ->
  Data.DateTime.DateTime
dateTime record = Data.DateTime.DateTime date time
  where
  date :: Data.Date.Date
  date = Data.Date.canonicalDate year month day
    where
    day :: Data.Date.Component.Day
    day = get (Data.Symbol.SProxy :: _ "day")

    month :: Data.Date.Component.Month
    month = Option.getWithDefault bottom (Data.Symbol.SProxy :: _ "month") options

    year :: Data.Date.Component.Year
    year = get (Data.Symbol.SProxy :: _ "year")

  get ::
    forall label proxy record' value.
    Data.Enum.BoundedEnum value =>
    Data.Symbol.IsSymbol label =>
    Prim.Row.Cons label Int record' Option =>
    proxy label ->
    value
  get proxy = case Option.get proxy options of
    Data.Maybe.Just x -> Data.Enum.toEnumWithDefaults bottom top x
    Data.Maybe.Nothing -> bottom

  options :: Option.Option Option
  options = Option.fromRecord record

  time :: Data.Time.Time
  time = Data.Time.Time hour minute second millisecond
    where
    hour :: Data.Time.Component.Hour
    hour = get (Data.Symbol.SProxy :: _ "hour")

    minute :: Data.Time.Component.Minute
    minute = get (Data.Symbol.SProxy :: _ "minute")

    millisecond :: Data.Time.Component.Millisecond
    millisecond = get (Data.Symbol.SProxy :: _ "millisecond")

    second :: Data.Time.Component.Second
    second = get (Data.Symbol.SProxy :: _ "second")

spec :: Test.Spec.Spec Unit
spec =
  Test.Spec.describe "HowTo.ProvideAnEasierAPIForDateTime" do
    spec_dateTime

spec_dateTime :: Test.Spec.Spec Unit
spec_dateTime =
  Test.Spec.describe "dateTime" do
    let
      dateTime' :: Data.DateTime.DateTime
      dateTime' = dateTime { minute: 30, month: Data.Date.Component.April, year: 2019 }
    Test.Spec.it "sets the year" do
      Test.Spec.Assertions.shouldEqual
        (Data.Enum.fromEnum (Data.Date.year (Data.DateTime.date dateTime')))
        2019
    Test.Spec.it "sets the month" do
      Test.Spec.Assertions.shouldEqual
        (Data.Date.month (Data.DateTime.date dateTime'))
        Data.Date.Component.April
    Test.Spec.it "sets the minute" do
      Test.Spec.Assertions.shouldEqual
        (Data.Enum.fromEnum (Data.Time.minute (Data.DateTime.time dateTime')))
        30
