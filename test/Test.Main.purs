module Test.Main (main) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect as Effect
import Option as Opt
import Test.Assert (assert)

someOption :: Opt.Option ( foo :: Boolean, bar :: Int )
someOption = Opt.empty

bar1 :: Int
bar1 = 31

anotherOption :: Opt.Option ( foo :: Boolean, bar :: Int )
anotherOption = Opt.set (SProxy :: _ "bar") bar1 someOption

main :: Effect.Effect Unit
main = do
  assert $ Just bar1 == (Opt.get (SProxy :: _ "bar") anotherOption)
