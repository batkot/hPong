import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import qualified PairSpecs as Pair

main :: IO ()
main = defaultMain tests

tests = [ Pair.tests ]
