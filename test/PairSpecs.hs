module PairSpecs
    ( tests
    ) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Pair as P

instance (Arbitrary a) => Arbitrary (P.Pair a) where
    arbitrary = P.Pair <$> arbitrary <*> arbitrary

tests :: Test
tests = testGroup "Pong Specs"
            [ testProperty "Pair addition is commutative" prop_pair_add_commutative
            , testProperty "Pair addition is associative" prop_pair_add_associative]

prop_pair_add_commutative :: Pair Int -> Pair Int -> Bool
prop_pair_add_commutative a b =
    P.add a b == P.add b a

prop_pair_add_associative :: Pair Int -> Pair Int -> Pair Int -> Bool
prop_pair_add_associative a b c =
    (a `P.add` b) `P.add` c == a `P.add` (b `P.add` c)
