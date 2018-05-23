{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           Test.ChasingBottoms
import           Test.Framework                       as Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Poly

import qualified Data.Monoid                          (First (..))
import qualified Data.Semigroup                       (First (..))
import           Data.Semigroup.Option.Lazy

import           Data.List.NonEmpty                   (NonEmpty (..))

instance Arbitrary a => Arbitrary (Option a) where
    arbitrary = fmap Option arbitrary
    shrink = map Option . shrink . getOption

instance Arbitrary a =>
         Arbitrary (NonEmpty a) where
    arbitrary = fmap (uncurry (:|)) arbitrary
    shrink = map (uncurry (:|)) . shrink . toTuple
      where
        toTuple (x :| xs) = (x, xs)

instance EqProp a => EqProp (Option a) where
    Option x =-= Option y = x =-= y

instance EqProp a => EqProp (NonEmpty a) where
    (x :| xs) =-= (y :| ys) = (x,xs) =-= (y,ys)

instance EqProp A where
    (=-=) = (===)

instance EqProp B where
    (=-=) = (===)

instance EqProp C where
    (=-=) = (===)

sameAsFirst :: Int -> [Bool] -> Property
sameAsFirst n' xs' = cover bottMonoid 10 "_|_" (bottMonoid === bottSemigr)
  where
    n = abs n' `mod` 3
    xs = take n xs'
    bottMonoid = isBottom (monoidFold (botList xs))
    bottSemigr = isBottom (semigrFold (botList xs))
    monoidFold = Data.Monoid.getFirst . foldMap (Data.Monoid.First . Just)
    semigrFold =
        fmap Data.Semigroup.getFirst .
        getOption .
        foldMap (Option . Just . Data.Semigroup.First)

botList :: [Bool] -> [()]
botList []         = bottom
botList (True:xs)  = () : botList xs
botList (False:xs) = bottom : botList xs

testBatch :: TestBatch -> Framework.Test
testBatch (name, tests) = testGroup name (map (uncurry testProperty) tests)

main :: IO ()
main =
    defaultMain
        [ testBatch (semigroup (undefined :: Option (NonEmpty A)))
        , testBatch (monoid (undefined :: Option (NonEmpty B)))
        , testBatch (functor (undefined :: Option (A, B, C)))
        , testBatch (applicative (undefined :: Option (A, B, C)))
        , testBatch (monad (undefined :: Option (A, B, C)))
        , testBatch (alternative (undefined :: Option A))
        , testBatch (monadOr (undefined :: Option (A, B)))
        , testBatch (traversable (undefined :: Option (A, B, [C])))
        , testBatch (monadFunctor (undefined :: Option (A, B)))
        , testBatch (monadApplicative (undefined :: Option (A, B)))
        , testProperty "Same semantics as Data.Monoid.First" sameAsFirst ]
