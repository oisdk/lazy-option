{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}
{-# LANGUAGE CPP                #-}

module Data.Semigroup.Option.Lazy
  (Option(..)
  ,option)
  where

import           Prelude             (Eq, Ord, Read, Show,
                                      errorWithoutStackTrace, (.))

import           Data.Maybe          (Maybe (..), maybe)
import           Data.Ord            (Ordering (..), compare)

import           Data.Monoid         (Monoid (mappend, mempty))
import           Data.Semigroup      (Semigroup (stimes, (<>)))


import           Control.Applicative (Alternative (empty, (<|>)),
                                      Applicative (pure, (*>),(<*>)
#if MIN_VERSION_base(4,10,0)
                                                  ,liftA2
#endif
                                                  ))
import           Control.Monad       (Monad ((>>), (>>=)), MonadPlus)
import           Control.Monad.Fix   (MonadFix (mfix))
import           Data.Foldable       (Foldable (foldMap))
import           Data.Functor        (Functor (fmap), (<$>))
import           Data.Traversable    (Traversable (traverse))

import           Data.Data           (Data)
import           GHC.Generics        (Generic, Generic1)

newtype Option a = Option
    { getOption :: Maybe a
    } deriving (Eq,Ord,Show,Read,Data,Generic,Generic1)

instance Functor Option where
    fmap _ (Option Nothing)  = Option Nothing
    fmap f (Option (Just x)) = Option (Just (f x))

instance Applicative Option where
    pure = \x -> Option (Just x)

    Option (Just f) <*> xs = fmap f xs
    Option Nothing  <*> _  = Option Nothing

#if MIN_VERSION_base(4,10,0)
    liftA2 f (Option (Just x)) (Option (Just y)) = Option (Just (f x y))
    liftA2 _ _ _                                 = Option Nothing
#endif

    Option (Just _) *> x = x
    Option Nothing  *> _ = Option Nothing

instance Monad Option where
    Option (Just x) >>= k = k x
    _               >>= _ = Option Nothing
    (>>) = (*>)

instance Alternative Option where
    empty = Option Nothing

    Option Nothing <|> y = y
    x              <|> _ = x

instance MonadPlus Option

instance MonadFix Option where
    mfix f = Option (mfix (getOption . f))

instance Foldable Option where
    foldMap f (Option (Just m)) = f m
    foldMap _ (Option Nothing)  = mempty

instance Traversable Option where
    traverse f (Option (Just a)) = Option . Just <$> f a
    traverse _ (Option Nothing)  = pure (Option Nothing)

-- | Fold an 'Option' case-wise, just like 'maybe'.
option :: b -> (a -> b) -> Option a -> b
option n j (Option m) = maybe n j m

instance Semigroup a =>
         Semigroup (Option a) where
    Option Nothing  <> ys = ys
    Option (Just x) <> ys = Option (Just (option x (x <>) ys))

    stimes _ (Option Nothing) = Option Nothing
    stimes n (Option (Just a)) =
        case compare n 0 of
            LT -> errorWithoutStackTrace "stimes: Option, negative multiplier"
            EQ -> Option Nothing
            GT -> Option (Just (stimes n a))

instance Semigroup a =>
         Monoid (Option a) where
    mempty = Option Nothing
    mappend = (<>)
