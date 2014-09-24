{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Data.Fix2 where

newtype Fix f = Fix { unFix :: f (Fix f)}
data Fix2 f g = In2 (f (Fix2 f g) (Fix2 g f))

instance Show (f (Fix2 f g) (Fix2 g f)) => Show (Fix2 f g) where
         show (In2 x) = "(" ++ show x ++ ")"

instance Eq (f (Fix2 f g) (Fix2 g f)) => Eq (Fix2 f g) where
         (In2 x) == (In2 y) = x == y

instance Ord (f (Fix2 f g) (Fix2 g f)) => Ord (Fix2 f g) where
         (In2 x) `compare` (In2 y) = x `compare` y
