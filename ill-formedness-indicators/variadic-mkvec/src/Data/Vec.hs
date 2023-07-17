{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vec (
    Vec (VCons, VNil),
    Rev (rev),
  ) where

import           Data.Kind (Type)

-----

-- | A bog standard Haskell heterogenous vector data type.
data Vec :: [Type] -> Type where
    VNil  ::                Vec '[]
    VCons :: x -> Vec xs -> Vec (x : xs)

instance Show (Vec '[]) where show VNil = "VNil"

instance (Show x, Show (Vec xs)) => Show (Vec (x : xs)) where
    showsPrec p (VCons x xs) =
          showParen (p > 10)
        $ showString "VCons " . showsPrec 11 x . showChar ' ' . showsPrec 11 xs

-----

-- | I don't use a @Reversed@ type family for the sake of simplicity.
--
-- Somewhat surprisingly, I also don't use a functional dependency. I suspect
-- the fact that this class is only used by 'MkLitVec', which has an
-- Ill-Formedness Indicator, means that a fundep would never be useful, since
-- the type indices will necesssarily be concrete.
class Rev acc xs acc' where rev :: Vec acc -> Vec xs -> Vec acc'

instance (acc' ~ acc) => Rev acc '[] acc' where
    rev acc = \VNil -> acc

instance Rev (x : acc) xs acc' => Rev acc (x : xs) acc' where
    rev acc = \(VCons x xs) -> rev (VCons x acc) xs
