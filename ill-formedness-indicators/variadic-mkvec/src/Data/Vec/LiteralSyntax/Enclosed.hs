{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | See 'litVec'.
module Data.Vec.LiteralSyntax.Enclosed (
    litVec,
    variadic,
  ) where

import           Data.Proxy (Proxy (Proxy))
import           Data.Vec
import           Data.Void (Void)
import           GHC.TypeLits (ErrorMessage (Text), TypeError)

-----

class MkLitVec (ifi :: Void) xs a where mkLitVec :: Proxy ifi -> Vec xs -> a

instance (MkLitVec ifi (x : xs) a) => MkLitVec ifi xs (x -> a) where
    mkLitVec ifi acc = \x -> mkLitVec ifi (VCons x acc)

newtype Variadic a = MkVariadic { {- | See 'litVec' -} variadic :: a }

instance (a ~ Vec xs', Rev '[] xs xs') => MkLitVec ifi xs (Variadic a) where
    mkLitVec _ifi acc = MkVariadic (rev VNil acc)

-----

-- | An alias of 'TypeError'
--
-- This family avoids a compile-time error in the /definition/ of 'beginVec'.
-- That is its only use. <https://gitlab.haskell.org/ghc/ghc/-/issues/20241>
-- should happily supplant this in future GHCs.
type family DelayTypeError (msg :: ErrorMessage) :: Void where
    DelayTypeError msg = TypeError msg

-- | This could include more details of the user's error, by taking type
-- parameters and including them in the message via 'GHC.TypeLits.ShowType'.
type LitVecErrMsg = Text "Likely accidental use of `litVec' outside of `variadic'!"

-- | A variadic vector constructor
--
-- Template: @'variadic' ('litVec' ...)@
--
-- Any use of 'litVec' outside of the 'variadic' syntactic context---even in a
-- closed-world context!---will cause a compile-time error. A determined user
-- could subvert that compile-time error, but we don't expect any user to
-- desire that or do so accidentally.
litVec :: MkLitVec (DelayTypeError LitVecErrMsg) '[] a => a
litVec = mkLitVec (Proxy :: Proxy (DelayTypeError LitVecErrMsg)) VNil
