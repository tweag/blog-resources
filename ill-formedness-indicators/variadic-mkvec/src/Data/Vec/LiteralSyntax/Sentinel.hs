{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | See 'beginVec'.
module Data.Vec.LiteralSyntax.Sentinel (
    beginVec,
    endVec,
  ) where

import           Data.Proxy (Proxy (Proxy))
import           Data.Vec
import           Data.Void (Void)
import           GHC.TypeLits (ErrorMessage (Text), TypeError)

-----

class MkBeginVec (ifi :: Void) xs a where mkBeginVec :: Proxy ifi -> Vec xs -> a

instance (MkBeginVec ifi (x : xs) a) => MkBeginVec ifi xs (x -> a) where
    mkBeginVec ifi acc = \x -> mkBeginVec ifi (VCons x acc)

data Sentinel = Sentinel

-- | Incoherent so that GHC will select the other 'MkBeginVec' instance even when
-- the function domain is a type variable (despite the possibility that it
-- could later unify with 'Sentinel'). Such greedy instance selection is
-- acceptable, even desirable, in this case, since we intend for 'beginVec' and
-- 'endVec' to be treated as a /syntactic/ begin-end pair, never incrementally
-- assembled via combinators, never abstracted over, etc.
--
-- The syntactic intention is enforced by the Ill-Formedness Indicator.
instance {-# INCOHERENT #-} (a ~ Vec xs', Rev '[] xs xs') => MkBeginVec ifi xs (Sentinel -> a) where
    mkBeginVec _ifi acc = \Sentinel -> rev VNil acc

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
type LitVecErrMsg = Text "Likely accidental use of `beginVec' without `endVec'!"

-- | A variadic vector constructor
--
-- Template: @'beginVec' ... 'endVec'@
--
-- Any use of 'beginVec' that is not applied to 'endVec'---even in a
-- closed-world context!---will cause a compile-time error. A determined user
-- could subvert that compile-time error, but we don't expect any user to
-- desire that or do so accidentally.
beginVec :: MkBeginVec (DelayTypeError LitVecErrMsg) '[] a => a
beginVec = mkBeginVec (Proxy :: Proxy (DelayTypeError LitVecErrMsg)) VNil

-- | See 'beginVec'.
endVec :: Sentinel
endVec = Sentinel
