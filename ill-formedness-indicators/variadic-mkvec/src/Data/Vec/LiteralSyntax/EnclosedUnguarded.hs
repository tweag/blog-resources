{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | See 'litVec'.
module Data.Vec.LiteralSyntax.EnclosedUnguarded (
    litVec,
    variadic,
  ) where

import           Data.Vec

-----

class MkLitVec xs a where mkLitVec :: Vec xs -> a

instance (MkLitVec (x : xs) a) => MkLitVec xs (x -> a) where
    mkLitVec acc = \x -> mkLitVec (VCons x acc)

newtype Variadic a = MkVariadic { {- | See 'litVec' -} variadic :: a }

instance (a ~ Vec xs', Rev '[] xs xs') => MkLitVec xs (Variadic a) where
    mkLitVec acc = MkVariadic (rev VNil acc)

-----

-- | A variadic vector constructor
--
-- Template: @'variadic' ('litVec' ...)@
--
-- If you forget to apply 'variadic', the inferred type and/or error messages
-- will include an inscrutable context laden with implementation details.
-- "Data.Vec.LiteralSyntax.Enclosed" improves that UX by a careful use of
-- 'GHC.TypeLits.TypeError' called an Ill-Formedness Indicator.
-- "Data.Vec.LiteralSyntax.EnclosedOI" only partially acheives that
-- improvement, using `-XOverlappingInstances` instead of Ill-Formedness
-- Indicators.
litVec :: MkLitVec '[] a => a
litVec = mkLitVec VNil
