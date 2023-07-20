{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | See 'litVec'.
module Data.Vec.LiteralSyntax.EnclosedOI (
    litVec,
    variadic,
  ) where

import           Data.Vec
import           GHC.TypeLits (ErrorMessage (Text), TypeError)

-----

class MkLitVec xs a where mkLitVec :: Vec xs -> a

instance {-# OVERLAPS #-} (MkLitVec (x : xs) a) => MkLitVec xs (x -> a) where
    mkLitVec acc = \x -> mkLitVec (VCons x acc)

newtype Variadic a = MkVariadic { {- | See 'litVec' -} variadic :: a }

instance {-# OVERLAPS #-} (a ~ Vec xs', Rev '[] xs xs') => MkLitVec xs (Variadic a) where
    mkLitVec acc = MkVariadic (rev VNil acc)

type LitVecErrMsg = Text "Likely accidental use of `litVec' outside of `variadic'!"
instance TypeError LitVecErrMsg => MkLitVec xs a where mkLitVec = error "unreachable"

-----

-- | A variadic vector constructor
--
-- Template: @'variadic' ('litVec' ...)@
--
-- If you forget to apply 'variadic' in an open-world context (eg in a
-- definition with no signature), the inferred type will include an inscrutable
-- context laden with implementation details. "Data.Vec.LiteralSyntax.Enclosed"
-- improves that UX by careful use of 'GHC.TypeLits.TypeError'.
litVec :: MkLitVec '[] a => a
litVec = mkLitVec VNil
