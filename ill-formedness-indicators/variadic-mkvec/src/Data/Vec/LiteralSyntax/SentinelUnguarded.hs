{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | See 'beginVec'.
module Data.Vec.LiteralSyntax.SentinelUnguarded (
    endVec,
    beginVec,
  ) where

import           Data.Vec

-----

class MkBeginVec xs a where mkBeginVec :: Vec xs -> a

instance (MkBeginVec (x : xs) a) => MkBeginVec xs (x -> a) where
    mkBeginVec acc = \x -> mkBeginVec (VCons x acc)

data Sentinel = Sentinel

-- | Incoherent so that GHC will select the other 'MkBeginVec' instance even when
-- the function domain is a type variable (despite the possibility that it
-- could later unify with 'Sentinel'). Such greedy instance selection is
-- acceptable, even desirable, in this case, since we intend for 'beginVec' and
-- 'endVec' to be treated as a /syntactic/ begin-end pair, never incrementally
-- assembled via combinators, never abstracted over, etc.
--
-- The syntactic intention would be enforced by an Ill-Formedness Indicator,
-- like the one added in "Data.Vec.LiteralSyntax.Sentinel".
--
-- "Data.Vec.LiteralSyntax.EnclosedUnguarded" avoids the need for this
-- incoherency, by using a slightly different shape of syntactic pair.
instance {-# INCOHERENT #-} (a ~ Vec xs', Rev '[] xs xs') => MkBeginVec xs (Sentinel -> a) where
    mkBeginVec acc = \Sentinel -> rev VNil acc

-----

-- | A variadic vector constructor
--
-- Template: @'beginVec' ... 'endVec'@
--
-- If you forget to supply 'endVec', the inferred type and/or error messages
-- will include an inscrutable context laden with implementation details.
-- That's the motivation of adding an Ill-Formedness Indicator in
-- "Data.Vec.LiteralSyntax.Sentinel".
beginVec :: MkBeginVec '[] a => a
beginVec = mkBeginVec VNil

-- | See 'beginVec'.
endVec :: Sentinel
endVec = Sentinel
