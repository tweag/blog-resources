{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wmissing-signatures #-}

module Test1 (module Test1) where

import qualified Data.Vec.LiteralSyntax.Enclosed as X
import qualified Data.Vec.LiteralSyntax.EnclosedOI as XOI
import qualified Data.Vec.LiteralSyntax.EnclosedUnguarded as XU
import qualified Data.Vec.LiteralSyntax.Sentinel as Y
import qualified Data.Vec.LiteralSyntax.SentinelUnguarded as YU

-----

-- All of the definitions have the perfect inferred signature when the syntax
-- is used as intended.

enclosedGoodInferredSignature  a b c = X.variadic (X.litVec a b c)
enclosedGoodInferredSignature1 a b c = XOI.variadic (XU.litVec a b c)
enclosedGoodInferredSignature2 a b c = XU.variadic (XU.litVec a b c)

sentinelGoodInferredSignature  a b c = Y.beginVec a b c Y.endVec 
sentinelGoodInferredSignature' a b c = YU.beginVec a b c YU.endVec 

-----

-- In the open-world context, we get a bad inferred signature from the syntax
-- mistake instead of a good error message because these definitions did not
-- use Ill-Formedness Indicators.

enclosedBadInferredSignature  a b c = XU.litVec a b c
enclosedBadInferredSignature' a b c = XOI.litVec a b c

sentinelBadInferredSignature a b c = YU.beginVec a b c
