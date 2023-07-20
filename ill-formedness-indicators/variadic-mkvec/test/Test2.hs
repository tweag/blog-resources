{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}

module Test2 (module Test2) where

import           Data.Vec (Vec)
import qualified Data.Vec.LiteralSyntax.Enclosed as X
import qualified Data.Vec.LiteralSyntax.EnclosedOI as XOI
import qualified Data.Vec.LiteralSyntax.EnclosedUnguarded as XU
import qualified Data.Vec.LiteralSyntax.Sentinel as Y
import qualified Data.Vec.LiteralSyntax.SentinelUnguarded as YU

-----

-- The Ill-Formedness Indicators and the overlapping instance both give good
-- error messages when a signature is provided but the syntax mistake was made.

enclosedGoodErrorMessage  :: a -> b -> c -> Vec [a, b, c]
enclosedGoodErrorMessage' :: a -> b -> c -> Vec [a, b, c]
enclosedGoodErrorMessage  a b c = X.litVec a b c
enclosedGoodErrorMessage' a b c = XOI.litVec a b c

sentinelGoodErrorMessage :: a -> b -> c -> Vec [a, b, c]
sentinelGoodErrorMessage a b c = Y.beginVec a b c

-----

-- The minimal definitions instead give bad error messages.

enclosedBadErrorMessage :: a -> b -> c -> Vec [a, b, c]
enclosedBadErrorMessage a b c = XU.litVec a b c

sentinelBadErrorMessage :: a -> b -> c -> Vec [a, b, c]
sentinelBadErrorMessage a b c = YU.beginVec a b c
