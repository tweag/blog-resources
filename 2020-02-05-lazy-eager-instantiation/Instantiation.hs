{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}

module Instantiation where

-- | Define a synonym for `id`
myId = id

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-- | Instantiate the polymorphic `id` synonym
id_int :: Int -> Int
-- This does not compile:
-- id_int = myId @Int
-- While this does work:
id_int = id @Int

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-- | Showcase simplified sumbsumption
f :: forall a. a -> forall b. b -> b
f x = id

g = f
-- Under GHC 8.10, the inferred type for `g` becomes
-- g :: forall {a} {b}. a -> b -> b

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

h = \ x -> f
-- Under GHC 8.10, the inferred type for `h` becomes
-- h :: forall {a} {b} {c} a -> b -> c -> c

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-- | Non-recursive, single-equation function
bar1 True  = \ x -> id

-- | Multi-equation function
bar2 True  = \ x -> id
bar2 False = error "Impossible case for reasons"

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

funny :: (forall a. a -> a) -> ()
funny _ = ()

-- | Another non-recursive, single-equation function
baz1 True  = funny

-- | Multi-equation function: gets rejected!
-- baz2 True  = funny
-- baz2 False = funny

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-- | Synonym for `(+)` to showcase the monomorphism restriction
plus = (+)

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-- | Function which we expect to diverge
diverge = let !x = undefined in ()

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-- | Functions using implicit arguments
x :: (?i :: Int) => Int
x = ?i

y :: (?i :: Int) => Int
y = let ?i = 5 in x

z :: Int
z = let ?i = 6 in y
