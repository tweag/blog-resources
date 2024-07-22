{-# LANGUAGE ScopedTypeVariables #-} -- to bring `m` into scope in
                                     -- instance `KnownNat (S m)` and `mkMod`
{-# LANGUAGE TypeApplications #-}    -- to be able to write `get @m`
{-# LANGUAGE AllowAmbiguousTypes #-} -- for the type of `get`
{-# LANGUAGE KindSignatures #-}      -- to allow the `:: Nat` signatures
{-# LANGUAGE DataKinds #-}           -- to allow `Z` and `S` at compile-time

module ThreeIsNotAType where

import Data.Char

class Shoutable a where
  shout :: a -> String
instance Shoutable Char where
  shout x = [Data.Char.toUpper x]
instance Shoutable a => Shoutable [a] where
  shout xs = concatMap shout xs
instance Shoutable Int where
  shout = show

shoutIt :: Shoutable a => a -> IO ()
shoutIt x = putStrLn (shout x)

main :: IO ()
main = shoutIt "hello"

-----------------------

data Nat = Z | S Nat

newtype Modulus (n :: Nat) = Mod Integer

-- KnownNat n says that the compile-time value of `n` is available at run-time.
class KnownNat (n :: Nat) where
  get :: Integer   -- an ambiguous type
instance KnownNat Z where
  get = 0
instance KnownNat m => KnownNat (S m) where
  get = 1 + get @m

-- Pack an Integer into a Modulus, taking the `mod` as appropriate.
mkMod :: forall m. KnownNat m => Integer -> Modulus m
mkMod x = Mod (x `mod` get @m)

-- Define arithmetic operations:
instance KnownNat n => Num (Modulus n) where
  Mod a + Mod b  = mkMod (a + b)
  Mod a - Mod b  = mkMod (a - b)
  Mod a * Mod b  = mkMod (a * b)
  abs m          = m   -- Moduli are not negative.
  signum (Mod x) = mkMod (signum x)
  fromInteger n  = mkMod n
  negate (Mod x) = mkMod (-x)
