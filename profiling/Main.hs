module Main where

import Data.Foldable (foldl')
import qualified Data.Vector as Vector

main :: IO ()
main = do
  let
    pair n = [Add n, Sub n]
    ops = concatMap pair [0..10000]
    result = applyMany ops 0
  print result

--------------------------------------------------------------------------------
-- Code

data Op = Add Int | Sub Int
  deriving (Show, Eq)

addSlow :: Int -> Int -> Int
addSlow n k = Vector.foldl (+) k (Vector.replicate n 1)
{-# SCC addSlow #-}

add :: Int -> Int -> Int
add x y = y + x
{-# SCC add #-}

sub :: Int -> Int -> Int
sub x y = y - x
{-# SCC sub #-}

apply :: Op -> Int -> Int
apply (Add n) = addSlow n
apply (Sub n) = sub n
{-# SCC apply #-}

applyMany :: [Op] -> Int -> Int
applyMany ops k = foldl' (\n op -> apply op n) k ops
