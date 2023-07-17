module Main (main) where

import qualified Test1 ()
import qualified Test2 ()

import           System.Exit (die)

main :: IO ()
main = die "This test always fails; the only point of it is that building it causes 'Test1' and 'Test2' to generate warnings, which are to be checked by other scripts."
