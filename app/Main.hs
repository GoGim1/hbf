module Main where

import Interpreter

main :: IO ()
main = getLine >>= exec