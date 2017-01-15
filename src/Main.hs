module Main where

import Interpreter

main :: IO ()
main = runFileProgram "./program.txt"
