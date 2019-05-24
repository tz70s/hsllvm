module Main where

import Hsllvm

main :: IO ()
main = do
  putStrLn "Welcome to Hsllvm, this is a REPL mode."
  repl
