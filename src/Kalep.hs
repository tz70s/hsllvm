module Kalep
  ( compile
  , repl
  )
where

import Kalep.Parser
import System.IO

compile :: IO ()
compile = putStrLn "Start compiler."

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  interact_
 where
  interact_ = do
    putStr "kalep> "
    line <- getLine
    case line of
      s | s == ":quit" || s == ":q" -> return ()
      _                             -> processSingleline line >> interact_

processSingleline :: String -> IO ()
processSingleline line = do
  let res = parseToplevel line
  case res of
    Left  err -> print err
    Right ex  -> mapM_ print ex
