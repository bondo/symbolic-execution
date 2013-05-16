module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.IO.Error (ioError, userError)

import Language.Python.Common.Pretty (pretty, render)

import Utils (instrumentModule)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) . ioError $ userError "Two arguments required: input file and output file."
  let [infile, outfile] = args
  contents <- readFile infile
  case instrumentModule contents of
    Left err  -> ioError . userError $ "Program transformation failed: " ++ err
    Right mod -> writeFile outfile  . render . pretty $ mod
-- > runhaskell Main.hs tests/simple1.py tests/simple1.out.py && echo 'Success! Program transformed.'
