import System.IO (hFlush, stdout)
import Control.Monad (forever)
import Utils (evalIO)

prompt = putStr ">>> " >> hFlush stdout >> getLine >>= evalIO

main = do 
  putStrLn "Awesome-sauce Hatchet-wielding Hitchhiker python interpreter!"
  putStrLn "Currently only (some) python expressions are supported."
  forever prompt
