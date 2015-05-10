module Main (main) where

import System.IO (putStrLn)
import Control.Concurrent (threadDelay)

import qualified Control.FoldDebounce as Fdeb

printValue :: Int -> IO ()
printValue v = putStrLn ("value = " ++ show v)

main :: IO ()
main = do
  trigger <- Fdeb.new Fdeb.Args { Fdeb.cb = printValue, Fdeb.fold = (+), Fdeb.init = 0 }
                      Fdeb.def { Fdeb.delay = 500000 }
  trigger 1
  trigger 2
  trigger 3
  threadDelay 1000000 -- During this, "value = 6" is printed.
  trigger 4
  threadDelay 1000    -- Nothing is printed.
  trigger 5
  threadDelay 1000000 -- During this, "value = 9" is printed.
  
