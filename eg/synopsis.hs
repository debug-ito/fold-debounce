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
  let send' = Fdeb.send trigger
  send' 1
  send' 2
  send' 3
  threadDelay 1000000 -- During this period, "value = 6" is printed.
  send' 4
  threadDelay 1000    -- Nothing is printed.
  send' 5
  threadDelay 1000000 -- During this period, "value = 9" is printed.
  Fdeb.close trigger
  
