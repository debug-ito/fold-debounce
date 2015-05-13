module Control.FoldDebounceSpec (main, spec) where

import Control.Concurrent.Chan (Chan,newChan,writeChan,readChan)
import Test.Hspec
import qualified Control.FoldDebounce as F

main :: IO ()
main = hspec spec

forFIFO :: ([Int] -> IO ()) -> F.Args Int [Int]
forFIFO cb = F.Args {
  F.cb = cb, F.fold = (\l v -> l ++ [v]), F.init = []
  }

fifoTrigger :: Chan [Int] -> F.Opts Int [Int] -> IO (F.Trigger Int)
fifoTrigger output opts = F.new (forFIFO $ writeChan output) opts

spec :: Spec
spec = do
  describe "new" $ do
    it "emits single output event for single input event" $ do
      output <- newChan
      trig <- fifoTrigger output F.def { F.delay = 50000 }
      F.send trig 10
      readChan output `shouldReturn` [10]
      F.close trig
