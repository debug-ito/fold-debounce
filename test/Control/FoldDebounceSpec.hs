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

fifoTrigger :: F.Opts Int [Int] -> IO (F.Trigger Int, Chan [Int])
fifoTrigger opts = do
  output <- newChan
  trig <- F.new (forFIFO $ writeChan output) opts
  return (trig, output)

spec :: Spec
spec = do
  describe "new" $ do
    it "emits single output event for single input event" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 50000 }
      F.send trig 10
      readChan output `shouldReturn` [10]
      F.close trig
    it "multiple events in a FIFO list" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 50000 }
      F.send trig 10
      F.send trig 20
      F.send trig 30
      readChan output `shouldReturn` [10,20,30]
      F.close trig
