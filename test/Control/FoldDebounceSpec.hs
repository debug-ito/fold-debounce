module Control.FoldDebounceSpec (main, spec) where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan,newTChan,writeTChan,readTChan,tryPeekTChan,tryReadTChan)
import Control.Concurrent (threadDelay)
import Test.Hspec
import qualified Control.FoldDebounce as F

main :: IO ()
main = hspec spec

forFIFO :: ([Int] -> IO ()) -> F.Args Int [Int]
forFIFO cb = F.Args {
  F.cb = cb, F.fold = (\l v -> l ++ [v]), F.init = []
  }

callbackToTChan :: TChan [Int] -> [Int] -> IO ()
callbackToTChan output = atomically . writeTChan output

fifoTrigger :: F.Opts Int [Int] -> IO (F.Trigger Int [Int], TChan [Int])
fifoTrigger opts = do
  output <- atomically $ newTChan
  trig <- F.new (forFIFO $ callbackToTChan output) opts
  return (trig, output)

spec :: Spec
spec = do
  describe "Trigger" $ do
    it "emits single output event for single input event" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 50000 }
      F.send trig 10
      atomically (readTChan output) `shouldReturn` [10]
      F.close trig
    it "emits single FIFO list for multiple input events" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 50000 }
      F.send trig 10
      F.send trig 20
      F.send trig 30
      atomically (readTChan output) `shouldReturn` [10,20,30]
      F.close trig
    it "waits for more events that follow the first event" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 500000 }
      F.send trig 10
      threadDelay 30000
      atomically (tryPeekTChan output) `shouldReturn` Nothing
      threadDelay 500000
      atomically (tryReadTChan output) `shouldReturn` Just [10]
      F.close trig
    it "emits the output event 'delay' interval after the first input event (alwaysResetTimer = False)" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 500000 }
      F.send trig 10
      threadDelay 100000
      F.send trig 20
      threadDelay 100000
      F.send trig 30
      threadDelay 100000
      F.send trig 40
      threadDelay 100000
      F.send trig 50
      threadDelay 200000
      atomically (tryReadTChan output) `shouldReturn` Just [10,20,30,40,50]
      F.close trig
    it "emits the output event 'delay' interval after the last input event (alwaysResetTimer = True)" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 500000 }
      F.send trig 10
      threadDelay 100000
      F.send trig 20
      threadDelay 100000
      F.send trig 30
      threadDelay 100000
      F.send trig 40
      threadDelay 100000
      F.send trig 50
      threadDelay 200000
      atomically (tryReadTChan output) `shouldReturn` Nothing
      threadDelay 400000
      atomically (tryReadTChan output) `shouldReturn` Just [10,20,30,40,50]
    it "does nothing after closed" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 10000 }
      F.send trig 10
      atomically (readTChan output) `shouldReturn` [10]
      F.close trig
      F.send trig 20
      threadDelay 200000
      atomically (tryReadTChan output) `shouldReturn` Nothing
  describe "forStack" $ do
    it "creates a stacked FoldDebounce" $ do
      output <- atomically $ newTChan
      trig <- F.new (F.forStack $ callbackToTChan output)
                    F.def { F.delay = 50000 }
      F.send trig 10
      F.send trig 20
      F.send trig 30
      atomically (readTChan output) `shouldReturn` [30,20,10]
      F.close trig
  describe "forMonoid" $ do
    it "creates a FoldDebounce for Monoids" $ do
      output <- atomically $ newTChan
      trig <- F.new (F.forMonoid $ callbackToTChan output)
                    F.def { F.delay = 50000 }
      F.send trig [10]
      F.send trig [20]
      F.send trig [30]
      atomically (readTChan output) `shouldReturn` [10,20,30]
      F.close trig
      
