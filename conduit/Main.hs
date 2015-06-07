{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Applicative (Applicative)
import Control.Monad (void)

import qualified Control.FoldDebounce as F
import Data.Conduit (Source, Sink, await, yield, ($$))
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Resource (MonadBaseControl, MonadThrow, 
                                     allocate, register, resourceForkIO, runResourceT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM (newTChanIO, writeTChan, readTChan, atomically)

import Control.Concurrent (threadDelay)
import qualified Data.Conduit.List as CL

ticker :: Int -> Int -> Source IO Int
ticker delay_ms maximum = ticker' 0 where
  ticker' count = if count >= maximum then return () else do
    yield count
    liftIO $ threadDelay (delay_ms * 1000)
    ticker' (count + 1)

main :: IO ()
main = do
  let deb = debounceSource (F.forStack $ const $ return ()) F.def {F.delay = 5000000}
  (deb $ ticker 1000 20) $$ CL.mapM_ print

debounceSource :: (MonadThrow m, MonadBase IO m, MonadIO m, Applicative m, MonadBaseControl IO m)
               => F.Args i o -> F.Opts i o -> Source m i -> Source m o
debounceSource args opts src = do
  out_chan <- liftIO $ newTChanIO
  let retSource = do
        mgot <- liftIO $ atomically $ readTChan out_chan
        case mgot of
          Nothing -> return ()
          Just got -> yield got >> retSource
  lift $ runResourceT $ do
    (_, trig) <- allocate (F.new args { F.cb = atomically . writeTChan out_chan . Just }
                                 opts)
                          (F.close)
    void $ register $ atomically $ writeTChan out_chan Nothing
    void $ resourceForkIO $ lift (src $$ trigSink trig)
  retSource

trigSink :: (MonadIO m) => F.Trigger i o -> Sink i m ()
trigSink trig = trigSink' where
  trigSink' = do
    mgot <- await
    case mgot of
      Nothing -> return ()
      Just got -> do
        liftIO $ F.send trig got
        trigSink'

