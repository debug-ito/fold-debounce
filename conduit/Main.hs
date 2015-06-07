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

main :: IO ()
main = undefined

debounceSource :: (MonadThrow m, MonadBase IO m, MonadIO m, Applicative m, MonadBaseControl IO m)
               => F.Opts i o -> (o -> i -> o) -> o -> Source m i -> Source m o
debounceSource opts f acc src = do
  out_chan <- liftIO $ newTChanIO
  let retSource = do
        mgot <- liftIO $ atomically $ readTChan out_chan
        case mgot of
          Nothing -> return ()
          Just got -> yield got >> retSource
  lift $ runResourceT $ do
    (_, trig) <- allocate (F.new F.Args { F.cb = atomically . writeTChan out_chan . Just,
                                                 F.fold = f, F.init = acc }
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

