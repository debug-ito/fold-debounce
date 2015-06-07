module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (void)

import qualified Control.FoldDebounce as F
import Data.Conduit (Source, Sink, await, yield, ($$))
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate, release, liftResourceT, resourceForkIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (TChan, newTChanIO, writeTChan, readTChan, atomically)

main = undefined

debounceSource :: (MonadResource mi, Monad mo) => F.Opts i o -> (o -> i -> o) -> o -> Source mi i -> mi (Source mo o)
debounceSource opts f acc src = do
  out_chan <- lift $ newTChanIO
  (key_trig, trig) <- allocate (F.new F.Args { F.cb = atomically . writeTChan out_chan . Just,
                                        F.fold = f, F.init = acc }
                                opts)
                               F.close
  let retSource = do
        mgot <- lift $ atomically $ readTChan out_chan
        case mgot of
          Nothing -> return ()
          Just got -> yield got >> retSource
  void $ liftResourceT $ resourceForkIO $ lift (src $$ trigSink trig key_trig out_chan)
  return retSource

-- (src $$ ...) :: (MonadResource mi) => mi ()
-- lift (src $$ .. ) :: (MonadResource mi) => t mi ()
-- resourceForkIO :: (MonadBaseControl IO m) => ResourceT m () -> ResourceT m ThreadId
--
-- MonadBaseControl IO の制約が満たせないかも。追加すればいいか。
-- 


trigSink :: (MonadResource mi) => F.Trigger i o -> ReleaseKey -> TChan (Maybe a) -> Sink i mi ()
trigSink trig key_trig out_chan = trigSink' where
  trigSink' = do
    mgot <- await
    case mgot of
      Nothing -> do
        release key_trig
        liftIO $ atomically $ writeTChan out_chan Nothing -- should it be guaranteed by bracket-like feature? maybe it should be in "allocate" arg.
        return ()
      Just got -> do
        liftIO $ F.send trig got
        trigSink'


-- foldDebounceC :: MonadResource m => F.Opts -> (o -> i -> o) -> o -> Conduit i m o
-- foldDebounceC opts f acc = bracketP createTrigger closeTrigger makeConduit where
--   createTrigger = do
--     out_chan <- newTChanIO
--     trigger <- F.new F.Args { F.cb = atomically . writeTChan out_chan, F.fold = f, F.init = acc} opts
--     return (trigger, out_chan)
--   closeTrigger (trig, _) = F.close trig
--   makeConduit (trig, out_chan) = do
--     (Left <$> await) <|> (Right <$> (lift $ readTChan out_chan)) -- ConduitM i o STM rをどう実行すればよい？？

-- 参考。やはりstm-conduitを使うべきなのか？
-- 
-- http://stackoverflow.com/questions/15594556/fusing-conduits-with-multiple-inputs
-- http://stackoverflow.com/questions/15594556/fusing-conduits-with-multiple-inputs
-- http://stackoverflow.com/questions/16757060/one-processing-conduit-2-io-sources-of-the-same-type
-- http://therning.org/magnus/posts/2015-01-22-000-combining-inputs-in-conduit.html
-- http://bjoeris.com/2013/03/24/multi-input-conduits/

