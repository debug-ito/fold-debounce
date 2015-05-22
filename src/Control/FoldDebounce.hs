-- |
-- Module: Control.FoldDebounce
-- Description: Fold multiple events that happen in a given period of time
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
-- Synopsis:
-- 
-- > module Main (main) where
-- > 
-- > import System.IO (putStrLn)
-- > import Control.Concurrent (threadDelay)
-- > 
-- > import qualified Control.FoldDebounce as Fdeb
-- > 
-- > printValue :: Int -> IO ()
-- > printValue v = putStrLn ("value = " ++ show v)
-- > 
-- > main :: IO ()
-- > main = do
-- >   trigger <- Fdeb.new Fdeb.Args { Fdeb.cb = printValue, Fdeb.fold = (+), Fdeb.init = 0 }
-- >                       Fdeb.def { Fdeb.delay = 500000 }
-- >   let send' = Fdeb.send trigger
-- >   send' 1
-- >   send' 2
-- >   send' 3
-- >   threadDelay 1000000 -- During this period, "value = 6" is printed.
-- >   send' 4
-- >   threadDelay 1000    -- Nothing is printed.
-- >   send' 5
-- >   threadDelay 1000000 -- During this period, "value = 9" is printed.
-- >   Fdeb.close trigger
-- 
-- This module is similar to "Control.Debounce". It debouces input
-- events and regulates the frequency at which the action (callback)
-- is executed.
--
-- The difference from "Control.Debounce" is:
--
-- * With "Control.Debounce", you cannot pass values to the callback
-- action. This module folds (accumulates) the input events (type @i@)
-- and passes the folded output event (type @o@) to the callback.
-- 
-- * "Control.Debounce" immediately runs the callback at the first
-- input event. This module just starts a timer at the first input,
-- and runs the callback when the timer expires.
--
-- __IMPORTANT NOTE__: currently you have to add @-threaded@ option to
-- ghc linker to use this module. I'm not sure if you can use it with
-- other compilers.
--
-- The API and documentation is borrowed from a Perl module called
-- AnyEvent::Debounce. See <https://metacpan.org/pod/AnyEvent::Debounce>
--
--
module Control.FoldDebounce (
  -- * Create the trigger
  new,
  Trigger,
  -- * Parameter types
  Args(..),
  Opts,
  def,
  -- ** Accessors for 'Opts'
  -- $opts_accessors
  delay,
  alwaysResetTimer,
  -- ** Preset parameters
  forStack,
  forMonoid,
  forVoid,
  -- * Use the trigger
  send,
  -- * Finish the trigger
  close,
  -- * Exception types
  OpException(..)
) where

import Prelude hiding (init)
import Data.Monoid (Monoid, mempty, mappend)
import Control.Monad (void)
import Control.Applicative ((<|>), (<$>))
import Control.Concurrent (forkFinally)
import Control.Exception (Exception, SomeException)
import Data.Typeable (Typeable)

import Data.Default (Default(def))
import Control.Concurrent.STM (TChan, readTChan, newTChanIO, writeTChan,
                               TVar, registerDelay, readTVar, writeTVar, newTVarIO,
                               STM, retry, atomically, throwSTM)
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)

-- | Mandatory parameters for 'new'.
data Args i o = Args {
  -- | The callback to be called when the output event is
  -- emitted. Note that this action is run in a different thread than
  -- the one calling 'send'.
  -- 
  -- The callback should not throw any exception. In this case, the
  -- 'Trigger' is abnormally closed, causing
  -- 'UnexpectedClosedException' when 'close'.
  cb :: o -> IO (),

  -- | The binary operation of left-fold. The left-fold is evaluated strictly.
  fold :: o -> i -> o,

  -- | The initial value of the left-fold.
  init :: o
}

-- $opts_accessors
-- You can update fields in 'Opts' via these accessors.
--



-- | Optional parameters for 'new'. You can get the default by 'def'
-- function.
data Opts i o = Opts {  
  -- | The time (in microsecond) to wait after receiving an event
  -- before sending it, in case more events happen in the interim.
  --
  -- Default: 1 second (1000000)
  delay :: Int,
  
  -- | Normally, when an event is received and it's the first of a
  -- series, a timer is started, and when that timer expires, all
  -- events are sent. If you set this parameter to True, then
  -- the timer is reset after each event is received.
  --
  -- Default: False
  alwaysResetTimer :: Bool
}

instance Default (Opts i o) where
  def = Opts {
    delay = 1000000,
    alwaysResetTimer = False
    }

-- | 'Args' for stacks. Input events are accumulated in a stack, i.e.,
-- the last event is at the head of the list.
forStack :: ([i] -> IO ()) -- ^ 'cb' field.
         -> Args i [i]
forStack mycb = Args { cb = mycb, fold = (flip (:)),  init = []}

-- | 'Args' for monoids. Input events are appended to the tail.
forMonoid :: Monoid i
             => (i -> IO ()) -- ^ 'cb' field.
             -> Args i i
forMonoid mycb = Args { cb = mycb, fold = mappend, init = mempty }

-- | 'Args' that discards input events. Although input events are not
-- folded, they still start the timer and activate the callback.
forVoid :: IO () -- ^ 'cb' field.
        -> Args i ()
forVoid mycb = Args { cb = const mycb, fold = (\_ _ -> ()), init = () }

-- | Internal input to the worker thread.
data ThreadInput i = TIEvent i -- ^ A new input event is made
                   | TIFinish  -- ^ the caller wants to finish the thread.

-- | Internal state of the worker thread.
data ThreadState = TSOpen -- ^ the thread is open and running
                 | TSClosedNormally -- ^ the thread is successfully closed
                 | TSClosedAbnormally SomeException -- ^ the thread is abnormally closed with the given exception.

-- | A trigger to send input events to FoldDebounce. You input data of
-- type @i@ to the trigger, and it outputs data of type @o@.
data Trigger i o = Trigger {
  trigInput :: TChan (ThreadInput i),
  trigState :: TVar ThreadState
}

-- | Create a FoldDebounce trigger.
new :: Args i o -- ^ mandatory parameters
    -> Opts i o -- ^ optional parameters
    -> IO (Trigger i o) -- ^ action to create the trigger. 
new args opts = do
  chan <- newTChanIO
  state_tvar <- newTVarIO TSOpen
  let putState = atomically . writeTVar state_tvar
  void $ forkFinally (threadAction args opts chan)
                     (either (putState . TSClosedAbnormally) (const $ putState TSClosedNormally))
  return $ Trigger chan state_tvar

getThreadState :: Trigger i o -> STM ThreadState
getThreadState trig = readTVar (trigState trig)

-- | Send an input event.
--
-- If the 'Trigger' is already closed, it throws
-- 'AlreadyClosedException'. If the 'Trigger' has been abnormally
-- closed, it throws 'UnexpectedClosedException'.
send :: Trigger i o -> i -> IO ()
send trig in_event = atomically $ do
  state <- getThreadState trig
  case state of
    TSOpen -> writeTChan (trigInput trig) (TIEvent in_event)
    TSClosedNormally -> throwSTM AlreadyClosedException
    TSClosedAbnormally e -> throwSTM $ UnexpectedClosedException e

-- | Close and release the 'Trigger'. If there is a pending output event, the event is fired immediately.
--
-- If the 'Trigger' has been abnormally closed, it throws 'UnexpectedClosedException'.
close :: Trigger i o -> IO ()
close trig = do
  atomically $ whenOpen $ writeTChan (trigInput trig) TIFinish
  atomically $ whenOpen $ retry -- wait for closing
  where
    whenOpen stm_action = do
      state <- getThreadState trig
      case state of
        TSOpen -> stm_action
        TSClosedNormally -> return ()
        TSClosedAbnormally e -> throwSTM $ UnexpectedClosedException e

-- | Exception type used by FoldDebounce operations
data OpException = AlreadyClosedException -- ^ You attempted to 'send' after the trigger is already 'close'd.
                 | UnexpectedClosedException SomeException -- ^ The 'SomeException' is thrown in the background thread.
                 deriving (Show, Typeable)

instance Exception OpException

---

threadAction :: Args i o -> Opts i o -> TChan (ThreadInput i) -> IO ()
threadAction args opts in_chan = threadAction' Nothing Nothing where 
  threadAction' mtimeout mout_event = do
    start_time <- getCurrentTime
    mgot <- waitInput in_chan mtimeout
    case mgot of
      Nothing -> fireCallback args mout_event >> threadAction' Nothing Nothing
      Just (TIFinish) -> fireCallback args mout_event
      Just (TIEvent in_event) -> do
        let next_out = doFold args mout_event in_event
        evaled_next_out <- next_out `seq` return next_out
        end_time <- getCurrentTime
        threadAction' (Just $ nextTimeout opts mtimeout start_time end_time) (Just evaled_next_out)
  
waitInput :: TChan a      -- ^ input channel
          -> Maybe Int    -- ^ timeout in microseconds. If 'Nothing', it never times out.
          -> IO (Maybe a) -- ^ 'Nothing' if timed out
waitInput in_chan mtimeout = do
  timer <- maybe (newTVarIO False) registerDelay mtimeout
  atomically $ (Just <$> readTChan in_chan) <|> (checkTimeout timer)
  where
    checkTimeout timer = do
      timed_out <- readTVar timer
      if timed_out then return Nothing else retry

fireCallback :: Args i o -> Maybe o -> IO ()
fireCallback _ Nothing = return ()
fireCallback args (Just out_event) = cb args out_event

doFold :: Args i o -> Maybe o -> i -> o
doFold args mcurrent in_event = let current = maybe (init args) id mcurrent
                                in fold args current in_event

nextTimeout :: Opts i o -> Maybe Int -> UTCTime -> UTCTime -> Int
nextTimeout opts morig_timeout start_time end_time
  | alwaysResetTimer opts = delay opts
  | otherwise = if raw_result < 0 then 0 else raw_result
  where
    elapsed_usec = round $ (* 1000000) $ toRational $ diffUTCTime end_time start_time
    raw_result = maybe (delay opts) (subtract elapsed_usec) morig_timeout
