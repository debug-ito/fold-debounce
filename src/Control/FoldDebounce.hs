-- |
-- Module: Control.FoldDebounce
-- Description: Fold multiple events that happen in a given period of time
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
-- Synopsis:
--
-- TBW
-- 
--
module Control.FoldDebounce (
  -- * Create the trigger
  new,
  new',
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
  close
) where

import Prelude hiding (init)
import Data.Monoid (Monoid)
import Control.Monad (when)
import Control.Applicative ((<|>), (<$>), (<*>))
import Control.Concurrent (ThreadId, killThread)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Data.Default (Default(def))
import Control.Concurrent.STM (TChan, registerDelay, readTVar, readTChan, newTVarIO, writeTChan, retry, atomically)

-- | Mandatory parameters for 'new'.
data Args i o = Args {
  cb :: o -> IO (),
  -- ^ The callback to be called when the output event is emitted.

  fold :: o -> i -> o,
  -- ^ The binary operation of left-fold.

  init :: o
  -- ^ The initial value of the left-fold.
}

-- $opts_accessors
-- You can update fields in 'Opts' via these accessors.
--



-- | Optional parameters for 'new'. You can get the default by 'def'
-- function.
data Opts i o = Opts {  
  delay :: Int,
  -- ^ The time (in microsecond) to wait after receiving an event
  -- before sending it, in case more events happen in the interim.
  --
  -- Default: 1 second (1000000)
  
  alwaysResetTimer :: Bool
  -- ^ Normally, when an event is received and it's the first of a
  -- series, a timer is started, and when that timer expires, all
  -- events are sent. If you set this initarg to a true value, then
  -- the timer is reset after each event is received.
}

instance Default (Opts i o) where
  def = Opts {
    delay = 1000000,
    alwaysResetTimer = False
    }

-- | 'Args' for stacks. Input events are accumulated in a list as if
-- it were a stack, i.e., the last event is at the head of the list.
forStack :: ([i] -> IO ()) -- ^ 'cb' field.
         -> Args i [i]
forStack = undefined

-- | 'Args' for monoids. Input events are appended to the tail.
forMonoid :: Monoid i
             => (i -> IO ()) -- ^ 'cb' field.
             -> Args i i
forMonoid = undefined

-- | TBW. '()' is a Monoid, by the way.
forVoid :: IO () -- ^ 'cb' field.
        -> Args i ()
forVoid = undefined

-- | A trigger to send input events to FoldDebounce.
data Trigger i o = Trigger {
  trigThread :: ThreadId,
  trigInput :: TChan i,
  trigAlive :: IORef Bool
}

-- | Create a FoldDebounce trigger.
new :: Args i o -- ^ mandatory parameters
    -> Opts i o -- ^ optional parameters
    -> IO (Trigger i o) -- ^ action to get the trigger. 
new args opts = uncurry Trigger <$> newThread args opts <*> newIORef True

-- | 'new' with default 'Opts'
new' :: Args i o -> IO (Trigger i o)
new' a = new a def

whenAlive :: Trigger i o -> IO () -> IO ()
whenAlive trig action = readIORef (trigAlive trig) >>= (\alive -> when alive action)

-- | Send an input event.
send :: Trigger i o -> i -> IO ()
send trig in_event = whenAlive trig $ atomically $ writeTChan (trigInput trig) in_event

-- | Close and release the 'Trigger'.
close :: Trigger i o -> IO ()
close trig = whenAlive trig $ do
  killThread (trigThread trig)
  writeIORef (trigAlive trig) True

---

newThread :: Args i o -> Opts i o -> IO (ThreadId, TChan i)
newThread args opts = undefined

waitInput :: TChan i      -- ^ input channel
          -> Maybe Int    -- ^ timeout in microseconds. If 'Nothing', it never times out.
          -> IO (Maybe i) -- ^ 'Nothing' if timed out
waitInput in_chan mtimeout = do
  timer <- maybe (newTVarIO False) registerDelay mtimeout
  atomically $ (Just <$> readTChan in_chan) <|> (checkTimeout timer)
  where
    checkTimeout timer = do
      timed_out <- readTVar timer
      if timed_out then return Nothing else retry
    
