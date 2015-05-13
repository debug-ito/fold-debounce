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
  -- * Use the trigger
  send,
  -- * Finish the trigger
  close
) where

import Prelude hiding (init)
import Data.Monoid (Monoid)
import Data.Default (Default(def))

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

-- | A trigger to send input events to FoldDebounce.
data Trigger i o

-- | Create a FoldDebounce trigger.
new :: Args i o -- ^ mandatory parameters
    -> Opts i o -- ^ optional parameters
    -> IO (Trigger i o) -- ^ action to get the trigger. 
new = undefined

-- | 'new' with default 'Opts'
new' :: Args i o -> IO (Trigger i o)
new' a = new a def

-- | Send an input event.
send :: Trigger i o -> i -> IO ()
send = undefined

-- | Close and release the 'Trigger'.
close :: Trigger i o -> IO ()
close = undefined
