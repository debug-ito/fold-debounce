-- |
-- Module: Control.FoldDebounce
-- Description: Fold multiple events that happen in a given period of time
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
-- Synopsis:
-- 
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
-- >   trigger 1
-- >   trigger 2
-- >   trigger 3
-- >   threadDelay 1000000 -- During this, "value = 6" is printed.
-- >   trigger 4
-- >   threadDelay 1000    -- Nothing is printed.
-- >   trigger 5
-- >   threadDelay 1000000 -- During this, "value = 9" is printed.
--
--
module Control.FoldDebounce (
  -- * Create the trigger
  new,
  new',
  -- * Parameter types
  Args(..),
  Opts,
  def,
  -- ** Accessors for 'Opts'
  delay,
  alwaysResetTimer,
  -- ** Preset parameters
  forStack,
  forMonoid
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

-- | Create a FoldDebounce trigger action.
new :: Args i o -- ^ mandatory parameters
    -> Opts i o -- ^ optional parameters
    -> IO (i -> IO()) -- ^ action to get the trigger. You can use the
                      -- trigger to input an event to the
                      -- FoldDebounce.
new = undefined

-- | 'new' with default 'Opts'
new' :: Args i o -> IO (i -> IO ())
new' a = new a def

