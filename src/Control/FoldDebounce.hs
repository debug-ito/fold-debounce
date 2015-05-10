-- |
-- Module: Control.FoldDebounce
-- Description: Fold multiple events that happen in a given period of time
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module Control.FoldDebounce (
  
) where

import Prelude hiding (init)
import Data.Monoid (Monoid)

-- How can we mix mandatory and optional named arguments??

data Settings i o = Settings {
  cb :: o -> IO (),
  -- ^ The callback to be called when the output event is emitted.
  --
  -- Default: do nothing

  folder :: o -> i -> o,
  -- ^ The binary operation of left-fold.

  init :: o,
  -- ^ The initial value of the left-fold.
  
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

def :: Settings i o
def = Settings {
  cb = undefined,
  folder = undefined,
  init = undefined,
  delay = 1000000,
  alwaysResetTimer = False
  }

settings :: (o -> i -> o) -> o -> Settings i o
settings = undefined

listSettings :: Settings i [i]
listSettings = undefined

monoidSettings :: Monoid i => Settings i i
monoidSettings = undefined

new :: Settings i o -> IO (i -> IO())
new = undefined


