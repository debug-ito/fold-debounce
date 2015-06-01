# Revision history for fold-debounce

## 0.2.0.0  -- 2015-06-01

* The debounce period now starts from the time when the first input
  event is sent. Previously it was the time when the first input event
  is popped from the input queue. That lead to weird behavior when the
  input traffic was too intense.

* Now this module works without -threaded ghc option thanks to new
  dependency on 'stm-delay' package.

* Bumped the major version due to the new dependency.


## 0.1.0.0  -- 2015-05-22

* First version. Released on an unsuspecting world.
