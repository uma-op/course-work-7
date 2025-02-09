module DebugUtils where

import qualified Debug.Trace as Trace

trace :: Show a => a -> a
trace v = Trace.trace (show v) v
