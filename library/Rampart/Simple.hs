module Rampart.Simple
  ( Interval
  , toInterval
  , Relation(..)
  , relate
  )
where

import Prelude

import Rampart hiding (Relation(..), relate)
import qualified Rampart

data Relation = Before | Concurrent | After

relate :: Ord a => Interval a -> Interval a -> Relation
relate a b = case Rampart.relate a b of
  Rampart.Before -> Before
  Rampart.Meets -> Concurrent
  Rampart.Overlaps -> Concurrent
  Rampart.FinishedBy -> Concurrent
  Rampart.Contains -> Concurrent
  Rampart.Starts -> Concurrent
  Rampart.Equal -> Concurrent
  Rampart.StartedBy -> Concurrent
  Rampart.During -> Concurrent
  Rampart.Finishes -> Concurrent
  Rampart.OverlappedBy -> Concurrent
  Rampart.MetBy -> Concurrent
  Rampart.After -> After
