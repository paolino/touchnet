{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Timed where

import Control.Lens
import Control.Arrow ((&&&))
import Control.Lens.TH
import Data.List
import Data.Ord
import List

data Timed m = Timed {
        _ttl :: Int,
        _hits :: Int,
        _message :: m
        }
        deriving Show

$(makeLenses ''Timed)


decTimed :: Timed m -> Timed m
decTimed =  over ttl $ subtract 1

hit :: Timed m -> Timed m
hit = over hits (+1)

least :: [Timed m] -> Maybe (m,Int)
least [] = Nothing
least xs = Just . (view message &&& view  ttl). head . sortBy (comparing $ view hits) $ xs

hitm :: Eq m => (m,Int) -> [Timed m] -> [Timed m]
hitm (x,t) ys = maybe (Timed t 1 x:ys) (\(c,f) -> f $ hit c) $ select ((==x) . view message) $ ys

type TimedTimed m = Timed (Timed m)

-- eliminate all timed values where time is over (negative) after subtracting 1
turn :: [Timed m] -> [Timed m]
turn = filter ((>0) . view ttl) . map decTimed
