{-# LANGUAGE DeriveFunctor,TemplateHaskell #-}
-- | values tagged with a 2d position
module Positioned where

import Control.Lens.TH
import Data.List (sortBy)
import Data.Ord (comparing)

data Positioned a = Positioned {
        _value :: a,
        _asc :: Double,
        _ord ::Double 
        } deriving (Functor)

makeLenses ''Positioned

distance :: Positioned a -> Positioned b -> Double
distance (Positioned _ x1 y1)  (Positioned _ x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

-- | compute the only neighbor in sight
isolated :: Double -> Positioned a -> [Positioned b] -> Maybe (Positioned b)
isolated d x xs = case filter ((<d) . distance x) xs of 
        [y] -> Just y
        _ -> Nothing


