{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
module Timed where

import Control.Lens
import Control.Lens.TH

data Timed m = Timed {
        _ttl :: Int,
        _message :: m
        }

$(makeLenses ''Timed)


-- eliminate all timed values where time is over (negative) after subtracting 1
decTimeds :: [Timed m] -> [Timed m]
decTimeds = filter ((>0) . view ttl) . map (over ttl $ subtract 1) 

