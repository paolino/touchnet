{-# LANGUAGE TemplateHaskell, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Seq where

import System.Random
import Control.Lens
import Control.Lens.TH
import Data.List (mapAccumL)


-- | Sequence identifier
newtype Key = Key Int deriving (Eq,Show,Num, Ord, Enum)

-- | A sequence is an infinite list of something with an identifier 
data Seq a = Seq {
        _key :: Key,
        _stream :: [a]
        } deriving(Functor)

$(makeLenses ''Seq)


-- | extract the tail of a Seq
tailSeq :: Seq a -> Seq a
tailSeq = over stream tail 

headSeq :: Seq a -> a
headSeq = head . view  stream 
-- | match sequences by identifier . A lot of differentiating power is lost
instance Eq (Seq a) where
        (==) (Seq i _) (Seq j _) = i == j


-- | debugging instance
instance Show a => Show (Seq a) where
        show (Seq i xs) = show (i,take 5 xs)

-- | a sequence with holes
type SeqM a = Seq (Maybe a)

mkBoolSeq       :: Key -- ^ random seed and identifier
                -> Int -- ^ frequency of True values
                -> Seq Bool
mkBoolSeq s@(Key n) freq = Seq s $ map (==0) $ randomRs (0,freq) $ mkStdGen n


-- | pseudo random sequence of Nothing and Just Freq, consume 2 keys
mkSeq   :: Key  -- ^ identifier, random seed
        -> Int  -- ^ channel space power
        -> Int  -- ^ mean delta between positives
        -> SeqM Int

mkSeq s@(Key n) chans freq = let
        Seq _ fs = mkBoolSeq s freq
        cs = randomRs (0,chans) $ mkStdGen $ n + 1
        in Seq s $ snd $ mapAccumL (\(c:cs) d -> (cs,if d then Just c else Nothing)) cs fs

-- | repeat the message with a given freq
mkSeqProd       :: Key
                -> Int
                -> m 
                -> SeqM m
mkSeqProd n freq x = Seq n [if y then Just x else Nothing | y <- view stream $ mkBoolSeq n freq] 


