{-# LANGUAGE TemplateHaskell, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Seq where

import Control.Lens
import Control.Lens.TH


-- | Sequence identifier
newtype Key = Key Int deriving (Eq,Show,Num, Ord)

-- | A sequence is an infinite list of something with an identifier 
data Seq a = Seq {
        _key :: Key,
        _stream :: [a]
        } deriving(Functor)

$(makeLenses ''Seq)


-- | extract the tail of a Seq
tailSeq :: Seq a -> Seq a
tailSeq = over stream tail 

-- | match sequences by identifier . A lot of differentiating power is lost
instance Eq (Seq a) where
        (==) (Seq i _) (Seq j _) = i == j

-- | debugging instance
instance Show a => Show (Seq a) where
        show (Seq i xs) = show (i,take 5 xs)

-- | a sequence with holes
type SeqM a = Seq (Maybe a)
