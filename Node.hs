{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
import System.Random (randomR, StdGen)
import Data.Maybe (catMaybes, isJust)
import Control.Arrow ((&&&), first, second)
import Data.List (lookup, groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function  (on)

-------------------------------------------------------------
-- | list rolling
roll :: [a] -> [a]
roll [] = []
roll (x:xs) = xs ++ [x]

-- | list modification
select :: (a -> Bool) -> [a] -> Maybe (a,a -> [a])
select f [] = Nothing
select f (x:xs) 
        | f x = Just (x,(:xs))
        | otherwise =  fmap (second $ fmap (x:)) $ select f xs
-------------------------------------------------------------


type Freq = Int

data Chan = Common | Chan Freq deriving (Eq,Ord)

data Seq = Seq Int [Maybe Freq]
core (Seq i x) = x
dropSeq (Seq i (x:xs)) = Seq i xs

instance Eq Seq where
        (==) (Seq i _) (Seq j _) = i == j

data Node = Node {
        duty :: Bool,
        transmit :: Seq,
        receives :: [(Seq,Int)],
        publ :: [Bool],
        widen :: [Bool]
        }


-- | Stepping results. Any constructor result hold the new node. Receiving mode is completed with the received message
data Step      = Receive Chan (Maybe Seq -> Node)  -- ^ receive mode , possibly receive a seq on chan
                | Transmit Chan Seq Node  -- ^ transmit mode, send a seq on chan
                | Sleep Node  -- ^ sleep mode , just step

-- | split a list of Step 
partitionStep :: [Step] -> ([((Chan,Seq),Node)],[(Chan,Maybe Seq -> Node)],[Node])
partitionStep [] = ([],[],[])
partitionStep (Transmit c s n : xs) = let
        (ts,rs,ss) = partitionStep xs
        in (((c,s),n):ts,rs,ss)
partitionStep (Receive c f : xs) = let
        (ts,rs,ss) = partitionStep xs
        in (ts,(c,f):rs,ss)
partitionStep (Sleep n : xs) = let
        (ts,rs,ss) = partitionStep xs
        in (ts,rs,n:ss)

-- | view pattern that forget heads of receivers
stepReceivers :: [(Seq,Int)] -> [(Seq,Int)]
stepReceivers = map (first dropSeq)

-- | 
agnosticReceive n Nothing = n
agnosticReceive n (Just s) = insertSeq s $ n 

-- | insert a new receiver in a node 
insertSeq :: Seq -> Node -> Node
insertSeq s n@(Node False ts rss ps ws) 
                | s `elem` map fst rss  = n
                | otherwise = Node False ts ((s,0) : rss) ps ws 


-- | Core logic. Node fields are tested sequentially with pattern matching. First success fires the right Step
step :: Node -> Step 
-- obliged to listen in the common chan
step (Node True (dropSeq -> ts) (stepReceivers -> rss) (_:ps) (_:ws)) = Receive Common . agnosticReceive $ Node False ts rss ps ws
-- time to transmit: we transmit a receiving seq and roll the receiving seqs
step (Node False (Seq i (Just c:ts)) (head &&& roll -> (x,rss)) (_:ps) (_:ws)) 
        = Transmit (Chan c) (dropSeq . fst $ x) $ Node False (Seq i ts) (stepReceivers rss) ps ws
--  time to listen on a receiving seq
step    (Node False (dropSeq -> ts) 
                (select (isJust . head . core . fst) -> Just ((Seq i (Just c:xs),n),g)) 
                (_:ps) (_:ws)) = Receive (Chan c) f where
        new k = Node False ts (stepReceivers $ g (Seq i $ Just c : xs,k)) ps ws
        f Nothing = new $ n - 1
        f (Just s) = insertSeq s . new $ 0
-- time to transmit on common chan our transmit seq, setting the duty to listen right after
step (Node False (dropSeq -> ts) (stepReceivers -> rss) (True:ps) (_:ws)) = Transmit Common ts $ Node True ts rss ps ws 
-- time to receive freely on common channel
step (Node False (dropSeq -> ts) (stepReceivers -> rss) (_:ps) (True:ws)) = Receive Common . agnosticReceive $ Node False ts rss ps ws
-- time to sleep
step (Node False (dropSeq -> ts) (stepReceivers -> rss) (_:ps) (_:ws)) = Sleep $ Node False ts rss ps ws


-- | eliminate unresponsive sequences
forget :: Int -> Node -> Node
forget n (Node v ts rss ps ws) = Node v ts (filter ((> negate n) . snd) rss)  ps ws

{-
-- | A list of Nodes with a 2d pos
type World = [(Node, Pos)]

type Pos = (Float,Float)

(x1,x2) -. (y1,y2) = (x1-y1,x2-y2)
(x,y) ^-^ 2 = x ^ 2 + y ^ 2

distance r px py = (sqrt $ (px -. py) ^-^ 2) < r

stepWorld :: Int -> World -> World
stepWorld v ns = let
        xs = map step ns 
        (unzip -> (ts,nts),rs,ss) = partitionStep xs
        ms = map head . filter ((==) 1 . length) . groupBy ((==) `on` fst ) . sortBy (comparing fst ) $ ts
        nrs = map f rs
        f (c,g) = g $ lookup c ms 
        in nts ++ map (forget v) nrs ++ ss
-}
