{-# LANGUAGE ViewPatterns #-}
import System.Random (randomR, StdGen)
import Data.Maybe (catMaybes, isJust)
import Control.Arrow ((&&&), first, second)
import Data.List (lookup, groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function  (on)
type Freq = Int

data Chan = Common | Chan Freq deriving (Eq,Ord)

data Seq = Seq Int [Maybe Freq]
modCore f (Seq i x) = Seq i $ f x
core (Seq i x) = x
instance Eq Seq where
        (==) (Seq i _) (Seq j _) = i == j

data Node a = Node {
        position :: a,
        duty :: Bool,
        transmit :: Seq,
        receives :: [(Seq,Int)],
        publ :: [Bool],
        widen :: [Bool]
        }

roll (x:xs) = xs ++ [x]

select :: (a -> Bool) -> [a] -> Maybe (a,a -> [a])
select f [] = Nothing
select f (x:xs) 
        | f x = Just (x,(:xs))
        | otherwise =  fmap (second $ fmap (x:)) $ select f xs

data Step a = Receive Chan (Maybe Seq -> Node a) | Transmit Chan Seq (Node a) | Sleep (Node a) 

partitionStep :: [Step a] -> ([((Chan,Seq),Node a)],[(Chan,Maybe Seq -> Node a)],[Node a])
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

dropSeq (Seq i (x:xs)) = Seq i xs

stepReceivers :: [(Seq,Int)] -> [(Seq,Int)]
stepReceivers = map (first dropSeq)

agnosticReceive n Nothing = n
agnosticReceive n (Just s) = insertSeq s $ n 

insertSeq :: Seq -> Node a -> Node a
insertSeq s n@(Node pos False ts rss ps ws) 
                | s `elem` map fst rss  = n
                | otherwise = Node pos False ts ((s,0) : rss) ps ws 

step :: Node a -> Step a

step (Node pos True (dropSeq -> ts) (stepReceivers -> rss) (_:ps) (_:ws)) = Receive Common . agnosticReceive $ Node pos False ts rss ps ws
step (Node pos False (Seq i (Just c:ts)) (head &&& roll -> (x,rss)) (_:ps) (_:ws)) 
        = Transmit (Chan c) (dropSeq . fst $ x) $ Node pos False (Seq i ts) (stepReceivers rss) ps ws

step    (Node pos 
                False 
                (dropSeq -> ts) 
                (select (isJust . head . core . fst) -> Just ((Seq i (Just c:xs),n),g)) 
                (_:ps) 
                (_:ws)
        ) = Receive (Chan c) f where
        new k = Node pos False ts (stepReceivers $ g (Seq i $ Just c : xs,k)) ps ws
        f Nothing = new $ n - 1
        f (Just s) = insertSeq s . new $ 0

step (Node pos False (dropSeq -> ts) (stepReceivers -> rss) (True:ps) (_:ws)) = Transmit Common ts $ Node pos True ts rss ps ws 
step (Node pos False (dropSeq -> ts) (stepReceivers -> rss) (_:ps) (True:ws)) = Receive Common . agnosticReceive $ Node pos False ts rss ps ws
step (Node pos False (dropSeq -> ts) (stepReceivers -> rss) (_:ps) (_:ws)) = Sleep $ Node pos False ts rss ps ws

forget :: Int -> Node a -> Node a
forget n (Node pos v ts rss ps ws) = Node pos v ts (filter ((> negate n) . snd) rss)  ps ws

type World = [Node Pos]

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
