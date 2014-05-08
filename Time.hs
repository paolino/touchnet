{-# LANGUAGE FlexibleContexts, ParallelListComp #-}

module Time where

import Control.Concurrent

import System.Random
import System.Time
import Control.Arrow ((&&&))
import Data.List.Ordered
import Data.Ord (comparing)
import qualified Data.IntMap as M

millis :: IO Integer
millis = getClockTime >>= \(TOD x y) -> return $ x * 1000 + (y `div` 1000000000)

type Millis = Integer
type Frequence = Integer
type Channel = Int

data Sequenza a = Sequenza StdGen Frequence Millis a deriving (Show ,Read)


tsequenza (Sequenza _ _ t _) = t
load (Sequenza _ _ _ x) = x
gene (Sequenza g _ _ _) = g

update :: (StdGen -> a -> a) -> Sequenza a -> Sequenza a
update fg (Sequenza g fr t x) = let 
        (dt,g') = randomR (0,fr) g
        x' = fg g' x
        in Sequenza g' fr (t + dt) x'

newtype Stream a = Stream (IO (a, Stream a))
       
cycol :: Ord b => (a -> b) -> (a -> a) -> [a] -> [a]
cycol z f (x:xs) = insertBagBy (comparing z) (f x) xs

mkStream :: (StdGen -> a -> a) -> [Sequenza a] -> Stream a
mkStream fg  = 
        let next xs@(x:_) = do
                now  <- millis 
                threadDelay ((1000 *) . fromIntegral $ tsequenza x - now)
                return (load x , 
                        Stream . next $ cycol tsequenza (update fg) xs) 
        in Stream . next .  sortOn tsequenza 

runStream :: (StdGen -> a -> a) -> (a -> IO ()) -> [Sequenza a] -> IO ()
runStream fg f = let
        c (Stream s) = do
                (x,s') <- s
                f x
                c s'
        in c . mkStream fg

ex1 :: Millis -> IO [Sequenza Int]
ex1 l = do
        now <- millis 
        return [Sequenza (mkStdGen n) l now n | n <- [0..9]]

type Index = Int

data Node a = Node {
        message :: (Index,a),
        state :: a,
        receivers :: [Index],
        pos :: (Float,Float)
        }


        
