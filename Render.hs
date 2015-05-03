{-# LANGUAGE ParallelListComp, ViewPatterns, DeriveDataTypeable, ImplicitParams, TemplateHaskell #-}



import Positioned hiding (ord)
import World
import Node
import Stepping
import Seq
import Timed
import List


import Data.Char
import Control.Concurrent.STM
import Control.Concurrent hiding (Chan)
import Prelude hiding (mapM, concat, foldr, concatMap, elem, sum)
import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.IO.Game hiding (Key)
import Data.Foldable
import Data.Traversable
import qualified Data.Set as S
import Data.List (sortBy, sort)
import Data.Ord (comparing)
import Control.Arrow
import Control.Monad
import Data.Typeable
import System.Console.CmdArgs
import Control.Lens
import Control.Lens.TH
{-
-- | NodeConfiguration of a node. 
data NodeConfiguration = NodeConfiguration {
        txfrequency :: Int, -- ^ transmissions frequency
        pufrequency :: Int, -- ^ pubblicity frequency
        rodfrequency :: Int, -- ^ node self message frequency
        droplevel :: Int, -- ^ max number of consecutive missed listenings before dropping
        alertlevel :: Int, -- ^ minimum number of listeners to stop using sync listening window on common channel
        neighborlevel :: Int, -- ^ minimum number of neighbors to stop listening on common channel
        memory :: Int, -- ^ number of remembered messages
        numchannels :: Int, -- ^ channel spectrum 
        lmessagettl :: Int -- ^ message duration in memory
        } deriving (Show)-- , Typeable, Data)
sample = Configuration 
        {       frames = def &= help "number of frames per second" &= opt (25 :: Int)
        ,       radiorange = def &= help "radio range in video units" &=  opt (0.3::Double)
        ,       txfrequency = def &= help "mean delta in frames between node transmission events" &= opt (10 :: Int)
        ,       pufrequency = def &= help "mean delta in frames between node self publicity events" &= opt (20 :: Int)
        ,       rodfrequency = def &= help "mean delta in frames between node fact production event" &= opt (20 :: Int)
        ,       memory = def &= help "node fact memory capcacity" &= opt (5 :: Int)
        ,       numchannels = def &= help "channel diversity" &= opt (30 :: Int)
        ,       lmessagettl = def &= help "message time to live" &= opt (50::Int)
        ,       droplevel = def &= help "max number of consecutive missed listenings before dropping" &= opt (3 :: Int)
        ,       alertlevel = def &= help "minimum number of listeners to stop using sync listening window on common channel"  &= opt (3::Float)
        ,       neighborlevel = def &= help "minimum number of neighbors to stop listening on common channel" &= opt (5::Float)
        }
         &= summary "Simulation of touchnet protocol"
-}
nodeConf = NodeConfiguration {
        txfrequency = 20, -- ^ transmissions frequency
        pufrequency = 100, -- ^ pubblicity frequency
        rodfrequency = 100, -- ^ node self message frequency
        droplevel = 5, -- ^ max number of consecutive missed listenings before dropping
        alertlevel = 1, -- ^ minimum number of listeners to stop using sync listening window on common channel
        neighborlevel = 1, -- ^ minimum number of neighbors to stop listening on common channel
        neighbormemory = 5, -- ^ minimum number of neighbors to stop listening on common channel
        memory = 10, -- ^ number of remembered messages
        numchannels = 10, -- ^ channel spectrum 
        lmessagettl = 500 -- ^ message duration in memory
        }


data Graphics = Graphics {
        _world :: World Future Char,
        _selected :: Maybe Key,
	_radiorange :: Float,
	_curname :: Char,
	_highlighted :: Maybe Key
        }

makeLenses ''Graphics
main :: IO ()
main =         
        -- args <- cmdArgs sample
        let ?nconf = nodeConf
        in playIO (InWindow "Zen" (800, 600) (5, 5))
                0 -- color
                30 -- frames
                (Graphics (World [] 0) Nothing 100 'a' Nothing) -- starting world
                render -- render world
                handle -- handle events
                (\_ w -> return $ over world (stepWorld (view radiorange w)) w) -- stepworld




handle :: (?nconf :: NodeConfiguration) => Event -> Graphics -> IO Graphics


-- map the movement if a node is selected
handle (EventMotion p@(x',y')) g@(view selected &&& view (world . nodes) -> (Just k, xs)) = let
        Just (view value -> z, f) = select ((== k) . view (value . node . transmit . key)) $ xs
        in return $ set (world . nodes) (f $ Positioned z x' y') $ set highlighted Nothing g
handle (EventMotion p@(x',y')) g = return $ set highlighted (nearest x' y' (view world g)) g

-- add a new node in mouse position
handle (EventKey (MouseButton RightButton) Down (Modifiers Up Up Up) (x',y')) g = do
	return $ over curname succ $ over world (add x' y' $ view curname g) g


handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x',y')) g = return $ set selected (nearest x' y' $ view world g) g

handle (EventKey (MouseButton LeftButton) Up (Modifiers Up Up Up) _) g = return $ set selected Nothing g

handle _ x = return x
{-

handle (EventKey (Char c) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,jf) = do  
        let     Close n  f : xs' = sortBy (comparing $ distance p . view (node . load)) xs
                n' = over roduction (fmap (fmap (const $ Letter c)))  n
        return (World i $ Close n' f : xs',Nothing,jf)
regular :: Int -> [(Float,Float)]
regular n = take (n + 1) $ map (\a -> (cos a,sin a)) [0,2*pi/fromIntegral n..]
-}
renderText rr c x x0 y0 = translate x0 y0  $ Pictures  $ [color (greyN 0.05) $ circle rr, color c $ scale 0.2 0.2 $ text $ sort x]


nodePicture :: Float -> Positioned (String, State Char) -> Picture

nodePicture rr (Positioned (ms,Sleep _) x0 y0) = renderText rr (greyN 0.1) ms x0 y0 
nodePicture rr (Positioned (ms, ReceiveFree _ _) x0 y0) = renderText rr cyan ms x0 y0 
nodePicture rr (Positioned (ms, ReceiveCommon _ _) x0 y0) = renderText rr blue ms x0 y0 
nodePicture rr (Positioned (ms, TransmitFree _ _ _) x0 y0) = renderText rr yellow ms x0 y0 
nodePicture rr (Positioned (ms, TransmitCommon _ _ _) x0 y0) = renderText  rr red ms x0 y0 

fromPositioned (Positioned _ x y) = (x,y)
positionOf :: Graphics -> Key -> Maybe (Positioned (Future Char))
positionOf g k = let 
	ps = view (world . nodes) g
	ks = map (view (value .node . transmit . key)) ps
	in lookup k $ zip ks ps
render :: Graphics -> IO Picture
render (Graphics (World xs _) _ rr _ ms) = return $ 
        let  rs = over (traverse . value) 
                        (map (view message) . view (node . messages) 
                                &&& applyFuture) xs
	{-
	let  	rays Nothing = []
		rays (Just x) = color blue $ Pictures $ map (\p -> Line [fromPositioned x,fromPositioned p])
			 (view (value . node . neighbors . stream . key) x) 
	-}
        in Pictures $ map (nodePicture rr) rs
        
        

{-
triangolo p q = [p' `summa` scala 0.05 (dpq' `summa` per1)
                , p' `summa` scala 0.8 dpq'
                , p' `summa` scala 0.03 (dpq' `summa` per2)
                ,p' `summa` scala 0.03 (dpq' `summa` per1) ] where
        (per1,per2) = perp dpq'
        d = distance p q
        r = sqrt (800 ** 2 + 600 ** 2)
        dpq = diffa q p
        p' = p `summa` scala (11/r/d) dpq 
        q' = q `diffa` scala (11/r/d) dpq 
        dpq' = diffa q' p'
        perp (x,y) = ((-y,x),(y,-x))
        scala k (x,y) = (k*x,k*y)
        summa (x1,y1) (x2,y2) = (x1 + x2,y1 + y2)
        diffa (x1,y1) (x2,y2) = (x1 - x2,y1 - y2)

-}

