{-# LANGUAGE ParallelListComp, ViewPatterns, DeriveDataTypeable, ImplicitParams, TemplateHaskell #-}



import Positioned
import World
import Node
import Stepping
import Seq
import Timed




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
        txfrequency = 10, -- ^ transmissions frequency
        pufrequency = 20, -- ^ pubblicity frequency
        rodfrequency = 20, -- ^ node self message frequency
        droplevel = 5, -- ^ max number of consecutive missed listenings before dropping
        alertlevel = 3, -- ^ minimum number of listeners to stop using sync listening window on common channel
        neighborlevel = 5, -- ^ minimum number of neighbors to stop listening on common channel
        memory = 20, -- ^ number of remembered messages
        numchannels = 10, -- ^ channel spectrum 
        lmessagettl = 50 -- ^ message duration in memory
        }
worldConf = WorldConfiguration {
        radiorange = 0.3
        }


data Graphics = Graphics {
        _world :: World Future Char,
        _selected :: Maybe Key
        }
makeLenses ''Graphics
main :: IO ()
main =         
        -- args <- cmdArgs sample
        let ?nconf = nodeConf
            ?wconf = worldConf
        in playIO (InWindow "Zen" (800, 600) (5, 5))
                0 -- color
                30 -- frames
                (Graphics (World [] 0) Nothing) -- starting world
                render -- render world
                (\_ x -> return x) -- handle events
                (\_ w -> return $ over world stepWorld w) -- stepworld




{-
handle :: (?configuration :: Configuration Float) => Event -> (World (Close Pos Letter),Maybe Key,Int) -> IO (World (Close Pos Letter),Maybe Key,Int)
handle (EventMotion (zot -> p)) (World i xs,Just k,jf) = let
        Just (Close (Node hs ms _ ts rss ps l q w) f , rm) = select ((==k) . view (node . transmit . key)) xs
        in return (World i $ rm (Close (Node hs ms p ts rss ps l q w) f) ,Just k,jf)


handle (EventKey (Char c) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,jf) = do  
        let     Close n  f : xs' = sortBy (comparing $ distance p . view (node . load)) xs
                n' = over roduction (fmap (fmap (const $ Letter c)))  n
        return (World i $ Close n' f : xs',Nothing,jf)



handle (EventKey (MouseButton RightButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj) = return (World i $ x:xs,Nothing,fj + 1) where
        x = closeU $ (\(Node xs _ a ts rss ps l q w) -> Node xs [] a ts rss ps l q w) $ mkNode fj p $ Letter (toEnum $ fromEnum 'a' + fj)

handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (zot -> p)) (World i [],_,fj) = return (World i [],Nothing,fj)

handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj) = let
        Close (Node hs ms _ ts rss ps l q w) f : _ = sortBy (comparing $ distance p . view (node . load)) xs
        in return (World i $ xs,Just (view key ts),fj)

handle (EventKey (MouseButton LeftButton) Up (Modifiers Up Up Up) (zot -> p)) (World i xs,_,fj) = let
        in return (World i $ xs,Nothing,fj)

handle _ x = return x

regular :: Int -> [(Float,Float)]
regular n = take (n + 1) $ map (\a -> (cos a,sin a)) [0,2*pi/fromIntegral n..]
-}
renderText c x x0 y0 = translate x0 y0 . scale 20 20 $ Pictures  $ [color c $ scale 0.005 0.007 $ text x]


nodePicture :: Positioned (String, State Char) -> Picture

nodePicture (Positioned (ms,Sleep _) x0 y0) = renderText white ms x0 y0 
nodePicture (Positioned (ms, ReceiveFree _ _) x0 y0) = renderText blue ms x0 y0 
nodePicture (Positioned (ms, ReceiveCommon _ _) x0 y0) = renderText cyan ms x0 y0 
nodePicture (Positioned (ms, TransmitFree _ _ _) x0 y0) = renderText green ms x0 y0 
nodePicture (Positioned (ms, TransmitCommon _ _ _) x0 y0) = renderText yellow ms x0 y0 

render :: Graphics -> IO Picture
render (Graphics (World xs _) _) = return $ 
        let  rs = over (traverse . value) 
                        (map (view message) . view (node . messages) 
                                &&& applyFuture) xs
        in scale 800 600 $ Pictures $ map nodePicture rs
        
        

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

