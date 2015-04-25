module List where
import Control.Arrow (second)

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

-- | 
pick :: (a -> Bool) -> [a] -> Maybe (a,[a])
pick f [] = Nothing
pick f (x:xs)
        | f x = Just (x,xs)
        | otherwise = fmap (second (x:)) $ pick f xs

toHead f g  = fmap (\(x,xs) -> g x : xs) . pick f 

-------------------------------------------------------------

