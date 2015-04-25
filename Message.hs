import qualified Data.Map as M

type Index = Int

data Load       
        = Get  -- going in accepting mode
        | Set  -- using an accepting mode
        | Known Index -- telling others who is on the air

data Message = Message Index Load

data Operation = Scanning [Index] | Accepting [Index] | Operation [Index] [Index] [Index]


incoming        :: [Bool]  -- random stream (True is transmit)
                -> Index   -- id of the node
                -> Operation  -- node state
                -> Maybe Message -- a message is on the air
                -> (Operation, Maybe Message, [Bool]) -- (new state for the node, a possible message production, the left random stream)

-- while scanning, known ids' are collected
incoming rs k (Scanning is) (Just (Message _ (Known j))) = (Scanning (j:is), Nothing,rs)
-- while scanning, receiving a Get, trigger a Set message, and put the node in Operation state with the collected ids'
incoming rs k (Scanning is) (Just (Message j Get)) =  (Operation is is [],Just (Message k Set),rs)
-- what ever received or not leave the node in Scanning state
incoming rs k (Scanning is) _ = (Scanning is, Nothing,rs)

-- while accepting, if a Set message is received, the id is collected and we go into Operation state
incoming rs k (Accepting is) (Just (Message j Set)) = (Operation (j:is) (j:is) [], Nothing,rs) 
-- we go into Operation state anyway
incoming rs k (Accepting is) _ = (Operation is is [], Nothing,rs)

-- while in Operation, if we spit out all the known ids and nothing was collected we go in Scanning mode
incoming rs k (Operation _ [] []) _ =  (Scanning [],Nothing,rs)
-- while in Operation, if we spit out all the known ids and something was collected we go in Accepting mode and trigger a Get message
incoming rs k (Operation _ [] zs) _ =  (Accepting zs , Just (Message k Get),rs)
-- when known ids are left, if the next boolean is true we spit it out else we do nothing aside consuming the boolean
incoming (r:rs) k (Operation is (j:js) zs) (Just (Message i (Known w))) = 
        let     zs' = if i `elem` is then w:zs else zs
                (m,js') = if r then (Just (Message k (Known j)),js) else (Nothing,j:js)
        in (Operation is js' zs', m, rs)

