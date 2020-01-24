import qualified Data.Map as M
import qualified Data.Set as S
-- We have 2 wires, they each start at the same point (central port).
-- The wires have intersections.
-- We want to find the intersection point closest to the central port.
-- Our distance function is the Manhattan distance.
--
-- Approach:
-- 1. We read in the inputs from the file. We expect a list of length 2
-- where each element is a String. String -> IO ([String], [String])
-- 2. We map each element in this list (String, String) and do a string split on
-- ',' and map it to ([String], [String])
-- 3. Each element in the underlying list, is of the form (U|D|R|L)\d+
-- we want each of these string lists to be a list of coordinates of the wire
-- ([String], [String]) -> [[(Integer, Integer)], [(Integer, Integer)]]
-- [[....(0,0)], [(0,1),(0,2),...(0,75)]] ["D30", ...]
-- 4. We want to find the intersection of these 2 wires. We can just create
-- 2 sets for each, find the intersection, and return the one with the lowest
-- Manhattan distance.

-- This takes a starting point and a 'direction' and returns the list
-- of coordinates we get if we move accordingly.
-- Example (0,0) "R2" -> [(0,1),(0,2)]
traverseFromPoint ::
  (Integer, Integer)
  -> String
  -> [(Integer, Integer)]
traverseFromPoint (x,y) (direction:sNumberOfSteps) = let numberOfSteps = (read sNumberOfSteps :: Integer) 
  in case direction of
    'U' -> [(x,y+delta) | delta <- [1..numberOfSteps]]
    'D' -> [(x,y-delta) | delta <- [1..numberOfSteps]]
    'R' -> [(x+delta,y) | delta <- [1..numberOfSteps]]
    'L' -> [(x-delta,y) | delta <- [1..numberOfSteps]]
    _ -> error ("traverseFromPoint got an 'impossible' direction: " ++ (show direction))

-- Takes list of wire paths ["R75", "D30",...] and returns the entire
-- path of the wire in coordinates [(0,0), (1,0),...]
traceWirePath ::
  [String]
  -> [(Integer, Integer)] 
traceWirePath wirePathSteps = concat wirePathCoords
  where f = \acc wirePath -> acc ++ [traverseFromPoint (last $ last acc) wirePath]
        wirePathCoords = foldl f [[(0,0)]] wirePathSteps

wireIntersections ::
  [(Integer, Integer)]
  -> [(Integer, Integer)]
  -> [(Integer, Integer)]
wireIntersections firstWirePath secondWirePath = S.toList $ (S.fromList firstWirePath) `S.intersection` (S.fromList secondWirePath)

-- This is a very naive function that can break if the input isn't great.
-- # Whatever.
parseWirePath ::
  String
  -> [String]
parseWirePath s = words sSpaceSeparated
  where sSpaceSeparated = map (\c -> if c == ',' then ' ' else c) s

intersectingWireCoords ::
  [(Integer, Integer)]
  -> [(Integer, Integer)]
  -> [(Integer, Integer)]
intersectingWireCoords firstWireCoords secondWireCoords = intersectingCoords
  where intersectingCoords = wireIntersections (tail firstWireCoords) (tail secondWireCoords)

delayDistanceCalculator ::
  M.Map (Integer,Integer) Integer
  -> M.Map (Integer,Integer) Integer
  -> (Integer,Integer)
  -> Integer
delayDistanceCalculator firstWireMap secondWireMap coord = (firstWireMap M.! coord) + (secondWireMap M.! coord)

readInputs ::
  String
  -> IO Integer
readInputs filename = do
  rawContent <- readFile filename
  let xs = lines rawContent
  let firstWireCoords = traceWirePath $ parseWirePath $ head xs
  let secondWireCoords = traceWirePath $ parseWirePath $ last xs
  let intersectingCoords = intersectingWireCoords firstWireCoords secondWireCoords
-- We want to map each coordinate to the Manhattan distance, then take the
-- minimum
  let minimumDistance = minimum $ map (\(x,y) -> abs(x) + abs(y)) intersectingCoords
  return minimumDistance

readInputs2 ::
  String
  -> IO Integer
readInputs2 filename = do
  rawContent <- readFile filename
  let xs = lines rawContent
  let firstWireCoords = traceWirePath $ parseWirePath $ head xs
  let secondWireCoords = traceWirePath $ parseWirePath $ last xs
  let intersectingCoords = intersectingWireCoords firstWireCoords secondWireCoords
  let firstWireCoordsToIndex = M.fromList $ reverse $ zip firstWireCoords [0..]
  let secondWireCoordsToIndex = M.fromList $ reverse $ zip secondWireCoords [0..]
  let distanceFn = delayDistanceCalculator firstWireCoordsToIndex secondWireCoordsToIndex
-- We want to map each coordinate to the Manhattan distance, then take the
-- minimum
  let minimumDistance = minimum $ map distanceFn intersectingCoords
  return minimumDistance

