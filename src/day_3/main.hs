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

readInputs ::
  String
  -> IO Integer
readInputs filename = do
  rawContent <- readFile filename
  let xs = lines rawContent
  let firstWirePath = parseWirePath $ head xs
  let secondWirePath = parseWirePath $ last xs
  let firstWireCoords = traceWirePath firstWirePath
  let secondWireCoords = traceWirePath secondWirePath
  let intersectionCoords = wireIntersections (tail firstWireCoords) (tail secondWireCoords)
-- We want to map each coordinate to the Manhattan distance, then take the
-- minimum
  let minimumManhattanDistance = minimum $ map (\(x,y) -> abs(x) + abs(y)) intersectionCoords
  return minimumManhattanDistance

-- When we trace the wirePath, we now want to also have the index of it since
-- it's the 'delay'. [(0,0),(1,0),..,(1,0),..] -> [((0,0),0), ((1,0),1)]
-- From here we can convert this to a map since it's associative array
-- dict[coord] = delay
-- intersection dict1.keys() dict2.keys() -> intersecting_coords
-- intersecting_coords -> net_delay -> min [[dict1[coord] + dict2[coord]]]
readInputs2 ::
  String
  -> IO Integer
readInputs2 filename = do
  rawContent <- readFile filename
  let xs = lines rawContent
  let firstWirePath = parseWirePath $ head xs
  let secondWirePath = parseWirePath $ last xs
  let firstWireCoords = traceWirePath firstWirePath
  let secondWireCoords = traceWirePath secondWirePath
  let firstWireCoordsToIndex = M.fromList $ reverse $ zip (tail firstWireCoords) [1..]
  let secondWireCoordsToIndex = M.fromList $ reverse $ zip (tail secondWireCoords) [1..]
  let intersectingMap = M.intersection firstWireCoordsToIndex secondWireCoordsToIndex
  let calculateNetDelay = (\coord _ -> (firstWireCoordsToIndex M.! coord) + (secondWireCoordsToIndex M.! coord))
  let intersectingCoordsToNetDelay = M.mapWithKey calculateNetDelay intersectingMap
  let minimumDelay = minimum $ map (\(_,netDelay) -> netDelay) (M.toList intersectingCoordsToNetDelay)
  return minimumDelay
