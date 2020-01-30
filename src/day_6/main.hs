import Debug.Trace
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Planet = String
type PlanetGraph = M.Map Planet (S.Set Planet)
type PlanetOutDegree = M.Map Planet Integer

dangerouslyParseOrbit ::
    String
    -> (Planet, Planet)
dangerouslyParseOrbit orbitStr = (orbiteePlanet, tail pOrbiterPlanet)
    where (orbiteePlanet, pOrbiterPlanet) = break (==')') orbitStr

buildPlanetGraphAndOutDegree ::
    [(Planet, Planet)]
    -> (PlanetGraph, PlanetOutDegree)
buildPlanetGraphAndOutDegree orbitTuples = (planetGraph, planetDegrees)
    where graphBuilder = (\graph (orbiteePlanet, orbiterPlanet) -> (M.insertWith (S.union) orbiterPlanet S.empty $ M.insertWith (S.union) orbiteePlanet (S.singleton orbiterPlanet) graph))
          degreeBuilder = (\degrees (orbiteePlanet, orbiterPlanet) -> (M.insertWith (+) orbiterPlanet 1 $ M.insertWith (+) orbiteePlanet 0 degrees))
          planetGraph = foldl graphBuilder M.empty orbitTuples
          planetDegrees = foldl degreeBuilder M.empty orbitTuples

buildBidirectionalPlanetGraph ::
    [(Planet, Planet)]
    -> PlanetGraph
buildBidirectionalPlanetGraph orbitTuples = planetGraph
    where graphBuilder = (\graph (orbiteePlanet, orbiterPlanet) -> (M.insertWith (S.union) orbiterPlanet (S.singleton orbiteePlanet) $ M.insertWith (S.union) orbiteePlanet (S.singleton orbiterPlanet) graph))
          planetGraph = foldl graphBuilder M.empty orbitTuples

getSourcePlanets ::
    PlanetOutDegree
    -> [Planet]
getSourcePlanets planetDegrees = M.keys $ M.filter (==0) planetDegrees

getTotalOrbitsInvolvingPlanet ::
    Planet
    -> Integer
    -> PlanetGraph
    -> Integer
getTotalOrbitsInvolvingPlanet planet distance planetGraph = if orbitters == S.empty
                                                               then
                                                                   distance
                                                               else
                                                                   distance + (sum $ map orbitsOfOrbitters orbittersList)
    where orbitters = M.findWithDefault S.empty planet planetGraph
          orbittersList = S.toList orbitters
          orbitsOfOrbitters = (\planet' -> getTotalOrbitsInvolvingPlanet planet' (distance + 1) planetGraph)

shortestTransfersBetweenPlanets ::
    Planet
    -> Planet
    -> PlanetGraph
    -> Integer
shortestTransfersBetweenPlanets startingPlanet targetPlanet planetGraph = shortestTransfersBetweenPlanets' S.empty (-2) startingPlanet
    where
        shortestTransfersBetweenPlanets' seenPlanets currentTransfers currentPlanet
            | currentPlanet == targetPlanet = currentTransfers
            | S.member currentPlanet seenPlanets = 2 ^ 63
            | otherwise = let orbitters = (M.findWithDefault S.empty currentPlanet planetGraph)
                          in if orbitters == S.empty
                                then
                                    2 ^ 63
                                else
                                    minimum $ map (shortestTransfersBetweenPlanets' (S.insert currentPlanet seenPlanets) (currentTransfers + 1)) (S.toList orbitters)

firstPart ::
    IO ()
firstPart = do
    rawContent <- readFile "./input.txt"
    let content = map dangerouslyParseOrbit $ lines rawContent
    let (planetGraph, planetDegree) = buildPlanetGraphAndOutDegree content
    let sourcePlanets = getSourcePlanets planetDegree
    let totalOrbits = sum $ map (\planet -> getTotalOrbitsInvolvingPlanet planet 0 planetGraph) sourcePlanets
    putStrLn $ show $ totalOrbits

secondPart ::
    IO ()
secondPart = do
    rawContent <- readFile "./input.txt"
    let content = map dangerouslyParseOrbit $ lines rawContent
    let bidirectionalPlanetGraph = buildBidirectionalPlanetGraph content
    let shortestTransfers = shortestTransfersBetweenPlanets "YOU" "SAN" bidirectionalPlanetGraph
    putStrLn $ show $ shortestTransfers
