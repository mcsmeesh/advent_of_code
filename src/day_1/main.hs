stringToInteger ::
  String
  -> Integer
stringToInteger = read

fuelRequiredForMass ::
  Integer
  -> Integer
fuelRequiredForMass mass = (mass `div` 3) - 2

netFuelRequiredForMass ::
  Integer
  -> Integer
netFuelRequiredForMass mass = sum fuelRequirements
  where fuelRequirements = tail $ takeWhile (>0) $ iterate fuelRequiredForMass mass

totalFuelRequired ::
  String
  -> (Integer -> Integer)
  -> IO ()
totalFuelRequired filename fuelRequirementFn = do
  rawContent <- readFile filename
  let totalFuelRequirements = sum $ map (fuelRequirementFn . stringToInteger) (lines rawContent)
  putStrLn (show totalFuelRequirements) 

totalFuelRequiredPt1 ::
  IO ()
totalFuelRequiredPt1 = totalFuelRequired "input.txt" fuelRequiredForMass

totalFuelRequiredPt2 ::
  IO ()
totalFuelRequiredPt2 = totalFuelRequired "input.txt" netFuelRequiredForMass

