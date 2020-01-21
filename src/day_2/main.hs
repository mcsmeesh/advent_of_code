import Control.Monad.State
-- import Debug.Trace

type ProgramValue = Integer -- The value is always the value at index 0 of current state
type ProgramState = (Int, [Integer])

initializePositionValues ::
  Integer
  -> Integer
  -> [Integer]
  -> [Integer]
initializePositionValues val1 val2 (x:_:_:xs) = (x:val1:val2:xs)
initializePositionValues _ _ _ = error "initializePositionValues hit an 'impossible' pattern match"

-- Takes in an index, a value at that index, initial list, and returns the
-- updated list
updateValueAtIndex ::
  Int
  -> Integer
  -> [Integer]
  -> [Integer]
updateValueAtIndex index value xs = (take (index) xs) ++ [value] ++ (drop (index+1) xs)

runOp ::
  [Int]
  -> [Integer]
  -> [Integer]
runOp (opCode:firstIndex:secondIndex:destinationIndex:_) stateValues = updateValueAtIndex (fromIntegral destinationIndex) destinationValue stateValues
  where firstArg = stateValues !! firstIndex
        secondArg = stateValues !! secondIndex
        destinationValue = case opCode of
          1 -> firstArg + secondArg
          2 -> firstArg * secondArg
          _ -> error ("runOp hit an unsupported opCode: " ++ (show opCode) ++ " " ++ (show stateValues))
runOp ops _ = error ("runOp hit an 'impossible' pattern match: " ++ (show ops))

-- Runs program until it halts
runProgram ::
  Bool
  -> State ProgramState ProgramValue
runProgram False = do
  (_, finalState) <- get
  return (head finalState)
runProgram True = do
  (index, currentState) <- get
  let opCode = currentState !! index
  case opCode of
    99 -> put (index, currentState)
    _ -> put (index+4, runOp (drop index (map fromIntegral currentState)) currentState)
  runProgram (opCode /= 99)

stringToInteger ::
  String
  -> Integer
stringToInteger = read

runProgramWithNounAndVerb ::
  Integer
  -> Integer
  -> [Integer]
  -> (Integer, Integer, ProgramValue)
runProgramWithNounAndVerb noun verb content = (noun, verb, evalState (runProgram True) (0, initializedContent))
  where initializedContent = initializePositionValues noun verb content

programPt1 ::
  [Integer]
  -> (Integer, Integer, ProgramValue)
programPt1 = runProgramWithNounAndVerb 12 2

programPt2 ::
  [Integer]
  -> (Integer, Integer, ProgramValue)
programPt2 content = head $ filter (\(_,_,output) -> output == 19690720) combinations
  where combinations = [runProgramWithNounAndVerb noun verb content | noun <- [1..99], verb <- [1..99]]

runProgramPt1 ::
  IO ()
runProgramPt1 = do
  rawContent <- readFile "input.txt"
  let content = map read $ lines rawContent
  putStrLn $ show $ programPt1 content


runProgramPt2 ::
  IO ()
runProgramPt2 = do
  rawContent <- readFile "input.txt"
  let content = map read $ lines rawContent
  putStrLn $ show $ programPt2 content
