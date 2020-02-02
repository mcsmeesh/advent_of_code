import Control.Monad
import Control.Monad.State
import Data.List
import Debug.Trace

type ShouldHalt = Bool
type Input = [Integer]
type Output = [Integer]
type IntCodeIndex = Integer
type SeekedIntCodeProgram = [Integer]
type IntCodeProgram = [Integer]
type IntCodeState = (Input, Output, IntCodeIndex, IntCodeProgram)
type IntCodeStateMonad a = State IntCodeState a

type DoubletInstruction = (Integer, Integer)
type TripletInstruction = (Integer, Integer, Integer)
type QuartetInstruction = (Integer, Integer, Integer, Integer)
type Mode = Char

modeValueTupleToValue ::
    (Mode, Integer)
    -> IntCodeProgram
    -> Integer
modeValueTupleToValue (mode, value) program = case mode of
    '1' -> value
    '0' -> program !! (fromIntegral value)
    _ -> error ("Unknown mode: " ++ [mode])

updateProgram ::
    IntCodeIndex
    -> Integer
    -> IntCodeProgram
    -> IntCodeProgram
updateProgram index value program = (take (fromIntegral index) program) ++ [value] ++ (drop (fromIntegral (index + 1)) program)

parseOpcode ::
    Integer
    -> Integer
parseOpcode rawOpcode = p
    where p = if (rawOpcode `mod` 100) == 99 then 99 else (rawOpcode `mod` 10)

parseProgramToQuartet ::
    SeekedIntCodeProgram
    -> IntCodeProgram
    -> QuartetInstruction
parseProgramToQuartet (opCodeWModes:p1:p2:p3:_) program = (opCode, p1Value, p2Value, p3)
    where opCodeWModesStr = show opCodeWModes
          opCode = parseOpcode opCodeWModes
          numImplicitModes = 4 - (length opCodeWModesStr) + 1
          modes = reverse $ (replicate numImplicitModes '0') ++ (init opCodeWModesStr)
          p1Value = modeValueTupleToValue ((modes !! 1), p1) program
          p2Value = modeValueTupleToValue ((modes !! 2), p2) program

parseProgramToTriplet ::
    SeekedIntCodeProgram
    -> IntCodeProgram
    -> TripletInstruction
parseProgramToTriplet (opCodeWModes:p1:p2:_) program = (opCode, p1Value, p2Value)
    where opCodeWModesStr = show opCodeWModes
          opCode = parseOpcode opCodeWModes
          numImplicitModes = 4 - (length opCodeWModesStr) + 1
          modes = reverse $ (replicate numImplicitModes '0') ++ (init opCodeWModesStr)
          p1Value = modeValueTupleToValue ((modes !! 1), p1) program
          p2Value = modeValueTupleToValue ((modes !! 2), p2) program

parseProgramToDoublet ::
    SeekedIntCodeProgram
    -> IntCodeProgram
    -> DoubletInstruction
parseProgramToDoublet (opCodeWModes:p1:_) program = case parseOpcode opCodeWModes of
    3 -> (3, p1)
    4 -> if (length $ show opCodeWModes) == 1 then (4, program !! (fromIntegral p1)) else (4, p1)
    _ -> error "Unsupported operation for doublet"

runOpCode1 ::
    IntCodeStateMonad ()
runOpCode1 = do
    (inputs, outputs, index, program) <- get
    let seekedProgramValues = drop (fromIntegral index) program
    let (_, arg1, arg2, destIndex) = parseProgramToQuartet seekedProgramValues program
    put (inputs, outputs, index + 4, updateProgram destIndex (arg1 + arg2) program)

runOpCode2 ::
    IntCodeStateMonad ()
runOpCode2 = do
    (inputs, outputs, index, program) <- get
    let seekedProgramValues = drop (fromIntegral index) program
    let (_, arg1, arg2, destIndex) = parseProgramToQuartet seekedProgramValues program
    put (inputs, outputs, index + 4, updateProgram destIndex (arg1 * arg2) program)

runOpCode3 ::
    IntCodeStateMonad ()
runOpCode3 = do
    (inputs, outputs, index, program) <- get
    let seekedProgramValues = drop (fromIntegral index) program
    let (_, destIndex) = parseProgramToDoublet seekedProgramValues program
    put ((tail inputs), outputs, index + 2, updateProgram destIndex (head inputs) program)

runOpCode4 ::
    IntCodeStateMonad ()
runOpCode4 = do
    (inputs, outputs, index, program) <- get
    let seekedProgramValues = drop (fromIntegral index) program
    let (_, outputValue) = parseProgramToDoublet seekedProgramValues program
    put (inputs, outputValue:outputs, index + 2, program)

runOpCode5 ::
    IntCodeStateMonad ()
runOpCode5 = do
    (inputs, outputs, index, program) <- get
    let seekedProgramValues = drop (fromIntegral index) program
    let (_, value, jumpToIndex) = parseProgramToTriplet seekedProgramValues program
    if value /= 0
        then
            put (inputs, outputs, jumpToIndex, program)
        else
            put (inputs, outputs, index + 3, program)

runOpCode6 ::
    IntCodeStateMonad ()
runOpCode6 = do
    (inputs, outputs, index, program) <- get
    let seekedProgramValues = drop (fromIntegral index) program
    let (_, value, jumpToIndex) = parseProgramToTriplet seekedProgramValues program
    if value == 0
        then
            put (inputs, outputs, jumpToIndex, program)
        else
            put (inputs, outputs, index + 3, program)

runOpCode7 ::
    IntCodeStateMonad ()
runOpCode7 = do
    (inputs, outputs, index, program) <- get
    let seekedProgramValues = drop (fromIntegral index) program
    let (_, arg1, arg2, destIndex) = parseProgramToQuartet seekedProgramValues program
    let storeValue = if arg1 < arg2 then 1 else 0
    put (inputs, outputs, index + 4, updateProgram destIndex storeValue program)

runOpCode8 ::
    IntCodeStateMonad ()
runOpCode8 = do
    (inputs, outputs, index, program) <- get
    let seekedProgramValues = drop (fromIntegral index) program
    let (_, arg1, arg2, destIndex) = parseProgramToQuartet seekedProgramValues program
    let storeValue = if arg1 == arg2 then 1 else 0
    put (inputs, outputs, index + 4, updateProgram destIndex storeValue program)

runIntCode ::
    ShouldHalt
    -> IntCodeStateMonad Output
runIntCode False = do
    (_, outputs, _, _) <- get
    return outputs
runIntCode True = do
    (inputs, outputs, index, program) <- get
    let opcode = parseOpcode (program !! (fromIntegral index))
    case opcode of
        99 -> return () -- Do nothing on halt
        1 -> runOpCode1
        2 -> runOpCode2
        3 -> runOpCode3
        4 -> runOpCode4
        5 -> runOpCode5
        6 -> runOpCode6
        7 -> runOpCode7
        8 -> runOpCode8
        _ -> error "Unimplemented"
    runIntCode (opcode /= 99 && opcode /= 4)

isHalted ::
    IntCodeState
    -> Bool
isHalted (_, _, index, program) = (program !! (fromIntegral index)) == 99

createThruster ::
    IntCodeProgram
    -> Input
    -> (Input -> Output)
createThruster program initialInputs = chainedThrusters
    where initialStates = map (\input -> (([input] :: Input),([] :: Output),0,program)) initialInputs
          fn = (\acc (initialInputs, outputs, index, program) -> (\prevOutputs -> evalState (runIntCode True) (initialInputs ++ prevOutputs, outputs, index, program)) . acc)
          chainedThrusters = foldl fn id initialStates

runThrustersWithFeedbackLoop ::
    (Input, [IntCodeState])
    -> (Input, [IntCodeState])
runThrustersWithFeedbackLoop (initialInput, intCodeStates) = if isHalted ((last . snd) thrustersAfterIteration) then thrustersAfterIteration else runThrustersWithFeedbackLoop thrustersAfterIteration
    where addInputToState (additionalInput:_) (thrusterInput, thrusterOutputs, thrusterCodeIndex, thrusterProgram) = (thrusterInput ++ [additionalInput], thrusterOutputs, thrusterCodeIndex, thrusterProgram)
          fn = \(additionalInput, previousThrusterStates) thrusterState ->
              let
                  (thrusterOutputs, nextThrusterState) = runState (runIntCode True) (addInputToState additionalInput thrusterState)
              in
                  (thrusterOutputs, previousThrusterStates ++ [nextThrusterState])
          thrustersAfterIteration = foldl fn (initialInput, []) intCodeStates

firstPart ::
    IO ()
firstPart = do
    rawContent <- readFile "./input.txt"
    let program = map read $ lines rawContent
    let thrusterPhaseCombos = permutations [0..4]
    let thrusters = map (createThruster program) thrusterPhaseCombos
    let outputs = concatMap ($ [0]) thrusters
    putStrLn $ show $ maximum outputs

secondPart ::
    IO ()
secondPart = do
    rawContent <- readFile "./input.txt"
    let program = map read $ lines rawContent
    let thrusterPhaseCombos = permutations [5..9]
    let thrusterInitialStates = map (map (\phase -> ([phase], [], 0, program))) thrusterPhaseCombos
    let outputs = map (\iState -> runThrustersWithFeedbackLoop ([0], iState)) thrusterInitialStates
    let signals= concatMap fst outputs
    putStrLn $ show $ maximum signals
