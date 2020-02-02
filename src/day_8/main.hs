import Data.Char
import Data.List

type ImageLayer = [Integer]
type Width = Integer
type Height = Integer
type ImageDimensions = (Width, Height)

-- Taken from https://stackoverflow.com/a/12882583
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunksOf n zs

numbersToImageLayers ::
    [Integer]
    -> ImageDimensions
    -> [ImageLayer]
numbersToImageLayers numbers (width, height) = chunksOf (fromIntegral (width * height)) numbers

countOfNumberInLayer ::
    Integer
    -> ImageLayer
    -> Integer
countOfNumberInLayer number layer = fromIntegral . length $ filter (==number) layer

getPixelColor ::
    [Integer]
    -> Integer
getPixelColor layerPixels = case find (/=2) layerPixels of
    Nothing -> 2
    Just n -> n

integerToPixelChar ::
    Integer
    -> Char
integerToPixelChar n = if n /= 0 then '*' else ' '

firstPart ::
    IO ()
firstPart = do
    rawContent <- readFile "./input.txt"
    let numbers = map (toInteger . digitToInt) $ rawContent
    let imageLayers = numbersToImageLayers numbers (25, 6)
    let zeroCountWithLayerIndex = zip (map (countOfNumberInLayer 0) imageLayers) [0..]
    let minimumZeroCountIndex = snd $ minimum zeroCountWithLayerIndex
    let targetLayer = imageLayers !! minimumZeroCountIndex
    putStrLn $ show $ (countOfNumberInLayer 1 targetLayer) * (countOfNumberInLayer 2 targetLayer)

secondPart ::
    IO ()
secondPart = do
    rawContent <- readFile "./input.txt"
    let numbers = map (toInteger . digitToInt) $ rawContent
    let imageLayers = numbersToImageLayers numbers (25, 6)
    let imageLayerPixels = transpose imageLayers
    let imagePixels = map getPixelColor imageLayerPixels
    mapM_ putStrLn $ map (map integerToPixelChar) (chunksOf 25 imagePixels)
