isSixDigits ::
  String
  -> Bool
isSixDigits (_:_:_:_:_:_:[]) = True
isSixDigits _ = False

isMonotonicallyIncreasing ::
  String
  -> Bool
isMonotonicallyIncreasing (x1:x2:xs) = if x1 > x2 then False else isMonotonicallyIncreasing (x2:xs)
isMonotonicallyIncreasing (x1:[]) = True
isMonotonicallyIncreasing [] = True

hasGroupOfTwoMatch ::
  String
  -> Bool
hasGroupOfTwoMatch (c1:c2:c3:c4:c5:c6:[]) = (c1 == c2) || (c2 == c3) || (c3 == c4) || (c4 == c5) || (c5 == c6)
hasGroupOfTwoMatch _ = False

hasExactlyGroupOfTwoMatch ::
  String
  -> Bool
hasExactlyGroupOfTwoMatch (c1:c2:c3:c4:c5:c6:[])
  | c1 == c2 && c2 /= c3 = True
  | c2 == c3 && c1 /= c2 && c3 /= c4 = True
  | c3 == c4 && c2 /= c3 && c4 /= c5 = True
  | c4 == c5 && c3 /= c4 && c5 /= c6 = True
  | c5 == c6 && c4 /= c5 = True
  | otherwise = False
hasExactlyGroupOfTwoMatch _ = False

numberOfPasswordsBetweenRange ::
  Integer
  -> Integer
  -> Int
numberOfPasswordsBetweenRange low high = length $ filter f numberStrings
  where numberStrings = map show [low..high]
        f = (\x -> all (== True) $ map ($ x) [isSixDigits, isMonotonicallyIncreasing, hasExactlyGroupOfTwoMatch])

