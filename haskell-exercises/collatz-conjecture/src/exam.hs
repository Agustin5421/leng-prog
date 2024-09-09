--PARCIAL

--Ejercicio 1:
encode :: String -> [(Int, Char)]
encode [] = []
encode xs = [(length x, head x) | x <- group xs]

--Without group
classicEncode :: String -> [(Int, Char)]
classicEncode (x: xs) = encodeHelper 1 x xs

encodeHelper :: Int -> Char -> String -> [(Int, Char)]
encodeHelper n x [] = [(n, x)] 
encodeHelper n x (y:ys) 
              | y == x  = encodeHelper (n + 1) y ys
              | otherwise = (n, x) : encodeHelper 1 y ys
              
              
              
--Ejercicio 2
data Element = One Char
                | Repeat Char Int deriving (Show)
          
          
encodeAsElement :: String -> [Element]
encodeAsElement s = encodeAsElemHelper (encode s)

encodeAsElemHelper :: [(Int, Char)] -> [Element]
encodeAsElemHelper [] = []
encodeAsElemHelper ((n, c):xs) 
                  | n <= 1 = One c : encodeAsElemHelper xs
                  | otherwise = Repeat c n : encodeAsElemHelper xs



--Ejercicio 3
--a
data Student = Student {
      idNumber  :: Int,
      name :: String
} deriving (Show)

average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

--b
averageScores:: [(Int, Int)] -> Map Int Double
averageScores [] = Map.empty
averageScores xs =  Map.fromList averageList
    where
      averageList = map avg (Map.toList mapScoresList)
      avg (a,b) = (a, average b )
      mapScoresList = foldl ins Map.empty xs
      ins carry  (myId, score) = Map.insertWith (++) myId [score] carry


--c
scoreAsString :: Int -> Map.Map Int Double -> IO ()
scoreAsString studentId scoresMap =
    case Map.lookup studentId scoresMap of
        Just score -> putStrLn (show score)
        Nothing -> putStrLn("a")

--d
printAverages :: [Student] -> Map.Map Int Double -> IO ()
printAverages [] _ = return ()
printAverages (student:students) scoresMap = do
    let studentId = idNumber student
    let studentName = name student
    case Map.lookup studentId scoresMap of
        Just score -> putStrLn (studentName ++ " " ++ show score)
        Nothing -> putStrLn (studentName ++ " Not Show")
    printAverages students scoresMap
{--
averageScores :: [(Int, Int)] -> [(Int, Double)]
averageScores scores =
    let n = foldl' updateStudent [] scores
    in map calculateProm n

updateStudent :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int, Int)]
updateStudent [] (studentId, score) = [(studentId, score, 1)]
updateStudent ((id, totalScore, count):rest) (studentId, score)
    | id == studentId = (id, totalScore + score, count + 1) : rest
    | otherwise = (id, totalScore, count) : updateStudent rest (studentId, score)

calculateProm :: (Int, Int, Int) -> (Int, Double)
calculateProm (studentId, totalScore, count) =
    (studentId, fromIntegral totalScore / fromIntegral count)
--}