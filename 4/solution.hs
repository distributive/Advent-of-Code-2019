import Control.Monad
import Data.List.Split

main = do
        contents <- liftM (map (read::String->Int) . splitOn "-") $ readFile "input"
        print $ solutionA (contents!!0) (contents!!1)
        print $ solutionB (contents!!0) (contents!!1)

solutionA :: Int -> Int -> Int
solutionA a b = length [x | x <- [a..b], isIncreasing x, hasDouble x, isSixDigits x]

solutionB :: Int -> Int -> Int
solutionB a b =
    length [x | x <- [a..b], isIncreasing x, hasDouble x, isSixDigits x, numberDoubles x > numberTriples x]



isSixDigits :: Int -> Bool
isSixDigits x = length (show x) == 6

isIncreasing :: Int -> Bool
isIncreasing x = isIncreasingString $ show x
    where
        isIncreasingString    []  = True
        isIncreasingString (x:[]) = True
        isIncreasingString (x:y:ys)
            | x <= y    = isIncreasingString (y:ys)
            | otherwise = False

hasDouble :: Int -> Bool
hasDouble x = hasDoubleString $ show x
    where
        hasDoubleString    []  = False
        hasDoubleString (x:[]) = False
        hasDoubleString (x:y:ys)
            | x == y    = True
            | otherwise = hasDoubleString (y:ys)

numberDoubles :: Int -> Int
numberDoubles x = numberDoublesString $ show x
    where
        numberDoublesString    []  = 0
        numberDoublesString (x:[]) = 0
        numberDoublesString (x:y:ys)
            | x == y    = 1 + numberDoublesString (dropWhile (==x) ys)
            | otherwise =     numberDoublesString (y:ys)

numberTriples :: Int -> Int
numberTriples x = numberTriplesString $ show x
    where
        numberTriplesString    []  = 0
        numberTriplesString (x:[]) = 0
        numberTriplesString (x:y:[]) = 0
        numberTriplesString (x:y:z:zs)
            | x == y && y == z = 1 + numberTriplesString (dropWhile (==x) zs)
            | otherwise        =     numberTriplesString (y:z:zs)
