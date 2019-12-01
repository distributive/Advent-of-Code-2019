import Control.Monad

main = do
        contents <- liftM (map read . words) $ readFile "input"
        print $ solutionA $ contents
        print $ solutionB $ contents

solutionA :: [Int] -> Int
solutionA input = foldr (\x -> \y -> x `div` 3 - 2 + y) 0 input

solutionB :: [Int] -> Int
solutionB input = foldr recFuel 0 input
    where
        recFuel x y
            | fuel x > 0 = fuel x + recFuel (fuel x) y
            | otherwise  = y
        fuel x = x `div` 3 - 2
