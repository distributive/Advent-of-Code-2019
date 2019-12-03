import Control.Monad

main = do
        contents <- liftM (map read . words) $ readFile "input"
        print $ solutionA $ contents
        print $ solutionB $ contents

solutionA :: [Int] -> Int
solutionA (x:_:_:xs) = head $ runCode (x:12:2:xs) 0

solutionB :: [Int] -> Int
solutionB input = test 19690720 input $ concat $ map diag $ iterate (1+) 0
    where
        diag x = [(m,n) | n <- [0..x], let m = x - n]
        test ans code (pair:pairs)
            | runCode (applyPair code pair) 0 !! 0 == ans = 100 * fst pair + snd pair
            | otherwise                                   = test ans code pairs
        applyPair (x:_:_:xs) (m,n) = (x:m:n:xs)

runCode :: [Int] -> Int -> [Int]
runCode code i
    | code!!i ==  1 = runCode (add (deref (i+1)) (deref (i+2)) (index(i+3))) $ i + 4
    | code!!i ==  2 = runCode (mul (deref (i+1)) (deref (i+2)) (index(i+3))) $ i + 4
    | code!!i == 99 = code
    | otherwise     = [-1]
    where
        add x y i = (slice 0 i code) ++ [x+y] ++ (slice (i+1) (length code) code)
        mul x y i = (slice 0 i code) ++ [x*y] ++ (slice (i+1) (length code) code)
        index i = code!!i
        deref i = code!!(code!!i)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)
