import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split

type Coord = (Int, Int)

main = do
        contents <- liftM (map (splitOn ",") . words) $ readFile "input"
        print $ solutionA (contents!!0) (contents!!1)
        print $ solutionB (contents!!0) (contents!!1)

solutionA :: [String] -> [String] -> Int
solutionA inputA inputB =
    manhatten $ foldr closest (10000000,10000000) $ intersections inputA inputB

solutionB :: [String] -> [String] -> Int
solutionB inputA inputB =
    minimum $ map (stepLength $ (coversWithSteps inputA, coversWithSteps inputB)) $ intersections inputA inputB



-- Gets the intersections of two wires
intersections :: [String] -> [String] -> [Coord]
intersections a b = Set.toList $ Set.delete (0,0) $ Set.intersection (covers a) (covers b)
    where
        covers :: [String] -> Set Coord
        covers path = snd $ foldl doCover ((0,0), Set.empty) path

        doCover :: (Coord, Set Coord) -> String -> (Coord, Set Coord)
        doCover ((x,y), grid) ('U':xs) = ((x, y + read xs), Set.union grid $ Set.fromList [(u,v) | let u = x, v <- [y..y + read xs]])
        doCover ((x,y), grid) ('D':xs) = ((x, y - read xs), Set.union grid $ Set.fromList [(u,v) | let u = x, v <- [y - read xs..y]])
        doCover ((x,y), grid) ('R':xs) = ((x + read xs, y), Set.union grid $ Set.fromList [(u,v) | u <- [x..x + read xs], let v = y])
        doCover ((x,y), grid) ('L':xs) = ((x - read xs, y), Set.union grid $ Set.fromList [(u,v) | u <- [x - read xs..x], let v = y])

-- Gets the coordinates covered by a wire with attached step length
coversWithSteps :: [String] -> [(Coord, Int)]
coversWithSteps path = snd $ foldl doCover ((0,0), [((0,0), 0)]) path
    where
        doCover :: (Coord, [(Coord, Int)]) -> String -> (Coord, [(Coord, Int)])
        doCover ((x,y), grid) ('U':xs) = ((x, y + read xs), grid ++ [((u,v), d) | let u = x, v <- [y+1..y + read xs], let d = (v-y) + (snd $ last grid)])
        doCover ((x,y), grid) ('D':xs) = ((x, y - read xs), grid ++ reverse [((u,v), d) | let u = x, v <- [y - read xs..y-1], let d = (y-v) + (snd $ last grid)])
        doCover ((x,y), grid) ('R':xs) = ((x + read xs, y), grid ++ [((u,v), d) | u <- [x+1..x + read xs], let v = y, let d = (u-x) + (snd $ last grid)])
        doCover ((x,y), grid) ('L':xs) = ((x - read xs, y), grid ++ reverse [((u,v), d) | u <- [x - read xs..x-1], let v = y, let d = (x-u) + (snd $ last grid)])

        overlaps :: [(Coord, Int)] -> (Coord, Int) -> [(Coord, Int)] -> [(Coord, Int)]
        overlaps coverA (b, bd) base =
            let intersects = filter (\(coord, int) -> coord == b) coverA in
            if length intersects > 0
                then let (a, ad) = head intersects in (a, ad + bd):base
                else base



-- General coord-based helper functions
closest :: Coord -> Coord -> Coord
closest x y
    | manhatten x < manhatten y = x
    | otherwise                 = y

manhatten :: Coord -> Int
manhatten (m,n) = abs m + abs n

stepLength :: ([(Coord, Int)], [(Coord, Int)]) -> Coord -> Int
stepLength covers coord =
    (snd $ head $ filter (\(c, d) -> c == coord) $ fst covers) +
    (snd $ head $ filter (\(c, d) -> c == coord) $ snd covers)
