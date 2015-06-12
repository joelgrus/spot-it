module SpotIt where

import Control.Monad (guard)
import qualified Data.Map as Map

data Point = OrdinaryPoint Int Int Int  -- x, y, n
           | PointAtInfinity Int Int    -- m, n
           | VerticalInfinity Int       -- n
    deriving (Ord, Eq, Show)

data Line = OrdinaryLine Int Int Int    -- m, b, n
          | VerticalLine Int Int        -- x, n
          | LineAtInfinity Int          -- n
    deriving (Ord, Eq, Show)

upTo :: Int -> [Int]
upTo n = [0 .. (n - 1)]

-- hey, Haskell has list comprehensions too!
ordinaryPoints :: Int -> [Point]
ordinaryPoints n = [OrdinaryPoint x y n | x <- upTo n, y <- upTo n]

infinitePoints :: Int -> [Point]
infinitePoints n = VerticalInfinity n : [PointAtInfinity m n | m <- upTo n]

allPoints :: Int -> [Point]
allPoints n = ordinaryPoints n ++ infinitePoints n

allLines :: Int -> [Line]
allLines n = ordinaryLines n ++ verticalLines n ++ [LineAtInfinity n] where
    ordinaryLines n = [OrdinaryLine m b n | m <- upTo n, b <- upTo n]
    verticalLines n = [VerticalLine x n | x <- upTo n]

pointsOnLine :: Line -> [Point]
pointsOnLine (OrdinaryLine m b n) = PointAtInfinity m n : 
    [OrdinaryPoint x ((m * x + b) `mod` n) n | x <- upTo n]
pointsOnLine (VerticalLine x n) = VerticalInfinity n :
    [OrdinaryPoint x y n | y <- upTo n]
pointsOnLine (LineAtInfinity n) = infinitePoints n

createDeck :: Int -> [String] -> [[String]]
createDeck n picNames = map (remap . pointsOnLine) $ allLines n
    where
        encoding = Map.fromList $ zip (allPoints n) picNames
        remap :: [Point] -> [String]
        remap = map (encoding Map.!)
                
-- create a deck with "pictures" labeled 0 to 56
-- createDeck 7 (map show [0..])

picsInCommon :: [String] -> [String] -> [String]
picsInCommon card1 card2 = do
    pic1 <- card1
    pic2 <- card2
    guard $ pic1 == pic2
    return pic1