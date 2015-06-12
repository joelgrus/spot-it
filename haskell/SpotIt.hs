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

ordinaryPoints :: Int -> [Point]
ordinaryPoints n = do
    x <- upTo n
    y <- upTo n
    return $ OrdinaryPoint x y n

infinitePoints :: Int -> [Point]
infinitePoints n = VerticalInfinity n : do
    m <- upTo n
    return $ PointAtInfinity m n

allPoints :: Int -> [Point]
allPoints n = ordinaryPoints n ++ infinitePoints n

allLines :: Int -> [Line]
allLines n = ordinaryLines n ++ verticalLines n ++ [LineAtInfinity n] where
    ordinaryLines n = do
        m <- upTo n
        b <- upTo n
        return $ OrdinaryLine m b n
    verticalLines n = do
        x <- upTo n
        return $ VerticalLine x n
        
pointsOnLine :: Line -> [Point]
pointsOnLine (OrdinaryLine m b n) = PointAtInfinity m n : do
    x <- upTo n
    return $ OrdinaryPoint x ((m * x + b) `mod` n) n
pointsOnLine (VerticalLine x n) = VerticalInfinity n : do
    y <- upTo n
    return $ OrdinaryPoint x y n
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