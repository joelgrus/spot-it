module SpotIt where

import Control.Monad (guard)
import qualified Data.Map as Map

data Point = OrdinaryPoint Int Int  -- x, y
           | PointAtInfinity Int    -- m
           | VerticalInfinity       --
    deriving (Ord, Eq, Show)

data Line = OrdinaryLine Int Int    -- m, b
          | VerticalLine Int        -- x
          | LineAtInfinity          -- 
    deriving (Ord, Eq, Show)

upTo :: Int -> [Int]
upTo n = [0 .. (n - 1)]

-- hey, Haskell has list comprehensions too!
ordinaryPoints :: Int -> [Point]
ordinaryPoints n = [OrdinaryPoint x y | x <- upTo n, y <- upTo n]

infinitePoints :: Int -> [Point]
infinitePoints n = VerticalInfinity : [PointAtInfinity m | m <- upTo n]

allPoints :: Int -> [Point]
allPoints n = ordinaryPoints n ++ infinitePoints n

allLines :: Int -> [Line]
allLines n = ordinaryLines n ++ verticalLines n ++ [LineAtInfinity] where
    ordinaryLines n = [OrdinaryLine m b | m <- upTo n, b <- upTo n]
    verticalLines n = [VerticalLine x | x <- upTo n]

pointsOnLine :: Int -> Line -> [Point]
pointsOnLine n (OrdinaryLine m b) = PointAtInfinity m : 
    [OrdinaryPoint x ((m * x + b) `mod` n) | x <- upTo n]
pointsOnLine n (VerticalLine x) = VerticalInfinity :
    [OrdinaryPoint x y | y <- upTo n]
pointsOnLine n LineAtInfinity = infinitePoints n

type Picture = String
type Card = [Picture]
type Deck = [Card]

createDeck :: Int -> [Picture] -> Deck
createDeck n picNames = map (remap . pointsOnLine n) $ allLines n
    where
        encoding = Map.fromList $ zip (allPoints n) picNames
        remap = map (encoding Map.!)
                
-- create a deck with "pictures" labeled 0 to 56
-- createDeck 7 (map show [0..])

picsInCommon :: Card -> Card -> [Picture]
picsInCommon card1 card2 = do
    pic1 <- card1
    pic2 <- card2
    guard $ pic1 == pic2
    return pic1

picInCommon :: Card -> Card -> Picture
picInCommon c1 c2 = (!! 0) $ picsInCommon c1 c2
    
play :: Deck -> IO ()
play (card1:card2:cards) = do
    putStrLn $ show card1
    putStrLn $ show card2
    let common = picInCommon card1 card2
    guess <- getLine
    if guess == common
        then putStrLn "correct!"
        else putStrLn "wrong!"
    play cards
play _ = return ()
