module SpotIt where

import qualified Data.Map as Map

-- | We define three types of points:
-- * an ordinary point (x, y)
-- * a point at infinity corresponding to a slope m
-- * the point at infinity corresponding to infinite slope
data Point = OrdinaryPoint Int Int  -- x, y
           | PointAtInfinity Int    -- m
           | VerticalInfinity       --
    deriving (Ord, Eq, Show)

-- | Similarly, we define three types of lines:
-- * an ordinary line with slope m and intercept b
-- * a vertical line through (x, 0)
-- * the line at infinity
data Line = OrdinaryLine Int Int    -- m, b
          | VerticalLine Int        -- x
          | LineAtInfinity          -- 
    deriving (Ord, Eq, Show)

-- | there are n^2 ordinary points (x, y)
ordinaryPoints :: Int -> [Point]
ordinaryPoints n = [OrdinaryPoint x y | x <- [0..n-1], y <- [0..n-1]]

-- | along with n + 1 points at infinity, one for each slope
infinitePoints :: Int -> [Point]
infinitePoints n = VerticalInfinity : [PointAtInfinity m | m <- [0..n-1]]

-- | combine these to get all points
allPoints :: Int -> [Point]
allPoints n = ordinaryPoints n ++ infinitePoints n

-- | we can do the same to get all of the lines:
-- * n^2 ordinary lines,
-- * n vertical lines,
-- * 1 line at infinity
allLines :: Int -> [Line]
allLines n = ordinaryLines n ++ verticalLines n ++ [LineAtInfinity] where
    ordinaryLines n = [OrdinaryLine m b | m <- [0..n-1], b <- [0..n-1]]
    verticalLines n = [VerticalLine x | x <- [0..n-1]]

-- | given n and a Line, return the points on the line
-- three different cases depending on the type of line
pointsOnLine :: Int -> Line -> [Point]
pointsOnLine n (OrdinaryLine m b) = PointAtInfinity m : 
    [OrdinaryPoint x ((m * x + b) `mod` n) | x <- [0..n-1]]
pointsOnLine n (VerticalLine x) = VerticalInfinity :
    [OrdinaryPoint x y | y <- [0..n-1]]
pointsOnLine n LineAtInfinity = infinitePoints n

-- type aliases for our game constructs
type Picture = String
type Card = [Picture]
type Deck = [Card]

-- | Given n and a list of "pictures", create the deck of order n by
-- * finding all n^2 + n + 1 points
-- * creating a Map : Point -> Picture
-- * finding all n^2 + n + 1 lines 
-- * mapping each line to the list of points it contains
-- * mapping each list of points to the list of corresponding pictures
createDeck :: Int -> [Picture] -> Deck
createDeck n picNames = map (remap . pointsOnLine n) $ allLines n
    where
        encoding = Map.fromList $ zip (allPoints n) picNames
        remap = map (encoding Map.!)
                
-- For instance, to create a deck with "pictures" labeled 0 to 56, we could do
deck :: Deck
deck = createDeck 7 (map show [0..])

-- | Given two cards, return the unique picture they have in common
-- OK to use unsafe head because intersection is always one picture
picInCommon :: Card -> Card -> Picture
picInCommon card1 card2 = head commons
    where
        commons = [pic1 | pic1 <- card1, pic2 <- card2, pic1 == pic2]

-- | Given a deck, play a game
play :: Deck -> IO ()
play (card1:card2:cards) = do
    putStrLn $ show card1
    putStrLn $ show card2
    guess <- getLine
    putStrLn $ if guess == picInCommon card1 card2 then "correct!" else "wrong!"
    play cards
play _ = return ()
