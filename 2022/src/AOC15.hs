{-# LANGUAGE FlexibleContexts #-}
module Y2022.AOC15 where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Text.ParserCombinators.ReadP
import Text.Read (read)
import Control.Monad
import Data.Ord
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Either (isRight,isLeft)
import Control.Applicative (liftA2)
import Debug.Trace
import Safe

data Coord = Coord { x :: Int, y :: Int } deriving (Eq,Ord, Show)
data Grid = Grid {
        xspan :: (Int,Int)
      , yspan :: (Int,Int)
      , sensors2RadiiAndClosest :: M.Map Coord (Int, Coord)
      , beacons :: S.Set Coord
    } deriving (Show)
type Interval = (Int,Int)



run1 :: Int -> String -> IO ()
run1 rowNr fileName =
    withGrid fileName $ \g -> let
        covered = findEmptyOnRow g (xspan g) rowNr
        in putStrLn $ "res" ++ (show $ length covered) -- ++ show covered

run2 :: Interval -> Interval -> String -> IO ()
run2 xRange yRange fileName =
    withGrid fileName $ \g -> let
        covered = findEmpty g xRange yRange
        -- in putStrLn $ "res" ++ (show $ length covered) -- ++ show covered
        in putStrLn $ "res" ++ (show $ length covered) ++ show covered



withGrid :: String -> (Grid -> IO ()) -> IO ()
withGrid fileName f = do
    inp <- readFile fileName
    case parseInput inp of
      Just pairs -> let
        g = makeGrid pairs
        in do
            putStrLn $ "pairs" ++ show pairs
            putStrLn ""
            putStrLn $ "grid" ++ show g
            putStrLn ""
            f g
      Nothing -> return ()

{-
####B
#####
#####
#####
#######
-}

distance c1 c2 = abs ((x c2) - (x c1)) + abs ((y c2) - (y c1))

parseInput :: String -> Maybe [(Coord, Coord)]
parseInput inp = sequence $ fmap parseRow inputLines
  where
    parseRow s = case readP_to_S rowParser s of
      (res,"") : []  -> Just res
      otherwise -> error $ "Bad " ++ (s) ++ " : " ++ (show $ length otherwise) ++ " : " ++ show otherwise
    inputLines = lines inp
    rowParser = let
      digitParser :: ReadP Int
      digitParser = do
        negs <- many $ char '-'
        digits <- many1 $ satisfy (flip L.elem ['0'..'9'])
        return $ read $ negs ++ digits
      coordParser :: ReadP Coord
      coordParser = do
        _ <- string "x="
        x <- digitParser
        _ <- string ", y="
        y <- digitParser
        return $ Coord x y
      in do
        _ <- string "Sensor at "
        sensorPos <- coordParser
        _ <- string ": closest beacon is at "
        beaconPos <- coordParser
        eof
        return $ (,) sensorPos beaconPos

findEmpty :: Grid -> Interval -> Interval -> [Coord]
findEmpty g xRange yRange = concat $ fmap (findEmptyOnRow g xRange) [fst $ yRange .. snd $ yRange]

findEmptyOnRow :: Grid -> Interval -> Int -> [Coord]
findEmptyOnRow g xRange rowNr = let
    sensors = fmap (\(a,b) -> (a,fst b)) $ M.toList $ sensors2RadiiAndClosest g
    sensorsInRange = filter (\(s, radii) -> radii >= abs ((y s) - rowNr) ) sensors
    sensorsInRangeSorted = L.sortBy (comparing (\s -> (y $ fst s) - rowNr )) $ sensorsInRange
    in traceShow rowNr $ findEmptyOnRow2 sensorsInRangeSorted xRange rowNr


-- findEmptyOnRow2 :: [(Coord,Int)] -> Interval -> Int -> [Coord]
-- findEmptyOnRow2 sensors xRange rowNr = let
--     intervalsFromSensors = traceShowId $ concat $ fmap sensToInterval sensors
--     sensToInterval :: (Coord, Int) -> [(Int,Int)]
--     sensToInterval (s,radii) = if halfWidth >= 0 then [(x s -halfWidth , x s + halfWidth)] else []
--         where
--             halfWidth = radii - vertDistFromSensor
--             vertDistFromSensor = abs (y s - rowNr)
--     contained i = any (\(b,t) -> i >= b && i <= t) intervalsFromSensors
--     in fmap (flip Coord rowNr) $ filter (not . contained) [fst $ xRange .. snd $ xRange]

findEmptyOnRow2 :: [(Coord,Int)] -> Interval -> Int -> [Coord]
findEmptyOnRow2 sensors xRange rowNr = let
    intervalsFromSensors = concat $ fmap sensToInterval sensors
    -- mergeInts (x1:x2:xs) =
    --mergeInts (x1:xs) =
    --mergeInts [] =
    intervals = let
        foo inp = let
            out = mergeRanges $ L.sortOn snd inp
            in if L.length inp == L.length out then out else foo out
        in foo intervalsFromSensors
    sensToInterval :: (Coord, Int) -> [(Int,Int)]
    sensToInterval (s,radii) = if halfWidth >= 0 then [(x s -halfWidth , x s + halfWidth)] else []
        where
            halfWidth = radii - vertDistFromSensor
            vertDistFromSensor = abs (y s - rowNr)
    contained i = any (\(b,t) -> i >= b && i <= t) intervals
    in case intervals of
        x1 : x2 : xs -> error $ show rowNr ++ " : " ++ show intervals
        x1 : xs -> []
        [] -> error "boo"


mergeRanges (i1@(lo1,hi1) : i2@(lo2,hi2) : rest)
    | overlaps i1 i2 = mergeRanges ((merge i1 i2) : rest)
    | hi1 + 1 == lo2 = mergeRanges ((merge i1 i2) : rest)

             -- or (lo1,hi2) : mergeRanges rest, to merge only adjacent ranges
mergeRanges (interval:rest) = interval : mergeRanges rest
mergeRanges [] = []

merge i1@(a1, b1) i2@(a2, b2) = (min a1 a2, max b1 b2)
--
overlaps i1@(a1, b1) i2@(a2, b2) = contains i1 a2 || contains i1 b2 || encloses i1 i2 || encloses i2 i1
--
encloses i1@(a1, b1) i2@(a2, b2) = a1 < a2 && b1 > b2
--
contains (a1, b1) x1 = a1 <= x1 && x1 <= b1




    --filter (not . isCovered) $ fmap (flip Coord rowNr) [fst $ xRange .. snd $ xRange]
    ----filter (not . flip S.member (beacons g)) $
    --    where
    --        isCovered pos = any (uncurry posCoveredBySensor) sensors
    --          where
    --            posCoveredBySensor s radii =  -- traceShow (show pos ++ " : " ++ show dist ++ " " ++ show radii) $
    --                radii >= dist
    --                where dist = (distance pos s)

makeGrid :: [(Coord, Coord)] -> Grid
makeGrid coordPairs = Grid xSpan ySpan sensors beacons
    where
        sensors = M.fromList $ fmap (\(s,b) -> (,) s (distance s b, b) ) coordPairs
        -- coords = concatMap (\(a,b) -> [a,b]) coordPairs
        coords = M.toList $ fmap (\(r,_) -> r) sensors
        beacons = S.fromList $ fmap snd coordPairs
        -- spans cs acc = (minimum $ accCoords . fst, maximum accCoords) where accCoords = fmap acc cs
        spans :: [(Coord,Int)] -> (Coord -> Int) -> (Int,Int)
        spans cs acc = (minimum $ accCoords (-1), maximum $ accCoords 1)
            where accCoords sig = fmap (\c -> (acc $ fst c) + sig * snd c) cs
        xSpan = spans coords x
        ySpan = spans coords y
