{-# LANGUAGE FlexibleContexts #-}
module Y2022.AOC18 where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Text.ParserCombinators.ReadP
import Text.Read (read)
import Control.Monad
import Data.Ord
import Data.Maybe (fromMaybe, maybeToList)
import Data.Proxy
import Data.Either (isRight,isLeft)
import Control.Applicative (liftA2)
import Debug.Trace
import Safe
import Data.Tree

escapeDuration = 30


run1 :: String -> IO ()
run1 fileName =
    withGrid fileName $ \g -> let
        res = calcSides g
        in putStrLn $ "res : " ++ show res


data Cat = Drop | Outside | Empty deriving (Eq,Show)

run2 :: String -> IO ()
run2 fileName =
    withGrid fileName $ \poses -> let
        -- poses = fmap t3toPos $ S.toList g
        dims = dropDims $ poses
        initial = makeInitial dims Empty
        posesAsList = S.toList poses
        grid = makeCubes posesAsList Drop initial
        Just res = fill grid Outside ( == Empty) (Vec3 1 1 1)
        onlyEmpty = findEmpty res Empty
        onlyEmptySides = calcSides onlyEmpty
        -- res = 1
        in do
            putStrLn $ "initial : " ++ show initial
            putStrLn $ "grid : " ++ show grid
       --     putStrLn $ "boo : " ++ (show $ fmap ((==Empty)) $ atPos grid (Vec3 1 1 1) )
       --     putStrLn $ "boo : " ++ (show $ put Outside grid (Vec3 1 1 1) )
            putStrLn $ "dims : " ++ show dims
            putStrLn $ "at pos : " ++ (show $ atPos grid $ Vec3 2 2 5)
            putStrLn $ "to deduct from part 1 : " ++ show onlyEmptySides
            --putStrLn $ "res : " ++ show res

makeInitial dims def = makeDim (x dims) (makeDim (y dims) (makeDim (z dims) def))
            where makeDim dim def = M.fromList $ zip [fst dim .. snd dim] $ L.repeat def


type Cubes = M.Map Int (M.Map Int (S.Set Int))
type CubesAsSet = S.Set Pos
type Cubes3 d = M.Map Int (M.Map Int (M.Map Int d))

data Vec3 a = Vec3 { x :: a, y :: a, z :: a } deriving (Show,Eq, Ord)
type Pos = Vec3 Int
type Dim = Vec3 Range
type Range = (Int,Int)

t3toPos (x,y,z) = Vec3 x y z

dropDims :: S.Set Pos -> Dim
dropDims poses = case fmap minMax [x, y , z] of
        rx : ry : rz : [] -> Vec3 rx ry rz
    where
        minMax f = (L.minimum coords, L.maximum coords)
            where
            coords = fmap f $ S.toList poses

makeCubes :: [Pos] -> d -> Cubes3 d -> Cubes3 d
makeCubes coords def initial = foldl (put def) initial coords
    --add :: Cubes3 a -> (Int,Int,Int) -> Cubes3 a

put def t (Vec3 x y z) = M.insertWith fy x (M.fromList [(y, M.singleton z def)]) t
    where
    -- fy :: M.Map Int (M.Map Int d) -> M.Map Int (M.Map Int d) -> M.Map Int (M.Map Int d)
    fy newY oldY = M.insertWith fz y (M.singleton z def) oldY
    -- fz :: M.Map Int d -> M.Map Int d -> M.Map Int d
    fz newZ oldZ = M.insert z def oldZ
    -- fx newX oldX = M.insertWith fy x newX oldX

adjacent = [(1,0,0),(-1,0,0),(0,1,0),(0,-1,0),(0,0,1),(0,0,-1)]

calcSides :: CubesAsSet -> Int
calcSides cubes = foldl addFreeSides 0 cubes
    where
    sideCoords (Vec3 x y z) = fmap (\(dx,dy,dz) -> Vec3 (x + dx) (y + dy) (z + dz)) adjacent
    addFreeSides :: Int -> Pos -> Int
    addFreeSides t p  = t + (L.length $ filter id $ fmap (not . flip S.member cubes) $ sideCoords p)

fill :: Cubes3 c -> c -> (c -> Bool) -> Pos -> Maybe (Cubes3 c)
fill grid def isFillable pos
    | (not $ maybe False id (fmap (isFillable) $ atPos grid pos) ) = Nothing
    | otherwise = Just $ foldl (\t e -> maybe t id $ fill t def isFillable e) newGrid $ fmap (addP pos . t3toPos) adjacent
        where newGrid =
            --traceShow pos $
                        put def grid pos

findEmpty :: Eq c => Cubes3 c -> c -> CubesAsSet
findEmpty grid needle = foldl (fx) S.empty $ M.toList grid
    where
    -- fx :: S.Set Pos -> (Int, M.Map Int (M.Map Int cc)) -> S.Set Pos
    fx tx (xx,v) = foldl (fy) tx $ M.toList v
        where
        fy ty (yy,v) = foldl (fz) ty $ M.toList v
            where
            fz tz (zz,v) = if v == needle then (S.insert (Vec3 xx yy zz) tz) else tz
            --fz tz (zz,v) = foldl (\t (e -> if v == Empty then (S.insert (Vec3 xx yy zz) t) else t) tz v

addP :: Pos -> Pos -> Pos
addP (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 {x = x1 + x2, y = y1 + y2, z = z1 + z2 }

atPos :: Cubes3 c -> Pos -> Maybe c
atPos cubes pos = do
    xx <- M.lookup (x pos) cubes
    yy <- M.lookup (y pos) xx
    M.lookup (z pos) yy



withGrid :: String -> (CubesAsSet -> IO ()) -> IO ()
withGrid fileName f = do
    inp <- readFile fileName
    case parseInput inp of
      Just coords -> let
        props = S.fromList $ fmap t3toPos coords
        in do
            putStrLn $ "flow valves :" ++ (show props)
                    -- putStrLn $ "valves : " ++ show valves
            putStrLn ""
            f props
      Nothing -> return ()


parseInput :: String -> Maybe [(Int,Int,Int)]
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
      in do
        x <- digitParser
        _ <- char ','
        y <- digitParser
        _ <- char ','
        z <- digitParser
        eof
        return (x,y,z)

{-
grid : fromList [
    (1,fromList [
        (1,fromList [
            (1,Empty),(2,Empty),(3,Empty)]),
        (2,fromList [
            (1,Empty),(2,Drop),(3,Empty)]),
        (3,fromList [
            (1,Empty),(2,Empty),(3,Empty)]),
        (5,fromList [
            (5,Drop)])
     ]
    )
   ,(2,fromList [
        (1,fromList [(1,Drop),(2,Empty),(3,Empty)]),(2,fromList [(1,Empty),(2,Drop),(3,Empty)]),(3,fromList [(1,Empty),(2,Empty),(3,Drop)]),(4,fromList [(4,Drop)]),(5,fromList [(5,Drop)]),(6,fromList [(6,Drop)])]),(3,fromList [(1,fromList [(1,Empty),(2,Empty),(3,Empty)]),(2,fromList [(1,Empty),(2,Drop),(3,Empty)]),(3,fromList [(1,Empty),(2,Empty),(3,Empty)]),(5,fromList [(5,Drop)])])]

-}