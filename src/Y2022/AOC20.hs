{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Y2022.AOC20 where

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
import qualified Data.Char as Char

run1 :: String -> IO ()
run1 fileName =
    withInput fileName $ \g -> let
        res = mix1 (L.length g) g
        nums = findNumsAfterZero [1000, 2000, 3000] res
        in do
          putStrLn $ "mix " ++ (show $ res)
          putStrLn $ "res " ++ (show $ nums)
          putStrLn $ "sum " ++ (show $ foldr (+) 0 nums)


run2 :: String -> IO ()
run2 fileName =
    withInput fileName $ \g -> let
        gg = fmap (*811589153) g
        res = let
          size = (L.length g)
          in foldl (\t e -> mix2 size t gg) gg [1..10]
        nums = findNumsAfterZero [1000, 2000, 3000] res
        in do
          putStrLn $ "inp " ++ (show $ gg)
          putStrLn $ "mix " ++ (show $ res)
          putStrLn $ "res " ++ (show $ nums)
          putStrLn $ "sum " ++ (show $ foldr (+) 0 nums)


findNumsAfterZero :: [Int] -> [Int] -> [Int]
findNumsAfterZero ns worked = let
  zeroPos = (maybe (error "no zero") id $  L.elemIndex 0 worked)
  size = (L.length worked)
  findNth n = mod n size
  in fmap (worked !! ) $ traceShowId $ fmap (findNth . (+(zeroPos))) ns


posEval size newPosVirt = 1 + mod (newPosVirt-1) (size - 1)


mix2 :: Int -> [Int] -> [Int] -> [Int]
mix2 maxMoves inp originalOrder = let
  size = length originalOrder
  movePos _  worked [] = worked
  movePos movesDone worked (num : xs)
    | movesDone < maxMoves = let
        Just pos = L.elemIndex num originalOrder
        newPos = posEval size $ pos + num
        in movePos (movesDone + 1) (move pos newPos id worked) xs
    | otherwise = worked
  in movePos 0 inp originalOrder


mix1 :: Int -> [Int] -> [Int]
mix1 maxMoves inp = let
  lifted = fmap Left inp
  size = length lifted
  movePos movesDone worked
    | movesDone < maxMoves =
      case filter (isLeft . snd) $ zip [0..] worked of
        (pos, Left num) : xs -> let
          newPos = traceShowId $ posEval size $ pos + num
          in movePos (movesDone + 1) $ move pos newPos (\(Left x) -> Right x) worked
        []  ->  worked
    | otherwise = worked
  unlift x = case x of
    Right y -> [y]
    Left y -> [y]
  in concatMap unlift $ movePos 0 lifted


move :: Int -> Int -> (a -> a) -> [a] -> [a]
move old new f worked
  | old >= 0 && old < size && new >= 0 && new < size  = let
    add new el worked = take new worked ++ (el : drop new worked)
    num = worked !! old
    remove old worked = (take old worked) ++ (drop (old + 1) worked)
    in add new (f num) $ remove old worked
  | otherwise = error "bad args"
    where size = L.length worked


withInput :: String -> ([Int] -> IO ()) -> IO ()
withInput fileName f = do
    inp <- readFile fileName
    case parseInput inp of
      Just res -> let
        in do
            putStrLn $ "input :" ++ show res
            putStrLn ""
            f res
      Nothing -> return ()


parseInput :: String -> Maybe [Int]
parseInput inp = sequence $ fmap parseRow inputLines
    where inputLines = lines inp


parseRow :: String -> Maybe Int
parseRow s = case readP_to_S rowParser s of
    (res,"") : []  -> Just res
    otherwise -> error $ "Bad " ++ (s) ++ " : " ++ (show $ length otherwise) ++ " : " ++ show otherwise
    where
        rowParser :: ReadP Int
        rowParser = let
          digitParser :: ReadP Int
          digitParser = do
            negs <- many $ char '-'
            digits <- many1 $ satisfy (flip L.elem ['0'..'9'])
            return $ read $ negs ++ digits
          in do
            number <- digitParser
            eof
            return number