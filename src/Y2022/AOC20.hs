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
    withGrid fileName $ \g -> let
        res = mix (L.length g) g
        nums = findNumsAfterZero [1000, 2000, 3000] res
        in do
          putStrLn $ "mix " ++ (show $ res)
          putStrLn $ "res " ++ (show $ nums)
          putStrLn $ "sum " ++ (show $ foldr (+) 0 nums)
        -- res1 = boo g
        -- in case findBestChildren (S 0 0 (Move "AA")) res1 of
        --     Just res ->  putStrLn $ show $ fmap (drop 1) res  -- ++ show covered
        --     Nothing -> putStrLn "boo"


findNumsAfterZero :: [Int] -> [Int] -> [Int]
findNumsAfterZero ns worked = let
  zeroPos = (maybe (error "no zero") id $  L.elemIndex 0 worked)
  size = (L.length worked)
  findNth n = mod n size
  in fmap (worked !! ) $ traceShowId $ fmap (findNth . (+(zeroPos))) ns


-- <a|b|c|d|e|f|

-- posEval size newPosVirt = let
--   newPosTmp = mod (newPosVirt) size
--   in 1 + mod (newPosTmp - 1) (size - 1)  -- if newPosTmp == 0 then size - 1 else newPosTmp
{-
   a b c a
   c     b
   b     c
   a c b a
-}
--posEval size newPosVirt =
--  case mod (((signum newPosVirt) * (signum (div newPosVirt size)) + newPosVirt) size of
--    0 -> size - 1
--    x -> x

posEval size newPosVirt = 1 + mod (newPosVirt-1) (size -1)



mix :: Int -> [Int] -> [Int]
mix maxMoves inp = let
  lifted = fmap Left inp
  size = length lifted
  movePos movesDone worked
    | movesDone < maxMoves =
      case filter (isLeft . snd) $ zip [0..] worked of
        (pos, Left num) : xs -> let
          --newPos = case traceShowId $  if num /= 0 then mod (traceShowId $ pos + num) size else pos of
          --            0 -> size -1
          --            x -> x
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


withGrid :: String -> ([Int] -> IO ()) -> IO ()
withGrid fileName f = do
    inp <- readFile fileName
    case parseInput inp of
      Just bluePrints -> let
        -- props = M.fromList $ fmap (\v -> (valveLabel v, v)) valves
        in do
            putStrLn $ "input :" ++ show bluePrints
                    -- putStrLn $ "valves : " ++ show valves
            putStrLn ""
            f bluePrints
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