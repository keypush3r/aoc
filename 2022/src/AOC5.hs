
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Y2022.AOC5 where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromJust,fromMaybe,maybeToList)
import Data.Foldable (toList)
import Control.Applicative hiding ( many)
import Control.Monad
import Data.Ord (comparing)
import Text.ParserCombinators.ReadP

type Crate = Char
type CrateStack = [Crate]
type CrateStacks = [CrateStack]

data Move = Move {
  num :: Int
, from :: Int
, to :: Int
} deriving (Show,Eq)

restack :: Bool -> CrateStacks -> Move -> CrateStacks
restack is9001 stacks (Move {..}) = let
  stacksMap = M.fromList $ zip [1..] stacks
  stack = fromMaybe (error "boo") $ M.lookup from stacksMap
  (remain, moved) = splitAt (length stack - num) stack
  stackMoveOp = if is9001 then id else L.reverse
  res = M.update (\x -> Just remain) from $ M.update (\x -> Just $ x ++  stackMoveOp moved ) to stacksMap
  in fmap snd $ L.sortBy (comparing fst) $ M.toList res

restackMulti :: Bool -> CrateStacks -> [Move] -> CrateStacks
restackMulti is9001 stacks moves = L.foldl (\state move -> restack is9001 state move) stacks moves


numbers :: [Char]
numbers = ['0'..'9']

intParser = many $ satisfy $ \x -> L.elem x numbers

moveParser :: ReadP Move
moveParser = do
  _ <- string "move "
  numS <- intParser
  _ <- string " from "
  fromS <- intParser
  _ <- string " to "
  toS <- intParser
  eof
  return $  Move (read numS) (read fromS) (read toS)


movesFromText :: String -> [Move]
movesFromText str = let
  contents = lines str
  (res : _)   = forM contents $ readP_to_S moveParser
  in fmap fst res


stacksFromText :: String -> CrateStacks
stacksFromText str = let
  rows = reverse $ lines str
  (firstRow : _) = rows
  emptyStacks = replicate (length firstRow) []
  addCrates stacks crates = fmap (\(stack, crateChar) -> if crateChar /= ' ' then (stack ++ [crateChar]) else stack) $ zip stacks crates
  in foldl (\t e -> addCrates t e) emptyStacks rows


solveEx :: Bool -> [String]
solveEx is9001 = restackMulti is9001 (stacksFromText exSetup) (movesFromText exMoves)
  where
    exSetup =
      " D \n\
      \NC \n\
      \ZMP"
    exMoves =
      "move 1 from 2 to 1\n\
      \move 3 from 1 to 3\n\
      \move 2 from 2 to 1\n\
      \move 1 from 1 to 2"


solveQuiz :: Bool -> IO [String]
solveQuiz is9001  = do
  contents <- readFile "./input_aoc5.txt"
  return $ restackMulti is9001 (stacksFromText inputStacks) (movesFromText contents)
  where
    inputStacks :: String
    inputStacks =
          "B     N H\n\
          \V  PT V P\n\
          \W CTS H N\n\
          \T JZMNF L\n\
          \Q WNJTQRB\n\
          \NBQRVFDFM\n\
          \HWSJPWLPS\n\
          \DDTFGBBHZ"





---- OUTPUT

{-

ghci> solveEx
["C","M","PDZN"]
ghci> solve
["WVQQNF","LWVFJ","PDPBFRPFSB","TMHDH","NT","TZHWJZMD","HQNWBTBCJ","NSNVLSRBTP","G"]

-}
