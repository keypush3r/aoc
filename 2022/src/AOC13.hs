module Y2022.AOC13 where

import qualified Data.Map as M
import qualified Data.List as L
import Text.ParserCombinators.ReadP
import Text.Read (read)
import Control.Monad
import Data.Ord
import Control.Applicative (liftA2)
import Debug.Trace

newtype SubPacket = SubPacket [Either Int SubPacket] deriving (Eq,Show)

compPktOrder a b = case compPkt a b of
  Just x | x -> LT
         | not x -> GT
  Nothing -> EQ

compPkt (SubPacket pp1) (SubPacket pp2) = f pp1 pp2
  where
    f [] [] = Nothing
    f [] (p2:p2rest) = Just True
    f (p1:p1rest) [] = Just False
    f (p1:p1rest) (p2:p2rest) = case resOpt of
      Just res -> Just res
      Nothing -> f p1rest p2rest
      where
        resOpt =
          case (p1,p2) of
            (Left p1, Left p2) | p1 == p2 -> Nothing
                               | otherwise -> Just $ p1 < p2
            (Right p1, Right p2) -> compPkt p1 p2
            (Left p1, Right p2) -> compPkt (SubPacket [Left p1]) p2
            (Right p1, Left p2) -> compPkt p1 (SubPacket [Left p2])


parseInput :: String -> Maybe [(SubPacket,SubPacket)]
parseInput inp = sequence $ traceShowId $ fmap (complete . parsePair) pairsStr
  where
    complete (x,y) = liftA2 (,) x y
    parsePair (x,y) = (parsePacket x, parsePacket y)
    parsePacket s = case readP_to_S packetParser s of
      (res,"") : []  -> Just res
      otherwise -> error $ "Bad " ++ (s) ++ " : " ++ (show $ length otherwise)++ " : " ++ show otherwise
    inputLines = lines inp
    pairUp (x1 : x2 : rest) = (x1,x2) : pairUp rest
    pairUp [] = []
    pairsStr :: [(String,String)]
    pairsStr = pairUp res
      where res = fmap snd $ filter ((/=0) . (flip mod 3) . fst) $ zip [1..] inputLines


packetParser :: ReadP SubPacket
packetParser = let
  digitParser :: ReadP Int
  digitParser = fmap read $ many1 $ satisfy (flip L.elem ['0'..'9'])
  elemParser :: ReadP (Either Int SubPacket)
  elemParser = choice [(fmap Left digitParser), (fmap Right packetParser)]
  in do
    _ <- char '['
    elems <- sepBy elemParser (char ',')
    _ <- char ']'
    return $ SubPacket elems


inp1 = "[1,1,3,1,1]\n\
  \[1,1,5,1,1]\n\
  \\n\
  \[[1],[2,3,4]]\n\
  \[[1],4]\n\
  \\n\
  \[9]\n\
  \[[8,7,6]]\n\
  \\n\
  \[[4,4],4,4]\n\
  \[[4,4],4,4,4]\n\
  \\n\
  \[7,7,7,7]\n\
  \[7,7,7]\n\
  \\n\
  \[]\n\
  \[3]\n\
  \\n\
  \[[[]]]\n\
  \[[]]\n\
  \\n\
  \[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
  \[1,[2,[3,[4,[5,6,0]]]],8,9]"

run = do
  -- let inp = inp1
  inp <- readFile "./aoc13inp.txt"
  case parseInput inp of
    -- PART 1 :
    -- Just res -> let
    --   Just correct = fmap (filter snd . zip [1..]) $ sequence $ fmap (uncurry compPkt) res
    --   in do
    --     return $ foldr (\e t -> t + (fst e)) 0 $ correct
    -- PART 2 :   
    Just res -> let
       Just divPkgs = parseInput "[[2]]\n[[6]]"
       [(divPkg1, divPkg2)] = divPkgs
       correct = zip [1..] $ L.sortBy compPktOrder $ (concatMap (\(a,b) -> [a,b]) $ res ++ divPkgs)
       [(pkg1Pos,_)] = filter ((==divPkg1) . snd) correct
       [(pkg2Pos,_)] = filter ((==divPkg2) . snd) correct
       in do
        mapM_ (putStrLn . show) $ correct
        return (pkg1Pos * pkg2Pos)
    Nothing -> error "Bad input"


