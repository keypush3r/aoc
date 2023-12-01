{-# LANGUAGE RecordWildCards  #-}
module Y2022.AOC21 where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Sequence as Q
import Text.ParserCombinators.ReadP
import Text.Read (read)
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import Control.Monad
import Control.Applicative
import Debug.Trace
import Data.Tree
import Safe

data Terrain = Open | Wall deriving (Show, Eq)

data Vec2 = Vec2 { x :: Int, y :: Int } deriving (Show, Eq)

data Direction = No | Ea | So | We deriving (Show, Eq, Ord)

data Rot = LR | RR deriving (Show, Eq)

data Cmd = Move Int | Turn Rot deriving (Show, Eq)

data State = State { pos :: Vec2, dir :: Direction } deriving (Show, Eq)

type TerrainMap = World Terrain
type World a = [M.Map Int a]
type Input = (TerrainMap, [Cmd])


run1 :: String -> IO ()
run1 fileName = withInput fileName $ \g@(world, path) -> let
  start = State (findStart world) Ea
  dims = getDims world
  res = walkPath (warp1 world dims) world start path
  end@(State pos dir) = last res
  score = 1000 * (1 + y pos) + 4 * (1 + x pos) + (dirValue dir)
  in do
    -- putStrLn $ "warp " ++ (show $ warp world dims (dirTrans Ea) $ Vec2 11 5)
    --putStrLn $ "expr " ++ (show $ world)
    -- putStrLn $ "expr " ++ (show $ res )
    -- putStrLn $ "" ++ (drawMap world (\e -> case e of ; Open -> '.' ; Wall -> '#' ))
    putStrLn $ "" ++ (drawPathOnMap dims world res)
    putStrLn $ "dims " ++ (show dims)
    putStrLn $ "start " ++ (show start)
    putStrLn $ "end " ++ (show end)
    putStrLn $ "expr " ++ (show score)


withInput :: String -> (Input -> IO ()) -> IO ()
withInput fileName f = readFile fileName >>= maybe (error "boo") f . parseInput


walkPath :: (Vec2 -> Vec2 -> Maybe Vec2) -> TerrainMap -> State -> [Cmd] -> [State]
walkPath _ w s [] = [s]
walkPath warp w s ((Turn t):xs) = s : walkPath warp w (rotState t s) xs
walkPath warp w s@(State pos dir) ((Move distance):xs) = forward pos distance
  where
  forward curPos 0 = walkPath warp w (State curPos dir) xs
  forward curPos distanceLeft = let
    translation@(Vec2 dx dy) = dirTrans dir
    onBoard = fmap (flip (,) newPosVirt) posTerrainOpt
      where
        newPosVirt = add curPos translation
        posTerrainOpt = terrain w newPosVirt
    Just (posTerrain, newPos) = onBoard <|> (warp translation curPos >>= \e -> fmap (flip (,) e) $ terrain w e)
    in case posTerrain of
      Open -> (State curPos dir) : forward newPos (distanceLeft - 1)
      Wall -> (walkPath warp w (State curPos dir) xs)


warp1 :: TerrainMap -> Vec2 -> Vec2 -> Vec2 -> Maybe Vec2
warp1 w dims t@(Vec2 dx dy) fromPos = maybe (warp1 w dims t posNew) Just $ atMay w yNew >>= M.lookup (xNew)
  where posNes = Vec2 xNew yNew
        yNew = mod (dy + y fromPos) $ y dims
        xNew = mod (dx + x fromPos) $ x dims


getDims :: World a -> Vec2
getDims world = Vec2 {..}
  where x = 1 + (maximum $ fmap (maximum . M.keys) world)
        y = L.length world

add v1 v2 = Vec2 (x v1 + x v2) (y v1 + y v2)

terrain w pos = atMay w (y pos) >>= (M.lookup (x pos))

dirs = [Ea,So,We,No]

-- 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).
dirValue dir = fromMaybe (error "bad index" ) $ L.elemIndex dir dirs

translations = M.fromList [(No, Vec2 0 (-1)), (Ea, Vec2 1 0), (So, Vec2 0 1), (We, Vec2 (-1) 0)]

dirTrans d = fromMaybe (error "cant happen") $ M.lookup d translations

findStart w = Vec2  (minimum $ M.keys $ M.filter (==Open) $ w !! y) y where y = 0

rotState turn (State pos dir) = (State pos newDir)
  where
    newDir = let
      dirIndex = fromMaybe (error "asdfas") $ L.elemIndex dir dirs
      newDirIndex = mod (dirIndex + if turn == LR then (-1) else 1) 4
      in dirs !! newDirIndex
      

drawPathOnMap :: Vec2 -> TerrainMap -> [State] -> String
drawPathOnMap dims world path = flip (drawMap dims) tileRenderer $ flip appendPathToMap path $ terrain2Move world
  where terrain2Move = fmap (fmap Left)
        tileRenderer (Left ter) | ter == Open = '.'
                                | ter == Wall = '#'
        tileRenderer (Right dir) | dir == No = '^'
                                 | dir == Ea = '>'
                                 | dir == So = 'v'
                                 | dir == We = '<'


drawMap :: Vec2 -> World a -> (a -> Char) -> String
drawMap dims world tileRenderer = unlines $ fmap rowRenderer world
  where
  rowRenderer row = fmap renderTile [0..(x dims - 1)]
    where renderTile xx = maybe ' ' tileRenderer $ M.lookup xx row


appendPathToMap :: World (Either Terrain Direction) -> [State] -> World (Either Terrain Direction)
appendPathToMap world [] = world
appendPathToMap world (State (Vec2 {..}) dir:xs) = flip appendPathToMap xs $ toList $ Q.adjust modifyRow y $ Q.fromList world
  where modifyRow row = M.insert x (Right dir) row


parseInput :: String -> Maybe (TerrainMap, [Cmd])
parseInput s = case readP_to_S inputParser s of
  (res,"") : []  -> Just res
  otherwise -> error $ "Bad " ++ (s) ++ " : " ++ (show otherwise)
  where
  inputParser = let
    mapRowParser = let
      tileParser = satisfy $ flip L.elem [' ', '.', '#']
      char2Tile x = fromMaybe (error "bad tile") $ M.lookup x $ M.fromList [('.', Open), ('#', Wall)]
      in do
        tiles <- many1 $ tileParser
        return $ fmap char2Tile $ M.fromList $ filter ((/= ' ') . snd) $ zip [0..] tiles
    digitParser = do
      digits <- many1 $ satisfy (flip L.elem ['0'..'9'])
      return $ read $ digits
    turnCmdParser ch cmd = char ch >>= (return . const cmd)
    in do
      world <- many1 $ do
        row <- mapRowParser
        _ <- char '\n'
        return row
      _ <- char '\n'
      path <- do
        start <- fmap Move digitParser
        cont <- many1 $ do
          dir <- choice [turnCmdParser 'R' (Turn RR), turnCmdParser 'L' (Turn LR)]
          move <- fmap Move digitParser
          return [dir, move]
        return $ start : concat cont
      _ <- char '\n'
      eof
      return (world, path)