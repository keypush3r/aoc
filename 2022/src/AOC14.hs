module Y2022.AOC14 where

import qualified Data.Map as M
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

data CoordState = Empty | Rock | Sand deriving (Show,Eq)
type Grid = M.Map Int [CoordState]
data Coord = Coord { x :: Int, y :: Int } deriving (Eq,Show)
type Path = [Coord]
type Interval = (Int,Int)
data DropRes = Stop | OffGrid deriving (Eq, Show)
data P1Grid = P1Grid Grid
data P2Grid = P2Grid { bottom :: Int,  gri :: Grid }

class GridLike a where
    getState :: a -> Coord -> Maybe CoordState
    drawState :: Coord -> CoordState -> a -> a
    makeGridType :: Proxy a -> Interval -> Interval -> Coord -> (Interval -> Int -> Grid) -> a
    gg :: a -> Grid
    stopCheck :: a -> Coord -> Bool


run :: GridLike a => Proxy a -> String -> IO ()
run gridType fileName = do
  inp <- readFile fileName
  let dropCoord = (Coord 500 0)
  case parseInput inp of
    Just paths -> let
        res = L.unfoldr (\(n,grid) ->
            case dropSand grid dropCoord of
                Right g -> if stopCheck g dropCoord
                    then Nothing
                    else Just ((n+1,g),(n+1,g))
                Left _ -> Nothing
            ) (0, buildGrid gridType paths dropCoord)
        in case fmap fst $ L.uncons $ L.reverse res of
            Just (dropped,g) -> do
                putStrLn $ renderGrid $ gg g
                putStrLn $ "Dropped " ++ show dropped
                putStrLn $ (show $ M.size $ gg g) ++ " : " ++ (show $ L.length $ fromMaybe [] $ M.lookup 500 $ gg g)
            Nothing -> error "Bad"
    Nothing -> return ()


instance GridLike P1Grid where
    getState (P1Grid grid) (Coord x y) = do
        col <- M.lookup x grid
        atMay col y
    drawState pos st (P1Grid grid) = P1Grid $ drawStateGen pos st grid
    makeGridType _ xSpan ySpan dropCoord  fkn = P1Grid $ fkn xSpan (snd ySpan)
    gg (P1Grid grid) = grid
    stopCheck _ _ = False


instance GridLike P2Grid where
    getState (P2Grid vertSize grid) (Coord x y)
        | y >= vertSize = Just Rock
        | otherwise = do
            col <- M.lookup x grid
            atMay col y
    drawState pos st (P2Grid bot grid) = P2Grid bot $ drawStateGen pos st grid
    makeGridType _ xSpan ySpan dropCoord fkn = P2Grid (height) $ fkn xSpanMod (height)
        where
            xSpanMod = (,) (mid - halfWidth) (mid + halfWidth)
            halfWidth = height
            mid = (x dropCoord)
            height = 2 + snd ySpan
    gg (P2Grid _ grid) = grid
    stopCheck g dropCoord = getState g dropCoord == Just Sand

drawStateGen (Coord x y) st g = M.update updateRow x g
    where
        updateRow row = Just $ take y row ++ [st] ++ drop (y+1) row


dropSand :: GridLike a => a -> Coord -> Either DropRes a
dropSand grid pos =
    case getState grid pos of
        Just cSt -> case cSt of
            Empty -> let
                below = pos { y = y pos + 1 }
                allBelow = [below, below { x = x pos - 1 }, below{ x = x pos + 1 }]
                testPaths (path : rest) = case dropSand grid path of
                    Left OffGrid -> Just $ Left OffGrid
                    Left Stop -> testPaths rest
                    Right g -> Just $ Right g
                testPaths [] = Nothing
                in case testPaths allBelow of
                    Just res -> res
                    Nothing -> Right $ drawState pos Sand grid
            Rock -> Left Stop
            Sand -> Left Stop
        Nothing -> Left OffGrid


parseInput :: String -> Maybe [Path]
parseInput inp = sequence $ fmap parseRow inputLines
  where
    parseRow s = case readP_to_S rowParser s of
      (res,"") : []  -> Just res
      otherwise -> error $ "Bad " ++ (s) ++ " : " ++ (show $ length otherwise)++ " : " ++ show otherwise
    inputLines = lines inp
    rowParser :: ReadP Path
    rowParser = let
      digitParser :: ReadP Int
      digitParser = fmap read $ many1 $ satisfy (flip L.elem ['0'..'9'])
      coordParser :: ReadP Coord
      coordParser = do
        x <- digitParser
        _ <- char ','
        y <- digitParser
        return $ Coord x y
      in do
        elems <- sepBy coordParser (string " -> ")
        eof
        return elems


renderGrid :: Grid -> String
renderGrid grid = concat $ L.intersperse "\n" $ fmap (fmap toChar) $ L.transpose $ M.elems grid
    where
        toChar st = case st of
            Empty -> '.'
            Rock ->  '#'
            Sand ->  'O'


buildGrid :: GridLike a => Proxy a -> [Path] -> Coord -> a
buildGrid gridType paths dropCoord = foldl (\t e -> drawPath t e) emptyGrid paths
    where
        drawPath grid (x1:x2:xs) = drawPath (drawLine grid x1 x2) (x2:xs)
        drawPath grid (x1:xs) = grid
        drawPath grid [] = grid

        drawLine grid from to  = case () of
            () | from == to -> newGrid
               | otherwise -> drawLine newGrid nextFrom to
               where
                    nextFrom = Coord (next x) (next y)
                    next op = op to + nextDiff
                        where
                            nextDiff = if diff == 0 then  0
                                         else signum diff * (-1 + abs diff)
                            diff = (op from) - (op to)
                    newGrid = drawState from Rock grid

        emptyGrid = makeGridType gridType xSpan ySpan dropCoord $
            \horiz vertSize ->  M.fromList $ fmap (flip (,) $ L.replicate (1 + vertSize) Empty) [fst horiz..snd horiz]
        allInputCoords = concat paths
        ySpan = (0, L.maximum $ fmap y allInputCoords)
        xSpan = findEdges $ fmap x allInputCoords
        findEdges xs = (L.minimum xs, L.maximum xs)

