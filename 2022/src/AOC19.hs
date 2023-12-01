{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Y2022.AOC19 where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Text.ParserCombinators.ReadP
import Text.Read (read)
import Control.Monad
import Data.Ord
import Data.Maybe (fromMaybe, maybeToList)
import Data.Proxy
import Data.Function (on)
import Data.Either (isRight,isLeft)
import Control.Applicative (liftA2)
import Debug.Trace
import Safe
import Data.Tree
import qualified Data.Char as Char




data ResourceType = Ore | Clay | Obsidian | Geode deriving (Show, Eq, Ord, Read)

type Robots = M.Map RobotType Int
type Produced = M.Map ResourceType Int
type RobotType = ResourceType

type Blueprint = M.Map RobotType (M.Map ResourceType Int)

type BlueprintId = Int
type Blueprints = M.Map BlueprintId Blueprint


run1 :: String -> IO ()
run1 fileName =
    withGrid fileName $ \g -> let
        (_, b) : bs = M.toList g
        (depth, res) = evaluateBlueprint b
        in do
          putStrLn $ "woo " ++ (drawTree $ fmap show res)
          putStrLn $ "bp " ++ show b
          mapM_ (putStrLn . show) $ topPaths res depth
          --putStrLn $ "woo " ++ (show $ listRes 24 [] $ evaluateBlueprint b)
        -- res1 = boo g

        -- in case findBestChildren (S 0 0 (Move "AA")) res1 of
        --     Just res ->  putStrLn $ show $ fmap (drop 1) res  -- ++ show covered
        --     Nothing -> putStrLn "boo"




type State = (Int, Maybe Build, Produced)

type Build = RobotType
type Path = [Maybe Build]

heuristic = comparing (Down . resourceProd Geode) <> comparing (Down . resourceProd Obsidian)
      <> comparing (Down . resourceProd Clay) <> comparing (Down . resourceProd Ore)

initialMachinery = M.singleton Ore 1


evaluateBlueprint blueprint = (collectionDuration,) $ foldl evaluate initialTree mileStones
  where
  mileStones = fmap (*8) [1..3]
  collectionDuration = head $ L.reverse mileStones
  initialTree : xs = buildTrees collectionDuration blueprint 0 initialMachinery M.empty
  evaluate :: Tree State -> Int -> Tree State
  evaluate tree maxDepth = let
    topPathsSorted = S.map fst $ topPaths tree maxDepth
    in maybe (error "filter tree returned Nothing") id $ filterTree maxDepth (\x ->
      -- traceShow ("Path " ++ show x) $
        any (L.isPrefixOf x) topPathsSorted) [] tree


topPaths tree maxDepth = let
  paths :: [(Path, (Robots, Produced))]
  paths = fmap augRobots $ listRes maxDepth [] tree
    where
      augRobots (path, prod) = (path, (makeRobots path, prod))
      makeRobots :: Path -> Robots
      makeRobots [] = initialMachinery ; makeRobots (x : xs) = case x of ; Nothing -> rest ; Just y -> M.unionWith (+) (M.singleton y 1) rest
        where
        rest = makeRobots xs

  fractions = 30
  -- a10th = max 100 (div (L.length paths) fractions + (signum $ mod (L.length paths) fractions))
  a10th = 50
  sorted = L.sortBy ((on) heuristic (fst . snd)) paths
  in S.fromList $ take a10th sorted


listRes :: Int -> Path -> Tree State -> [(Path, Produced)]
listRes maxDepth pathToHere (Node (time, ba, prod) children)
  | time == maxDepth = [(newPath, prod)]
  | time < maxDepth = (>>=) children (listRes maxDepth newPath)
  where
  newPath = pathToHere ++ [ba]

filterTree :: Int -> (Path -> Bool) -> Path -> Tree State -> Maybe (Tree State)
filterTree maxDepth f path (Node s@(_,ba,_) children)
  | maxDepth <= L.length newPath = Just $ (Node s children)
  | f newPath =
      case filterTrees maxDepth f newPath children of
        [] -> Nothing
        res -> Just $ Node s res
  | otherwise = Nothing
    where newPath = path ++ [ba]

filterTrees :: Int -> (Path -> Bool) -> Path -> [Tree State] -> [Tree State]
filterTrees maxDepth f path children = concatMap (maybeToList . filterTree maxDepth f path)   children



buildTrees :: Int -> Blueprint -> Int -> Robots -> Produced -> [Tree State]
buildTrees maxDepth bluePrint currentTime builtRobots prod
    | currentTime < maxDepth = makeNodes
    | otherwise = []
    where
        makeNodes = fmap buildTree machineBuildActions
        newTime = currentTime + 1
        buildTree Nothing = let
          newProd = newProduced prod
          in Node (newTime, Nothing, newProd) $ buildTrees maxDepth bluePrint newTime builtRobots newProd
        buildTree ba@(Just robotType)  = let
            producedAfterPurchase = M.unionWith (-) prod (maybe M.empty id $ M.lookup robotType bluePrint)
            newBuiltRobots = M.insertWith (+) robotType 1 builtRobots
            newProd = newProduced producedAfterPurchase
            in Node (newTime, ba, newProd) $ buildTrees maxDepth bluePrint newTime newBuiltRobots newProd
        affordableRobots = fmap fst $ filter canBuild $ M.toList bluePrint
            where
            canBuild (robotType, costs) = all affordResource $ M.toList costs
                where affordResource (resourceType, cost) = maybe False (\have -> have >= cost) $ M.lookup resourceType prod
        machineBuildActions = Nothing : fmap (Just ) affordableRobots
        newProduced currentProd = M.unionWith (+) currentProd builtRobots

withGrid :: String -> (M.Map BlueprintId Blueprint -> IO ()) -> IO ()
withGrid fileName f = do
    inp <- readFile fileName
    case parseInput inp of
      Just bluePrints -> let
        -- props = M.fromList $ fmap (\v -> (valveLabel v, v)) valves
        in do
            -- putStrLn $ "flow valves :" ++ show bluePrints
                    -- putStrLn $ "valves : " ++ show valves
            putStrLn ""
            f bluePrints
      Nothing -> return ()


resourceProd :: ResourceType -> Produced -> Int
resourceProd res prod = maybe 0 id $ M.lookup res prod


parseInput :: String -> Maybe Blueprints
parseInput inp = fmap M.fromList $ sequence $ fmap parseRow inputLines
    where inputLines = lines inp

parseRow :: String -> Maybe (BlueprintId, Blueprint)
parseRow s = case readP_to_S rowParser s of
    (res,"") : []  -> Just res
    otherwise -> error $ "Bad " ++ (s) ++ " : " ++ (show $ length otherwise) ++ " : " ++ show otherwise
    where
        rowParser :: ReadP (BlueprintId, Blueprint)
        rowParser = let
          digitParser :: ReadP Int
          digitParser = do
            negs <- many $ char '-'
            digits <- many1 $ satisfy (flip L.elem ['0'..'9'])
            return $ read $ negs ++ digits
          resourceTypeParser = many1 $ satisfy (flip L.elem (['A'..'Z'] ++ ['a'..'z']))
          parseRobotRequirements :: ReadP (RobotType, (M.Map ResourceType Int))
          parseRobotRequirements = let
            resourceReqParser :: ReadP (ResourceType, Int)
            resourceReqParser = do
                quantity <- digitParser
                _ <- char ' '
                resource <- resourceTypeParser
                return (read $ capitalized resource, quantity)
            in do
                _ <- string "Each "
                robotType <- resourceTypeParser
                _ <- string " robot costs "
                resourceReqs <- sepBy resourceReqParser (string " and ")
                _ <- char '.'
                return (read $ capitalized robotType, M.fromList resourceReqs)
          in do
            _ <- string "Blueprint "
            blueprintId <- digitParser
            _ <- string ": "
            bluePrintParts <- sepBy parseRobotRequirements (string " ")
            eof
            return $ (blueprintId, M.fromList bluePrintParts)

capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []