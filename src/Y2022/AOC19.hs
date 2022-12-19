{-# LANGUAGE FlexibleContexts #-}
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
        in putStrLn "woo"
        -- res1 = boo g

        -- in case findBestChildren (S 0 0 (Move "AA")) res1 of
        --     Just res ->  putStrLn $ show $ fmap (drop 1) res  -- ++ show covered
        --     Nothing -> putStrLn "boo"


collectionDuration = 24

{-
boo g = filterTrees (\(S t acc a) -> maybe True (acc >=) $ M.lookup t mileStoneCriterias) res0
    where
        res0 = buildTrees g "AA" escapeDuration S.empty 0



run11 :: String -> IO ()
run11 fileName =
    withGrid fileName $ \g -> let
        res = boo g
        in do
            putStrLn "Best : "
            forM_ res $ \x -> putStrLn $ drawTree $ fmap show  x -- ++ show covered


filterTree :: (State -> Bool) -> Tree State -> Maybe (Tree State)
filterTree f n@(Node s []) = Just n
filterTree f (Node s children) = case filterTrees f children of
    [] -> Nothing
    res -> Just $ Node s res

filterTrees :: (State -> Bool) -> [Tree State] -> [Tree State]
filterTrees f children = concatMap (maybeToList . filterTree f) $ filter (\n@(Node s _) -> f s) children


delayEnforcing = 6
scorePerTimeReq = 33

--mileStoneCriterias = M.fromList [(27,560),(24,872),(17,1292),(15,1326),(10,1500),(5,1650)]
mileStoneCriterias = M.fromList [(27,300),(24,400),(20,800),(15,1600),(10,1800),(5,1900),(0,1950)]

maxDepth = 0
numOfFlowCensors = 15


findBest :: Tree State -> Maybe (Int, [State])
findBest (Node s@(S t acc a) []) = Just (acc,[s])
findBest (Node s@(S t acc a) children) = findBestChildren s children

findBestChildren :: State -> [Tree State] -> Maybe (Int, [State])
findBestChildren a [] = error "should not happen"
findBestChildren a children = fmap (fmap (a:)) bestOpt
    where
    retained = filter (\(Node (S t acc a) _) -> scoreCriteria t acc) children
            where
                scoreCriteria _ _ = True
            --scoreCriteria t acc = scoreRequirement <= acc
            --    where scoreRequirement = max 0 (((escapeDuration - t) - delayEnforcing) * scorePerTimeReq )
    bestOpt :: Maybe (Int,[State])
    bestOpt = case retained of
        [] -> Nothing
        xs -> do
            (y, ys) <- L.uncons $ concatMap maybeToList $ fmap (findBest) xs
            return $ L.maximumBy (comparing fst) $ y : ys


data Action = Move ValveName | Open ValveName deriving (Eq, Show)
type Res = (ValveName, Int)

data State = S Int Int Action deriving (Show, Eq)

-}

type State = (Robots, Produced)

data BuildAction = Build RobotType | Noop


buildTrees :: Int -> Blueprint -> Int -> Robots -> Produced -> [Tree State]
buildTrees maxDepth bluePrint timeArriving machines prod
    | timeArriving > maxDepth = makeNodes 
    | otherwise = []
    where
        makeNodes = fmap buildNode machineBuildActions
        buildNode (Build robotType) = let
            newProduced
            in M.unionWith (-) prod (maybe M.empty id $ M.lookup robotType bluePrint)
        affordableRobots = fmap fst $ filter canBuild $ M.toList bluePrint
            where
            canBuild (robotType, costs) = all affordResource $ M.toList costs
                where affordResource (resourceType, cost) = maybe False (\have -> have >= cost) $ M.lookup resourceType prod
        machineBuildActions = Noop : fmap Build affordableRobots
        newProduced = M.unionWith (+) prod machines

withGrid :: String -> (M.Map BlueprintId Blueprint -> IO ()) -> IO ()
withGrid fileName f = do
    inp <- readFile fileName
    case parseInput inp of
      Just bluePrints -> let
        -- props = M.fromList $ fmap (\v -> (valveLabel v, v)) valves
        in do
            putStrLn $ "flow valves :" ++ show bluePrints
                    -- putStrLn $ "valves : " ++ show valves
            putStrLn ""
            f bluePrints
      Nothing -> return ()


resourceProd :: ResourceType -> Produced -> Int
resourceProd res prod = maybe 0 id $ M.lookup res prod

--heuristic :: Produced -> Ordering
heuristic = comparing (resourceProd Geode) <> comparing (resourceProd Obsidian)
    <> comparing (resourceProd Clay) <> comparing (resourceProd Ore)

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
            -- Blueprint 30: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 14 clay. Each geode robot costs 4 ore and 10 obsidian.
            _ <- string "Blueprint "
            blueprintId <- digitParser
            _ <- string ": "
            bluePrintParts <- sepBy parseRobotRequirements (string " ")
            eof
            return $ (blueprintId, M.fromList bluePrintParts)

capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []