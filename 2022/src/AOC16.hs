{-# LANGUAGE FlexibleContexts #-}
module Y2022.AOC16 where

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

type ValveName = String

type ValvesProps = M.Map ValveName ValveProps

data ValveProps = ValveProps {
    valveLabel :: ValveName, flowRate :: Int,  leadsTo :: [ValveName]
} deriving (Show, Eq)

escapeDuration = 30

boo g = filterTrees (\(S t acc a) -> maybe True (acc >=) $ M.lookup t mileStoneCriterias) res0
    where
        res0 = buildTrees g "AA" escapeDuration S.empty 0


run1 :: String -> IO ()
run1 fileName =
    withGrid fileName $ \g -> let
        res1 = boo g
        in case findBestChildren (S 0 0 (Move "AA")) res1 of
            Just res ->  putStrLn $ show $ fmap (drop 1) res  -- ++ show covered
            Nothing -> putStrLn "boo"

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

buildTrees :: ValvesProps -> ValveName -> Int -> S.Set ValveName -> Int -> [Tree State]
buildTrees props currentValve timeArriving openedValves accPressure
    | timeArriving > maxDepth && numOfFlowCensors > S.size openedValves = fmap handleAction availableActions
    | otherwise = []
    where
        curValveProps = maybe (error "asdf") id $ M.lookup currentValve props
        handleAction act@(Move travelTo) =
            Node (S newTime accPressure act) $ buildTrees props travelTo newTime openedValves accPressure
        handleAction act@(Open valveName ) =
            Node (S newTime newAcc act) $ buildTrees props valveName newTime (S.insert valveName openedValves) newAcc
                where
                    newAcc = (accPressure + (flowRate curValveProps) * (timeArriving - 1))
        newTime = (timeArriving - 1)
        availableActions = moves ++ opens
            where
                moves =  fmap (Move ) $ leadsTo curValveProps
                opens = if (not $ S.member currentValve openedValves) && (0 < flowRate curValveProps)
                    then [Open currentValve ]
                    else []



withGrid :: String -> (M.Map ValveName ValveProps -> IO ()) -> IO ()
withGrid fileName f = do
    inp <- readFile fileName
    case parseInput inp of
      Just valves -> let
        props = M.fromList $ fmap (\v -> (valveLabel v, v)) valves
        in do
            putStrLn $ "flow valves :" ++ (show $ L.length $ filter ((>0) . flowRate) valves)
                    -- putStrLn $ "valves : " ++ show valves
            putStrLn ""
            f props
      Nothing -> return ()


parseInput :: String -> Maybe [ValveProps]
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
      parseValve = many1 $ satisfy (flip L.elem ['A'..'Z'])
      in do
        _ <- string "Valve "
        valveLabel <- parseValve
        _ <- string " has flow rate=" -- 0
        flowRate <- digitParser
        _ <- choice [string "; tunnels lead to valves ", string "; tunnel leads to valve "]
        leadsTo <- sepBy parseValve (string ", ")
        eof
        return $ ValveProps valveLabel flowRate leadsTo

