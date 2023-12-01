module Y2022.AOC21 where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Text.ParserCombinators.ReadP
import Text.Read (read)
import Control.Monad
import Debug.Trace
import Data.Tree

type ExprRef = String

data Op = Add | Sub | Mult | Div deriving (Show,Eq)

data ExprS = LitS Int | ApplyS ExprRef ExprRef Op deriving (Show,Eq)

type NamedExpr = (ExprRef, ExprS)
type Input = [NamedExpr]
type Deps = M.Map ExprRef [ExprRef]

data Expr = Lit Int | Apply Expr Expr Op deriving (Show,Eq)


run1 :: String -> IO ()
run1 fileName = withInput fileName $ \g -> let
  deps = buildDeps g
  tree : [] = buildExprRefTree deps
  (exprName, expr) = buildExpr (M.fromList g) tree
  in putStrLn $ "expr " ++ (show $ eval expr)


run2 :: String -> Int -> Int -> IO ()
run2 fileName guess maxSteps = withInput fileName $ \gPrim -> let
  deps = buildDeps gPrim
  tree : [] = buildExprRefTree deps
  gRaw = M.fromList gPrim

  rightSide = let
    (exprName, expr) = buildExpr gRaw tree
    Apply left right op = expr
    in eval right
  fkn guess2 = let
    g = M.insert "humn" (LitS guess2) gRaw
    (exprName, expr) = buildExpr g tree
    Apply left right op = expr
    in rightSide - eval left
  res = newtonsMethod maxSteps (fkn) (100 + fkn guess) (10 + guess) guess
  in do
    putStrLn $ "fkn guss " ++ (show $ fkn guess)
    putStrLn $ "rightside " ++ (show $ rightSide)
    putStrLn $ "res iter " ++ (show $ res)


newtonsMethod :: Int -> (Int -> Int) -> Int -> Int -> Int -> [(Int,Int)]
newtonsMethod stepsMax f previousResult previousGuess currentGuess
  | stepsMax <= 0 = []
  | otherwise = let
    currentResult = f currentGuess
    diff = previousResult - currentResult
    in  ((,) currentResult currentGuess) : case () of
        () | currentResult == 0 -> error $ "found" ++ show currentGuess
           | otherwise -> (newtonsMethod (stepsMax - 1) f currentResult previousGuess nextGuess )
              where nextGuess = currentGuess - ( (div ((previousGuess - currentGuess) * currentResult) diff))

-- (if (currentResult * 2) < diff && (currentResult * 2) > diff  then 1 else 0)

withInput :: String -> (Input -> IO ()) -> IO ()
withInput fileName f = readFile fileName >>= maybe (error "boo") f . parseInput


eval :: Expr -> Int
eval (Lit x) = x
eval (Apply expL expR op) = (f op) (eval expL) (eval expR)
  where
  f Add = (+)
  f Sub = (-)
  f Mult = (*)
  f Div = (div)


buildExpr :: M.Map ExprRef ExprS -> Tree ExprRef -> (ExprRef, Expr)
buildExpr inp (Node k children) = let
  exprS = maybe (error "asdf") id $ M.lookup k inp
  in (,) k $
    case exprS of
       LitS val | L.null children -> Lit val
       ApplyS leftS rightS op | 2 == L.length children -> let
        operands = M.fromList $ fmap (buildExpr inp) children
        pick kk = maybe (error "term not found") id $ M.lookup kk operands
        in Apply (pick leftS) (pick rightS) op


buildExprRefTree :: Deps -> [Tree ExprRef]
buildExprRefTree deps = buildTrees roots
  where
  allChildren = S.fromList $ concat $ M.elems deps
  roots = S.toList $ S.difference (S.fromList $ M.keys deps) allChildren
  buildTrees parents = fmap buildTree parents
    where buildTree k = Node k $ buildTrees children
            where children = maybe [] id $ M.lookup k deps


buildDeps :: [NamedExpr] -> Deps
buildDeps inp = foldl f M.empty inp
  where f t (name, ApplyS eRef1 eRef2 op) = M.insert name [eRef1,eRef2] t
        f t (name, _) = t


parseInput :: String -> Maybe Input
parseInput inp = sequence $ fmap parseRow $ lines inp


parseRow :: String -> Maybe NamedExpr
parseRow s = case readP_to_S rowParser s of
  (res,"") : []  -> Just res
  otherwise -> error $ "Bad " ++ (s) ++ " : " ++ (show otherwise)
  where
  rowParser = let
    digitParser = do
      negs <- many $ char '-'
      digits <- many1 $ satisfy (flip L.elem ['0'..'9'])
      return $ read $ negs ++ digits
    nameParser = count 4 $ satisfy (flip L.elem ['a'..'z'])
    opParser = do
      let ops = [('+',Add),('-',Sub),('*',Mult),('/',Div)]
      opS <- satisfy (flip L.elem $ fmap fst ops)
      return $ maybe (error "cant happen") id $ M.lookup opS $ M.fromList ops
    appParser = do
      left <- nameParser
      _ <- char ' '
      op <- opParser
      _ <- char ' '
      right <- nameParser
      return $ ApplyS left right op
    valueParser = fmap LitS digitParser
    in do
      name <- nameParser
      _ <- string ": "
      job <- choice [valueParser, appParser]
      eof
      return (name, job)