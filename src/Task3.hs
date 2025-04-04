{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Task3 where

import Task1 (tokenize, Parse, Parse(..))
import Task2 (Expr, Expr(..), Eval, Eval(..), parse'', evalExpr)
import Data.Maybe (mapMaybe)
import Data.List (nub)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
-- >>> solveSAT "z x xor"
-- Just True

solveSAT :: String -> Maybe Bool
solveSAT = solve . tokenize

-- >>> comprehend ['x', 'y', 'z'] [0, 1]
-- [('x',0),('x',1),('y',0),('y',1),('z',0),('z',1)]
comprehend :: [a] -> [b] -> [(a, b)]
comprehend xs ys = [(x, y) | x <- xs, y <- ys]


-- >>> possibleSet $ extractVars $ tokenize "x y xor x y and or"
-- Just [[("x",False),("y",False)],[("x",False),("y",True)],[("x",True),("y",False)],[("x",True),("y",True)]]
possibleSet :: Maybe [String] -> Maybe [[(String, Bool)]]
possibleSet (Just vars) = Just $ sequence [comprehend [v] [False, True] | v <- vars]
possibleSet _           = Nothing

-- >>> extractVars $ tokenize "x y xor x y and or"
-- Just ["x","y"]
extractVars :: [String] -> Maybe [String]
extractVars s = Just (nub (filter (`notElem` ["and", "or", "xor"]) s))


instance Parse Bool where
  parse "true"  = Just True
  parse "false" = Just False
  parse _       = Nothing  


instance Parse BoolOp where
  parse "and" = Just And
  parse "or"  = Just Or
  parse "xor" = Just Xor
  parse _     = Nothing 

data BoolOp = And | Or | Xor
  deriving Show

instance Eval Bool BoolOp where
  evalBinOp op = case op of
    And -> (&&)
    Or  -> (||)
    Xor -> (/=)

solve :: [String] -> Maybe Bool
solve tokens = do
  exprs   <- parse'' tokens :: Maybe (Expr Bool BoolOp)
  vars    <- extractVars tokens  
  sets    <- possibleSet (Just vars)
  let res  = mapMaybe (`evalExpr` exprs) sets  
  Just (or res)
