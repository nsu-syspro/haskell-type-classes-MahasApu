{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Task2 where

import Task1 (Parse, Parse(..), tokenize, Stack, Stack(..), pop, push, isDigit, toInteger')

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op =
    Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving Show

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving Show

-- * Parsing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
--
instance (Parse a, Parse op) => Parse (Expr a op) where
  parse x = parse'' (tokenize x)

parse'' :: (Parse a, Parse op) => [String] -> Maybe (Expr a op)
parse'' tokens = case foldl go (Just (Stack [])) tokens of
    Just (Stack [val]) -> Just val
    _                  -> Nothing

  where
    go :: (Parse a, Parse op) => Maybe (Stack (Expr a op)) -> String -> Maybe (Stack (Expr a op))
    go (Just stack) token =
        case parseLit token of
            Just val -> Just $ push (Lit val) stack
            Nothing  -> case parseOp token of
                        Just op -> apply op stack
                        Nothing -> Just $ push (Var token) stack
    go _ _ = Nothing

    parseLit :: (Parse a) => String -> Maybe a
    parseLit = parse

    parseOp :: (Parse op) => String -> Maybe op
    parseOp = parse

    apply :: op -> Stack (Expr a op) -> Maybe (Stack (Expr a op))
    apply binOp stack = do
      (r, stack1) <- pop stack
      (l, stack2) <- pop stack1
      Just (push (BinOp binOp l r) stack2)


parseInteger :: String -> Maybe Integer
parseInteger i
  | isDigit  i  = Just (toInteger' i)
  | otherwise   = Nothing

instance Parse Integer where
  parse :: String -> Maybe Integer
  parse = parseInteger

instance Parse IntOp where
  parse op = case op of
    "+" -> Just Add
    "*" -> Just Mul
    "-" -> Just Sub
    _   -> Nothing

-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a

instance Eval Integer IntOp where
  evalBinOp op = case op of
    Add -> (+)
    Mul -> (*)
    Sub -> (-)

-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
--
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr _    (Lit i) = Just i
evalExpr vars (Var v) = lookup v vars 
evalExpr vars (BinOp op l r) = do
    left  <- evalExpr vars l  
    right <- evalExpr vars r
    Just (evalBinOp op left right)  


-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
--
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger vars s  = case parse s :: Maybe (Expr Integer IntOp) of
  Just e  -> evalExpr vars e
  Nothing -> Nothing

-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'Reify' function is required to reconcile generic type
-- of intermediate 'Expr' expression with concrete type using 'a' and 'op'.
--
evaluate :: (Eval a op, Parse a, Parse op) => Reify a op -> [(String, a)] -> String -> Maybe a
evaluate reify m s = case parse s of
  Just e -> evalExpr m (reify e)
  Nothing -> Nothing

-- * Helpers

-- | Helper type for specifying 'Expr' with
-- concrete 'a' and 'op' in generic context
type Reify a op = Expr a op -> Expr a op

-- | Helper for specifying 'Expr' with 'Integer' and 'IntOp' in generic context
reifyInteger :: Reify Integer IntOp
reifyInteger = id



