{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings


module Task1 where

import Prelude (Integer, Show, String, Maybe (Just, Nothing), words, (*), (+), (.), read, Foldable (elem, foldl), Functor(fmap), Bool, ($), all)

-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
--
evalIExpr :: IExpr -> Integer
evalIExpr (Lit i)   = i
evalIExpr (Add l r) = evalIExpr l + evalIExpr r
evalIExpr (Mul l r) = evalIExpr l * evalIExpr r


-- * Lexical part and some utils

tokenize :: String -> [String]
tokenize = words

toInteger :: String -> Integer
toInteger = read

isDigit :: String -> Bool
isDigit = all (\ c -> c `elem` ['0' .. '9'])

-- * Stack for Reverse Polish Notation

newtype Stack a = Stack [a]
  deriving (Show)

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack (x : xs)) = Just (x, Stack xs)
pop  _               = Nothing


-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
--
instance Parse IExpr where
  parse = parse' . tokenize


parse' :: [String] -> Maybe IExpr
parse' tokens = case foldl go (Just (Stack [])) tokens of
    Just (Stack [val]) -> Just val  
    _                  -> Nothing

  where 
    go :: Maybe (Stack IExpr) -> String -> Maybe (Stack IExpr)
    go (Just stack) token = case token of
      "+"   -> apply Add stack
      "*"   -> apply Mul stack 
      val   -> if   isDigit val 
               then Just $ push (Lit (toInteger val)) stack
               else Nothing
    go _ _   = Nothing

    apply :: (IExpr -> IExpr -> IExpr) -> Stack IExpr -> Maybe (Stack IExpr)
    apply binOp sstack = do 
      (r, stack1) <- pop sstack
      (l, stack2) <- pop stack1
      Just (push (binOp l r) stack2)


-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing

evaluateIExpr :: String -> Maybe Integer
evaluateIExpr = fmap evalIExpr . parse
