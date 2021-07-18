{-# LANGUAGE DeriveFunctor #-}
module Parser where

import Control.Monad ( ap, MonadPlus(..) )
import qualified Control.Applicative ( Applicative(..), Alternative(..) )
import Data.Char

newtype Parser a = P { unP :: State -> Consumed a }

data State = State {
    stateString :: String,  -- left input string
    statePos    :: Pos      -- current pos : (row, col)
}

data Consumed a =
    Consumed (Reply a)
  | Empty (Reply a)
  deriving (Functor)

data Reply a =
    Ok a State Message
  | Error Message
  deriving (Functor)

data Message = Msg Pos String [String]  -- error's pos, unpected and expected

type Pos = (Int, Int) -- row and column 

instance Show Message where
    show (Msg pos s exp) =
        "Parse error at " ++ show pos ++ ", "
        ++ show exp ++ " expected, "
        ++ "but unexpected : " ++ show s

instance Functor Parser where
    fmap f p = P $ \s -> case unP p s of
        result -> fmap f result

instance Applicative Parser where
    pure = parserReturn
    (<*>) = ap

instance Monad Parser where
    return = parserReturn
    (>>=) = parserBind

instance MonadPlus Parser where
    mzero = parserZero
    mplus = parserPlus

instance Control.Applicative.Alternative Parser where
    empty = mzero
    (<|>) = mplus

parserReturn :: a -> Parser a
parserReturn x = P $ \s -> Empty $ Ok x s $ Msg (statePos s) [] []

parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind m k = P $ \s -> case unP m s of
    Consumed (Ok a s' _) -> case unP (k a) s' of
                                Consumed reply -> Consumed reply
                                Empty reply    -> Consumed reply
    Empty (Ok a s' _)    -> case unP (k a) s' of
                                Consumed reply -> Consumed reply
                                Empty reply    -> Empty reply
    Consumed (Error msg) -> Consumed $ Error msg
    Empty (Error msg)    -> Empty $ Error msg

parserZero :: Parser a
parserZero = P $ \s -> Empty $ Error $ Msg (statePos s) [] []

parserPlus :: Parser a -> Parser a -> Parser a
parserPlus p q = P $ \s -> case unP p s of
    Empty (Error msg1) -> case unP q s of
                            Empty (Error msg2) -> mergeError msg1 msg2
                            Empty (Ok a s' msg2) -> mergeOk a s' msg1 msg2
                            consumed -> consumed
    result             -> result
  where
    mergeError msg1 msg2 = Empty $ Error $ merge msg1 msg2
    mergeOk a s msg1 msg2 = Empty $ Ok a s $ merge msg1 msg2
    merge (Msg pos s exp1) (Msg _ _ exp2) = Msg pos s $ exp1 ++ exp2

try :: Parser a -> Parser a
try p = P $ \s -> case unP p s of
    Consumed (Error msg) -> Empty $ Error msg
    result -> result

infix 0 <?>
infixr 1 <|>

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

(<?>) :: Parser a -> String -> Parser a
p <?> exp = P $ \s -> case unP p s of
    Empty (Error msg)   -> Empty $ Error $ expect msg exp
    Empty (Ok a s' msg) -> Empty $ Ok a s' $ expect msg exp
    other               -> other
  where
    expect (Msg pos s _) exp = Msg pos s [exp]



satisfy :: (Char -> Bool) -> Parser Char
satisfy test = P $ \(State s pos) -> case s of
    (c:cs) | test c -> let pos' = nextPos pos c
                           state' = State cs pos'
                       in pos' `seq` Consumed $ Ok c state' $ Msg pos [] []
           | otherwise -> Empty $ Error $ Msg pos [c] []
    []  -> Empty $ Error $ Msg pos "end of input" []

nextPos :: Pos -> Char -> Pos
nextPos (row, col) c = case c of
    '\n' -> (row + 1, 1)
    _    -> (row, col + 1)

digit = satisfy isDigit <?> "digit"
letter = satisfy isAlpha <?> "letter"
char c = satisfy (==c) <?> show c

many :: Parser a -> Parser [a]
many p = do
    { x <- p
    ; xs <- many p
    ; return (x:xs)
    }
    <|> return []

many1 p = do
    x <- p
    xs <- many p
    return (x:xs)

oneOf cs = satisfy (`elem` cs)

noneOf cs = satisfy (`notElem` cs)

integer :: Parser Integer
integer = read <$> many1 digit

string [] = return []
string (c:cs) = do
    x <- char c
    xs <- string cs
    return (x:xs)

identifier = do
    firstLetter <- letter
    leftLetters <- many (letter <|> digit <|> char '_')
    return (firstLetter:leftLetters)

whitespace = oneOf [' ', '\t', '\n']
whitespaces = many whitespace

parse :: Parser a -> String -> Either Message (a, String)
parse p s = case unP p (State s (1, 1)) of
    Consumed (Ok a s' _) -> Right (a, stateString s')
    Consumed (Error msg) -> Left msg
    Empty (Ok a s' _)    -> Right (a, stateString s')
    Empty (Error msg)    -> Left msg

keywordLet = do
    string "let"
    many1 whitespace
    identifier

test = try keywordLet <|> identifier

{-
>>> parse (whitespaces >> identifier) "     x4_5 dkd" 
Right ("x4_5"," dkd")
-}
