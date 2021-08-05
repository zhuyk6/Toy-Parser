{-# LANGUAGE LambdaCase #-}
module ApplicativeParser where

import Control.Applicative
import Data.Char

newtype Parser a = P { runP :: String -> [(String, a)] }

instance Functor Parser where
    fmap f p = P $ \s -> map (\(s', a) -> (s', f a)) $ runP p s

instance Applicative Parser where
    pure a = P $ \s -> [(s, a)]
    (P fp) <*> (P xp) = P $ \s -> [(s'', f x) | (s', f) <- fp s, (s'', x) <- xp s']

instance Alternative Parser where
    empty = P $ const []
    (P p) <|> (P q) = P $ \s -> p s ++ q s


---

predP :: (Char -> Bool) -> Parser Char
predP test = P $ \case
    (x:xs) | test x -> [(xs, x)]
    _               -> []

charP :: Char -> Parser Char 
charP c = predP (== c)

stringP :: String -> Parser String
-- stringP [] = pure []
-- stringP (x:xs) = (:) <$> charP x <*> stringP xs
stringP = foldr (\c p -> (:) <$> charP c <*> p) (pure [])

spaceP :: Parser Char 
spaceP = choice $ map charP " \t\n\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* many spaceP

symbolP = lexeme . stringP

between open close p = open *> p <* close

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

--- 

runParser :: Parser a -> String -> [a]
runParser p s = [a | (s', a) <- runP p s, null s']

runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p s = case runParser p s of 
    [a] -> Just a
    _   -> Nothing 

---

data BinOp = Add | Mul deriving (Eq, Show)

data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE   Expr
          | ZeroE  
    deriving (Show)

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 

exprP = constP <|> binOpExprP <|> negP <|> zeroP

constP = lexeme $ ConstE . read <$> some (predP isDigit)

binOpExprP = between (symbolP "(") (symbolP ")") 
    $ (\a f b -> BinOpE f a b) <$> exprP <*> binOpP <*> exprP


binOpP = choice [
      Add <$ symbolP "+"
    , Mul <$ symbolP "*"
    ]

negP = fmap NegE $ symbolP "-" *> exprP

zeroP = ZeroE <$ symbolP "z"


{-
>>> runParser (many spaceP *> exprP) " 1 "
[ConstE 1]
-}


--- 

-- ambiguous grammar:
-- E -> E' + E | E' * E | C 
-- E' -> C + E | C * E | C 
-- C -> integer 

e = (\a f b -> BinOpE f a b) <$> e' <*> binOpP <*> e <|> c 
e' = (\a f b -> BinOpE f a b) <$> c <*> binOpP <*> e <|> c 
c = constP

{-
>>> runParser (e) "1+2*3"
[BinOpE Mul (BinOpE Add (ConstE 1) (ConstE 2)) (ConstE 3),BinOpE Add (ConstE 1) (BinOpE Mul (ConstE 2) (ConstE 3))]
-}
