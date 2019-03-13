-- | Simple parser combinators

module Wax.Parser where

import Prelude
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a,String)] }

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s -> [(f a, b) | (a, b) <- cs s]

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  (Parser cs1) <*> (Parser cs2) = Parser $ \s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]

instance Alternative Parser where
  empty = Parser $ \_ -> []
  p <|> q = Parser $ \s ->
    case parse p s of
    []  -> parse q s
    res -> res

runParser :: Parser a -> String -> Either String a
runParser m s = case parse m s of
  [(a, [])] -> Right a
  [(_, _)]  -> Left "parser did not consume entire stream"
  _         -> Left "parser error"

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser parser
  where parser (x:xs) | p x = [(x, xs)]
        parser _            = []

char :: Char -> Parser Char
char = satisfy . (==)

space :: Parser Char
space = char ' '

string :: String -> Parser String
string [] = pure []
string (x:xs) = char x *> string xs *> pure (x:xs)

token :: Parser a -> Parser a
token p = many space *> p <* many space

--------------------------------------------------------------------------------
main :: IO ()
main = putStrLn "parser example"
