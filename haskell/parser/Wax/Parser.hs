-- | Simple parser combinators

module Wax.Parser where

import Prelude
import Control.Applicative

data Result a = Success String a | Failure String String

newtype Parser a = Parser { parse :: String -> Result a }

instance Functor Result where
  fmap _ (Failure s e) = Failure s e
  fmap f (Success s a) = Success s $ f a

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s -> f <$> cs s

instance Applicative Parser where
  pure a = Parser $ \s -> Success s a
  (Parser cs1) <*> (Parser cs2) = Parser $ \s -> case cs1 s of
    Failure s1 e -> Failure s1 e
    Success s1 f -> f <$> cs2 s1

instance Alternative Parser where
  empty   = Parser $ \_ -> Failure "" "Empty parser"
  p <|> q = Parser $ \s -> case parse p s of
    Failure _ _     -> parse q s
    r@(Success _ _) -> r

satisfy :: (Char -> Bool) -> String -> Parser Char
satisfy p e = Parser parser
  where parser (x:xs) | p x = Success xs x
        parser s            = Failure s e

anyChar :: Parser Char
anyChar = satisfy (\_ -> True) ""

char :: Char -> Parser Char
char c = satisfy (c ==) (show c)

space :: Parser Char
space = char ' '

string :: String -> Parser String
string [] = pure []
string (x:xs) = char x *> string xs *> pure (x:xs)

token :: Parser a -> Parser a
token p = many space *> p <* many space

--------------------------------------------------------------------------------
runParser :: Parser a -> String -> Either String a
runParser m s = case parse m s of
  Success "" a -> Right a
  Success s0 _ -> Left $ "parser did not consume entire stream: " <> s0
  Failure s0 e -> Left $ concat
    [ "parser error", "\n"
    , "  expected: ", e, "\n"
    , "     input: \"", s0, "\""
    ]

--------------------------------------------------------------------------------
main :: IO ()
main =
  let t = "hello"
      i = "    hells  "
  in do
    putStrLn $ "Going to parse token '" <> t <> "' in '" <> i <> "'"
    case runParser (token . string $ t) i of
      Left e -> putStrLn e
      Right _ -> putStrLn "success"
