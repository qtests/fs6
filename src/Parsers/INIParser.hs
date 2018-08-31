  
  module Parsers.INIParser
  (
      ini
    , Parser (..)
    -- , INI (..)
    , read2MaybeInt
    , read2MaybeInteger
    , read2MaybeDouble
    , read2MaybeUTCTime
    , lookupSectionVariable
  )
  where


import Data.Functor
import Control.Applicative
import Control.Monad

import Data.Map hiding (empty)
import Data.Char
import Data.Time
import Text.Read (readMaybe)

  
type Variables = Map String String
type Sections = Map String Variables

newtype INI = INI Sections deriving Show
  
data Parser a = Parser { runParser :: String -> Maybe (a, String) }
  
  
instance Functor Parser where
  fmap f (Parser p) =
    let parserfunc input = do
          (x, remaining) <- p input
          return (f x, remaining)
    in Parser parserfunc


instance Applicative Parser where
  pure x = let parserfunc input = Just (x, input)
           in Parser parserfunc

  (Parser pf) <*> (Parser pa) =
    let parserfunc input = do
          (f, remaining) <- pf input
          (a, remaining2) <- pa remaining
          return (f a, remaining2)

    in Parser parserfunc

instance Monad Parser where
  return = pure

  (Parser pa) >>= fab =
    let parsefunc input = do
          (a, remaining) <- pa input
          runParser (fab a) remaining
    in Parser parsefunc


instance Alternative Parser where

  empty = Parser (\_ -> Nothing )

  (Parser pa) <|> (Parser pb) =
    let parsefunc input = case pa input of
                            Nothing -> pb input
                            Just (x, remaining) -> Just (x, remaining)
    in Parser parsefunc

  some v =
    let parsefunc input = do
          (x, remaining) <- runParser v input
          (xs, remaining2) <- runParser (many v) remaining
          return (x:xs, remaining2)
    in Parser parsefunc

  many v =
    let parsefunc input = case runParser (some v) input of
                            Just (xs, remaining) -> Just (xs, remaining)
                            Nothing -> Just ([], input)
    in Parser parsefunc
          
      


instance MonadPlus Parser where

conditional :: (Char -> Bool) -> Parser Char
conditional f =
    let parsefunc [] = Nothing
        parsefunc (x:xs) | f x = Just (x, xs)
        parsefunc _ = Nothing
    in Parser parsefunc

  
--- Parsing Functions -----------------------------------

char :: Char -> Parser Char
char c = conditional (== c) 

bracketed :: Parser a -> Parser b -> Parser c -> Parser b
bracketed pa pb pc = do
  pa
  b <- pb
  pc
  return b

bracketOpen :: Parser Char
bracketOpen = char '['

bracketClose :: Parser Char
bracketClose = char ']'

alphanum :: Parser Char
alphanum = conditional isAlphaNum

isAlphaNumExt :: Char -> Bool
isAlphaNumExt x = (isAlphaNum x) || ('_' == x) || ('-' == x)

alphanumExt :: Parser Char
alphanumExt = conditional isAlphaNumExt

isWhiteSpace :: Char -> Bool
isWhiteSpace ' ' = True
isWhiteSpace '\t' = True
isWhiteSpace _ = False

whitespace :: Parser Char
whitespace = conditional isWhiteSpace

whitespaces :: Parser String
whitespaces = many whitespace

sectionName :: Parser String
sectionName = bracketed whitespaces (some alphanum) whitespaces

sectionHeader :: Parser String
sectionHeader = bracketed bracketOpen sectionName bracketClose

name :: Parser String
name = (some alphanumExt)

quote :: Parser Char
quote = char '\"'

quotedchar :: Parser Char
quotedchar = conditional (\c -> isAlphaNumExt c || isWhiteSpace c)

quotedvalue :: Parser String
quotedvalue = bracketed quote (many quotedchar) quote

value :: Parser String
value = name <|> quotedvalue

assignment :: Parser (String,String)
assignment = do
  whitespaces
  name <- name
  whitespaces
  char '='
  whitespaces
  value <- value
  return (name, value)

newline :: Parser Char
newline = conditional (\c -> c == '\r' || c == '\n' )

newlines :: Parser ()
newlines = many newline >> return ()

blank :: Parser ()
blank = whitespaces >> newline >> return ()

blanks :: Parser ()
blanks = many blank >> return ()

assignments :: Parser Variables
assignments = fromList <$> many (blanks >> assignment)

section :: Parser (String, Variables)
section = do
  blanks
  whitespaces
  name <- sectionHeader
  blanks
  variables <- assignments
  return (name, variables)

ini :: Parser INI
ini = (INI . fromList) <$> many section

--- Processing Functions -----------------------------------

read2MaybeInt :: String -> Maybe Int
read2MaybeInt x = readMaybe x :: Maybe Int


read2MaybeInteger :: String -> Maybe Integer
read2MaybeInteger x = readMaybe x :: Maybe Integer


read2MaybeDouble :: String -> Maybe Double
read2MaybeDouble x = readMaybe x :: Maybe Double


read2MaybeUTCTime :: String -> String -> Maybe UTCTime
read2MaybeUTCTime format x = parseTimeM True defaultTimeLocale format x :: Maybe UTCTime


lookupSectionVariable :: INI -> String -> String -> Maybe String
lookupSectionVariable ini secName secVariable = variableValue
  where
    (INI sections) = ini
    variableValue = do 
      asection <- Data.Map.lookup secName sections 
      Data.Map.lookup secVariable asection 

