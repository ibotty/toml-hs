-- a parser based on parsec to parse toml files
module Toml (tDocument) where

-- import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec hiding (many, (<|>))

import Control.Applicative

import Numeric (readDec, readFloat, readSigned)

import Data.Time.Format (readsTime)
import Data.Time.Clock (UTCTime)
import System.Locale (defaultTimeLocale)


data TValue = TString String
            | TInteger Int
            | TFloat Float
            | TBool Bool
            | TDateTime UTCTime
            | TArray [TValue]
    deriving (Show)

-- get a parser from a read function
parserFromReads :: ReadS a -> Parser a
parserFromReads readFun = do s <- getInput
                             case readFun s of
                                  [(n, s')] -> n <$ setInput s'
                                  _         -> empty

spaces' :: Parser ()
spaces' = skipMany (oneOf "\t ")

trim :: Parser a -> Parser a
trim parser = spaces *> parser <* spaces

trim' :: Parser a -> Parser a
trim' parser = spaces' *> parser <* spaces'

eol' :: Parser ()
eol' = spaces' <* skipMany tComment <* eol <* skipCommentAndNewlines
    where eol = char '\n'

skipCommentAndNewlines :: Parser ()
skipCommentAndNewlines = spaces >> skipMany (tComment >> spaces)


-- primitives
-------------

tString :: Parser String
-- tString = between (char '"') (char '"') (many tChar)
tString = char '"' *> many tChar <* char '"'
    where tChar :: Parser Char
          tChar = char '\\' *> tEscape <|> satisfy (`notElem` "\\\"")
          tEscape :: Parser Char
          tEscape = choice $ zipWith decode "0tnr\"\\" "\0\t\n\r\"\\"
              where decode c r = r <$ char c

tNum :: Real a => ReadS a -> Parser a
tNum readPrim = parserFromReads $ readSigned readPrim

tInt :: Parser Int
tInt = tNum readDec

tFloat :: Parser Float
tFloat = tNum readFloat

tBool :: Parser Bool
tBool = True <$ string "true"
    <|> False <$ string "false"

tDatetime :: Parser UTCTime
tDatetime = parserFromReads $ readsTime defaultTimeLocale "%FT%XZ"

-- note: trim accepts newline, so this is ok
tArray :: Parser [TValue]
tArray = char '[' *> spaces' *> choice parsers <* char ']'
    where prepParser :: Parser TValue -> Parser [TValue]
          prepParser parser = trim parser  `sepBy1` char ','
          parsers = map prepParser constructor_parsers

--tArray = choice parsers
--    where outer :: Parser TValue -> Parser [TValue]
--          outer val = char '[' *> spaces' *> inner val <* spaces' <* char ']'
--          inner :: Parser TValue -> Parser [TValue]
--          inner val = trim val  `sepBy` char ','
--          parsers = map outer constructor_parsers


tValue :: Parser TValue
tValue = choice constructor_parsers <?> "TOML value"

constructor_parsers :: [ Parser TValue]
constructor_parsers = [ TString <$> tString
                , TDateTime <$> tDatetime
                , TInteger <$> tInt
                , TFloat <$> tFloat
                , TBool <$> tBool
                , TArray <$> tArray
                ]


-- ignore comments
tComment :: Parser ()
-- tComment = skipMany1 (char '#') >> skipMany (noneOf "\n")
tComment = char '#' *> skipMany (noneOf "\n")

tIdentifier :: Parser String
tIdentifier = many1 (alphaNum <|> oneOf "_")

tAssignment :: Parser (String, TValue)
tAssignment = (,) <$> (tIdentifier <* trim' (char '=')) <*> tValue <* eol'

tKeygroupHeader :: Parser [String]
tKeygroupHeader =  char '[' *> trim' (tIdentifier `sepBy1` char '.') <* char ']' <* eol'

tKeygroup :: Parser ([String], [(String, TValue)])
tKeygroup = (,) <$> tKeygroupHeader
                <*> many tAssignment

tDocument :: Parser [([String], [(String, TValue)])]
tDocument = skipCommentAndNewlines *> many tKeygroup
