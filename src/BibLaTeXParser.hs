{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}

module BibLaTeXParser (parseFile, parseEntry, bibtexString, Field, camelCase, normalEntryParser) where

import BibLaTeX

import Text.Parsec hiding (parse)
import Text.Parsec ((<?>), (<|>))

import Control.Monad.Identity (Identity)

import Data.Char

import Data.Map (Map)
import qualified Data.Map as Map

import LaTeXParser (latexCommand, BibLaTeXParser, parse)


camelCase :: String -> String
camelCase s = first : last
    where first = (toUpper . head) s
          last = map toLower $ tail s

skipTillEntry = manyTill anyChar (char '@')

entryTypeParser :: BibLaTeXParser String
entryTypeParser = do
    name <- many1 letter
    spaces
    return $ camelCase name

{-
data Field = Field  { name :: String, value :: String }
instance Show Field where
    show (Field name content) = show name ++ " = " ++ show content
-}

bibtexString :: Bool -> BibLaTeXParser String
bibtexString withQuotes = do
    start <-  many1 (noneOf (if withQuotes then "{}\\" else "{}\\\"")) <|> curlyString withQuotes <|> latexCommand
    end <- option "" $ bibtexString withQuotes
    return $ start ++ end

curlyString :: Bool -> BibLaTeXParser String
curlyString insideQuotes = between (char '{') (char '}') (option "" $ string "\"" <|> bibtexString True)

quotedString = between (char '"') (char '"') $ option "" (bibtexString False)

stringConstant :: BibLaTeXParser String
stringConstant = do
    id <- idParser
    s <- getState
    let value = Map.lookup id s
    case value of
        Just s -> do
            rest <- option "" $ char '#' >> spaces >> bibtexString False
            return $ s ++ rest
        _ -> fail $ "String constant " ++ id ++ " undefined"

fieldParser :: BibLaTeXParser (Field)
fieldParser = do
    name <- idParser
    _ <- spaces >> char '=' >> spaces
    content <- curlyString False <|> quotedString <|> many1 digit <|> stringConstant
    _ <- spaces
    let fff = if name == "ciao" then
                Field (Generic name content)
            else
                Field (Year 12)
    return fff
 {-
    case mkConstr x of
        Just c -> return $ c name content
        Nothing -> return $ Generic name content
-}

separator = char ',' >> spaces

idParser = do
    firstChar <- letter
    rest <- many (noneOf " {}\"=,@")
    spaces
    return $ firstChar : rest

--entryParser :: BibLaTeXParser  (String, String, [Field t])
entryParser = do
    _ <- skipTillEntry
    entryType <- entryTypeParser
    _ <- char '{' >> spaces
    (name, fields) <- if entryType == "String" then stringEntryParser else normalEntryParser
    return (entryType, name, fields)

normalEntryParser :: BibLaTeXParser (String, [Field])
normalEntryParser = do
    name <- idParser
    separator
    fields <- sepEndBy fieldParser separator -- todo: skip repeated fields
    _ <- char '}'
    spaces
    return $ (name, fields)

stringEntryParser :: BibLaTeXParser (String, [Field])
stringEntryParser = do
    field <- fieldParser
    _ <- char '}'
    spaces
    s <- getState
    let (name, value) =
                case field of
                     Field (Generic a b) -> (a, b)
                     _ -> ("", "")
    let s' = Map.insert (name) (value) s

    setState s'
    return $ ("", [])


parseFile = parse (sepEndBy entryParser (many $ noneOf "@"))

parseEntry = parse entryParser

parseLatex = parse (bibtexString False)