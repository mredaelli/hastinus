module BibLaTeXParser where

import qualified Text.Parsec as Parsec

import Text.Parsec ((<?>), (<|>))

import Control.Applicative

import Control.Monad.Identity (Identity)

import Data.Char
import Data.Maybe
import Text.Read

import Data.Map (Map)
import qualified Data.Map as Map

type ParserState = Map String String


camelCase :: String -> String
camelCase s = first : last
    where first = (toUpper . head) s
          last = map toLower $ tail s

parse rule state  = Parsec.runParser rule state "(source)"

skipTillEntry = Parsec.manyTill Parsec.anyChar (Parsec.char '@')

entryTypeParser :: Parsec.Parsec String ParserState String
entryTypeParser = do
    name <- Parsec.many1 Parsec.letter
    Parsec.spaces
    return $ camelCase name

data Field = Field  { name :: String, value :: String }
instance Show Field where
    show (Field name content) = show name ++ " = " ++ show content

bibtexString :: Bool -> Parsec.Parsec String ParserState String
bibtexString withQuotes = do
    start <-  Parsec.many1 (Parsec.noneOf (if withQuotes then ['{','}', '\\'] else ['{','}', '"', '\\'])) Parsec.<|>  curlyString withQuotes Parsec.<|>  latexCommand
    end <- Parsec.option "" $ bibtexString withQuotes
    return $ start ++ end

curlyString :: Bool -> Parsec.Parsec String ParserState String
curlyString insideQuotes = do
    value <- Parsec.between (Parsec.char '{') (Parsec.char '}') (Parsec.option "" $ Parsec.string "\"" Parsec.<|> bibtexString True)
    return value

quotedString = do
    value <- Parsec.between (Parsec.char '"') (Parsec.char '"') $ Parsec.option "" (bibtexString False)
    return value

latexCommand = do
    _ <- Parsec.char '\\'
    what <- latexSpecial Parsec.<|>  latexNormal
    return what

latexSpecial = do
    what <- Parsec.oneOf "\"'^"
    return $ case what of
        '^' -> "è"

latexNormal = do
    what <- idParser
    return $ case what of
        "S" -> "§"



stringConstant :: Parsec.Parsec String ParserState String
stringConstant = do
    id <- idParser
    s <- Parsec.getState
    let value = Map.lookup id s
    case value of
        Just s -> do
            rest <- Parsec.option "" $ Parsec.char '#' >> Parsec.spaces >> bibtexString False
            return $ s ++ rest
        _ -> fail $ "String constant " ++ id ++ " undefined"

fieldParser :: Parsec.Parsec String ParserState Field
fieldParser = do
    name <- idParser
    Parsec.spaces >> Parsec.char '=' >> Parsec.spaces
    content <- curlyString False Parsec.<|> quotedString Parsec.<|> Parsec.many1 Parsec.digit Parsec.<|> stringConstant
    Parsec.spaces
    return $ Field name content

separator = Parsec.char ',' >> Parsec.spaces

idParser = do
    firstChar <- Parsec.letter
    rest <- Parsec.many (Parsec.noneOf [' ', '{', '}', '"', '=', ',', '@'])
    Parsec.spaces
    return $ firstChar : rest

entryParser :: Parsec.Parsec String ParserState  (String, String, [Field])
entryParser = do
    _ <- skipTillEntry
    entryType <- entryTypeParser
    _ <- Parsec.char '{' >> Parsec.spaces
    (name, fields) <- if entryType == "String" then stringEntryParser else normalEntryParser
    return (entryType, name, fields)

normalEntryParser :: Parsec.Parsec String ParserState (String, [Field])
normalEntryParser = do
    name <- idParser
    separator
    fields <- Parsec.sepEndBy fieldParser separator
    _ <- Parsec.char '}'
    Parsec.spaces
    return (name, fields)

stringEntryParser :: Parsec.Parsec String ParserState (String, [Field])
stringEntryParser = do
    field <- fieldParser
    _ <- Parsec.char '}'
    Parsec.spaces
    s <- Parsec.getState
    let s' = Map.insert (name field) (value field) s
    Parsec.setState s'
    return ("", [])




-------------------------------

data BibType = Book | Article | Incollection | Set | Collection | Manual | Online | Patent | Periodical | Report | Thesis
    deriving (Show, Eq, Enum, Bounded, Read)

data BibPerson = BibPerson { fname :: String, lname :: String }
data BibPersonList = BibPersonList [BibPerson] Bool
data BibRange = BibRange (Int, Int)
data BibRangeList = BibRangeList [BibRange] Bool

--data Field t = Field { name:: String, value :: t}



data StandardField t = Author BibPersonList |
                             Editor BibPersonList |
                             Publisher String |
                             Year Int |
                             Pages BibRangeList



data BibEntry = BibEntry { kind :: BibType, id_ :: String, fields :: [Field] }
    deriving (Show)



toBibTeX :: (String, String, [Field]) -> Maybe BibEntry
toBibTeX s1 =
    let (stype, sname, f) = s1
    in do
        entryType <- readMaybe (camelCase stype) :: Maybe BibType
        return BibEntry{kind = entryType, id_ = sname, fields = f }

parseFile = parse (Parsec.sepEndBy entryParser (Parsec.many $ Parsec.noneOf ['@'])) Map.empty

parseEntry = parse entryParser Map.empty