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

grave = chr 0x300
acute = chr 0x301
circumflex = chr 0x302
tilde = chr 0x303
macron = chr 0x304
overline = chr 0x305
breve = chr 0x306
dotabove = chr 0x307
diaeresis = chr 0x308
hook = chr 0x309
ring = chr 0x30a
aacute = chr 0x30b
caron = chr 0x30c
vlineabove = chr 0x30d
vvlineabove = chr 0x30e
ggrave = chr 0x30f
cedilla = chr 0x327
ogonek = chr 0x328
barred = chr 0x337
barunder = chr 0x331 -- macron below
dotover = chr 0x307
dotunder = chr 0x323
endash = chr 0x2013
emdash = chr 0x2014
nonbreak = chr 0x00a0


latexSpecial :: Parsec.Parsec String ParserState String
latexSpecial = do
    accentSel <- Parsec.oneOf $ concat (Map.keys latexSpecials ++ Map.keys latexAccents)
    what <- Parsec.letter Parsec.<|> Parsec.between (Parsec.char '{') (Parsec.char '}') Parsec.letter
    let accentMod = Map.lookup [accentSel] latexSpecials
    return $ case accentMod of
        Just " " -> " "
        Just accentModUTF -> what : accentModUTF
        _ -> ['\\', accentSel] ++ [what]

latexNormal :: Parsec.Parsec String ParserState String
latexNormal = do
    what <- Parsec.many1 Parsec.letter
    let res = Map.lookup what latexNormals
    return $ case res of
        Just s -> s
        _ -> '\\' : what



latexSpecials = Map.fromList [
    (" ", " "),
    ("~", "~"),
    ("%", "%"),
    ("{", "{"),
    ("$", "$"),
    ("_", "_"),
    ("P", "¶"),
    ("#", "#"),
    ("&", "&"),
    ("}", "}")
    --("{}", "")
    --("[^][{][}]", "^"),
    --("[~][{][}]", "~")
    ]

latexAccents = Map.fromList [
    ("^", [circumflex]),
    ("`", [grave]),
    ("'", [acute]),
    ("\"", [diaeresis]),
    ("~", [tilde]),
    ("=", [macron]),
    (".", [dotover])
    ]

latexNormals = Map.fromList [
    ("H", [aacute]),
    ("c", [cedilla]),
    ("k", [ogonek]),
    ("l", [barred]),
    ("b", [barunder]),
    ("d", [dotunder]),
    ("r", [ring]),
    ("u", [breve]),
    ("v", [caron]),
    ("S", "§"),
    ("aa", "å"),
    ("AA", "Å"),
    ("ae", "æ"),
    ("AE", "Æ"),
    ("oe", "œ"),
    ("OE", "Œ"),
    ("o", "ø"),
    ("O", "Ø"),
    ("ss", "ß"),
    ("l", "ł"),
    ("L", "Ł"),
    ("dh", "ð"),
    ("DH", "Ð"),
    ("dj", "đ"),
    ("DJ", "Ð"),
    ("ng", "ŋ"),
    ("NG", "Ŋ"),
    ("dag", "†"),
    ("ddag", "‡"),
    ("pounds", "£"),
    ("textasciicircum", "^"),
    ("textasciitilde", "~"),
    ("textasteriskcentered", "∗"),
    ("textbackslash", "\\"),
    ("textbar", "|"),
    ("textbraceleft", "{"),
    ("textbraceright", "}"),
    ("textbullet", "•"),
    ("textcopyright", "©"),
    ("textdagger", "†"),
    ("textdaggerdbl", "‡"),
    ("textdollar", "$"),
    ("textellipsis", "…"),
    ("textemdash", [emdash]),
    ("textendash", [endash]),
    ("textexclamdown", "¡"),
    ("textgreater", ">"),
    ("textless", "<"),
    ("textordfeminine", "ª"),
    ("textordmasculine", "º"),
    ("textparagraph", "¶"),
    ("textperiodcentered", "·"),
    ("textquestiondown", "¿"),
    ("textquotedblleft", "“"),
    ("textquotedblright", "”"),
    ("textquoteleft", "‘"),
    ("textquoteright", "’"),
    ("textregistered", "®"),
    ("textsection", "§"),
    ("textsterling", "£"),
    ("texttrademark", "™"),
    ("textunderscore", "_"),
    ("textvisiblespace", "␣")
    ]




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