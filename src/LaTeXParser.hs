module LaTeXParser (BibLaTeXParser, parse, latexCommandName, latexCommand) where

import Data.Char
import Data.List (elem, (\\), nub)

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec hiding (parse)

type ParserState = Map String String
type BibLaTeXParser = Parsec String ParserState

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

parse parser = runParser parser Map.empty ""

ijNoAccent :: BibLaTeXParser Char
ijNoAccent = do
    _ <- char '\\'
    char 'i' <|> char 'j'

latexStrangeStarts = nub (concat (Map.keys latexAccents)) ++ concat (Map.keys latexNormals) \\ (['a'..'z']++['A'..'Z'])

latexCommandName :: BibLaTeXParser String
latexCommandName =
    char '\\' >>
    (count 1 (oneOf latexStrangeStarts) <|> many1 (letter <|> digit))

latexAccents = Map.fromList [
    ("^", [circumflex]),
    ("`", [grave]),
    ("'", [acute]),
    ("\"", [diaeresis]),
    ("~", [tilde]),
    ("=", [macron]),
    (".", [dotover]),
    ("H", [aacute]),
    ("c", [cedilla]),
    ("k", [ogonek]),
    ("l", [barred]),
    ("b", [barunder]),
    ("d", [dotunder]),
    ("r", [ring]),
    ("u", [breve]),
    ("v", [caron])
    ]

latexNormals = Map.fromList [
    (" ", " "), -- -
    ("%", "%"), -- --
    ("{", "{"), -- --
    ("$", "$"), -- -
    ("_", "_"), -- -
    ("#", "#"), -- -
    ("&", "&"), -- -
    ("}", "}"), -- --
    ("\\", "\\"), -- -
    --("~", "~"),
    --("{}", "")
    --("[^][{][}]", "^"),
    --("[~][{][}]", "~"),
    ("P", "¶"),
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
    ("th", "þ"),
    ("TH", "Þ"),
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


latexCommand :: BibLaTeXParser String
latexCommand = do
    command <- latexCommandName
    case Map.lookup command latexNormals of
         Just specUTF -> return specUTF
         _ -> case Map.lookup command latexAccents of
             Just accentModUTF -> do
                 char <- (letter <|> ijNoAccent) <|> between (char '{') (char '}') (letter <|> ijNoAccent)
                 return $ char : accentModUTF
             _ -> return $ '\\' : command
