module LaTeXParser where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>), (<|>))

import Data.Char

import Data.Map (Map)
import qualified Data.Map as Map

type ParserState = Map String String

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

latexCommand :: Parsec.Parsec String ParserState String
latexCommand = do
    _ <- Parsec.char '\\'
    latexNonAlpha Parsec.<|>  latexNormal

latexNonAlpha = latexAccent Parsec.<|> latexSpecial

ijNoAccent = do
    _ <- Parsec.char '\\'
    Parsec.char 'i' Parsec.<|> Parsec.char 'j'

latexAccent = do
    accentSel <- Parsec.oneOf $ concat (Map.keys latexAccents)
    what <- (Parsec.letter Parsec.<|> ijNoAccent) Parsec.<|> Parsec.between (Parsec.char '{') (Parsec.char '}') (Parsec.letter Parsec.<|> ijNoAccent)
    let accentMod = Map.lookup [accentSel] latexAccents
    return $ case accentMod of
        Just accentModUTF -> what : accentModUTF
        _ -> ['\\', accentSel] ++ [what]

latexSpecial = do
    sel <- Parsec.oneOf " "
    case sel of
        ' ' -> return  " "

latexNormal = do
    what <- Parsec.many1 Parsec.letter
    _ <- Parsec.spaces
    let res = Map.lookup what latexNormals
    return $ case res of
        Just s -> s
        _ -> '\\' : what



latexSpecials = Map.fromList [
    (" ", " "),
--    ("~", "~"),
    ("%", "%"),
    ("{", "{"),
    ("$", "$"),
    ("_", "_"),
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
    ("P", "¶"),
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

