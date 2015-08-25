module Accentuate where

import Data.Char (chr)
import Data.List (intercalate, foldl')
import Data.List.Split (splitOn)


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

accentSubstitutions = [
    ("\\^", circumflex),
    ("`", grave),
    ("'", acute),
    ("\"", diaeresis),
    ("~", tilde),
    ("H", aacute),
    ("c", cedilla),
    ("k", ogonek),
    ("l", barred),
    ("=", macron),
    ("b", barunder),
    ("\\.", dotover),
    ("d", dotunder),
    ("r", ring),
    ("u", breve),
    ("v", caron)
    ]

-- The Regexps
regexp4acc :: String -> String
regexp4acc s = "\\\\" ++ s ++"([a-zA-Z])|\\\\" ++ s ++"[{]([a-zA-Z])[}]"

endash = chr 0x2013
emdash = chr 0x2014
nonbreak = chr 0x00a0

constSubstitutions = [
    --("[~]", "~"),
    --("[{][}]", ""), -- todo: problem with other substitutions?
    --("[%]", "%"),
    --("[{]", "{"),
    --("[$]", "$"),
    --("[_]", "_"),
    --("P", "¶"),
    --("[#]", "#"),
    --("[&]", "&"),
    --("[}]", "}"),
    --("S", "§"),
    --("aa", "å"),
    --("AA", "Å"),
    --("ae", "æ"),
    --("AE", "Æ"),
    --("oe", "œ"),
    --("OE", "Œ"),
    --("o", "ø"),
    --("O", "Ø"),
    ("ss", "ß"),
    --("l", "ł"),
    --("L", "Ł"),
    --("dh", "ð"),
    --("DH", "Ð"),
    --("dj", "đ"),
    --("DJ", "Ð"),
    --("ng", "ŋ"),
    --("NG", "Ŋ"),
    --("dag", "†"),
    --("ddag", "‡"),
    --("pounds", "£"),
    --("textasciicircum", "^"),
    --("textasciitilde", "~"),
    --("textasteriskcentered", "∗"),
    --("textbackslash", "\\"),
    --("textbar", "|"),
    --("textbraceleft", "{"),
    --("textbraceright", "}"),
    --("textbullet", "•"),
    --("textcopyright", "©"),
    --("textdagger", "†"),
    --("textdaggerdbl", "‡"),
    --("textdollar", "$"),
    --("textellipsis", "…"),
    ("textemdash", [emdash]),
    ("textendash", [endash]),
    --("textexclamdown", "¡"),
    --("textgreater", ">"),
    --("textless", "<"),
    --("textordfeminine", "ª"),
    --("textordmasculine", "º"),
    --("textparagraph", "¶"),
    --("textperiodcentered", "·"),
    --("textquestiondown", "¿"),
    --("textquotedblleft", "“"),
    ("textquotedblright", "”")
    --("textquoteleft", "‘"),
    --("textquoteright", "’"),
    --("textregistered", "®"),
    --("textsection", "§"),
    --("textsterling", "£"),
    --("texttrademark", "™"),
    --("textunderscore", "_"),
    --("textvisiblespace", "␣"),
    --("[^][{][}]", "^"),
    --("[~][{][}]", "~")
    ]
--textsuperscript, textcircled

regexp4const :: String -> String
regexp4const s = "\\\\" ++ s ++ "( |([^ a-zA-Z1-9])|$)"

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old


replaceAll :: Regex -> (String -> String) -> String -> String
replaceAll re f s = start end
  where (_, end, start) = foldl' go (0, s, id) $ getAllMatches $ match re s
        go (ind,read,write) (off,len) =
            let (skip, start) = splitAt (off - ind) read
                (matched, remaining) = splitAt len start
            in (off + len, remaining, write . (skip++) . (f matched ++))

-- Rearrange the parameters of subRegex
--subRegex' :: String -> String -> String -> String
--subRegex' reg newval orig = subRegex (mkRegex reg) orig newval

-- Rearrange the parameters of subRegex
subRegex'' :: String -> (String ->String) -> String  -> String
--subRegex'' reg newval orig = replaceAll (makeRegex reg) (const newval) orig
subRegex'' reg newval orig = replaceAll (makeRegex reg) newval orig

--latexBaseCommands = foldr (.) id (map (\(f, t) -> subRegex'' (regexp4const f) $ t ++ "\\2" ) constSubstitutions)
latexBaseCommands = foldr (.) id (map (\(f, t) -> subRegex'' (regexp4const f) $ (\l-> f++ [last l]) ) constSubstitutions)

--latexAccents = foldr (.) id (map (\(f, t) -> subRegex'' (regexp4acc f) $ "\\1\\2" ++ [t]) accentSubstitutions)
latexAccents = foldr (.) id (map (\(f, t) -> subRegex'' (regexp4acc f) $ (\l-> l++f)) accentSubstitutions)

latexFixIJ :: String -> String
latexFixIJ = subRegex'' (regexp4const "i") (\s -> 'i' : "\\1") .  subRegex'' (regexp4const "j") (\l -> 'j' : "\\1")

-- Removes LaTeX stuff from the string, as much as possible
latexToUnicode :: String -> String
latexToUnicode = replace "~" [nonbreak] -- todo: problem with ~ obtained from other commands!
--    . replace "\\ " " "
    . latexAccents
--    . latexBaseCommands
    . latexFixIJ


latexSpecial = [
    ("^", circumflex),
    ("`", grave),
    ("'", acute),
    ("\"", diaeresis),
    ("~", tilde),
    ("=", macron),
    (".", dotover),
    ("~", "~"),
    ("{}", ""),
    ("%", "%"),
    ("{", "{"),
    ("$", "$"),
    ("_", "_"),
    ("P", "¶"),
    ("#", "#"),
    ("&", "&"),
    ("}", "}"),
    ("[^][{][}]", "^"),
    ("[~][{][}]", "~")
    ]

latexNormal = [
    ("H", aacute),
    ("c", cedilla),
    ("k", ogonek),
    ("l", barred),
    ("b", barunder),
    ("d", dotunder),
    ("r", ring),
    ("u", breve),
    ("v", caron),
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
    ("textquotedblright", "”")
    ("textquoteleft", "‘"),
    ("textquoteright", "’"),
    ("textregistered", "®"),
    ("textsection", "§"),
    ("textsterling", "£"),
    ("texttrademark", "™"),
    ("textunderscore", "_"),
    ("textvisiblespace", "␣")
    ]

-- The Regexps
regexp4acc :: String -> String
regexp4acc s = "\\\\" ++ s ++"([a-zA-Z])|\\\\" ++ s ++"[{]([a-zA-Z])[}]"

endash = chr 0x2013
emdash = chr 0x2014
nonbreak = chr 0x00a0

