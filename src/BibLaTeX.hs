module BibLaTeX where

import BibLaTeXParser (Field, camelCase)
import Data.Maybe
import Text.Read

data BibType = Book | Article | Incollection | Set | Collection | Manual | Online | Patent | Periodical | Report | Thesis
    deriving (Show, Eq, Enum, Bounded, Read)

data BibPerson = BibPerson { fname :: String, lname :: String }
data BibPersonList = BibPersonList [BibPerson] Bool
data BibRange = BibRange (Int, Int)
data BibRangeList = BibRangeList [BibRange] Bool

data StandardField = Author BibPersonList |
                             Editor BibPersonList |
                             Publisher String |
                             Year Int |
                             Pages BibRangeList
    deriving (Show, Eq, Read)

instance Read Year where
    readsPrec _ input = [2005, ""]

data BibEntry = BibEntry { kind :: BibType, id_ :: String, fields :: [Field] }
    deriving (Show)

toBibTeX :: (String, String, [Field]) -> Maybe BibEntry
toBibTeX s1 =
    let (stype, sname, f) = s1
    in do
        entryType <- readMaybe (camelCase stype) :: Maybe BibType
        return BibEntry{kind = entryType, id_ = sname, fields = f }