{-# LANGUAGE GADTs #-}
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

data StandardField t where
    Author :: StandarField BibPersonList
    Editor :: StandarField BibPersonList
    Publisher :: StandarField String
    Year :: StandarField Int
    Pages :: StandarField BibRangeList
    Generic :: StandarField String

data BibEntry = BibEntry { kind :: BibType, id_ :: String, fields :: [Field] }
    deriving (Show)

toBibTeX :: (String, String, [Field]) -> Maybe BibEntry
toBibTeX s1 =
    let (stype, sname, f) = s1
    in do
        entryType <- readMaybe (camelCase stype) :: Maybe BibType
        return BibEntry{kind = entryType, id_ = sname, fields = f }