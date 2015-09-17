{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module BibLaTeX where

import Data.Data (Data, readConstr)
import Data.Typeable

import Data.Maybe
import Text.Read

--------------------
-- Fields

data BibPerson = BibPerson { fname :: String, lname :: String }
data BibPersonList = BibPersonList [BibPerson] Bool
data BibRange = BibRange (Int, Int)
data BibRangeList = BibRangeList [BibRange] Bool

type PersonListField = Field' BibPersonList
type StringField = Field' String
type IntField = Field' Int
type RangeField = Field' BibRange

class Fieldable t

instance Fieldable String
instance Fieldable Int

data Field' t where
    Author  :: BibPerson -> PersonListField
    Authors  :: BibPersonList -> PersonListField
    Editor  :: BibPersonList -> PersonListField
    Publisher :: String -> StringField
    Year :: Int -> IntField
    Pages  :: BibRange -> RangeField
    Generic :: String -> String -> StringField
--} deriving (Data, Typeable)

data Field = forall t. Field (Field' t)
type Fields = [Field]

class FieldLike t

instance FieldLike (Field' Int)
instance FieldLike (Field' String)


unpack :: Field' a -> a
unpack a = case a of
    Author e -> BibPersonList [e] False
    Authors e -> e

--mkField = readConstr Field

--fValue :: Field t -> t
--fValue Author a = a
--fValue Editor a = a

f1 = Author $ BibPerson "Massimo" "Redaelli"
f12 = Authors $ BibPersonList [BibPerson "Massimo" "Redaelli"] True



--------------------------------
-- Entries

data BibType = Book | Article | Incollection | Set | Collection | Manual | Online | Patent | Periodical | Report | Thesis
    deriving (Show, Eq, Enum, Bounded, Read)


data BibEntry = BibEntry { kind :: BibType, id_ :: String, fields :: Fields }

--tttt = BibEntry { kind = Book, id_ = "ggg", fields = Fields [f1, f12]}

{-
toBibTeX :: (String, String, [Field]) -> Maybe BibEntry
toBibTeX s1 =
    let (stype, sname, f) = s1
    in do
        entryType <- readMaybe (camelCase stype) :: Maybe BibType
        return BibEntry{kind = entryType, id_ = sname, fields = f }
-}