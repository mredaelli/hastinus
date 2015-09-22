{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module BibLaTeX where

import Data.Data (Data, readConstr, toConstr)
import Data.Typeable

import Data.Maybe
import Text.Read hiding (get)

--------------------
-- Fields

data BibPerson = BibPerson { fname :: String, lname :: String }
data BibPersonList = BibPersonList [BibPerson] Bool
data BibRange = BibRange (Int, Int)
data BibRangeList = BibRangeList [BibRange] Bool

class Field' t ft where
  value :: t -> t
  value = id

data Author = Author { value :: BibPerson } deriving Typeable
data Authors = Authors { value :: BibPersonList } deriving Typeable
   {- Editor  :: BibPersonList -> PersonListField
    Publisher :: String -> StringField
    Year :: Int -> IntField
    Pages  :: BibRange -> RangeField
    Generic :: String -> String -> StringField
-}

instance Field' Author BibPersonList
instance Field' (Authors) BibPersonList

-- data Field = (Field' t =>  forall t. Field t)
--type Fields = [Field]

--fields = fmap Field

--get :: (t -> Field' t) -> Fields -> Maybe t
--get c v = undefined



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


-- data BibEntry = BibEntry { kind :: BibType, id_ :: String, fieldList :: Fields }

-- tttt = BibEntry { kind = Book, id_ = "ggg", fieldList = fields [f1, f12]}

{-
toBibTeX :: (String, String, [Field]) -> Maybe BibEntry
toBibTeX s1 =
    let (stype, sname, f) = s1
    in do
        entryType <- readMaybe (camelCase stype) :: Maybe BibType
        return BibEntry{kind = entryType, id_ = sname, fields = f }
-}