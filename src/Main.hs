module Main where

import BibLaTeXParser (parseFile, parseEntry, bibtexString)

import Text.Parsec (runParser)

import Data.Map (Map)
import qualified Data.Map as Map

import System.IO

import Data.Text.ICU as ICU (compare, fromString, fromText, normalize, isNormalized, NormalizationMode( NFD ) )
import Data.Text (pack, unpack)


main = do
   hSetEncoding stdout utf8
   let result = runParser (bibtexString False) Map.empty  "" "This is an \\important example of the many text commands of LaTeX. \
    \In Italian we can use for instance \\'e, as in \\textquotedblleft perch\\'e\\textquotedblright\\ ,\
    \\\`{e}, \\'o, \\`o, \\`a, \\`\\i and \\`u, but not much more than that \\textendash\\ although you could \
    \go a bit archaic, and use also \\^{\\i} for some endings: not a great \\% of Italians do, mind you. In German they have all their crazy \\\"o- \
    \and \\\"{a}-, \\\"u's, and even good old \\ss. For instance S\\\"u\\ss." {-
   s <- readFile "biblatext-test.bib"
   let result = parseFile s-- "@string{ciao = {ciao}} @Article{key03, author= ciao, title=\"A d title\",       }  @article{rt, ar={k{\"}r}, pr=\"f{g}g{f{{}ff{}}}d\",}Hello @article{test, year=2015, pippo=\"ciao\"} "
   --let result = parseFile "  @Article{key03, author={c}, title=\"A {bunch {of} braces {in}} title\"    } "-}
   case result of
       Right v -> do
            --let res = map toBibTeX v
            --let res = (isNormalized NFD . pack) v
            putStrLn  v
       Left err -> putStrLn ("whoops, error: "++show err)
