module Main where

import BibLaTeXParser (parseFile, parseEntry)
import BibLaTeX (toBibTeX)

import System.Exit

import Data.Char
import Data.Either
import Control.Monad

import Test.HUnit


assertDoesntParse :: String -> Assertion
assertDoesntParse str = case parseFile str of
    Right _ -> assertFailure $ "Should have failed, but parsed: " ++ str
    Left _ -> return ()

assertParse :: String -> Assertion
assertParse str = case parseFile str of
    Left _ -> assertFailure $ "Should have parsed, but didn't: " ++ str
    Right _ -> return ()

shouldFail = [
    "@Article{key03, title = \"The lonely { brace\",  }",
    "@Article{key01,  author = \"Simon \\\"the saint\\\" Templar\" }"
    --, "@Article{key01, title = { The history of @ sign } }" this actually works in BibLateX
    ]

shouldPass = [
     "@Article{key03, title = \"A {bunch {of} braces {in}} title\" } ",
     "@Article{key01,  author = \"Simon {\"}the {saint\"} Templar\" }" ,
     "@Article{key01, title = \"The history of @ sign\" }"
     ]

fail1 str = TestCase $ assertDoesntParse str

pass1 str = TestCase $ assertParse str

biblatexTest = TestCase $ do
    s <- readFile "biblatext-test.bib"
    assertParse s


runTests = runTestTT . TestList $ map fail1 shouldFail ++ map pass1 shouldPass ++ [biblatexTest]

main :: IO ()
main = do
    cs@(Counts _ _ errs fails) <- runTests
    putStrLn (showCounts cs)
    if errs > 0 || fails > 0
        then exitFailure
        else exitSuccess