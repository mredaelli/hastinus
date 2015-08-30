module Main where

import BibLaTeXParser (parseFile, parseEntry, bibtexString)

import Text.Parsec (runParser)

import Data.Map (Map)
import qualified Data.Map as Map

import System.IO

import Data.Text.ICU as ICU (compare, fromString, fromText, normalize, isNormalized, NormalizationMode( NFD ) )
import Data.Text (pack, unpack)

import Data.List.Split


main = do
   let a1 = "Des fames am\\`{e}t habitasse dapid\\^{u}s sagittis vari\\^{u}s int\\`{e}g\\`{e}r interdum sapien velit eleifend aptent\\'{e} vestibulum, lectus ac ligula feugiat fusc\\'{e} fringilla ultr\\^{u}c\\'{e}as arc\\^{u} himenaeos d\\`{e}s semper tortor f\\'{e}lis pharetra, hac aenean portitors\\'{e} dui quis pulv\\^{\\i}ar habitant himenaeos litor\\'{e} risus suscipit \\`{e}st fac\\^{\\i}lisis mauris. Augue lorem aliquam sed morbi risius odio placerat morbi taciti, dui aliquet \\\"{\\i}psum sagittis congue facilisis s\\'{e}d cong\\'{e}s quisqu\\'{e}es faucibus, dapid\\^{u}s plac\\'{e}rat primis bib\\'{e}ndum null\\\"{a} amet curabitur vehicula duis n\\^{\\i}bh. Molestie p\\'{e}er mass\\`{e} pr\\'{e}tium dictumst quam nisl \\`{e}st class ut li\\c{c}l\\`{a} d\\\"{\\i}am risus, justo nam\\'{e} fusce tristiqu\\'{e} blandit massa euismod n\\`{e}tus cubli\\^{a} faucibus ultricies \\\"{\\i}psum \\\"{\\i}n, eleifend nam\\'{e} class dictumst ullamcorp\\'{e}r ante d'hac malesuada ultrices aliquet curabitur 34, 908 fermentum des int\\`{e}g\\`{e}r lilitoxic. Fringlilia mal\\'{e}sdum tac\\^{\\i}ti\\'{e} vari\\^{u}s condimentum s\\`{e}n\\`{e}ctus lac\\^{\\i}na sociosqu pulv\\^{\\i}ar, n\\^{u}llam n\\'{e}c nostra quisqu\\'{e}es a\\'{e}nean odio t\\`{u}rpus vulputat\\'{e} \\'{e}tiam, bibendum morbi sapien."
       a2 = "Des fames amèt habitasse dapidûs sagittis variûs intègèr interdum sapien velit eleifend aptenté vestibulum, lectus ac ligula feugiat fuscé fringilla ultrûcéas arcû himenaeos dès semper tortor félis pharetra, hac aenean portitorsé dui quis pulvîar habitant himenaeos litoré risus suscipit èst facîlisis mauris. Augue lorem aliquam sed morbi risius odio placerat morbi taciti, dui aliquet ïpsum sagittis congue facilisis séd congés quisquées faucibus, dapidûs placérat primis bibéndum nullä amet curabitur vehicula duis nîbh. Molestie péer massè prétium dictumst quam nisl èst class ut liçlà dïam risus, justo namé fusce tristiqué blandit massa euismod nètus cubliâ faucibus ultricies ïpsum ïn, eleifend namé class dictumst ullamcorpér ante d'hac malesuada ultrices aliquet curabitur 34, 908 fermentum des intègèr lilitoxic. Fringlilia malésdum tacîtié variûs condimentum sènèctus lacîna sociosqu pulvîar, nûllam néc nostra quisquées aénean odio tùrpus vulputaté étiam, bibendum morbi sapien."
   hSetEncoding stdout utf8
   let result = runParser (bibtexString False) Map.empty  "" a1
   {-let result = runParser (bibtexString False) Map.empty  "" "This is an \\important example of the many text commands of LaTeX. \
    \In Italian we can use for instance \\'e, as in \\textquotedblleft perch\\'e\\textquotedblright\\ ,\
    \\\`{e}, \\'o, \\`o, \\`a, \\`\\i and \\`u, but not much more than that \\textendash\\ although you could \
    \go a bit archaic, and use also \\^{\\i} for some endings: not a great \\% of Italians do, mind you. In German they have all their crazy \\\"o- \
    \and \\\"{a}-, \\\"u's, and even good old \\ss. For instance S\\\"u\\ss."
   s <- readFile "biblatext-test.bib"
   let result = parseFile s-- "@string{ciao = {ciao}} @Article{key03, author= ciao, title=\"A d title\",       }  @article{rt, ar={k{\"}r}, pr=\"f{g}g{f{{}ff{}}}d\",}Hello @article{test, year=2015, pippo=\"ciao\"} "
   --let result = parseFile "  @Article{key03, author={c}, title=\"A {bunch {of} braces {in}} title\"    } "-}
   case result of
       Right v -> do
            putStrLn  v
            putStrLn $ a2
            putStrLn $ show (ICU.compare [] (pack v) (pack a2))
       Left err -> putStrLn ("whoops, error: "++show err)
   s <- readFile "biblatext-test.bib"
   let ss = split (dropDelims . dropBlanks $ onSublist "\n\n") s
   putStrLn . show $ head ss
