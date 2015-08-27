module Main where

import BibLaTeXParser (parseFile, parseEntry, parseLatex)
import BibLaTeX (toBibTeX)

import System.Exit

import Data.Char
import Data.Either
import Control.Monad
import Control.Arrow (second)

import Data.Text.ICU as ICU (compare, fromString, fromText)
import Data.Text (pack)

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


equivalences = map (second pack) [
    ("More than 50\\% of mice earns less than 1\\$ per day \\{which is totally un\\_ac\\_cep\\_ta\\_ble!\\}",
     "More than 50% of mice earns less than 1$ per day {which is totally un_ac_cep_ta_ble!}"),
    ("Let's see what happens with {unbalanced braces inside braces: \\{ }. Nothing much?",
     "Let's see what happens with unbalanced braces inside braces: { . Nothing much?"),
    ("You might want more than one space, like this: \\ \\ I wonder if I should collapse whitespace like TeX does",
     "You might want more than one space, like this:   I wonder if I should collapse whitespace like TeX does"),
    ("\\% of \\#s that are useful? Hard \\& tough to tell\\}",
     "% of #s that are useful? Hard & tough to tell}"),
    ("A pretty picture: \\\\\\#/. Almost a smilie",
     "A pretty picture: \\#/. Almost a smilie"),

     ("",
      ""),
     -- lorem text from http://generator.lorem-ipsum.info/
     -- conversion to TeX with http://w2.syronex.com/jmr/latex-symbols-converter
     ("Des fames am\\`{e}t habitasse dapid\\^{u}s sagittis vari\\^{u}s int\\`{e}g\\`{e}r interdum sapien velit eleifend aptent\\'{e} vestibulum, lectus ac ligula feugiat fusc\\'{e} fringilla ultr\\^{u}c\\'{e}as arc\\^{u} himenaeos d\\`{e}s semper tortor f\\'{e}lis pharetra, hac aenean portitors\\'{e} dui quis pulv\\^{\\i}ar habitant himenaeos litor\\'{e} risus suscipit \\`{e}st fac\\^{\\i}lisis mauris. Augue lorem aliquam sed morbi risius odio placerat morbi taciti, dui aliquet \\\"{\\i}psum sagittis congue facilisis s\\'{e}d cong\\'{e}s quisqu\\'{e}es faucibus, dapid\\^{u}s plac\\'{e}rat primis bib\\'{e}ndum null\\\"{a} amet curabitur vehicula duis n\\^{\\i}bh. Molestie p\\'{e}er mass\\`{e} pr\\'{e}tium dictumst quam nisl \\`{e}st class ut li\\c{c}l\\`{a} d\\\"{\\i}am risus, justo nam\\'{e} fusce tristiqu\\'{e} blandit massa euismod n\\`{e}tus cubli\\^{a} faucibus ultricies \\\"{\\i}psum \\\"{\\i}n, eleifend nam\\'{e} class dictumst ullamcorp\\'{e}r ante d'hac malesuada ultrices aliquet curabitur 34, 908\\mbox{\\texteuro}{} fermentum des int\\`{e}g\\`{e}r lilitoxic. Fringlilia mal\\'{e}sdum tac\\^{\\i}ti\\'{e} vari\\^{u}s condimentum s\\`{e}n\\`{e}ctus lac\\^{\\i}na sociosqu pulv\\^{\\i}ar, n\\^{u}llam n\\'{e}c nostra quisqu\\'{e}es a\\'{e}nean odio t\\`{u}rpus vulputat\\'{e} \\'{e}tiam, bibendum morbi sapien.",
      "Des fames amèt habitasse dapidûs sagittis variûs intègèr interdum sapien velit eleifend aptenté vestibulum, lectus ac ligula feugiat fuscé fringilla ultrûcéas arcû himenaeos dès semper tortor félis pharetra, hac aenean portitorsé dui quis pulvîar habitant himenaeos litoré risus suscipit èst facîlisis mauris. Augue lorem aliquam sed morbi risius odio placerat morbi taciti, dui aliquet ïpsum sagittis congue facilisis séd congés quisquées faucibus, dapidûs placérat primis bibéndum nullä amet curabitur vehicula duis nîbh. Molestie péer massè prétium dictumst quam nisl èst class ut liçlà dïam risus, justo namé fusce tristiqué blandit massa euismod nètus cubliâ faucibus ultricies ïpsum ïn, eleifend namé class dictumst ullamcorpér ante d'hac malesuada ultrices aliquet curabitur 34, 908€ fermentum des intègèr lilitoxic. Fringlilia malésdum tacîtié variûs condimentum sènèctus lacîna sociosqu pulvîar, nûllam néc nostra quisquées aénean odio tùrpus vulputaté étiam, bibendum morbi sapien."),
     ("Czech: Lo\\v{r}\\'{e}m ips\\'{u}m dol\\'{o}r sit amet, no vita\\'{e} graecis \\v{s}ed, \\r{u}t f\\r{u}git tan\\v{t}as tra\\v{c}tatos v\\'{\\i}s, aliquid d\\v{e}licatiss\\'{\\i}mi a\\v{d} s\\'{\\i}t! Ex his f\\v{e}\\'{u}giat dol\\'{o}res, novum primis \\v{e}ripuit eum \\v{t}e, possim d\\v{e}finit\\'{\\i}onem i\\v{d} nam. Facete hones\\v{t}atis e\\'{a} quo, son\\'{e}t content\\'{\\i}one\\v{s} mea e\\r{u}! Vix ad clita imperdiet referrentur.",
      "Czech: Lořém ipsúm dolór sit amet, no vitaé graecis šed, ůt fůgit tanťas tračtatos vís, aliquid dělicatissími aď sít! Ex his fěúgiat dolóres, novum primis ěripuit eum ťe, possim děfinitíonem iď nam. Facete honesťatis eá quo, sonét contentíoneš mea eů! Vix ad clita imperdiet referrentur."),
     ("Danish: Lorem ipsum dolor sit \\aa{}met, mel \\o{}mnis mucius eu, ex ius habeo facer dicant? Velit forensibus no per, reque animal nam ut. Primis utroque s\\aa{}lut\\aa{}ndi cum \\aa{}n? Cum mentitum ins\\o{}lens hendrerit ei, mea eu sumo forensibus abh\\o{}rre\\ae{}nt. Ne nam mutat n\\o{}luisse platonem, meis zril oblique ne cum.",
      "Danish: Lorem ipsum dolor sit åmet, mel ømnis mucius eu, ex ius habeo facer dicant? Velit forensibus no per, reque animal nam ut. Primis utroque sålutåndi cum ån? Cum mentitum insølens hendrerit ei, mea eu sumo forensibus abhørreænt. Ne nam mutat nøluisse platonem, meis zril oblique ne cum."),
     ("Hungarian: Lorem ipsum d\\\"{o}lor sit amet, has doctus mand\\'{a}mus \\'{a}t! Mea gr\\'{a}eci q\\'{u}aesti\\\"{o} ne! Eos \\'{e}i maz\\'{\\i}m n\\H{o}str\\'{u}m, at p\\'{o}stea persi\\'{u}s explicari \\'{u}s\\H{u}, his obliq\\H{u}e princ\\'{\\i}pes splendide \\'{u}t. Q\\'{u}i postea d\\'{o}lores expetend\\'{\\i}s \\\"{u}t, at du\\'{\\i}s \\\"{o}r\\'{a}tio labit\\\"{u}r qu\\'{\\i}, no illum ponderum nom\\'{\\i}nati se\\'{a}.",
      "Hungarian: Lorem ipsum dölor sit amet, has doctus mandámus át! Mea gráeci qúaestiö ne! Eos éi mazím nőstrúm, at póstea persiús explicari úsű, his obliqűe princípes splendide út. Qúi postea dólores expetendís üt, at duís örátio labitür quí, no illum ponderum nomínati seá."),
     ("Icelandic: Lorem ipsum dolor sit amet, volumus vol\\'{u}\\TH{}pat p\\'{e}rsecut\\'{\\i} in vim, ei congue possim m\\'{e}\\'{\\i}, ex v\\'{\\i}m alii nolu\\'{\\i}sse conc\\'{e}ptam? Cu p\\'{e}r vide p\\'{e}t\\'{e}nt\\'{\\i}\\'{u}m posid\\\"{o}ni\\'{u}m, qui admodum detr\\ae{}xit indoct\\'{u}m no? T\\'{e} \\'{e}rip\\'{u}i\\TH{} convenire vim. Er\\ae{}t lat\\'{\\i}ne v\\\"{o}c\\'{\\i}bus m\\'{e}l ad, d\\'{e}l\\'{\\i}cat\\ae{} suavit\\ae{}te ins\\TH{}ructi\\'{o}r ne p\\'{e}r, ferr\\'{\\i} n\\'{o}minav\\'{\\i} his in! Audiam an\\'{\\i}mal fac\\'{\\i}lisi \\'{e}x qui. N\\'{o} lab\\'{o}re v\\'{\\i}ven\\dh{}\\\"{o} q\\'{u}i?",
      "Icelandic: Lorem ipsum dolor sit amet, volumus volúÞpat pérsecutí in vim, ei congue possim méí, ex vím alii noluísse concéptam? Cu pér vide péténtíúm posidöniúm, qui admodum detræxit indoctúm no? Té éripúiÞ convenire vim. Eræt latíne vöcíbus mél ad, délícatæ suavitæte insÞructiór ne pér, ferrí nóminaví his in! Audiam anímal facílisi éx qui. Nó labóre vívenðö qúi?"),
     ("Italian: Lorem ipsum dol\\'{o}r s\\'{\\i}t \\'{a}met, no vix electram persequer\\'{\\i}s? Et h\\`{a}s cons\\`{u}latu concludaturque. Ad eos f\\`{a}lli aliquip, an modo deb\\`{e}t e\\`{u}ism\\`{o}d me\\'{\\i}, ut tal\\`{e} soleat m\\'{e}d\\`{\\i}ocrem pro? Ad eam dico postulant definiebas, \\`{a}t his \\'{a}dip\\`{\\i}sc\\`{\\i}ng appellant\\`{u}r. Libris inermis eum id, v\\'{\\i}x probo phaedrum erroribus \\'{a}n? \\`{E}q\\`{u}idem bl\\'{a}nd\\`{\\i}t mel ex.",
      "Italian: Lorem ipsum dolór sít ámet, no vix electram persequerís? Et hàs consùlatu concludaturque. Ad eos fàlli aliquip, an modo debèt eùismòd meí, ut talè soleat médìocrem pro? Ad eam dico postulant definiebas, àt his ádipìscìng appellantùr. Libris inermis eum id, víx probo phaedrum erroribus án? Èqùidem blándìt mel ex."),
     ("Yugoslavian: Lorem ip\\v{s}um dolor \\v{s}it amet, in ius qua\\v{s} volutpat gloriatur. Sale impedit ac\\v{c}ommodare at his, pro verear commune dissentiet an, iuvaret pertinax delicata vis ne? Vi\\v{s} ea illum equidem erroribus, pri ferri offi\\v{c}ii\\v{s} contentione\\v{s} ea, modo dolor ex vim? \\v{S}it nisl partiendo \\'{c}omprehensam ad, ipsum utroque vix ad! Eum utamur scripserit eu, porro appetere eu eam, vocent nominavi ei pri.",
      "Yugoslavian: Lorem ipšum dolor šit amet, in ius quaš volutpat gloriatur. Sale impedit acčommodare at his, pro verear commune dissentiet an, iuvaret pertinax delicata vis ne? Viš ea illum equidem erroribus, pri ferri offičiiš contentioneš ea, modo dolor ex vim? Šit nisl partiendo ćomprehensam ad, ipsum utroque vix ad! Eum utamur scripserit eu, porro appetere eu eam, vocent nominavi ei pri."),
     ("Swedish: L\\\"{o}rem ipsum dolor sit \\ae{}met, dic\\aa{}t necessitatibus sit an. Dol\\\"{o}re semper postulant te eos, nisl regi\\\"{o}ne interpret\\aa{}ris eos ei, m\\ae{}lis audire apeirian n\\\"{o} nam. Errem n\\\"{o}minavi copiosae ea cum, lorem debet mnesarchum duo ne, n\\aa{}m trit\\ae{}ni nominavi te. Pro ne pl\\aa{}cerat ponderum vulput\\ae{}te?",
      "Swedish: Lörem ipsum dolor sit æmet, dicåt necessitatibus sit an. Dolöre semper postulant te eos, nisl regiöne interpretåris eos ei, mælis audire apeirian nö nam. Errem nöminavi copiosae ea cum, lorem debet mnesarchum duo ne, nåm tritæni nominavi te. Pro ne plåcerat ponderum vulputæte?")
    ]

assertParseEquivalence str1 str2 = case parseLatex str1 of
    Left _ -> assertFailure $ "Should have parsed, but didn't: " ++ str1
    Right s -> assertBool "LaTeX string error:" $ ICU.compare [] (pack s) str2 == EQ

equivalenceTests = map (TestCase . uncurry assertParseEquivalence) equivalences

fail1 str = TestCase $ assertDoesntParse str

pass1 str = TestCase $ assertParse str

biblatexTest = TestCase $ do
    s <- readFile "biblatext-test.bib"
    assertParse s

trivialTest = TestCase $ assertBool "what?!" True

runTests = runTestTT . TestList $
    -- biblatexTest
    -- map fail1 shouldFail
    -- ++ map pass1 shouldPass
    -- ++ equivalenceTests
    [trivialTest]

main :: IO ()
main = do
    cs@(Counts _ _ errs fails) <- runTests
    putStrLn (showCounts cs)
    if errs > 0 || fails > 0
        then exitFailure
        else exitSuccess