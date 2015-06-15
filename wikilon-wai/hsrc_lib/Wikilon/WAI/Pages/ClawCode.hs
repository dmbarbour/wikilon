{-# LANGUAGE OverloadedStrings #-}

-- | 'Command Line for Awelon' or Claw is a simplified language aimed
-- at REPLs and similar experiences. Claw optimizes input of words and
-- numbers and short texts, at the expense of inputting raw ABC, other
-- tokens, and large texts.
module Wikilon.WAI.Pages.ClawCode
    ( clawDocs
    ) where

import Data.Monoid
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes

-- | a simple webpage that describes Claw code
clawDocs :: WikilonApp
clawDocs = basicWebPage $ \ w _ _ -> do
    H.head $ do
        htmlHeaderCommon w
        H.title "Command Line for Awelon"
    H.body ! A.class_ "docs" ! A.style "margin: 0 auto; width: 960px" $
        clawDocsHTML

clawDocsHTML :: HTML
clawDocsHTML = do
    H.h1 "Command Language (or Line) for Awelon"
    H.p $ H.b "Internet Media Type:" <> " text/vnd.org.awelon.claw"
    H.p $ H.b "Filename Extensions:" <> " (not applicable) "
    let lnAO = href uriAODocs $ "Awelon Object (AO)"
    H.p $ "Command Language for Awelon, or Claw, is a syntactic sugar for " <> lnAO <> ".\n\
          \Claw is intended for use with REPLs and command line shells, where efficient\n\
          \keyboard input is needed in small doses of one line or so. Under the hood, AO\n\
          \code should be used to represent Claw code, so Claw only appears in editors.\n\
          \"
    H.p $ "Claw code is not optimized for readability. But it is much more readable than\n\
          \the AO code that it desugars into.\n\
          \"
    H.h2 "Claw Syntax"
    H.p $ "Claw is very Forth-like. Claw code consists of words, numbers, inline texts,\n\
          \blocks, and escapes into Awelon Bytecode (ABC). Claw words refer to an AO\n\
          \dictionary. Most logic and data should be in the dictionary so users only\n\
          \need a few words and arguments on a command line.\n\
          \"
    H.p $ "Claw Elements:"
    H.ul $ do
        H.li $ (H.b "words") <> " are represented directly: inc mul"
        H.li $ (H.b "numbers") <> " are represented conventionally: 3.141 2/3 -7"
        H.li $ (H.b "short texts") <> " are quoted inline: \"foo\" \"Hello, World!\""
        H.li $ (H.b "blocks") <> " contain more Claw code: [2 mul] 53 repeat"
        H.li $ (H.b "bytecode") <> " escaped form \\vrwlc"
        H.li $ (H.b "tokens") <> " escaped form \\{token content}"
        H.li $ (H.b "full texts") <> " escaped form \\\"ABC text goes here...\\n~"
    H.p $ "The three escape forms cannot be mixed, i.e. such that we can easily\n\
          \know from reading a single character whether we're dealing with text,\n\
          \a token, or bytecode.\n\
          \"
    H.p $ "Claw code is generally associated with a namespace, which determines\n\
          \the implicit prefix for all words. Words outside this namespace may\n\
          \be represented explicitly using the escaped expansion `\\{%word}`. Some\n\
          \editors may support switching namespaces for regions of code, but the\n\
          \normal case is to just use one namespace, add or redirect to words\n\
          \as needed, and let the dictionary swallow any redundancy.\n\
          \"
    H.h2 "Claw Semantics"
    H.p $ "Claw expands in a simple fashion into AO code. This AO code can be\n\
          \reliably parsed back into same Claw code that produces it. Thus, the\n\
          \expansion is the semantics.\n\
          \"
    H.p $ "Expansions Of:"
    H.ul $ do
        H.li $ (H.b "words") <> " `foo` expands into {%NSfoo} for namespace NS"
        H.li $ (H.b "integral numbers") <> ": `-7` expands to `\\#7- integral`"
        H.li $ (H.b "rational numbers") <> ": `2/3` expands into `\\#2#3 rational`"
        H.li $ (H.b "decimal numbers") <> ": `3.141` expands into `\\#3141#3- decimal`" 
        H.li $ (H.b "short texts") <> " expand into long texts"
        H.li $ (H.b "block") <> " structure is preserved, Claw blocks to AO blocks"
        H.li $ (H.b "escaped bytecode, tokens, texts") <> " expand into AO code directly"
    H.p $ "Note that the words 'integral' and so on further expand with the associated\n\
          \namespace. Decimals are expanded as written, e.g. 1.00 is `\\#100#2- decimal`\n\
          \so we know that we recorded two zeroes after the decimal point.\n\
          \"
    H.p $ "Claw code might be later extended with new syntax and expansion semantics,\n\
          \e.g. to support scientific numbers, units, vectors and matrices, and so on.\n\
          \"
    H.h2 "Claw Practice"
    let lnForth = href "http://en.wikipedia.org/wiki/Forth_(programming_language)" "Forth"
    H.p $ "Claw offers a very " <> lnForth <> "-like programming experience. Like Forth,\n\
          \Claw becomes unreadable somewhere between ten and twenty tokens. Continuous\n\
          \factoring, pushing logic into the dictionary, is essential for comprehension,\n\
          \accessibility, and reuse. Naming of words is a major basis for documentation."

    H.h2 "Effectful Claw Environments"
    H.p $ "In context of a command line shell or REPL, Claw code will usually be provided\n\
          \one line or small text area at a time expressing a state→state function. Effects\n\
          \are possible based on a multi-agent concept: outside agents observe and influence\n\
          \the state between user commands. All effects are asynchronous. User commands must\n\
          \return before any effects are observed by outside agents.\n\
          \"

