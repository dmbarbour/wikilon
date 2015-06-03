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
    H.h1 "Command Line for Awelon"
    H.p $ H.b "Internet Media Type:" <> " text/vnd.org.awelon.claw"
    H.p $ H.b "Filename Extensions:" <> " .claw"
    let lnAO = href uriAODocs $ "Awelon Object (AO)"
    H.p $ "Command Line for Awelon, or Claw, is a very thin wrapper for " <> lnAO <> ".\n\
          \Claw is intended for use with REPLs and command line shells, i.e. keyboard\n\
          \input at sizes around one line. Claw is not optimized for readability, but\n\
          \will generally be more readable in plain text than AO code."

    H.h2 "Claw Syntax"
    H.p $ "The assumption made for Claw is that the vast majority of user inputs will\n\
          \consist of words, numbers, short texts, and blocks. For other ABC content,\n\
          \escapes are necessary. Elements of Claw:"
    H.ul $ do
        H.li $ (H.b "words") <> " are represented directly: inc mul"
        H.li $ (H.b "numbers") <> " are represented conventionally: 3.141 2/3 -7"
        H.li $ (H.b "short texts") <> " are quoted inline: \"foo\" \"Hello, World!\""
        H.li $ (H.b "blocks") <> " contain more Claw code: [2 mul] 53 repeat"
        H.li $ (H.b "bytecode") <> " escaped form \\vrwlc"
        H.li $ (H.b "tokens") <> " escaped form \\{token content}"
        H.li $ (H.b "full texts") <> " escaped form \\\"ABC text goes here...\\n~"
    H.p $ "The three escape forms cannot be mixed. I.e. the bytecode is for primitive\n\
          \bytecodes only (no tokens, blocks, or texts). ABC blocks cannot be escaped.\n\
          \Full texts are necessary for double quotes or linefeeds. Elements must be\n\
          \separated by spaces or linefeeds (excepting inner edge of a block)."
    H.p $ "Claw reserves characters (|){}<> for future extensions."

    H.h2 "Claw Semantics"
    H.p $ "Claw is a very thin layer above AO. To compile Claw to AO, we translate\n\
          \every word to {%word}, convert values almost directly, and inject the\n\
          \three escaped forms. However, the value conversion requires attention:\n\
          \rather than converting directly, e.g. from `[2/3]` to `[#2#3/*]`, Claw\n\
          \injects an implicit \\l, i.e. from `[2/3]` to `[#2#3/*l]l`."
    H.p $ "The idea is that Claw will generally operate on a (stack*env) pair, where\n\
          \we want the environment to hold a stable location so we can access it no\n\
          \matter how much we manipulate the stack. The extra \\l shifts the 7 so it\n\
          \adds to the stack. A good Claw dictionary should contain useful words that\n\
          \operate on the stack."
    H.p $ "Claw will perform a basic simplification: use of \\r after any value will\n\
          \cancel with the implicit \\l. Guaranteeing this simplification ensures Claw\n\
          \precisely encodes anything AO encodes."

    H.h2 "Claw Practice"
    let lnForth = href "http://en.wikipedia.org/wiki/Forth_(programming_language)" "Forth"
    H.p $ "Claw offers a very " <> lnForth <> "-like programming experience. Like Forth,\n\
          \Claw becomes unreadable somewhere between ten and twenty elements. Careful\n\
          \factoring is essential as much for comprehension and accessibility as for\n\
          \reuse. Words are a primary basis for documentation."
    H.p $ "Unlike Forth, Awelon project is intended for use with very large dictionaries.\n\
          \This means we'll be using larger words to disambiguate in context. Claw does\n\
          \not provide namespaces to mitigate this, but an interactive REPL or shell may\n\
          \be able to simulate namespaces by hiding common suffixes or prefixes from view.\n\
          \Use of color or icons to indicate hidden components is an interesting possibility."
    H.p $ "I would like to also use Claw for iPython-notebook inspired programming."

    H.h2 "Effectful Claw Environments"
    H.p $ "A useful Command Line Shell or REPL must be able to interact with the\n\
          \outside world, e.g. load a web page, query a database, write a file."
    H.p $ "While AO code and hence Claw code is purely functional, it is always\n\
          \possible to model effects in terms of manipulating values. As a simple\n\
          \example, if our environment contained an outbox and inbox, we could\n\
          \model sending messages by writing a function that adds values to the\n\
          \outbox, and between commands we could receive messages via the inbox."
    H.p $ "While an inbox is illustrative, it is insufficient for multi-step tasks\n\
          \that should proceed without human intervention."
    let lnAVM = href uriAVMDocs "AVM"
    H.p $ "Claw REPLs or Shells shall instead contain an abstract virtual machine,\n\
          \an " <> lnAVM <> ". The (stack*env) environment from earlier expands to\n\
          \(stack*(AVM*ext)). The AVM receives messages, and after each user action\n\
          \it is signaled such that any user-manipulated outbox may be sent. An AVM\n\
          \consists of a (state*(behavior*signal)) triple. The AVM's state is the\n\
          \only shared state; the stack and extended environment belong to the user."
    H.p $ "The main constraint is that all effects are asynchronous. This can make\n\
          \it difficult to, for example, grab a file and directly use the result.\n\
          \DSLs or monadic code could model synchronous behaviors above asynchronous\n\
          \effects. If necessary, and assuming we can still keep Claw very thin, Claw\n\
          \may later be extended with syntactic sugar to simplify writing this code."
    H.p $ "Development of a good 'default' AVM is left to our dictionaries. I.e.\n\
          \we should be able to construct fresh Claw shells from a single word."

