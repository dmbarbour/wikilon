{-# LANGUAGE OverloadedStrings #-}

-- | 'Command Line for Awelon' or Claw is a simplified language aimed
-- at REPLs and similar experiences. Claw optimizes input of words and
-- numbers and short texts, at the expense of inputting raw ABC, other
-- tokens, and large texts.
module Wikilon.WAI.Pages.ClawCode
    ( clawDocs
    ) where

import Control.Monad
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
    H.h1 "Command Language (or Line) for Awelon (claw)"
    H.p $ H.b "Internet Media Type:" <> " text/vnd.org.awelon.claw"
    H.p $ "Claw is intended for use with REPLs and command shells via keyboard. Claw\n\
          \offers a very Forth-like user experience, i.e. stream numbers and words to\n\
          \manipulate stacks and models in a command environment.\n\
          \"
    H.h2 "Claw Syntax"
    H.p $ "Primarily, claw code focuses on words, numbers, and small texts. In a normal\n\
          \first-order command line scenario, this should be all you need. Claw further\n\
          \includes blocks for higher order programming, a simple namespace model for\n\
          \concision, and escaped forms for completeness.\n\
          \"
    H.ul $ do
        H.li $ (H.b "words") <> " represented directly `inc mul`"
        H.li $ (H.b "numbers") <> " include `3.141 2/3 -7 6.02e23`"
        H.li $ (H.b "short texts") <> " quoted inline: \"foo\", \"Hello, World!\""
        H.li $ (H.b "blocks") <> " contain claw code, e.g. [2 mul] 53 repeat"
        H.li $ (H.b "namespace") <> " annotations have form `#foo:`"
        H.li $ (H.b "escaped bytecode") <> " has form \\vrwlc"
        H.li $ (H.b "escaped tokens") <> " have form \\{token content}"
        H.li $ (H.b "escaped texts") <> " have form \\\"ABC text goes here...\\n~"
        H.li $ (H.b "escaped blocks") <> " have form \\[claw code]"
    H.p $ "Multi-line texts or texts containing double quote must be escaped."

    H.h2 "Claw Semantics"
    let lnAO = href uriAODocs $ "Awelon Object (AO)"
    H.p $ "Semantically, claw code is an " <> (H.em "editable view") <> " of " <> lnAO <> "\n\
          \code. Claw's entire semantics is a trivial expansion into AO. Examples:"
    H.table ! A.style "border: thin" $ do
        H.tr $ (H.th "Code") <> (H.th "Expansion")
        let lSamp =
                [("2/3",        "2 3 ratio")
                ,("4/10",       "4 10 ratio")
                ,("3.141",      "3141 3 decimal")
                ,("-1.20",      "-120 2 decimal")
                ,("6.02e23",    "6.02 23 exp10")
                ,("42",         "\\#42 integer")
                ,("-7",         "\\#7- integer")
                ,("#NS",        "\\{&ns:NS}")
                ,("[inc mul]",  "\\[inc mul] block")
                ,("inc mul",    "\\{%NSinc} \\{%NSmul}")
                ,("\"foo\"",    "\\\"foo" <> H.br <> "~ literal")
                ]
        forM_ lSamp $ \ (c,x) -> (H.tr c) <> (H.tr x) 

    H.p $ "Some of these expansions require multiple steps, e.g. from `2/3` we\n\
          \expand to `2 3 ratio`, which must be further expanded. At the bottom\n\
          \level we have the escape forms, though code within an escaped block\n\
          \must further be expanded."
    H.p $ "Claw's expansion is reversible. We can start from generated AO and\n\
          \parse it back into the more concise claw program that generates it.\n\
          \This is a valuable property; see Claw Extensions below."
    H.p $ "Note that no simplification is performed at this level. While 2/5\n\
          \and 4/10 should mean the same thing, the distinct representations\n\
          \will be preserved. In part, this aims to improve reversibility."

    H.h3 "Claw Extensions"
    H.p $ "Claw should be shared and stored in the canonical AO form. Doing so:"
    H.ul $ do
        H.li "simplifies back-end processing (which only needs to know AO)"
        H.li "greatly improves portability and extensibility of claw code"
    H.p "If ratios were an extension then `2/3` would become `2 3 ratio` in\n\
        \editable views that lack this extension. If we introduce features\n\
        \for vectors or matrices or monadic do-notation, not every editor\n\
        \will need to immediately support them.\n\
        \"
    H.p "Interestingly, it may be feasible to extend claw in structured editors\n\
        \with support for rich media - graphs, tables, diagrams, canvases, colors,\n\
        \sliders, forms, and so on."

    H.h2 "Claw Namespaces"
    H.p $ "A claw stream has exactly one namespace. The namespace is set by\n\
          \a token of form `#foo:`. If not set, claw defaults to the root\n\
          \namespace `#`. If set within a block, the namespace applies only\n\
          \until the end of that block.\n\
          \"
    H.p $ "The namespace becomes an implicit prefix for every word. For example,\n\
          \`[#foo: inc mul]` is equivalent to `[# foo:inc foo:mul]`. Namespaces\n\
          \apply even to words from expanding numbers and other claw elements.\n\
          \Words integer, literal, block are responsible for pushing data onto\n\
          \whatever stack is appropriate for the command environment.\n\
          \"
    H.p $ "The role of namespaces is to support alternate command environments\n\
          \within a single dictionary. Without namespaces, we'd effectively be\n\
          \restricted to toplevel words (for concision) and hence would support\n\
          \only one command environment.\n\
          \"

    H.h2 "Claw Environments and Effects"

    H.p $ "Claw commands are pure `state→state` functions on the command environment.\n\
          \However, between commands we might render the updated environment. If that\n\
          \environment includes a stack, we might render the top few elements on the\n\
          \stack. If the environment includes a model of a turtle-graphics canvas, we\n\
          \could render that canvas. The simple command-render loop serves as a REPL."
    H.p $ "Effects modeled in this manner are inherently asynchronous, though APIs\n\
          \could use a model of futures and promises to regain the look and feel of\n\
          \synchronous behavior."
    H.p $ "To achieve real-world effects, e.g. to control a robot, we'll use a shared\n\
          \state metaphor. User commands are still atomic state→state functions, but\n\
          \between user commands another agent might also read or manipulate the state.\n\
          \As a simplistic example, the state could include an outbox for messages that\n\
          \the agent empties and delivers between commands."
    let avm = href uriAVMDocs "AVM" 
    H.p $ "A more sophisticated example, our environment could include an " <> avm <> "\n\
          \that receives and processes messages between user commands. If we continously\n\
          \render the environment as it updates, we can model a real user interface."
    H.p $ "Namespaces will allow exploration of many command environments."

