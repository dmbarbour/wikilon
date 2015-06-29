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
    H.body ! A.class_ "docs" ! A.style "margin: 0 auto; width: 640px" $
        clawDocsHTML

clawDocsHTML :: HTML
clawDocsHTML = do
    H.h1 "Command Language for Awelon (claw)"
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
    H.p $ "Any use of escaped forms suggests that some words should be added\n\
          \to the dictionary. Also, when a command doesn't fit a single line,\n\
          \users should refactor it into smaller commands."
    H.h2 "Claw Semantics"
    let lnAO = href uriAODocs $ "Awelon Object (AO)"
    H.p $ "Claw's entire semantics is a trivial expansion into " <> lnAO <> " bytecode."
    H.p $ "Examples:"
    let tableStyle = " text-align: left;\
                     \ border-collapse: separate;\
                     \ border-spacing: 30px 0;\
                     \ margin: 30px 30px 30px 15px;"
    H.table ! A.style tableStyle $ do
        H.tr $ do
            H.th "Code" ! A.scope "col" 
            H.th "Expansion" ! A.scope "col"
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
        forM_ lSamp $ \ (c,x) -> H.tr $ (H.td (H.code c)) <> (H.td (H.code x)) 

    H.p $ "Some expansions require multiple steps, e.g. from `2/3` we expand to\n\
          \`2 3 ratio`, which must further expand from integers and words. The\n\
          \base case is just the four escape forms, which expand more or less\n\
          \directly into bytecode (though escaped claw blocks require further\n\
          \expansion of the contained claw code)."

    H.p $ "No simplification is performed. E.g numbers `2/5` and `4/10` should\n\
          \have the same behavior, but that is left to discretion of the `ratio`\n\
          \word. The trivial expansion of claw code should preserve structure."

    H.p $ "Due to the underlying bytecode, claw code is tacit concatenative.\n\
          \Developers can understand each word or token as executing in order,\n\
          \with blocks representing first-class functions. Claw offers a very\n\
          \Forth-like experience (modulo environment and effects; see below)."

    H.h2 "Claw Extensions"
    H.p $ "The expansion process for claw code is reversible. That is, we can\n\
          \parse a higher level claw structure from the generated AO code. This\n\
          \isn't guaranteed to be exactly the same (if you wrote `4 10 ratio`\n\
          \it shall parse back into `4/10`). But this behavior will be simple,\n\
          \predictable, transparent; and the resulting view is more accessible\n\
          \compared to raw AO code."
    H.p $ "Sharing and storing code in the expanded AO format has advantages:"
    H.ul $ do
        H.li $ (H.strong "extensibility") <> ": if ratios were an extension,\
             \ then `2/3` remains accessible as `2 3 ratio` in editors or\
             \ views that lack the extension."
        H.li $ (H.strong "portability") <> ": semantics are preserved by the\
             \ AO format regardless of available extensions."
        H.li $ (H.strong "simplicity") <> ": back-end processing doesn't need\
             \ to know anything about claw syntax or extensions."
    H.p $ "Vectors, matrices, and monadic do-notation are viable extensions\n\
          \to claw code that might be explored in the near future. Structured\n\
          \editors could feasibly extend claw code with rich media: tables,\n\
          \colors and color-pickers, graphs, diagrams, canvases, sliders and\n\
          \checkboxes, etc.."

    H.h2 "Claw Environments and Effects"
    H.p $ "We can understand each command as a pure `env→env` function."
    H.p $ "A claw command is a short string of words and numbers.\n\
          \Words are given meaning based on an implicit AO dictionary. An AO\n\
          \dictionary is self-contained and every word is purely functional.\n\
          \Thus claw commands are purely functional.\n\
          \We apply each command to the environment. Hence, the basic effect\n\
          \is manipulation of state. After updates, we render the environment\n\
          \for the human user, or at least part of it. E.g. for a turtle graphics\n\
          \REPL, we might render the top few elements on a stack and a canvas\n\
          \modeled as values in the environment."
    let avm = href uriAVMDocs "AVM" 
    H.p $ "Real-world effects are integrated by leveraging shared-state concepts.\n\
          \Between atomic user commands, background agents observe or influence\n\
          \the environment value. An outbox of messages might be cleared and sent.\n\
          \A data model or tuple space might be updated. General purpose effects\n\
          \could leverage the " <> avm <> " concepts of messaging and capabilities.\n\
          \Effects modeled above shared state are inherently asynchronous. However,\n\
          \a model of promises and futures could feasibly regain the look and feel\n\
          \of synchronous APIs and commands for convenience."
    H.p $ "Ultimately, we need a stable structure for our `env` value such that our\n\
          \background agents and renderers can process it and integrate useful effects.\n\
          \The shape of `env` will depend on the features we need."

    H.h2 "Claw Namespaces"

    H.p $ "Claw introduces a simple namespace concept."

    H.ul $ do
        H.li "Every region of a claw stream has exactly one namespace."
        H.li "Set namespace via a token prefixed with hash, e.g. `#NS`."
        H.li "Namespace tokens expand into an annotations, `\\{&ns:NS}`"
        H.li "The empty string is the default namespace, e.g. token `#`."
        H.li "If set within block, namespace applies only within block."
        H.li "Namespace becomes prefix when expanding words to tokens."
        H.li "That includes words integer, ratio, etc. from expansions."

    H.p $ "The role of namespaces is to support multiple command environments\n\
          \from a single dictionary. Shared metaphors between environments may\n\
          \need to use the same word with slightly different implementations.\n\
          \Words must remain concise for efficient use by humans."

    H.p $ "Claw's namespace model has the advantages of being simple, transparent,\n\
          \unambiguous. There is no need to inspect a codebase to determine how\n\
          \words will bind. The main disadvantage is that common words, even those\n\
          \with exactly the same implementation, must be repeated (or redirected)\n\
          \for each namespace in the dictionary. Automation might mitigate this."
