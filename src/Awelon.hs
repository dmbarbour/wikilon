{-# LANGUAGE NoImplicitPrelude #-}
-- | Awelon Language
--
-- Awelon language has a very simple syntax and semantics. The syntax
-- is essentially a stream of words (with special forms for texts and
-- annotations) and first-class subprograms. The semantics is based on
-- rewriting with only four primitive combinators.
--
-- The syntax is intended for use with projectional editing. Purely
-- textual projections might introduce decimal numbers, lists, DSLs,
-- local variables and lambdas and let expressions, and so on. But 
-- we can feasibly build graphical programming environments above the
-- simple syntax.
--
-- The semantics is based on confluent rewriting, i.e. it's purely
-- functional and deterministic but may also work with incomplete
-- programs, and any subprogram may be evaluated. This works with 
-- the syntactic projections, too, since the output may be subject
-- to the same view models as the input.
--
-- Awelon has only four primitive rewrite rules, one primitive data
-- element (the block of code):
--
--     [B][A]a == A[B]              (apply)
--     [B][A]b == [[B]A]            (bind)
--        [A]c == [A][A]            (copy)
--        [A]d ==                   (drop)
--
-- While these semantics are simple, they're far from bare metal and
-- some extra work is required to achieve performance. Arithmetic and
-- number models must be recognized by the runtime and accelerated to
-- perform efficient number crunching. Linear algebra too, ideally.
--
-- So the job of a runtime or compiler becomes software acceleration
-- for a variety of widely useful models, in addition to more basic
-- optimizations.
--
-- Awelon is complicated further by annotations, parenthetical words
-- that extend the runtime but do not affect formal program semantics.
-- These extensions may affect performance (e.g. adding parallelism,
-- memoizing computations, optimizing representations) or cause an
-- invalid program to fail faist (e.g. simple type assertions).
--
-- Wikilon aims to support Awelon with reasonable performance.
-- 
module Awelon
    ( module Awelon.Syntax
    , module Awelon.Hash
    , module Awelon.Dict
    , module Awelon.Dict.Format
    ) where

import Awelon.Syntax
import Awelon.Hash
import Awelon.Dict
import Awelon.Dict.Format


