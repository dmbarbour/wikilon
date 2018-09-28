namespace Awelon
open Data.ByteString
open System.Collections.Generic

// Intepretation of Awelon.
//
// Awelon programs logically rewrite to equivalent Awelon programs.
// But Awelon can be naively implemented as a stack language, we 
// only need some care to preserve or recover words that should not
// link/rewrite at evaluation time. It seems that for every word we
// might link, we should compute static link conditions.
//
// For performance, acceleration is essential. At this time, use of
// acceleration is simplified to `[F](accel-nat-add) => [!nat-add]`.
// That is, we don't need to validate that `F` correctly implements
// the accelerated nat-add operation at the interpreter layer. We can
// and should validate it at other layers, such as a linter or type
// checker. In any case, this makes it easy to treat accelerators as
// built-in functions for fast interpretation.
// 
// Besides accelerators, other important concerns are memoization,
// caching, stowage, parallelism, and debug trace outputs. Interpreter
// requires a full environment model, with some implicit sharing under
// the hood. Further, we might also need to track *quotas*, to limit
// evaluation efforts.
module Interpret =

    /// Linker Conditions
    ///
    /// A word will link into a computation under two conditions:
    /// 
    /// - Arity: There are sufficient arguments on the stack to link.
    /// - Avail: A word's evaluated definition has available outputs. 
    ///
    /// Arity can range 0..9, due to the (a2)..(a9) annotations. Arity
    /// zero implies a word is not awaiting further input. Avail has
    /// no upper limit, but there's simply no need to calculate it above
    /// the maximum arity 9.
    ///
    /// A "noun" will have arity=0, avail=1. A noun may simply be treated
    /// as a value for binding, etc..
    type LinkArity = int    // how many inputs this observes on data stack
    type LinkAvail = int    // how many outputs are immediately available

    /// 
    ///
    /// 



    // Stowage References

    type Env =
        val Src : Dict

    /// Our Interpreter will process programs as follows:
    /// 
    /// - Nouns and Verbs are distinct.
    /// - Evaluated data is distinct from un-evaluated.
    /// - 


    /// Interpreter-layer values should be optimized towards runtime
    /// interpretations. Relevantl
    /// 
    


    /// I assume `[code](accel)` will be used within source to 
    /// indicate accelerated implementations. Wh



    
    

