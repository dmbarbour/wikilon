namespace Awelon
open Data.ByteString
open System.Collections.Generic

// Intepretation of Awelon.
//
// Awelon programs logically rewrite to equivalent Awelon programs.
// But we can treat Awelon similar to a stack language for simple
// interpretation, such that our 'input' and 'output' is a stack.
// In this case, we would evaluate starting with an empty stack.
//
// Besides stack and program, we also assume an environment that
// supports memoization, stowage, trace outputs, parallelism, and
// similar annotation-guided features.
//
// For performance, acceleration is essential. To simplify our

In this context,
// we'll have programs that act like big-step operations 
//
// 
// Providing "input" to a program is represented in terms of 
// constructing a larger program, composing a program providing
// input data with a program that requires input data. Programs
// should not have any external state, modulo cached computations
// (memoization) and stowage resources (virtual memory).
//
// Lazy linking is a potential challenge. To efficiently perform 
// lazy linking, we need to know about input arity and available
// data. Further, we should evaluate any word at most once, so we
// require an environment to memoize evaluated results. 
//
// For performance, an interpreter should support acceleration and
// avoid 
//
// In most contexts, we'll also want quotas for our environment,
// limiting how much work we perform. We can halt early, without
// fully evaluating our program. 
module Interpret =


    type Program =

    /// 
    type 




    /// A fully qualified word, e.g. `d/foo`, or annotations or other
    /// elements we might wish to memoize.
    type Symbol = ByteString
    type Prog = Parser.Prog
    type Env =
        { src  : Dict
          memo : Map<Symbol,Prog>
          vmem : Stowage
        }
    type EP = (struct(Env * Prog))

    let interpret (struct(env,prog) : EP) : EP =
        




    /// Our initial interpretation environment. I'll need to abstract
    /// this eventually, or at least add features gradually.
    type Env =
        val Src  : Dict
        val private Mem : Dictionary<Symbol,Lazy<Prog>>
        val Stow : Stowage 
        new(src:Src, db:Stowage) =
            let mem = new Dictionary<Symbol,Prog>()
            { Src = src; Stow = db; Mem = mem }

        /// We can update the source bound to the environment. This may
        /// precisely invalidate definitions based on differences.  
        member env.Update (src:Src) : unit = lock (env.Mutex) (fun () ->
            env.Mem.Clear() // TODO: precise invalidation
            env.Src <- src)

        /// Forget all the computations.
        member env.Reset() : unit = lock (env.Mutex) (fun () -> 
            env.Mem.Clear())



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



    
    

