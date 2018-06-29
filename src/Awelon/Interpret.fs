namespace Awelon
open Data.ByteString

// This interpreter module provides a simplistic evaluation for Awelon
// code. It isn't strictly an "interpreter" - there may be optimization
// and compilation to a small degree. A main goal is to keep it simple.
// For example, we'll make relatively little effort to integrate with
// durable cache results, or support reactive update.
//
// Note: The interpreter does not perform 'execution' of an application
// model. Another layer would be required for that role!
module Interpret =

    /// Src is a generic, key-value database for source code. The 
    /// argument is a fully qualified word (e.g. `foo/bar`). The
    /// result should be its unprocessed definition, if one exists.
    /// Src is assumed to be pure, i.e. definitions are constant.
    type Src = ByteString -> ByteString option

    /// Normally, we'll use a Dict as our code source.
    let dictSrc (d:Dict) (w:ByteString) =
        match Dict.tryFind w d with
        | Some def -> Some (def.Data)
        | None -> None

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



    
    

