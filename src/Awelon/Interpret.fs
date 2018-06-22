namespace Awelon
open Data.ByteString

// This interpreter module provides a simplistic evaluation for Awelon
// code. It isn't strictly an "interpreter" - there may be optimization
// and compilation to a small degree. A main goal is to keep it simple.
// For example, we'll make relatively little effort to integrate with
// durable cache results, or support reactive update.
module Interpret =

    type Word = Parser.Word
    type Defs = Word → Parser.Program option
    
    
    

