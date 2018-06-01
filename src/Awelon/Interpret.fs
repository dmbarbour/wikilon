namespace Awelon
open Data.ByteString

// This `Awelon.Interpret` module aims to provide a simple interpreter
// or pseudo-compiler with not-too-terrible performance. This likely 
// won't scale well, but support from accelerators should help.
// 
// Ultimately, I'd like to experiment with performance via rewriting
// to intermediate language layers, perhaps an STG machine.
module Interpreter =
    type Word = Parser.Word

    /// Trivial Dict model used by the Interpreter.
    type Dict = Word -> ByteString option
    
    

