namespace Wikilon
open Stowage
open Data.ByteString
open Suave

// The main goal right now is to get something useful running ASAP.

module WS =

    type Params =
        { db    : Stowage.DB
          admin : ByteString 
          // might add logging, etc.
        }



    let mkApp (p:Params) = 
        Successful.OK ("Hello World")


