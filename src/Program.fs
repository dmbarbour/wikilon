
open System
open System.Security.Cryptography

let helpMsg = """
Wikilon is a wiki-inspired development environment for Awelon. This program
starts a local web server, which is mostly configured online. Arguments:

    [-h] print this help message
    [-p Port] bind specified port
    [-db Dir] where to store data
    [-admin] print admin password

For initial configuration you'll need the -admin option, but you may configure
normal user accounts with administrative authorities. The password provided is
volatile, good until process reset.
"""

type Args = {
  help : bool;
  port : int;
  dbdir : string;
  admin : bool;
  bad : string list;
}

let defaultArgs : Args = {
  help = false;
  port = 3000;
  dbdir = "wiki";
  admin = false;
  bad = [];
}

let (|Int|_|) (s : string) = 
    let mutable ival = 0
    if System.Int32.TryParse(s, &ival) 
        then Some ival 
        else None

let rec procArgs xs (a : Args) : Args = 
    match xs with
    | []   -> {a with bad = List.rev a.bad }
    | "-h"::xs' -> procArgs xs' { a with help = true }
    | "-p"::(Int p)::xs' when (p >= 0) -> procArgs xs' { a with port = p }
    | "-db"::dir::xs' -> procArgs xs' { a with dbdir = dir }
    | "-admin"::xs' -> procArgs xs' { a with admin = true }
    | x::xs' -> procArgs xs' {a with bad = x :: a.bad }

let getEntropy (n : int) : byte[] = 
    let mem = Array.zeroCreate n
    do RandomNumberGenerator.Create().GetBytes(mem)
    mem


[<EntryPoint>]
let main argv =
    let args : Args = procArgs (List.ofArray argv) defaultArgs
    if args.help then printfn "%s" helpMsg; 0 else 
    let bad = not (List.isEmpty args.bad)
    if bad then printfn "Unrecognized args (try -h): %A" args.bad; (-1) else
    printfn "TODO: start server on port %i, db %s" (args.port) (args.dbdir)
    0 // return an integer exit code



