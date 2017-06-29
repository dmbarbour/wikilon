module Wikilon.Main

open System
open System.IO
open System.Security.Cryptography
open Stowage
open Suave

let helpMsg = """
Wikilon is a wiki-inspired development environment for Awelon. This program
starts a local web server, which is mostly configured online. Arguments:

    [-h]      print this help message
    [-p Port] bind specified port (default 3000)
    [-ip IP]  bind IP or DNS (default 127.0.0.1)
    [-db Dir] where to store data (default wiki)
    [-admin]  print temporary admin password

For initial configuration you'll need the -admin option, but you may configure
normal user accounts with administrative authorities. The password provided is
volatile, good until process reset.
"""

// TODO: enable configuration of multiple bindings via URL
//   e.g. "https://example.com:23" and "http://example.org:80"
// Also, enable configuration of TLS.

type Args = {
  help : bool;
  port : int;
  ip : string;
  home : string;
  admin : bool;
  bad : string list;
}

let defaultArgs : Args = {
  help = false;
  port = 3000;
  ip = "127.0.0.1";
  home = "wiki";
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
    | "-p"::(Int p)::xs' when (p > 0) -> procArgs xs' { a with port = p }
    | "-ip"::ip::xs' -> procArgs xs' { a with ip = ip }
    | "-db"::dir::xs' -> procArgs xs' { a with home = dir }
    | "-admin"::xs' -> procArgs xs' { a with admin = true }
    | x::xs' -> procArgs xs' {a with bad = x :: a.bad }

let getEntropy (n : int) : byte[] = 
    let mem = Array.zeroCreate n
    do RandomNumberGenerator.Create().GetBytes(mem)
    mem

let setAppWorkingDir fp =
    do Directory.CreateDirectory(fp) |> ignore
    Directory.SetCurrentDirectory(fp)

[<EntryPoint>]
let main argv =
    let args : Args = procArgs (List.ofArray argv) defaultArgs
    if args.help then printfn "%s" helpMsg; 0 else 
    let bad = not (List.isEmpty args.bad)
    if bad then printfn "Unrecognized args (try -h): %A" args.bad; (-1) else
    do setAppWorkingDir args.home
    let svc = { defaultConfig with 
                 hideHeader = true
                 bindings = [ HttpBinding.createSimple HTTP args.ip args.port
                            ]
               }
    let admin = if not args.admin then None else
                let pw = Hash.alphabet // portion of hash (getEntropy 64) 
                printfn "admin %s" pw  // TODO! should be randomized!
                Some pw

    let db = "db" // todo: prepare stowage database
    let app = WS.mkApp db admin
    do startWebServer svc app
    0 // return an integer exit code



