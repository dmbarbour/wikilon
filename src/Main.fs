module Wikilon.Main

open System
open System.IO
open System.Security.Cryptography
open Suave
open Data.ByteString

let helpMsg = """
Wikilon is a wiki-inspired development environment for Awelon. This program
starts a local web server, which is mostly configured online. Arguments:

    [-h]      print this help message
    [-p Port] bind specified port (default 3000)
    [-ip IP]  bind IP or DNS (default 127.0.0.1)
    [-db Dir] where to store data (default wiki)
    [-admin]  print a temporary admin password

Initial configuration of Wikilon requires the -admin password, but the admin
can create other users with administrative authorities. The admin password
is valid only until the process restarts.

Wikilon does not directly support TLS. Use a reverse proxy such as NGINX to
wrap local Wikilon connections with SSL/TLS/HTTPS. IP 0.0.0.0 is a wildcard,
but one should consider security implications.
"""

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

let getEntropy (n : int) : ByteString = 
    let mem = Array.zeroCreate n
    do RandomNumberGenerator.Create().GetBytes(mem)
    Data.ByteString.unsafeCreateA mem

let hashStr = Stowage.Hash.hash >> Data.ByteString.toString

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
                let e = getEntropy 64
                let pw = hashStr(e).Substring(0,24)
                do printfn "admin:%s" pw
                Some pw
    let dbMaxSize = (4 * 1024 * 1024) // todo: enable configuration

    let db = Stowage.DB.load "db" dbMaxSize
    let app = WS.mkApp db admin
    do startWebServer svc app
    0 // return an integer exit code



