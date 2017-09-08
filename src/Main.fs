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
    [-dir Dir] where to store data (default wiki)
    [-size GB] maximum database size (default 3000)
    [-admin]  print a temporary admin password

Most configuration of Wikilon is managed online, and requires administrative
authority. Requesting an `-admin` password makes the admin account available
until process reset. The admin can manage authorities for other accounts.

Wikilon does not support TLS. Instead, use a reverse proxy (like NGINX) to 
wrap the HTTP service with SSL/TLS/HTTPS. IP 0.0.0.0 is a wildcard, but do
consider security implications. 
"""

type Args = {
  help : bool;
  port : int;
  ip : string;
  home : string;
  size : int;
  admin : bool;
  bad : string list;
}

let defaultArgs : Args = {
  help = false;
  port = 3000;
  ip = "127.0.0.1";
  home = "wiki";
  size = 3000
  admin = false;
  bad = [];
}

let (|Nat|_|) (s : string) : int option = 
    let mutable ival = 0
    if System.Int32.TryParse(s, &ival) && (ival > 0)
        then Some ival 
        else None

let rec procArgs xs (a : Args) : Args = 
    match xs with
    | []   -> {a with bad = List.rev a.bad }
    | "-h"::xs' -> procArgs xs' { a with help = true }
    | "-p"::(Nat p)::xs' -> procArgs xs' { a with port = p }
    | "-ip"::ip::xs' -> procArgs xs' { a with ip = ip }
    | "-dir"::dir::xs' -> procArgs xs' { a with home = dir }
    | "-size"::(Nat n)::xs' -> procArgs xs' { a with size = n }
    | "-admin"::xs' -> procArgs xs' { a with admin = true }
    | x::xs' -> procArgs xs' {a with bad = x :: a.bad }

let getEntropy (n : int) : ByteString = 
    let mem = Array.zeroCreate n
    do RandomNumberGenerator.Create().GetBytes(mem)
    BS.unsafeCreateA mem

let hashStr = Stowage.Hash.hash >> BS.toString

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
                let pw = hashStr(getEntropy 64).Substring(0,24)
                do printfn "admin:%s" pw
                Some pw
    let db = Stowage.API.openDB "data" (1024 * args.size)
    let app = WS.mkApp db admin
    try startWebServer svc app
    finally Stowage.API.closeDB db
    0 // return an integer exit code



