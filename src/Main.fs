module Wikilon.Main

open System
open System.IO
open System.Threading
open System.Security.Cryptography
open Stowage
open Suave
open Data.ByteString

let helpMsg = """
Wikilon is a wiki-inspired development environment for Awelon. This program
starts a local web server, which is mostly configured online. Arguments:

    [-help]   print this help message
    [-p Port] bind specified port (default 3000)
    [-ip IP]  bind IP or DNS (default 127.0.0.1)
    [-dir Dir] where to store data (default wiki)
    [-size GB] maximum database size (default 100)
    [-cache MB] space-speed tradeoff (default 100)
    [-admin]  print a temporary admin password

Configuration of Wikilon is managed online. Requesting an `-admin` password
makes the admin account available until process reset. The admin can create
other accounts with administrative authorities.

Wikilon does not have built-in support for TLS. Try a reverse proxy, such as
NGINX, to add the TLS layer if needed.
"""

type Args = {
  help : bool;
  port : int;
  ip : string;
  home : string;
  size : int;
  cache : int;
  admin : bool;
  bad : string list;
}

let defaultArgs : Args = {
  help = false;
  port = 3000;
  ip = "127.0.0.1";
  home = "wiki";
  size = 100
  cache = 100
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
    | "-help"::xs' -> procArgs xs' { a with help = true }
    | "-p"::(Nat p)::xs' -> procArgs xs' { a with port = p }
    | "-ip"::ip::xs' -> procArgs xs' { a with ip = ip }
    | "-dir"::dir::xs' -> procArgs xs' { a with home = dir }
    | "-size"::(Nat n)::xs' -> procArgs xs' { a with size = n }
    | "-cache"::(Nat n)::xs' -> procArgs xs' { a with cache = n }
    | "-admin"::xs' -> procArgs xs' { a with admin = true }
    | x::xs' -> procArgs xs' {a with bad = x :: a.bad }

let getEntropy (n : int) : ByteString = 
    let mem = Array.zeroCreate n
    do RandomNumberGenerator.Create().GetBytes(mem)
    BS.unsafeCreateA mem

let hashStr = Stowage.RscHash.hash >> BS.toString

let setAppWorkingDir fp =
    do Directory.CreateDirectory(fp) |> ignore
    Directory.SetCurrentDirectory(fp)

// thoughts: it might be useful to separate the authorizations DB
// from the main storage layer, e.g. to simplify integration with
// open ID models.

[<EntryPoint>]
let main argv =
    let args : Args = procArgs (List.ofArray argv) defaultArgs
    if args.help then printfn "%s" helpMsg; 0 else 
    let bad = not (List.isEmpty args.bad)
    if bad then printfn "Unrecognized args (try -help): %A" args.bad; (-1) else
    do setAppWorkingDir args.home
    let adminPass =
        if not args.admin then None else
        let pw = getEntropy 64 |> Stowage.RscHash.hash |> BS.take 20
        do printfn "admin:%s" (BS.toString pw)
        Some pw
    Stowage.Cache.resize (1_000_000UL * (uint64 args.cache))
    use dbStore = new Stowage.LMDB.Storage("data", (1024 * args.size))
    let dbRoot = Stowage.DB.fromStorage dbStore
    let dbWiki = DB.withPrefix (BS.fromString "wiki/") dbRoot
    let wsParams : WS.Params = { db = dbWiki; admin = adminPass }
    let app = WS.mkApp wsParams
    let cts = new CancellationTokenSource()
    let svc = 
        { defaultConfig with 
            hideHeader = true
            bindings = [ HttpBinding.createSimple HTTP args.ip args.port
                       ]
            cancellationToken = cts.Token
        }
    let (_,serve) = startWebServerAsync svc app
    Async.Start(serve, cts.Token) 
    printfn "Press any key to halt."
    Console.ReadKey true |> ignore<ConsoleKeyInfo>
    cts.Cancel()
    0 // return an integer exit code


