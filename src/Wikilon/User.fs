namespace Wikilon
open Data.ByteString
open Stowage

// Wikilon's "User model".
// 
// I'm not familiar with conventional user models. My idea for Wikilon
// is to model a user as something like a game avatar, with an inventory
// and equipment and location. In this case, the 'inventory' can include
// authorities and clipboards, 'equipment' can support preferences, macros,
// and views, and 'location' is something like a navigation history and
// session information.
//
// The concrete representation of a user might be an Awelon dictionary,
// to support easy export, import, and debugging. Alternatively, we could
// use a fragment of a dictionary.
//
// I'm interested in supporting remote authentication, e.g. from Google
// or Facebook. But I could support simple passwords, too, e.g. using a
// salt and encoded password pair.
// 
// This is relatively low priority, however. Early on, I can start with
// just admin vs guest.

(*
module User =

    ///

    let private getEntropy (n : int) : ByteString = 
        let mem = Array.zeroCreate n
        do RandomNumberGenerator.Create().GetBytes(mem)
        BS.unsafeCreateA mem

    let createSalt () : ByteString = 
        getEntropy 64 |> RscHash.hash |> BS.take 24


    /// Instead of storing plain-text passwords, we'll mangle things.

    let manglePassword salt pass = 
*)
