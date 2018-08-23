namespace Wikilon
open Data.ByteString
open Stowage

// Wikilon's "User model".
//
// My current idea is to model our set of users as an Awelon dictionary,
// where each user may hierarchically be modeled as having an inventory,
// clipboard, equipment, and similar. Authorities can be modeled via an
// equipped hat/badge/role, keyring, or similar.
//
// I'm interested in supporting remote authentication, e.g. from Google
// or Facebook. But I could support simple passwords, too, e.g. using a
// salt and encoded password pair.
//
// But it seems I should probably delay authentication issues until we
// have something else working. Also, 
This is relatively low priority.
//


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

