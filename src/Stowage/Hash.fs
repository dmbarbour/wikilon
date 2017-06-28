
(* Stowage simply uses a 280-bit Blake2b hash function, but encodes
 * this result in 56 characters of an unusual base32 alphabet:
 *
 *     bcdfghjklmnpqrstBCDFGHJKLMNPQRST 
 *
 * That is, we simply use the first sixteen consonants of the alphabet,
 * in lower then upper case. This resists accidental construction of 
 * anything otherwise meaningful to humans or machines.
 *
 * These hashes are used as references between immutable binary data,
 * enabling flexible representations. We use conservative GC, so hashes
 * must be separated by non-hash characters. 
 *)
module Stowage.Hash

open Konscious.Security.Cryptography

let alphabet = "bcdfghjklmnpqrstBCDFGHJKLMNPQRST"

// probably want to hash an arbitrary byte-stream.
// output is a string

