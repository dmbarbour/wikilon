namespace Wikilon
open Awelon
open Stowage
open Data.ByteString

// Wikilon will host multiple dictionaries and multiple users.
// 
// Active dictionaries will define as set of bots via `app-*`. But we'll
// also have inactive dictionaries such as checkpoints and histories, or
// perhaps based on quotas and payments.
//
// We could perhaps model Wikilon itself using the same fractal system,
// with a set of bots operating on a partially persistent environment. It
// might also be useful to have a `master` dictionary.
//
// In any case, it should be easy to build projections over variables,
// and to wait on variables in a projection for COMET-style real-time
// updates.
// 
//module WikiState =






