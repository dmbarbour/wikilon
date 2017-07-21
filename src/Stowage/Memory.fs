
namespace Stowage.Internal

open System.Security
open System.Runtime.InteropServices

[< SecuritySafeCriticalAttribute >]
module internal Memory =
    // Stowage needs fast native byte comparisons on check/write. 
    type size_t = unativeint

    module Native =
        // conditional compilation seems awkward here.
        //[< DllImport("msvcrt", CallingConvention = CallingConvention.Cdecl) >]
        [< DllImport("c", CallingConvention = CallingConvention.Cdecl) >]
        extern int memcmp(nativeint l, nativeint r, size_t len);

    // implement memcmp via Marshal.ReadByte.
    let memcmp_simple (l : nativeint) (r : nativeint) (len : int) : int =
        let rec loop ix =
                if (len = ix) then 0 else
                let lb = Marshal.ReadByte(l,ix)
                let rb = Marshal.ReadByte(r,ix)
                if (lb = rb) then loop (1 + ix) else
                if (lb < rb) then (-1) else 1
        loop 0

    // TODO: benchmark use of memcmp vs. Native.memcmp
    let inline memcmp l r n = 
        assert(n >= 0)
        //Native.memcmp(l,r, size_t n)
        memcmp_simple l r n

    // constant-time equality comparison.
    let memcteq (l : nativeint) (r : nativeint) (len : int) : bool =
        let mutable accum = 0uy
        for ix = 0 to (len - 1) do
            let lb = Marshal.ReadByte(l,ix)
            let rb = Marshal.ReadByte(r,ix)
            accum <- (accum ||| (lb ^^^ rb))
        (0uy = accum)



