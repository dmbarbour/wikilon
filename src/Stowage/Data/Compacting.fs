namespace Stowage
open Data.ByteString

/// A simple compactable value type wrapper.
///
/// This covers the common case where we want to potentially hold a
/// local summary of data that is written to disk. 
type Compacting<'V, 'M when 'V :> IMeasured<'M>> =
    | Local of 'V
    | Remote of 'M * LVRef<'V>

module Compacting =
    let cLocal = byte 'L'
    let cRemote = byte 'R'

    let codec (cM:Codec<'M>) (cV:Codec<'V>) (threshold:int) = 
        { new Codec<Compacting<'V,'M>> with
            member c.Write elem dst =
                match elem with
                | Local v -> 
                    EncByte.write cLocal dst
                    cV.Write v dst
                | Remote (m,ref) ->
                    EncByte.write cRemote dst
                    cM.Write m dst
                    EncLVRef.write ref dst
            member c.Read db src =
                let b0 = EncByte.read src
                if (cLocal = b0) then
                    Local (cV.Read db src)
                else
                    let m = cM.Read db src
                    let ref = EncLVRef.read cV db src
                    Remote(m,ref)
            member c.Compact db elem =
                match elem with
                | Remote (m,ref) -> 
                    let struct(m',szM) = cM.Compact db m
                    struct(Remote (m',ref), 1 + szM + rscHashLen)
                | Local v ->
                    let struct(v',szV) = cV.Compact db v
                    if (szV < threshold) then 
                        struct(Local v', 1 + szV) 
                    else
                        let struct(m,szM) = cM.Compact db (v'.Measure)
                        let ref = LVRef.stow cV db v'
                        struct(Remote (m, ref), 1 + rscHashLen + szM)
        }
                    
                    



