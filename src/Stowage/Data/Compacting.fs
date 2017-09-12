namespace Stowage
open Data.ByteString

/// A simple compactable value type wrapper.
///
/// This covers the common case where we want to potentially hold a
/// local summary of data that is written to disk. 
type Compacting<'V, 'M when 'V :> IMeasured<'M>> =
    | Local of 'V
    | Remote of LVRef<'V> * 'M

module Compacting =

    let codec (cM:Codec<'M>) (cV:Codec<'V>) (threshold:int) = 
        { new Codec<Compacting<'V,'M>> with
            member c.Write elem dst =
                match elem with
                | Local v -> 
                    EncByte.write (byte '[') dst
                    cV.Write v dst
                    EncByte.write (byte ']') dst
                | Remote (ref,m) ->
                    EncByte.write (byte '{') dst
                    ByteStream.writeBytes (ref.ID) dst
                    EncByte.write (byte '}') dst
                    cM.Write m dst
            member c.Read db src =
                let b0 = EncByte.read src
                if (b0 = (byte '[')) then
                    let v = cV.Read db src
                    EncByte.expect (byte ']') src
                    Local v
                else if(b0 <> (byte '{')) then
                    raise ByteStream.ReadError
                else
                    let h = ByteStream.readBytes rscHashLen src
                    EncByte.expect (byte '}') src
                    let m = cM.Read db src
                    let ref = LVRef.wrap (VRef.wrap cV db h)
                    Remote(ref,m)
            member c.Compact db elem =
                match elem with
                | Remote (ref,m) -> 
                    let struct(m',szM) = cM.Compact db m
                    struct(Remote (ref,m'), 2 + rscHashLen + szM)
                | Local v ->
                    let struct(v',szV) = cV.Compact db v
                    if (szV < threshold) then 
                        struct(Local v', 2 + szV) 
                    else
                        let ref = LVRef.stow cV db v'
                        let struct(m,szM) = cM.Compact db (v'.Measure)
                        struct(Remote (ref,m), 2 + rscHashLen + szM)
        }
                    
                    



