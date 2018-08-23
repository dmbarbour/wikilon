
namespace Wikilon
open Stowage

/// A time-stamp. Alias to .Net System Ticks. This is mostly for
/// internal use within applications.
type TimeStamp = int64
module TimeStamp =
    let now () : TimeStamp = System.DateTime.UtcNow.Ticks
    let toDateTime (tm:TimeStamp) : System.DateTime = System.DateTime(tm,System.DateTimeKind.Utc)
    let fromDateTime (dt:System.DateTime) : TimeStamp = dt.ToUniversalTime().Ticks

// A snapshot-based history model.
module Snapshot =

    /// A history is modeled as a list of time-stamped snapshots with
    /// the latest snapshots near the head. Exponential decay is used
    /// to limit the list length, erasing intermediate snapshots to 
    /// keep entries fairly well distributed over time. The longer our
    /// history, the more snapshots we'll preserve. Length can be set
    /// via the codec, to erase entries upon compaction.
    type H<'V> = (struct(TimeStamp * 'V)) list

    /// Add an entry to the history. Adds to best location. Under
    /// normal circumstances, it's assumed you'll add timestamps
    /// near the head (i.e. for more recent entries), so this would
    /// be O(1).
    let rec add (tm:TimeStamp) (v:'V) (h:H<'V>) : H<'V> =
        match h with
        | (struct(tmHd,vHd)::h') when (tmHd > tm) ->
            struct(tmHd,vHd) :: (add tm v h')
        | _ -> (struct(tm,v)::h)

    /// Find historical entry most relevant to a given timestamp.
    /// May return None, if no entry exists for the given time.
    let rec tryFind (tm:TimeStamp) (h:H<'V>) : 'V option =
        match h with
        | (struct(tmHd,vHd)::h') ->
            if (tm >= tmHd) then Some vHd else
            tryFind tm h'
        | [] -> None

    let inline private tmDiff tm tm' = abs (tm - tm')

    // find least difference of timestamps within history
    let rec private leastTimeDiff tm ltd hst =
        match hst with
        | (struct(tm',_)::hst') ->
            let ltd' = min ltd (tmDiff tm tm')
            leastTimeDiff tm' ltd' hst'
        | [] -> ltd

    // remove first entry matching a timestamp difference
    let rec private delTimeDiff tm ltd hst =
        match hst with
        | (struct(tm',v)::hst') ->
            if ltd = (tmDiff tm tm') then hst' else
            struct(tm',v)::(delTimeDiff tm' ltd hst')
        | [] -> []

    // remove one entry from history based on least time difference.
    // Does not remove head entry from chunk.
    let private decayChunk hst =
        match hst with
        | (struct(tm0,v0)::hst') ->
            let ltd0 = System.Int64.MaxValue
            let ltd = leastTimeDiff tm0 ltd0 hst'
            struct(tm0,v0)::(delTimeDiff tm0 ltd hst')
        | [] -> []

    // exact sized split-list (may failwith insufficient data).
    // I assume `n` is relatively small, so I can use the stack.
    let rec private splitListAt n lst =
        if (0 = n) then struct([],lst) else
        match lst with
        | (hd::lst') ->
            let struct(tl,rem) = splitListAt (n-1) lst'
            struct(hd::tl,rem)
        | [] -> failwith "insufficient data"

    // take fair-sized chunks of size at least k. Fair-size means
    // the smallest chunk and largest chunk are at most one element
    // different in size. When our list is large compared to k, the
    // size of our chunks will swiftly approach k (from above).
    let private fairChunks (k:int) (lst0:'V list) : ('V list) seq =
        assert(k > 0)
        let n = List.length lst0
        if (0 = n) then Seq.empty else
        let ct = n / k // ct chunks of at least size k
        if (ct < 2) then Seq.singleton lst0 else
        let szMin = (n/ct) // minimum size for each chunk
        assert(szMin >= k) // our contract assumption
        let rem = (n%ct) // first rem chunks have sz = szMin+1
        let step (struct(idx,lst)) =
            if (idx = ct) then (assert(List.isEmpty lst); None) else
            let sz = szMin + if (idx < rem) then 1 else 0
            let struct(chunk,lst') = splitListAt sz lst
            Some(chunk, struct(idx+1,lst'))
        Seq.unfold step (struct(0,lst0))

    /// Decay a history by removing about 1/K snapshots, for k at 
    /// least 3. Based on an exponential decay model with fairness.
    let decay (k:int) (hst:H<'V>) : H<'V> =
        assert(k >= 3)
        hst |> fairChunks k |> Seq.map decayChunk |> List.concat

    // incremental decay until quota is reached (elim 1/K per step)
    let rec private decayToQuota (q:int) (k:int) (h:H<'V>) : H<'V> =
        if (q >= (List.length h)) then h else 
        decayToQuota q k (decay k h)

    /// Codec with lossy, exponential-decay inspired compaction model.
    ///
    /// With this codec, you choose an approximate number of snapshots
    /// to retain. Upon compaction, snapshots will incrementally erase
    /// until quota is met, using a fair exponential decay algorithm
    /// that results in snapshots of the past being further temporally
    /// distributed compared to snapshots of recent changes. This will
    /// ensure deep history is robust against large numbers of "recent"
    /// updates, while keeping size constant (modulo changes in size 
    /// of individual snapshots). 
    let codec (entryQuota:int) (cV:Codec<'V>) : Codec<H<'V>> =
        let cTV = EncPair.codec' (EncVarInt.codec) cV
        { new Codec<H<'V>> with
            member __.Write h dst = EncArray.write cTV (List.toArray h) dst
            member __.Read db src = List.ofArray (EncArray.read cTV db src)
            member __.Compact db h0 =
                let hQ = decayToQuota entryQuota 9 h0
                let struct(hQA',sz) = EncArray.compact' cTV db (List.toArray hQ)
                struct(List.ofArray hQA', sz)
        }

// TODO: snapshot histories are useful for dictionaries, but an alternative
// is an event-based update history. In that case, we would want to merge
// updates during the decay step - i.e. a monoidal update-event type. This
// might be worth developing for some other parts of Wikilon. Eventually.
        
        
        

    
