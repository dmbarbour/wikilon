
namespace Wikilon

/// A time-stamp. Alias to .Net System Ticks. 
type TimeStamp = int64
module TimeStamp =
    let now () : TimeStamp = System.DateTime.UtcNow.Ticks
    let toDateTime (tm:TimeStamp) : System.DateTime = System.DateTime(tm,System.DateTimeKind.Utc)
    let fromDateTime (dt:System.DateTime) : TimeStamp = dt.ToUniversalTime().Ticks

module History =

    /// A history modeled as a list of time-stamped snapshots,
    /// ordered so the latest times are near the head. We will
    /// gradually use exponential decay to erase older entries,
    /// trying to keep entries well distributed over time.
    type H<'V> = (struct(TimeStamp * 'V)) list

    /// Add an entry to the history. Adds to best location.
    let rec add (tm:TimeStamp) (v:'V) (h:H<'V>) : H<'V> =
        match h with
        | (struct(tmHd,vHd):h') when (tmHd > tm) ->
            (struct(tmHd,vHd)) : (add tm v h')
        | _ -> (struct(tm,v):h)

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

    // remove one entry from the history, selecting the one
    // with the least timestamp difference. 
    let private decayChunk hst =
        match hst with
        | (struct(tm0,v0)::hst') ->
            let ltd0 = System.Int64.MaxValue
            let ltd = leastTimeDiff tm0 ltd0 hst'
            struct(tm0,v0)::(delTimeDiff tm0 ltd hst')
        | [] -> []

    /// Decay a history by removing about 1/K snapshots.
    /// Uses an exponential decay model with fairness.
    /// Never deletes the most recent entry.
    let decay (k:int) (hst:H<'V>) : H<'V> =
        // Note: chunkBySize isn't "fair" in the sense that it
        // can result in a leftover chunks of one or two items.
        // Ideally, I'd use a fair chunking algorithm, but that
        // is surprisingly painful to write. For now, I'm using 
        // a simplistic reversal before chunking to ensure our
        // historically last chunk is full sized, and thus that
        // the final element can be deleted. 
        hst |> List.rev 
            |> List.chunkBySize k 
            |> List.map (List.rev >> decayChunk)
            |> List.rev 
            |> List.concat 

    // Break a collection size N into chunks of size near K,
    // of roughly even size. (Modulo when N < K). We'll 
    let private chunkSizes n k =
        assert(k > 1)
        if (n < k) then [n] else
        let c1 = (n / k)
        let b1 = n / c1
        
        let chunkRem1 = n / chunk

        if (n % k
        
        assert(k > 1)
        let chunkCount = 
            
        let ct = n / k
        let rem = n % k
        if (ct > rem) then // add 1 to each of rem chunks.
            let mem = Array.create ct k
            for i = 1 to rem do
                mem.[i
                 
            // we can simply add 1 to rem chunks.
        else // try a smaller chunk size
            if 
            let ct' = 1 + ct
            let rem
        
        let minChunks = n / (k + 1)
        let maxChunks = n / k
        

    let decay (k:int) (h:H<'V>) : H<'V> =
        


    /// Delete roughly 10% of the historical entries. Always
    /// preserves the most recent entry. Distributes deletions
    /// more or 
    let decimate (h:H<'V>) : H<'V> =
        List.map decayChunk (List.chunkBySize 7 h)

    // compute reasonable chunk sizes near `n` for a given k.
    let private chunkSizes n k = 
        assert(k > 1)
        if (1 > n) then [] else
        if (k >= n) then [n] else
        

        if (k >= n) then [n] else
        let chunkCt = (n + ((2*k)/3)) / k
        let base = n / chunkCt
        let rem = 

        let 
        if (0 = (n % k)) then 
        let minChunks 
        // say we have k=10, so we mostly want chunks of 10-11.
        // 
        // if n < 11, then we can simply return one chunk.
        // if n > 10, we want to split the elements evenly in 2 chunks

        let maxChunks = n / (k + 1)

        let maxFullChunks = n / k
        let maxLargeChunks = n / (k+1)
        if (maxFullChunks < 1) then [n] else
        if  
        if (k >= n) then [n] else
        if (k >= (n/2)) then [(n/2); n - (n/2)] else
        

    // Split a list into chunks of roughly size K.
    let chunk

    /// Exponential Decay Model.
    ///
    /// This will delete some snapshots to help control storage costs.
    /// Precisely, we'll drop 1 in K values for a given K, distributed
    /// evenly over the history, preferring to drop entries that have
    /// not been 

 eliminating snapshots that are near each other

    




