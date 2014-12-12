A history-preserving key-value database for Wikilon.

# Wikilon Database Requirements

## Expected Content

Wikilon, if successful, will host multitudes of program artifacts: functions, documents, spreadsheets, ray traced models, music, game worlds, and so on. Wikilon has two major centers of data:

1. the dictionary, consisting of AO words (which become wiki pages)
2. the filesystem, where every element is a purely functional object

While the dictionary consists only of AO functions, the idea is to leverage naming conventions such that the dictionary (together with a sufficient degree of caching) becomes almost an operating system of its own. For example:

* automatic tests, with zero-button automatic testing, are simply words starting with the prefix `test.`
* a spreadsheet `foo` might be modeled using a collection of words of form `a1$foo` and `b3$foo`. 
* web applications might use a name like `wikilon/picturesOfCats`, and be accessible by URL.

But the dictionary is insufficient for certain purposes, such as tracking user input, session information, IDE-layer caching, and use of Wikilon as a tailorable software platform, e.g. for robot control or an MMORPG. Thus Wikilon needs a state layer suitable for these purposes. To this end, I'm designing Wikilon's own variation on the filesystem. Rather than dumb files as byte streams, Wikilon's filesystem consists of purely functional objects made from Awelon bytecode. These objects shall take the same basic forms as embedded literal objects, modeling something like:

        type Object = { query :: Input → Response, update :: Input → Object }

The benefit of having a system of objects is that these objects can easily protect their own invariants and guard information. Thusly, we can support an intermediate layer of permissions: in addition to read-write at the source layer, we can have query-update permissions at the object layer. Usefully, this object model is also readily adapted to RDP, e.g. if we replace the 'update' argument with a set of inputs. (In addition to pure objects, the Wikilon filesystem might transparently support stateless scripts that can operate on multiple objects.)

A secondary benefit is that interesting forms of semantic compression and separate compilation become available when files are known to consist of bytecode.

## Historical Snapshots

Besides supporting values as they are, I wish to track values as they were - e.g. a few thousand snapshots from history, plus named snapshots. This history will be very useful for debugging, retroactive testing, computing differences, understanding a system through its evolution, resilience against vandalism, and so on. To this end, I'm planning on a composition of windowed history (keep the last K snapshots) with a [logarithmic history](https://awelonblue.wordpress.com/2014/10/08/logarithmic-history-v3/) (which keeps N snapshots that reach exponentially into the past).

(This is orthogonal to whatever history the objects might track internally.)

## Fine Grained Persistence

Say we have an object in the filesystem that contains a very large data structure, e.g. a balanced tree map with a million elements. A simple query or update on this object should take logarithmic time. However, a naive serialization of objects would undermine this, e.g. requiring we load all million elements into memory prior to the query, or store all million after the update. This would lead to ugly workarounds, as developers pursue performance over aesthetics.

To make a filesystem of objects viable, it's important that we can load and manipulate just the portion of the objects that we're using. Normal filesystems achieve this through fseek or mmap, which introduces all sorts of accidental complexity in the form of alignment and sizing concerns. Ideally, we could work with plain old values even in the persistence layer.

So, I think we need persistence below the granularity of 'Object', such that we can load only the parts of an object that we're using. The resulting system would be simple and efficient, with no need for ad-hoc workarounds. From the ABC layer, this persistence may be guided by developer annotations, and perhaps a few heuristics.

## Structure Sharing and GC

Structure sharing is essential for efficient representation of historical snapshots, and is augmented when taken together with fine-grained persistence. And it's also useful as a general default: when developers know that structure sharing is the default, it simplifies a great deal of reasoning about performance, caching, and communication. 

Of course, structure sharing comes with a requirement for GC. Given the massive scale of a database, any GC that scans the whole database would be a disaster. And in context of historical snapshots, the common GC assumption that "objects tend to die young" is invalid. But I think reference counting could work well, and could be made concurrent (e.g. via queueing).

# Proposed Database Design

Wikilon's database will build upon a small set of key-value databases. This underlying database will take care of all the more primitive issues, such as: efficiency, scalability, persistence, concurrency. After much research, spinning in circles, and analysis paralysis, I've selected **LMDB** for its simplicity, performance characteristics, and friendly license. (I'm at a stage where I needed to pick one, run with it, trust my analysis, and stop second guessing myself.)

## Values Database

Above LMDB, I'll implement the following **values database**:

        type Value = ... structured ...
        type Address = Int64  
        type ValHash = Int64  -- e.g. via murmur hash 
        table values: Address → Value
        table refcts: Address → Int64
        table refct0: Address → ()
        table caddrs: ValHash → [Address]
        table roots:  String → Value (or → Address)

Essentially, I'm simulating an extended, persistent memory for loading and storing values. The values in this case are immutable; after construction, they are only garbage collected. The content address table `caddrs` makes it easy to perform structure sharing within larger values. Use of consecutive addresses ensures that newly constructed values tend to be near one another in the address space, which (according to [work by Robert Harper and Guy Blelloch](https://existentialtype.wordpress.com/2014/09/28/structure-and-efficiency-of-computer-programs/)) should be sufficient to guarantee near-optimal cache performance (assuming appropriate design of data structures and algorithms).

In this case the addresses will simply increment by one for each value added, since the address doesn't need to account for space. I'm going to make a simplifying assumption, that we never run out of values. This will hold true for at least a few centuries, and we can address the compacting/reuse problem later with a goal of optimizing cache performance by moving related values nearer to each other.

The `refct0` table supports concurrent background GC. It's acting as a queue for addresses that have reached zero reference counts. The `roots` table is a place to record persistent values that we can load into memory.

## Database is just a Value

After building the value model above LMDB, we'll be much closer to Haskell's general approach of modeling everything with values. At this point, I'll just go the rest of the way: a database, including all historical values, is simply modeled using a value. It's then up to the data structure to make this efficient. 

Not every persistent value will need to track a history. But databases with history will model that history explicitly, i.e. as part of their value. 

# Everything else...

Modeling the history and such will be shifted to the Wikilon package. So this wikilon-db will actually be closer to a value-db. Maybe I should rename it? I would like to provide a nicer interface for this value database, e.g. compared to LMDB's `MDB_val` structures. An interesting possibility is also to combine multiple writer transactions in the higher layer.

I already have ideas on how to use this for Awelon values... and perhaps to internalize ABC resources.

An interesting possibility is to support multi-writer transactions, e.g. by explicitly modeling STM above database values.
