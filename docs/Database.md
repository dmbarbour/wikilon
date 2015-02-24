
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

# Implementation

**VCache** was developed primarily for Wikilon. VCache is essentially an **acid-state** replacement, designed for scalability and working with very large values.

## Database is a Value

I've contemplated two basic designs for the database: modeling the database as a stream, or as a series of snapshots. I favor the series of snapshots as much simpler, i.e. because it's easier to find the value associated with a particular instant in time, and it's easier to delete intermediate historical versions. With structure sharing between versions, it shouldn't be too much worse than the streaming model for space efficiency, i.e. because we're mostly storing the deltas. Since VCache also supports comparisons on VRefs, I should be able to quickly diff versions of the database.

Anyhow, each snapshot is a value, and so is the historical database.

This still leaves a lot of work! E.g. managing indices and cached computations, and validating updates before accepting them, and merging work by multiple users.

## Decay Heuristics

The logarithmic history can be implemented a number of ways. I've been leaning towards the 'decimate the population whenever it gets too high' approach, e.g. if I want 2000 samples then I can reduce from 2000 to 1800 whenever I hit 2000, taking each group of ten and reduce it to nine. This approach offers a lot of heuristic flexibility in deciding which item of each group is destroyed.

I would like to favor conditions like having a more stable gap between samples, and of favoring samples with higher popularity - e.g. possibly by independently also tracking usage information. (10x higher precision? 20k samples or so? example words in each sample, too?).

Besides the logarithmic history, I think it would be useful to support 'tags' such that developers may preserve exact versions of a dictionary under a given tag. It might be necessary to model the master version of the dictionary as just another sequence of tagged versions, and allow multiple independent dictionaries to coexist.

## Model of Words

I have a couple options for modeling words. One is to model them as integers, which might simplify renaming the words or tuning how words are rendered for each user, etc.. or making relationships between words more explicit. But it isn't clear that this would really save me very much. 

The big motivation for modeling words as integers is to simplify renaming of words or allowing users to render words differently. But these purposes would be hindered by: keeping a historical database, enabling user-defined syntax, or the community-level goal of supporting shared language tools reusable from project to project. 

So, for now, I'll just stick with text for words.

## Word Search

If I want to find the right word, it is likely in one of these scenarios:

* lazyness, just enter a few characters and get the rest of the word
* type search, fill a hole in some context; I need to find suitable words.
* relationship search
 * find all words matching a given prefix or suffix
 * find all words that use a given word directly (or K levels of indirection)

A good question is whether I should keep these indices persistently, or compute them as needed. And whether they should be computed for a specific version of the dictionary, or for all versions. For now, I'll err on the side of not putting more into the persistence layer; it's always easy to add it later.





