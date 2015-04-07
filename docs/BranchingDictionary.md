
# Branching Dictionary

Wikilon is based on a 'dictionaries' concept instead of packages. A dictionary is always complete (i.e. no external dependencies), but developers can easily fork, merge, curate, and maintain dictionaries. Content is usually easy to cherry-pick between dictionaries, which would serve a role similar to packages.

A branching dictionary is useful because it enables multiple versions to exist with different levels of stability and curation. Branches also serve as version tags, e.g. if developers wish to 'freeze' a version of a dictionary they can do so by forking then keeping one branch stable. 

## Branching with History

Having historical information for a dictionary is useful. It can ensure greater resilience to mistakes or vandalism. It can help us find when a bug was introduced, and thus help trace it to a particular source. However, to avoid space concerns of keeping large histories, I'll be using an exponential decay model, i.e. keeping a logarithmic-scale history.

### Branching while Forgetting

One challenge is to support both branches and logarithmic history. Options?

* History for all Branches: `History (Map Branch Image)`
* History modeled per Branch: `Map Branch (History Image)`
 * Collect each history independently.
 * Collect all histories concurrently.

A history for the set of all branches would be a simple approach. It provides a snapshot view for multiple dictionaries, though I'm not aware of any scenarios where that property would be desirable. The disadvantage is that there is no way for developers to precisely administrate a branch, e.g. to delete specific versions of content for infosec, intellectual property, or privacy reasons, or to mark a particular branch as dead so it can be cleared more aggressively.

Collecting branch histories independently has its own problems: old, unused branches will continue to take a fixed amount of space, even though nobody needs the older values. It seems wise that old, stable branches of the dictionary should gradually decay to make space for new, growing branches. 

The third option seems best at the moment. We track the total number of images for all branches. When above some threshold, we collect all the histories. The total number of collectable images across all branches may thus be constrained at a whole-wiki level, while preserving effective comparisons within each dictionary. Usefully, we can also introduce features such as removing specific snapshots, or tuning decay rates for 'dead' branches.

So it seems this is an approach I shall pursue for now.

## Continuous and Pending Merges

It seems useful that we might have a 'branch' of the wiki for every developer, i.e. as a simple basis for transactional editing with undo. And a useful feature, in context of editing with the intent to push content back upstream, is to know:

* when have conflicting edits been made to our origin?
* when and where is conflict between user edits likely?

For computer supported cooperative work (CSCW), it's important that these questions be answered almost in real-time. This suggests that users operating on the same curated 'origin' should know about each other, what each individual is working on, and have some ability to perform pull requests even at this layer. 

A branch may always be disconnected from its origin if there is no intention to push and pull between them. In a general sense, I should probably model all sorts of relationships between branches: pushes, pulls, pull-requests, origins, intentions, and maybe some issues management and bug trackers. So our model of a branch might be a fair bit more sophisticated than just a history dictionary images.

## Merging and Cherry Picking 

Merging usually involves computing a change-set then applying it. 

The change-set is computed relative to some common historical image. So, it's important that we also track when merges have occurred between branches so we know what are the common origins are. Computing a change-set relative to a historical image allows users to isolate content that a specific human was manipulating from the content manipulated by other humans.

We can potentially reverse this, if we force humans to continuously merge content from the head of their origin branch. But an interesting point is that every branch probably needs a proper copy of its origin at the time of branching. 

When merging, a useful feature is potentially to *cherry pick* content. 

I wonder if it might be useful to create specialized dictionaries, i.e. a dictionary whose content is filtered down to just a few toplevel words and all their deep dependencies, as a potential basis for cherry picking. 

When merging, one mechanism to eliminate conflicts is to rename content. But it isn't clear that we have a generic way to rename content once we start considering [extensible syntax](ExtensibleSyntax.md). I'll need to think about this.



Now, it would be pretty trivial to just grab the current value of a word. But I think we can do better if we grab the whole history of the remote wiki, perhaps filtered for whatever words you've cherry-picked and the dependencies for those words.

Merging histories has the advantage of being a streaming model with respect to synchronization - e.g. we can continue to receive transactional updates to the remote wiki, and checkpoint/regenerate the merged dictionary accordingly. Further, keeping histories can help us recover information about words with the same origin, and hide origins for non-conflicting words.

I haven't yet hammered out the details of these ideas. But I think the way forward with forking or inheriting from other wikis will be based on such merges.









