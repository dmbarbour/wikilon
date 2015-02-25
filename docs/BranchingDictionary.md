
Git, Mercurial, and other DVCS systems model *branching and merging* of content. This is useful: each developer can operate in his or her own little bubble while still interacting meaningfully with the community, by pushing and pulling content, requests, and issues. Branches can also provide stability, e.g. by allowing some branches to be more carefully curated than others, or using branches to tag release versions. I don't need to worry about 'frozen' words and so on, because it's easy to obtain a frozen snapshot of a whole dictionary.

The challenge is to support both branches and logarithmic history. Options?

* History for all Branches: `History (Map Branch Image)`
* History modeled per Branch: `Map Branch (History Image)`
 * Collect each history independently.
 * Collect all histories concurrently.

A history for the set of all branches would be a simple approach. It provides a snapshot view for multiple dictionaries, though I'm not aware of any scenarios where that property would be desirable. The disadvantage is that there is no way for developers to precisely administrate a branch, e.g. to delete specific versions of content for infosec, intellectual property, or privacy reasons, or to mark a particular branch as dead so it can be cleared more aggressively.

Collecting branch histories independently has its own problems: old, unused branches will continue to take a fixed amount of space, even though nobody needs the older values. It seems wise that old, stable branches of the dictionary should gradually decay to make space for new, growing branches. 

The third option seems best at the moment. We track the total number of images for all branches. When above some threshold, we collect all the histories. The total number of collectable images across all branches may thus be constrained at a whole-wiki level, while preserving effective comparisons within each dictionary. Usefully, we can also introduce features such as removing specific snapshots, or tuning decay rates for 'dead' branches.

So it seems this is an approach I shall pursue for now.

