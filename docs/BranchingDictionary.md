
# Branching Dictionary

Wikilon is based on a 'dictionaries' concept instead of packages. A dictionary is always complete (i.e. no external dependencies), but developers can easily fork, merge, curate, and maintain dictionaries. Content is usually easy to cherry-pick between dictionaries, which would serve a role similar to packages.

A branching dictionary is useful because it enables multiple versions to exist with different levels of stability and curation. Branches also serve as version tags, e.g. if developers wish to 'freeze' a version of a dictionary they can do so by forking then keeping one branch stable. 

Each branch will have an *incomplete* history of versions. I plan to leverage exponential decay (logarithmic history) models, i.e. such that we lose intermediate versions the further we look into the past. Branches may decay independently, or I may choose to decay all branches at once. An abandoned branch will decay more rapidly than others.

A challenge with incomplete histories is that I cannot readily align commentary (e.g. change logs, merge history, issue trackers) with commits. I'm tempted to support this instead as part of the dictionary itself. Doing so would make this information more accessible, extensible, flexible. 

I wonder if we can align 'merge' behaviors with sealed values. E.g. for rendering, we'll probably leverage discretionary sealers like `{:foo}` to help drive rendering, looking up an associated render function. Potentially, we can do the same for merge behavior, looking up a merge function in the dictionaries. This would make it easy for us to combine changelogs and issue sets when merging.

## Curation of Branches

Typical workflows with branching involve having some development branches, some feature branches, some branches that are more experimental than others, some that are tightly curated. I'm not entirely sure how to do the access control for all of this yet, but I don't believe it will be an issue.

Relatedly, we might want to receive notifications when a 'related' branch is updated. A branch is related on our future:

1. if we're going to merge branch F into Master, we want to hear about changes in Master, so we can merge them into F.
1. if someone else is going to merge branch F' into Master, we want to hear about changes in F', so we can avoid conflicts and coordinate efforts.
1. if omeone else wants to merge G into F', which will be merged into master, we want to hear about it, again to avoid conflicts.

Of course we cannot see the future, so we probably need to announce our intentions. We might announce and coordinate via the Master dictionary itself, using a specific word, but that seems awkward and difficult to control. It may be best to coordinate via the AVM layers and network model, with an understanding that a dictionary is (more or less) hosted by an AVM.

For computer supported cooperative work (CSCW), it's important that these questions be answered almost in real-time. This suggests that users operating on the same curated 'origin' should know about each other, what each individual is working on, and have some ability to perform pull requests even at this layer. 

A branch may always be disconnected from its origin if there is no intention to push and pull between them. In a general sense, I should probably model all sorts of relationships between branches: pushes, pulls, pull-requests, origins, intentions, and maybe some issues management and bug trackers. So our model of a branch might be a fair bit more sophisticated than just a history dictionary images.

## Merging and Cherry Picking 

Merging usually involves computing a change-set then applying it. 

The change-set is computed relative to some common historical image. So, it's important that we also track when merges have occurred between branches so we know what are the common origins are. Computing a change-set relative to a historical image allows users to isolate content that a specific human was manipulating from the content manipulated by other humans.

When merging, a useful feature is potentially to *cherry pick* content, i.e. to merge a specific set of words (and their dependencies). Also, the ability to rename on merge might be useful. I'm not entirely sure how to address these, except maybe to specify a set of toplevel words that we wish to merge.

Now, it would be pretty trivial to just grab the current value of a word. But I think we can do better if we grab the whole history of the remote wiki, perhaps filtered for whatever words you've cherry-picked and the dependencies for those words.

Merging histories has the advantage of being a streaming model with respect to synchronization - e.g. we can continue to receive transactional updates to the remote wiki, and checkpoint/regenerate the merged dictionary accordingly. Further, keeping histories can help us recover information about words with the same origin, and hide origins for non-conflicting words.

I haven't yet hammered out the details of these ideas. But I think the way forward with forking or inheriting from other wikis will be based on such merges.









