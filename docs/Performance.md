
Wikilon Performance
===================

I don't expect Wikilon to be blazing fast. However, I would like to achieve at least reasonable performance. Here are some viable mechanisms:

1. Partial evaluation for embedded values for fast load and quotation.
1. Structure sharing and lazy loading for large values through VCache.
1. Optimize internal representation of blocks and text for efficient slicing. 
1. Optimize a dictionary of specific, common functions similar to ABCD. 
1. Optimize data representation for collections types with dictionary ops.
1. Compile some resources into plugins or separate VMs or processes.

I plan to focus compilation efforts towards deployment of VM images. This is especially suitable for compiling abstract virtual machines from our [network model](NetworkModel.md), or sets thereof. Security is a motivation here: with VMs, the attack surface is smaller even than the OS kernel. Further, this eliminates external dependencies, other than deployment. 

My earlier efforts at Haskell plugins for performance haven't been successful. In part, my plugin types were too abstract. Concrete types might give GHC more to work with. In part, Haskell's plugins system needs an overhaul. This might be worth pursuing for fast interpreters, but I'd prefer to try simpler methods first.

## Wikilon ABC in the Dictionaries?

Wikilon's internal ABC is primarily intended for the abstract virtual machines. An interesting question is whether this is worth pursuing at the dictionary layer. 

* Having 'fast' ABC at the dictionary layer may prove useful for structural editing, especially if I ever work with very large structures where I can leverage the interning features of Wikilon.
* OTOH, it is simple to keep pure ABC or 'raw source' in the dictionary. This is stable, easy for import, export, update. It is inefficient for large structures, but that might encourage better factoring.

I lean in favor of the latter 'keep it simple and stable' approach. However, to support [dictionary apps](ApplicationModel.md), structure editors with large structure, the latter approach may require some extra layers of indirection in the form of caching and related features.

