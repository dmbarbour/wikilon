
acid-state is pretty nice, but keeping everything in memory all the time does limit scalability. While this won't be an issue for a little while, I anticipate creation of video games, art, and other objects, even as a major use case for Wikilon. While logarithmic history does limit how much we keep around, it multiplies storage costs by a large coefficient (e.g. up to 1000x for 1000 samples).

I expect to reach the memory limits sooner rather than later. 

At that time, I may need to develop a variation on the acid-state API, or perhaps an intermediate data-type, to support transparent paging of objects. Doing so should be feasible, if a bit hackish, and might require a separate GC model. Still, if I can efficiently implement the illusion that all information is kept in memory and processed *as a value*, then a few unsafePerformIO hacks to externalize storage of the value should be worthwhile.

A similar, interesting idea is the TX library, which creates persistence above STM. This technique might allow using a lot of smaller variables and values, rather than one large value. Using lots of smaller variables might prove advantageous, both for paging and for allowing parallel writes on different state resources.

I'll eventually be forced to work on these scalability issues.
