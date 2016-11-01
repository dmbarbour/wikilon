# Awelon Object (AO)

## AO Development

AO development consists of dictionary construction, sharing, and maintenance. 

Humans will directly develop AO primarily through [editable views](EditingAO.md), which must be defined within a dictionary and rendered through an intermediate service. A web service is likely, but a [filesystem in userspace](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) adapter could enable developers to leverage emacs, vim, and so on. This is essentially a [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) pattern, with the AO dictionary as the storage layer.

Indirectly, dictionaries are also developed through the Awelon project [application model](ApplicationModel.md). Software agents and humans both systematically manipulate the dictionary via RESTful patterns, modeling databases, work orders, and publish-subscribe patterns as a basis for real-time systems with real-world effects. In this context, a dictionary serves as a substrate for applications much like filesystems do conventionally, except with linking, computation, composition, and spreadsheet-like characteristics built in.

See linked documents on editable views and the application model for details. 


## AO Evaluation and Linking

Dictionaries are the basic unit of AO evaluation. AO evaluation logically rewrites an AO dictionary into a different representation of the same AO dictionary. 

        eval :: Dictionary → Dictionary

In practice, this evaluation will frequently be lazy. Instead, we evaluate only a subset of the dictionary sufficient a particular observation - i.e. to evaluate a word's definition or a specific AO query string.

### Incremental Computing and Caching 


Our application state is modeled by a compositional structure `x*y`. With persistence, updates will tend to involve either the the `x` or `y` but not both. Hence, it becomes feasible to leverage a cached `V(x)` or `V(y)` from a prior computation. This applies hierarchically, i.e. `x` might equal `v*w`. Many systems will fit this pattern or can be made to fit with a little consideration.

A question, then, is how do we know what to cache? I propose the following:

* we *implicitly* cache evaluation over words
* we *explicitly* cache any other computation

Cached evaluation over words is a natural fit for AO. We evaluate before linking anyway, we use a word many times within a dictionary. And in contexts like Wikilon, we'll frequently look up evaluations of words based on external HTTP requests. Incremental computing is one more reason among many. 

Explicit caching will be expressed by memoization annotation:

        [computation]{&memo}

Memoization doesn't force immediate evaluation. Instead, this annotation tells our runtime to use a memoization cache if we later evaluate. Caching may be heuristic, based on observed time/space tradeoffs. 

Developers can use simple techniques such as modeling suitable memoization points and batches in data structures to mitigate potential efficiency issues of fine-grained caching. Careful use of stowage can also help, by reducing the serialization overhead for stowed fragments of the computation.

#### Cache Design

Caching can be implemented by taking a *secure hash* of the representation and performing a lookup. In case of `{%word}` tokens, we do not know whether those words would be linked during evaluation or preserved as symbols. So conservatively we might include both the `{%word}` symbol and a reference to the word's static link object. 

        [{%foo}{%bar}{%baz}]{&memo}

        cacheID = SecureHash {%foo}{%bar}{%baz} (foo)(bar)(baz)
            where (X) is cacheID of X's static link object

Taking these constraints overall, we might assume four tables of form:

* **dictionary:** word → def. Developed by humans and software agents. 
* **clients:** word → words. Computed directly from the dictionary. 
* **data cache:** secure hash → def. Where we keep cached computations.
* **word cache:** word → (eval, link, metadata). Computed by evaluator. 

Our dictionary has a set of definitions. We maintain an index of immediate clients for each word for reverse lookup, rename, incremental cache invalidation, etc.. Our data cache is updated by the `{&memo}` annotation or a local equivalent, may be shared by multiple dictionaries, and may be maintained heuristically (e.g. random deletion is okay). 

Challenges surround maintenance of the *word cache*. Upon updating a word's definition, its static link object may be invalid, and transitively any clients of that word. 

An easy maintenance technique is perhaps to conservatively clear the word cache then rebuild it lazily. Assuming the *data cache* is used to evaluate words and static link objects, we might avoid the bulk of unnecessary recomputation. However, it might be advantageous to more precisely invalidate the word cache. This requires evaluation *during* invalidation, which has its own challenges. 

A heuristic balance of precise and lazy cache maintenance may prove effective in the general case, e.g. based on an effort quota upon performing each dictionary update. 

*Aside:* We can potentially improve cache precision for words by removing symbols we know will be inlined. Use of the [VerSum SeqHash](https://people.csail.mit.edu/nickolai/papers/vandenhooff-versum.pdf) concept could even logically inline subprograms for purpose of hashing and caching. It seems worth looking into.

### Accelerated Dictionary

Acceleration is a primary approach to achieving performance in ABC systems. An AO/ABC runtime will specify accelerated dictionaries, e.g. with definitions like `@i [][]baad` (inline) that it recognizes and hand-optimizes. More broadly, accelerators can also leverage specialized representations like for natural number construction and arithmetic, or accelerating evaluation of process networks to leverage multiple CPUs.

This is discussed more under [ABC](AboutABC.md). 
