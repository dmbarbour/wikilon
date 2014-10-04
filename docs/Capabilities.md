
What capabilities should Wikilon expose to the programmers?

Well, there are many kinds of capabilities:

* **annotations** hints for performance and debugging
* **value sealing** for security and rights amplification
* **uniqueness** modeling uniqueness and sources thereof 
* **imperative** direct manipulation of state and network
* **reactive** continuous observation and influence!
* **time-oriented** access to logical times; delays; expirations
* **space-oriented** access to networks, logical partitions, code distribution 
* **state modeling** access state resources; possibly develop state models (w/ linearity)
* **internal reflective** dynamic typing, display, optimizations
* **external reflective** interact with the AO dictionary!
* **bootstrap** initial web applications, etc. (bootstrap dict, too?)
* **accelerators** pure functions built-in for performance
 * note: accelerator capabilities should be instance-specific
 * todo: develop ABCD for global accelerators
* **testing** support capabilities

Regarding **state** for imperative and reactive capabilities:

* MUST respect causal commutativity and spatial idempotence
 * no race conditions on observations
 * updates from multiple capabilities are idempotent
 * perhaps favor temporal state (for multiple readers)
 * perhaps favor CRDTs (for multiple writers)
 * may limit temporal ops via expirations
* SHOULD be associated with a persistent session/workspace concept
 * new session will essentially have a new set of session state objects
 * session may be associated with one or more *shared* spaces 
  * use case: IRC, multi-player games, multi-agent systems, etc.
  * develop a 'rooms' concept for sharing, publish/subscribe
 * web-apps are also observed within a session

I expect I'll want to provide different caps to different apps.

Different applications?

* normal web-applications
* console command streams
* iPython notebook-like apps
* live/reactive web-applications
* 'installed' RDP apps
* ???

My intuition is that I can cover most use-cases by staged programming:

* an application-builder is a word like `wikilon/foo`
* receives URL+Query parameters (or a subset of query parameters?)
 * e.g. for `/foo/URL?query...` we'll show the URL and query
 * might also provide the `/foo` if requested
* staged: builds an application to run, i.e. another block
* in first stage, has a stage-1 powerblock (distinct from stage2)
 * can ask for fairly specific stage-2 capabilities
 * can utilize URL and query parameters to help build app
 * can reflect on the Wikilon dictionary (session specific!)
* second stage is limited to capabilities requested in first stage
* actual runtime applies *transparently!* within a given user-session
* to support cross-compile???
 * hide the web-appy parts (cookies, URLs, JavaScript, DOM, AJAX, etc.)
 * focus on building semantic elements (links, displays, widgets, state, etc.)


