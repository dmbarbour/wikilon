
I'm not especially keen on the plugins route for Wikilon. I really don't want to pollute the AO dictionaries with code for other languages (neither Haskell nor JavaScript). I'd much rather compile ABC directly to applications on the target with a set of semantics-level capabilities (e.g. widgets and comms, not HTML and AJAX). 

However, it may prove an effective capability to extend Wikilon on-the-fly:

* AO code constructs Haskell code that meets some Plugin API or type
* the Haskell code is then compiled and loaded as a Haskell plugin
* the plugin effect is made available as a generated capability

This might be useful in special cases, e.g. to extend Wikilon with GPGPU or FPGA computations, or to integrate Wikilon instances with sound systems or robotics systems. OTOH, it might be better just to extend Wikilon directly with a few special cases, and thus to *avoid pfocus on developing AO code for other layers. 

The possibility exists. For now, I'm just recording it.

Obviously, this would be presented as a capability in its own right, available to the master session and delegable to client sessions.

(A related possibility to plugins is to generate and spin up a new server-side process, and interact with that. This approach might work pretty well if I can guarantee a safe program, e.g. via a limited intermediate language.)
