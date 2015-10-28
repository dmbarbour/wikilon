
Wikilon Events and Views
========================

So far (June 2015) I've used a rather ad-hoc approach for Wikilon to provide views and events via WAI. However, this probably won't work very well when I start introducing security concerns, logging and history tracking, cache invalidation, multiple pages with common views, etc.. I need a more robust approach.

One option is to push a lot more of the dictionary metadata into the dictionaries directly, allowing a useful sort of explicit reflection on the dictionary's history and state. However, I'm not sure this is a good idea.

What I would like to do is divide the responsibilities more clearly:

* one module that supports the stateful model and update events 
* one module that supports flexible, composable snapshot views of Wikilon
 * may need to integrate with events in some way, i.e. for mutable views
* the WAI module that presents views over HTTP
* perhaps a separate AVM module that presents Wikilon over the AVM network

I could push a lot more metadata into the dictionaries, including the event logs and such. 

It might take a week or two to perform this refactoring, if I focus on just it.

(haha, a week?! It's going to take a couple months.)

IDEAS:

* need to model a web framework
 * background computations
 * views updated via WebSockets
* maybe build a proper web-app model
 * something I can model primarily in ABC
 * semantic CSS and web content instead of HTML
 * 

* updateable views with WebSockets, ideally should be implicit? 
 * these are not yet critical, though. 
* something like MFlow, modeling multi-page forms with validation?
* focus on something like RDP or lenses with implicit reactivity
* better support for access to values (lists, etc.) as ABC
 * might combine well with reactive update streams

* support for web-apps from dictionary
 * an abstract web-api with support for pre-rendering of initial frame
 * an abstract web-app model with support for updating the dictionary 
 * support for claw-like views and indicating what is cached and precomputed 
 * web-app state (e.g. cookies, multi-page forms) represented as ABC values

* compilation to java-script
 * bi-directional; preserve ABC for composition, recovery
 * maybe support claw-views of javascript, too (keep dict structure?)

* support for communicating and updating (e.g. via web-sockets) values in ABC
 * maybe model AVM per web page? could support updates to functions, too.
 * but minimally, apply streaming updates of commands to page model state?

A heavy focus on pushing web-apps into the dictionary seems like a *very good thing*. It potentially allows me to escape Haskell code a lot more quickly.

I should really use or take inspiration from the APIs of Yesod or Snap or similar. The authors of those have already spent a lot of time thinking about these problems. Though, I don't like a lot of what they're doing with session cookies and similar, I might be able to model state my own way.

Routing should probably be based on some sort of types-driven references with a notion of locality. Maybe a record of type→queryType→URL functions? Something I can also model within ABC abstract pages, too.

Stylesheets... should probably be more abstract in nature, oriented around abstract spans (or divs) and classes. This should be oriented around the same abstract API I want for modeling web pages from Wikilon. I can then 'compile' the abstract style model into HTML CSS (and cache this result).

Should I try rel="alternate stylesheet" to allow switching between dark and light styles?

Can I provide a good 'front page' based on active editing sessions?





