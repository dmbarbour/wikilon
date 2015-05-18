# URL Designs for Wikilon

Apparently, URL design is supposed to be one of the FIRST things I do. Oops! (Live and learn, I guess.) Or maybe not - HATEOS and RESTful design fellows argue URLs themselves aren't very relevant. But, if URLs aren't relevant from the REST perspective, it shouldn't hurt to apply advice from the UX perspective. Last night I trawled several 'URL design' websites to get some advice. The primary advice seems to be:

* find the domain-model nouns
* /collection/element organization
* keep it simple
* avoid verbs where feasible, use HTTP verbs instead
* avoid 'private' URLs like `example.com/settings` dependent on user
* use blacklist if toplevel is user-defined
* URLs are for humans, avoid hashcode names
* APIs should have verion numbers (?) but see [Content-Type](ContentType.md).
* URLs should be stable, good for bookmarking

I'm not fond of the 'blacklist' idea, so I'll instead leave the toplevel to administration. As far as URL hashcodes go: I can probably avoid them for the normal user experience, but I might still favor opaque URLs for machines, session models, etc.. Of course, in these cases, the URL should be none of the user's business and unhackable. Anyhow, I spent some time working through the advice from people vastly more experienced in this subject than myself.

Here's a rough sketch for what I'm thinking at this time:

        /d/                     collection of dictionaries
        /d/dictName/            individual dictionaries
        /d/dictName/w/          collection of words in dictionary
        /d/dictName/w/foo       individual word in dictionary
        /d/dictName/issues      maybe associate content with dictionaries

        /u/                     collection of users
        /u/userName             user home-page
        /u/userName/profile
        /u/userName/memberships     ... etc.   
        /u/userName/activity
        /u/userName/trophies
        /u/userName/blog

        /about
        /news
        /blog

        /admin
        /admin/logs
        /admin/size
        /login

        /m/                     collection of hosted machines
        /m/machineName          (shall we support nicknames?)
        /dht/                   relationships to distributed hashtable routing

        (miscellaneous ideas?)
        /ws/opaqueWebSocketId
        /s/opaqueSessionId
        /c/capabilityId

At this point, I'm still not entirely certain what all services I'll be providing. I'd like to support something like CodeBubble desktops, CSCW, mobility between devices. That might require new sharable resources. I will likely need to model pull requests and *expected* future relationships (e.g. this branch over here might be pulled) to help users track potential future conflicts and better cooperate. Issues seem like something to model carefully or even model *within* each dictionary (such that issues lists are easily exported and shared). 

## Security

I'd like to favor capability-based security even in the web interfaces. However, it isn't clear to me how to achieve this without mangling most of the URIs. I think capabilities are probably not a very good fit for how most users will use capabilities. 

Maybe I should stick with user logins to start, but add the ability to create capabilities to specific resources under a given authority. In particular, something like IDE sessions might make decent capabilities, as might web-sockets or bindings to machines.

For now, I should probably at least use 'digest' based authentication for users. (I'd really hate to use basic auth.)

## Subdomains?

An interesting possibility is to leverage subdomains for routing, e.g. such that we can use `machineId.example.com` to route to a particular abstract virtual machine. However, setting this up seems to be painful, and I'm not sure the effort is worthwhile. If the need arises, I'll look into this in the future. Meanwhile, routing to machines via the URI is more immediately useful.

## Naming AVMs

I'd like to consider `NameCoin` or similar cooperative decentralized models integration as a basis for giving AVMs a common name. I'd prefer this to use of 'pet names'.

## Service Ideas

Twitter integration might be neat if it means I can receive and answer requests.

# Priorities

Dealing with all the different HTTP methods and Accept modes and so on seems to be very painful, but is something that can accumulate over time. For now, I should probably aim to have just enough to get some useful work done, then grow the feature set as needed. What has the highest priority?

* import and export of a dictionary as a file 
 * this makes it easier to export content then reset Wikilon
 * can begin editing via external dictionary files

* interfaces to edit and view words within a dictionary
 * view the raw definiton ABC code
 * view the structural of the definition
 * view the compiled ABC code
  * view annotated compiled ABC code
 * edit the raw definition ABC code
 * edit the structure of the definition



* add words to or edit a dictionary
* export a dictionary
* evaluation of expressions
* automatic zero-button testing, typechecks 
* track recent changes (and change efforts? patches?) for a dictionary
* track type failures, test failures, etc..
* simple compilation of words to JavaScript?
* development of 'dictionary apps': 
 * apps having no dependencies outside of a dictionary
 * notebooks, spreadsheets, wikis, calculators, small games
 * may be able to update dictionary (dict as state, dataflow)
 * may have some client-side state in browser (browser AVMs? React?)

* Users should have logs for items updated 
 * logs contain enough information to track abuse
 * simple exponential decay model of some sort
* Dictionary should have log for recent updates
 * ability to query which words are updated and when
 * ability to 'slice' time on these updates
 * ability to subscribe to log as a web socket
 * includes whodunnit information
* 


## 







