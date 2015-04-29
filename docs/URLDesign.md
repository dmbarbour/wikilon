# URL Designs for Wikilon

Apparently, URL design is supposed to be one of the FIRST things I do. Oops! (Live and learn, I guess.)

Last night I trawled through several 'URL design' websites to get some advice. The primary advice seems to be:

* find the domain-model nouns
* /collection/element organization
* avoid verbs where feasible
* keep it simple (obviously)
* avoid 'private' URLs like `example.com/settings` dependent on user
* use blacklist if toplevel is user-defined
* URLs are for humans, avoid hashcode names
* avoid one-off URLs 
* APIs should have verion numbers

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

