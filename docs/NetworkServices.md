
I'm pretty happy with the [network model](NetworkModel.md) developed so far. 

Of course, for AVMs to be useful, we must provide capabilities to bind real-world effects. 

Which services should be built into Wikilon?

* reflection services
 * access inner structure of values
 * possible typechecking services
 * access to dictionaries
* performance services (annotations?)
 * JIT or specialization for blocks? 
 * support for ABC resources?

* an equivalent of XMLHttpRequest
* publish web apps, apis, sockets, pages
* support for e-mail based services?
* support for AMQP or a related system?
* internet of things access?

* local service registries?
 * publish-subscribe
 * tuple spaces
 * demand monitor like services
* support for TCP or UDP outputs? security concerns.
* deployment of virtual machines to cloud services

In general, having less built into Wikilon is preferable, because it will require less runtime infrastructure for AVMs as virtual machines. OTOH, providing higher level services as capabilities is very convenient when getting started, reduces performance risk while we await a good compiler, and can still be supported via 'kernel' AVMs. 

It isn't too difficult to mix things up a bit, due to the capability based effects, and to gradually shift implementations from built-ins into a trusted kernel AVM. So what we might need to do is to allow admins to specify trusted AVMs that are given low-level capabilities.
