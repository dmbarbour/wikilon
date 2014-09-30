
I plan to leverage WebSockets for the more reactive applications. But I think it might be worthwhile to model *logical* sockets distinctly from the actual implementation layer. Properties for logical sockets:

* *transactional* - instead of 'bytes' as the atomic unit, we have 'paragraphs'. A paragraph may consist of multiple sentences, i.e. multiple logical operations. A paragraph is executed transactionally. Multiple paragraphs can be composed and executed transactionally.

* *transport layer independent* - a logical socket is not directly related to the transport layer, and can be implemented over multiple transport concepts. Wikilon should implement at least two: sockets over HTTP PUT and GET (e.g. using `PUT /ws/socketId/paragraphNumber`), and also via W3C websockets (though an N:1 relationship might be useful).

* *resumable* - a logical socket is persistent. We can recover from disruption. 

* *capability secure* - sockets will be uniquely identified by a large random number (perhaps an HMAC). 

For the most part, the *content* of these sockets shall be [compressed](Compression.md) ABC streams. That is, decompression of the ABC stream will be explicit (perhaps implemented in JavaScript), but transport-layer decryption will be separate.

I like the idea of sessions being *transferrable*. But I'm thinking it might be better to model that not as transferring sockets, but rather as transferring a higher level session concept from one device to another. Mostly orthogonal to sockets.
