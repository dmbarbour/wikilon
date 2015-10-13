
I'd like to try developing a FUSE filesystem adapter for Awelon project and Wikilon.

Different files would correspond to different views, e.g. allowing me to view a word's ABC code or a Claw view or a compiled view. We might also support [sessions](Sessions.md), enabling editing of multiple words within a file or directory.

I could probably model this above HTTP, such that the only argument needed is a URL for the Wikilon server (and maybe username, password). I will need to ensure Wikilon provides a lot of appropriate APIs for this adapter.
