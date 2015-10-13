
The idea of an 'edit session' is that I can:

* edit multiple words together
* tune which I'm editing
* tune various view parameters
* create, fork, save, delete sessions

To support edit sessions, Wikilon will need some means of representing and storing them. I have two options here:

1. create a stateful sessions model on the Wikilon side
2. dictionary application, each session represented as a word

I favor the latter option. Tracking sessions in the dictionary will preserve sessions for import/export and will ensure sessions use the same historical preservation model as everything else. Access to historical sessions could be useful for data mining later on, etc.. 

As a dictionary application, each 'session' should ideally also be meaningful as a function. Some options:

1. treat any arbitrary word as a session by grabbing the words it invokes.
2. model a record of functions with a sub-word structure for the session.
3. annotate words in a session with a session identifier, pollutes words.

The first option is problematic because it's difficult to control which words are exposed in a session, and difficult to provide view metadata (ordered views, arrangement on a canvas). The last option is problematic because it pollutes the dictionary, adding annotations to non-session words. 

The second option is relatively promising. Something like a claw expansion:

        \{&category:session}
        SESSION
            SESSION-ELEM [foo] "foo" SESSION-ELEM-END
            SESSION-ELEM [bar] "bar" SESSION-ELEM-END
        SESSION-END

I'll need to tune the details. Maybe add support for hierarchical sessions, session inheritance, namespaces. Rather than one size fits all, try distinct session models for different IDE types (e.g. session presented as a page or file vs. session presented as a canvas). A session's "meaning" might primarily involve constructing a record or association list of functions. This record would then be useful for modeling first-class modules, esp. if taken together with high levels of static partial evaluation.
