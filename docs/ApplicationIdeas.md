
# Ideas for Hosted Applications

I have a lot of ideas about some [applications](ApplicationModel.md) to develop. Some are only half-baked.

## REPL

A [claw-based](EditingAO.md) REPL is an obvious option. It's also a very direct fit for our command pattern dictionary objects. 

## Functional Forums

Given a purely functional REPL, it seems a natural extension is to model a branching REPL structure, rely on reverse lookups to find the 'end' of each 'thread'.

## Diagrams or Turtle Graphics

An early approach to constructive graphics would be very fun. More importantly, it makes the system more approachable and explainable. It would also fit nicely with form-based controls.

## iPython Notebook

Basically... a REPL with constructive graphics?

## Edit Sessions

Some sort of CSCW model with edit sessions. Users track their own activity in the dictionary for easy records and display. This is easily shared between users, subject to import/export, and also serves as a nice record for historical archives and studies. Sessions would be "editable views" of a sort, since we need to peek in to see which words should be part of the session. 

As functions, sessions could model some metadata per word, forming a record (or sequence) of functions. Alternatively, modeling something like an FBP canvas of editable boxes and arrows could be cool as a basis for sessions, in which case session function would construct the function described by its diagram. We could model different types of sessions for different types of IDEs

## Command Shell

Like a REPL, but with more interaction by an external agent to support effects.

## Music Models

Some ability to model music and play it via the browser would be nice. 





