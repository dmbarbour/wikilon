
Kyle Blake has developed abcc, which does some typechecking based on an infinite terms model.

However, I'd like also to look into Bidirectional Typechecking.

I could get something akin to nominative types by simply assuming a given word should have the same type wherever it is used (unless indicated otherwise). And type assertions could thus be modeled in the dictionary, e.g. having `type:A foo type:B` in the dictionary, we effectively assert that given type:A and processing by foo, we return type:B.
