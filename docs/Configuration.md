
# Wikilon Configuration

To make Wikilon a fully tailorable web service, I need the ability to add:

* tab icons (favoring PNG format)
* banner images
* styles and stylesheets

I would like for most configuration of Wikilon to occur through manipulation of a dictionary. It could be a dictionary simply named `config` or `master` or similar, though this might also be configurable. Interestingly, we might be able to bind our website to a dictionary in a manner such that it is just a 'dictionary app'. I.e. under *every* dictionary, we might be able to find something like:

        /d/dictName/wikilon/

And at this point we've got a new 'view' of wikilon, complete with its own CSS and tab icons and so on. This would make it easy to test multiple alternative sites, and would also simplify configuration of wikilon.

