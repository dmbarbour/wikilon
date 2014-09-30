
With multiple users, I should be concerned with:

* User Display Preferences
* Private User Data
* Sessions and Workspaces
* Password Recovery and Two Factor Authentication
* Delegations and Revocations
* Authorities and Privileges

.. and perhaps more.

A curious issue is how users and their state should interact with transactions, multi-user transactions. 

One possible interaction:

* a user signs up with an e-mail address
* by e-mail, the user receives a capability URL
* the user is brought to a page which sets a session cookie for that user
* user can review active transactions, start a new transaction, or continue an existing one
* user can also delegate authority to another user, review delegated authorities, and revoke authorities

Regarding new sessions or transactions:

* each new session needs a new workspace, i.e. to avoid 'viewing' anything
* a new session gets a capability identifier
* users can bring other people in specifically for a transaction
* the commit/abort authority must be specifically delegated





