sqlite.el
=========

SQLite Interface for EmasLisp


# From EmacsWiki

This code was retrieved from the EmacsWiki at Dicember 26, 2013.
At that point was two developers:

* @cnngimenez
* @kidd (Raimon Grau)

# Manual and Usage

Take a look at the [SQLite EmacsWiki page](http://www.emacswiki.org/emacs/SQLite-el).

An example for querying the database:

```elisp
(require 'sqlite)
    
;; Open the SQLite DB
(let ((descriptor (sqlite-init "~/mydb.sqlite")))
    
	;; Query something. Can be SQL or SQLite commands like ".tables".
    (setq res (sqlite-query descriptor "SELECT * FROM persona"))
	
	;; Make more queries...
	;; ...
	
	;; Close SQLite subprocess
    (sqlite-bye descriptor)
	
    res
)
```

# Licence

This work is under the GNU GPLv3 licence.

# Happy Coding!
