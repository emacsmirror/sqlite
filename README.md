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

;; typical usage involves three functions
(let ((db (sqlite-init "~/mydb.sqlite")))  ; open connection
  (unwind-protect  ; perform body, return value, then clean up
      (let ((res (sqlite-query db "SELECT * FROM persona")))
        ;; Make more queries...  more calculations ...
        ;; ...
        res)  ; the return value
    (sqlite-bye db))  ; close connection
)
```

# Licence

This work is under the GNU GPLv3 licence.

# Happy Coding!
