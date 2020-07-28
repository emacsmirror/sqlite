;;; sqlite.el --- use sqlite via elisp
;;
;; Filename: sqlite.el
;; Description:
;; Author: Christian Giménez
;; Maintainer:
;; Created: mié feb 13 11:12:31 2013 (-0300)
;; Version: 1.0
;; Last-Updated: dom feb 24 20:50:48 2013 (-0300)
;;           By: Christian
;;     Update #: 105
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl', `subr-x'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 26-Jul-2020    Mario Frasca
;;    Added subr-x dependency, worked at waiting for command completion.
;; 21-Dec-2013    Raimon Grau
;;    Improved style and change chomp to sqlite-chomp to avoid name collisions
;; 24-Feb-2013    Christian
;;    Last-Updated: dom feb 24 20:49:45 2013 (-0300) #103 (Christian)
;;    `add-to-list' doesn't let you push new elements if they are already. We don't want this behaviour.
;; 24-Feb-2013    Christian
;;    Last-Updated: dom feb 24 20:30:10 2013 (-0300) #95 (Christian)
;;    There is a problem in the regexp `sqlite-regexp-sqlite-command'. It doesn't test the "." at the begining of the string.
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 19:22:25 2013 (-0300) #93 (Christian)
;;    Some problems with the first query are now solved!
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 18:39:10 2013 (-0300) #84 (Christian)
;;    `sqlite-init' has filename expansion. You don't need to write the absolute path of the file.
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 18:27:16 2013 (-0300) #78 (Christian)
;;    `sqlite-query' adds  ";" at the end of the query for you.
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 18:21:06 2013 (-0300) #76 (Christian)
;;    Now varios process works. Checking SQLite errors when querying works.
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 16:09:48 2013 (-0300) #41 (Christian)
;;    Adding a list for descriptor, process buffers and its files(`sqlite-process-alist').
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 02:21:02 2013 (-0300) #25 (Christian)
;;    Adding support for making various sqlite process.
;; 14-Feb-2013    Christian
;;    Last-Updated: jue feb 14 01:28:15 2013 (-0300) #4 (Christian)
;;    Added `sqlite-bye' function for finishing the sqlite process.
;; 14-Feb-2013    Christian
;;    Last-Updated: mié feb 13 11:18:46 2013 (-0300) #3 (Christian)
;;    Now output buffer doesn't appear. Sqlite connects and still works.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; For usage and explanations see http://www.emacswiki.org/emacs/SQLite-el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'cl))

;; Adapted from http://mysite.verizon.net/mbcladwell/sqlite.html#connect

(defvar sqlite-program "sqlite3" "Name the SQLite3 executable.  If not in $PATH, please specify full path.")

(defvar sqlite-output-buffer "*sqlite-output*" "Name of the SQLite output buffer.")

(defvar sqlite-include-headers nil "If non-nil, include headers in query results.")

;; Process list storing and manipulation
;; ----------------------------------------

(defvar sqlite-process-plist nil
  "A plist that associates descriptor to buffer process and databse file.
Example:
'(1 (\"*sqlite-process1*\" \"~/mydb1.sqlite\")
  2 (\"*sqlite-process2*\" \"~/databases/mydb2.sqlite\"))")

(defvar sqlite-descriptor-counter 0
  "This is a counter that adds 1 for each sqlite process opened. Used for referencing each sqlite process uniquely.")

(defun sqlite-register-descriptor (descriptor buffer file)
  "Register the descriptor with the buffer given adding it into `sqlite-process-plist'."
  (setq sqlite-process-plist (plist-put sqlite-process-plist descriptor `(,buffer ,file))))

(defun sqlite-descriptor-buffer (descriptor)
  "Return the buffer associated to the DESCRIPTOR"
  (car (plist-get sqlite-process-plist descriptor)))

(defun sqlite-descriptor-database (descriptor)
  "Return the database file associated to the DESCRIPTOR"
  (cadr (plist-get sqlite-process-plist descriptor)))

(defun sqlite-unregister-descriptor (descriptor)
  "Remove the descriptor from the list of process buffers `sqlite-process-plist'."
  (setq sqlite-process-plist (plist-put sqlite-process-plist descriptor nil)))

;; ----------------------------------------

(defun sqlite-init (db-file)
  "Initialize sqlite interface opening the DB-FILE sqlite file.
This starts the process given by `sqlite-program' and prepares it
for queries.  Return the sqlite process descriptor, a unique id
that you can use to retrieve the process or send a query. "
  (let* ((db-file (expand-file-name db-file))
         (comint-use-prompt-regexp t)
         (comint-prompt-regexp "^\\(sqlite\\)?> ")
         (process-buffer (make-comint
                          (format "sqlite-process-%04d" sqlite-descriptor-counter)
                          sqlite-program nil db-file))
         (process (get-buffer-process process-buffer)))
    (unless process
      (error "Can't create new process"))
    (save-excursion
      (condition-case nil
          (set-buffer process-buffer)
        (error))
      (shell-mode))
    (sqlite-register-descriptor sqlite-descriptor-counter process-buffer db-file)
    (incf sqlite-descriptor-counter)
    (while (accept-process-output process 0.1))
    (comint-redirect-send-command-to-process ".mode list" sqlite-output-buffer process nil t)
    (comint-redirect-send-command-to-process ".separator |" sqlite-output-buffer process nil t)
    ;; configure whether headers are desired or not
    (comint-redirect-send-command-to-process (if sqlite-include-headers ".headers on" ".headers off")
                                             sqlite-output-buffer process nil t)
    (comint-redirect-send-command-to-process ".prompt \"> \"\"...> \"" sqlite-output-buffer process nil t)
    (while (accept-process-output process 0.1))
    (get-buffer-create sqlite-output-buffer))
  (1- sqlite-descriptor-counter))

(defun sqlite-bye (descriptor &optional noerror)
  "Finish the sqlite process sending the \".quit\" command.
Returns t if everything is fine.
nil if the DESCRIPTOR points to a non-existent process buffer.
If NOERROR is t, then will not signal an error when the DESCRIPTOR is not registered."
  (let* ((comint-use-prompt-regexp t)
         (comint-prompt-regexp "^\\(sqlite\\)?>")
         (process-buffer (sqlite-descriptor-buffer descriptor))
         (process (get-buffer-process process-buffer)))
    (if (get-buffer-process process-buffer)
        (progn ;; Process buffer exists... unregister it
          (set-process-query-on-exit-flag (get-process process) nil)
          (comint-redirect-send-command-to-process ".quit" sqlite-output-buffer process nil t)
          (while (accept-process-output process 0.1))
          (sqlite-unregister-descriptor descriptor)
          (kill-buffer process-buffer)
          t)
      (progn
        (sqlite-unregister-descriptor descriptor) ;; We unregister the descriptor nevertheless
        (unless noerror
          (error "Process buffer doesn't exists for that descriptor"))
        nil))))

(defun sqlite-parse-line ()
  "Parse result line at point, returning the list column values.
Empty is replaced with nil."
  (let* ((line (string-trim (thing-at-point 'line))))
    (mapcar (lambda (item)
              (and (not (equal item ""))
                   item))
            (mapcar #'string-trim (split-string line "|")))))

(defun sqlite-parse-result ()
  "Parse the lines in the current buffer into a list of lists.
This is intended to be called with *sqlite-output* being the
current buffer, but it's up to the caller to make sure, this
function will not enforce it.  The first line can be a header
line, depending on the value of sqlite-include-headers.  The
result looks like this: (header-list row1-list row2-list
row3-list) "
  (let ((num-lines (count-lines (goto-char (point-min)) (point-max)))
        (results-rows nil))
    (if (sqlite-error-line) ;; Check if it is an error line
        (error (concat "SQLite process error:" (string-trim (buffer-string)))))
    (dotimes (counter num-lines)
      (push (sqlite-parse-line) results-rows)
      (forward-line))
    (nreverse results-rows)))

(defconst sqlite-regexp-error "Error:\\(.*\\)$"
  "This regexp must match the error return of SQLite. There must be a parenthesis surrounding the error message for matching it with:
    `match-string' 1
This is used for `sqlite-check-errors' for raising errors with messages.")

(defun sqlite-error-line ()
  "Return t if the thing-at-point matches `sqlite-regexp-error'. Else, return nil."
  (if (string-match sqlite-regexp-error (string-trim (thing-at-point 'line)))
      t
    nil))

(defvar sqlite-regexp-sqlite-command "^\\..*"
  "This regexp must match an SQLite command. This is used for identifying which is an SQL command and which is a proper SQLite command.")

(defun sqlite-prepare-query (sql-command)
  "Add a query terminator to SQL-COMMAND if necessary
SQLite commands start with \".\" and don't need terminator."
  (cond ((string-match "^\\." sql-command)
         sql-command)
        ((string-match ";$" sql-command)
         sql-command)
        (t (concat sql-command ";"))))

(defun sqlite-query (descriptor sql-command)
  "Send a query to the SQLite process and return the result.
DESCRIPTOR is the Sqlite instance descriptor given by `sqlite-init'.
Return list of lists, as
    (header-list row1-list row2-list row3-list)
"
  (let* ((comint-use-prompt-regexp t)
         (comint-prompt-regexp "^\\(sqlite\\)?>")
         (process-buffer (sqlite-descriptor-buffer descriptor))
         (process (get-buffer-process process-buffer)))
    (unless process
      (error "SQLite process buffer doesn't exist!"))
    (with-current-buffer sqlite-output-buffer
      (erase-buffer)
      (comint-redirect-send-command-to-process
       (sqlite-prepare-query sql-command)
       sqlite-output-buffer process nil t)
      (while (accept-process-output process 0.1))
      (sqlite-parse-result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sqlite.el ends here

(provide 'sqlite)
