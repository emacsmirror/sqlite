;;; sqlite.el --- Use sqlite via ELisp  -*- lexical-binding: t; -*-
;;

;; Copyright 2020 cnngimenez
;;
;; Author: cnngimenez
;; Maintainer: cnngimenez
;; Description: SQLite interface for ELisp
;; Version: 1.0
;; Keywords: extensions, lisp, sqlite
;; URL: https://gitlab.com/cnngimenez/sqlite.el
;; Package-Requires: ((emacs "24.5"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; For usage and explanations see https://www.emacswiki.org/emacs/SQLite-el.
;;

;;; Code:


;; Adapted from:
;; https://web.archive.org/web/*/http://mysite.verizon.net/mbcladwell/sqlite.html#connect

(defvar sqlite-program "sqlite3" "Full path name of the SQLITE executable.")

(defvar sqlite-output-buffer "*sqlite-output*" "Name of the SQLite output buffer.")

(defvar sqlite-process-buffer "sqlite-process"
  "*Name of the SQLITE process buffer.  This is where SQL commands are sent.")

(defvar sqlite-include-headers nil "If non-nil, include headers in query results.")

;; Process list storing and manipulation
;; ----------------------------------------

(defvar sqlite-process-alist nil
  "Contains each descriptor with their buffers.
An alist that contains each descriptor with the corresponding buffers
process and the file opened.
Example:
 (setq sqlite-process-alist
  '(
      (1 . '(\"*sqlite-process1*\" \"~/mydb1.sqlite\"))
      (2 . '(\"*sqlite-process2*\" \"~/databases/mydb2.sqlite\"))

  ))")

(defvar sqlite-descriptor-counter 0
  "This is a counter that adds 1 for each sqlite process opened.
Used for referencing each sqlite process uniquely.")

(defun sqlite-register-descriptor (descriptor buffer file)
  "Register DESCRIPTOR with the buffer BUFFER and FILE.

Registering is adding the proper association into `sqlite-process-alist'."
  (add-to-list 'sqlite-process-alist (cons descriptor (list (list buffer file)))))

(defun sqlite-descriptor-buffer (descriptor)
  "Return the buffer asociated to the given DESCRIPTOR."
  (car (cadr (assoc descriptor sqlite-process-alist))))

(defun sqlite-unregister-descriptor (descriptor)
  "Remove DESCRIPTOR from the list of process buffers `sqlite-process-alist'."
  (setq sqlite-process-alist (assq-delete-all descriptor sqlite-process-alist)))

;; ----------------------------------------


(defun sqlite-init (db-file)
  "Initialize sqlite interface opening the DB-FILE sqlite file.
This start the process given by `sqlite-program' and prepare it for queries.

Returns the sqlite process descriptor, a unique id that you can use to retrieve
the process or send a query."
  (let ((process-buffer (concat "sqlite-process" (number-to-string sqlite-descriptor-counter))) ; name of the process buffer
        )
    (setq db-file (expand-file-name db-file))

    (apply #'make-comint
           process-buffer
           sqlite-program  nil `(,db-file ))

    (setq process-buffer (concat "*" process-buffer "*")) ;; Asteriscs automatically added by `make-comint'

    (sqlite-register-descriptor sqlite-descriptor-counter process-buffer db-file)
    (setq sqlite-descriptor-counter (+ sqlite-descriptor-counter 1))

    (accept-process-output (get-buffer-process process-buffer)) ;; Wait for the results and for first echo
    ;; We use CSV for parsing results.
    (comint-redirect-send-command-to-process ".mode csv" sqlite-output-buffer (get-buffer-process process-buffer) nil t)
    (comint-redirect-send-command-to-process ".separator |" sqlite-output-buffer (get-buffer-process process-buffer) nil t)
    ;; We remove prompt.
    (comint-redirect-send-command-to-process ".prompt \"\"" sqlite-output-buffer (get-buffer-process process-buffer) nil t)
    (accept-process-output (get-buffer-process process-buffer) 1) ;; Wait for the results and for first echo
    ;; Add headers.
    (comint-redirect-send-command-to-process (if sqlite-include-headers ".headers on" ".headers off")
                                             sqlite-output-buffer (get-buffer-process process-buffer) nil t)
    (accept-process-output (get-buffer-process process-buffer) 1) ;; Wait for the results and for first echo

    (get-buffer-create sqlite-output-buffer))

  (- sqlite-descriptor-counter 1))

(defun sqlite-bye (descriptor &optional noerror)
  "Finish the sqlite process sending the \".quit\" command.

Returns t if everything is fine.
nil if the DESCRIPTOR points to a non-existent process buffer.

If NOERROR is t, then will not signal an error when the DESCRIPTOR is not registered."
  (let ((process-buffer (sqlite-descriptor-buffer descriptor)))
    (if (get-buffer-process process-buffer)
        (progn ;; Process buffer exists... unregister it
          (set-process-query-on-exit-flag (get-process (get-buffer-process process-buffer)) nil)
          (comint-redirect-send-command-to-process ".quit" sqlite-output-buffer (get-buffer-process process-buffer) nil t)
          (sqlite-unregister-descriptor descriptor)
          (kill-buffer process-buffer)
          t)
      (progn
        (sqlite-unregister-descriptor descriptor) ;; We unregister the descriptor nevertheless
        (unless noerror
          (error "Process buffer doesn't exists for that descriptor"))
        nil))))

(defun sqlite-take-next-value (line)
  "Take one value up to a \",\" from LINE.  This considers the character \".
Return a list with two elements: (value rest-of-line)"
  (if (equal line "")
      nil
    (let ((vals (split-string line "|")))
      (list (car vals) (mapconcat #'identity (cdr vals) "|")))))

(defun sqlite-parse-line ()
  "Take one line from the current buffer and parse it.
Return a list of elements per column."
  (let ((line (sqlite-chomp (thing-at-point 'line)))
        (parsed nil)
        (next t))
    (while next
      (setq next (sqlite-take-next-value line))
      (when next
	(setq parsed (append parsed (car next)))
        ;; (add-to-list 'parsed (car next) t 'ignore)
        (setq line (cadr next))))
    parsed))

(defun sqlite-parse-result ()
  "Parse the results to create a list of header-list plus rows-lists.

Result: (header-list row1-list row2-list row3-list)"
  (let*  ((begin (goto-char (point-min)))		;4
          (end (goto-char (point-max)))
          (num-lines (count-lines begin end))
          (counter 0)
          (results-rows ()))
    (goto-char (point-min))
    (if (sqlite-error-line) ;; Check if it is an error line
        (error (concat "SQLite process error:" (sqlite-chomp (buffer-string)))))
    ;; no error, as Fredie Mercury said: "show must go on..."
    (while ( < counter num-lines)
      (setq results-rows (append results-rows (list (sqlite-parse-line))))
      ;; (add-to-list 'results-rows (sqlite-parse-line) t 'ignore)
      (forward-line)
      (setq counter (+ 1 counter)))
    (car `(,results-rows))))

(defconst sqlite-regexp-error "Error:\\(.*\\)$"
  "Regexp used to match the error returned by SQLite process.
There must be a parenthesis surrounding the error message for matching it with:
    `match-string' 1
This is used for `sqlite-check-errors' for raising errors with messages.")

(defun sqlite-error-line ()
  "Return t if the current line match an error.
Return t if the `sqlite-output-buffer' buffer match the `sqlite-regexp-error'.
Else, return nil."
  (with-current-buffer sqlite-output-buffer
    (if (string-match sqlite-regexp-error (sqlite-chomp (thing-at-point 'line)))
        t
      nil)))

(defvar sqlite-regexp-sqlite-command "^\\..*"
  "Regexp that match an SQLite command.
This is used for identifying which is an SQL command and which is a proper
SQLite command.")

(defun sqlite-prepare-query (sql-command)
  "Return the query prepared.

If the query start with \".\" means that is a SQLite command, so we don't add a
\";\" at the end; else, we add a \";\" beacuse it is an SQL command.
Remember: two \";\" has no effect in SQLite! :)

SQL-COMMAND is a the string with the SQL to be passed to the SQLite process."
  (if (string-match sqlite-regexp-sqlite-command sql-command)
      sql-command
    (concat sql-command ";")))

(defun sqlite-query (descriptor sql-command)
  "Send a query to the SQLite process and return the result.

DESCRIPTOR is the Sqlite instance descriptor given by `sqlite-init'.
SQL-COMMAND is a string with the the SQL command.

Result:
The result is a \"table\" like the following:
\(header-list row1-list row2-list row3-list)"
  (let ((process-buffer (sqlite-descriptor-buffer descriptor)))
    (unless (get-buffer-process process-buffer)
      (error "SQLite process buffer doesn't exist!"))
    (with-current-buffer sqlite-output-buffer
      (erase-buffer)

      (comint-redirect-send-command-to-process
       (sqlite-prepare-query sql-command) ;; Sometimes developers don't want to add a ";" in the query...
       sqlite-output-buffer (get-buffer-process process-buffer) nil t)
      (accept-process-output (get-buffer-process process-buffer) 1)  ;need to wait to obtain results
      (let ((result (sqlite-parse-result))) ;; we want to return the result! not the erase-buffer return value
        (erase-buffer)
        result))))

(defun sqlite-chomp (str)
  "Trim whitespace from string.
STR the string to be trimmed."
  (let ((s (if (symbolp str)(symbol-name str) str)))
    (save-excursion
      (while (and
              (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
              (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
        (setq s (replace-match "" t nil s)))
      (while (and
              (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
              (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
        (setq s (replace-match "" t nil s))))
    s))

(provide 'sqlite)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sqlite.el ends here


