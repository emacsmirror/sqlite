;;; sqlite-test.el --- Tests for sqlite.el -*- lexical-binding: t; -*-

;; Copyright 2018 cnngimenez
;;
;; Author: cnngimenez
;; Version: 1.1
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

;; These are some tests for the sqlite.el package.

;;; Code:

(require 'sqlite)
(save-excursion
  (get-buffer-create sqlite-output-buffer))

(ert-deftest sqlite-test-parse-line-test ()
  (with-temp-buffer
    (insert "1|\"test city\"|\"test type\"|20180612T19:05:00
2|\"test city\"|\"test type\"|20180612T19:05:00
3|Test|\"1234\"|2018-06-14T17:36:22.524Z
4|\"test city\"|\"test type\"|20180612T19:05:00
\n")
    (goto-char 1)
    (should
     (equal
      (sqlite-parse-line)
      '("1" "\"test city\"" "\"test type\"" "20180612T19:05:00")))
    (forward-line)
    (should
     (equal
      (sqlite-parse-line)
      '("2" "\"test city\"" "\"test type\"" "20180612T19:05:00")))))

(ert-deftest sqlite-test-parse-result-test ()
  (should
   (with-temp-buffer
     (insert "1|\"test city\"|\"test type\"|20180612T19:05:00
2|\"test city\"|\"test type\"|20180612T19:05:00
3|Test|\"1234\"|2018-06-14T17:36:22.524Z
4|\"test city\"|\"test type\"|20180612T19:05:00
")
     (equal
      (sqlite-parse-result)
      '(("1" "\"test city\"" "\"test type\"" "20180612T19:05:00")
        ("2" "\"test city\"" "\"test type\"" "20180612T19:05:00")
        ("3" "Test" "\"1234\"" "2018-06-14T17:36:22.524Z")
        ("4" "\"test city\"" "\"test type\"" "20180612T19:05:00")))))
  (should  ;; empty field is converted to nil
   (equal
    (with-temp-buffer (insert "id|value\n1|t\n2|") (sqlite-parse-result))
    '(("id" "value") ("1" "t") ("2" nil))))
  (should  ;; fields are trimmed
   (equal
    (with-temp-buffer (insert " id | value \n 1 | t \n 2 | ") (sqlite-parse-result))
    '(("id" "value") ("1" "t") ("2" nil))))
  (should  ;; also "middle" fields are trimmed
   (equal
    (with-temp-buffer (insert " id | value1 | value2 \n 1 | t | \n 2 |   | t") (sqlite-parse-result))
    '(("id" "value1" "value2") ("1" "t" nil) ("2" nil "t")))))

(provide 'sqlite-test)
  
;;; sqlite-test.el ends here
