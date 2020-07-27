;;; test.el --- 

;; Copyright 2018 poo
;;
;; Author: poo@juno
;; Version: $Id: test.el,v 0.0 2018/07/08 17:01:17 poo Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'test)

;;; Code:

(require 'sqlite)
(save-excursion
  (get-buffer-create sqlite-output-buffer))

(ert-deftest sqlite-parse-line-test ()
  (with-temp-buffer
    (insert "1|\"test city\"|\"test type\"|20180612T19:05:00
2|\"test city\"|\"test type\"|20180612T19:05:00
3|Test|\"1234\"|2018-06-14T17:36:22.524Z
4|\"test city\"|\"test type\"|20180612T19:05:00\n")
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

(ert-deftest sqlite-parse-result-test ()
  (should
   (with-temp-buffer
     (insert "1|\"test city\"|\"test type\"|20180612T19:05:00
2|\"test city\"|\"test type\"|20180612T19:05:00
3|Test|\"1234\"|2018-06-14T17:36:22.524Z
4|\"test city\"|\"test type\"|20180612T19:05:00")
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

(ert-deftest sqlite-prepare-query-test ()
  (should (equal (sqlite-prepare-query ".quit") ".quit"))
  (should (equal (sqlite-prepare-query "select * from dual;") "select * from dual;"))
  (should (equal (sqlite-prepare-query "select * from dual") "select * from dual;")))

;;; test.el ends here
