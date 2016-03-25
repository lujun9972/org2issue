;;; org2issue.el --- org to github issue based blog

;; Copyright (C) 2004-2016 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-03-24
;; Version: 0.1
;; Keywords: convenience, github, org
;; Package-Requires: ((org "8.0") (emacs "24.4") (ox-gfm "0.1") (gh "0.1"))
;; URL: https://github.com/lujun9972/org2issue

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; org2issue's code can be found here:
;;   http://github.com/lujun9972/org2issue

;;; Commentary:

;; org2issue is a little tool that use eww to preview current org-file when save automatically 

;; Quick start:

;; 1. specify ~org2issue-user~ as your github username
;; 2. specify ~org2issue-blog-repo~ as the blog repository name
;; 3. open the org file and execute =M-x org2issue=

;; BUGS
;; + It can't handle chinese properly in Emacs25.x
;; + It can't add issue labels.

;; To add issue labels. You have to redefine the method `gh-issues-issue-req-to-update` as below:
;; #+BEGIN_SRC emacs-lisp
;; (defmethod gh-issues-issue-req-to-update ((req gh-issues-issue))
;;   (let ((assignee (oref req assignee))
;;         (labels (oref req labels))
;;         (milestone (oref req milestone))
;;         (to-update `(("title" . ,(oref req title))
;;                      ("state" . ,(oref req state))
;;                      ("body" . ,(oref req body)))))

;;     (when labels (nconc to-update `(("labels" . ,(oref req labels) ))))
;;     (when milestone
;;       (nconc to-update `(("milestone" . ,(oref milestone number)))))
;;     (when assignee
;;       (nconc to-update `(("assignee" . ,(oref assignee login) ))))
;;     to-update))
;; #+END_SRC

;;; Code:
(require 'gh)
(require 'gh-issues)
(require 'ox-gfm)

(defgroup org2issue nil
  "org to github issue based blog")

(defcustom org2issue-user user-login-name
  "Github username"
  :group 'org2issue
  :type 'string)

(defcustom org2issue-blog-repo "blog"
  "The blog repository name"
  :group 'org2issue
  :type 'string)

(defun org2issue--read-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((case-fold-search t)
        (match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

(defun org2issue--get-title ()
  "Get the title of org file."
  (or (org2issue--read-org-option "TITLE")
      (file-name-sans-extension (buffer-name))))

(defun org2issue--get-tags ()
  "Get the tags of org file"
  (let ((tags (org2issue--read-org-option "TAGS")))
    (when tags
      (apply #'vector (split-string tags)))))

(defun org2issue--json-encode-string (string)
  "Patch for json.el in emacs25"
  (let ((l (length string))
        (start 0)
        res mb)
    ;; Only escape quotation mark, backslash and the control
    ;; characters U+0000 to U+001F (RFC 4627, ECMA-404).
    (while (setq mb (string-match "[\"\\[:cntrl:]]\\|\\cc" string start))
      (let* ((c (aref string mb))
             (special (rassq c json-special-chars)))
        (push (substring string start mb) res)
        (push (if special
                  ;; Special JSON character (\n, \r, etc.).
                  (string ?\\ (car special))
                ;; Fallback: UCS code point in \uNNNN form.
                (format "\\u%04x" c))
              res)
        (setq start (1+ mb))))
    (push (substring string start l) res)
    (push "\"" res)
    (apply #'concat "\"" (nreverse res))))

;;;###autoload
(defun org2issue ()
  (interactive)
  (let ((api (gh-issues-api "api"))
        (tags (org2issue--get-tags))
        (title (org2issue--get-title))
        (body (org-export-as 'gfm))
        response)
    (unwind-protect 
        (let ((issue (make-instance 'gh-issues-issue
                                    :title title
                                    :body body
                                    :labels tags)))
          (when (version<= "25.0" emacs-version)
            (advice-add 'json-encode-string :override #'org2issue--json-encode-string))
          (setq response (gh-issues-issue-new api org2issue-user org2issue-blog-repo issue)))
      (when (advice-member-p #'org2issue--json-encode-string 'json-encode-string)
        (advice-remove 'json-encode-string #'org2issue--json-encode-string)))
    (message "response: %s" (oref (oref response data) url))))

(provide 'org2issue)
;;; org2issue.el ends here

