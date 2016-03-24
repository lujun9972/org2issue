(require 'gh)
(require 'gh-issues)
(require 'ox-gfm)

(defvar org2issue-user "lujun9972"
  "")

(defvar org2issue-blog-repo "lujun9972.github.com"
  "")

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
  ""
  (let ((tags (org2issue--read-org-option "TAGS")))
    (when tags
      (apply #'vector (split-string tags)))))

(defun org2issue ()
  (interactive)
  (let* ((api (gh-issues-api "api"))
	 (tags (org2issue--get-tags))
         (title (org2issue--get-title))
         (body (org-export-as 'gfm))
         (issue (make-instance 'gh-issues-issue
                               :title title
                               :body body
                               :labels tags)))
    (gh-issues-issue-new api org2issue-user org2issue-blog-repo issue)))

(provide 'org2issue)
