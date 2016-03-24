(require 'gh)
(require 'gh-issues)
(require 'ox-gfm)

(defvar org2gibb-user "lujun9972"
  "")

(defvar org2gibb-blog-repo "lujun9972.github.com"
  "")

(defvar org2gibb-api (gh-issues-api "api")
  "")
(defun org2gibb--read-org-option (option)
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

(defun org2gibb--get-title ()
  "Get the title of org file."
  (or (org2gibb--read-org-option "TITLE")
      (file-name-sans-extension (buffer-name))))

(defun org2gibb--get-tags ()
  ""
  (let ((tags (org2gibb--read-org-option "TAGS")))
    (apply #'vector (split-string tags))))

(defun org2gibb ()
  (interactive)
  (let* ((tags (org2gibb--get-tags))
         (title (org2gibb--get-title))
         (body (org-export-as 'gfm))
         (issue (make-instance 'gh-issues-issue
                               :title title
                               :body body
                               :labels tags)))
    (gh-issues-issue-new org2gibb-api org2gibb-user org2gibb-blog-repo issue)))

(provide 'org2gibb)
