(defun org2issue-github-add (title body tags)
  (let ((api (gh-issues-api "api"))
        (issue (make-instance 'gh-issues-issue
                              :title title
                              :body body
                              :labels tags)))
    (oref (gh-issues-issue-new api org2issue-user org2issue-blog-repo issue) data)))

(defun org2issue-github-update (title body tags orign-issue-data &optional delete)
  (let* ((api (gh-issues-api "api"))
         (orign-issue-data (split-string orign-issue-data))
         (issue (make-instance 'gh-issues-issue
                               :title title
                               :body body
                               :labels tags
                               :state (if delete
                                          'closed
                                        'open)))
         (org2issue-user (nth 0 orign-issue-data))
         (org2issue-blog-repo (nth 1 orign-issue-data))
         (org2issue-number (string-to-number (nth 2 orign-issue-data))))
    (oref (gh-issues-issue-update api org2issue-user org2issue-blog-repo org2issue-number issue) data)))

(defun org2issue-github-delete (title body tags orign-issue-data)
  (org2issue-github-update title body tags orign-issue-data t))

(defun org2issue-github-get-open-issues ()
  (let* ((api (gh-issues-api "api"))
         (issues (reverse (oref (gh-issues-issue-list api org2issue-user org2issue-blog-repo)
                                data))))
    (remove-if (lambda (issue)
                 (string= "close" (oref issue state)))
               issues)))

(defun org2issue-github-issue-to-string (issue)
  (cl-assert (gh-issues-issue-p issue) t "should only accept gh-issues-issue object")
  (let ((html-url (oref issue html-url))
        (state (oref issue state)))
    (when (string-equal "open" state)
      (s-format org2issue-update-item-format 'oref issue))))

(defun org2issue-github-issue-to-url (issue)
  (oref issue html-url))
