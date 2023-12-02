;;; elsewhere-test.el --- Tests for elsewhere.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Wesley Nelson <wgn@wesnel.dev>

;; Author: Wesley Nelson <wgn@wesnel.dev>
;; Maintainer: Wesley Nelson <wgn@wesnel.dev>
;; Created: 23 Jul 2023

;; Version: 1.3.0
;; Package-Requires: ((emacs "29.1") (elsewhere "1.3.0"))

;; Keywords: convenience

;; URL: https://github.com/wesnel/elsewhere

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'rx))

(require 'cl-generic)
(require 'elsewhere)
(require 'ert)
(require 'ert-x)
(require 'log-edit)
(require 'vc)

(defvar elsewhere--test-cleanup-hook nil
  "Functions for cleanup at the end of an ert test.
Don't set it globally; the functions should be let-bound.")

(cl-defstruct elsewhere--test-repo-spec-git
  "Used for initializing data in a `Git' repo."
  remote
  branch
  test-file-name
  test-file-contents
  commit)

(cl-defgeneric elsewhere--test-initialize-repo (spec)
  "Use SPEC to init data in a VC repo in `default-directory'.
Returns a buffer inside the repo.")

(cl-defgeneric elsewhere--test-destroy-repo (spec)
  "Use SPEC to destroy data in a VC repo in `default-directory'.")

(cl-defmethod elsewhere--test-initialize-repo ((spec elsewhere--test-repo-spec-git))
  "Use SPEC to init data in a `Git' VC repo in `default-directory'.
Returns a buffer inside the repo."
  (message "Initializing test data in Git repo")
  (message "Test Git repo spec: %s" spec)
  (let* ((tmp-name (expand-file-name
                    (elsewhere--test-repo-spec-git-test-file-name spec)
                    default-directory))
         (tmp-buff (find-file tmp-name))
         (process-environment (append '("EMAIL=wgn@example.com"
                                        "GIT_AUTHOR_NAME=wgn"
                                        "GIT_COMMITTER_NAME=wgn")
                                      process-environment))
         (tmp-contents (elsewhere--test-repo-spec-git-test-file-contents spec))
         (remote (elsewhere--test-repo-spec-git-remote spec))
         (branch (elsewhere--test-repo-spec-git-branch spec))
         (commit (elsewhere--test-repo-spec-git-commit spec)))

    (when tmp-contents
      (message "Writing contents to file: %s" tmp-name)
      (write-region tmp-contents nil tmp-name nil 'nomessage))

    (with-current-buffer tmp-buff
      (message "Saving file: %s" (buffer-file-name))
      (save-buffer)

      (message "Registering file in Git: %s" (buffer-file-name))
      (vc-register)

      (when remote
        ;; TODO: Is there a built-in way to add a remote?
        (message "Adding Git remote as origin: %s" remote)
        (vc-do-command
         "*vc*"
         0
         vc-git-program
         nil
         "remote"
         "add"
         "origin"
         remote)

        (should (equal remote
                       (vc-git-repository-url (buffer-file-name)))))

      (when branch
        (message "Creating Git branch: %s" branch)
        ;; HACK: `vc-create-branch' prompts for user input, so we
        ;;       instead use `vc-git-command'.
        (vc-git-command "*vc*" 0 nil "checkout" "-b" branch)

        (let* ((raw (with-output-to-string
                      (with-current-buffer standard-output
                        (vc-git--out-ok "symbolic-ref" "HEAD"))))
               (branchp (string-match
                         "^\\(refs/heads/\\)?\\(.+\\)$" raw)))
          (should branchp)
          (should (equal branch (match-string 2 raw))))

        (message "Checked out Git branch: %s" branch))

      (when commit
        (message "Creating commit on: %s" (or branch "HEAD"))
        (vc-git-checkin nil commit)))

    (message "Finished initializing test data in Git repo")
    tmp-buff))

(cl-defmethod elsewhere--test-destroy-repo ((spec elsewhere--test-repo-spec-git))
  "Use SPEC to destroy data in a `Git' VC repo in `default-directory'."
  (message "Destroying test data in Git repo")
  (message "Test Git repo spec: %s" spec)
  (let* ((tmp-name (expand-file-name
                    (elsewhere--test-repo-spec-git-test-file-name spec)
                    default-directory))
         (tmp-buff (get-file-buffer tmp-name)))
    (when tmp-buff
      (message "Negating modified flag for file: %s" tmp-name)
      (with-current-buffer tmp-buff
        (set-buffer-modified-p nil))

      (message "Killing buffer: %s" tmp-buff)
      (kill-buffer tmp-buff)))
  (message "Finished destroying test data in Git repo"))

(defmacro elsewhere--test-with-repo (backend spec &rest body)
  "Create a temporary test directory with a VC repo inside.
BACKEND is the VC backend to use, SPEC is a configuration for
test data used inside the VC repo, and BODY will be executed with
a `default-directory' equal to this temporary test directory."
  `(ert-with-temp-directory
    tempdir
    (message "Created test temp dir: %s" tempdir)
    (let* ((vc-handled-backends (list ,backend))
           (default-directory tempdir)
           (process-environment process-environment)
           (should-toggle-transient-mark-mode (not transient-mark-mode))
           elsewhere--test-cleanup-hook)
      (unwind-protect
          (progn
            (when should-toggle-transient-mark-mode
              (message "Turning on transient-mark-mode")
              (transient-mark-mode +1))

            (add-hook
             'elsewhere--test-cleanup-hook
             (lambda ()
               (message "Running test cleanup hook")
               (elsewhere--test-destroy-repo ,spec)

               (when should-toggle-transient-mark-mode
                 (message "Turning off transient-mark-mode")
                 (transient-mark-mode -1))

               (message "Finished running test cleanup hook")))

            (should (file-directory-p default-directory))
            (message "Creating repo with backend: %s" ,backend)
            (vc-create-repo ,backend)
            (should (equal (vc-responsible-backend default-directory) ,backend))
            (message "Created repo with backend: %s" ,backend)

            (message "Initializing test data in repo")
            (let ((tmp-buff (elsewhere--test-initialize-repo ,spec)))
              (with-current-buffer tmp-buff
                (message "Executing test body in buffer: %s" tmp-buff)
                ,@body
                (message "Finished executing test body"))))
        (ignore-errors
          (message "About to run test cleanup hooks")
          (run-hooks 'elsewhere--test-cleanup-hook)
          (message "Finished running test cleanup hooks"))))))

(defun elsewhere--test-contains-hash? (text &rest regexps)
  "Return non-nil if TEXT is a hash and all provided REGEXPS match.
Any text in TEXT which matches one of the REGEXPS will be removed
before performing the SHA format check.  If any of the REGEXPS
don't match, then the function will return nil.  After all
REGEXPS have been matched and removed, the remaining text left
over will be checked to see if it appears to be in the format of
a SHA hash."
  (let ((regexp (car regexps)))
    (if regexp
        (progn
          (message "Checking regexp: %s" regexp)
          (and (string-match regexp text)
               (apply #'elsewhere--test-contains-hash?
                      (replace-regexp-in-string regexp "" text)
                      (cdr regexps))))
      (message "Checking for hash format in string: %s" text)
      (string-match (rx line-start (= 40 alphanumeric) line-end) text))))

;; FIXME: With my flymake setup, I see the this warning on the following line:
;;          reference to free variable ‘utf-8’
;;        Where does this originate from?
(ert-deftest elsewhere--test-elsewhere-open-git-remote-not-configured ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :branch "branch"
    :test-file-name "elsewhere.el")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (ert-fail "Bad URL was opened"))))))
     (should (length= browse-url-handlers 1))
     (should-error
      (elsewhere-open nil nil nil t)
      :type 'user-error))))

(ert-deftest elsewhere--test-elsewhere-open-git-remote-not-supported ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://example.com/wesnel/elsewhere.git"
    :branch "branch"
    :test-file-name "elsewhere.el")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (ert-fail "Bad URL was opened"))))))
     (should (length= browse-url-handlers 1))
     (should-error
      (elsewhere-open nil nil nil t)
      :type 'user-error))))

(ert-deftest elsewhere--test-elsewhere-open-backend-not-supported ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :branch "branch"
    :test-file-name "elsewhere.el")
   (let ((elsewhere-recognized-backends nil)
         (browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (ert-fail "Bad URL was opened"))))))
     (should (length= browse-url-handlers 1))
     (should-error
      (elsewhere-open nil nil nil t)
      :type 'user-error))))

(ert-deftest elsewhere--test-elsewhere-open-with-start-and-end ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :branch "branch"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (equal "https://github.com/wesnel/elsewhere/blob/branch/elsewhere.el#L2-L5"
                                url)))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (let ((start (region-beginning))
           (end (region-end)))
       (deactivate-mark)
       (should (not (use-region-p)))
       (should (equal 2 (line-number-at-pos start)))
       (should (equal 5 (line-number-at-pos end)))
       (elsewhere-open nil start end t)))))

(ert-deftest elsewhere--test-elsewhere-open-with-start ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :branch "branch"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (equal "https://github.com/wesnel/elsewhere/blob/branch/elsewhere.el"
                                url)))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (let ((start (region-beginning)))
       (deactivate-mark)
       (should (not (use-region-p)))
       (elsewhere-open nil start nil t)))))

(ert-deftest elsewhere--test-elsewhere-open-github-http-branch ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :branch "branch"
    :test-file-name "elsewhere.el")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (equal "https://github.com/wesnel/elsewhere/blob/branch/elsewhere.el"
                                url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-github-ssh-branch ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@github.com:wesnel/elsewhere.git"
    :branch "branch"
    :test-file-name "elsewhere.el")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (equal "https://github.com/wesnel/elsewhere/blob/branch/elsewhere.el"
                                url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-github-http-rev ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://github.com/wesnel/elsewhere/blob/")
                          (rx "/elsewhere.el" line-end))))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-github-http-rev-interactive ()
 (elsewhere--test-with-repo
  'Git
  (make-elsewhere--test-repo-spec-git
   :remote "https://github.com/wesnel/elsewhere.git"
   :test-file-name "elsewhere.el"
   :commit "message")
  (let ((completing-read-function
         (lambda (_prompt
                  _collection
                  &optional
                  _predicate
                  _require-match
                  _initial-input
                  _hist
                  def
                  _inherit-input-method)
           def))
        (browse-url-handlers
         '(("\\`http"
            . (lambda (url &rest args)
                (should (elsewhere--test-contains-hash?
                         url
                         (rx line-start "https://github.com/wesnel/elsewhere/blob/")
                         (rx "/elsewhere.el" line-end))))))))
    (should (length= browse-url-handlers 1))
    (elsewhere-open nil nil nil))))

(ert-deftest elsewhere--test-elsewhere-open-github-ssh-rev ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@github.com:wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://github.com/wesnel/elsewhere/blob/")
                          (rx "/elsewhere.el" line-end))))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-github-http-rev-with-region ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://github.com/wesnel/elsewhere/blob/")
                          (rx "/elsewhere.el#L2-L5" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-github-ssh-rev-with-region ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@github.com:wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://github.com/wesnel/elsewhere/blob/")
                          (rx "/elsewhere.el#L2-L5" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-github-http-rev-with-region-one-line ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://github.com/wesnel/elsewhere/blob/")
                          (rx "/elsewhere.el#L2" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-char)
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-github-ssh-rev-with-region-one-line ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@github.com:wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://github.com/wesnel/elsewhere/blob/")
                          (rx "/elsewhere.el#L2" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-char)
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-gitlab-http-branch ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://gitlab.com/wesnel/elsewhere.git"
    :branch "branch"
    :test-file-name "elsewhere.el")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/branch/elsewhere.el"
                                url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-gitlab-ssh-branch ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@gitlab.com:wesnel/elsewhere.git"
    :branch "branch"
    :test-file-name "elsewhere.el")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/branch/elsewhere.el"
                                url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-gitlab-http-rev ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://gitlab.com/wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://gitlab.com/wesnel/elsewhere/-/blob/")
                          (rx "/elsewhere.el" line-end))))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-gitlab-ssh-rev ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@gitlab.com:wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://gitlab.com/wesnel/elsewhere/-/blob/")
                          (rx "/elsewhere.el" line-end))))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-gitlab-http-rev-with-region ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://gitlab.com/wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://gitlab.com/wesnel/elsewhere/-/blob/")
                          (rx "/elsewhere.el#L2-L5" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-gitlab-ssh-rev-with-region ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@gitlab.com:wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://gitlab.com/wesnel/elsewhere/-/blob/")
                          (rx "/elsewhere.el#L2-L5" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-gitlab-http-rev-with-region-one-line ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://gitlab.com/wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://gitlab.com/wesnel/elsewhere/-/blob/")
                          (rx "/elsewhere.el#L2" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-char)
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-gitlab-ssh-rev-with-region-one-line ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@gitlab.com:wesnel/elsewhere.git"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://gitlab.com/wesnel/elsewhere/-/blob/")
                          (rx "/elsewhere.el#L2" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-char)
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-sourcehut-http-branch ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://git.sr.ht/~wgn/elsewhere"
    :branch "branch"
    :test-file-name "elsewhere.el")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (equal "https://git.sr.ht/~wgn/elsewhere/tree/branch/item/elsewhere.el"
                                url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-sourcehut-ssh-branch ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@git.sr.ht:~wgn/elsewhere"
    :branch "branch"
    :test-file-name "elsewhere.el")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (equal "https://git.sr.ht/~wgn/elsewhere/tree/branch/item/elsewhere.el"
                                url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-sourcehut-http-rev ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://git.sr.ht/~wgn/elsewhere"
    :test-file-name "elsewhere.el"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://git.sr.ht/~wgn/elsewhere/tree/")
                          (rx "/item/elsewhere.el" line-end))))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-sourcehut-ssh-rev ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@git.sr.ht:~wgn/elsewhere"
    :test-file-name "elsewhere.el"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://git.sr.ht/~wgn/elsewhere/tree/")
                          (rx "/item/elsewhere.el" line-end))))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-sourcehut-http-rev-with-region ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://git.sr.ht/~wgn/elsewhere"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://git.sr.ht/~wgn/elsewhere/tree/")
                          (rx "/item/elsewhere.el#L2-5" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-sourcehut-ssh-rev-with-region ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@git.sr.ht:~wgn/elsewhere"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://git.sr.ht/~wgn/elsewhere/tree/")
                          (rx "/item/elsewhere.el#L2-5" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-sourcehut-http-rev-with-region-one-line ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://git.sr.ht/~wgn/elsewhere"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://git.sr.ht/~wgn/elsewhere/tree/")
                          (rx "/item/elsewhere.el#L2" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-char)
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(ert-deftest elsewhere--test-elsewhere-open-sourcehut-ssh-rev-with-region-one-line ()
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@git.sr.ht:~wgn/elsewhere"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7"
    :commit "message")
   (let ((browse-url-handlers
          '(("\\`http"
             . (lambda (url &rest args)
                 (should (elsewhere--test-contains-hash?
                          url
                          (rx line-start "https://git.sr.ht/~wgn/elsewhere/tree/")
                          (rx "/item/elsewhere.el#L2" line-end))))))))
     (should (length= browse-url-handlers 1))
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-char)
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(provide 'elsewhere-test)

;;; elsewhere-test.el ends here
