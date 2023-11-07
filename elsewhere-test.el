;;; elsewhere-test.el --- Tests for elsewhere.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Wesley Nelson <wgn@wesnel.dev>

;; Author: Wesley Nelson <wgn@wesnel.dev>
;; Maintainer: Wesley Nelson <wgn@wesnel.dev>
;; Created: 23 Jul 2023

;; Version: 1.1.0
;; Package-Requires: ((emacs "29.1") (elsewhere "1.1.0"))

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

(require 'cl-generic)
(require 'elsewhere)
(require 'ert)
(require 'ert-x)
(require 'vc)

(defvar elsewhere--test-cleanup-hook nil
  "Functions for cleanup at the end of an ert test.
Don't set it globally; the functions should be let-bound.")

(cl-defstruct elsewhere--test-repo-spec-git
  "Used for initializing data in a Git repo."
  remote
  branch
  test-file-name
  test-file-contents)

(cl-defgeneric elsehwere--test-initialize-repo (spec)
  "Use SPEC to init data in a VC repo in `default-directory'.
Returns a buffer inside the repo.")

(cl-defmethod elsewhere--test-initialize-repo ((spec elsewhere--test-repo-spec-git))
  "Use SPEC to init data in a VC repo in `default-directory'.
Returns a buffer inside the repo."
  (message "Initializing test data in Git repo")
  (message "Test Git repo spec: %s" spec)
  (let* ((tmp-name (expand-file-name
                    (elsewhere--test-repo-spec-git-test-file-name spec)
                    default-directory))
         (tmp-buff (find-file tmp-name))
         (tmp-contents (elsewhere--test-repo-spec-git-test-file-contents spec))
         (remote (elsewhere--test-repo-spec-git-remote spec))
         (branch (elsewhere--test-repo-spec-git-branch spec)))

    (when tmp-contents
      (message "Writing contents to file: %s" tmp-name)
      (write-region tmp-contents nil tmp-name nil 'nomessage))

    (with-current-buffer tmp-buff
      (message "Saving file: %s" (buffer-file-name))
      (save-buffer)

      (message "Registering file in Git: %s" (buffer-file-name))
      (vc-register)

      ;; TODO: is there a built-in way to add a remote?
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

      (when branch
        (message "Creating Git branch: %s" branch)
        ;; HACK: `vc-create-branch' prompts for user input, so we
        ;; instead use `vc-git-command'.
        (vc-git-command "*vc*" 0 nil "checkout" "-b" branch)

        (let* ((raw (with-output-to-string
                      (with-current-buffer standard-output
                        (vc-git--out-ok "symbolic-ref" "HEAD"))))
               (branchp (string-match
                         "^\\(refs/heads/\\)?\\(.+\\)$" raw)))
          (should branchp)
          (should (equal branch (match-string 2 raw))))

        (message "Checked out Git branch: %s" branch))

      (should (equal remote
                     (vc-git-repository-url (buffer-file-name)))))

    (message "Finished initializing test data in Git repo")
    tmp-buff))

(cl-defgeneric elsehwere--test-destroy-repo (spec)
  "Use SPEC to destroy data in a VC repo in `default-directory'.")

(cl-defmethod elsewhere--test-destroy-repo ((spec elsewhere--test-repo-spec-git))
  "Use SPEC to destroy data in a VC repo in `default-directory'."
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
  `(ert-with-temp-directory tempdir
     (message "Created test temp dir: %s" tempdir)
     (let* ((vc-handled-backends (list ,backend))
            (default-directory tempdir)
            (process-environment process-environment)
            elsewhere--test-cleanup-hook)
       (unwind-protect
           (progn
             (add-hook
              'elsewhere--test-cleanup-hook
              (lambda ()
                (message "Running test cleanup hook")
                (elsewhere--test-destroy-repo ,spec)
                (message "Finished running test cleanup hook")))

             (should (file-directory-p default-directory))
             (message "Creating repo with backend: %s" ,backend)
             (vc-create-repo ,backend)
             (should (equal (vc-responsible-backend default-directory) ,backend))
             (message "Created repo with backend: %s" ,backend)

             (message "Initializing test data in repo")
             (let* ((tmp-buff (elsewhere--test-initialize-repo ,spec)))
               (with-current-buffer tmp-buff
                 (message "Executing test body in buffer: %s" tmp-buff)
                 ,@body
                 (message "Finished executing test body"))))
         (ignore-errors
           (message "About to run test cleanup hooks")
           (run-hooks 'elsewhere--test-cleanup-hook)
           (message "Finished running test cleanup hooks"))))))

(ert-deftest elsewhere--test-elsewhere-open ()
  "Test the function `elsewhere-open'."
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :branch "main"
    :test-file-name "elsewhere.el")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://github.com/wesnel/elsewhere/blob/main/elsewhere.el"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@github.com:wesnel/elsewhere.git"
    :branch "main"
    :test-file-name "elsewhere.el")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://github.com/wesnel/elsewhere/blob/main/elsewhere.el"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@github.com:wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2-L5"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (transient-mark-mode +1)
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@github.com:wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2-L5"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (transient-mark-mode +1)
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://github.com/wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (transient-mark-mode +1)
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-char)
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@github.com:wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (transient-mark-mode +1)
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-char)
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://gitlab.com/wesnel/elsewhere.git"
    :branch "main"
    :test-file-name "elsewhere.el")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/main/elsewhere.el"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@gitlab.com:wesnel/elsewhere.git"
    :branch "main"
    :test-file-name "elsewhere.el")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/main/elsewhere.el"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://gitlab.com/wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@gitlab.com:wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://gitlab.com/wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2-L5"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (transient-mark-mode +1)
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@gitlab.com:wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2-L5"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (transient-mark-mode +1)
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-line 3)
     (should (equal 5 (line-number-at-pos)))
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "https://gitlab.com/wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (transient-mark-mode +1)
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-char)
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t)))
  (elsewhere--test-with-repo
   'Git
   (make-elsewhere--test-repo-spec-git
    :remote "git@gitlab.com:wesnel/elsewhere.git"
    :branch "c64ad3953dfbd7bbf23d36fe302b1e54112022d1"
    :test-file-name "elsewhere.el"
    :test-file-contents "1\n2\n3\n4\n5\n6\n7")
   (let* ((browse-url-handlers
           '(("\\`http" . (lambda (url &rest args)
                            (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2"
                                           url)))))))
     (should (length= browse-url-handlers 1))
     (transient-mark-mode +1)
     (forward-line)
     (should (equal 2 (line-number-at-pos)))
     (push-mark nil t t)
     (forward-char)
     (should (equal 2 (line-number-at-pos (mark))))
     (should (use-region-p))
     (elsewhere-open nil nil nil t))))

(provide 'elsewhere-test)

;;; elsewhere-test.el ends here
