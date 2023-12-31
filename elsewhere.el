;;; elsewhere.el --- Open version-controlled code in your web browser  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Wesley Nelson <wgn@wesnel.dev>

;; URL: https://github.com/wesnel/elsewhere
;; Author: Wesley Nelson <wgn@wesnel.dev>
;; Maintainer: Wesley Nelson <wgn@wesnel.dev>
;; Created: 24 Jul 2023
;; Keywords: convenience

;; Version: 1.3.1
;; Package-Requires: ((emacs "29.1"))

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

;; Use `elsewhere' to open a file or a marked region of a file as a
;; permalinked webpage in your browser.
;;
;; Open a version-controlled file, (optionally) mark a region in the
;; file, and call `elsewhere-open' interactively by executing M-x
;; elsewhere-open.  You should see a webpage open in your browser.

;;; Code:

(defconst elsewhere-version "1.3.1"
  "Current version of elsewhere.")

(eval-when-compile
  (require 'rx)
  (require 'subr-x))

(require 'vc)
(require 'vc-git)

(defgroup elsewhere nil
  "Open version-controlled code in your web browser."
  :prefix "elsewhere-"
  :link '(url-link :tag "Report a Bug" "https://github.com/wesnel/elsewhere/issues")
  :link '(url-link :tag "Homepage" "https://github.com/wesnel/elsewhere")
  :group 'convenience)

(defcustom elsewhere-recognized-backends '((Git . elsewhere--build-url-git))
  "Maps supported `vc-handled-backends' values to URL builders."
  :type '(alist :key-type symbol
                :value-type function)
  :group 'convenience)

(defcustom elsewhere-host-regexp-github-http (rx line-start
                                                 "http"
                                                 (zero-or-one "s")
                                                 "://github.com/")
  "Regexp for matching the host in a GitHub HTTP remote URL."
  :type 'string
  :group 'convenience)

(defcustom elsewhere-host-regexp-github-ssh (rx line-start
                                                "git@github.com:")
  "Regexp for matching the host in a GitHub SSH remote URL."
  :type 'string
  :group 'convenience)

(defcustom elsewhere-host-regexps-github `(,elsewhere-host-regexp-github-http ,elsewhere-host-regexp-github-ssh)
  "Regexps for matching the host in a GitHub remote URL."
  :type '(repeat string)
  :group 'convenience)

(defcustom elsewhere-host-regexp-gitlab-http (rx line-start
                                                 "http"
                                                 (zero-or-one "s")
                                                 "://gitlab.com/")
  "Regexp for matching the host in a GitLab HTTP remote URL."
  :type 'string
  :group 'convenience)

(defcustom elsewhere-host-regexp-gitlab-ssh (rx line-start
                                                "git@gitlab.com:")
  "Regexp for matching the host in a GitLab SSH remote URL."
  :type 'string
  :group 'convenience)

(defcustom elsewhere-host-regexps-gitlab `(,elsewhere-host-regexp-gitlab-http ,elsewhere-host-regexp-gitlab-ssh)
  "Regexps for matching the host in a GitLab remote URL."
  :type '(repeat string)
  :group 'convenience)

(defcustom elsewhere-host-regexp-sourcehut-http (rx line-start
                                                    "http"
                                                    (zero-or-one "s")
                                                    "://git.sr.ht/")
  "Regexp for matching the host in a Sourcehut HTTP remote URL."
  :type 'string
  :group 'convenience)

(defcustom elsewhere-host-regexp-sourcehut-ssh (rx line-start
                                                   "git@git.sr.ht:")
  "Regexp for matching the host in a Sourcehut SSH remote URL."
  :type 'string
  :group 'convenience)

(defcustom elsewhere-host-regexps-sourcehut `(,elsewhere-host-regexp-sourcehut-http ,elsewhere-host-regexp-sourcehut-ssh)
  "Regexps for matching the host in a Sourcehut remote URL."
  :type '(repeat string)
  :group 'convenience)

(defcustom elsewhere-recognized-remotes-git `((,elsewhere-host-regexps-github . elsewhere--build-url-git-github)
                                              (,elsewhere-host-regexps-gitlab . elsewhere--build-url-git-gitlab)
                                              (,elsewhere-host-regexps-sourcehut . elsewhere--build-url-git-sourcehut))
  "Maps supported `Git' remote URLs to URL builders."
  :type '(alist :key-type (repeat string)
                :value-type function)
  :group 'convenience)

(defcustom elsewhere-dot-git-suffix ".git"
  "The suffix which is added to the end of a repo name."
  :type 'string
  :group 'convenience)

;;;###autoload
(defun elsewhere-open (&optional buffer start end headless?)
  "Open the current buffer and region in your web browser.
If BUFFER is not provided, then it will default to the current
buffer.  If the points START and END are provided, then the
region delineated by those line numbers will be incorporated into
the URL.  Otherwise, START and END will default to the
currently-selected region (if any).  If HEADLESS? is non-nil,
then do not prompt for user input."
  (interactive)
  (let ((url (elsewhere-build-url buffer start end t headless?)))
    (browse-url url)))

;;;###autoload
(defun elsewhere-build-url (&optional buffer start end silent? headless?)
  "Build a permalinked URL for the BUFFER and region.
If BUFFER is not provided, then it will default to the current
buffer.  If the points START and END are provided, then the
region delineated by those points will be incorporated into the
URL.  Otherwise, START and END will default to the
currently-selected region (if any).  If SILENT? is non-nil, then
suppress the writing of the URL to the echo area.  If HEADLESS?
is non-nil, then do not prompt for user input."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (let* ((use-region (unless (and start end) (use-region-p)))
               (start (if use-region (region-beginning) start))
               (end (if use-region (region-end) end))
               (top (when start (line-number-at-pos start)))
               (bottom (when end (line-number-at-pos end)))
               (file (buffer-file-name buffer))
               (backend (vc-responsible-backend file))
               (pairing (assq backend elsewhere-recognized-backends)))
          (if (not pairing) (user-error "This VC backend is not supported: %s" backend)
            (let* ((builder (cdr pairing))
                   (url (funcall builder file top bottom headless?)))
              (unless silent? (message url))
              url)))))))

(defun elsewhere--is-matching-any-remote? (prefixes remote)
  "Check if REMOTE matches any remote in the list PREFIXES."
  (and (not (null prefixes))
       (or (string-match-p (car prefixes) remote)
           (elsewhere--is-matching-any-remote? (cdr prefixes) remote))))

(defun elsewhere--build-url-git (file top bottom &optional headless?)
  "Build the URL for the FILE on a `Git' remote.
It is assumed that FILE is open in the current buffer.  If the line
numbers TOP and BOTTOM are provided, then the region delineated by
those line numbers will be incorporated into the URL.  If HEADLESS? is
non-nil, then the `Git' revision will be the current revision of the
current buffer.  Otherwise, the `Git' revision will be chosen using
`completing-read'."
  (let* ((remote (elsewhere--choose-git-remote file headless?))
         (pairing (assoc remote elsewhere-recognized-remotes-git 'elsewhere--is-matching-any-remote?))
         (rev (elsewhere--choose-git-revision file headless?))
         (path (file-relative-name file (vc-root-dir))))
    (if (not pairing) (user-error "This Git remote is not supported")
      (let ((builder (cdr pairing)))
        (funcall builder remote rev path top bottom)))))

(defun elsewhere--get-git-repo-dot-git-path (regexps remote)
  "Use REGEXPS to trim the host information off of REMOTE."
  (if (null regexps) remote
    (let ((regexp (car regexps)))
      (replace-regexp-in-string regexp "" (elsewhere--get-git-repo-dot-git-path (cdr regexps) remote)))))

(defun elsewhere--get-git-repo-path (regexps remote)
  "Use REGEXPS to trim the host information off of REMOTE.
Also, trim the .git suffix from the end of the repository name."
  (string-remove-suffix elsewhere-dot-git-suffix (elsewhere--get-git-repo-dot-git-path regexps remote)))

(defun elsewhere--build-url-git-github (remote rev path &optional top bottom)
  "Build URL for PATH at commit REV from REMOTE on GitHub.
If the line numbers TOP and BOTTOM are provided, then the region
delineated by those line numbers will be incorporated into the URL."
  (let* ((repo (elsewhere--get-git-repo-path elsewhere-host-regexps-github remote))
         (base (format "https://github.com/%s/blob/%s/%s" repo rev path)))
    (if (and top bottom)
        (if (not (eq top bottom))
            (format "%s#L%d-L%d" base top bottom)
          (format "%s#L%d" base top))
      base)))

(defun elsewhere--build-url-git-gitlab (remote rev path &optional top bottom)
  "Build URL for PATH at commit REV from REMOTE on GitLab.
If the line numbers TOP and BOTTOM are provided, then the region
delineated by those line numbers will be incorporated into the URL."
  (let* ((repo (elsewhere--get-git-repo-path elsewhere-host-regexps-gitlab remote))
         (base (format "https://gitlab.com/%s/-/blob/%s/%s" repo rev path)))
    (if (and top bottom)
        (if (not (eq top bottom))
            (format "%s#L%d-L%d" base top bottom)
          (format "%s#L%d" base top))
      base)))

(defun elsewhere--build-url-git-sourcehut (remote rev path &optional top bottom)
  "Build URL for PATH at commit REV from REMOTE on Sourcehut.
If the line numbers TOP and BOTTOM are provided, then the region
delineated by those line numbers will be incorporated into the URL."
  (let* ((repo (elsewhere--get-git-repo-path elsewhere-host-regexps-sourcehut remote))
         (base (format "https://git.sr.ht/%s/tree/%s/item/%s" repo rev path)))
    (if (and top bottom)
        (if (not (eq top bottom))
            (format "%s#L%d-%d" base top bottom)
          (format "%s#L%d" base top))
      base)))

(defun elsewhere--choose-git-remote (file &optional headless?)
  "Choose the `Git' remote to use for FILE.
If HEADLESS? is non-nil, then the `Git' remote will be the
default for this file.  Otherwise, the `Git' remote will be
chosen using `completing-read'."
  (let ((choices (elsewhere--list-remotes-git file)))
    (when (not choices)
      (user-error "No Git remote configured for file: %s" file))
    (if headless?
        (cdr (car choices))
      (cdr (assoc (elsewhere--choose-remote-interactively
                   (mapcar (lambda (pair) (car pair)) choices)
                   (car (car choices)))
                  choices)))))

;; FIXME: `vc' should have this function, if they don't already.
(defun elsewhere--list-remotes-git (file)
  "Construct an alist of `Git' remote names and URLs for FILE.
It is assumed that FILE is open in the current buffer."
  (let ((default-directory (vc-git-root file)))
    (with-temp-buffer
      (vc-git-command (current-buffer) 0 nil "remote")
      (mapcar (lambda (remote-name)
                `(,remote-name . ,(vc-git-repository-url file remote-name)))
              (split-string (buffer-string))))))

(defun elsewhere--choose-remote-interactively (choices &optional default)
  "Use `completing-read' to choose a remote among CHOICES.
If provided, DEFAULT is the default choice to use if nothing else
is selected or entered by the user."
  (elsewhere--choose-interactively "Choose remote" choices default))

(defun elsewhere--choose-git-revision (file &optional headless?)
  "Choose the `Git' revision to use for FILE.
It is assumed that FILE is open in the current buffer.  If HEADLESS?
is non-nil, then the `Git' revision will be the current revision of
the current buffer.  Otherwise, the `Git' revision will be chosen
using `completing-read'."
  (let* ((found-branches (vc-git-branches))
         (no-branches? (or (length= found-branches 0)
                           (and (length= found-branches 1)
                                (not (car found-branches)))))
         (found-branches (if no-branches?
                             ;; Use symbolic-ref to get the default branch.
                             (list (elsewhere--get-current-ref))
                           found-branches))
         (choices found-branches)
         (last-commit (vc-working-revision file))
         (no-commit? (not last-commit))
         (choices (if no-commit?
                      choices
                    (append (list last-commit) choices)))
         (default (car choices)))
    (if headless?
        default
      (elsewhere--choose-revision-interactively choices default))))

(defun elsewhere--get-current-ref ()
  "Use the `Git' symbolic-ref command to get the current ref on HEAD."
  (let* ((ref-output
          (with-output-to-string
            (with-current-buffer standard-output
              (vc-git--out-ok "symbolic-ref" "HEAD"))))
         (has-match (string-match
                     "^\\(refs/heads/\\)?\\(.+\\)$"
                     ref-output))
         (current-ref (when has-match
                        (match-string 2 ref-output))))
    current-ref))

(defun elsewhere--choose-revision-interactively (choices &optional default)
  "Use `completing-read' to choose a revision among CHOICES.
If provided, DEFAULT is the default choice to use if nothing else
is selected or entered by the user."
  (elsewhere--choose-interactively "Choose revision" choices default))

(defun elsewhere--choose-interactively (prompt choices &optional default)
  "Use `completing-read' to choose among CHOICES.
PROMPT is the prompting text which will be displayed to the user.
This function will handle adding a trailing semicolon to PROMPT, so exclude
that from PROMPT.  If provided, DEFAULT is the default choice to
use if nothing else is selected or entered by the user.  This
 function will handle adding information about the DEFAULT to the
 PROMPT, so exclude that from PROMPT as well."
  (completing-read (if default
                       (format "%s (default %s): " prompt default)
                     (format "%s:" prompt))
                   choices
                   nil
                   nil
                   nil
                   nil
                   default))

(provide 'elsewhere)

;;; elsewhere.el ends here
