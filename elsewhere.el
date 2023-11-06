;;; elsewhere.el --- Open version-controlled code in your web browser  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Wesley Nelson <wgn@wesnel.dev>

;; Author: Wesley Nelson <wgn@wesnel.dev>
;; Maintainer: Wesley Nelson <wgn@wesnel.dev>
;; Created: 24 Jul 2023

;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

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

;; Use `elsewhere' to open a file or a marked region of a file as a
;; permalinked webpage in your browser.
;;
;; Open a version-controlled file, (optionally) mark a region in the
;; file, and call `elsewhere-open' interactively by executing M-x
;; elsewhere-open.  You should see a webpage open in your browser.

;;; Change Log:

;; 2023-07-26 - v1.0.0
;; * Support `Git' backend from `vc-handled-backends'
;; * Support GitHub and GitLab
;; * Support generating URL in echo area
;; * Support opening generated URL in browser using `browse-url'
;; * Support choosing a revision for URLs using `completing-read'

;;; Code:

(eval-when-compile
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

(defcustom elsewhere-host-regexp-github-http "^https?://github.com/"
  "Regexp for matching the host in a GitHub HTTP remote URL."
  :type 'string
  :group 'convenience)

(defcustom elsewhere-host-regexp-github-ssh "^git@github.com:"
  "Regexp for matching the host in a GitHub SSH remote URL."
  :type 'string
  :group 'convenience)

(defcustom elsewhere-host-regexps-github `(,elsewhere-host-regexp-github-http ,elsewhere-host-regexp-github-ssh)
  "Regexps for matching the host in a GitHub remote URL."
  :type '(repeat string)
  :group 'convenience)

(defcustom elsewhere-host-regexp-gitlab-http "^https?://gitlab.com/"
  "Regexp for matching the host in a GitLab HTTP remote URL."
  :type 'string
  :group 'convenience)

(defcustom elsewhere-host-regexp-gitlab-ssh "^git@gitlab.com:"
  "Regexp for matching the host in a GitLab SSH remote URL."
  :type 'string
  :group 'convenience)

(defcustom elsewhere-host-regexps-gitlab `(,elsewhere-host-regexp-gitlab-http ,elsewhere-host-regexp-gitlab-ssh)
  "Regexps for matching the host in a GitLab remote URL."
  :type '(repeat string)
  :group 'convenience)

(defcustom elsewhere-recognized-remotes-git `((,elsewhere-host-regexps-github . elsewhere--build-url-git-github)
                                              (,elsewhere-host-regexps-gitlab . elsewhere--build-url-git-gitlab))
  "Maps supported `Git' remote URLs to URL builders."
  :type '(alist :key-type (repeat string)
                :value-type function)
  :group 'convenience)

(defcustom elsewhere-dot-git-suffix ".git"
  "The suffix which is added to the end of a repo name."
  :type 'string
  :group 'convenience)

;;;###autoload
(defun elsewhere-open (&optional buffer start end interactive?)
  "Open the current buffer and region in your web browser.
If BUFFER is not provided, then it will default to the current buffer.
If the points START and END are provided, then the region
delineated by those line numbers will be incorporated into the URL.
Otherwise, START and END will default to the currently-selected
region (if any).  If the function is called interactively, then
INTERACTIVE? will be set to t."
  (interactive (list nil nil nil t))
  (let* ((url (elsewhere-build-url buffer start end interactive?)))
    (browse-url url)))

;;;###autoload
(defun elsewhere-build-url (&optional buffer start end interactive?)
  "Build a permalinked URL for the BUFFER and region.
If BUFFER is not provided, then it will default to the current buffer.
If the points START and END are provided, then the region
delineated by those points will be incorporated into the URL.
Otherwise, START and END will default to the currently-selected
region (if any).  If the function is called interactively, then
INTERACTIVE? will be set to t."
  (interactive (list nil nil nil t))
  (let* ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (let* ((use-region (unless (and start end) (use-region-p)))
               (start (when use-region (region-beginning)))
               (end (when use-region (region-end)))
               (top (when start (line-number-at-pos start)))
               (bottom (when end (line-number-at-pos end)))
               (file (buffer-file-name buffer))
               (backend (vc-responsible-backend file))
               (pairing (assq backend elsewhere-recognized-backends)))
          (if (not pairing) (user-error "This VC backend is not supported")
            (let* ((builder (cdr pairing))
                   (url (funcall builder file top bottom interactive?)))
              (when interactive? (message url))
              url)))))))

(defun elsewhere--is-matching-any-remote? (prefixes remote)
  "Check if REMOTE matches any remote in the list PREFIXES."
  (and (not (null prefixes))
       (or (string-match-p (car prefixes) remote)
           (elsewhere--is-matching-any-remote? (cdr prefixes) remote))))

(defun elsewhere--build-url-git (file top bottom &optional interactive?)
  "Build the URL for the FILE on a `Git' remote.
If the line numbers TOP and BOTTOM are provided, then the region
delineated by those line numbers will be incorporated into the URL.  If
INTERACTIVE? is non-nil, then the git revision will be calculated
interactively.  Otherwise, the URL will use the current revision."
  (let* ((remote (vc-git-repository-url file))
         (pairing (assoc remote elsewhere-recognized-remotes-git 'elsewhere--is-matching-any-remote?))
         (rev (if interactive? (elsewhere--choose-git-revision-interactively (vc-working-revision file))
                (vc-working-revision file)))
         (path (file-relative-name file (vc-root-dir))))
    (if (not pairing) (user-error "This Git remote is not supported")
      (let* ((builder (cdr pairing)))
        (funcall builder remote rev path top bottom)))))

(defun elsewhere--get-git-repo-dot-git-path (regexps remote)
  "Use REGEXPS to trim the host information off of REMOTE."
  (if (null regexps) remote
    (let* ((regexp (car regexps)))
      (replace-regexp-in-string regexp "" (elsewhere--get-git-repo-dot-git-path (cdr regexps) remote)))))

(defun elsewhere--get-git-repo-path (regexps remote)
  "Use REGEXPS to trim the host information off of REMOTE.
Also, trim the .git suffix from the end of the repository name."
  (string-remove-suffix elsewhere-dot-git-suffix (elsewhere--get-git-repo-dot-git-path regexps remote)))

(defun elsewhere--build-url-git-github (remote rev path &optional top bottom)
  "Build the URL for the PATH at commit REV from REMOTE on GitHub.
If the line numbers TOP and BOTTOM are provided, then the region
delineated by those line numbers will be incorporated into the URL."
  (let* ((repo (elsewhere--get-git-repo-path elsewhere-host-regexps-github remote))
         (base (format "https://github.com/%s/blob/%s/%s" repo rev path)))
    (if (and top bottom) (if (not (eq top bottom)) (format "%s#L%d-L%d" base top bottom)
                          (format "%s#L%d" base top))
      base)))

(defun elsewhere--build-url-git-gitlab (remote rev path &optional top bottom)
  "Build the URL for the PATH at commit REV from REMOTE on GitLab.
If the line numbers TOP and BOTTOM are provided, then the region
delineated by those line numbers will be incorporated into the URL."
  (let* ((repo (elsewhere--get-git-repo-path elsewhere-host-regexps-gitlab remote))
         (base (format "https://gitlab.com/%s/-/blob/%s/%s" repo rev path)))
    (if (and top bottom) (if (not (eq top bottom)) (format "%s#L%d-L%d" base top bottom)
                          (format "%s#L%d" base top))
      base)))

(defun elsewhere--choose-git-revision-interactively (default)
  "Choose the Git revision to use interactively using `completing-read'.
DEFAULT is used as the default value."
  (completing-read (format "Choose revision (default %s):" default)
                   (vc-git-branches)
                   nil
                   nil
                   nil
                   nil
                   default))

(provide 'elsewhere)

;;; elsewhere.el ends here
