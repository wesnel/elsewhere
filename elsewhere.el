;;; elsewhere.el --- Open version-controlled code in your web browser  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Wesley Nelson <wgn@wesnel.dev>

;; Author: Wesley Nelson <wgn@wesnel.dev>
;; Maintainer: Wesley Nelson <wgn@wesnel.dev>
;; Created: 24 Jul 2023

;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))

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
;; elsewhere-open. You should see a webpage open in your browser.

;;; Change Log:

;; 2023-07-24 - v1.0.0
;; * Add support for GitHub

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

(defcustom elsewhere-host-regexp-github-http "^https?:\/\/\github.com\/"
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

(defcustom elsewhere-recognized-remotes-git `((,elsewhere-host-regexps-github . elsewhere--build-url-git-github))
  "Maps supported `Git' remote URLs to URL builders."
  :type '(alist :key-type (repeat string)
                :value-type function)
  :group 'convenience)

(defcustom elsewhere-dot-git-suffix ".git"
  "The suffix which is added to the end of a repo name."
  :type 'string
  :group 'convenience)

(defun elsewhere--build-url (buffer &optional start end)
  "Build the URL for the BUFFER.
If the points START and END are provided, then the region
delineated by those points will be incorporated into the URL."
  (with-current-buffer buffer
    (save-mark-and-excursion
      (let* ((file (buffer-file-name buffer))
             (backend (vc-backend file))
             (pairing (assq backend elsewhere-recognized-backends)))
        (if (not pairing) (user-error "This VC backend is not supported")
          (let* ((builder (cdr pairing)))
            (funcall builder file start end)))))))

(defun elsewhere--is-matching-any-remote? (prefixes remote)
  "Check if REMOTE matches any remote in the list PREFIXES."
  (and (not (null prefixes))
       (or (string-match-p (car prefixes) remote)
           (elsewhere--is-matching-any-remote? (cdr prefixes) remote))))

(defun elsewhere--build-url-git (file &optional start end)
  "Build the URL for the FILE on a `Git' remote.
If the points START and END are provided, then the region
delineated by those points will be incorporated into the URL."
  (let* ((remote (vc-git-repository-url file))
         (pairing (assoc remote elsewhere-recognized-remotes-git 'elsewhere--is-matching-any-remote?))
         (rev (vc-working-revision file))
         (file (file-relative-name file (vc-root-dir))))
    (if (not pairing) (user-error "This Git remote is not supported")
      (let* ((builder (cdr pairing)))
        (funcall builder remote rev file start end)))))

(defun elsewhere--get-git-repo-dot-git-path (regexps remote)
  "Use REGEXPS to trim the host information off of REMOTE."
  (if (null regexps) remote
    (let* ((regexp (car regexps)))
      (replace-regexp-in-string regexp "" (elsewhere--get-git-repo-dot-git-path (cdr regexps) remote)))))

(defun elsewhere--get-git-repo-path (regexps remote)
  "Use REGEXPS to trim the host information off of REMOTE.
Also, trim the .git suffix from the end of the repository name."
  (string-remove-suffix elsewhere-dot-git-suffix (elsewhere--get-git-repo-dot-git-path regexps remote)))

(defun elsewhere--build-url-git-github (remote rev file &optional start end)
  "Build the URL for the FILE at commit REV from REMOTE on GitHub.
If the points START and END are provided, then the region
delineated by those points will be incorporated into the URL."
  ;; TODO: Make this function more reusable for new hosts.
  (let* ((repo (elsewhere--get-git-repo-path elsewhere-host-regexps-github remote))
         (base (format "https://github.com/%s/blob/%s/%s" repo rev file)))
    (if (and start end) (format "%s#L%d-L%d" base (line-number-at-pos start) (line-number-at-pos end))
      base)))

;;;###autoload
(defun elsewhere-open (buffer &optional start end)
  "Open the BUFFER in your web browser.
If the points START and END are provided, then the region
delineated by those points will be opened. If this function is
called interactively, then BUFFER will default to the current
buffer and START and END will default to the currently-selected
region (if any)."
  (interactive
   (if (use-region-p) (list (current-buffer) (region-beginning) (region-end))
     (list (current-buffer))))
  (let* ((url (elsewhere--build-url buffer start end)))
    (browse-url url)))

(provide 'elsewhere)

;;; elsewhere.el ends here
