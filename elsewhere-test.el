;;; elsewhere-test.el --- Tests for elsewhere.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Wesley Nelson <wgn@wesnel.dev>

;; Author: Wesley Nelson <wgn@wesnel.dev>
;; Maintainer: Wesley Nelson <wgn@wesnel.dev>
;; Created: 23 Jul 2023

;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (elsewhere "1.0.0"))

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

(require 'elsewhere)
(require 'ert)
(require 'vc)

(ert-deftest elsewhere--test-elsewhere-build-url ()
  "Test the function `elsewhere-build-url'."
  (let* ((buffer (current-buffer))
         (rev (vc-working-revision buffer)))
    (should (equal (format "https://github.com/wesnel/elsewhere/blob/%s/elsewhere-test.el" rev)
                   (elsewhere-build-url buffer)))
    (should (equal (format "https://github.com/wesnel/elsewhere/blob/%s/elsewhere-test.el#L2-L5" rev)
                   (elsewhere-build-url buffer 2 5)))
    (should (equal (format "https://github.com/wesnel/elsewhere/blob/%s/elsewhere-test.el#L2" rev)
                   (elsewhere-build-url buffer 2 2)))))

(ert-deftest elsewhere--test-elsewhere--build-url-git-github ()
  "Test the function `elsewhere--build-url-git-github'."
  (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el"
                 (elsewhere--build-url-git-github "https://github.com/wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el")))
  (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el"
                 (elsewhere--build-url-git-github "git@github.com:wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el")))
  (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2-L5"
                 (elsewhere--build-url-git-github "https://github.com/wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el" 2 5)))
  (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2-L5"
                 (elsewhere--build-url-git-github "git@github.com:wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el" 2 5)))
  (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2"
                 (elsewhere--build-url-git-github "https://github.com/wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el" 2 2)))
  (should (equal "https://github.com/wesnel/elsewhere/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2"
                 (elsewhere--build-url-git-github "git@github.com:wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el" 2 2))))

(ert-deftest elsewhere--test-elsewhere--build-url-git-gitlab ()
  "Test the function `elsewhere--build-url-git-gitlab'."
  (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el"
                 (elsewhere--build-url-git-gitlab "https://gitlab.com/wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el")))
  (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el"
                 (elsewhere--build-url-git-gitlab "git@gitlab.com:wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el")))
  (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2-L5"
                 (elsewhere--build-url-git-gitlab "https://gitlab.com/wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el" 2 5)))
  (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2-L5"
                 (elsewhere--build-url-git-gitlab "git@gitlab.com:wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el" 2 5)))
  (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2"
                 (elsewhere--build-url-git-gitlab "https://gitlab.com/wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el" 2 2)))
  (should (equal "https://gitlab.com/wesnel/elsewhere/-/blob/c64ad3953dfbd7bbf23d36fe302b1e54112022d1/elsewhere.el#L2"
                 (elsewhere--build-url-git-gitlab "git@gitlab.com:wesnel/elsewhere.git" "c64ad3953dfbd7bbf23d36fe302b1e54112022d1" "elsewhere.el" 2 2))))

(provide 'elsewhere-test)

;;; elsewhere-test.el ends here
