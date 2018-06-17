;;; my-submodule.el ---

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Version: 0.1
;; Keywords:

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

;; This the list of project I work sometime and want to keep
;; up-to-date.  Hence these are submodules and overide the default
;; path.

(require 'status)
(require 'j2m-tools)
(require 'org-msg)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil")
(setq org-msg-startup "hidestars indent inlineimages")
(setq org-msg-signature "

Regards,

#+begin_signature
-- *Jeremy* \\\\
/One Emacs to rule them all/
#+end_signature")
(require 'pdfgrep)
(require 'rscope)
(require 'google-maps)
(require 'offlineimap)
(require 'git-commit-mode)
(require 'magit)
(require 'magit-blame)
(require 'dtrt-indent)
(add-to-list 'load-path (concat j2m-config-directory "/gnus/lisp"))
(require 'gnus)
(require 'gnus-demon)
(require 'gnus-sum)
(add-to-list 'load-path (concat j2m-config-directory "/tramp/lisp"))
(require 'tramp)
(add-to-list 'load-path (concat j2m-config-directory "/org-mode/lisp"))
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'org-table)
(add-to-list 'load-path (concat j2m-config-directory "/notmuch/emacs"))

(provide 'my-submodule)
