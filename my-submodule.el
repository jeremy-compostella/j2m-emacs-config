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

(add-to-list 'load-path (concat j2m-config-directory "/status"))
(require 'status)
(add-to-list 'load-path (concat j2m-config-directory "/j2m-tools"))
(require 'j2m-tools)
(require 'org-msg)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t")
(setq org-msg-startup "hidestars indent inlineimages"
      org-msg-greeting-fmt "\nHi *%s*,\n\n"
      org-msg-greeting-name-limit 3
      org-msg-default-alternatives '((new		. (text html))
				     (reply-to-html	. (text html))
				     (reply-to-text	. (text)))
      org-msg-convert-citation t
      org-msg-signature "

Regards,

#+begin_signature
-- 
*Jeremy*
/One Emacs to rule them all/
#+end_signature")
(require 'pdfgrep)
(require 'rscope)
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
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     '(("^ *\\([-]\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "•"))))
	       ("\\(\\\\deg\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "°"))))
	       ("\\(\\\\rArr\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "⇒"))))
	       ("\\(\\\\rarr\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "→"))))
	       ("\\(\\\\hArr\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "⇔"))))
	       ("\\(\\\\micro{}\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "µ"))))
	       ("\\(\\\\harr\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "↔"))))
	       ("\\(\\\\larr\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "←"))))
	       ("\\(\\\\isin\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "∈"))))
	       ("\\(\\\\approx\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "≈"))))
	       ("\\(\\\\sad\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "☹"))))
	       ("\\(\\\\ge\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "≥"))))
	       ("\\(\\\\le\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "≤3"))))
	       ("\\(\\\\trade\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "™"))))
	       ("\\(\\\\reg\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "®"))))
	       ("\\(\\\\ldquo\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "“"))))
	       ("\\(\\\\rdquo\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "”"))))
	       ("\\(\\\\smiley\\) "
		(0 (prog1 ()
		     (compose-region (match-beginning 1) (match-end 1) "☺"))))))))


(provide 'my-submodule)
