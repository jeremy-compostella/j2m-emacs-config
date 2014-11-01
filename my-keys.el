;;; my-keys.el --- My keys configuration for GNU/Emacs

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

;; Window movements
(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-l") 'windmove-left)
(global-set-key (kbd "C-M-m") 'windmove-right)
(global-set-key (kbd "C-x à") 'kill-buffer-and-window)

;; Personal global keys configuration
(global-set-key (kbd "C-x p") 'previous-error)
(global-set-key (kbd "C-x n") 'next-error)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-c q") 'quit-window)
(global-set-key (kbd "C-ù") 'delete-backward-char)
(global-set-key (kbd "C-M-ù") 'backward-kill-word)
(global-set-key (kbd "C-x m") 'message-mail)
(global-set-key (kbd "C-c SPC") (lambda ()
				  (interactive)
				  (with-current-buffer "*compilation*"
				    (recompile))))

;; Basic mode hooks
(defun my-code-mode-hook (map)
  (define-key map (kbd "M-n") 'forward-paragraph)
  (define-key map (kbd "M-p") 'backward-paragraph)
  (hs-minor-mode)
  (hs-hide-initial-comment-block)
  (setq show-trailing-whitespace t)
  (flyspell-prog-mode)
  (dtrt-indent-mode t))

(defun my-c-mode-hook ()
  (my-code-mode-hook c-mode-map))
(eval-after-load 'cc-mode
  '(add-hook 'c-mode-hook 'my-c-mode-hook))

(defun my-emacs-lisp-mode-hook ()
  (my-code-mode-hook emacs-lisp-mode-map))
(eval-after-load 'lisp-mode
  '(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook))

(defun my-makefile-mode-hook ()
  (my-code-mode-hook makefile-mode-map))
(eval-after-load 'make-mode
  '(add-hook 'makefile-mode-hook 'my-makefile-mode-hook))

(defun my-sh-mode-hook ()
  (my-code-mode-hook sh-mode-map))
(eval-after-load 'sh-script
  '(add-hook 'sh-mode-hook 'my-sh-mode-hook))

(defun release-my-global-keys (map)
  "I have defined some personal global key bindings that conflict
with some mode like org-mode for instance.  This function is used
to unset these local mode definitions."
  (define-key map (kbd "C-M-j") nil)
  (define-key map (kbd "C-M-k") nil)
  (define-key map (kbd "C-M-l") nil)
  (define-key map (kbd "C-M-m") nil)
  (define-key map (kbd "C-c SPC") nil))

(defun my-hexl-mode-hook ()
  (release-my-global-keys hexl-mode-map))
(eval-after-load 'hexl
  '(add-hook 'hexl-mode-hook 'my-hexl-mode-hook))

(defun my-org-mode-hook ()
  (release-my-global-keys org-mode-map)
  (set-fill-column 80)
  (turn-on-auto-fill)
  (flyspell-mode))
(add-hook 'org-mode-hook 'my-org-mode-hook)
(defun my-org-agenda-mode-hook ()
  (define-key org-agenda-mode-map (kbd "q") 'quit-window))
(add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)

(global-set-key (kbd "C-c o")
		(define-prefix-command 'my-org-prefix))
(define-key 'my-org-prefix "f" 'org-switchb)
(define-key 'my-org-prefix "c" 'org-capture)
(define-key 'my-org-prefix "a" 'org-agenda-list)
(define-key 'my-org-prefix "t" 'org-todo-list)

(defun my-shell-mode-hook ()
  (release-my-global-keys shell-mode-map))
(eval-after-load 'shell
  '(add-hook 'shell-mode-hook 'my-shell-mode-hook))

(defun my-notmuch-mode-hook ()
  (release-my-global-keys notmuch-show-mode-map))
(eval-after-load 'notmuch
  '(add-hook 'notmuch-show-hook 'my-notmuch-mode-hook))

(defun my-diff-mode-hook ()
  (release-my-global-keys diff-mode-map))
(eval-after-load 'diff
  '(add-hook 'diff-mode-hook 'my-diff-mode-hook))

(defun my-gnus-show-my-inbox ()
  (interactive)
  (unless (gnus-alive-p)
    (gnus)
    (quit-window))
  (gnus-summary-read-group "INBOX" nil t))
(global-set-key (kbd "C-x M") 'my-gnus-show-my-inbox)

(defun my-gnus-kill-open-articles ()
  (interactive)
  (save-selected-window
    (dolist (w (window-list-1))
      (with-selected-window w
	(when (or (eq major-mode 'gnus-article-mode)
		  (string= "*Article INBOX*" (buffer-name (current-buffer))))
	  (kill-buffer-and-window))))))

(defun my-gnus-summary-mode-hook ()
  (release-my-global-keys gnus-summary-mode-map)
  (define-key gnus-summary-mode-map (kbd "q")
    (lambda ()
      (interactive)
      (my-gnus-kill-open-articles)
      (quit-window)))
  (define-key gnus-summary-mode-map (kbd "M-g")
    (lambda ()
      (interactive)
      (my-gnus-kill-open-articles)
      (gnus-summary-rescan-group))))
(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-mode-hook)

(defun my-gnus-group-mode-hook ()
  (release-my-global-keys gnus-group-mode-map))
(add-hook 'gnus-group-mode-hook 'my-gnus-group-mode-hook)

(defun my-message-mode-hook ()
  (release-my-global-keys message-mode-map))
(add-hook 'message-mode-hook 'my-message-mode-hook)

(defun my-magit-mode-hook ()
  (define-key magit-mode-map (kbd "M-n") 'diff-hunk-next)
  (define-key magit-mode-map (kbd "M-p") 'diff-hunk-prev))
(add-hook 'magit-mode-hook 'my-magit-mode-hook)

(global-set-key (kbd "C-c m")
		(define-prefix-command 'my-magit-prefix))
(define-key 'my-magit-prefix "s" 'magit-status)
(define-key 'my-magit-prefix "l" 'magit-log)
(define-key 'my-magit-prefix "b" 'magit-blame-mode)
(define-key 'my-magit-prefix "r" 'magit-reflog)

(global-set-key (kbd "C-x d") 'duplicate-lines)
(global-set-key (kbd "C-c k") 'kill-line-at-point)
(global-set-key (kbd "C-x c l") 'copy-line-at-point)
(global-set-key (kbd "C-x c s") 'copy-symbol-at-point)
(global-set-key (kbd "C-x c w") 'copy-word-at-point)

(global-set-key (kbd "C->") 'increment-number-at-point)
(global-set-key (kbd "C-<") 'decrement-number-at-point)
(global-set-key (kbd "M-P") (lambda () (interactive) (move-lines -1)))
(global-set-key (kbd "M-N") (lambda () (interactive) (move-lines 1)))

(global-set-key (kbd "M-.") 'rscope-find-global-definition)
(global-set-key (kbd "M-,") 'rscope-find-this-symbol)

(provide 'my-keys)
