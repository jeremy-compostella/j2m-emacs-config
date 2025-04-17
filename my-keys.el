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
(global-set-key (kbd "C-M-;") 'windmove-right)

(require 'buffer-move)
(global-set-key (kbd "C-M-S-j") 'buf-move-down)
(global-set-key (kbd "C-M-S-k") 'buf-move-up)
(global-set-key (kbd "C-M-S-l") 'buf-move-left)
(global-set-key (kbd "C-M-:") 'buf-move-right)
(setq windmove-wrap-around t)

(defun my-quit ()
  (interactive)
  (quit-restore-window))

;; Personal global keys configuration
(global-set-key (kbd "C-x p") 'previous-error)
(global-set-key (kbd "C-x n") 'next-error)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-c q") 'my-quit)
(global-set-key (kbd "C-'") 'delete-backward-char)
(global-set-key (kbd "C-M-'") 'backward-kill-word)
(global-set-key (kbd "C-c SPC") (lambda ()
				  (interactive)
				  (with-current-buffer "*compilation*"
				    (recompile))))

;; Basic mode hooks
(defun my-code-mode-hook (map)
  (define-key map (kbd "M-n") 'forward-paragraph)
  (define-key map (kbd "M-p") 'backward-paragraph)
  (define-key map (kbd "C-c q") 'my-quit)
  (hs-minor-mode)
  (hs-hide-initial-comment-block)
  (setq show-trailing-whitespace t)
  (flyspell-prog-mode)
  (dtrt-indent-mode t))

(defun my-c-mode-hook ()
  (my-code-mode-hook c-mode-map)
  (set-fill-column 96))
(eval-after-load 'cc-mode
  '(add-hook 'c-mode-hook 'my-c-mode-hook))

(defun my-c++-mode-hook ()
  (my-code-mode-hook c++-mode-map))
(eval-after-load 'cc-mode
  '(add-hook 'c++-mode-hook 'my-c++-mode-hook))

(defun my-emacs-lisp-mode-hook ()
  (my-code-mode-hook emacs-lisp-mode-map)
  (set-fill-column 80))
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

(defun my-conf-mode-hook ()
  (my-code-mode-hook conf-mode-map))
(eval-after-load 'conf-mode
  '(add-hook 'conf-unix-mode-hook 'my-conf-mode-hook))

(defun my-python-mode-hook ()
  (my-code-mode-hook python-mode-map))
(eval-after-load 'python
  '(add-hook 'python-mode-hook 'my-python-mode-hook))

(defun my-java-mode-hook ()
  (my-code-mode-hook java-mode-map))
(eval-after-load 'cc-mode
  '(add-hook 'java-mode-hook 'my-java-mode-hook))

(defun release-my-global-keys (map)
  "I have defined some personal global key bindings that conflict
with some mode like org-mode for instance.  This function is used
to unset these local mode definitions."
  (define-key map (kbd "C-M-j") nil)
  (define-key map (kbd "C-M-k") nil)
  (define-key map (kbd "C-M-l") nil)
  (define-key map (kbd "C-M-;") nil)
  (define-key map (kbd "C-c SPC") nil))

(defun my-hexl-mode-hook ()
  (release-my-global-keys hexl-mode-map)
  (define-key hexl-mode-map (kbd "C-c q") 'my-quit))
(eval-after-load 'hexl
  '(add-hook 'hexl-mode-hook 'my-hexl-mode-hook))

(defun my-org-mode-hook ()
  (release-my-global-keys org-mode-map)
  (variable-pitch-mode)
  (visual-line-mode)
  (define-key org-mode-map (kbd "M-n") 'forward-paragraph)
  (define-key org-mode-map (kbd "M-p") 'backward-paragraph)
  (setq-local sort-fold-case t)
  (flyspell-mode)
  (turn-off-auto-fill))
(eval-after-load 'org
  '(add-hook 'org-mode-hook 'my-org-mode-hook))
(defun my-org-agenda-mode-hook ()
  (define-key org-agenda-mode-map (kbd "q") 'my-quit))
(eval-after-load 'org-agenda
  '(add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook))

(defun my-gnus-article-mode-hook ()
  (make-local-variable 'page-delimiter)
  (setq page-delimiter "")
  (define-key gnus-article-mode-map (kbd "M-n") 'forward-page)
  (define-key gnus-article-mode-map (kbd "M-p") 'backward-page)
  (define-key gnus-article-mode-map (kbd "C-c C-e") 'my-browse-mail))
(add-hook 'gnus-article-mode-hook 'my-gnus-article-mode-hook)

(defun my-org-agenda ()
  (interactive)
  (let ((org-agenda-include-inactive-timestamps t))
    (org-agenda-list)
    (org-agenda-check-type t 'agenda)
    (org-agenda-fortnight-view)
    (org-agenda-manipulate-query-add)
    (org-agenda-log-mode '(4))
    (goto-char (point-min))
    (search-forward "now - - - - - - - - - - " nil t)
    (recenter)
    (goto-char (line-beginning-position))
    (define-key org-agenda-mode-map (kbd "g") 'my-org-agenda)))

(global-set-key (kbd "C-c o")
		(define-prefix-command 'my-org-prefix))
(define-key 'my-org-prefix "f" 'org-switchb)
(define-key 'my-org-prefix "c" 'org-capture)
(define-key 'my-org-prefix "a" 'my-org-agenda)
(define-key 'my-org-prefix "t" 'org-todo-list)
(define-key 'my-org-prefix "l" 'org-store-link)

(defun my-text-mode-hook ()
  (set-fill-column 72)
  (turn-on-auto-fill)
  (flyspell-mode))
(eval-after-load 'text-mode
  '(add-hook 'text-mode-hook 'my-text-mode-hook))

(defun my-pdf-view-mode-hook ()
  (release-my-global-keys pdf-view-mode-map))
(eval-after-load 'view
  '(add-hook 'view-mode-hook 'my-pdf-view-mode-hook))

(defun my-shell-mode-hook ()
  (release-my-global-keys shell-mode-map))
(eval-after-load 'shell
  '(add-hook 'shell-mode-hook 'my-shell-mode-hook))

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
  (define-key gnus-summary-mode-map (kbd "<S-return>")
    'gnus-summary-scroll-down)
  (define-key gnus-summary-mode-map (kbd "M-g")
    (lambda ()
      (interactive)
      (my-gnus-kill-open-articles)
      (gnus-summary-rescan-group))))
(eval-after-load 'gnus-sum
  '(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-mode-hook))

(defun my-gnus-group-mode-hook ()
  (release-my-global-keys gnus-group-mode-map))
(eval-after-load 'gnus-group
  '(add-hook 'gnus-group-mode-hook 'my-gnus-group-mode-hook))

(defun my-message-mode-hook ()
  (release-my-global-keys message-mode-map))
(eval-after-load 'message
  '(add-hook 'message-mode-hook 'my-message-mode-hook))

(defun my-inferior-python-mode-hook ()
  (release-my-global-keys inferior-python-mode-map))
(eval-after-load 'python
  '(add-hook 'inferior-python-mode-hook 'my-inferior-python-mode-hook))

(defun my-conf-mode-hook ()
  (release-my-global-keys conf-mode-map))
(eval-after-load 'conf
  '(add-hook 'conf-mode-hook 'my-conf-mode-hook))

(defun my-magit-mode-hook ()
  (define-key magit-mode-map (kbd "M-n") 'diff-hunk-next)
  (define-key magit-mode-map (kbd "M-p") 'diff-hunk-prev))
(eval-after-load 'magit-mode
  '(add-hook 'magit-mode-hook 'my-magit-mode-hook))

(defun my-compilation-send-string (str)
  (interactive "sString to send: ")
  (comint-send-string (get-buffer-process (current-buffer)) str))
(defun my-compilation-mode-hook ()
  (define-key compilation-mode-map (kbd "p") 'compilation-previous-error)
  (define-key compilation-mode-map (kbd "n") 'compilation-next-error)
  (define-key compilation-mode-map (kbd "e") 'my-compilation-send-string)
  (define-key compilation-mode-map (kbd "SPC") 'compilation-display-error))
(eval-after-load 'compile
  '(add-hook 'compilation-mode-hook 'my-compilation-mode-hook))

(defun my-magit-buffer-list ()
  (let ((bufs (seq-filter (lambda (x)
			    (string-prefix-p "magit: " x))
			  (mapcar 'buffer-name (buffer-list)))))
    (if (string= (car bufs) (buffer-name (current-buffer)))
	(cdr bufs)
      bufs)))

(defun my-magit-switch-buffer (buffer-name)
  (interactive (list (ido-completing-read "Buffer: " (my-magit-buffer-list) t)))
  (switch-to-buffer buffer-name))

(global-set-key (kbd "C-c m")
		(define-prefix-command 'my-magit-prefix))
(define-key 'my-magit-prefix "m" 'my-magit-switch-buffer)
(define-key 'my-magit-prefix "s" 'magit-status)

(define-key 'my-magit-prefix "l" 'magit-log-head)
(define-key 'my-magit-prefix "b" 'magit-blame)
(define-key 'my-magit-prefix "r" 'magit-reflog-current)

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
