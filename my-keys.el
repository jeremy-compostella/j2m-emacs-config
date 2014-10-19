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

;; Personal global key configuration
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-c q") 'quit-window)

;; Basic mode hook
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

(defun release-my-move-keys (map)
  "I use C-M-[jklm] keys to navigate respectively to the down,
up, left and right window.  Some mode like org-mode for instance
use these keys too.  This function is used to unset these local
mode definitions."
  (define-key map (kbd "C-M-j") nil)
  (define-key map (kbd "C-M-k") nil)
  (define-key map (kbd "C-M-l") nil)
  (define-key map (kbd "C-M-m") nil))

(defun my-hexl-mode-hook ()
  (release-my-move-keys hexl-mode-map))
(eval-after-load 'hexl
  '(add-hook 'hexl-mode-hook 'my-hexl-mode-hook))

(defun my-org-mode-hook ()
  (release-my-move-keys org-mode-map))
(add-hook 'org-mode-hook 'my-org-mode-hook)
(defun my-org-agenda-mode-hook ()
  (define-key org-agenda-mode-map (kbd "q") 'quit-window))
(add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)
(global-set-key (kbd "C-c o f") 'org-switchb)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o a") 'org-agenda-list)
(global-set-key (kbd "C-c o t") 'org-todo-list)

(require 'magit)
(defun my-magit-mode-hook ()
  (define-key magit-mode-map (kbd "M-n") 'diff-hunk-next)
  (define-key magit-mode-map (kbd "M-p") 'diff-hunk-prev))
(add-hook 'magit-mode-hook 'my-magit-mode-hook)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)
(global-set-key (kbd "C-c m b") 'magit-blame-mode)
(global-set-key (kbd "C-c m r") 'magit-reflog)

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
