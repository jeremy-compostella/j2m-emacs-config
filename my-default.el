;;; my-default.el --- My base configuration for GNU/Emacs

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

;; UI configuration
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      visible-bell t)

(setq-default cursor-type 'bar)

(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(column-number-mode t)
(show-paren-mode t)
;; (setq default-frame-alist '((undecorated . t)))

(setq custom-theme-directory (concat j2m-config-directory "/theme"))
(load-theme 'j2m t)

;; (cond ((eq system-type 'windows-nt)
;;        (set-default-font "-outline-Arial monospaced for SAP-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1" t t)
;;        (set-face-attribute 'default nil :height 80))
;;       ((eq system-type 'gnu/linux)
;;        (set-default-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" t t)
;;        ;; (set-face-attribute 'default nil :height 100)
;;        ))

;; Load local submodule
(require 'my-submodule)

;; Desktop mode and savehist
(desktop-save-mode t)
(savehist-mode t)

;; Dictionary
(setq ispell-dictionary "english")

;; My favorite file selection mode is IDO !
(ido-mode t)
(ido-everywhere t)
(setq ido-use-filename-at-point 'guess)

;; Server mode
(setq server-host "localhost"
      server-use-tcp nil)
(server-mode t)

;; Windows management
(winner-mode t)

;; I'm a lazy guy
(fset 'yes-or-no-p 'y-or-n-p)

;; Recursive minibuffer is tricky but so convenient
(setq enable-recursive-minibuffers t)

;; Base configuration
(setq message-log-max 10000
      history-length 500)
(set-language-environment 'utf-8)

;; I always want to be prompted to exit GNU/Emacs
(require 'tramp)
(defun confirm-exit-emacs ()
  "Ask for confirmation before exiting emacs."
  (interactive)
  (when (y-or-n-p "Are you sure you want to exit Emacs? ")
    (save-some-buffers)
    (tramp-cleanup-all-connections)
    (tramp-cleanup-all-buffers)
    t))
(add-hook 'kill-emacs-query-functions 'confirm-exit-emacs)

;; Tramp
;;  Allow /sudo on remote machines over ssh
(set-default 'tramp-default-proxies-alist
	     '((".*" "\\`root\\'" "/ssh:%h:")))

;; Dired mode
(setq dired-listing-switches "-alh")
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; Org-mode
(setq org-completion-use-ido t
      org-agenda-include-diary t
      org-agenda-window-setup 'other-window
      org-directory (expand-file-name "~/Documents")
      org-confirm-babel-evaluate nil)
(setq org-capture-templates
      `(("a" "Add an appointment into the agenda" entry
	 (file+headline ,(concat org-directory "/Agenda.org") "Appointments")
	 "* %?\n %^T\n ")
      	("e" "Add a task about Emacs" entry
	 (file+headline ,(concat org-directory "/EmacsTasks.org") "Tasks")
	 "* TODO %? %^G")
      	("n" "Add a note entry" entry
	 (file+headline ,(concat org-directory "/Notes.org") "Notes")
	 "* %? %^G\n %a\n")
	("t" "Add a task" entry
	 (file+headline ,(concat org-directory "/Tasks.org") "Tasks")
	 "* TODO %? %^G\n %^T\n%a\n ")))
(dolist (cur org-capture-templates)
  (let ((target (nth 3 cur)))
    (when (or (eq (car target) 'file)
	      (eq (car target) 'file+headline))
      (add-to-list 'org-agenda-files (cadr (nth 3 cur))))))

;; Automatic org-agenda to appt
(defun update-appt-from-org-agenda ()
  (let ((already-open (get-buffer "Agenda.org"))
	(buf (find-file-noselect (concat org-directory "/Agenda.org"))))
    (when (or (not already-open) (buffer-modified-p buf))
      (with-current-buffer buf (save-buffer))
      (org-agenda-to-appt t))))
(run-at-time t 60 'update-appt-from-org-agenda)

(defface my-org-todo-base-face
  '((t :weight bold :box (:line-width 2 :style released-button) :inherit default))
  "Base face for org-todo")
(face-spec-set 'org-todo
  '((t :background "red3" :foreground "white" :inherit my-org-todo-base-face)))
(face-spec-set 'org-done
  '((t :background "green4" :foreground "white" :inherit my-org-todo-base-face)))
(defface my-org-todo-on-hold-face
  '((t :background "#0071c5" :foreground "white" :inherit my-org-todo-base-face))
  "Face used for todo keywords that indicate \"on-hold\" items.")
(defface my-org-todo-in-progress-face
  '((t :background "yellow2" :foreground "black" :inherit my-org-todo-base-face))
  "Face used for todo keywords that indicate \"in-progress\" items.")
(defface my-org-todo-almost-done-face
  '((t :background "yellow" :foreground "black" :inherit my-org-todo-base-face))
  "Face used for todo keywords that indicate \"almost-done\" items.")

(setq org-todo-keyword-faces
      '(("IN-ANALYSIS"	.	my-org-todo-in-progress-face)
	("IN-PROGRESS"	.	my-org-todo-in-progress-face)
	("IMPLEMENTED"	.	my-org-todo-almost-done-face)
	("VALIDATION"	.	my-org-todo-almost-done-face)
	("SUBMITTED"	.	my-org-todo-almost-done-face)))

;; Email configuration
(require 'smtpmail)
(setq ;; SMTP
      user-mail-address "jeremy.compostella@gmail.com"
      user-full-name "Jeremy Compostella"
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      ;; GNUS
      gnus-select-method '(nnimap "local"
      mail-user-agent 'gnus-user-agent
      message-forward-as-mime t
      message-make-forward-subject-function 'message-forward-subject-fwd
      gnus-select-method '(nnimap "localhost"
				  (nnimap-address "localhost")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)
				  (nnimap-inbox "INBOX"))
      gnus-use-scoring t
      gnus-use-adaptive-scoring t
      gnus-use-full-window nil
      gnus-summary-line-format "%U%R%(%-14,14&user-date; %*%-15,15f%B %us%)\n"
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root " ● "
      gnus-sum-thread-tree-false-root " ○ "
      gnus-sum-thread-tree-single-indent " ● "
      gnus-sum-thread-tree-leaf-with-other " ├► "
      gnus-sum-thread-tree-single-leaf " ╰► "
      gnus-sum-thread-tree-vertical " │"
      gnus-thread-sort-functions '(gnus-thread-sort-by-date))

(require 'ispell)
(require 'flyspell)
(defun my-message-edit-hook ()
  (flyspell-mode))
(add-hook 'message-mode-hook 'my-message-edit-hook)

(add-hook 'gnus-before-startup-hook 'offlineimap)

;; Compilation
(setq compilation-scroll-output t
      compilation-read-command nil
      compilation-window-height 15
      compilation-ask-about-save nil)

;; Status
(status-add-to-right 'status-gnus)
(turn-on-status)

;; Package locations
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; Shell
(setq explicit-shell-file-name "/bin/bash")

;; My key shortcuts
(require 'my-keys)

(provide 'my-default)
