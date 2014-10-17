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

(cond ((eq system-type 'windows-nt)
       (set-default-font "-outline-Arial monospaced for SAP-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")
       (set-face-attribute 'default nil :height 80))
      ((eq system-type 'gnu/linux)
       (set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")))

(setq custom-theme-directory (concat j2m-config-directory "/theme"))
(load-theme 'j2m t)

;; Desktop mode and savehist
(desktop-save-mode t)
(savehist-mode t)

;; My favorite file selection mode is IDO !
(ido-mode t)
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

;; I always want to be prompter to exit GNU/Emacs
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

;; Dired mode
(setq dired-listing-switches "-alh")

;; ERC
(autoload 'erc-nick-notify-mode "erc-nick-notify"
  "Minor mode that calls `erc-nick-notify-cmd' when his nick gets
mentioned in an erc channel" t)
(eval-after-load "erc" '(erc-nick-notify-mode t))

;; My key shortcuts
(require 'my-keys)

(provide 'my-default)
