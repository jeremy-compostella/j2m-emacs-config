;; Copyright (C) 2014 Jérémy Compostella <jeremy.compostella@gmail.com>

;; Based on tango-dark-theme.el

;; Thanks to Chong Yidong <cyd@stupidchicken>
;;           Jan Moringen <jan.moringen@uni-bielefeld.de>

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

(require 'color)

(deftheme j2m
  "Face colors using the Tango palette (dark background).
Basic, Font Lock, Isearch, Gnus, Message, Ediff, Flyspell,
Semantic, and Ansi-Color faces are included.")

(let ((class '((class color) (min-colors 89)))
      ;; Tango palette colors.
      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#0071c5")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (plum-1 "#e090d7") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#555753") (alum-6 "#262B2C")
      ;; Not in Tango palette; used for better contrast.
      (cham-0 "#b4fa70") (blue-0 "#8cc4ff") (plum-0 "#e9b2e3")
      (red-0 "#ff4b4b")  (alum-5.5 "#41423f") (alum-7 "#212526")
      (intel "#0071c5") (intel-grey "#384854"))

  (custom-theme-set-faces
   'j2m
   ;; Ensure sufficient contrast on low-color terminals.
   '(variable-pitch ((t (:family "Intel Clear" :height 120))))
   '(fixed-pitch ((t (:font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
			    :height 100))))
   `(default ((((class color) (min-colors 4096))
   	       (:foreground ,alum-1 :background ,alum-6))
   	      (((class color) (min-colors 256))
   	       (:foreground ,alum-1 :background "#222"))
   	      (,class
   	       (:foreground ,alum-1 :background "black"))))
   '(default ((t (:font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))))
   `(cursor ((,class (:background ,red-1))))
   ;; Highlighting faces
   `(fringe ((,class (:background ,alum-7))))
   `(highlight ((,class (:foreground nil :background ,intel-grey))))
   `(region ((,class (:background ,alum-5))))
   `(secondary-selection ((,class (:background ,blue-3))))
   `(isearch ((,class (:foreground ,alum-1 :background ,intel))))
   `(lazy-highlight ((,class (:background ,intel-grey))))
   `(trailing-whitespace ((,class (:background ,red-3))))
   ;; Mode line faces
   `(mode-line ((,class
		 (:background ,intel :foreground ,alum-1))))
   `(mode-line-inactive ((,class
			   :background ,intel-grey :foreground ,alum-1)))
   `(compilation-mode-line-fail ((,class (:foreground ,red-3))))
   `(compilation-mode-line-run  ((,class (:foreground ,intel))))
   `(compilation-mode-line-exit ((,class (:foreground ,cham-3))))
   `(compilation-info ((,class (:foreground ,intel))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,blue-0))))
   `(escape-glyph ((,class (:foreground ,butter-3))))
   `(error ((,class (:foreground "red"))))
   `(warning ((,class (:foreground ,orange-1))))
   `(success ((,class (:foreground ,cham-1))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,plum-1))))
   `(font-lock-comment-face ((,class (:foreground ,blue-1))))
   `(font-lock-constant-face ((,class (:foreground ,plum-0))))
   `(font-lock-function-name-face ((,class (:foreground ,butter-1))))
   `(font-lock-keyword-face ((,class (:foreground ,cham-0))))
   `(font-lock-string-face ((,class (:foreground ,choc-1))))
   `(font-lock-type-face ((,class (:foreground ,blue-0))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange-1))))
   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-0))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground ,plum-1))))
   `(gnus-group-news-1-low ((,class (:foreground ,plum-2))))
   `(gnus-group-news-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-news-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-news-3 ((,class (:foreground ,cham-1))))
   `(gnus-group-news-3-low ((,class (:foreground ,cham-2))))
   `(gnus-group-news-4 ((,class (:foreground ,plum-0))))
   `(gnus-group-news-4-low ((,class (:foreground ,choc-2))))
   `(gnus-group-news-5 ((,class (:foreground ,orange-1))))
   `(gnus-group-news-5-low ((,class (:foreground ,orange-2))))
   `(gnus-group-news-low ((,class (:foreground ,butter-2))))
   `(gnus-group-mail-1 ((,class (:foreground ,plum-1))))
   `(gnus-group-mail-1-low ((,class (:foreground ,plum-2))))
   `(gnus-group-mail-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-mail-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-mail-3 ((,class (:foreground ,cham-1))))
   `(gnus-group-mail-3-low ((,class (:foreground ,cham-2))))
   `(gnus-group-mail-low ((,class (:foreground ,butter-2))))
   `(gnus-header-content ((,class (:weight normal :foreground ,butter-3))))
   `(gnus-header-from ((,class (:foreground ,butter-2))))
   `(gnus-header-subject ((,class (:foreground ,cham-1))))
   `(gnus-header-name ((,class (:foreground ,blue-1))))
   `(gnus-header-newsgroups ((,class (:foreground ,choc-2))))
   `(gnus-summary-high-ancient ((,class (:foreground ,alum-1))))
   `(gnus-summary-high-read ((,class (:foreground ,alum-1))))
   `(gnus-summary-high-ticked ((,class (:foreground "#e090d7"))))
   `(gnus-summary-high-undownloaded ((,class (:foreground ,red-0))))
   `(gnus-summary-high-unread ((,class (:foreground ,alum-1 :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,choc-1))))
   `(gnus-summary-low-read ((,class (:foreground ,choc-1))))
   `(gnus-summary-low-ticked ((,class (:foreground "gold"))))
   `(gnus-summary-low-undownloaded ((,class (:foreground ,choc-1))))
   `(gnus-summary-low-unread ((,class (:foreground ,choc-1))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,alum-1))))
   `(gnus-summary-normal-read ((,class (:foreground ,alum-1))))
   `(gnus-summary-normal-ticked ((,class (:foreground "gold"))))
   `(gnus-summary-normal-undownloaded ((,class (:foreground ,alum-1))))
   `(gnus-summary-normal-unread ((,class (:foreground ,alum-1 :weight bold))))
   ;; Message faces
   `(message-header-name ((,class (:foreground ,blue-1))))
   `(message-header-cc ((,class (:foreground ,butter-3))))
   `(message-header-other ((,class (:foreground ,choc-2))))
   `(message-header-subject ((,class (:foreground ,cham-1))))
   `(message-header-to ((,class (:foreground ,butter-2))))
   `(message-cited-text ((,class (:foreground ,cham-1))))
   `(message-separator ((,class (:foreground ,plum-1))))
   ;; SMerge faces
   `(smerge-refined-change ((,class (:background ,blue-3))))
   ;; Ediff faces
   `(ediff-current-diff-A ((,class (:background ,alum-5))))
   `(ediff-fine-diff-A ((,class (:background ,orange-2))))
   `(ediff-even-diff-A ((,class (:background ,choc-2))))
   `(ediff-odd-diff-A ((,class (:background ,alum-5.5))))
   `(ediff-current-diff-B ((,class (:background ,alum-5))))
   `(ediff-fine-diff-B ((,class (:background ,orange-2))))
   `(ediff-even-diff-B ((,class (:background ,choc-2))))
   `(ediff-odd-diff-B ((,class (:background ,alum-5.5))))
   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline (:color ,orange-1 :style wave)))))
   `(flyspell-incorrect ((,class (:underline (:color ,red-3 :style wave)))))
   ;; Semantic faces
   `(semantic-decoration-on-includes ((,class (:underline ,alum-4))))
   `(semantic-decoration-on-private-members-face
     ((,class (:background ,plum-3))))
   `(semantic-decoration-on-protected-members-face
     ((,class (:background ,choc-3))))
   `(semantic-decoration-on-unknown-includes
     ((,class (:background ,red-3))))
   `(semantic-decoration-on-unparsed-includes
     ((,class (:background ,alum-5.5))))
   `(semantic-tag-boundary-face ((,class (:overline ,blue-1))))
   `(semantic-unmatched-syntax-face ((,class (:underline ,red-1))))
   ;; Status
   `(status-time-face ((,class (:height 1.0 :foreground ,blue-1 :weight bold))))
   `(status-date-face ((,class (:foreground ,blue-0 :weight bold))))
   `(status-project-manager-face ((,class (:height 1.0 :foreground ,blue-0 :weight bold))))
   `(status-battery-charging-face ((,class (:foreground ,cham-0))))
   `(status-battery-charged-face ((, class (:foreground ,alum-1))))
   `(status-battery-discharging-face ((, class (:foreground ,red-1))))
   `(status-purple-face ((, class (:foreground "white" :background ,intel))))
   ;; Org Mode
   `(org-date ((, class (:foreground ,alum-3))))
   `(org-tag ((, class (:slant italic :foreground ,alum-3 :height 1.0))))
   `(org-level-1 ((, class (:foreground "yellow"  :height 1.2))))
   `(org-ellipsis ((, class (:foreground "LightGoldenrod"))))
   `(org-checkbox-statistics-todo ((, class ())))
   `(org-checkbox-statistics-done ((, class ())))
   `(org-formula ((t (:inherit fixed-pitch))))
   `(org-link ((t (:inherit fixed-pitch :foreground "#8cc4ff" :underline t))))
   `(org-table ((t (:inherit fixed-pitch :foreground ,blue-0))))
   `(org-meta-line ((t (:inherit fixed-pitch :foreground ,blue-0))))
   '(org-code ((t (:inherit fixed-pitch :foreground "grey70"))))
   `(org-quote ((t (:inherit variable-pitch :slant italic :foreground ,blue-0
			     :background ,(color-darken-name (face-attribute 'default :background) 3)))))
   `(org-block ((t (:inherit fixed-pitch
			     :background ,(color-darken-name (face-attribute 'default :background) 3)))))
   '(org-document-info ((t (:inherit fixed-pitch :foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))   ;; Powerline
   `(powerline-active0 ((,class (:inherit mode-line :box (:line-width 3 :color ,intel)))))
   `(powerline-inactive0 ((,class (:background "grey30" :foreground "white" :inherit mode-line :box (:line-width 3 :color "grey30")))))
   ;; ERC
   `(erc-input-face ((,class (:foreground ,blue-1))))
   `(erc-my-nick-face ((,class (:foreground ,blue-1)))))

  (custom-theme-set-variables
   'j2m
   `(ansi-color-names-vector [,alum-7 ,red-0 ,cham-0 ,butter-1
			      ,blue-1 ,plum-1 ,blue-0 ,alum-1])))

(provide-theme 'j2m)

;; Local Variables:
;; no-byte-compile: t
;; End:
