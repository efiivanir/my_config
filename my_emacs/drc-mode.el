;;; drc-mode.el --- sample major mode for editing calibre drc runsets. -*- coding: utf-8; lexical-binding: t; -*-


;; Author: Efi Ivanir ( efi.ivanir@gmail.com )
;; Version: 0.0
;; Created: 2023_03_12
;; Keywords: languages
;; Homepage: 

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; short description here

;; full doc on how to use here

;;; Code:

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq drc-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("VERBATIM" "#IFNDEF" "#ENDIF" "elseif" "else" "if" "#DEFINE" "FLAG" "while"))
            (x-types '("float" "integer" "key" "list" "rotation" "string" "vector"))
            (x-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
            (x-events '("at_rot_target" "at_target" "attach"))
            (x-functions '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-events-regexp (regexp-opt x-events 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-types-regexp . 'font-lock-type-face)
          (,x-constants-regexp . 'font-lock-constant-face)
          (,x-events-regexp . 'font-lock-builtin-face)
          (,x-functions-regexp . 'font-lock-function-name-face)
          (,x-keywords-regexp . 'font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode drc-mode c-mode "lsl mode"
  "Major mode for editing Calibre DRC runsets"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((drc-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'drc-mode)

;;; drc-mode.el ends here
;;###autoload
(add-to-list 'auto-mode-alist '("\\.drc" . drc-mode))
