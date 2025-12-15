(setq initial-scratch-message
      ";;Iva-emacs DID NOT LOAD CORRECTLY.
;;Check the warnings and messages buffers, or restart with --debug-init")

(defconst ivanir-custom "emacs-custom.el"
    "name of the customization file")

(defconst ivanir-directory
    (file-name-directory (or load-file-name (buffer-file-name)))
    "Directory where init.el exists, this will be the root dir")

(defconst user-emacs-directory (concat ivanir-directory "my_emacs_d")
   "Directory which replace ~/.emacs.d")

(defconst package-user-dir (concat user-emacs-directory "/elpa")
   "Directory where external packages will be install")

(defconst ivanir-themes-dir (concat user-emacs-directory "themes")
   "location of the themes directory")
(setq custom-theme-directory ivanir-themes-dir)

(defconst ivanir-custom-file (locate-user-emacs-file ivanir-custom)
    "location of the customization file")

  ;; Save any custom set variable in ivanir-custom-file rather than at the end of init.el:
(setq custom-file ivanir-custom-file)
(load custom-file 'noerror 'nomessage)

(setq user-full-name "Efi Ivanir"
      user-mail-address "efi.ivanirnir@gmail.com")

(defconst ivanir-xwindow (eq window-system 'x)
  "Non-nil if we are in X-Window")

(defconst ivanir-current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" .   "https://orgmode.org/elpa/")
                         ("elpa" .  "https://elpa.gnu.org/packages/")))

(package-initialize)
(package-refresh-contents)
(when (not package-archive-contents)
    (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)

(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)


;; Definitions from better defaults
(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
;; =================================
;; End
;; ================================
(setq initial-scratch-message
      (let ((current-user (split-string (user-full-name) " ")))
        (format ";; Happy hacking %s!" (if current-user (car current-user) ivanir-current-user))))



