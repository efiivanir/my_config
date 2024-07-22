(setq initial-scratch-message
      ";;Iva-emacs DID NOT LOAD CORRECTLY.
;;Check the warnings and messages buffers, or restart with --debug-init")

(defconst iva-custom "emacs-custom.el"
    "name of the customization file")

(defconst iva-directory
    (file-name-directory (or load-file-name (buffer-file-name)))
    "Directory where init.el exists, this will be the root dir")

(defconst user-emacs-directory (concat iva-directory "emacs.d")
   "Directory which replace ~/.emacs.d")

(defconst package-user-dir (concat user-emacs-directory "/elpa")
   "Directory where external packages will be install")

(defconst iva-themes-dir (concat iva-directory "themes")
   "location of the themes directory")
(setq custom-theme-directory iva-themes-dir)

(defconst iva-custom-file (locate-user-emacs-file iva-custom)
    "location of the customization file")
  ;; Save any custom set variable in iva-custom-file rather than at the end of init.el:
(setq custom-file iva-custom-file)
(load custom-file 'noerror 'nomessage)

(setq user-full-name "Efi Ivanir"
      user-mail-address "efi.ivanir@gmail.com")

(defconst iva-xwindow (eq window-system 'x)
  "Non-nil if we are in X-Window")

(defconst iva-current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
             ("org" . "https://orgmode.org/elpa/")
             ("elpa" . "https://elpa.gnu.org/packages/")))
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

;; =========================================
;; savehist-mode
;; ========================================
;;Remembering minibuffer prompt history
;;One thing you will do a lot in Emacs is enter text into minibuffer prompts. Everything from M-x, isearch, the describe-* commands, and even
;;the shell modes will receive a lot of input from you over time.

;;You’ll quickly realize that it would be helpful for Emacs to remember the things you’ve entered into these prompts the next time
;;you use them. That’s where the savehist-mode comes in!

;;When you enable this mode, you will be able to use M-n (next-history-element) and M-p (previous-history-element) key bindings in almost every
;;minibuffer (and shell) prompt to call up the inputs you used previously for the current command.

;;I also like to set the history-length to a reasonable number to reduce the impact that reading these history files can have on Emacs’ startup performance.
(setq history-length 25)
(savehist-mode 1)



(setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        backup-by-copying t
        frame-inhibit-implied-resize t
        ediff-window-setup-function 'ediff-setup-windows-plain
        custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))))

(setq default-frame-alist
      '((top . 20) (left . 5)
        (width . 200) (height . 50)
        ;; (cursor-color . "white")
        ;; (cursor-type . box)
        ;; (foreground-color . "yellow")
        ;; (background-color . "black")
        ;; (font . "-*-Courier-normal-r-*-*-13-*-*-*-c-*-iso8859-1")
       )
)

(menu-bar-mode +1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode +1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode +1))


;; ===================================
;; Basic Customization
;; ===================================
(setq inhibit-startup-message t)    ;; Hide the startup message

(use-package material-theme
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'material t)   
)  

;; (use-package leuven-theme
;;   :config
;;   (load-theme 'leuven-dark t))


(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                woman-mode-hook
                man-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(fset 'yes-or-no-p 'y-or-n-p)
;;; Delete selection when typing
(delete-selection-mode t)
(setq debug-on-error t)
(transient-mark-mode 1) ;; No region when it is not highlighted

;;; CUA mode
;; (cua-mode t)
;; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;; (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

  ;;; Aditional fonts
(defvar efs/default-font-size 150)
(defvar efs/default-variable-font-size 150)
(set-face-attribute 'default nil :height 140)


;; ===================================
;; Customization of faces,etc.
;; ===================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (awk . t)
     (python . t)
     (fortran . t)
     (latex . t)
     (lilypond . t)
     (lua . t)))
 '(org-emphasis-alist
   '(("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("+"
      (:strike-through t))
     ("~"
      (:overline t)
      verbatim)))
 '(package-selected-packages
   '(gnu-elpa-keyring-update php-mode magit use-package symon rainbow-mode rainbow-delimiters pdf-tools org-super-agenda org-bullets neotree hydra htmlize highlight-indent-guides helm-bibtex guide-key guess-language fortpy elpy dired-subtree dired-rainbow calfw-org calfw-ical calfw auctex-lua auctex-latexmk))
 
 '(template-use-package t nil (template)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mu4e-context-face ((t (:foreground "dark green" :weight bold :background "grey"))))
 '(org-ellipsis ((t (:foreground "magenta" :underline t :overline t :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "pink"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "light blue"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "light green"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "gray")))))

;; ==============================================
;; helpful
;; =============================================
;; Helpful is an alternative to the built-in Emacs help that provides much more contextual information.
(use-package helpful
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)
    
  :ensure t
)
;; ==============================================
;; Rainbow Delimiters
;; =============================================
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; ==================================================
;; recentf-mode
;;==================================================
;;Remembering recently edited files
;;When you do a lot of work with Emacs, you will probably want to get back to files you recently edited.
;;Instead of using find-file to go hunt those files down again, you can enable recentf-mode to have Emacs
;;remember the files you edited most recently:
(recentf-mode 1)

;; ==================================================
;; save-place-mode
;;==================================================
;; Remembering the last place you visited in a file
;; Sometimes it’s convenient for Emacs to remember the last location you were at when you visited a particular file.
;;The save-place-mode can help with that!
;; Once you turn on this mode, Emacs will drop your cursor to the last visited location in any file that you open.
;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; ==================================================
;; global-auto-revert
;;==================================================
;;Automatically revert buffers for changed files
;;One thing that can be annoying about Emacs when you first start using it is that it doesn’t automatically refresh file
;;buffers when the file on disk has been changed outside of Emacs. This can often happen when you’re using tools that generate
;;some kind of text file output that you need to read in an Emacs buffer.
;;The global-auto-revert-mode will make Emacs watch the files for all open buffers for changes on disk and it will automatically
;;refresh those buffers if they don’t have unsaved changes!

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;;I also like adding the following setting to cause other types of buffers in Emacs to update when related files on disk have changed.
;;The place this is most useful is when you’re using Emacs’ excellent Dired package! The following setting will cause Dired buffers
;;to be automatically refreshed when files get added or deleted from the directory you are browsing:

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)


;; ===================================
;; pdf-tools
;; ===================================
(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install)
  (pdf-loader-install)
)

;; ===================================
;; Bind keys
;; ===================================
(define-key global-map [(control z)] (function undo))
(define-key global-map [(control a)] (function mark-whole-buffer))
(define-key global-map [(control f)] (function recentf-open-files))

(global-set-key [(control ?`)] (function kill-this-buffer))

(defun insert-gui-primary-selection ()
    "If no region is selected, insert current gui selection at point."
    (interactive)
    (when (not (use-region-p))
      (let ((text (gui-get-selection)))
	(when text
	  (push-mark (point))
	  (insert-for-yank text)))))
(global-set-key [(meta insert)] #'insert-gui-primary-selection)

;;; Meta-Control-L = switch to last buffer
(defun switch-to-other-buffer ()
   "Alternates between the two most recent buffers"
   (interactive)
   (switch-to-buffer (other-buffer)))
(define-key global-map [(meta control l)] (function switch-to-other-buffer))

;; =================================
;; emojify-mode
;; ================================
(use-package emoji-github
)

(use-package emojify
  :hook (after-init . global-emojify-mode))


;; =================================
;; Python development
;; ================================
(use-package elpy :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package flycheck :ensure t
  :defer t
  )
(with-eval-after-load 'elpy
  (delete 'elpy-module-flymake elpy-modules))

;; ;; Enable Flycheck
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode)
;; )

(use-package py-autopep8 :ensure t
  :defer t
;  :hook (elpy-mode . py-autopep8-enable-on-save)
  )

;; Use IPython for REPL
(setq python-shell-interpreter "ipython"
;      python-shell-interpreter-args "console --simple-prompt"
      python-shell-interpreter-args "--pylab"
      
      python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "ipython")


;; =================================
;; Yasnippet
;; ================================
(use-package yasnippet)
(use-package yasnippet-snippets)
(yas-global-mode 1) ; always on

;;; Directory tree where to find snippets (subdirectories must be mode names).
;;; The t means JIT loading, which saves time during Emacs startup.
(yas-load-directory (locate-user-emacs-file "snippets") t)

;;; Enable YAS only for C++
(add-hook 'c-mode-common-hook
          '(lambda ()
             (yas-minor-mode)))

;;; Trigger key
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)

;; =================================
;; Helm definitoion
;; ================================
(use-package helm
  :init
    ;;(require 'helm-config)
    (setq helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t)
  :config 
    (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
    (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
    (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
    (global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
    (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
    (global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
    (global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
    (global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
    (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste

    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (when (executable-find "curl")
      (setq helm-net-prefer-curl t))
    

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t
        helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
        helm-recentf-fuzzy-match t
        helm-autoresize-mode t)
    
    
    (defun spacemacs//helm-hide-minibuffer-maybe ()
      "Hide minibuffer in Helm session if we use the header line as input field."
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face
                       (let ((bg-color (face-background 'default nil)))
                         `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))


    (add-hook 'helm-minibuffer-set-up-hook
              'spacemacs//helm-hide-minibuffer-maybe)

    ;; (setq helm-autoresize-max-height 0)
    ;; (setq helm-autoresize-min-height 20)
    
)

(use-package helm-org

  )
(use-package html2org

  )

;; =================================
;; C++ definitoion
;; ================================
;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration))))
  :init
  (setq lsp-keep-workspace-alive nil
        lsp-signature-doc-lines 5
        lsp-idle-delay 0.5
        lsp-prefer-capf t
        lsp-client-packages nil)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
)

(use-package lsp-treemacs
)

(use-package helm-lsp
)

(use-package projectile
)

(use-package hydra
)
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

(use-package company
)

(use-package avy
)

(use-package which-key
  :config 
    (setq which-key-idle-delay 0.3)
    (setq which-key-popup-type 'frame)
    (which-key-mode)
    (which-key-setup-minibuffer)
    (set-face-attribute 'which-key-local-map-description-face nil 
  :weight 'bold)
  :ensure t
)


(use-package helm-xref
)

(use-package dap-mode
)
;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))


;; =================================
;; Org-mode
;; ================================


;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)
(use-package org-view-mode
  :ensure t)

;; =================================
;; Ivanir small procedures
;; ================================
;; My version of a function to duplicate a line that works nice with undo and doesn't mess with the cursor position.
;; It was the result of a discussion in gnu.emacs.sources from November 1997.
(defun iva-duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))
(global-set-key (kbd "C-d") 'iva-duplicate-line)

;; =================================
;; End
;; ================================
(setq initial-scratch-message
      (let ((current-user (split-string (user-full-name) " ")))
        (format ";; Happy hacking %s!" (if current-user (car current-user) iva-current-user))))



;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   (mapc #'disable-theme custom-enabled-themes)
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil)

;;   ;; Maybe define some palette overrides, such as by using our presets
;;   (setq modus-themes-common-palette-overrides
;;         modus-themes-preset-overrides-intense)

;;   ;; Load the theme of your choice.
;;   (load-theme 'modus-vivendi)

;;   ;; Using a single aspect
;;   ;(setq modus-themes-mode-line '(borderless))

;;   ;; Using multiple aspects
;;   (setq modus-themes-mode-line '(accented borderless padded))
;;   (setq modus-themes-completions 'minimal)

;;   (setq modus-themes-bold-constructs t)
;;   ;(setq modus-themes-italic-constructs t)
;;   ;(setq modus-themes-paren-match '(bold intense underline))
;;   ;(setq modus-themes-syntax '(faint))
;;   ;(setq modus-themes-syntax '(alt-syntax))
;;   ;(setq modus-themes-syntax '(green-strings yellow-comments))

;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; (use-package all-the-icons
;;   :if (display-graphic-p))


;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (mapc #'disable-theme custom-enabled-themes)
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; =================================
;; common search
;; ================================
;; (setq common-file "/home/ivanir/Prog/py_dir/hypv22/hyperpvapitechgf22/common.py")
;; (setq matches '())
;; (setq pattern "^\w+\s+=\s+")
;; (setq new  (get-buffer-create "TempBuff"))
;; (switch-to-buffer new)
;; (insert-file-contents common-file)
;; (goto-char (point-min))
;; (while (re-search-forward pattern nil t)
;;                                         ;(add-to-list 'matches (match-string-no-properties 1) t))
;; (message "aa"))
     
;; (mark-whole-buffer)
;; (setq contents (buffer-substring (mark) (point)))
;; (kill-buffer new)
;; (save-match-data
;;     (let ((pos 0)
;;           matches)
;;       (while (string-match "^\w+\s+\=\s+" contents pos)
;;         (push (match-string 0 string) matches)
;;         (setq pos (match-end 0)))
;;       matches))



      
