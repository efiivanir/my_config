(let
((local-file (expand-file-name "local.el" user-emacs-directory)))
(when (file-exists-p local-file)
(load local-file 'noerror)))

(let
  ((customization-file (expand-file-name "custom.el" user-emacs-directory)))
(when (file-exists-p customization-file)
  (setq custom-file customization-file)
  (load custom-file 'noerror)))

(setq default-frame-alist
      '(
        (fringe-mode (quote (1 . 1)) nil (fringe))
        (fringes-outside-margins nil t)
        (right-fringe . 0)
        (left-fringe)
        (left-fringe-width nil)
        (frame-resize-pixelwise t)
        (border-color . "black")
        (menu-bar-lines . 1)))

(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function " %b"
                                                                 ("%b - Dir:  " default-directory)))))))
(when (display-graphic-p)
  (set-fontset-font "fontset-default" nil
                    (font-spec :size 20 :name "Symbola")))

(cond ((eq system-type 'windows-nt)
       (setq inhibit-compacting-font-caches t
             w32-use-native-image-API t)))

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(add-hook 'focus-out-hook #'garbage-collect)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(defun ms/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun ms/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
        "Enhances `load-theme' to disable enabled themes for a clean slate."
        (unless no-enable
          (ms/disable-all-themes))
        (prog1
            (apply f theme-id no-confirm no-enable args)))

(advice-add 'load-theme
            :around
            #'ms/load-theme-advice)

(use-package solarized-theme
  :demand t
  :disabled
  :config
  (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
  (set-face-attribute 'font-lock-comment-face nil :italic t)
  (set-face-attribute 'font-lock-doc-face nil :italic t)
  (setq solarized-high-contrast-mode-line nil
        solarized-distinct-fringe-background t
        solarized-use-more-italic t
        solarized-use-less-bold nil
        solarized-distinct-doc-face t)
  (load-theme 'solarized-zenburn t))

(use-package leuven-theme
  :demand t
  :disabled
  :config
  (setq leuven-scale-outline-headlines nil
        leuven-scale-org-agenda-structure nil)
  (load-theme 'leuven t))

(use-package poet-theme
  :demand t
  :disabled
  :config
  (set-face-attribute 'default nil :family "monofur" :height 110)
  (set-face-attribute 'fixed-pitch nil :family "monofur")
  (set-face-attribute 'variable-pitch nil :family "Baskerville Old Face")
  (load-theme 'poet t))

(use-package doom-themes
  :demand t
  :disabled
  :config
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (load-theme 'doom-solarized-dark t)
)

(use-package material-theme
  :demand t
  :disabled
  :config
  (load-theme 'material t))

(use-package color-theme-sanityinc-tomorrow
  :demand t
  :disabled
  :config
  (load-theme 'sanityinc-tomorrow-day t))

(use-package parchment-theme
  :demand t
  :disabled
  :config
  (load-theme 'parchment t))

(use-package darkplus-theme
  :load-path "lisp"
  :demand t
  :disabled
  :config
  (load-theme 'darkplus t))

(use-package modus-operandi-theme
  :demand t
  :disabled t
  :custom
  (modus-operandi-theme-proportional-fonts t)
  (modus-operandi-theme-scale-headings t)
  :config
  (load-theme 'modus-operandi t))

(use-package nord-theme
; :demand t
 :config
 (load-theme 'nord t))

(use-package files
  :ensure nil
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  ;; backup settings
  (backup-by-copying t "don't clobber symlinks")
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t "use versioned backups")
  :config
  (setq confirm-kill-processes nil))

(use-package ace-window
:ensure t
:init
(progn
(global-set-key [remap other-window] 'ace-window)
(custom-set-faces
'(aw-leading-char-face
((t (:inherit ace-jump-face-foreground :height 3.0)))))))

(defalias 'qrr 'query-replace-regexp)

(when (window-system)
  (use-package font-lock+
    :load-path "site-lisp"
    :demand t))

(use-package all-the-icons
  :demand t)

(use-package all-the-icons-dired
  :demand t
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :ensure t
  :demand t
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
  (all-the-icons-ivy-setup)
   )

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(appt-activate 1)
(setq appt-message-warning-time 10
appt-display-interval 5)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)

(use-package autorevert
:ensure nil
:config
 (setq auto-revert-verbose nil
       global-auto-revert-mode t
       global-auto-revert-non-file-buffers t))

;(setq auto-save-file-name-transforms
;      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq auto-save-visited-interval 60)
(auto-save-visited-mode 1)

(use-package centaur-tabs
  :demand t
  :init (setq centaur-tabs-set-bar 'over)
  :config
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-bar t
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker " ● "
        centaur-tabs-close-button " × "
        centaur-tabs-style "bar")
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode))

(setq calendar-date-style 'european
      calendar-week-start-day 1
      calendar-latitude [48 9 north]
      calendar-longitude [11 34 east]
      calendar-mark-holidays-flag t
      calendar-time-display-form
      '(24-hours ":" minutes
                 (if time-zone " (")
                 time-zone
                 (if time-zone ")"))
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                               "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                 "Juni" "Juli" "August" "September"
                                 "Oktober" "November" "Dezember"]
      solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang")
      holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-float 5 0 2 "Muttertag")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit"))
      holiday-christian-holidays
      '(
        (holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc -48 "Rosenmontag")
        (holiday-easter-etc -2 "Karfreitag")
        (holiday-easter-etc  0 "Ostersonntag")
        (holiday-easter-etc +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariä Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        (holiday-float 11 3 1 "Buß- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20)
        (holiday-fixed 12  8 "Mariä Empfängnis"))
      calendar-holidays
      (append holiday-general-holidays holiday-local-holidays holiday-other-holidays
              holiday-christian-holidays holiday-solar-holidays))

(use-package calfw
  :disabled
  :commands cfw:open-calendar-buffer
  :bind ("<C-f12>" . open-calendar)
  :init
  (use-package calfw-org
    :commands (cfw:open-org-calendar cfw:org-create-source))

  (use-package calfw-cal
    :commands (cfw:open-diary-calendar cfw:cal-create-source))

  (use-package calfw-ical
    :commands (cfw:open-ical-calendar cfw:ical-create-source))

  (defun open-calendar ()
    "Open calendar."
    (interactive)
    (cfw:open-calendar-buffer
               :contents-sources
               (list
                (cfw:org-create-source "Gray") ; org source
                (cfw:cal-create-source "Orange") ; diary source
                ))))

(use-package system-packages
  :custom
  (system-packages-noconfirm t))

(use-package emacs
  :ensure nil
  :custom
  (ad-redefinition-action 'accept)
  (blink-cursor-mode nil)
  (byte-compile-warnings
   (quote
    (redefine callargs free-vars unresolved obsolete noruntime interactive-only)))
  (column-number-mode t)
  (compilation-message-face (quote default))
  (confirm-nonexistent-file-or-buffer nil)
  (debug-on-quit nil)
  (delete-by-moving-to-trash t)
  (delete-selection-mode t)
  (display-line-numbers-type nil)
  (echo-keystrokes 0.5)
  (electric-pair-mode t)
  (fast-but-imprecise-scrolling t)
  (fill-column 90)
  (global-font-lock-mode t nil (font-lock))
  (indent-region-mode t)
  (indent-tabs-mode nil "Spaces!")
  (tab-always-indent 'complete "smart tab behavior - indent or complete")
  (indicate-empty-lines t)
  (inhibit-startup-screen t "Don't show splash screen")
  (initial-buffer-choice t)
  (initial-scratch-message nil)
  (kill-whole-line t)
  (line-spacing 0.2)
  (linum-format " %6d ")
  (mouse-drag-copy-region t)
  (require-final-newline t)
  (save-interprogram-paste-before-kill t)
  (sentence-end-double-space nil)
  (show-paren-mode t)
  (size-indication-mode t)
  (special-display-buffer-names (quote ("*Completions*")))
  (tab-width 2)
  (use-dialog-box nil "Disable dialog boxes")
  (visible-bell t))

(use-package custom
  :ensure nil
  :custom
  (custom-safe-themes t "Treat all themes as safe"))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-init-info t
        dashboard-startup-banner 'official
        dashboard-banner-logo-title (message "Emacs ready in %s with %d garbage collections."
                                             (format "%.2f seconds"
                                                     (float-time
                                                      (time-subtract after-init-time before-init-time)))
                                             gcs-done)
        dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face)
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (agenda . 5))
        show-week-agenda-p t)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :demand t)

(use-package date2name
  :demand t)

(use-package diminish
:ensure t
:demand t
:diminish abbrev-mode
:diminish auto-fill-function
:diminish eldoc-mode
:diminish counsel-mode
:diminish visual-line-mode
:diminish undo-tree-mode
:diminish company-mode)

(use-package dired
  :ensure nil
  :custom (dired-dwim-target t "guess a target directory")
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-toggle
  :defer t)

(use-package dired-hide-dotfiles
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-package dired-git-info
    :ensure t
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(setq global-display-line-numbers-mode t
      global-hl-line-sticky-flag t
      global-visual-line-mode t)

(setq history-delete-duplicates t)

(use-package disk-usage
  :ensure t)

(unless (and (fboundp 'server-running-p)
             (server-running-p))
  (server-start))

;  (use-package s
;    :demand t)
;  (require 's)

(use-package filetags
    :demand t
    :custom
    (filetags-load-controlled-vocabulary-from-file t "read CV from .filetags files within same or upper directories")
)

(use-package golden-ratio-scroll-screen
  :ensure t
  :bind(([remap scroll-down-command] . golden-ratio-scroll-screen-down)
        ([remap scroll-up-command] . golden-ratio-scroll-screen-up)))

(use-package helpful
  :bind (
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(global-hl-line-mode 1)
(setq hl-line-face 'hl-line)

(use-package ido
  :config
  :disabled t
  (setq ido-create-new-buffer (quote always)
        ido-enable-flex-matching t)
  (ido-mode (quote buffers))
  :ensure nil)

(use-package ivy
  :pin MELPA
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :config
  (setq ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full)
  (ivy-mode))

(use-package counsel
  :after ivy
  :ensure t
  :config (counsel-mode)
  :bind ("\C-x\C-r" . counsel-recentf))

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key "\C-co" 'occur)

(defalias 'list-buffers 'ibuffer)

(define-key ctl-x-map "f" 'find-file)

;  (global-set-key "\C-c\C-r" 'comment-region)

(global-set-key "\M-g" 'goto-line)

(global-set-key "\C-c\C-v" '(lambda () (interactive) (revert-buffer nil t)))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode 1)
  :bind
  ([S-f10] . menu-bar-mode))

(when (window-system)
  (use-package minimal
    :load-path "lisp/minimal"
    :demand t))

(use-package minions
  :demand t
  :init (minions-mode)
  :config
  (setq minions-mode-line-lighter "#"))

(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

(use-package mode-icons
  :demand t
  :config
  (mode-icons-mode)
)

(use-package paradox
  :defer 1
  :config
  (setq  paradox-automatically-star nil)
  (paradox-enable))

(when (display-graphic-p)
  (use-package moody
    :demand t
    :config
    (setq x-underline-at-descent-line t)
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)))

(use-package msb
:config
(msb-mode 1)
:ensure nil)

(blink-cursor-mode 0)

(cond ((eq system-type 'gnu/linux)
       (use-package openwith
         :config
         (openwith-mode t)
         (setq openwith-associations '(("\\.pdf\\'" "evince" (file)))))))

(use-package recentf
:config
(setq recentf-max-saved-items 40
      recentf-max-menu-items 15
      recentf-menu-open-all-flag t
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-cleanup-auto 'never)
(add-to-list 'recentf-exclude  (expand-file-name package-user-dir))
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
:hook (after-init . recentf-mode))

(use-package saveplace
  :config
  (save-place-mode t)
  :ensure nil)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(defun my/split-below-last-buffer (prefix)
"Split the window above/below and display the previous buffer.
If prefix arg is provided, show current buffer twice."
(interactive "p")
(split-window-below)
(other-window 1 nil)
(if (= prefix 1)
(switch-to-next-buffer)))

(defun my/split-right-last-buffer (prefix)
"Split the window left/right and display the previous buffer
If prefix arg is provided, show current buffer twice."
(interactive "p")
(split-window-right)
(other-window 1 nil)
(if (= prefix 1) (switch-to-next-buffer)))

(global-set-key (kbd "C-x 2")  'my/split-below-last-buffer)
(global-set-key (kbd "C-x 3")  'my/split-right-last-buffer)
(setq switch-to-prev-buffer-skip 'this)

(use-package time
  :defer t
  :ensure nil
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-day-and-date t)
  (display-time-interval 30)
  (display-time-string-forms
   (quote
    ((if display-time-day-and-date
         (format "%s %s. %s " dayname day monthname)
       "")
     (format "%s:%s%s"
             (if display-time-24hr-format 24-hours 12-hours)
             minutes
             (if display-time-24hr-format "" am-pm)))))
  :config
  (display-time-mode t))

(use-package treemacs
  :ensure t
  :defer t
  :init
  :config
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package undo-tree
  :config
  ;; Always have it on
  (global-undo-tree-mode)

  ;; Each node in the undo tree should have a timestamp.
  (setq undo-tree-visualizer-timestamps t)

  ;; Show a diff window displaying changes between undo nodes.
  (setq undo-tree-visualizer-diff t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  :ensure nil)

(use-package whitespace
:diminish whitespace-mode)

(use-package which-key
:hook (after-init . which-key-mode))

(setq-default abbrev-mode t)     ;; enable abbreviations
(setq save-abbrevs 'silently)    ;; save abbreviations upon exiting emacs
(if (file-exists-p abbrev-file-name)
(quietly-read-abbrev-file))  ;; reads the abbreviations file on startup

(use-package comment-dwim-2
:bind (("M-;" . comment-dwim-2)))

(use-package company-web
:diminish t
:hook (after-init . global-company-mode))

(use-package company
  :diminish
  :config
  (setq company-begin-commands '(self-insert-command)
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-align-annotations 't)
  :hook (after-init . global-company-mode))

(when ( window-system)
  (use-package company-box
    :after company
    :diminish
    :hook (company-mode . company-box-mode)
    :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)
  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
            (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
            (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))))))
 )

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(use-package deft
:init
(setq deft-directory (concat cloud-dir "org"))
:config
(setq deft-default-extension "org"
deft-use-filename-as-title nil
deft-use-filter-string-for-filename t
deft-auto-save-interval 0
deft-org-mode-title-prefix t
deft-file-naming-rules
  '((noslash . "-")
    (nospace . "-")
    (case-fn . capitalize)))
:bind ("C-x C-d" . deft-find-file))

(defun bjm-deft-save-windows (orig-fun &rest args)
  (setq bjm-pre-deft-window-config (current-window-configuration))
  (apply orig-fun args)
  )

(advice-add 'deft :around #'bjm-deft-save-windows)

(defun bjm-quit-deft ()
  "Save buffer, kill buffer, kill deft buffer, and restore window config to the way it was before deft was invoked"
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  (switch-to-buffer "*Deft*")
  (kill-this-buffer)
  (when (window-configuration-p bjm-pre-deft-window-config)
    (set-window-configuration bjm-pre-deft-window-config)
    )
  )

(global-set-key (kbd "C-c q") 'bjm-quit-deft)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ediff-diff-options "--binary -w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package grip-mode
  :ensure t)

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol)
      hippie-expand-dabbrev-as-symbol t
      hippie-expand-dabbrev-skip-space nil)

(use-package json-navigator)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc --from markdown -t html5 --standalone"))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 100))

(use-package olivetti
  :custom
  (olivetti-body-width 0.8)
  (olivetti-hide-mode-line t)
  )

(setq org-directory (concat cloud-dir "org"))

(setq  org-archive-location "archive.org::datetree/*** %S [/]")

(setq org-deadline-warning-days 30)

(setq  org-fast-tag-selection-single-key 'expert
       org-special-ctrl-a/e t
       org-special-ctrl-k t
       org-use-speed-commands (lambda () (and (looking-at org-outline-regexp) (looking-back "^\\**"))))

(setq  org-habit-graph-column 60
       org-log-done 'time
       org-log-into-drawer t
       org-log-redeadline 'time
       org-log-reschedule 'time)

(setq org-startup-indented t
      org-src-tab-acts-natively t)

(setq org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))
      org-refile-use-outline-path 'file)

(defun verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'verify-refile-target)

(org-agenda-to-appt)

(add-to-list 'org-structure-template-alist
             '("el" . "src emacs-lisp"))

(setq org-mobile-use-encryption t)

(defalias 'omp 'org-mobile-push)
(defalias 'omf 'org-mobile-pull)

(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(add-hook
 'org-after-refile-insert-hook
  'org-update-parent-todo-statistics)

(setq org-modules '(org-habit ol-info org-expiry))

(setq org-hierarchical-todo-statistics nil)

(setq org-duration-format '((special . h:mm)))

(setq org-return-follows-link t)

(setq org-adapt-indentation t)

(setq org-fast-todo-selection t
      org-fast-tag-selection-include-todo nil)

(setq org-catch-invisible-edits 'show)

(custom-set-faces
 '(org-drawer ((t (:weight thin :foreground "#586e75"))))
 )
 ;; hide drawers when cycling
(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp
                      (point-min)
                    (point)))
             (end (if globalp
                      (point-max)
                    (if (eq state 'children)
                        (save-excursion
                          (outline-next-heading)
                          (point))
                      (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward org-drawer-regexp end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at org-drawer-regexp)
              (let* ((start (1- (match-beginning 0)))
                     (limit
                      (save-excursion
                        (outline-next-heading)
                        (point)))
                     (msg (format
                           (concat
                            "org-cycle-hide-drawers:  "
                            "`:END:`"
                            " line missing at position %s")
                           (1+ start))))
                (if (re-search-forward "^[ \t]*:END:" limit t)
                    (outline-flag-region start (point-at-eol) t)
                  (user-error msg))))))))))
(add-hook 'org-cycle-hook 'org-cycle-hide-drawers)
(add-hook 'org-mode-hook (lambda ()
                           (local-set-key (kbd "C-c s") 'org-show-subtree)))

(use-package org-contrib)
(require 'org-expiry)
(org-expiry-insinuate)
(setq org-expiry-inactive-timestamps t)
(add-hook 'org-capture-before-finalize-hook
#'(lambda()
(save-excursion
(org-back-to-heading)
(org-expiry-insert-created))))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)

(add-hook 'org-mode-hook
(lambda () (imenu-add-to-menubar "Imenu")))

(setq org-imenu-depth 5)

(setq org-agenda-files (concat org-directory "/agenda.txt")
      org-agenda-date-weekend (quote (:foreground "Yellow" :weight bold))
      org-agenda-start-on-weekday nil
      org-agenda-block-separator 9472
      org-agenda-include-diary t
      org-agenda-insert-diary-extract-time t
      org-agenda-remove-tags t
      org-agenda-show-inherited-tags t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled) ;;don't show tasks that are scheduled or have deadlines in the
      org-agenda-skip-scheduled-if-deadline-is-shown t ;;don't give a warning colour to tasks with impending deadlines if they are scheduled to be done
      org-agenda-skip-scheduled-if-done t           org-agenda-skip-scheduled-if-deadline-is-shown t ;;don't give a warning colour to tasks with impending deadlines if they are scheduled to be done
      org-agenda-sorting-strategy ;;sort tasks in order of when they are due and then by priority
      (quote
       ((agenda time-up habit-down deadline-up priority-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))
      org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800)
        "......" "----------------")))

(setq org-agenda-category-icon-alist
   `(("Org" ,(list (all-the-icons-faicon "check-circle")) nil nil :ascent center)
     ("regularly" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
     ("buecher" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
     ("Journal" ,(list (all-the-icons-faicon "leanpub")) nil nil :ascent center)
     ("tasks" ,(list (all-the-icons-faicon "check-circle-o")) nil nil :ascent center)
     ("work" ,(list (all-the-icons-faicon "briefcase")) nil nil :ascent center)
     ("tree" ,(list (all-the-icons-faicon "tree")) nil nil :ascent center)
     ("team" ,(list (all-the-icons-faicon "users")) nil nil :ascent center)
     ))

(setq org-agenda-start-with-clockreport-mode t
      org-clock-report-include-clocking-task t
      org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 4 :fileskip0 t :compact t :narrow 80)))

(use-package org-super-agenda
  :demand t
  :config
  (org-super-agenda-mode))

(use-package doct
  :ensure t
  ;;recommended: defer until calling doct
  :commands (doct))

(setq org-capture-templates
              (doct `(
                      (,(format "%s\tCliplink capture task" (all-the-icons-faicon "globe" :face 'all-the-icons-blue :v-adjust 0.01))
                       :keys "c"
                       :file "Inbox.org"
                       :type entry
                       :template ("* %(org-cliplink-capture)")
                       :empty-lines 1)
                      (,(format "%s\tIdeen" (all-the-icons-octicon "file-text" :face 'all-the-icons-cyan :v-adjust 0.01))
                       :keys "i"
                       :file "Inbox.org"
                       :type entry
                       :template ("* %^{Title}
          %i"))
                      (,(format "%s\tJournal" (all-the-icons-faicon "sticky-note-o" :face 'all-the-icons-green :v-adjust 0.01))
                       :keys "j"
                       :file "Journal.org"
                       :datetree t
                       :type entry
                       :template ("* %U %?
          %i"))
                      (,(format "%s\tLog Time" (all-the-icons-material "timer" :face 'all-the-icons-green :v-adjust 0.01))
                       :keys "l"
                       :file "Journal.org"
                       :datetree t
                       :type entry
                       :template ("* %U - %^{Activity}  :TIME:")
                       :immediate-finish t
                       :clock-in t
                       :clock-keep t)
                      (,(format "%s\tOutcomes for today" (all-the-icons-material "today" :face 'all-the-icons-yellow :v-adjust 0.01))
                       :keys "o"
                       :file "Journal.org"
                       :datetree t
                       :type entry
                       :template ("* %U 3 Outcomes for today [%]   :ZIELE:
%[~/.emacs.d/config/org/.woche.txt]"))
                      (,(format "%s\tWeekly Review" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
                       :keys "r"
                       :file "Journal.org"
                       :datetree t
                       :type entry
                       :template ("* Weekly Review  :PERSOENLICH:
%[~/.emacs.d/config/org/.weeklyreview.txt]"))
                      (,(format "%s\tTasks" (all-the-icons-octicon "checklist" :face 'all-the-icons-yellow :v-adjust 0.01))
                       :keys "t"
                       :file "tasks.org"
                       :headline "Inbox:"
                       :type entry
                       :template ("* TODO %^{Task}
          %i")
                       :immediate-finish t)
                      (,(format "%s\tOutcomes for the week" (all-the-icons-faicon "calendar-check-o" :face 'all-the-icons-yellow :v-adjust 0.01))
                       :keys "w"
                       :file "Journal.org"
                       :datetree t
                       :type entry
                       :template ("* %U 3 Outcomes for the week [%]   :ZIELE:
%[~/.emacs.d/config/org/.woche.txt]"))
                      )
                    )
              )

(setq org-plantuml-jar-path (concat (expand-file-name no-littering-etc-directory) "org/plantuml.jar"))
(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (plantuml . t)))

(setq org-confirm-babel-evaluate nil)

(setq org-src-fontify-natively 't)

(defun shk-fix-inline-images ()
(when org-inline-image-overlays
(org-redisplay-inline-images)))

  (add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images)

(use-package org-books
    :custom
     (org-books-url-patterns
     (quote
      ((amazon . "^\\(www\\.\\)?amazon\\.")
       (goodreads . "^\\(www\\.\\)?goodreads\\.com")
       (isbn . "openlibrary\\.org")
       (manning . "^\\(www\\.\\)?manning\\."))))
    :config
    (setq org-books-file (concat cloud-dir "org/books.org"))
)

(setq org-export-exclude-tags '("NA")
      org-export-select-tags (quote ("PROJEKT" "TOPIC" "EXPORT"))
      org-export-with-tags nil
      org-export-with-toc nil
      org-export-with-todo-keywords nil)

(use-package htmlize)

(setq org-pandoc-menu-entry
          '(
            (?x "to docx and open." org-pandoc-export-to-docx-and-open)
            (?X "to docx." org-pandoc-export-to-docx)
            (?p "to plain and open." org-pandoc-export-to-plain-and-open)
            (?P "as plain." org-pandoc-export-as-plain)
            (?h "to html5 and open." org-pandoc-export-to-html5-and-open)
            (?H "as html5." org-pandoc-export-as-html5)))
    (use-package ox-pandoc
      :after org
;      :config (add-to-list 'org-pandoc-options '(toc . t))
      :demand t)

(setq org-ascii-text-width 150)

(use-package ox-hugo
  :demand t
  :after ox)

(use-package org-cliplink
:bind ("C-c p" . org-cliplink))

(setq org-clock-persist 'clock
      org-clock-in-resume t
      org-clock-report-include-clocking-task t
      org-clock-mode-line-total 'current
      org-clock-out-remove-zero-time-clocks t
      org-clock-clocked-in-display 'both
      org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
(org-clock-persistence-insinuate)

(defun clocktable-by-tag/shift-cell (n)
  (let ((str ""))
    (dotimes (i n)
      (setq str (concat str "| ")))
    str))

(defun clocktable-by-tag/insert-tag (params)
  (let ((tag (plist-get params :tags)))
    (insert "|--\n")
    (insert (format "| %s | *Tag time* |\n" tag))
    (let ((total 0))
      (mapcar
       (lambda (file)
         (let ((clock-data (with-current-buffer (find-file-noselect file)
                             (org-clock-get-table-data (buffer-name) params))))
           (when (> (nth 1 clock-data) 0)
             (setq total (+ total (nth 1 clock-data)))
             (insert (format "| | File *%s* | %.2f |\n"
                             (file-name-nondirectory file)
                             (/ (nth 1 clock-data) 60.0)))
             (dolist (entry (nth 2 clock-data))
               (insert (format "| | . %s%s | %s %.2f |\n"
                               (org-clocktable-indent-string (nth 0 entry))
                               (nth 1 entry)
                               (clocktable-by-tag/shift-cell (nth 0 entry))
                               (/ (nth 4 entry) 60.0)))))))
       (org-agenda-files))
      (save-excursion
        (re-search-backward "*Tag time*")
        (org-table-next-field)
        (org-table-blank-field)
        (insert (format "*%.2f*" (/ total 60.0)))))
    (org-table-align)))

(defun org-dblock-write:clocktable-by-tag (params)
  (insert "| Tag | Headline | Time (h) |\n")
  (insert "|     |          | <r>  |\n")
  (let ((tags (plist-get params :tags)))
    (mapcar (lambda (tag)
              (clocktable-by-tag/insert-tag (plist-put (plist-put params :match tag) :tags tag)))
            tags)))

(use-package org-download
  :bind ("C-c i" . org-download-yank)
  :hook (org-mode . org-download-enable)
  :config
  (setq org-download-method 'attach))

(org-add-link-type
 "tag" 'endless/follow-tag-link)

(defun endless/follow-tag-link (tag)
  "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
  (org-tags-view (null current-prefix-arg) tag))

(use-package toc-org
  :ensure t
  :after org
  :hook (org-mode . toc-org-enable))

(add-hook 'org-mode-hook
          (lambda ()
            (variable-pitch-mode 1)
            visual-line-mode))

(setq org-hide-emphasis-markers t
      org-fontify-done-headline t
      org-ellipsis "⤵"
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts nil)

(setq org-list-demote-modify-bullet
      (quote (("-" . "+")
              ("+" . "*")
              ("1." . "-")
              ("1)" . "-")
              ("A)" . "-")
              ("B)" . "-")
              ("a)" . "-")
              ("b)" . "-")
              ("A." . "-")
              ("B." . "-")
              ("a." . "-")
              ("b." . "-")))
      org-list-indent-offset 2)

(use-package org-superstar
  :custom
  (org-superstar-headline-bullets-list '("◉" "◎" "○" "►" "◇"))
  (org-superstar-item-bullet-alist '((?- . ?•) (?+ . ?➤) (?* . ?◦)))
:config
(setq org-superstar-cycle-headline-bullets t
      org-superstar-special-todo-items nil
      org-superstar-prettify-item-bullets t)
:hook (org-mode . org-superstar-mode))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "✎")
                                       ("#+END_SRC" . "□")
                                       ("#+begin_src" . "✎")
                                       ("#+end_src" . "□")
                                       (">=" . "≥")
                                       ("=>" . "⇨")
                                       ("[ ]" .  "☐")
                                       ("[X]" . "☑")
                                       ("[-]" . "?⛞" )
                                       ("#+TITLE" . "🕮")
                                       ("#+DATE" . "📆")
                                       ("#+AUTHOR" . "👤")
                                       ("#+COLUMNS" . "▓")
                                       ("#+EMAIL" . "🖂")
                                       ("#+OPTIONS" . ?⚙)
                                       ("#+TYP_TODO" . "☑")
                                       ("#+TAGS" . ?🏷)
                                       ("#+EXPORT_SELECT_TAGS" . ?🏷)
                                       ("#+EXPORT_EXCLUDE_TAGS" . ?🏷)
                                       ("#+DESCRIPTION" . ?🗎)))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)

(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(add-to-list 'org-emphasis-alist
             '("*" (:weight bold)))

(use-package org-pretty-tags
  :demand t
  :config
   (setq org-pretty-tags-surrogate-strings
         (quote
          (("TOPIC" . "☆")
           ("PROJEKT" . "💡")
           ("SERVICE" . "✍")
           ("Blog" . "✍")
           ("music" . "♬")
           ("security" . "🔥"))))
   (org-pretty-tags-global-mode))

(use-package org-fancy-priorities
  :diminish
  :demand t
  :defines org-fancy-priorities-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (unless (char-displayable-p ?❗)
    (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

(use-package org-num
  :ensure nil
  :after org
  :hook (org-mode . org-num-mode))

(use-package org-kanban)

(use-package org-sidebar
  :custom (org-sidebar-tree-side 'left)
  (org-sidebar-tree-jump-fn 'org-sidebar-tree-jump-source))

(use-package org-noter)

(use-package pdf-tools
  :pin MELPA
  :config
  (pdf-tools-install :no-query))

(use-package org-noter)

(use-package pdf-tools
  :pin MELPA
  :config
  (pdf-tools-install :no-query))

(use-package todotxt-mode
  :demand t
  :disabled
  :config (setq todotxt-default-file (expand-file-name (concat cloud-dir "Todo/todo.txt"))
                todotxt-default-archive-file (expand-file-name (concat cloud-dir "Todo/done.txt")))
  :bind ("C-c t" . todotxt-add-todo)
  ("C-c o" . todotxt-open-file))

(use-package ztree)

(use-package js2-mode
:config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
)

;(require 'less-css-mode)

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-use-overlays nil))

(cond ((eq system-type 'windows-nt)
       (use-package ssh-agency)
       (setenv "SSH_ASKPASS" "git-gui--askpass")))

(use-package php-mode
  :hook (php-mode . (lambda () (define-abbrev php-mode-abbrev-table "ex" "extends"))))

(use-package plantuml-mode
:config (setq plantuml-output-type "png")
:mode ("\\.puml\\'" . plantuml-mode))

(use-package powershell)

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package flycheck
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)
        flycheck-flake8-maximum-line-length 130)
  :hook
  (elpy-mode . flycheck-mode))

(use-package py-autopep8
  :hook
  (elpy-mode . py-autopep8-enable-on-save)
  )

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package web-mode
:mode (("\\.handlebars\\'" . web-mode) ("\\.html?\\'" . web-mode))
)

(defadvice kill-ring-save (before slick-copy activate compile)
"When called interactively with no active region, copy a single
line instead."
(interactive (if mark-active (list (region-beginning) (region-end)) (message
"Copied line") (list (line-beginning-position) (line-beginning-position
2)))))

(defadvice kill-region (before slick-cut activate compile)
"When called interactively with no active region, kill a single line instead."
(interactive
  (if mark-active (list (region-beginning) (region-end))
    (list (line-beginning-position)
      (line-beginning-position 2)))))

(defun ido-recentf-open ()
       "Use `ido-completing-read' to \\[find-file] a recent file"
       (interactive)
       (if (find-file (ido-completing-read "Find recent file: " recentf-list))
           (message "Opening file...")
         (message "Aborting")))

;    (global-set-key "\C-x\C-r" 'ido-recentf-open)

(setq kill-buffer-query-functions
(remq 'process-kill-buffer-query-function
kill-buffer-query-functions))

(defun insert-date (prefix)
"Insert the current date. With prefix-argument, use ISO format. With
two prefix arguments, add time."
(interactive "P")
(let ((format (cond
((not prefix) "%x")
((equal prefix '(4)) "%F")
((equal prefix '(16)) "%F %R"))))
(insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
