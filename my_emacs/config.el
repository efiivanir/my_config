(use-package all-the-icons
  :ensure t)

(message "use-package doom-themes")
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config)
  (setq neo-global--window nil)
  (setq hl-line-sticky-flag nil)
  (setq neo-vc-integration nil)
  (setq neotree-dir-button-keymap nil)
  (setq neotree-file-button-keymap nil)
  (setq neo-path--file-short-name nil)
  (setq neo-vc-for-node nil)
  (setq neo-buffer--insert-fold-symbol nil)
  (setq neo-buffer--node-list-set nil)
  (setq neo-buffer--newline-and-begin nil)
  (setq neo-global--select-window nil)
  (setq neo-buffer--insert-file-entry nil)
  (setq neo-buffer--insert-dir-entry nil)
  (setq neo-buffer--insert-root-entry nil))

(use-package leuven-theme
     :config
     (load-theme 'leuven t)
     (setq leuven-scale-outline-headlines nil)
     (setq leuven-scale-org-agenda-structure nil)
     (setq leuven-scale-volatile-highlight nil)
)

;; (message "use-package doom-modeline")
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

(message "use-package helm")
(use-package helm
:ensure t
:init
(helm-mode 1)
:bind
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("M-x" . 'helm-M-x))
(helm-autoresize-mode 1)

(message "use-package helm-lsp")
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(message "use-package company")
(use-package company
  :ensure t
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "<tab>") #'company-abort))
(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)
(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))
  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))
  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))
(global-set-key [tab] 'tab-indent-or-complete)
(add-hook 'after-init-hook 'global-company-mode)

;(use-package company-lsp
;  :ensure t
;  :commands company-lsp)

(message "use-package yasnippet")
(use-package yasnippet
  :ensure t
  :config
    (yas-reload-all))
(define-key global-map (kbd "C-c C-y") 'yas-new-snippet)
(defun yas/org-very-safe-expand ()
(let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(add-hook 'org-mode-hook
(lambda ()
(make-variable-buffer-local 'yas/trigger-key)
(setq yas/trigger-key [tab])
(add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
(define-key yas/keymap [tab] 'yas/next-field)))

(use-package org-bullets
  :ensure t
  ;:config
  ;(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
)

(use-package quickrun
  :ensure t
  :bind("<f5>". 'quickrun-shell))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         26)
	  
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(electric-pair-mode 1)
(setq electric-pair-pairs
'(
  (?\" . ?\")
  (?\' . ?\')
  (?\{ . ?\})))

(defun move-line (n)
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    (forward-line -1)
    (forward-char col)))
(defun move-line-up (n)
  (interactive "p")
  (move-line (if (null n) -1 (- n))))
(defun move-line-down (n)
  (interactive "p")
  (move-line (if (null n) 1 n)))
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(show-paren-mode 1)

(global-set-key (kbd "C-h") 'kill-whole-line)

(add-hook 'prog-mode-hook 'linum-mode)

(use-package lsp-mode
  :commands lsp
  :ensure t
  :hook(c-mode . lsp))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "~/.emacs.d/CondorEmacs.png")
  (setq dashboard-banner-logo-title "Welcome to Condor Emacs!")
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
	`(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
	    "Homepage"
	    "Browse homepage"
	    (lambda (&rest _) (browse-url "https://github.com/apemangr/Condor-Emacs/")))

	   (,(all-the-icons-faicon "archive" :height 1.1 :v-adjust 0.0)
	    "Update Packages"
	    "Click to updates your packages"
	    (lambda (&rest _) (auto-package-update-now)))

	   (,(all-the-icons-octicon "gear" :height 1.1 :v-adjust 0.0)
	    "Configuration"
	    "Click to config Condor Emacs"
	    (lambda (&rest _) (find-file "~/.emacs.d/config.org")))))))

(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

(setq org-startup-indented t
      org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
      org-ellipsis "  " ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

;; (add-hook 'org-mode-hook (lambda ()
;;    "Beautify Org Checkbox Symbol"
;;    (push '("[ ]" .  "☐") prettify-symbols-alist)
;;    (push '("[X]" . "☑" ) prettify-symbols-alist)
;;    (push '("[-]" . "❍" ) prettify-symbols-alist)
;;    (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
;;    (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
;;    (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
;;    (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
;;    (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
;;    (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
;;    (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
;;    (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
;;    (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
;;    (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
;;    (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
;;    (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
;;    (prettify-symbols-mode)))

(use-package dired
  :ensure nil
  :config
  ;; Switches passed to `ls' for Dired.
  (setq dired-listing-switches "-alt")
  :custom (dired-dwim-target t "guess a target directory")
  ;:hook
 ; (dired-mode . dired-hide-details-mode)
)
(use-package dired-hacks-utils
  :ensure t
  )

(use-package dired-toggle
  :defer t)

(use-package dired-hide-dotfiles
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package diredfl
  :ensure t
  :hook
  (dired-mode . diredfl-mode))

;; (use-package dired-subtree
;;   :ensure nil
;;   :config
;;   (bind-keys :map dired-mode-map
;;              ("i" . dired-subtree-insert)
;;              (";" . dired-subtree-remove)))

(use-package dired-git-info
    :ensure t
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package bookmark
   :config
   ;; Where to save the bookmarks.
   (setq bookmark-default-file (concat user-emacs-directory "bookmarks.bmk"))
                                       ;! A `.txt' extension would load Org at
                                       ;! the time `bookmark' is required!

   ;; Each command that sets a bookmark will also save your bookmarks.
   (setq bookmark-save-flag 1)
)

;; Extensions to standard library `bookmark.el'.
(use-package bookmark+ 
  :load-path "site-lisp/bookmark-plus"
  :config
  ;; Toggle an ANONYMOUS bookmark on the current line.
  (global-set-key (kbd "<C-f2>") #'bmkp-toggle-autonamed-bookmark-set/delete)
  (global-set-key (kbd "<S-f2>") #'bmkp-next-bookmark-this-file/buffer-repeat)
  ;; Delete all ANONYMOUS bookmarks in a buffer.
  (global-set-key (kbd "<C-S-f2>") #'bmkp-delete-all-autonamed-for-this-buffer)
  ;; View all bookmarks.
  (global-set-key (kbd "<M-f2>") #'helm-bookmarks)
  (add-hook 'find-file-hook #'bmkp-light-this-buffer)
  ;; Priorities of bookmark highlighting overlay types.
  (setq bmkp-light-priorities '((bmkp-autonamed-overlays     . 150)
                                (bmkp-non-autonamed-overlays . 160)))

  ;; Symbols for the fringe bitmaps to use to highlight a bookmark.
  (setq bmkp-light-left-fringe-bitmap 'filled-square)
  (setq bmkp-light-right-fringe-bitmap 'filled-square)
  ;; Default highlight style for ANONYMOUS (= default) bookmarks.
  (setq bmkp-light-style-autonamed 'line+lfringe)
  ;; Default highlight style for bookmarks WITH MNEMONICS.
  (setq bmkp-light-style-non-autonamed 'line+lfringe)
  ;; Automatically highlight bookmarks when set.
  (setq bmkp-auto-light-when-set 'all-in-buffer)
  ;; Automatically highlight bookmarks when jumped to.
  (setq bmkp-auto-light-when-jump 'all-in-buffer)
  ;; Don't propertize bookmark names to hold full bookmark data.
  (setq bmkp-propertize-bookmark-names-flag nil)
                                    ; We will often be going back and forth
                                    ; between using Bookmark+ and using
                                    ; vanilla Emacs.
  (setq bmkp-last-as-first-bookmark-file nil)
  ;; Name ANONYMOUS bookmarks with buffer name and line number.
  (setq bmkp-autoname-format "^%B:[0-9]+: %s")
  (setq bmkp-autoname-bookmark-function #'leuven-bmkp-autoname-line)
  (defun leuven-bmkp-autoname-line (position)
    "Name autonamed bookmark at POSITION using line number."
    (let ((line  (line-number-at-pos position)))
      ;; (format "%s:%d (%s)" (buffer-name) line (buffer-file-name))
      (format "%s:%d: %s"
              (buffer-name)
              line
              (buffer-substring-no-properties
               (line-beginning-position)
               (1- (line-beginning-position 2))))))
   )

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq drc-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("VERBATIM" "#IFNDEF" "#ENDIF" "elseif" "else" "if" "IFDEF"
                          "#DEFINE" "FLAG" "while" "VARIABLE"))
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
(define-derived-mode drc-mode c-mode "DRC mode"
  "Major mode for editing Calibre DRC runsets"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((drc-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'drc-mode)

;;; drc-mode.el ends here
;;###autoload
(add-to-list 'auto-mode-alist '("\\.drc" . drc-mode))
