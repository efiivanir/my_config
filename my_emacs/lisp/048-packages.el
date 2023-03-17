; * Emacs Lisp Packages

;; Another first thing in your init script should be (package-initialize) so that customization of Org variables (for example) doesn’t auto-load the old version that came with Emacs.

;; If you require something from Org mode, it must be after package-initialize.
;; The Package Menu Buffer

;;* 48 Emacs Lisp (info "(emacs)Packages")

(leuven--chapter leuven-load-chapter-48-packages "48 Emacs Lisp Packages"

;; To upgrade packages after running package-list-packages, type:

;;     U (mark Upgradable packages) and then
;;     x (eXecute the installs and deletions).

;; When it’s done installing all the packages, you delete the obsolete packages by hitting y (Yes) when asked.
;; Package Installation

;;     User’s Emacs Lisp packages in package-user-dir (~/.emacs.d/elpa)
;;     System-wide Emacs Lisp packages in package-directory-list

;; HTTP frontends for the Emacs repositories:

;;     http://elpa.gnu.org/packages/
;;     http://melpa.org/packages/

;; In Marmalade, the developers themselves do the packaging. In MELPA, packages are generated programmatically from upstream Git-based sources.

;; To list packages which should not be installed by Emacs Leuven, add something like this into your configuration file:

;; Comment the original leuven because I use other packahe mechanizem
(setq leuven-elpa-ignored-packages '(ess))

;;** 48.2 Package Installation

  (leuven--section "48.2 Package Installation")

  ;; Simple package system for GNU Emacs.
;  (try-require 'package)
;  (with-eval-after-load "package"
;
;    ;; Archives from which to fetch.
;    (setq package-archives
;          (append '(("org"   . "http://orgmode.org/elpa/")
;                    ("melpa" . "http://melpa.org/packages/"))
;                  package-archives))
;
;    ;; Load the latest version of all installed packages, and activate them.
;    (package-initialize)                ; Add ALL ELPA subdirs to `load-path'
;                                        ; and load `<pkg>-autoloads.el'.
;
;    (defconst leuven-elpa-packages '(ace-jump-helm-line
;                                     ace-jump-mode
;                                     ace-link
;                                     ace-window
;                                     ;; aggressive-indent
;                                     ant
;                                     anzu
;                                     auctex
;                                     auto-complete
;                                     bbdb
;                                     bookmark+
;                                     boxquote
;                                     ;; calfw
;                                     circe
;                                     color-identifiers-mode
;                                     company
;                                     company-tern
;                                     company-quickhelp
;                                     csv-mode
;                                     cygwin-mount
;                                     dictionary
;                                     diff-hl
;                                     diminish
;                                     dired+
;                                     ;; emacs-eclim
;                                     ess
;                                     expand-region
;                                     fancy-narrow
;                                     fill-column-indicator
;                                     flycheck
;                                     flycheck-color-mode-line
;                                     flycheck-ledger
;                                     fuzzy
;                                     git-commit
;                                     git-messenger
;                                     git-timemachine
;                                     google-this
;                                     google-translate
;                                     goto-chg
;                                     graphviz-dot-mode
;                                     helm
;                                     helm-ag
;                                     helm-descbinds
;                                     helm-ls-git
;                                     helm-swoop
;                                     hideshowvis
;                                     highlight-symbol
;                                     howdoi
;                                     htmlize
;                                     indent-guide
;                                     ;; jabber
;                                     js2-mode
;                                     key-chord
;                                     litable
;                                     idle-require
;                                     info+
;                                     interaction-log
;                                     ledger-mode
;                                     leuven-theme
;                                     ;; magit
;                                     markdown-mode
;                                     multi-term
;                                     multiple-cursors
;                                     ;; multi-term
;                                     pager
;                                     ;; paredit
;                                     pdf-tools
;                                     powerline
;                                     rainbow-delimiters
;                                     rainbow-mode
;                                     ;; redshank
;                                     tern
;                                     tidy
;                                     unbound
;                                     undo-tree
;                                     web-mode
;                                     which-key
;                                     ws-butler
;                                     yasnippet)
;      "A list of packages to ensure are installed at Emacs startup.")
;
;    (defcustom leuven-elpa-ignored-packages
;      nil
;      "List of packages that should be ignored by Leuven Emacs Config."
;      :group 'emacs-leuven
;      :type '(repeat (string)))
;
;    (defun leuven--missing-elpa-packages ()
;      "List packages to install for a full blown Leuven installation.
;These packages are neither built-in nor already installed nor ignored."
;      (let (missing-elpa-packages)
;        (dolist (pkg leuven-elpa-packages)
;          (unless (or (package-installed-p pkg)
;                      (locate-library (symbol-name pkg))
;                      (member pkg leuven-elpa-ignored-packages))
;            (push pkg missing-elpa-packages)))
;        missing-elpa-packages))
;
;    ;; Propose to install all the packages specified in `leuven-elpa-packages'.
;    ;; which are missing and which shouldn't be ignored.
;    (let ((missing-elpa-packages (leuven--missing-elpa-packages)))
;      (when missing-elpa-packages
;        ;; Download once the ELPA archive description.
;        (package-refresh-contents)      ; Ensure that the list of packages is
;                                        ; up-to-date.  Otherwise, new packages
;                                        ; (not present in the cache of the ELPA
;                                        ; contents) won't install.
;        (dolist (pkg (reverse missing-elpa-packages))
;          (if (yes-or-no-p (format "Install ELPA package `%s'? " pkg))
;              (ignore-errors
;                (package-install pkg))
;                                        ; Must be run after initializing
;                                        ; `package-initialize'.
;            (message (concat "Customize Emacs Leuven to ignore "
;                             "the `%s' package next times...") pkg)
;            (sit-for 1.5))))))
;
;;The following code will be used when Emacs 25 will be made mandatory:
;
;(setq package-selected-packages
;      '(auctex ess
;        company
;        js2-mode
;        magit
;        pdf-tools
;        paredit
;        visual-regexp))
;
;(package-initialize)
;(when (fboundp 'package-install-selected-packages) ; Emacs-v25
;  (package-install-selected-packages))
;
;; End comment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
))
(setq load-prefer-newer t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;; Load elisp libraries while Emacs is idle.
;https://nschum.de/src/emacs/idle-require/
;(use-package idle-require
;  :config
   ;; Idle time in seconds after which autoload functions will be loaded.
;  (setq idle-require-idle-delay 5)
  ;; Time in seconds between automatically loaded functions.
;  (setq idle-require-load-break 2)
  ;; Starts loading.
  ;(add-hook 'after-init-hook #'idle-require-mode)
; )



;(try-require 'idle-require)
;
;;; Fail-safe for `idle-require'.
;(if (not (featurep 'idle-require))
;  (defun idle-require (feature &optional file noerror)
;    (try-require feature)))
;
;(with-eval-after-load "idle-require"
;
;  ;; Idle time in seconds after which autoload functions will be loaded.
;  (setq idle-require-idle-delay 5)
;
;  ;; Time in seconds between automatically loaded functions.
;  (setq idle-require-load-break 2)
;
;  ;; Starts loading.
;  (add-hook 'after-init-hook #'idle-require-mode))
)                                       ; Chapter 48 ends here.


