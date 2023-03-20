;;; init.el --- Where all the magic begins
;;
;; This file allows Emacs to initialize my customizations
;; in Emacs lisp embedded in *one* literate Org-mode file.

;; This sets up the load path so that we can override it
(package-initialize nil)
;; Override the packages with the git version of Org and other packages
;(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp/")

;; Load the rest of the packages
(setq package-enable-at-startup nil)

(require 'org)
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/000-Init.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/000-Loading-Libraries.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/000-Environment.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/000-Debugging.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/048-Emacs-Lisp-Packages.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/001-The-Organization-of-the-Screen.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/006-Exiting-Emacs.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/007-Basic-Editing-Commands.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/008-The-Minibuffer.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/010-Help.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/011-The-Mark-and-the-Region.org")
(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/012-Killing-and-Moving-Text.org")

(org-babel-load-file "/home/ivanir/my_config/my_emacs/lisp/049-Customization.org")

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;(load custom-file)


;;; init.el ends here
