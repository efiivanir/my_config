(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(frame-resize-pixelwise t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(dired-hide-dotfiles dired-toggle diredfl dired-narrow dired-git-info treemacs lsp-treemacs org-bullets company-lsp helm-lsp doom-modeline dashboard quickrun yasnippet company helm use-package async))
 '(scroll-bar-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defalias 'yes-or-no-p 'y-or-n-p)
(package-initialize)
(require 'package)
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

