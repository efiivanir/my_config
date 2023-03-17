; * Header
;;
;; To get more debug info about the packages getting loaded, add the following
;; line before requiring Leuven Emacs Config.
;;
;;     ;; show messages describing progress of loading Leuven Emacs Config
     (setq leuven-load-verbose t)
;;
;; To avoid be questioned about packages to add to your local Emacs
;; installation (though, I think you should install them), add the following
;; line before requiring Leuven Emacs Config.
;;
;;     ;; do not (try to) install extra Emacs packages
     (setq leuven-elpa-packages nil)
;;
; * Code:

;;;;;;;;;;;;;;;; Remove it after implement them::::::::::::::::::::::::::::::::::::::::::::::
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "yellow" :distant-foreground "gtk_selection_fg_color")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst leuven--emacs-version "20160625.1211"
  "Leuven Emacs Config version (date of the last change).")

(message "* --[ Loading Leuven Emacs Config %s]--" leuven--emacs-version)

;; Turn on Common Lisp support.
(eval-when-compile (require 'cl-lib))       ; Provide useful things like `setf'.

(defconst leuven--before-time (float-time)
  "Value of `float-time' before loading the Leuven Emacs Config library.")

(defmacro measure-time (message &rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((start (current-time)))
     ,@body
     (message "__%s (in %.02f s)___________________________"
              ,message (float-time (time-since start)))))

;;; User Customizable Internal Variables

(defgroup leuven nil
  "Set of Emacs customizations (better defaults)."
  :group 'convenience
  :group 'text)

(defcustom leuven-load-verbose nil
  "If non-nil, means show messages describing progress of loading Leuven Emacs Config."
  :group 'emacs-leuven
  :type 'integer)

(when (and (string-match "GNU Emacs" (version))
           leuven-load-verbose)
  (defadvice message (before leuven-when-was-that activate)
    "Add time stamps to `message' output."
    (ad-set-arg 0 (concat (format-time-string "[%Y-%m-%d %T.")
                          (substring (format-time-string "%N") 0 3)
                          (format-time-string "] ")
                          (ad-get-arg 0)))))

;; Allow quick include/exclude of setup parts -- DO NOT EDIT the DEFVAR!
(defvar leuven-load-chapter-0-environment t) ; required
(defvar leuven-load-chapter-0-loading-libraries t) ; required
(defvar leuven-load-chapter-0-debugging t)
(defvar leuven-load-chapter-48-packages t)
(defvar leuven-load-chapter-1-screen t)
(defvar leuven-load-chapter-6-exiting t)
(defvar leuven-load-chapter-7-basic t)
(defvar leuven-load-chapter-8-minibuffer t)
(defvar leuven-load-chapter-10-help t)
(defvar leuven-load-chapter-11-mark t)
(defvar leuven-load-chapter-12-killing t)
(defvar leuven-load-chapter-13-registers t)
(defvar leuven-load-chapter-14-display t)
(defvar leuven-load-chapter-15-search t)
(defvar leuven-load-chapter-16-fixit t)
(defvar leuven-load-chapter-17-keyboard-macros t)
(defvar leuven-load-chapter-18-files t)
(defvar leuven-load-chapter-19-buffers t)
(defvar leuven-load-chapter-20-windows t)
(defvar leuven-load-chapter-21-frames t)
(defvar leuven-load-chapter-22-international t)
(defvar leuven-load-chapter-23-major-and-minor-modes t)
(defvar leuven-load-chapter-24-indentation t)
(defvar leuven-load-chapter-25-text t)
(defvar leuven-load-chapter-25.10-org-mode t)
(defvar leuven-load-chapter-25.11-tex-mode t)
(defvar leuven-load-chapter-26-programs t)
(defvar leuven-load-chapter-27-building t)
(defvar leuven-load-chapter-28-maintaining t)
(defvar leuven-load-chapter-29-abbrevs t)
(defvar leuven-load-chapter-30-dired t)
(defvar leuven-load-chapter-31-calendar-diary t)
(defvar leuven-load-chapter-32-sending-mail t)
(defvar leuven-load-chapter-34-gnus t)
(defvar leuven-load-chapter-36-document-view t)
(defvar leuven-load-chapter-38-shell t)
(defvar leuven-load-chapter-39-emacs-server t)
(defvar leuven-load-chapter-40-printing t)
(defvar leuven-load-chapter-41-sorting t)
(defvar leuven-load-chapter-44-saving-emacs-sessions t)
(defvar leuven-load-chapter-46-hyperlinking t)
(defvar leuven-load-chapter-47-amusements t)
(defvar leuven-load-chapter-49-customization t)
(defvar leuven-load-chapter-AppG-ms-dos t)
(defvar leuven-load-chapter-XX-emacs-display t)
(defvar leuven-load-chapter-99-debugging t)

(defvar leuven--load-times-list nil
  "List of chapters and time to load them.")

(defmacro leuven--chapter (chapterid chaptername &rest body)
  "When CHAPTERID is not nil, report as CHAPTERNAME the evaluation of BODY.
Save execution times in the global list `leuven--load-times-list'."
  `(when ,chapterid
     (let (before-chapter-time
           this-chapter-time)
       (when leuven-load-verbose
         (message "** %s" ,chaptername))
       (setq before-chapter-time (float-time))
       (setq leuven--before-section-time (float-time)) ; Init section time.
       (progn ,@body)
       (leuven--section (concat "[" ,chaptername " ends here]") 'end-of-chapter)
                                        ; Add fake closing section.
       (setq this-chapter-time
             (format "%.3f" (- (float-time) before-chapter-time)))
       (add-to-list 'leuven--load-times-list
                    (concat "| " ,chaptername " "
                            "| " this-chapter-time " |")))))

(defvar leuven--before-section-time (float-time)
  "Value of `float-time' before loading some section.")

(defun leuven--section (sectionname &optional end-of-chapter)
  "Report under SECTIONNAME the time taken since it was last saved.
Last time is saved in global variable `leuven--before-section-time'."
  (let ((this-section-time (- (float-time)
                              leuven--before-section-time)))
    (when leuven-load-verbose
      (when (not (equal this-section-time 0.000))
        (message "    Section time: %.3f s" this-section-time))
      (unless end-of-chapter (message "*** %s" sectionname)))
    ;; For next one.
    (setq leuven--before-section-time (float-time))))

(load-file "/home/ivanir/my_config/my_emacs/lisp/000-Loading-libraries.el")
(load "000-environment")
(load "000-debugging")
(load "048-packages")
(load "001-screen")
(load "006-exiting")
(load "007-Basic-Editing-Commands")
(load "008-The-Minibuffer")
(load "010-Help")
(load "011-The-Mark-and-the-Region")
(load "012-Killing-and-Moving-Text")
(load "013-Registers")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-link use-package multiple-cursors goto-chg expand-region bm avy ace-jump-mode)))
