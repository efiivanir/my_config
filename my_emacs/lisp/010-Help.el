; * Help

;In general, the following functions are very interesting for finding useful functions and documentation.
;
;apropos-documentation (C-h d)
;    Show symbols whose documentation contains matches for PATTERN. For example, C-h d yank properties RET.
;describe-function (C-h f)
;    Used with TAB completion to show documentation of functions.
;elisp-index-search (C-h E in Leuven Emacs Config)
;    Look up documentation on broad Emacs Lisp topics (in the indices of the Emacs Lisp Reference Manual).
;info-lookup-symbol (C-h S, or C-f1 in Leuven Emacs Config)
;    Bring the relevant section from the manual, which describes the topic in more detail.
;
;With all prefix keys, if you follow them with C-h (h for help), a list of key bindings in that prefix map is displayed. This is automatic and is one of the self-documenting features of Emacs.
;
;Try, for example C-x C-h or C-c C-x C-h to see what it does. I do use it to remind myself of rarely used rectangle commands: C-x r C-h.
;
;FYI, catman create the database files that are used by apropos or man -k.
;
;Note about PHP documentation lookup – PHP Mode has a nice feature to lookup the function’s definition in PHP’s manual in your web browser (M-x
;  php-search-documentation or C-c C-f). You can combine it with EWW to get the relevant manual page without leaving Emacs.
;
;Close the help buffer by pressing Q.

;;* 10 (info "(emacs)Help")

(leuven--chapter leuven-load-chapter-10-help "10 Help"

; * Help Summary

;;** 10.1 (info "(emacs)Help Summary")

  (leuven--section "10.1 (emacs)Help Summary")

  ;; Avoid the description of all minor modes.
  (defun leuven-describe-major-mode ()
    "Describe only `major-mode'."
    (interactive)
    (describe-function major-mode))

  ;; Look up subject in (the indices of the) Emacs Lisp manual.
  (global-set-key (kbd "C-h E") #'elisp-index-search)

; * Apropos

;You can ask what pertains to a given topic by typing M-x apropos RET pattern
;  RET.

;;** 10.4 (info "(emacs)Apropos")

  (leuven--section "10.4 (emacs)Apropos")

  (with-eval-after-load "apropos"

    ;; Apropos commands will search more extensively, checking all variables and
    ;; non-interactive functions as well.
    (setq apropos-do-all t))

  ;; (defun apropos-user-option (string)
  ;;   "Like apropos, but lists only symbols that are names of user
  ;; modifiable variables.  Argument REGEXP is a regular expression.
  ;;    Returns a list of symbols, and documentation found"
  ;;   (interactive "sVariable apropos (regexp): ")
  ;;   (let ((message
  ;;          (let ((standard-output (get-buffer-create "*Help*")))
  ;;            (print-help-return-message 'identity))))
  ;;     (if (apropos string  'user-variable-p)
  ;;         (and message (message message)))))

  ;; Show all variables whose name matches the pattern.
  (define-key help-map (kbd "A") #'apropos-user-option)

; * Misc Help Commands

; * update-info-dir

;The utility install-info is used to maintain the info/dir file.

; C-h K goes to the node in the Emacs manual describing the command bound to a key.

; In a manual, these key bindings will make your life easier:

; ^
;    Move “up” from this node.
;]
;    Go forward one node, considering all nodes as forming one sequence.
;l
;    Move back in history to the last node you were at.
;L
;    Go to menu of visited nodes.
;i
;    Search for a topic in this manual’s Index and go to index entry.
;s
;    Search for a regexp and select node it’s found in.

;;** 10.8 (info "(emacs)Misc Help")

  (leuven--section "10.8 (emacs)Misc Help")

  ;; Enter Info documentation browser.
  ;(global-set-key (kbd "<f1>") #'info)

  (defun leuven-describe-elisp-symbol-at-point ()
    "Get help for the symbol at point."
    (interactive)
    (let ((sym (intern-soft (current-word))))
      (unless
          (cond ((null sym))
                ((not (eq t (help-function-arglist sym)))
                 (describe-function sym))
                ((boundp sym)
                 (describe-variable sym)))
        (message "nothing"))))

  (global-set-key (kbd "<f1>") #'leuven-describe-elisp-symbol-at-point)

  ;; Display symbol definitions, as found in the relevant manual
  ;; (for AWK, C, Emacs Lisp, LaTeX, M4, Makefile, Sh and other languages that
  ;; have documentation in Info).
  (global-set-key (kbd "<C-f1>") #'info-lookup-symbol)

;When Info is called, the variable Info-directory-list is populated from:

;    the INFOPATH environment variable, or (if unset)
;    the Info-default-directory-list variable – non-existent directories will be removed when copied to Info-directory-list

;The best would be to set the INFOPATH environment variable so that you see the same manuals outside of Emacs, in the same shell from which you invoke Emacs.

;However, as Windows Emacs doesn’t see that Cygwin environment variable (it would well work with a Windows environment variable), I prefer to try and put the Windows configuration inside the Emacs configuration files.

;Normally, Info-directory-list is not intended to be settable by the user. But we must do so if we want to force our Info manuals before the standard ones (from Emacs). XXX Could we set Info-default-directory-list instead???

(with-eval-after-load "info"
  ;; List of directories to search for Info documentation files (in the order
  ;; they are listed).
  (when leuven--win32-p
    ;; (info-initialize)
    (setq Info-directory-list
          `(,(expand-file-name
              (concat (file-name-directory (locate-library "org")) "../doc/"))
            "c:/cygwin/usr/share/info/"
            ,@Info-directory-list)))

  ;; XXX Replace by add-to-list to ensure we don't insert duplicates (if Cygwin was already there).

;Info+ fontifies entries for reference items (functions, macros, commands, special forms, constants, options, other variables), and that includes their parameters, even those on continuation lines.

;With Info+, you can also merge an Info node with its subnodes into the same buffer, by calling Info-merge-subnodes (bound to +).

(with-eval-after-load "info+-autoloads"
  (idle-require 'info+))

(with-eval-after-load "info+"

  ;; Show breadcrumbs in the header line.
  (setq Info-breadcrumbs-in-header-flag t)

  ;; Don't show breadcrumbs in the mode line.
  (setq Info-breadcrumbs-in-mode-line-mode nil))

;In an Info page, w will copy the reference to the current node (the (%s)%s part of your format string).

;When called with a 0 argument (M-0 w), you get (info \"(%s)%s\").

)

;; Get a Unix manual page of the item under point.
(global-set-key (kbd "<S-f1>") #'man-follow)

(with-eval-after-load "man"
  ;; Make the manpage the current buffer in the current window.
  (setq Man-notify-method 'pushy))

;; Alias man to woman.
(defalias 'man 'woman)

;; Decode and browse Unix man-pages "W.o. (without) Man".
(with-eval-after-load "woman"
  (defalias 'man 'woman))

)                                       ; Chapter 10 ends here.
