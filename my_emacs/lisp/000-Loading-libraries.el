; * Loading Libraries of Lisp Code for Emacs

;; Adding the right Lisp directories to your load-path (list of directories where Emacs Lisp libraries – .el and .elc files – are installed) must be the very first thing in your .emacs file, before the first time packages are required, to make sure that you’re not picking up bits and pieces from older files (bundled with Emacs, and loaded before the path to the newest versions are set).

;; The most important directories are the last to be added to load-path (so that they become the first of the list)!

;; Use M-x list-load-path-shadows RET to display a list of external Emacs Lisp files that shadow Emacs builtins (listing potential load path problems).

;; Some Emacs modes are over 10K lines of code (e.g. nxml-mode, CEDET). Many packages (e.g. Org, Gnus) make use of the autoload feature, so that you only need to load a single file that define autoloaded functions.

;;* Loading Libraries of Lisp Code for Emacs

(leuven--chapter leuven-load-chapter-0-loading-libraries "0 Loading Libraries"

;; Load-path enhancement.
(defun leuven--add-to-load-path (this-directory)
  "Add THIS-DIRECTORY at the beginning of the load-path, if it exists."
  (when (and this-directory
             (file-directory-p this-directory))
    ;; TODO Add warning if directory does not exist.
    (let* ((this-directory (expand-file-name this-directory)))

      ;; directories containing a `.nosearch' file (such as
      ;; `auctex-11.88.6\style') should not made part of `load-path'.
      ;; TODO `RCS' and `CVS' directories should also be excluded.
      (unless (file-exists-p (concat this-directory "/.nosearch"))
        (add-to-list 'load-path this-directory)
        (when leuven-load-verbose
          (message "INFO- Added `%s' to `load-path'" this-directory))))))

;; Remember this directory.
(defconst leuven--directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory path of Leuven Emacs Config installation.")

;; (leuven--add-to-load-path
;;  (concat leuven--directory "site-lisp"))

;leuven--local-repos-directory is where you put Emacs Git/SVN/CSV repositories.

;; (defvar leuven--local-repos-directory "~/Public/Repositories/"
;;   "Directory containing additional Emacs Lisp public repositories.")

;; (leuven--add-to-load-path
;;  (concat leuven--local-repos-directory "babel"))
;; (leuven--add-to-load-path
;;  (concat leuven--local-repos-directory "emacs-bookmark-extension") ; XXX?
;;  )


;; leuven-user-lisp-directory is there so that you have an easy way of installing your own Emacs add-ons (which may be specific to the version of Emacs you’re running). This keeps your local add-ons apart from distro supplied ones.

;; It also means you can do a complete re-install of Emacs (or even your Linux distro) without impacting on stuff you have added by hand.

(defvar leuven-user-lisp-directory (concat user-emacs-directory "lisp/")
  "Directory containing personal additional Emacs Lisp packages.")

(leuven--add-to-load-path leuven-user-lisp-directory)

(defvar leuven-site-lisp-directory (concat user-emacs-directory "site-lisp/")
  "Directory containing saved Emacs Lisp packages.")

(leuven--add-to-load-path leuven-site-lisp-directory)



;; Require a feature/library if available; if not, fail silently.
(unless (fboundp 'try-require)
  (defun try-require (feature)
    "Attempt to load a FEATURE (or library).
  Return true if the library given as argument is successfully loaded.  If
  not, just print a message."
    (condition-case err
        (progn
          (if (stringp feature)
              (load-library feature)
            (require feature))
          t)                          ; Necessary for correct behavior in
                                      ; conditional expressions.
      (file-error
       (message "Requiring `%s'... missing" feature)
       nil))))

;; TEMPORARY.
(unless (fboundp 'with-eval-after-load)
  ;; Wrapper around `eval-after-load' (added in GNU Emacs 24.4).
  (defmacro with-eval-after-load (mode &rest body)
    "`eval-after-load' MODE evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,mode
       '(progn ,@body))))

; * Creating and selecting buffers

(defun switch-or-start (function buffer)
  "If the BUFFER is current, bury it.  If there is a buffer with that name,
switch to it; otherwise, invoke the FUNCTION."
  (if (equal (buffer-name (current-buffer)) buffer)
      (bury-buffer)
    (if (get-buffer buffer)
        (switch-to-buffer buffer)
      (funcall function))))

(defun switch-or-find-file (file)
  "If the FILE is current, bury it.  If there is a buffer with that name,
switch to it; otherwise, open it."
  (when (file-exists-p file)
    (if (and (buffer-file-name)
             (string= (expand-file-name file)
                      (expand-file-name (buffer-file-name))))
        (bury-buffer)
      (find-file file))))

)                                       ; Chapter 0-loading-libraries ends here.
