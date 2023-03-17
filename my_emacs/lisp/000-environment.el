; * Environment

;;* Environment

(leuven--chapter leuven-load-chapter-0-environment "0 Environment"

;;** Type of OS

  (leuven--section "Type of OS")

  (defconst leuven--linux-p
    (eq system-type 'gnu/linux)
    "Running a GNU/Linux version of Emacs.")

  (defconst leuven--mac-p
    (eq system-type 'darwin)
    "Running a Mac OS version of Emacs.")

  (defconst leuven--win32-p
    (eq system-type 'windows-nt)
    "Running a native Microsoft Windows version of Emacs.")

  (defconst leuven--cygwin-p
    (eq system-type 'cygwin)
    "Running a Cygwin version of Emacs.")

;;** MS Windows

  (defconst leuven--windows-program-files-dir ; sys-path.
    (cond (leuven--win32-p
           (file-name-as-directory (getenv "ProgramFiles(x86)")))
          (leuven--cygwin-p
           "/cygdrive/c/Program Files (x86)/")
          (t
           "/usr/local/bin/"))
    "Default Windows Program Files folder.")

;Use (display-graphic-p) (instead of window-system) in conditions.

;;** Window system

  (leuven--section "Window system")

  (defconst leuven--console-p
    (eq window-system nil)
    "Running a text-only terminal.")

  (defconst leuven--x-window-p
    (eq window-system 'x)
    "Running a X Window system.")

;;** Testing file accessibility

  (defun leuven--file-exists-and-executable-p (file)
    "Make sure the file FILE exists and is executable."
    (if file
        (if (file-executable-p file)
            file
          (message "WARN- Can't find executable `%s'" file)
          ;; Sleep 1.5 s so that you can see the warning.
          (sit-for 1.5))
      (error "Missing argument to \"leuven--file-exists-and-executable-p\"")))

;;** Init

  (leuven--section "Init")

  ;; Ensure that the echo area is always visible during the early stage of
  ;; startup (useful in case of error).
  (modify-all-frames-parameters
   '((height . 32)))

)                                       ; Chapter 0 ends here.

;; Function for finding executables See function --find-program at http://www.visualco.de/src/elisp/.emacs.el.html.END
