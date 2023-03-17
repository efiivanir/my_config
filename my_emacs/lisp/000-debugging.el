; * Debugging

;;* Debugging

(leuven--chapter leuven-load-chapter-0-debugging "0 Debugging"

;; Get the backtrace when uncaught errors occur.
(setq debug-on-error t)               ; Will be unset at the end.

;; Hit `C-g' while it's frozen to get an Emacs Lisp backtrace.
(setq debug-on-quit t)                ; Will be unset at the end.

)                                       ; Chapter 0 ends here.


