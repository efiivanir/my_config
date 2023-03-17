; * Exiting Emacs

;;* 6 (info "(emacs)Exiting") Emacs

(leuven--chapter leuven-load-chapter-6-exiting "6 Exiting Emacs"

  ;; Unbind "minimize".
  (global-unset-key (kbd "C-z"))

  ;; Quit with Alt + F4.
;  (global-set-key (kbd "<M-f4>") #'save-buffers-kill-terminal)

)                                       ; Chapter 6 ends here.
