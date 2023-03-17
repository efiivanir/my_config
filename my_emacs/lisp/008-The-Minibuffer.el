; * The Minibuffer

;;* 8 The (info "(emacs)Minibuffer")

(leuven--chapter leuven-load-chapter-8-minibuffer "8 The Minibuffer"

  ;; How long to display an echo-area message when the minibuffer is active.
  (setq minibuffer-message-timeout 0.5)

; * Editing in the Minibuffer

;If the output is short enough to display in the echo area (which is determined by the variables resize-mini-windows and max-mini-window-height), it is shown in echo area.

;;** 8.3 (info "(emacs)Minibuffer Edit")ing

  (leuven--section "8.3 (emacs)Minibuffer Editing")

  ;; Minibuffer and echo area windows resize vertically as necessary to fit
  ;; the text displayed in them.
  (setq resize-mini-windows t)

; * Completion

;;** 8.4 (info "(emacs)Completion")

  (leuven--section "8.4 (emacs)Completion")

  ;; Don't consider case significant in completion.
  (setq completion-ignore-case t)

;Within read-file-name, this variable is overridden by read-file-name-completion-ignore-case.

;; Ignore case when reading a file name.
(setq read-file-name-completion-ignore-case t) ; [Default: t on Windows]

;Within read-buffer, completion-ignore-case is overridden by read-buffer-completion-ignore-case.

;; Ignore case when reading a buffer name.
(setq read-buffer-completion-ignore-case t) ; [Default: nil].

;; Provide the same facility of `ls --color' inside Emacs.
(when (locate-library "dircolors")
  (autoload 'dircolors "dircolors" nil t)
  (add-hook 'completion-list-mode-hook #'dircolors))

;Repeating Minibuffer Commands

;Use M-x list-command-history to display the entire command history, showing all the commands C-x <ESC> <ESC> can repeat, most recent first.

)                                       ; Chapter 8 ends here.
