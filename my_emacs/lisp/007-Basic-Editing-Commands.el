; * Basic Editing Commands

;;* 7 (info "(emacs)Basic") Editing Commands

(leuven--chapter leuven-load-chapter-7-basic "7 Basic Editing Commands"

; Inserting Text

;;** 7.1 (info "(emacs)Inserting Text")

  (leuven--section "7.1 (emacs)Inserting Text")

  ;; Enter characters by their code in octal (for `C-q NNN RET').
  (setq read-quoted-char-radix 8)       ; 16 for hexadecimal (for Unicode char)

;Moving Point Location

;M-r
;    move-to-window-line.
;M-g g
;    Jump to the beginning of line number N (goto-line).

;;** 7.2 (info "(emacs)Moving Point") Location

  (leuven--section "7.2 (emacs)Moving Point Location")

  ;; Don't add newlines to end of buffer when scrolling.
  (setq next-line-add-newlines nil)

  ;; Print the current buffer line number.
  (global-set-key (kbd "M-G") #'what-line)

;Show line numbers temporarily – just when you’re going to a line number with goto-line.
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key [remap goto-line] #'goto-line-with-feedback)

;Basic Undoing Changes

;;** 7.4 (info "(emacs)Basic Undo")ing Changes

  (leuven--section "7.4 (emacs)Basic Undoing Changes")

  ;; Undo some previous changes.
  (global-set-key (kbd "C-z") #'undo)
  (global-set-key (kbd "<f11>") #'undo)

(with-eval-after-load "volatile-highlights-autoloads"
  (volatile-highlights-mode 1))

;; Treat undo history as a tree.
(with-eval-after-load "undo-tree-autoloads"

  ;; Enable Global-Undo-Tree mode.
  (global-undo-tree-mode 1))

(with-eval-after-load "undo-tree"

  (with-eval-after-load "diminish-autoloads"
    (diminish 'undo-tree-mode))

  ;; Display times relative to current time in visualizer.
  (setq undo-tree-visualizer-relative-timestamps t)

  ;; Display time-stamps by default in undo-tree visualizer.
  (setq undo-tree-visualizer-timestamps t)
                                      ; Toggle time-stamps display using `t'.

  ;; Display diff by default in undo-tree visualizer.
  (setq undo-tree-visualizer-diff t)  ; Toggle the diff display using `d'.

  (define-key undo-tree-map (kbd "C-/") nil)

  ;; (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-S-z") #'undo-tree-redo)
  (global-set-key (kbd "<S-f11>") #'undo-tree-redo))

)                                       ; Chapter 7 ends here.
