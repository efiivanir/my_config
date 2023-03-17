; * Killing and Moving Text

;;* 12 (info "(emacs)Killing") and Moving Text

(leuven--chapter leuven-load-chapter-12-killing "12 Killing and Moving Text"

; * Deletion and Killing

;; C-S-backspace
;;     Kill an entire line at once.

;;** 12.1 (info "(emacs)Deletion and Killing")

  (leuven--section "12.1 (emacs)Deletion and Killing")

  ;; Manipulate whitespace around point in a smart way.
  (global-set-key (kbd "M-SPC") #'cycle-spacing) ; vs `just-one-space'.

;; old ([2012-09-07 Fri] remove "compile" after "activate")

  ;; Add the ability to copy the current line without marking it (no
  ;; selection).
  (defadvice kill-ring-save (before leuven-slick-copy activate)
    "When called with no active region, copy the current line instead."
    (interactive
     (if (use-region-p) (list (region-beginning) (region-end))
       (message "Copied the current line")
       (list (line-beginning-position)
             (line-beginning-position 2)))))

  ;; Add the ability to cut the current line without marking it (no
  ;; selection).
  (defadvice kill-region (before leuven-slick-cut activate)
    "When called with no active region, kill the current line instead."
    (interactive
     (if (use-region-p) (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

;; new

    ;; (defadvice kill-ring-save (around leuven-slick-copy activate)
    ;;   "When called interactively with no active region, copy a single line instead."
    ;;   (if (or (use-region-p) (not (called-interactively-p 'any)))
    ;;       ad-do-it
    ;;     (kill-new (buffer-substring (line-beginning-position)
    ;;                                 (line-beginning-position 2)))
    ;;     (message "Copied line")))
    ;;
    ;; (defadvice kill-region (around leuven-slick-cut activate)
    ;;   "When called interactively with no active region, kill a single line instead."
    ;;   (if (or (use-region-p) (not (called-interactively-p 'any)))
    ;;       ad-do-it
    ;;     (kill-new (filter-buffer-substring (line-beginning-position)
    ;;                                        (line-beginning-position 2) t))))
    ;;
    ;; (defun yank-line (string)
    ;;   "Insert STRING above the current line."
    ;;   (beginning-of-line)
    ;;   (unless (= (elt string (1- (length string))) ?\n)
    ;;     (save-excursion (insert "\n")))
    ;;   (insert string))

;; XXX perf 2.00 s requiring bytecomp and warnings...

(defun duplicate-current-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(global-set-key (kbd "C-c d") #'duplicate-current-line)

; * Yanking

;;** 12.2 (info "(emacs)Yanking")

  (leuven--section "12.2 (emacs)Yanking")

  ;; Auto-indentation of pasted code in the programming modes (fall back to
  ;; default, non-indented yanking by preceding the yanking command `C-y' with
  ;; `C-u').
  (dolist (command '(yank
                     yank-pop))
    (eval `(defadvice ,command (after leuven-indent-region activate)
             "Indent `yank'ed text if programming mode (and no prefix)."
             (let ((mark-even-if-inactive t))
               (and (not current-prefix-arg)
                    (derived-mode-p 'prog-mode)
                    (indent-region (region-beginning) (region-end) nil))))))

;; Save clipboard strings into kill ring before replacing them.
(setq save-interprogram-paste-before-kill t)

;; ;; Rotating the kill ring changes the window system selection.
;; (setq yank-pop-change-selection t)

;; Yanking Earlier Kills

;; Interactively insert items from kill ring with M-x helm-show-kill-ring (see Helm).
;; Cutting and Pasting on Graphical Displays

;;** 12.3 (info "(emacs)Cut and Paste")

  (leuven--section "12.3 (emacs)Cut and Paste on Graphical Displays")

  ;; Make cut, copy and paste (keys and menu bar items) use the clipboard.
  (menu-bar-enable-clipboard)

; * Rectangles

;; To kill the text of a rectangular area (vertically selected text), use C-x r k (kill-rectangle). Or just “delete” the “region-rectangle” (without “killing” it) with C-x r d.

;; To copy a (series of) Org column(s) while avoiding the use of registers:

;;     select the region-rectangle,
;;     use the command copy-rectangle-as-kill (bound to C-x r M-w), then
;;     paste the copied rectangle by doing yank-rectangle (C-x r y).

;; To do the same with registers:

;;     select the region-rectangle,
;;     use C-x r r R to copy the rectangle to the register named R,
;;     use C-x r i R to insert the rectangle that is being held in the register named R.

;; To shift cells up/down within a column of an Org table while leaving remaining columns intact, use kill-rectangle and yank-rectangle.

;; To delete whitespace in each of the lines on the specified rectangle, use M-x delete-whitespace-rectangle.

;; Use C-x r t STRING RET to replace each line of a region-rectangle with a given string.
;; CUA Bindings

;; CUA mode sets up key bindings used in many other applications (C-x, C-c, C-v and C-z).

;; The C-x and C-c keys only do cut and copy when the region is active, so in most cases, they do not conflict with the normal function of these prefix keys.

;; If you really need to perform a command which starts with one of the prefix keys even when the region is active, you have three options:

;;     press the prefix key twice very quickly (within 0.2 seconds),
;;     press the prefix key and the following key within 0.2 seconds, or
;;     use the Shift key with the prefix key, i.e. C-S-x or C-S-c.

;; You can customize cua-enable-cua-keys to completely disable the CUA bindings, or cua-prefix-override-inhibit-delay to change the prefix fallback behavior.

;; CUA mode also provides enhanced rectangle support with visible rectangle highlighting. Though, since Emacs 24.4, rectangle-mark-mode is the new way.

;;     <C-RET> runs the command cua-set-rectangle-mark
  ;;     M-n runs the command cua-sequence-rectangle
  (cua-mode t)
  (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
  (transient-mark-mode 1) ;; No region when it is not highlighted
  (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

)                                       ; Chapter 12 ends here.
