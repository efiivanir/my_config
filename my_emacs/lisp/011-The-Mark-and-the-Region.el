; * The Mark and the Region

;;* 11 The (info "(emacs)Mark") and the Region

(leuven--chapter leuven-load-chapter-11-mark "11 The Mark and the Region"

;; Setting the Mark

;; Instead of setting the mark in order to operate on a region, you can also type:

;; C-SPC C-SPC
;;     *”Remember” a position* in the buffer (set the mark where point is), and
;; C-u C-SPC
;;     Later jump back to the mark in the buffer. You can use this repeatedly to navigate through the entire mark ring.

;; Look at `back-button’ Visual navigation through mark rings.END

;; Go to the place where you last changed something:

		 ;; Goto last change.
(use-package goto-chg
  :config
  (global-set-key (kbd "<C-S-backspace>") #'goto-last-change))
  

;; It moves point through buffer-undo-list positions.
;; Commands to Mark Textual Objects

;; M-@
;;     Set mark after end of next word (mark-word) without moving point.
;; C-M-@
;;     Set mark after end of following balanced expression (mark-sexp) without moving point.

;; Increase selected region by semantic units.

(use-package expand-region
  :config
  (global-set-key (kbd "C-+") #'er/expand-region)
  (global-set-key (kbd "C--") #'er/contract-region)

  )


;; Those key bindings are overridden by text-scale-increaseEND
;; Operating on the Region

;; Inserting text while the mark is active causes the text in the region to be
;; deleted first.
(delete-selection-mode 1)             ; Overwrite region.

; * Shift Selection

;; Shifted motion keys activate the mark momentarily.
;; Multiple selections

;; You can watch an intro to multiple-cursors at Emacs Rocks.

;; In other editors:

;;     https://www.sublimetext.com/docs/2/multiple_selection_with_the_keyboard.html
;;     https://docs.c9.io/multiple_cursors.html
;;     http://komodoide.com/screencasts/watch/87286656-multiple-selections/

;; Multiple cursors for Emacs.
(use-package multiple-cursors
  :config
  ;; Splitting the selection into lines
  ;; Make batch edits with Multiple Cursors: select a block of lines, and then split the region into lines which are then edited simultaneously.
  ;; Add a cursor to each (continuous) line in the current region.
  (global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines) ;!
  ;; Quick add next/previous
  ;; Use Multiple Cursors to rename variables quickly: add the next or previous occurrence of the current word to the selection.
  ;; Add a cursor and region at the next part of the buffer forwards that
  ;; matches the current region.
  (global-set-key (kbd "C->") #'mc/mark-next-like-this) ;!
  ;; Add a cursor and region at the next part of the buffer backwards that
  ;; matches the current region.
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this) ;!
  (global-set-key (kbd "C-M->") #'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-M-<") #'mc/skip-to-previous-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click)
  ;; Find All
  ;; Add all occurrences of the current word to the selection.
  (global-set-key (kbd "C-;") #'mc/mark-all-like-this-dwim) ;! Like Iedit.
  ;; (global-set-key (kbd "C-c C-w") #'mc/mark-all-like-this-dwim)
  ;; (global-set-key (kbd "C-x C-;") #'mc/mark-all-like-this-dwim)
  ;; Mark all parts of the buffer that matches the current region.
  (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this) ;!
  ;; (global-set-key (kbd "<C-RET>") #'mc/mark-more-like-this-extended) ; useful for completion
  (global-set-key (kbd "C-S-SPC") #'set-rectangular-region-anchor)
  (global-set-key (kbd "C-M-=") #'mc/insert-numbers)
  ;; Single Selection
  ;; To go from multiple selections to a single selection, press C-g or RET.
  ;; Multiple cursors for Emacs.
  ;; Commands to run for all cursors in multiple-cursors-mode.
  (setq mc/cmds-to-run-for-all
        '(cycle-spacing
          isearch-abort
          isearch-printing-char
          just-one-space
          kill-region
          leuven-fill-paragraph
          leuven-smart-punctuation-quotation-mark
          org-beginning-of-line
          org-end-of-line
          org-kill-line
          org-self-insert-command
          org-shiftdown
          org-shiftleft
          org-shiftright
          org-shiftup
          org-yank
          orgtbl-self-insert-command
          yas-expand))
  )


)                                       ; Chapter 11 ends here.
