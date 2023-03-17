; * Registers

;;* 13 (info "(emacs)Registers")

(leuven--chapter leuven-load-chapter-13-registers "13 Registers"

; *  Saving Positions in Registers

         ;;** 13.1 (info "(emacs)Position Registers")
     (leuven--section "13.1 (emacs)Position Registers")

     ;; C-x r SPC
     ;; point-to-register. Type any character to specify a register when prompted.
     ;; C-x r j
     ;; jump-to-register.

     (global-set-key (kbd "C-j") #'jump-to-register) ; Also on `C-x r j'.

    ;;Setup registers for files I commonly edit.

     (set-register ?a '(file . "/sudo::/etc/apt/sources.list"))
     (set-register ?b '(file . "~/.bashrc"))
     (set-register ?e `(file . ,(concat leuven--directory "emacs-leuven.txt")))
     (when (file-exists-p "~/org/personal/Personal.org")
       (set-register ?p '(file . "~/org/personal/Personal.org")))
     (when (file-exists-p "~/org/refile.org")
       (set-register ?r '(file . "~/org/refile.org")))
     (when (file-exists-p "~/org/work/Work.org")
       (set-register ?w '(file . "~/org/work/Work.org")))
     (set-register ?z '(file . "~/.zshrc"))

; *     Bookmarks

     ;; If you need to move to somewhere in your document and want a quick way to return, you can place a bookmark on the current line.

     ;; This is done by pressing C-x r m RET or C-x r m BOOKMARK RET.

     ;; When you want to return to a bookmark, you can press C-x r b. You’ll be prompted for the bookmark name, and it will open that file or directory.

     ;;** 13.7 (info "(emacs)Bookmarks")

     (leuven--section "13.7 (emacs)Bookmarks")
     ;; (use-package bookmarks
     ;;   :config
       ;; Where to save the bookmarks.
       (setq bookmark-default-file (concat user-emacs-directory "bookmarks.bmk"))
                            ;! A `.txt' extension would load Org at
                            ;! the time `bookmark' is required!
       ;; Each command that sets a bookmark will also save your bookmarks.
       (setq bookmark-save-flag 1)
       ;; Visible bookmarks in buffer.
       ;; Bookmark+ offers everything that bm.el offers, and quite a bit more.
       ;; You can bookmark arbitrary sets of files, from any locations.
       ;; You can bookmark Dired buffers (which, again, can actually list arbitrary files, not even necessarily in the same directory). Markings, subdir inclusions, and omissions are all recorded, and restored when you access the bookmark.
       ;; You can tag bookmarks or files, using arbitrary strings as tags. You can do this programmatically and interactively, by regexp, name, or Dired markings. Tags give you a great way to define sets of bookmarks or files – sets that can overlap, etc. They serve to categorize, but they can do more than that. You can use tags to, in effect, merge projects, split projects, define subprojects, and so on.
       ;; Key bindings:
       ;; C-x p RET (C-F2 in Emacs Leuven, Sublime Text and TextMate)
       ;; Toggle (set or delete) a bookmark at point.
       ;; C-x p C-down (S-F2 in Emacs Leuven, F2 in Sublime Text and TextMate)
       ;; Cycle to the next highlighted bookmark in the current buffer.
       ;; C-x p C-up (S-F2 in Sublime Text and TextMate)
       ;; Cycle to the previous highlighted bookmark in the current buffer.
       ;; C-x (4) j h
       ;; Jump to a highlighted bookmark (in another window).
       ;; C-x p ,
       ;; Show the bookmark list just for bookmarks for the current file/buffer.
       ;; C-S-f2 (in Emacs Leuven)
       ;; Clear all bookmarks for the current file/buffer.
       ;; Buffer-local nature of the bookmarks.
       ;; Annotating bookmarks.
       ;; (setq bm-marker
       ;; 	     ‘bm-marker-right bm-repository-file
       ;; 	      (concat emacs-persistence-directory “bm-repository”)
       ;; 	      bm-recenter t bm-highlight-style ‘bm-highlight-only-fringe
       ;; )
       ;; (global-set-key (kbd “C-c m m”) ‘bm-toggle)
       ;; (global-set-key (kbd “<right-fringe> <mouse-5>”) ‘bm-next-mouse) (global-set-key (kbd “<right-fringe> <mouse-4>”) ‘bm-previous-mouse)
       ;; (global-set-key (kbd “<right-fringe> <mouse-1>”) ‘bm-toggle-mouse)
;)
       ;; (use-package bm
       ;;   :ensure t
       ;;   :demand t
       ;; 	 :config
       ;; 	 (defvar bm-mode-map
       ;; 	   (let ((map (make-sparse-keymap)))
       ;; 	     (define-key map (kbd “m”) ‘bm-toggle)
       ;; 	     (define-key map (kbd “n”) (make-repeatable-command ‘bm-next))
       ;; 	     (define-key map (kbd “p”) (make-repeatable-command ‘bm-previous))
       ;; 	     (define-key map (kbd “L”) ‘bm-show-all)
       ;; 	     (define-key map (kbd “l”) ‘bm-show)
       ;; 	     (define-key map (kbd “s”) ‘bm-save)
       ;; 	     (define-key map (kbd “r”) ‘bm-load-and-restore) map) “Keymap for `bm.el’.”)
       ;; 	     (global-set-key (kbd “C-c m”) bm-mode-map)
       ;; 	 )

    ;;  (use-package bookmark+
    ;;    :config
    ;;    (global-set-key (kbd "<C-f2>") #'bmkp-toggle-autonamed-bookmark-set/delete)
    ;;    (global-set-key (kbd "<S-f2>") #'bmkp-next-bookmark-this-file/buffer-repeat)
    ;;    (global-set-key (kbd "<C-S-f2>") #'bmkp-delete-all-autonamed-for-this-buffer)
    ;;    (setq bmkp-light-left-fringe-bitmap 'filled-square)
    ;;    (setq bmkp-light-right-fringe-bitmap 'filled-square)
    ;;      ;; Default highlight style for autonamed (= default) bookmarks.
    ;;    (setq bmkp-light-style-autonamed 'line+lfringe)
    ;;      ;; Default highlight style for non-autonamed bookmarks.
    ;;    (setq bmkp-light-style-non-autonamed 'lfringe)
    ;;      ;; Automatically highlight bookmarks when set.
    ;;    (setq bmkp-auto-light-when-set 'any-bookmark)
    ;;      ;; Automatically highlight bookmarks when jumped to.
    ;;    (setq bmkp-auto-light-when-jump 'any-bookmark)
    ;;      ;; Don't propertize bookmark names to hold full bookmark data.
    ;;    (setq bmkp-propertize-bookmark-names-flag nil)
    ;;                         ; We will often be going back and forth
    ;;                         ; between using Bookmark+ and using
    ;;                         ; vanilla Emacs.
    ;;      ;; (setq bmkp-last-as-first-bookmark-file bookmark-default-file)

    ;;      ;; ;; Restoring bookmarks when on file find.
    ;;      ;; (add-hook 'find-file-hook #'bm-buffer-restore)
    ;; )

; *     Ace Jump
     (use-package ace-jump-mode
       :config
       (global-set-key (kbd "C-c SPC") #'ace-jump-mode)
       ;; Pop up a postion from ‘ace-jump-mode-mark-ring’, and jump back to that
       ;; position.
       (global-set-key (kbd "C-c C-SPC") #'ace-jump-mode-pop-mark)
       )
     (use-package ace-link
       :config
       ;; Setup the defualt shortcuts.
       (ace-link-setup-default "f")
     )

     ;; Jump to things.
     (use-package avy
       :config
       ;; Default keys for jumping.
       (setq avy-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
                           ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z))
       ;; Ace-jump during Isearch to one of the current candidates.
       (define-key isearch-mode-map (kbd "C-'") 'avy-isearch)
    )

     )                                       ; Chapter 13 ends here.
