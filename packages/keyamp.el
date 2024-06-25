;;; keyamp.el --- Keyboard Amplifier -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Egor Maltsev <x0o1@ya.ru>
;; Version: 1.0 2023-09-13 Koala Paw
;;      _                   _
;;    _|_|_               _|_|_
;;   |_|_|_|             |_|_|_|
;;           _ _     _ _
;;          | | |   | | |
;;          |_|_|   |_|_|

;; This package is part of input model.
;; Follow the link: https://github.com/xEgorka/keyamp

;;; Commentary:

;; Keyamp provides 3 modes: insert, command and repeat.
;; Command mode based on persistent transient keymap.

;; Repeat mode pushes transient remaps to keymap stack on top of
;; command mode for easy repeat of command chains during screen
;; positioning, cursor move and editing. Cursor shape indicates
;; active transient keymap. ESDF and IJKL are mostly used, DEL/ESC
;; and RET/SPC control EVERYTHING. Home row and thumb cluster only.
;; Repeat mode turned on/off *automatically* either by post command
;; hook or with idle timer.

;; DEL and SPC are two leader keys, RET activates insert mode, ESC
;; does command one. Holding down each of the keys posts control
;; sequence depending on mode. Keyboard has SYMMETRIC layout: left
;; side for editing, «No» and «Escape» while right side for moving,
;; «Yes» and «Enter». Any Emacs major or minor mode could be remapped
;; to fit the model, find examples in the package.

;; Karabiner integration allows to post control or leader sequences by
;; holding down a key. NO need to have any modifier or arrows keys at
;; ALL. Holding down posts leader layer. The same symmetric layout
;; might be configured on ANSI keyboard, ergonomic split and virtual
;; keyboards. See the link for layouts and karabiner config.

;; This package is a fork of xah-fly-keys.

;;; Code:



(defun get-bounds-of-block ()
  "Return the boundary (START . END) of current block."
  (let (xp1 xp2 (xblankRegex "\n[ \t]*\n"))
    (save-excursion
      (setq xp1 (if (re-search-backward xblankRegex nil 1)
                    (goto-char (match-end 0))
                  (point)))
      (setq xp2 (if (re-search-forward xblankRegex nil 1)
                    (match-beginning 0)
                  (point))))
    (cons xp1 xp2)))

(defun get-bounds-of-block-or-region ()
  "If region is active, return its boundary, else `get-bounds-of-block'."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (get-bounds-of-block)))


;; cursor movement

(defun up-line-wrap ()
  "On the first line go to the last line of buffer."
  (if (= (line-number-at-pos) 1)
      (forward-line (count-lines (point-min) (point-max)))))

(defun down-line-wrap (fun &rest r)
  "On the last line go to the first line of buffer."
  (if (= (line-number-at-pos) (+ 1 (count-lines (point-min) (point-max))))
      (forward-line (- (count-lines (point-min) (point-max))))
    (apply fun r)))

(defalias 'up-line 'previous-line "For transient keymaps.")
(defalias 'down-line 'next-line "For transient keymaps.")

(advice-add 'up-line   :before 'up-line-wrap)
(advice-add 'down-line :around 'down-line-wrap)
(advice-add 'up-line   :after (lambda (&rest r) (beginning-of-visual-line)))
(advice-add 'down-line :after (lambda (&rest r) (beginning-of-visual-line)))

(defun jump-mark ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
Save point to register before repeated call."
  (interactive)
  (unless (eq this-command last-command)
    (point-to-register ?2)) ; double hit 7 to jump back
  (set-mark-command t))

(defun beg-of-line ()
  "Move cursor to beginning of line."
  (interactive)
  (let ((xp (point)))
    (if visual-line-mode
        (beginning-of-visual-line)
      (if (eq major-mode 'eshell-mode) ; custom eshell bol
          (if (= (line-number-at-pos) (count-lines (point-min) (point-max)))
              (progn
                (beginning-of-line)
                (forward-char 4))
            (beginning-of-line))
        (back-to-indentation)
        (when (eq xp (point))
          (beginning-of-line))))))

(defun beg-of-line-or-block ()
  "Move cursor to beginning of line or previous block.

- When called first time, move cursor to beginning of char in current
  line (if already, move to beginning of line);
- When called again, move cursor backward by jumping over any sequence
  of whitespaces containing 2 blank lines;
- If `visual-line-mode' is on, beginning of line means visual line."
  (interactive)
  (let ((xp (point)))
    (if (or (equal (point) (line-beginning-position))
            (eq last-command this-command)
            (equal "D" (this-command-keys)) ; shift i Engram
            (equal "d" (this-command-keys)) ; repeat
            (equal [backspace] (this-command-keys))
            (unless (display-graphic-p)
              (= 127 (aref (this-command-keys) 0)))) ; DEL repeat
        (when
            (re-search-backward "\n[\t\n ]*\n+" nil 1)
          (skip-chars-backward "\n\t ")
          (forward-char))
      (if visual-line-mode
          (beginning-of-visual-line)
        (if (eq major-mode 'eshell-mode) ; custom eshell bol
            (progn
              (if (= (line-number-at-pos) (count-lines (point-min) (point-max)))
                  (progn
                    (beginning-of-line)
                    (forward-word)
                    (forward-char))
                (beginning-of-line)))
          (back-to-indentation)
          (when (eq xp (point))
            (beginning-of-line)))))))

(defun end-of-lyne ()
  "End of line or visual line."
  (interactive)
  (if visual-line-mode
      (end-of-visual-line)
    (end-of-line)))

(defun end-of-line-or-block ()
  "Move cursor to end of line or next block.
- when called first time, move cursor to end of line;
- when called again, move cursor forward by jumping over any sequence
  of whitespaces containing 2 blank lines;
- if `visual-line-mode' is on, end of line means visual line."
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command)
          (equal "T" (this-command-keys))  ; shift k Engram
          (equal "t" (this-command-keys))  ; repeat
          (equal " " (this-command-keys))) ; SPC repeat
      (re-search-forward "\n[\t\n ]*\n+" nil 1)
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line))))

(defun beg-of-line-or-buffer ()
  "If point is at the end of line, then go to the beginning of line. Else go
to the beginning of buffer, next press to the end of buffer, next press
return to the beginning of buffer. "
  (interactive)
  (if (equal (point) (point-min))
      (unless (eq major-mode 'eshell-mode)
        (goto-char (point-max))
        (forward-line -1))
    (if (and (equal (point) (line-end-position))
             (> (current-column) 0))
        (beg-of-line-or-block)
      (goto-char (point-min)))))

(defun end-of-line-or-buffer ()
  "If point is at the beginning of line, then go to the end of line. Else go
to the end of buffer, next press to the beginning of buffer, next press
return to the end of buffer."
  (interactive)
  (if (eq (count-lines 1 (point)) (- (count-lines (point-min) (point-max)) 1))
      (goto-char (point-min))
    (if (and (equal (point) (line-beginning-position))
             (> (current-column) 0))
        (end-of-line-or-block)
      (goto-char (point-max))
      (unless (eq major-mode 'eshell-mode)
        (forward-line -1)))))

(defvar brackets '("()" "[]" "{}" "<>")
  "A list of strings, each element is a string of 2 chars, the left
bracket and a matching right bracket. Used by `select-text-in-quote'
and others.")

(defconst left-brackets
  (mapcar (lambda (x) (substring x 0 1)) brackets)
  "List of left bracket chars. Each element is a string.")

(defconst right-brackets
  (mapcar (lambda (x) (substring x 1 2)) brackets)
  "List of right bracket chars. Each element is a string.")

(defconst punctuation-regex "[!?\".,`'#$%&*+:;=@^|~]+"
  "A regex string for the purpose of moving cursor to a punctuation.")

(defun backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `forward-punct'"
  (interactive "p")
  (re-search-backward punctuation-regex nil t n))

(defun forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `punctuation-regex' "
  (interactive "p")
  (re-search-forward punctuation-regex nil t n))

(defun backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `left-brackets'."
  (interactive)
  (re-search-backward (regexp-opt left-brackets) nil t))

(defun forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `right-brackets'."
  (interactive)
  (re-search-forward (regexp-opt right-brackets) nil t))

(defun goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `left-brackets'
and `right-brackets'."
  (interactive)
  (if (equal "%" (this-command-keys))
      ;; see equal sign mapping for russian, so because of conflict
      ;; while for Engram it is free and can be remapped:
      (progn
        (setq this-command 'text-scale-increase)
        (text-scale-increase 1))
    (if (nth 3 (syntax-ppss))
        (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
      (cond
       ((eq (char-after) ?\") (forward-sexp))
       ((eq (char-before) ?\") (backward-sexp))
       ((looking-at (regexp-opt left-brackets))
        (forward-sexp))
       ((prog2 (backward-char) (looking-at
                                (regexp-opt right-brackets)) (forward-char))
        (backward-sexp))
       (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING))))))

(defun sort-lines-block-or-region ()
  "Like `sort-lines' but if no region, do the current block."
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
    (sort-lines current-prefix-arg xp1 xp2)))

(defun sort-lines-key-value (beg end)
  "Sort key-values pairs by value."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr nil 'forward-line 'end-of-line
                 (lambda ()
                   (search-forward ":" nil nil 1)
                   (string-to-number (buffer-substring (point) (pos-eol))))))))

(defun narrow-to-region-or-block ()
  "Same as `narrow-to-region', but if no selection, narrow to the current block."
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
    (narrow-to-region xp1 xp2)))

(defalias 'back-word 'backward-word "For transient keymaps.")
(defalias 'forw-word 'forward-word "For transient keymaps.")

(defun activate-region ()
  "Select region. If region active, then exchange point and mark."
  (interactive)
  (if (region-active-p)
      (if (eq (mark) (point))
          (progn
            (set-mark-command t)
            (exchange-point-and-mark))
        (exchange-point-and-mark))
    (set-mark-command nil)))

(defalias 'half-page-backward 'View-scroll-half-page-backward)
(defalias 'half-page-forward 'View-scroll-half-page-forward)


;; editing commands

 (defun del-word ()
  "If next symbol not part of a word, delete the symbol. Otherwise kill
characters forward until encountering the end of the word."
  (interactive)
  (if (looking-at "[[:blank:][:alpha:][:digit:]]+")
      (kill-word 1)
    (delete-char 1)))

 (defun backward-del-word ()
  "If prev symbol not part of a word, delete the symbol. Otherwise kill
characters backward until encountering the end of the word."
  (interactive)
  (if (looking-back "[[:blank:][:alpha:][:digit:]]+" 1)
      (backward-kill-word 1)
    (delete-char -1)))

(defun copy-text-block ()
  "Copy text block to register 1."
  (interactive)
  (if (fboundp 'uncentered-cursor)
      (uncentered-cursor))
  (select-block)
  (sit-for 0.1)
  (copy-to-register ?1 (region-beginning) (region-end))
  (set-mark-command t)
  (set-mark-command t)
  (if (fboundp 'centered-cursor)
      (centered-cursor)))

(defun copy-selection (&rest r)
  "Copy selection."
  (if (region-active-p)
      (copy-region-as-kill (region-beginning) (region-end))))

(defun copy-line-or-selection ()
  "Copy current line or selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer
(respects `narrow-to-region')."
  (interactive)
  (let ((inhibit-field-text-motion nil))
    (if current-prefix-arg
        (copy-region-as-kill (point-min) (point-max))
      (if (region-active-p)
          (copy-region-as-kill (region-beginning) (region-end))
        (if (eq last-command this-command)
            (if (eobp)
                (progn)
              (progn
                (kill-append "\n" nil)
                (kill-append
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))
                 nil)
                (progn
                  (end-of-line)
                  (forward-char))))
          (if (eobp)
              (if (eq (char-before) 10)
                  (progn)
                (progn
                  (copy-region-as-kill
                   (line-beginning-position) (line-end-position))
                  (end-of-line)))
            (progn
              (copy-region-as-kill (line-beginning-position) (line-end-position))
              (end-of-line)
              (forward-char))))))))

(defun copy-shell-input (fun &rest r)
  "Copy shell input. Around advice for `copy-line-or-selection'. Prompt is └ $ .
Copy selected region if selected."
  (cond
   ((region-active-p)
    (copy-region-as-kill (region-beginning) (region-end)))
   ((and (eq major-mode 'vterm-mode)
         (string-equal "└ $ " (buffer-substring-no-properties
                               (line-beginning-position)
                               (+ 4 (line-beginning-position)))))
    (copy-region-as-kill (+ 6 (line-beginning-position)) (line-end-position)))
   ((and (eq major-mode 'eshell-mode)
         (string-equal "└ $ " (buffer-substring-no-properties
                               (line-beginning-position)
                               (+ 4 (line-beginning-position)))))
    (copy-region-as-kill (+ 4 (line-beginning-position)) (line-end-position)))
   (t (apply fun r))))

(advice-add 'copy-line-or-selection :around 'copy-shell-input)

(defun cut-line-or-selection ()
  "Cut current line or selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end) t)
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(defun copy-all ()
  "Copy buffer content to `kill-ring'. Respects `narrow-to-region'."
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copy"))

(defun paste-or-paste-prev ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.
When `universal-argument' is called first with a number arg,
paste that many times."
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (progn
          (dotimes (_ (prefix-numeric-value current-prefix-arg))
            (yank)))
      (if (eq real-last-command this-command)
          (yank-pop 1)
        (yank)))))

(defconst show-kill-ring-separator (concat "\n\n" (make-string 77 95) "\n\n")
  "A line divider for `show-kill-ring'.")

(defun show-kill-ring ()
  "Insert all `kill-ring' content in a new buffer named *copy stack*."
  (interactive)
  (let ((xbuf (generate-new-buffer "*copy stack*"))
        (inhibit-read-only t))
    (progn
      (switch-to-buffer xbuf)
      (funcall 'fundamental-mode)
      (mapc
       (lambda (x)
         (insert x show-kill-ring-separator ))
       kill-ring))
    (goto-char (point-min))))

(defun cut-bracket-text ()
  "Delete the matching brackets/quotes to the left of cursor,
including the inner text.
This command assumes the left of cursor is a right bracket, and there
is a matching one before it.
What char is considered bracket or quote is determined by current syntax table."
  (forward-sexp -1)
  (mark-sexp)
  (kill-region (region-beginning) (region-end)))

(defun cut-bracket-pair ()
  "Delete the matching brackets/quotes to the left of cursor.
After call, mark is set at the matching bracket position, so you can
`exchange-point-and-mark' to select it.
This command assumes the left of point is a right bracket, and there
is a matching one before it.
What char is considered bracket or quote is determined by current syntax table."
  (interactive)
  (let ((xp0 (point)) xp1)
    (forward-sexp -1)
    (setq xp1 (point))
    (goto-char xp0)
    (delete-char -1)
    (goto-char xp1)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- xp0 2))))

(defun cut-forward-bracket-pairs (&optional DeleteInnerTextQ)
  "Delete the matching brackets/quotes to the right of cursor.
If DeleteInnerTextQ is true, also delete the inner text.

After the command, mark is set at the left matching bracket position,
so you can `exchange-point-and-mark' to select it.

This command assumes the char to the right of point is a left bracket
or quote, and have a matching one after.

What char is considered bracket or quote is determined by current syntax table."
  (if DeleteInnerTextQ
      (progn
        (mark-sexp)
        (kill-region (region-beginning) (region-end)))
    (let ((xpt (point)))
      (forward-sexp)
      (delete-char -1)
      (push-mark (point) t)
      (goto-char xpt)
      (delete-char 1))))

(defun cut-bracket ()
  "Delete 1 character or cut quote/bracket pair and inner text. If
the char to the left of cursor is a matching pair, cut it alone
with inner text.

What char is considered bracket or quote is determined by current
syntax table.

If `universal-argument' is called first, do not delete inner text."
  (if (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end))
    (cond
     ((looking-back "\\s)" 1)
      (if current-prefix-arg
          (cut-bracket-pair)
        (cut-bracket-text)))
     ((looking-back "\\s(" 1)
      ;; (message "Left of cursor is opening bracket")
      (let (xpOpenBracketLeft
            (xpOpenBracketRight (point)) xisComment)
        (backward-char)
        (setq xpOpenBracketLeft (point))
        (goto-char xpOpenBracketRight)
        (forward-char)
        (setq xisComment (nth 4 (syntax-ppss)))
        (if xisComment
            (progn
              ;; (message "Cursor is in comment")
              (goto-char xpOpenBracketLeft)
              (if (forward-comment 1)
                  (kill-region (point) xpOpenBracketLeft)
                (message "Error hSnRp: parsing comment failed")))
          (progn
            ;; (message "Right 1 char of cursor is not in comment")
            (goto-char xpOpenBracketLeft)
            (forward-sexp)
            (if current-prefix-arg
                (cut-bracket-pair)
              (cut-bracket-text))))))
     ((looking-back "\\s\"" 1)
      (if (nth 3 (syntax-ppss))
          (progn
            (backward-char)
            (cut-forward-bracket-pairs (not current-prefix-arg)))
        (if current-prefix-arg
            (cut-bracket-pair)
          (cut-bracket-text))))
     (t
      (delete-char -1)))))

(defun delete-backward ()
  "Try cut bracket. If error, then delete char."
  (interactive)
  (let (ok)
    (unwind-protect
        (progn
          (cut-bracket)
          (setq ok t))
      (unless ok
        (if (looking-back "\\s)" 1)
            (delete-char -1)
          (delete-char 1))))))

(defun change-bracket-pairs (FromChars ToChars)
  "Change bracket pairs to another type or none.
For example, change all parenthesis () to square brackets [].
Works on current block or selection.

In Lisp code, FromChars is a string with at least 2 spaces.
E.g. \"( paren )\", \"[[ double bracket ]]\" etc. where the chars
before first space is the left bracket, and char after the last space
is the right bracket
(the middle is for convenience for user to type the char name in prompt).
ToChars is similar, with a special value of \" none \", replace by empty string."
  (interactive
   (let ((xbrackets
          '("\" double quote \""
            "' single quote '"
            "( paren )"
            "{ brace }"
            "[ square ]"
            "< greater >"
            "` backtick `"
            " none " )))
     (list
      (completing-read "Replace this:" xbrackets)
      (completing-read "To:" xbrackets))))
  (let (xp1 xp2 xleft xright xtoL xtoR
            (xss1 (split-string FromChars " "))
            (xss2 (split-string ToChars " ")))
    (let ((xbds (get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
    (setq xleft (car xss1) xright (car (last xss1)))
    (setq xtoL (car xss2) xtoR (car (last xss2)))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (let ((case-fold-search nil))
          (if (string-equal xleft xright)
              (let ((xx (regexp-quote xleft)))
                (goto-char (point-min))
                (while
                    (re-search-forward
                     (format "%s\\([^%s]+?\\)%s" xx xx xx)
                     nil t)
                  (overlay-put (make-overlay
                                (match-beginning 0) (match-end 0)) 'face 'highlight)
                  (replace-match (concat xtoL "\\1" xtoR) t)))
            (progn
              (progn
                (goto-char (point-min))
                (while (search-forward xleft nil t)
                  (overlay-put (make-overlay
                                (match-beginning 0) (match-end 0)) 'face 'highlight)
                  (replace-match xtoL t t)))
              (progn
                (goto-char (point-min))
                (while (search-forward xright nil t)
                  (overlay-put (make-overlay
                                (match-beginning 0) (match-end 0)) 'face 'highlight)
                  (replace-match xtoR t t))))))))))

(defun string-is-capitalized (s)
  "Check if S capitalized."
  (let ((case-fold-search nil))
    (equal (upcase s) s)))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Cycle in this order: Init Caps, ALL CAPS, all lower. Calculates initial state."
  (interactive)
  (let ((deactivate-mark nil) xp1 xp2)
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq xp1 (point))
        (if (string-is-capitalized (buffer-substring xp1 (1+ xp1)))
            (put this-command 'state 1))
        (skip-chars-forward "[:alpha:]")
        (setq xp2 (point))
        (if (string-is-capitalized (buffer-substring xp1 xp2))
            (put this-command 'state 2))))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region xp1 xp2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region xp1 xp2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region xp1 xp2)
      (put this-command 'state 0)))))

(defun toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor."
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (right-char)))

(defun upcase-sentence ()
  "Upcase first letters of sentences of current block or selection."
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        ;; after period or question mark or exclamation
        (goto-char (point-min))
        (while (re-search-forward "\\(\\.\\|\\?\\|!\\)[ \n]+ *\\([a-zа-я]\\)" nil 1)
          (upcase-region (match-beginning 2) (match-end 2))
          (overlay-put (make-overlay
                        (match-beginning 2) (match-end 2)) 'face 'highlight))
        ;; after a blank line, after a bullet, or beginning of buffer
        (goto-char (point-min))
        (while (re-search-forward "\\(\\`\\|• \\|\n\n\\)\\([a-zа-я]\\)" nil 1)
          (upcase-region (match-beginning 2) (match-end 2))
          (overlay-put (make-overlay
                        (match-beginning 2) (match-end 2)) 'face 'highlight))
        ;; for HTML. first letter after tag
        (when (eq major-mode 'html-mode)
          (goto-char (point-min))
          (while
              (re-search-forward "\\(<title>[ \n]?\\|<h[1-6]>[ \n]?\\|<p>[ \n]?\\|<li>[ \n]?\\|<dd>[ \n]?\\|<td>[ \n]?\\|<br ?/?>[ \n]?\\|<figcaption>[ \n]?\\)\\([a-zа-я]\\)" nil 1)
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay
                          (match-beginning 2) (match-end 2)) 'face 'highlight))))
      (goto-char (point-max)))
    (skip-chars-forward " \n\t")))

(defun title-case-region-or-line (&optional Begin End)
  "Title case text between nearest brackets, or current line or selection.

Capitalize first letter of each word, except words like {to, of, the,
a, in, or, and, …}. If a word already contains cap letters such as
HTTP, URL, they are left as is.

When called in a Elisp program, Begin End are region boundaries."
  (interactive)
  (let* ((xskipChars "^\"<>(){}[]")
         (xp0 (point))
         (xp1 (if Begin
                  Begin
                (if (region-active-p)
                    (region-beginning)
                  (skip-chars-backward xskipChars
                                       (line-beginning-position))
                  (point))))
         (xp2 (if End
                  End
                (if (region-active-p)
                    (region-end)
                  (goto-char xp0)
                  (skip-chars-forward xskipChars
                                      (line-end-position)) (point))))
         (xstrPairs [
                     [" A " " a "]
                     [" An " " an "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]

                     [" От " " от "]
                     [" Для " " для "]
                     [" И " " и "]
                     [" К " " к "]
                     [" С " " с "]
                     [" По " " по "]
                     [" В " " в "]
                     [" На " " на "]
                     [" Из " " из "]
                     [" Или " " или "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (xx)
             (goto-char (point-min))
             (while
                 (search-forward (aref xx 0) nil t)
               (replace-match (aref xx 1) t t)))
           xstrPairs))))))

(defun add-space-after-comma ()
  "Add a space after comma of current block or selection.
and highlight changes made."
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (goto-char (point-min))
      (while
          (re-search-forward ",\\b" nil t)
        (replace-match ", ")
        (overlay-put
         (make-overlay
          (match-beginning 0)
          (match-end 0)) 'face 'highlight)))))

(defun delete-blank-lines ()
  "Delete all newline around cursor."
  (interactive)
  (let (xp3 xp4)
    (skip-chars-backward "\n")
    (setq xp3 (point))
    (skip-chars-forward "\n")
    (setq xp4 (point))
    (delete-region xp3 xp4)))

(defun delete-spaces ()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor."
  (interactive)
  (let (p1 p2)
    (skip-chars-forward " \t　")
    (setq p2 (point))
    (skip-chars-backward " \t　")
    (setq p1 (point))
    (delete-region p1 p2)))

(defun shrink-whitespaces ()
  "Remove whitespaces around cursor.
Shrink neighboring spaces, then newlines, then spaces again, leaving
one space or newline at each step, till no more white space."
  (interactive)
  (let* ((xeol-count 0)
         (xp0 (point))
         xp1 ; whitespace begin
         xp2 ; whitespace end
         (xcharBefore (char-before))
         (xcharAfter (char-after))
         (xspace-neighbor-p (or (eq xcharBefore 32)
                                (eq xcharBefore 9)
                                (eq xcharAfter 32)
                                (eq xcharAfter 9))))
    (skip-chars-backward " \n\t　")
    (setq xp1 (point))
    (goto-char xp0)
    (skip-chars-forward " \n\t　")
    (setq xp2 (point))
    (goto-char xp1)
    (while (search-forward "\n" xp2 t)
      (setq xeol-count (1+ xeol-count)))
    (goto-char xp0)
    (cond
     ((eq xeol-count 0)
      (if (> (- xp2 xp1) 1)
          (progn
            (delete-horizontal-space) (insert " "))
        (progn (delete-horizontal-space))))
     ((eq xeol-count 1)
      (if xspace-neighbor-p
          (delete-spaces)
        (progn (delete-blank-lines) (insert " "))))
     ((eq xeol-count 2)
      (if xspace-neighbor-p
          (delete-spaces)
        (delete-blank-lines)))
     ((> xeol-count 2)
      (if xspace-neighbor-p
          (delete-spaces)
        (progn
          (goto-char xp2)
          (search-backward "\n")
          (delete-region xp1 (point))
          (insert "\n"))))
     (t (message "Nothing done. Logic error 40873. Should not reach here")))))

(defun fill-or-unfill ()
  "Reformat current block or selection to short/long line.
First call will break into multiple short lines. Repeated call toggles
between short and long lines.
This commands calls `fill-region' to do its work. Set `fill-column'
for short line length."
  (interactive)
  (let ((xisLongline
         (if (eq last-command this-command) (get this-command 'longline-p) t))
        (deactivate-mark nil)
        xp1 xp2)
    (let ((xbds (get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
    (if xisLongline
        (fill-region xp1 xp2)
      (let ((fill-column 99999))
        (fill-region xp1 xp2)))
    (put this-command 'longline-p (not xisLongline))))

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph)))

(defun unfill-region (Begin End)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region Begin End)))

(defun change-newline-chars-to-one (Begin End)
  "Replace newline char sequence by just one."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (re-search-forward "\n\n+" nil 1) (replace-match "\n")))))

(defun reformat-whitespaces-to-one-space (Begin End)
  "Replace whitespaces by one space."
  (interactive "r")
  (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (search-forward "\n" nil 1) (replace-match " "))
      (goto-char (point-min))
      (while (search-forward "\t" nil 1) (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward " +" nil 1) (replace-match " "))
      (goto-char (point-max))))

(defun reformat-to-multi-lines (&optional Begin End)
  "Replace spaces by a newline at ~70 chars, on current block or selection.
If `universal-argument' is called first, ask user for max width."
  (interactive)
  (let (xp1 xp2 (xminlen fill-column))
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (let ((xbds (get-bounds-of-block-or-region)))
        (setq xp1 (car xbds) xp2 (cdr xbds))))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil 1)
          (when (> (- (point) (line-beginning-position)) xminlen)
            (replace-match "\n")))))))

(defun reformat-lines ()
  "Reformat current block or selection into short lines or 1 long line.
When called for the first time, change to one line. Second call change
it to multi-lines. Repeated call toggles.
If `universal-argument' is called first, ask user to type max length
of line. By default, it is 66.

Note: this command is different from emacs `fill-region' or `fill-paragraph'.
This command never adds or delete non-whitespace chars. It only
exchange whitespace sequence."
  (interactive)
  (let (xisLong xp1 xp2)
    (setq xisLong (if (eq last-command this-command)
                      (get this-command 'is-long-p)))
    (let ((xbds (get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
    (if current-prefix-arg
        (reformat-to-multi-lines xp1 xp2)
      (if xisLong
          (reformat-to-multi-lines xp1 xp2)
        (progn
          (reformat-whitespaces-to-one-space xp1 xp2))))
    (put this-command 'is-long-p (not xisLong))))

(defun reformat-to-sentence-lines ()
  "Reformat current block or selection into multiple lines by ending period.
Move cursor to the beginning of next text block."
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (goto-char (point-min))
      (while (re-search-forward "\\([A-Za-zА-Яа-я0-9]+\\)[ \t]*\n[ \t]*\\([A-Za-zА-Яа-я0-9]+\\)" nil t)
        (replace-match "\\1 \\2"))
      (goto-char (point-min))
      (while (re-search-forward "\\([,]\\)[ \t]*\n[ \t]*\\([A-Za-zА-Яа-я0-9]+\\)" nil t)
        (replace-match "\\1 \\2"))
      (goto-char (point-min))
      (while (re-search-forward "  +" nil t) (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "\\([.?!]\\) +\\([(0-9A-Za-zА-Яа-я]+\\)" nil t) (replace-match "\\1\n\\2"))
      (goto-char (point-max))
      (while (eq (char-before) 32) (delete-char -1))))
  (re-search-forward "\n+" nil 1))

(defun space-to-newline ()
  "Replace space sequence to a newline char in current block or selection."
  (interactive)
  (let* ((xbds (get-bounds-of-block-or-region))
         (xp1 (car xbds))
         (xp2 (cdr xbds)))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (goto-char (point-min))
      (while (re-search-forward " +" nil t)
        (replace-match "\n")))))

(defun slash-to-backslash (&optional Begin End)
  "Replace slash by backslash on current line or region."
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "/" nil t)
          (replace-match "\\\\"))))))

(defun backslash-to-slash (&optional Begin End)
  "Replace backslash by slash on current line or region."
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\" nil t)
          (replace-match "/"))))))

(defun double-backslash-to-single (&optional Begin End)
  "Replace double backslash by single backslash on current line or region."
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\\\"  nil t)
          (replace-match "\\\\"))))))

(defun slash-to-double-backslash (&optional Begin End)
  "Replace slash by double backslash on current line or region."
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "/" nil t)
          (replace-match "\\\\\\\\"))))))

(defun double-backslash-to-slash (&optional Begin End)
  "Replace double backslash by slash on current line or region."
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\\\" nil t)
          (replace-match "/"))))))

(defun toggle-comment ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line."
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let ((xlbp (line-beginning-position))
          (xlep (line-end-position)))
      (if (eq xlbp xlep)
          (progn
            (comment-dwim nil))
        (if (eq (point) xlep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region xlbp xlep)
            (forward-line)))))))

(defun quote-lines (Begin End QuoteL QuoteR Sep)
  "Add quotes/brackets and separator (comma) to lines.
Act on current block or selection.
In lisp code, QuoteL QuoteR Sep are strings."
  (interactive
   (let* ((xbds (get-bounds-of-block-or-region))
         (xp1 (car xbds))
         (xp2 (cdr xbds))
         (xbrackets
          '(
            "\"double quote\""
            "'single quote'"
            "(paren)"
            "{brace}"
            "[square]"
            "`markdown`"
            "none"
            "other"
            )) xbktChoice xsep xsepChoice xquoteL xquoteR)
     (setq xbktChoice (completing-read "Quote to use:" xbrackets))
     (setq xsepChoice (completing-read "Line separator:" '("," ";" "none" "other")))
     (cond
      ((string-equal xbktChoice "none")
       (setq xquoteL "" xquoteR ""))
      ((string-equal xbktChoice "other")
       (let ((xx (read-string "Enter 2 chars, for begin/end quote:")))
         (setq xquoteL (substring-no-properties xx 0 1)
               xquoteR (substring-no-properties xx 1 2))))
      (t (setq xquoteL (substring-no-properties xbktChoice 0 1)
               xquoteR (substring-no-properties xbktChoice -1))))
     (setq xsep
           (cond
            ((string-equal xsepChoice "none") "")
            ((string-equal xsepChoice "other") (read-string "Enter separator:"))
            (t xsepChoice)))
     (list xp1 xp2 xquoteL xquoteR xsep)))
  (let ((xp1 Begin) (xp2 End) (xquoteL QuoteL) (xquoteR QuoteR) (xsep Sep))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (goto-char (point-min))
        (catch 'EndReached
          (while t
            (skip-chars-forward "\t ")
            (insert xquoteL)
            (end-of-line)
            (insert xquoteR xsep)
            (if (eq (point) (point-max))
                (throw 'EndReached t)
              (forward-char))))))))

(defun escape-quotes (Begin End)
  "Add slash before double quote in current line or selection.
Double quote is codepoint 34.
See also: `unescape-quotes'."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region Begin End)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" t t)))))

(defun unescape-quotes (Begin End)
  "Replace  「\\\"」 by 「\"」 in current line or selection.
See also: `escape-quotes'."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" t t)))))

(defun cycle-hyphen-lowline-space (&optional Begin End)
  "Cycle hyphen/lowline/space chars in selection or inside
quote/bracket or line, in that order.

The region to work on is by this order:
 1. If there is a selection, use that;
 2. If cursor is string quote or any type of bracket, and is within
 current line, work on that region;
 3. Else, work on current line."
  (interactive)
  ;; This function sets a property 'state.
  ;; Possible values are 0 to length of xcharArray.
  (let* (xp1
         xp2
         (xcharArray ["-" "_" " "])
         (xn (length xcharArray))
         (xregionWasActive-p (region-active-p))
         (xnowState (if (eq last-command this-command)
                        (get 'cycle-hyphen-lowline-space 'state)
                      0))
         (xchangeTo (elt xcharArray xnowState)))
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (let ((xskipChars "^\"<>(){}[]"))
          (skip-chars-backward xskipChars (line-beginning-position))
          (setq xp1 (point))
          (skip-chars-forward xskipChars (line-end-position))
          (setq xp2 (point))
          (push-mark xp1))))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (goto-char (point-min))
        (while (re-search-forward (elt xcharArray (% (+ xnowState 2) xn))
                                  (point-max) 1)
          (replace-match xchangeTo t t))))
    (when (or (string-equal xchangeTo " ") xregionWasActive-p)
      (goto-char xp2)
      (push-mark xp1)
      (setq deactivate-mark nil))
    (put 'cycle-hyphen-lowline-space 'state (% (+ xnowState 1) xn))))

(defun copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
If in dired, copy the current or marked files.
If a buffer is not file and not dired, copy value of `default-directory'."
  (interactive "P")
  (let ((xfpath
         (if (and (string-equal major-mode 'dired-mode)
                  (not DirPathOnlyQ))
             (progn
               (let ((xresult (mapconcat #'identity
                                         (dired-get-marked-files) "\n")))
                 (if (equal (length xresult) 0)
                     (progn default-directory )
                   (progn xresult))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (file-name-directory xfpath)
       (progn
         (message "%s" xfpath)
         xfpath )))))

(defun cut-text-block ()
  "Cut text block plus blank lines or selection."
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (progn
        (if (re-search-backward "\n[ \t]*\n+" nil 1)
            (setq xp1 (goto-char (match-end 0)))
          (setq xp1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil 1)
            (setq xp2 (match-end 0))
          (setq xp2 (point-max)))))
    (kill-region xp1 xp2)))

(defun clear-register-1 ()
  "Clear register 1.
See also: `paste-from-register-1', `copy-to-register'."
  (interactive)
  (copy-to-register ?1 (point-min) (point-min))
  (message "Clear register 1"))

(defun copy-to-register-1 ()
  "Copy current line or selection to register 1.
See also: `paste-from-register-1', `copy-to-register'."
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (copy-to-register ?1 xp1 xp2)
    (message "Copy register 1")))

(defun append-to-register-1 ()
  "Append current line or selection to register 1.
When no selection, append current line, with newline char.
See also: `paste-from-register-1', `copy-to-register'."
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
         (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (append-to-register ?1 xp1 xp2)
    (with-temp-buffer (insert "\n")
                      (append-to-register ?1 (point-min) (point-max)))
    (message "Append register 1")))

(defun paste-from-register-1 ()
  "Paste text from register 1.
See also: `copy-to-register-1', `insert-register'."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun isearch-yank-register-1 ()
  "Pull string from register-1 into search string."
  (interactive)
  (unless isearch-mode (isearch-mode t))
  (with-temp-buffer
    (insert-register ?1 t)
    (isearch-yank-string
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun copy-rectangle-to-kill-ring (Begin End)
  "Copy region as column (rectangle region) to `kill-ring'.
See also: `kill-rectangle', `copy-to-register'."
  (interactive "r")
  (require 'rect)
  (kill-new (mapconcat #'identity (extract-rectangle Begin End) "\n")))

(defun beg-of-buffer (fun &rest r)
  "Go to the beginning of buffer if no selection."
  (save-excursion
    (unless (use-region-p)
      (goto-char (point-min)))
    (apply fun r)))

(advice-add 'query-replace :around #'beg-of-buffer)
(advice-add 'query-replace-regexp :around #'beg-of-buffer)


;; insertion commands

(defun insert-date ()
  "Insert current date time.
Insert date in this format: yyyy-mm-dd.
If `universal-argument' is called first, prompt for a format to use.
If there is selection, delete it first."
  (interactive)
  (let ((xstyle
         (if current-prefix-arg
             (completing-read
              "Style:"
              '("ISO date • 2018-04-12"
                "ISO full • 2018-04-12T22:46:11-07:00"
                "ISO space • 2018-04-12 22:46:11-07:00"
                "org mode <2018-04-12 Thu>"
                "all digits • 20180412224611"
                "date and digits • 2018-04-12_224611"
                "weekday • 2018-04-12 Thursday"))
           "org mode <2018-04-12 Thu>")))
    (when (region-active-p) (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((string-match "^ISO date" xstyle) (format-time-string "%Y-%m-%d"))
      ((string-match "^org mode" xstyle) (concat " <" (format-time-string "%Y-%m-%d %a") ">"))
      ((string-match "^all digits" xstyle) (format-time-string "%Y%m%d%H%M%S"))
      ((string-match "^date and digits" xstyle) (format-time-string "%Y-%m-%d_%H%M%S"))
      ((string-match "^ISO full" xstyle)
       (concat
        (format-time-string "%Y-%m-%dT%T")
        (funcall (lambda (xx) (format "%s:%s" (substring xx 0 3) (substring xx 3 5))) (format-time-string "%z"))))
      ((string-match "^ISO space" xstyle)
       (concat
        (format-time-string "%Y-%m-%d %T")
        (funcall (lambda (xx) (format "%s:%s" (substring xx 0 3) (substring xx 3 5))) (format-time-string "%z"))))
      ((string-match "^weekday" xstyle) (format-time-string "%Y-%m-%d %A"))
      (t (format-time-string "%Y-%m-%d"))))))

(defun insert-bracket-pair (LBracket RBracket &optional WrapMethod)
  "Insert brackets around selection, word, at point, and maybe move cursor
 in between.
LBracket and RBracket are strings. WrapMethod must be either `line' or
`block'. `block' means between empty lines.

- If there is a an active region, add brackets around region;
- if WrapMethod is `line', wrap around line;
- if WrapMethod is `block', wrap around block;
- if cursor is at beginning of line and its not empty line and contain
  at least 1 space, wrap around the line;
- if cursor is at end of a word or buffer, one of the following will happen:
  xyz▮ → xyz(▮)
  xyz▮ → (xyz▮)
- if in one of the lisp modes:
  wrap brackets around word if any. e.g. xy▮z → (xyz▮). Or just (▮)."
  (if (region-active-p)
      (progn
        (let ((xp1 (region-beginning)) (xp2 (region-end)))
          (goto-char xp2) (insert RBracket)
          (goto-char xp1) (insert LBracket)
          (goto-char (+ xp2 2))))
    (let (xp1 xp2)
      (cond
       ((eq WrapMethod 'line)
        (setq xp1 (line-beginning-position) xp2 (line-end-position))
        (goto-char xp2)
        (insert RBracket)
        (goto-char xp1)
        (insert LBracket)
        (goto-char (+ xp2 (length LBracket))))
       ((eq WrapMethod 'block)
        (save-excursion
          (let ((xbds (get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
          (goto-char xp2)
          (insert RBracket)
          (goto-char xp1)
          (insert LBracket)
          (goto-char (+ xp2 (length LBracket)))))
       ( ;  Do line. Line must contain space
        (and
         (eq (point) (line-beginning-position))
         ;; (string-match " " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (not (eq (line-beginning-position) (line-end-position))))
        (insert LBracket)
        (end-of-line)
        (insert  RBracket))
       ((and
         (or ; Cursor is at end of word or buffer i.e. xyz▮
          (looking-at "[^-_[:alnum:]]")
          (eq (point) (point-max)))
         (not (string-equal major-mode "emacs-lisp-mode")))
        (progn
          (setq xp1 (point) xp2 (point))
          (insert LBracket RBracket)
          (search-backward RBracket)))
       (t (progn
            ;; Wrap around “word”. Basically, want all alphanumeric,
            ;; plus hyphen and underscore, but don't want space or
            ;; punctuations. Also want chinese chars.
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq xp1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq xp2 (point))
            (goto-char xp2)
            (insert RBracket)
            (goto-char xp1)
            (insert LBracket)
            (goto-char (+ xp2 (length LBracket)))))))))

(defun insert-paren ()                 (interactive) (insert-bracket-pair "(" ")"))
(defun insert-square-bracket ()        (interactive) (insert-bracket-pair "[" "]"))
(defun insert-brace ()                 (interactive) (insert-bracket-pair "{" "}"))
(defun insert-backtick-quote ()        (interactive) (insert-bracket-pair "`" "`"))
(defun insert-backtick-triple-quote () (interactive) (insert-bracket-pair "```\n" "\n```"))
(defun insert-double-curly-quote ()    (interactive) (insert-bracket-pair "“" "”"))
(defun insert-double-angle-quote ()    (interactive) (insert-bracket-pair "«" "»"))
(defun insert-ascii-double-quote ()    (interactive) (insert-bracket-pair "\"" "\""))
(defun insert-ascii-single-quote ()    (interactive) (insert-bracket-pair "'" "'"))
(defun insert-emacs-quote ()           (interactive) (insert-bracket-pair "`" "'"))

(defun insert-space-before ()
  "Insert space before cursor."
  (interactive)
  (insert " "))

(defun insert-formfeed ()
  "Insert a form feed char (codepoint 12)."
  (interactive)
  (insert "\n\u000c\n"))

(defun insert-column-a-z ()
  "Insert letters A to Z vertically, similar to `rectangle-number-lines'.
The commpand will prompt for a start char, and number of chars to insert.
The start char can be any char in Unicode."
  (interactive)
  (let ((xstartChar (string-to-char (read-string "Start char: " "a")))
        (xhowmany (string-to-number (read-string "How many: " "26")))
        (xcolpos (- (point) (line-beginning-position))))
    (dotimes (xi xhowmany)
      (progn
        (insert-char (+ xi xstartChar))
        (forward-line)
        (beginning-of-line)
        (forward-char xcolpos)))))


;; text selection

(defvar select-block-defer-timer nil "Defer `select-block' timer.")

(defun select-block-raw ()
  "Select the current/next block plus 1 blank line.
If region is active, extend selection downward by block."
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
    (progn
      (push-mark (point) t nil)
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil 1)
        (goto-char (match-end 0)))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil 1))))

(defun select-block ()
  "Defer `select-block-raw'."
  (interactive)
  (setq select-block-defer-timer
        (run-with-timer
         (/ keyamp-double-press-threshold 1000.0) nil 'select-block-raw)))

(advice-add 'up-line :before
            (lambda () "cancel `select-block-defer-timer'"
              (if (and (eq last-command 'select-block)
                       (timerp select-block-defer-timer))
                  (cancel-timer select-block-defer-timer))))

(defun select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line."
  (interactive)
  (when (bolp)
    (cond
     ((eq major-mode 'ibuffer-mode) (ibuffer-backward-filter-group))
     ((eq major-mode 'gnus-group-mode) (gnus-topic-goto-previous-topic 1))
     (t (beg-of-line-or-block))))
  (unless (bolp)
    (push-mark (point) t nil)
    (if (region-active-p)
        (if visual-line-mode
            (let ((xp1 (point)))
              (end-of-visual-line 1)
              (when (eq xp1 (point))
                (end-of-visual-line 2)))
          (forward-line 1)
          (end-of-line))
      (if visual-line-mode
          (progn
            (beginning-of-visual-line)
            (push-mark (point) t t)
            (end-of-visual-line))
        (push-mark (line-beginning-position) t t)
        (end-of-line)))))

(defvar extend-selection-defer-timer nil "Defer `extend-selection' timer.")
(defvar cur-highlight-regexp nil "Current highlight regexp.")

(defun extend-selection-raw ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

When there is no selection,
- if cursor is on a any type of bracket (including parenthesis,
  quotation mark), select whole bracketed thing including bracket
- else, select current word.

When there is a selection, the selection extension behavior is still
experimental. But when cursor is on a any type of bracket
(parenthesis, quote), it extends selection to outer bracket."
  (push-mark (point) t nil)
  (if (region-active-p)
      (progn
        (let ((xrb (region-beginning)) (xre (region-end)))
          (goto-char xrb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  ;; (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (push-mark (line-beginning-position) t t))
              (progn
                ;; (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq xrb (line-beginning-position))
            (progn
              (goto-char xrb)
              (let ((xfirstLineEndPos (line-end-position)))
                (cond
                 ((eq xre xfirstLineEndPos)
                  (progn
                    ;; (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< xre xfirstLineEndPos)
                  (progn
                    ;; (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> xre xfirstLineEndPos)
                  (progn
                    ;; (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char xre)
                    (if (eq (point) (line-end-position))
                        (progn
                          ;; (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        ;; (message "multiple lines but end is not eol. make it so" )
                        (goto-char xre)
                        (end-of-line)))))
                 (t (error "%s: logic error 42946" real-this-command))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              ;; (message "less than 1 line" )
              (end-of-line) ; select current line
              (push-mark (line-beginning-position) t t)))
           (t
            ;; (message "last resort" )
            nil))))
    (progn
      (cond
       ((= (point) (point-min))
        (push-mark (point) t t)
        (skip-chars-forward "-_a-zа-яA-ZА-Я0-9"))
       ((looking-at "\\s(")
        ;; (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-back ")" (- (point) 1))
        ;; (message "right bracket")
        (goto-matching-bracket)
        (mark-sexp))
       ((looking-at "\\s)")
        ;; (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        ;; (message "string quote")
        (mark-sexp)) ; string quote
       ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
       ;;  (message "beginning of line and not empty")
       ;;  (end-of-line)
       ;;  (push-mark (line-beginning-position) t t))
       (
        ;; (prog2 (backward-char) (looking-at "[-_a-zA-Z0-9]") (forward-char))
        (looking-back "[-_a-zа-яA-ZА-Я0-9]" (max (- (point) 1) (point-min)))
        ;; (message "left is word or symbol")
        (skip-chars-backward "-_a-zа-яA-ZА-Я0-9")
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (push-mark)
        (skip-chars-forward "-_a-zа-яA-ZА-Я0-9")
        (setq mark-active t)
        ;; (exchange-point-and-mark)
        )
       ((and (looking-at "[:blank:]")
             (prog2 (backward-char) (looking-at "[:blank:]") (forward-char)))
        ;; (message "left and right both space" )
        (skip-chars-backward "[:blank:]") (push-mark (point) t t)
        (skip-chars-forward "[:blank:]"))
       ((and (looking-at "\n")
             (eq (char-before) 10))
        ;; (message "left and right both newline")
        (skip-chars-forward "\n")
        (push-mark (point) t t)
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next block
       (t
        ;; (message "just mark sexp" )
        (mark-sexp)
        (exchange-point-and-mark))
       ;;
       )))
  (when (and (region-active-p)
             (< 0 (length (buffer-substring-no-properties
                           (region-beginning) (region-end)))))
    (setq cur-highlight-regexp
          (buffer-substring-no-properties
           (region-beginning) (region-end)))
    (highlight-regexp cur-highlight-regexp 'highlight-extend-selection)))

(defun extend-selection ()
  "Defer `extend-selection-raw'."
  (interactive)
  (setq extend-selection-defer-timer
        (run-with-timer
         (/ keyamp-double-press-threshold 1000.0) nil 'extend-selection-raw)))

(advice-add 'down-line :before
            (lambda () "cancel `extend-selection-defer-timer'"
              (if (and (eq last-command 'extend-selection)
                       (timerp extend-selection-defer-timer))
                  (cancel-timer extend-selection-defer-timer))))

(add-hook 'deactivate-mark-hook
          (lambda () (unhighlight-regexp cur-highlight-regexp)))

(defvar pair-brackets '(("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">")))

(defun select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \" ` ' and anything in `brackets'.
Limit selection by current line.
This command ignores nesting. For example, if text is
    (a(b)c▮)
the selected char is “c”, not “a(b)c”."
  (interactive)
  (when (bolp)
    (cond
     ((eq major-mode 'ibuffer-mode) (ibuffer-forward-filter-group))
     ((eq major-mode 'gnus-group-mode) (gnus-topic-goto-next-topic-line))
     (t (end-of-line)
        (end-of-line-or-block))))
  (unless (bolp)
    (push-mark (point) t nil)
    (let ((xskipChars (concat "^\"`'" (mapconcat #'identity brackets ""))))
      (skip-chars-backward xskipChars)
      (setq xskipChar (buffer-substring-no-properties (- (point) 1) (point)))
      (if (member xskipChar left-brackets)
          (setq xskipChar (cdr (assoc xskipChar pair-brackets))))
      (push-mark (point) t t)
      (skip-chars-forward (concat "^" xskipChar)))))

(defun deactivate-mark-if-active (&rest r)
  "Deactivate mark if region active."
  (if (region-active-p) (deactivate-mark)))


;; misc

(defun user-buffer-p ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it is not considered a user
buffer.
This function is used by buffer switching command and close buffer
command, so that next buffer shown is a user buffer."
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((string-equal major-mode "eww-mode") nil)
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "help-mode") nil)
   ((string-equal major-mode "doc-view-mode") nil)
   ((string-equal major-mode "diary-mode") nil)
   ((string-equal (buffer-name) "tetris-scores") nil)
   ((string-equal (buffer-name) "snake-scores") nil)
   ((string-equal buffer-file-truename org-agenda-file-1) nil)
   ((string-equal buffer-file-truename org-agenda-file-2) nil)
   ((string-equal buffer-file-truename org-agenda-file-3) nil)
   ((string-match ".+em/project+." default-directory) nil)
   ((and buffer-file-truename
         (string-match ".sql" buffer-file-truename)) nil)
   (t t)))

(defun prev-user-buffer ()
  "Switch to the previous user buffer."
  (interactive)
  (let ((i 0) (xbuf (current-buffer)))
    (previous-buffer)
    (while (< i 99)
      (if (not (user-buffer-p))
          (progn
            (previous-buffer)
            (setq i (1+ i))
            (when (= i 99)
              (message "%s" "No prev buffer")
              (switch-to-buffer xbuf)))
        (setq i 100)))))

(defun next-user-buffer ()
  "Switch to the next user buffer."
  (interactive)
  (let ((i 0) (xbuf (current-buffer)))
    (next-buffer)
    (while (< i 99)
      (if (not (user-buffer-p))
          (progn
            (next-buffer)
            (setq i (1+ i))
            (when (= i 99)
              (message "%s" "No next buffer")
              (switch-to-buffer xbuf)))
        (setq i 100)))))

(defun project-buffer-p ()
  "Return t if current buffer is a project buffer, else nil."
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((and (string-match ".+em/project+." default-directory)
         (not (string-equal major-mode "dired-mode"))) t)))

(defun prev-proj-buffer ()
  "Switch to the previous project buffer."
  (interactive)
  (let ((i 0) (xbuf (current-buffer)))
    (previous-buffer)
    (while (< i 99)
      (if (not (project-buffer-p))
          (progn
            (previous-buffer)
            (setq i (1+ i))
            (when (= i 99)
              (message "%s" "No prev proj buffer")
              (switch-to-buffer xbuf)))
        (setq i 100)))))

(defun next-proj-buffer ()
  "Switch to the next project buffer."
  (interactive)
  (let ((i 0) (xbuf (current-buffer)))
    (next-buffer)
    (while (< i 99)
      (if (not (project-buffer-p))
          (progn
            (next-buffer)
            (setq i (1+ i))
            (when (= i 99)
              (message "%s" "No next proj buffer")
              (switch-to-buffer xbuf)))
        (setq i 100)))))

(defun prev-eww-buffer ()
  "Switch to the previous eww buffer."
  (interactive)
  (let ((i 0) (xbuf (current-buffer)))
    (previous-buffer)
    (while (< i 99)
      (if (not (eq major-mode 'eww-mode))
          (progn
            (previous-buffer)
            (setq i (1+ i))
            (when (= i 99)
              (message "%s" "No prev eww buffer")
              (switch-to-buffer xbuf)))
        (setq i 100)))))

(defun next-eww-buffer ()
  "Switch to the next eww buffer."
  (interactive)
  (let ((i 0) (xbuf (current-buffer)))
    (next-buffer)
    (while (< i 99)
      (if (not (eq major-mode 'eww-mode))
          (progn
            (next-buffer)
            (setq i (1+ i))
            (when (= i 99)
              (message "%s" "No next eww buffer")
              (switch-to-buffer xbuf)))
        (setq i 100)))))

(defun alternate-buffer ()
  "Alternate buffer."
  (interactive)
  (if (string-equal (buffer-name (other-buffer)) "*Ibuffer*")
      (toggle-ibuffer)
    (switch-to-buffer (other-buffer))))

(defun prev-frame ()
  "Previous frame."
  (interactive)
  (other-frame -1))

(defun forw-frame ()
  "Next frame."
  (interactive)
  (other-frame 1))

(defun find-next-dir-file (&optional backward)
  "Find the next file (by name) in the current directory.
With prefix arg, find the previous file."
  (interactive "P")
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                (sort (directory-files
                                       (file-name-directory file) t nil t)
                                      'string<)))
           (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                     (length files))))
      (find-file (nth pos files)))))

(defun find-prev-dir-file ()
  "Find the prev file (by name) in the current directory."
  (interactive)
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                (sort (directory-files
                                       (file-name-directory file) t nil t)
                                      'string<)))
           (pos (mod (+ (cl-position file files :test 'equal) -1)
                     (length files))))
      (find-file (nth pos files)))))

(defun new-empty-buffer ()
  "Create a new empty buffer. New buffer is named file, file<2>, etc."
  (interactive)
  (let ((xbuf (generate-new-buffer "file")))
    (switch-to-buffer xbuf)
    (funcall initial-major-mode)
    xbuf))

(defvar recently-closed-buffers nil
  "A alist of recently closed buffers. Each element is (bufferName . filePath).
The max number to track is controlled by the variable
`recently-closed-buffers-max'.")

(defvar recently-closed-buffers-max 40
  "The maximum length for `recently-closed-buffers'.")

(defun save-close-cur-buf ()
  "Save and close current buffer. If the buffer is not a file, save it
to `user-emacs-directory' temp and named file_‹datetime›_‹randomhex›.txt.
Switch to the same buffer type after close, e.g. user or project."
  (interactive)
  (if (buffer-file-name)
      (when (buffer-modified-p) (save-buffer))
    (when (user-buffer-p)
      (widen)
      (when (not (equal (point-max) 1))
        (write-file
         (format "%sfile-%s-%x.txt"
                 (concat user-emacs-directory "temp/")
                 (format-time-string "%Y%m%d-%H%M%S")
                 (random #xfffff))))))
  (let ((xtype (if (project-buffer-p) "proj" "user")))
    (close-current-buffer)
    (cond
     ((eq major-mode 'dired-mode) nil)
     ((eq major-mode 'ibuffer-mode) nil)
     ((string-equal xtype "proj")
      (unless (project-buffer-p) (prev-proj-buffer)))
     ((string-equal xtype "user")
      (unless (user-buffer-p) (prev-user-buffer))))))

(defun close-current-buffer ()
  "Close the current buffer.
Similar to `kill-buffer', with the following addition:
- Prompt user to save if the buffer has been modified even if the
  buffer is not associated with a file.
- If the buffer is editing a source code file in an `org-mode' file,
  prompt the user to save before closing.
- If the buffer is a file, add the path to the list `recently-closed-buffers'."
  (interactive)
  (let ((xisOrgModeSourceFile (string-match "^*Org Src" (buffer-name))))
    (if (active-minibuffer-window)
        (minibuffer-keyboard-quit)
      (progn
        ;; Offer to save buffers that are non-empty and modified, even
        ;; for non-file visiting buffer (because `kill-buffer' does
        ;; not offer to save buffers that are not associated with
        ;; files).
        (when (and (buffer-modified-p)
                   (user-buffer-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction
                                              (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Save?" (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   xisOrgModeSourceFile)
          (if (y-or-n-p (format "Buffer %s modified; Save?" (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))
        ;; save to a list of closed buffer
        (when (buffer-file-name)
          (setq recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name))
                      recently-closed-buffers))
          (when (> (length recently-closed-buffers) recently-closed-buffers-max)
            (setq recently-closed-buffers (butlast recently-closed-buffers 1))))
        (kill-buffer (current-buffer))))))

(defun open-last-closed ()
  "Open the last closed file."
  (interactive)
  (if (> (length recently-closed-buffers) 0)
      (find-file (cdr (pop recently-closed-buffers)))
    (progn (message "No recently closed buffers in this session"))))

(defun open-recently-closed ()
  "Open recently closed file.
Prompt for a choice."
  (interactive)
  (find-file (completing-read "Open:" (mapcar (lambda (f) (cdr f))
                                              recently-closed-buffers))))

(defun list-recently-closed ()
  "List recently closed files."
  (interactive)
  (let ((xbuf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer xbuf)
    (mapc (lambda (xf) (insert (cdr xf) "\n"))
          recently-closed-buffers)))

(defun open-file-at-cursor ()
  "Open the file path under cursor.
If there is selection, use it for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›”
with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el”
for elisp files.

This command is similar to `find-file-at-point' but without prompting
for confirmation."
  (interactive)
  (let* ((xinput
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ((xp0 (point)) xp1 xp2
                  (xpathStops "^  \t\n\"`'“”|[]{}<>\\"))
              (skip-chars-backward xpathStops)
              (setq xp1 (point))
              (goto-char xp0)
              (skip-chars-forward xpathStops)
              (setq xp2 (point))
              (goto-char xp0)
              (buffer-substring-no-properties xp1 xp2))))
         xpath)
    (setq xpath
          (replace-regexp-in-string "^file://" ""
                                    (replace-regexp-in-string ":\\'" "" xinput)))
    (if (string-match-p "\\`https?://" xpath)
        (if (string-match-p "\\`https?://www.youtube.com" xpath)
            (movie xpath)
          (browse-url xpath))
      (progn
        (if (string-match "#" xpath)
            (let ((xfpath (substring xpath 0 (match-beginning 0)))
                  (xfractPart (substring xpath (1+ (match-beginning 0)))))
              (if (file-exists-p xfpath)
                  (progn
                    (find-file xfpath)
                    (goto-char (point-min))
                    (search-forward xfractPart))
                (when (y-or-n-p (format "No file %s. Create?" xfpath))
                  (find-file xfpath))))
          (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" xpath)
              (let ((xfpath (match-string-no-properties 1 xpath))
                    (xlineNum (string-to-number (match-string-no-properties 2 xpath))))
                (if (file-exists-p xfpath)
                    (progn
                      (find-file xfpath)
                      (goto-char (point-min))
                      (forward-line (1- xlineNum)))
                  (when (y-or-n-p (format "No file %s. Create?" xfpath))
                    (find-file xfpath))))
            (if (file-exists-p xpath)
                (if (and (> (length xpath) 0)
                         (not (string-equal xpath "//")))
                    (find-file xpath)
                  (setq this-command 'delete-other-windows)
                  (delete-other-windows))
              (if (file-exists-p (concat xpath ".el"))
                  (find-file (concat xpath ".el"))
                (setq this-command 'delete-other-windows)
                (delete-other-windows)))))))))

(defun url-paste-and-go ()
  "Go to URL from system clipboard."
  (let ((xurl (funcall interprogram-paste-function)))
    (if (string-match-p "\\`https?://www.youtube.com" xurl)
        (movie xurl)
      (eww xurl)
      (setq last-command 'eww-follow-link))))



(defvar run-output nil "Display name for output buffer.")

(defun run-current-go-file ()
  "Run, build or test current Go file. To build, call `universal-argument'
first. To test, file name must contain _test suffix."
  (interactive)
  (when (not buffer-file-name) (save-buffer))
  (when (buffer-modified-p) (save-buffer))
  (let* ((xoutputb "*run output*")
         (xfname buffer-file-name)
         (xbuf (buffer-name))
         (xprogName "go")
         (xcmdStr (concat xprogName " \""   xfname "\" &")))
    (setq xcmdStr (format
                   (cond (current-prefix-arg "%s build \"%s\" ")
                         ((string-match  "_test" xfname) "%s test \"%s\" ")
                         (t "%s run \"%s\" &"))
                   xprogName xfname))
    (progn
      (message "%s" xcmdStr)
      (shell-command xcmdStr xoutputb))))

(defconst run-current-file-map
  '(("pl" . "perl")
    ("py" . "python3")
    ("sh" . "bash"))
  "A association list that maps file extension to program name, used by
`run-current-file'. Each item is (EXT . PROGRAM), both strings. EXT
is file suffix (without the dot prefix), PROGRAM is program name or path,
with possibly command options. You can customize this alist.")

(defun run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call [python x.py]
in a shell.
Output is printed to buffer “*run output*”.
File suffix is used to determine which program to run, set in the variable
`run-current-file-map'.

If the file is modified or not saved, save it automatically before run."
  (interactive)
  (if (y-or-n-p "Run?")
      (progn
        (when (not buffer-file-name) (save-buffer))
        (let* ((xoutBuffer "*run output*")
               (xextAppMap run-current-file-map)
               (xfname buffer-file-name)
               (xbuf (buffer-name))
               (xfExt (file-name-extension xfname))
               (xappCmdStr (cdr (assoc xfExt xextAppMap)))
               xcmdStr)
          (setq xcmdStr
                (when xappCmdStr
                  (format "%s %s &"
                          xappCmdStr
                          (shell-quote-argument xfname))))
          (when (buffer-modified-p) (save-buffer))
          (cond
           ((string-equal xfExt "el")
            (load xfname))
           ((string-equal xfExt "go")
            (run-current-go-file))
           (t (if xappCmdStr
                  (progn
                    (message "Running %s" xcmdStr)
                    (shell-command xcmdStr xoutBuffer))
                (error "%s: Unknown file extension: %s" real-this-command xfExt))))
          (setq run-output (concat "run " xbuf))
          (enlarge-window-split)))
    (message "Abort run file")))

(defun clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or selection, respects `narrow-to-region'."
  (interactive)
  (let (xbegin xend)
    (if (region-active-p)
        (setq xbegin (region-beginning) xend (region-end))
      (setq xbegin (point-min) xend (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region xbegin xend)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+\n" nil 1) (replace-match "\n"))
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil 1) (replace-match "\n\n"))
        (goto-char (point-max))
        (while (eq (char-before) 32) (delete-char -1))))))

(defun make-backup ()
  "Make a backup copy of current file or dired marked files.
If in dired, backup current file or marked files.
The backup file name is in this format
 x.html~2018-05-15_133429~
 The last part is hour, minutes, seconds.
in the same dir. If such a file already exist, it is overwritten.
If the current buffer is not associated with a file, nothing's done."
  (interactive)
  (let ((xfname (buffer-file-name))
        (xdateTimeFormat "%Y-%m-%d_%H%M%S"))
    (if xfname
        (let ((xbackupName
               (concat xfname "~" (format-time-string xdateTimeFormat) "~")))
          (copy-file xfname xbackupName t)
          (message (concat "Backup: " xbackupName)))
      (if (eq major-mode 'dired-mode)
          (progn
            (mapc (lambda (xx)
                    (let ((xbackupName
                           (concat xx "~" (format-time-string xdateTimeFormat) "~")))
                      (copy-file xx xbackupName t)))
                  (dired-get-marked-files))
            (revert-buffer))
        (user-error "%s: buffer not file nor dired" real-this-command)))))

(defun make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `make-backup'.
If the current buffer is not associated with a file nor dired, nothing's done."
  (interactive)
  (if (buffer-file-name)
      (progn
        (make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (make-backup)))



(defun toggle-search-whitespace ()
  "Set `search-whitespace-regexp' to nil or to include hyphen lowline
tab newline. Explanation: when in isearch, space key can also stand
for other chars such as hyphen lowline tab newline. It depend on a
regex. It's convenient. But sometimes you want literal. This command
makes it easy to toggle."
  (interactive)
  (if (string-equal search-whitespace-regexp nil)
      (progn
        (setq search-whitespace-regexp "[-_ \t\n]+")
        (message "Space set to hyphen lowline tab newline space"))
    (progn
      (setq search-whitespace-regexp nil)
      (message "Space set to literal"))))

(defun isearch-cur-word ()
  "Call `isearch' on current word or “word” here is A to Z, a to z, and
hyphen [-] and lowline [_], independent of syntax table."
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (skip-chars-backward "-_A-ZА-Яa-zа-я0-9")
        (setq xp1 (point))
        (right-char)
        (skip-chars-forward "-_A-ZА-Яa-zа-я0-9")
        (setq xp2 (point))))
    (setq mark-active nil)
    (when (< xp1 (point))
      (goto-char xp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties xp1 xp2))))

(defun isearch-cur-word-forward ()
  "Forward `isearch-cur-word'."
  (interactive)
  (isearch-cur-word)
  (isearch-repeat-forward)
  (setq this-command 'isearch-cur-word-forward))

(defun isearch-cur-word-backward ()
  "Backward `isearch-cur-word'."
  (interactive)
  (isearch-cur-word)
  (isearch-repeat-backward)
  (setq this-command 'isearch-cur-word-backward))

(defun show-in-desktop ()
  "Show current file in desktop.
This command can be called when in a file buffer or in `dired'."
  (interactive)
  (let ((xpath (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files))
                       default-directory
                     (car (dired-get-marked-files)))
                 (if (buffer-file-name) (buffer-file-name) default-directory))))
    (cond
     ((string-equal system-type "darwin")
      (call-process "open" nil 0 nil "-R" xpath)))))

(defun open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in Emacs Lisp, if Fname is given, open that."
  (interactive)
  (let (xfileList xdoIt)
    (setq xfileList
          (if Fname
              (list Fname)
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
    (setq xdoIt (if (<= (length xfileList) 50) t
                  (y-or-n-p (format "Open %s files?" (length xfileList)))))
    (when xdoIt
      (cond
       ((and (string-equal major-mode 'dired-mode)
             (= 1 (length xfileList)) ; single video file
             (member (file-name-extension
                      (downcase (file-truename (nth 0 xfileList))))
                     video-extensions))
        (mapc (lambda (xfpath) (movie xfpath)) xfileList))
       ((or (and (string-equal major-mode 'dired-mode)
                 (string-match "Sound" (dired-get-filename)))
            (string-equal "mp3"
                          (file-name-extension
                           (downcase (file-truename (nth 0 xfileList))))))
        (if (get-buffer emms-playlist-buffer-name)
            (emms-add-dired)
          (emms-play-dired)))
       ((string-equal system-type "darwin")
        (mapc (lambda (xfpath) (call-process "open" nil 0 nil xfpath))
              xfileList))))))

(advice-add 'other-window :after
            (lambda (&rest r) "skip calc trail"
              (if (string-equal (buffer-name) "*Calc Trail*")
                  (other-window 1))))

(defvar next-window-or-frame-timer nil "Timer to manage double hit.")

(defun next-window-or-frame ()
  "Switch to next window. Double hit for alternate buffer."
  (interactive)
  (if (timerp next-window-or-frame-timer)
      (progn
        (setq this-command 'toggle-ibuffer)
        (toggle-ibuffer))
    (other-window 1)
    (setq next-window-or-frame-timer
          (run-with-timer (/ keyamp-double-press-timeout 1000.0) nil
                          (lambda () (setq next-window-or-frame-timer t))))))

(defun alternate-buf-or-frame ()
  "Switch to alternate buffer or frame.
If there more than one frame, switch to next frame."
  (interactive)
  (if (< 1 (length (frame-list)))
      (progn
        (setq this-command 'prev-frame)
        (prev-frame))
    (alternate-buffer)))

(defun kmacro-record ()
  "Start or stop macro recording."
  (interactive)
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-end-macro nil)
    (kmacro-start-macro nil)))

(defun window-half-height-p ()
  "Return t if WINDOW is as half high as its containing frame."
  (equal 5 (round (fceiling
                   (* 10 (/ (float (window-height))
                            (float (frame-height))))))))

(defun enlarge-window-split ()
  "Enlarge window if frame splitted equally."
  (if (and (window-half-height-p)
           (< 16 (window-height)))
      (enlarge-window (round (fceiling (* 0.3 (window-height)))))))

(defun change-wd-p ()
  "Ensure terminal input contains a command to change working directory
before actually send the cd command."
  (let ((xprompt "└ $ pushd . && ") (xprompt2 "└ $ % pushd . && "))
    (if (and (or (string-equal xprompt (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (+ (length xprompt) (line-beginning-position))))
                 (string-equal xprompt2 (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (+ (length xprompt2) (line-beginning-position)))))
             (or (string-equal (buffer-substring-no-properties
                                (+ (length xprompt) (line-beginning-position))
                                (line-end-position))
                               (car kill-ring))
                 (string-equal (buffer-substring-no-properties
                                (+ (length xprompt2) (line-beginning-position))
                                (line-end-position))
                               (car kill-ring)))) t)))

(defun get-wd ()
  "Get working directory."
  (if (one-window-p)
      (progn
        (switch-to-buffer (other-buffer))
        (copy-file-path t)
        (switch-to-buffer (other-buffer)))
    (other-window 1)
    (copy-file-path t)
    (other-window 1)))

(defun set-wd ()
  "Set working directory."
  (unless (string-equal
           (expand-file-name default-directory) (car kill-ring))
    (cond
     ((eq major-mode 'eshell-mode)
      (insert "pushd . && ")
      (yank)
      (if (change-wd-p)
          (eshell-send-input)))
     ((eq major-mode 'vterm-mode)
      (vterm-insert "pushd . && ")
      (vterm-yank)
      (if (change-wd-p)
          (vterm-send-return))))))

(defun change-wd ()
  "Change working directory."
  (interactive)
  (get-wd)
  (set-wd))

(advice-add 'completion-at-point :around
            (lambda (fun &rest r) "no need complete empty command"
              (if (and (eq major-mode 'eshell-mode)
                       (= 0 (length (buffer-substring-no-properties
                                     (+ 4 (line-beginning-position))
                                     (line-end-position)))))
                  (change-wd)
                (apply fun r))))

(defun eshell-split ()
  "Split eshell window below."
  (interactive)
  (if (one-window-p)
      (split-window-below))
  (enlarge-window-split)
  (other-window 1)
  (execute-kbd-macro (kbd "<escape>"))
  (execute-kbd-macro (kbd "6")))

(advice-add 'eshell :around
            (lambda (fun &rest r) "second press for split"
              (if (and (eq major-mode 'eshell-mode)
                       (one-window-p))
                  (progn
                    (split-window-below)
                    (switch-to-buffer (other-buffer))
                    (enlarge-window-split)
                    (other-window 1))
                (if (eq major-mode 'eshell-mode) ; not one window
                    (switch-to-buffer (other-buffer))
                  (apply fun r)))))

(defun kmacro-helper ()
  "Keyboard macro helper. Ad hoc redefine."
  (interactive)
  (toggle-debug-on-error))

(defalias 'kmacro-play 'call-last-kbd-macro)

(defun eshell-clear()
  "Clear screen eshell."
  (interactive)
  (eshell/clear t)
  (eshell-send-input))

(defun eshell-clear-input ()
  "Clear input eshell."
  (interactive)
  (if (region-active-p)
      (cut-line-or-selection)
    (beg-of-line)
    (kill-line)))

(defun eshell-search-input ()
  "Eshell history ido complete."
  (interactive)
  (let ((xhist (delete-dups (ring-elements eshell-history-ring))))
    (push "" xhist)
    (insert (ido-completing-read "Search input: " xhist))))

(defun terminal-split ()
  "Split terminal window below."
  (interactive)
  (if (one-window-p)
      (split-window-below))
  (enlarge-window-split)
  (other-window 1)
  (execute-kbd-macro (kbd "<escape>"))
  (execute-kbd-macro (kbd "9")))

(advice-add 'terminal :around
            (lambda (fun &rest r) "second press for split"
              (if (and (eq major-mode 'vterm-mode)
                       (one-window-p))
                  (progn
                    (split-window-below)
                    (switch-to-buffer (other-buffer))
                    (enlarge-window-split)
                    (other-window 1))
                (if (eq major-mode 'vterm-mode) ; not one window
                    (switch-to-buffer (other-buffer))
                  (apply fun r)))))

(defun vterm-up ()
  "Send `<up>' to the libvterm."
  (interactive)
  (vterm-send-key "<up>"))

(defun vterm-down ()
  "Send `<down>' to the libvterm."
  (interactive)
  (vterm-send-key "<down>"))

(defun vterm-backward-kill-word ()
  "Vterm backward kill word."
  (interactive)
  (vterm-send-key (kbd "C-w")))

(defun vterm-history-search ()
  "History search. Map C-a to history-incremental-search-backward in zshrc
and reverse-search-history in bashrc."
  (interactive)
  (vterm-send-key (kbd "C-a")))

(defun screenshot ()
  "Take screenshot on macOS."
  (interactive)
  (when (string-equal system-type "darwin")
    (call-process "screencapture" nil 0 nil "-W" "-U" "dummy")))

(defun clock ()
  "World clock."
  (interactive)
  (world-clock)
  (other-window 1))

(defun player ()
  "Run player."
  (interactive)
  (if (fboundp 'emms-playlist)
      (emms-playlist)
    (message "%s" "No player")))

(defun text-scale-reset ()
  "Reset text scale."
  (interactive)
  (text-scale-adjust 0))

(defun toggle-ibuffer ()
  "Toggle ibuffer.
Force switch to current buffer to update `other-buffer'."
  (interactive)
  (let ((xbuf (buffer-name)))
    (if (string-equal major-mode "ibuffer-mode")
        (switch-to-buffer (other-buffer))
      (progn
        (switch-to-buffer xbuf)
        (ibuffer)
        (condition-case nil
            (ibuffer-jump-to-buffer xbuf)
          (error nil))))))

(defun flyspell-goto-prev-error ()
  "Go to prev error."
  (interactive)
  (flyspell-goto-next-error t))

(defun sun-moon ()
  "Show the Sun and the Moon schedule."
  (interactive)
  (lunar-phases)
  (run-with-timer 1 nil 'sunrise-sunset))

(defun weather ()
  "Show weather."
  (interactive)
  (let ((url "https://www.windy.com")
        (lat (number-to-string calendar-latitude))
        (lon (number-to-string calendar-longitude)))
    (browse-url (concat url "/?" lat "," lon ",9"))))

(defun shopping ()
  "Toggle shopping list."
  (interactive)
  (find-file shopping-list-file))

(defun downloads ()
  "Go to Downloads."
  (interactive)
  (if (file-exists-p downloads-dir)
      (find-file downloads-dir)
    (message "Downloads not found")))

(defun agenda (&optional arg)
  "Modification of `org-agenda'.
Show current agenda. Do not select other window, balance windows."
  (interactive "P")
  (let ((x (get-buffer-window (current-buffer))))
    (org-agenda arg "a")
    (select-window x)
    (balance-windows)))

(advice-add 'org-agenda-redo :after 'balance-windows)

(defun todo ()
  "Modification of `org-todo'. Capitalize task title if not study."
  (interactive)
  (if (eq major-mode 'org-mode)
      (progn
        (org-todo)
        (unless (string-equal (buffer-name) "study")
          (beginning-of-line)
          (title-case-region-or-line)
          (beginning-of-line))))
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo)))

(defun toggle-gnus ()
  "Toggle gnus."
  (interactive)
  (if onlinep
      (progn
        (when (display-graphic-p)
          (setq xframe (make-frame-command))
          (other-frame 1))
        (if (get-buffer "*Group*")
            (switch-to-buffer "*Group*")
          (gnus)))
    (message "%s" "Offline")))

(defun sql ()
  "Open SQL client."
  (interactive)
  (find-file "~/.sql"))

(defvar sql-type "sqlite" "SQL type for client.")

(defun toggle-sql-type ()
  "Toggle `sql-type'."
  (interactive)
  (setq sql-type (if (equal sql-type "sqlite") "psql" "sqlite")))

(defun exec-query ()
  "Execute SQL statement separated by semicolon or selected region."
  (interactive)
  (unless (eq major-mode 'sql-mode)
    (error "Not SQL"))
  (let ((xconn (getenv "CONNINFO"))
        (xbuf (concat "*exec query*"))
        xquery xp1 xp2 (xsepRegex ";"))
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (setq xp1 (if (re-search-backward xsepRegex nil 1)
                      (goto-char (match-end 0))
                    (point)))
        (setq xp2 (if (re-search-forward xsepRegex nil 1)
                      (match-beginning 0)
                    (point)))))
    (setq xquery (string-trim
                  (concat (buffer-substring-no-properties xp1 xp2))))
    (switch-to-buffer-other-window (get-buffer-create xbuf))
    (with-current-buffer xbuf
      (read-only-mode))
    (goto-char (point-max))
    (push-mark (point) t nil)
    (let ((inhibit-read-only t))
      (when (string-equal sql-type "psql")
        (insert (shell-command-to-string
                 (format "psql %s -c \"%s\" -q" xconn xquery))))
      (when (string-equal sql-type "sqlite")
        ;; attach database can't parse ~
        (setq xquery (replace-regexp-in-string "~" (getenv "HOME") xquery))
        ;; way to send attach and dot commands along with sql query
        (let ((xres (shell-command-to-string
                     (format "sqlite3 <<EOF\n%s\nEOF" xquery))))
          (if (> (length xres) 0) (insert xres) (insert "No rows\n"))))
      (insert (concat (make-string 89 45) "\n")))
    (set-mark-command t)
    (other-window 1)))

(defun terminal ()
  "Run terminal emulator."
  (interactive)
  (vterm))

(defun novel ()
  "Read novel."
  (interactive)
  (switch-to-buffer "*novel*"))

(defun hippie-expand-undo ()
  "Undo the expansion."
  (interactive)
  (he-reset-string))

(defun pass-generate ()
  "Generate and copy pass."
  (interactive)
  (let ((xpass (read-from-minibuffer "Generate pass path: " "http/")))
    (shell-command (concat "pass generate -c " xpass))))

(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:
- for a function name where point is;
- for a variable name where point is."
  (interactive)
  (let (xsym)
    (cond
     ((setq xsym (ignore-errors
                   (with-syntax-table emacs-lisp-mode-syntax-table
                     (save-excursion
                       (or (not (zerop (skip-syntax-backward "_w")))
                           (eq (char-syntax (char-after (point))) ?w)
                           (eq (char-syntax (char-after (point))) ?_)
                           (forward-sexp -1))
                       (skip-chars-forward "`'")
                       (let ((obj (read (current-buffer))))
                         (and (symbolp obj) (fboundp obj) obj))))))
      (describe-function xsym))
     ((setq xsym (variable-at-point))
      (describe-variable xsym)))))

(defun dired-trash-move-adjust (fun &rest r)
  "Disable move to trash if move to trash is impossible. Use as :around advice."
  (when (or (string-match "DiskO" (file-truename (dired-get-filename)))
            (file-remote-p (car r)))
    (setq delete-by-moving-to-trash nil))
  (unwind-protect
      (apply fun r)
    (setq delete-by-moving-to-trash t))
  (setq delete-by-moving-to-trash t))

(advice-add 'dired-delete-file :around 'dired-trash-move-adjust)

(defvar video-extensions
  '("mkv" "mp4" "avi" "mov" "ts" "mts" "webm")
  "Open these video file extensions with `open-in-external-app'.")

(defvar external-extensions
  `("mp3" "m4a" "flac" "torrent" "app")
  "Open these file extensions with `open-in-external-app'.")

(setq external-extensions (append external-extensions video-extensions))

(defun dired-find-file-adjust (fun &rest r)
  "Adjust open file method in dired. Use as :around advice."
  (let ((x large-file-warning-threshold))
    (if (and (display-graphic-p)
             (member (file-name-extension
                      (downcase (file-truename (dired-get-filename))))
                     external-extensions))
        (progn
          (setq large-file-warning-threshold nil)
          (open-in-external-app))
      (apply fun r))
    (setq large-file-warning-threshold x)))

(advice-add 'dired-find-file :around 'dired-find-file-adjust)

(defun json-pretty ()
  "Prettify buffer if json file."
  (interactive)
  (if (string-equal (file-name-extension buffer-file-name) "json")
      (progn
        (json-pretty-print-buffer)
        (message "%s" "Pretty print json"))
    (message "%s" "Not json")))

(defun org-insert-source-code ()
  "Insert source code block."
  (interactive)
  (if (eq major-mode 'org-mode)
      (progn
        (org-insert-structure-template "src")
        (insert "bash")
        (newline))
    (message "%s" "Not org")))

(defun dired-toggle-mark ()
  "Toggle mark for the current file."
  (interactive)
  (if (string-equal " "
                    (buffer-substring-no-properties
                     (line-beginning-position) (1+ (line-beginning-position))))
      (dired-mark 1)
    (dired-unmark 1)))

(defun return-before (&rest r)
  "If region active deactivate mark conditionally and return to the point
before selection. This fun to be run as before advice for move fun."
  (interactive)
  (when (and (region-active-p)
             (or (eq last-command 'select-block)
                 (eq last-command 'extend-selection)
                 (eq last-command 'select-line)
                 (eq last-command 'select-text-in-quote)))
    (deactivate-mark)
    (set-mark-command t)
    (set-mark-command t)))

(defun delete-before (&rest r)
  "Delete selection right before insertion."
  (if (or (eq last-command 'extend-selection)
          (eq last-command 'select-text-in-quote))
      (delete-backward)))

(defun lookup-around (fun &rest r)
  "Lookup selection if buffer is read only and last command `extend-selection'.
Use as around advice e.g. for mouse left click after double click."
  (if (and (eq last-command 'extend-selection)
           buffer-read-only)
      (progn
        (return-before)
        (lookup-web))
    (apply fun r)))

(defun translate-around (fun &rest r)
  "Translate selection if buffer is read only and in eww."
  (if (and (eq major-mode 'eww-mode)
           buffer-read-only)
      (translate)
    (apply fun r)))

(defun quit ()
  "Confirm and quit. Because restart without confirm."
  (interactive)
  (if (y-or-n-p-with-timeout "Quit?" 3 nil)
      (save-buffers-kill-terminal)))

(defun toggle-frame-transparent ()
  "Toggle current frame transparency."
  (interactive)
  (when (display-graphic-p)
    (if (not (get 'toggle-frame-transparent 'state))
        (progn
          (put 'toggle-frame-transparent 'state t)
          (set-frame-parameter (selected-frame) 'alpha '(50 50)))
      (put 'toggle-frame-transparent 'state nil)
      (set-frame-parameter (selected-frame) 'alpha '(100 100)))))

(defun mouse-3 ()
  "Mouse right click. If buffer read only then lookup translation."
  (interactive)
  (if buffer-read-only (translate)))



(defun delete-or-split-window ()
  "Split window if one window, otherwise delete window."
  (interactive)
  (if (one-window-p)
      (progn
        (setq this-command 'split-window-below)
        (split-window-below))
    (setq this-command 'delete-window)
    (delete-window)))

(defun shrink-win ()
  "Shrink window to fit rows count."
  (let ((xl (count-lines (point-min) (point-max))))
    (if (< xl (window-total-height))
        (enlarge-window (- xl (window-total-height))))))

(defun shrink-completion-win ()
  "Shrink completion window."
  (let ((xbuf "*Completions*"))
    (when (get-buffer-window xbuf)
      (select-window (get-buffer-window xbuf))
      (shrink-win)
      (select-window (get-buffer-window completion-reference-buffer)))))

(defun completion-at-point-after (&rest r)
  "Setup after run completion at point."
  (let ((xbuf "*Completions*"))
    (when (get-buffer xbuf)
      (with-current-buffer xbuf
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (kill-line 4))))
      (shrink-completion-win)))
  (setq this-command 'completion-at-point))

(advice-add 'completion-at-point :after 'completion-at-point-after)

(defun delete-completion-win ()
  "Delete completion window."
  (interactive)
  (let ((xbuf "*Completions*"))
    (if (get-buffer-window xbuf)
        (progn
          (select-window (get-buffer-window xbuf))
          (delete-window)
          (select-window (get-buffer-window completion-reference-buffer)))
      (keyamp-escape))))

(defun scroll-one-pixel (&rest r)
  "Scroll one pixel up. Disables recentering cursor temporary."
  (if pixel-scroll-mode (pixel-scroll-pixel-up 1)))



(defgroup keyamp nil "Customization options for keyamp"
  :group 'help :prefix "keyamp-")

(defvar keyamp-command-hook nil "Hook for `keyamp-command'")
(defvar keyamp-insert-hook  nil "Hook for `keyamp-insert'")

(defconst keyamp-karabiner-cli
  "/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli"
  "Karabiner-Elements CLI executable. Optional for mode sync.")

;; Indicate mode in terminal
(defconst keyamp-command-indicator "🟢" "Command mode.")
(defconst keyamp-insert-indicator  "🟠" "Repeat edit.")
(defconst keyamp-repeat-indicator  "🔵" "Repeat view.")
(defconst keyamp-screen-indicator  "🟣" "Repeat screen.")

(defconst keyamp-idle-timeout (* 3 60)
  "Idle timeout for keymaps without self timeout.")
(defconst keyamp-defer-load-time 5 "Defer load second priority features.")
(defconst keyamp-double-press-timeout 300 "Double key press timeout in ms.")
(defconst keyamp-double-press-threshold 30
  "Double key press threshold in ms. Use higher values for network access.")
(unless (display-graphic-p)
  (setq keyamp-double-press-threshold 90))



(defvar keyamp-layouts nil "A alist. Key is layout name, string type.
Value is an alist, each element is of the form (\"e\" . \"d\").
First char is QWERTY, second is corresponding char of the destination layout.
When a char is not in this alist, they are assumed to be the same.")

(push '("qwerty" . nil) keyamp-layouts)

(push
 '("engineer-engram" .
   (("-" . "#") ("=" . "%") ("`" . "`")  ("q" . "b") ("w" . "y") ("e" . "o")
    ("r" . "u") ("t" . "'") ("y" . "\"") ("u" . "l") ("i" . "d") ("o" . "w")
    ("p" . "v") ("[" . "z") ("]" . "{")  ("a" . "c") ("s" . "i") ("d" . "e")
    ("f" . "a") ("g" . ",") ("h" . ".")  ("j" . "h") ("k" . "t") ("l" . "s")
    (";" . "n") ("'" . "q") ("\\" . "}") ("z" . "g") ("x" . "x") ("c" . "j")
    ("v" . "k") ("b" . "-") ("n" . "?")  ("m" . "r") ("," . "m") ("." . "f")
    ("/" . "p") ("_" . "|") ("+" . "^")  ("~" . "~") ("Q" . "B") ("W" . "Y")
    ("E" . "O") ("R" . "U") ("T" . "(")  ("Y" . ")") ("U" . "L") ("I" . "D")
    ("O" . "W") ("P" . "V") ("{" . "Z")  ("}" . "[") ("A" . "C") ("S" . "I")
    ("D" . "E") ("F" . "A") ("G" . ";")  ("H" . ":") ("J" . "H") ("K" . "T")
    ("L" . "S") (":" . "N") ("\"" . "Q") ("|" . "]") ("Z" . "G") ("X" . "X")
    ("C" . "J") ("V" . "K") ("B" . "_")  ("N" . "!") ("M" . "R") ("<" . "M")
    (">" . "F") ("?" . "P") ("1" . "7")  ("2" . "5") ("3" . "1") ("4" . "3")
    ("5" . "9") ("6" . "8") ("7" . "2")  ("8" . "0") ("9" . "4") ("0" . "6")
    ("!" . "@") ("@" . "&") ("#" . "/")  ("$" . "$") ("%" . "<") ("^" . ">")
    ("&" . "*") ("*" . "=") ("(" . "+")  (")" . "\\"))) keyamp-layouts)

(defvar keyamp-current-layout "engineer-engram"
  "The current keyboard layout. Value is a key in `keyamp-layouts'.")

(defvar keyamp--convert-table nil
  "A alist that's the conversion table from QWERTY to current layout.
Value structure is one of the key's value of `keyamp-layouts'.
Value is programmatically set from value of `keyamp-current-layout'.
Do not manually set this variable.")

(setq keyamp--convert-table
      (cdr (assoc keyamp-current-layout keyamp-layouts)))

(defun keyamp--convert-kbd-str (Charstr)
  "Return the corresponding char Charstr according to
`keyamp--convert-table'. Charstr must be a string that is, the argument
to `kbd'. E.g. \"a\" and \"a b c\". Each space separated token is
converted according to `keyamp--convert-table'."
  (mapconcat 'identity
             (mapcar
              (lambda (x) (let ((xresult (assoc x keyamp--convert-table)))
                            (if xresult (cdr xresult) x)))
              (split-string Charstr " +")) " "))

(defmacro keyamp--map (KeymapName KeyCmdAlist &optional Direct-p)
  "Map `keymap-set' over a alist KEYCMDALIST, with key layout remap.
The key is remapped from QWERTY to the current keyboard layout by
`keyamp--convert-kbd-str'.
If Direct-p is t, do not remap key to current keyboard layout."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       ,@(mapcar
          (lambda (xpair)
            `(keymap-set ,xkeymapName
               (,(if Direct-p #'identity #'keyamp--convert-kbd-str) ,(car xpair))
               ,(list 'quote (cdr xpair))))
          (cadr KeyCmdAlist)))))

(defmacro keyamp--remap (KeymapName CmdCmdAlist)
 "Map `keymap-set' remap over a alist CMDCMDALIST."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
   `(let ((,xkeymapName ,KeymapName))
      ,@(mapcar
         (lambda (xpair)
           `(keymap-set ,xkeymapName
                        ,(concat "<remap> <" (format "%s" (car xpair)) ">")
                        ,(list 'quote (cdr xpair))))
         (cadr CmdCmdAlist)))))

(defvar keyamp--deactivate-repeat-mode-fun nil "Repeat mode deactivate function.")
(defvar keyamp--repeat-mode-idle-timer nil "Repeat mode idle timer.")

(defun keyamp-cancel-repeat-mode-idle-timer ()
  "Cancel `keyamp--repeat-mode-idle-timer'."
  (if (timerp keyamp--repeat-mode-idle-timer)
      (cancel-timer keyamp--repeat-mode-idle-timer)))

(defmacro keyamp--set-map
    (KeymapName CmdList &optional CommandMode InsertMode How TimeOut)
  "Map `set-transient-map' using `advice-add' over a list CMDLIST.
- Advice default HOW :after might be changed by specific HOW;
- Activate COMMANDMODE or INSERTMODE mode optionally;
- Deactivate repeat mode after idle for TIMEOUT seconds."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       ,@(mapcar
          (lambda (xcmd)
            `(advice-add ,(list 'quote xcmd) (if ,How ,How :after)
                         (lambda (&rest r) "auto repeat"
                           (when (or (eq real-this-command 'repeat)
                                     (eq this-command ,(list 'quote xcmd)))
                             (if (and ,CommandMode keyamp-insert-p)
                                 (keyamp-command))
                             (setq keyamp--deactivate-repeat-mode-fun
                                   (set-transient-map ,xkeymapName))
                             (keyamp-cancel-repeat-mode-idle-timer)
                             (if (and ,TimeOut (not keyamp-insert-p))
                                 (setq keyamp--repeat-mode-idle-timer
                                       (run-with-idle-timer
                                        ,TimeOut nil 'keyamp-escape)))
                             (if ,InsertMode (keyamp-insert))))))
          (cadr CmdList)))))

(defmacro keyamp--set-map-hook
    (KeymapName HookList &optional CommandMode InsertMode RepeatMode)
  "Map `set-transient-map' using `add-hook' over a list HOOKLIST.
Activate command, insert or repeat mode optionally."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       ,@(mapcar
          (lambda (xhook)
            `(add-hook ,(list 'quote xhook)
                       (lambda () "Repeat."
                         (if (and ,CommandMode keyamp-insert-p)
                             (keyamp-command))
                         (if (and ,InsertMode (not keyamp-insert-p))
                             (keyamp-insert))
                         (setq keyamp--deactivate-repeat-mode-fun
                                 (set-transient-map ,xkeymapName))
                         (if ,RepeatMode (setq this-command 'keyamp--repeat-dummy)))))
          (cadr HookList)))))

(defmacro keyamp--map-leaders (KeymapName CmdCons)
  "Map leader keys using `keyamp--map'."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (keyamp--map ,xkeymapName
         '(("DEL" . ,(car (cadr CmdCons))) ("<backspace>" . ,(car (cadr CmdCons)))
           ("SPC" . ,(cdr (cadr CmdCons))))))))

(defmacro keyamp--map-tab (KeymapName Cmd)
  "Map TAB and <tab> keys to CMD using `keyamp--map'."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (keyamp--map ,xkeymapName '(("TAB" . ,Cmd) ("<tab>" . ,Cmd))))))

(defmacro with-sparse-keymap-x (&rest body)
  "Make sparse keymap x for next use in BODY."
  `(let ((x (make-sparse-keymap))) ,@body))

(defmacro advice-add-macro (SymList How Fun)
  "Map `advice-add' HOW over a list SYMLIST to FUN."
  `(progn
     ,@(mapcar
        (lambda (xcmd)
          `(advice-add ,(list 'quote xcmd) ,How ,Fun))
        (cadr SymList))))



(defvar keyamp-input-timeout 5 "Input timeout in seconds.")
(defvar keyamp-input-timer nil
  "Timer activates command mode if no command follows. Any command or self
insert cancel the timer.")

(defun keyamp-cancel-input-timer ()
  "Cancel `keyamp-input-timer'."
  (remove-hook 'post-command-hook 'keyamp-cancel-input-timer)
  (remove-hook 'post-self-insert-hook 'keyamp-cancel-input-timer)
  (if (timerp keyamp-input-timer)
      (cancel-timer keyamp-input-timer)))

(defun keyamp-input-timer-payload ()
  "Payload for `keyamp-input-timer'."
  (keyamp-cancel-input-timer)
  (keyamp-command))

(defun keyamp-start-input-timer (&rest r)
  "Start `keyamp-input-timer'."
  (keyamp-cancel-input-timer)
  (remove-hook 'post-command-hook 'keyamp-start-input-timer)
  (add-hook 'post-command-hook 'keyamp-cancel-input-timer)
  (add-hook 'post-self-insert-hook 'keyamp-cancel-input-timer)
  (setq keyamp-input-timer
        (run-with-timer keyamp-input-timeout nil 'keyamp-input-timer-payload)))



(defconst keyamp-engineer-engram-to-russian-computer
  '(("a" . "а") ("b" . "й") ("c" . "ф") ("d" . "ш") ("e" . "в")
    ("f" . "ю") ("g" . "я") ("h" . "о") ("i" . "ы") ("j" . "с")
    ("k" . "м") ("l" . "г") ("m" . "б") ("n" . "ж") ("o" . "у")
    ("q" . "э") ("r" . "ь") ("s" . "д") ("t" . "л") ("u" . "к")
    ("v" . "з") ("w" . "щ") ("x" . "ч") ("y" . "ц") ("z" . "х")
    ("." . "р") ("?" . "т") ("-" . "и") ("," . "п") ("'" . "е")
    ("`" . "ё") ("{" . "ъ") ("\"" . "н"))
  "Mapping for `keyamp-map-input-source'")

(defun keyamp-quail-get-translation (From)
  "Get translation Engineer Engram to russian-computer.
From character to character code."
  (let ((to (alist-get From keyamp-engineer-engram-to-russian-computer
             nil nil 'string-equal)))
    (when (stringp to)
      (string-to-char to))))

(defun keyamp-map-input-source (input-method)
  "Build reverse mapping for `input-method'.
Use Russian input source for command mode. Respect Engineer Engram layout."
  (require 'quail)
  (let ((xinput (symbol-name input-method))
        (xmods '(nil (control)))
        (message-log-max nil)
        (inhibit-message t))
    (activate-input-method xinput)
    (when (and current-input-method quail-keyboard-layout)
      (dolist (xmap (cdr (quail-map)))
        (let* ((xto (car xmap))
               (xfrom (if (string-equal keyamp-current-layout "engineer-engram")
                          (keyamp-quail-get-translation (char-to-string xto))
                        (quail-get-translation (cadr xmap) (char-to-string xto) 1))))
          (when (and (characterp xfrom) (characterp xto))
            (dolist (x xmods)
              (define-key local-function-key-map
                          (vector (append x (list xfrom)))
                          (vector (append x (list xto))))))))))
  (activate-input-method nil))

(defun toggle-input-source ()
  "Toggle input method."
  (interactive)
  (require 'quail)
  (if current-input-method
      (progn
        (activate-input-method nil)
        (message "ABC"))
    (activate-input-method 'russian-computer)
    (message "АБВ")))



(defconst quail-keyboard-layout-engineer-engram
  "\
                              \
  7@5&1/3$9<8>2*0=4+6\\#|%^`~  \
  bByYoOuU'(\")lLdDwWvVzZ{[    \
  cCiIeEaA,;.:hHtTsSnNqQ}]    \
  gGxXjJkK-_?!rRmMfFpP        \
                              "
  "Engineer Engram keyboard layout for Quail, e.g. for input method.")

(defun keyamp-push-quail-keyboard-layout ()
  "Push keyboard layout to quail."
  (push (cons "engineer-engram" quail-keyboard-layout-engineer-engram)
        quail-keyboard-layout-alist))

(defun keyamp-qwerty-to-current-layout ()
  "Toggle translation QWERTY layout to `keyamp-current-layout' on Emacs level.
Useful when `keyamp-current-layout' not available in OS or on keyboard level.
It is possible to have QWERTY keyboard using ANY custom layout in Emacs only."
  (interactive)
  (if (get 'keyamp-qwerty-to-current-layout 'state)
      (progn
        (put 'keyamp-qwerty-to-current-layout 'state nil)
        (quail-set-keyboard-layout "standard")
        (message "QWERTY keyboard to %s deactivated" keyamp-current-layout))
    (put 'keyamp-qwerty-to-current-layout 'state t)
    (quail-set-keyboard-layout keyamp-current-layout)
    (message "QWERTY keyboard to %s activated" keyamp-current-layout))
  (let ((xl (alist-get keyamp-current-layout keyamp-layouts nil nil 'string-equal)))
    (mapc #'(lambda (x)
              (keymap-set key-translation-map
                          (car x)
                          (if (get 'keyamp-qwerty-to-current-layout 'state)
                              (cdr x)))) xl)))


;; keymaps

(defvar keyamp-map (make-sparse-keymap)
  "Parent keymap of `keyamp-command-map'.
Define keys that are available in both command and insert modes here.")

(defvar keyamp-command-map (cons 'keymap keyamp-map)
  "Keymap that takes precedence over all other keymaps in command mode.
Inherits bindings from `keyamp-map'.

In command mode, if no binding is found in this map `keyamp-map' is
checked, then if there is still no binding, the other active keymaps
are checked like normal. However, if a key is explicitly bound to nil
in this map, it will not be looked up in `keyamp-map' and lookup will
skip directly to the normally active maps.

In this way, bindings in `keyamp-map' can be disabled by this map.
Effectively, this map takes precedence over all others when command mode
is enabled.")



(defconst keyamp-tty-seq-timeout 30
  "Timeout in ms to wait key sequence after ESC sent in tty.")

(defun keyamp-tty-ESC-filter (map)
  (if (and (equal (this-single-command-keys) [?\e])
           (sit-for (/ keyamp-tty-seq-timeout 1000.0)))
      [escape] map))

(defun keyamp-lookup-key (map key)
  (catch 'found
    (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

(defun keyamp-catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into <escape>."
  (when (memq (terminal-live-p (frame-terminal)) '(t pc))
    (let ((esc-binding (keyamp-lookup-key input-decode-map ?\e)))
      (keymap-set input-decode-map
                  "ESC" `(menu-item "" ,esc-binding :filter keyamp-tty-ESC-filter)))))

;; Map terminal ESC to <escape>.
(keymap-set key-translation-map "ESC" "<escape>")


;; setting keys

(keyamp--map keyamp-map
  '(("<escape>" . keyamp-escape)           ("S-<escape>" . ignore)

    ;; Control sequences for leaders. Russian converted from Engineer Engram.
    ;; The sequences are prefixes for key hold down in Karabiner.
    ("C-^" . keyamp-left-leader-map)       ("C-+" . keyamp-left-leader-map)
    ("C-_" . keyamp-right-leader-map)      ("C-И" . keyamp-right-leader-map)))

;; Single keys mapping must double in Russian here. All prefix sequences mapped
;; automatically using `keyamp-map-input-source'. If missing then same.
(keyamp--map keyamp-command-map
  '(("RET" . keyamp-insert)                ("<return>"    . keyamp-insert)          ("S-<return>"    . ignore)
    ("DEL" . keyamp-left-leader-map)       ("<backspace>" . keyamp-left-leader-map) ("S-<backspace>" . prev-proj-buffer)
    ("SPC" . keyamp-right-leader-map)                                               ("S-SPC"         . next-proj-buffer)

    ;; left half
    ("`" . make-frame-command)             ("ё" . make-frame-command)               ("~" . keyamp-qwerty-to-current-layout)  ("Ë" . keyamp-qwerty-to-current-layout)
    ("1" . kmacro-record)                                                           ("!" . ignore)
    ("2" . kmacro-play)                                                             ("@" . ignore)
    ("3" . kmacro-helper)                                                           ("#" . ignore)                           ("№" . ignore)
    ("4" . append-to-register-1)                                                    ("$" . ignore)
    ("5" . terminal)                                                                ("%" . ignore)

    ("q" . insert-space-before)            ("й" . insert-space-before)              ("Q" . ignore)                           ("Й" . ignore)
    ("w" . backward-del-word)              ("ц" . backward-del-word)                ("W" . ignore)                           ("Ц" . ignore)
    ("e" . undo)                           ("у" . undo)                             ("E" . todo)                             ("У" . todo)
    ("r" . del-word)                       ("к" . del-word)                         ("R" . ignore)                           ("К" . ignore)
    ("t" . cut-text-block)                 ("е" . cut-text-block)                   ("T" . ignore)                           ("Е" . ignore)

    ("a" . shrink-whitespaces)             ("ф" . shrink-whitespaces)               ("A" . ignore)                           ("Ф" . ignore)
    ("s" . open-line)                      ("ы" . open-line)                        ("S" . prev-proj-buffer)                 ("Ы" . prev-proj-buffer)
    ("d" . delete-backward)                ("в" . delete-backward)                  ("D" . repeat)                           ("В" . repeat)
    ("f" . newline)                        ("а" . newline)                          ("F" . next-proj-buffer)                 ("А" . next-proj-buffer)
    ("g" . activate-region)                ("п" . activate-region)                  ("G" . ignore)                           ("П" . ignore)

    ("z" . toggle-comment)                 ("я" . toggle-comment)                   ("Z" . ignore)                           ("Я" . ignore)
    ("x" . cut-line-or-selection)          ("ч" . cut-line-or-selection)            ("X" . ignore)                           ("Ч" . ignore)
    ("c" . copy-line-or-selection)         ("с" . copy-line-or-selection)           ("C" . ignore)                           ("С" . ignore)
    ("v" . paste-or-paste-prev)            ("м" . paste-or-paste-prev)              ("V" . ignore)                           ("М" . ignore)
    ("b" . toggle-letter-case)             ("и" . toggle-letter-case)               ("B" . ignore)                           ("И" . ignore)

    ;; right half
    ("6" . pass)                                                                    ("^" . ignore)
    ("7" . jump-to-register)                                                        ("&" . ignore)
    ("8" . copy-to-register)                                                        ("*" . goto-matching-bracket) ; QWERTY * → = Engineer Engram, QWERTY / → = RU PC Karabiner
    ("9" . toggle-case-fold-search)                                                 ("(" . ignore)
    ("0" . eshell)                                                                  (")" . ignore)
    ("-" . enlarge-window)                                                          ("_" . ignore)
    ("=" . goto-matching-bracket)                                                   ("+" . ignore)

    ("y"  . isearch-cur-word-forward)      ("н" . isearch-cur-word-forward)         ("Y" . ignore)                           ("Н" . ignore)
    ("u"  . back-word)                     ("г" . back-word)                        ("U" . ignore)                           ("Г" . ignore)
    ("i"  . previous-line)                 ("ш" . previous-line)                    ("I" . beg-of-line-or-block)             ("Ш" . beg-of-line-or-block)
    ("o"  . forw-word)                     ("щ" . forw-word)                        ("O" . ignore)                           ("Щ" . ignore)
    ("p"  . jump-mark)                     ("з" . jump-mark)                        ("P" . ignore)                           ("З" . ignore)
    ("["  . alternate-buffer)              ("х" . alternate-buffer)                 ("{" . ignore)                           ("Х" . ignore)
    ("]"  . write-file)                    ("ъ" . write-file)                       ("}" . ignore)                           ("Ъ" . ignore)
    ("\\" . bookmark-set)                                                           ("|" . ignore)

    ("h" . beg-of-line)                    ("р" . beg-of-line)                      ("H"  . ignore)                          ("Р" . ignore)
    ("j" . backward-char)                  ("о" . backward-char)                    ("J"  . isearch-cur-word-backward)       ("О" . isearch-cur-word-backward)
    ("k" . next-line)                      ("л" . next-line)                        ("K"  . end-of-line-or-block)            ("Л" . end-of-line-or-block)
    ("l" . forward-char)                   ("д" . forward-char)                     ("L"  . isearch-cur-word-forward)        ("Д" . isearch-cur-word-forward)
    (";" . end-of-lyne)                    ("ж" . end-of-lyne)                      (":"  . ignore)                          ("Ж" . ignore)
    ("'" . alternate-buf-or-frame)         ("э" . alternate-buf-or-frame)           ("\"" . ignore)                          ("Э" . ignore)

    ("n" . isearch-forward)                ("т" . isearch-forward)                  ("N" . isearch-backward)                 ("Т" . isearch-backward)
    ("m" . backward-left-bracket)          ("ь" . backward-left-bracket)            ("M" . ignore)                           ("Ь" . ignore)
    ("," . next-window-or-frame)           ("б" . next-window-or-frame)             ("<" . ignore)                           ("Б" . ignore)
    ("." . forward-right-bracket)          ("ю" . forward-right-bracket)            (">" . ignore)                           ("Ю" . ignore)
    ("/" . goto-matching-bracket)                                                   ("?" . ignore)

    ("<left>" . left-char)                 ("<right>" . right-char)
    ("<up>"   . up-line)                   ("<down>"  . down-line)))

(keyamp--map (define-prefix-command 'keyamp-left-leader-map)
  '(("TAB" . terminal-split)               ("<tab>"       . terminal-split)
    ("ESC" . ignore)                       ("<escape>"    . ignore)
    ("RET" . execute-extended-command)     ("<return>"    . execute-extended-command)
    ("DEL" . select-block)                 ("<backspace>" . select-block)
    ("SPC" . select-text-in-quote)

    ;; left leader left half
    ("`" . next-buffer)
    ("1" . periodic-chart)
    ("2" . kmacro-name-last-macro)
    ("3" . apply-macro-to-region-lines)
    ("4" . clear-register-1)
    ("5" . repeat-complex-command)

    ("q" . reformat-lines)
    ("w" . org-ctrl-c-ctrl-c)
    ("e" . delete-or-split-window)
    ("r" . query-replace)
    ("t" . copy-text-block)

    ("a" . kill-line)
    ("s" . prev-user-buffer)
    ("d" . alternate-buffer)
    ("f" . next-user-buffer)
    ("g" . rectangle-mark-mode)

    ("z" . universal-argument)
    ("x" . restart-emacs)
    ("c" . copy-to-register-1)
    ("v" . paste-from-register-1)
    ("b" . toggle-previous-letter-case)

    ;; left leader right half
    ("6" . quit)
    ("7" . number-to-register)
    ("8" . sql)
    ("9" . ignore)
    ("0" . eww)
    ("-" . proced)
    ("=" . screenshot)

    ("y" . find-name-dired)
    ("u" . flymake-goto-prev-error)

                                           ("i i"   . show-in-desktop)
    ("i DEL" . count-words)                ("i SPC" . count-matches)

    ("o"  . flymake-goto-next-error)
    ("p"  . show-kill-ring)
    ("["  . save-close-cur-buf)
    ("]"  . find-file)
    ("\\" . bookmark-rename)

    ("h"  . bookmark-jump)

    ("j s"   . glyphless-display-mode)     ("j l"   . narrow-to-region-or-block)
                                           ("j k"   . narrow-to-defun)
    ("j f"   . toggle-word-wrap)           ("j j"   . widen)
    ("j DEL" . hl-line-mode)               ("j SPC" . whitespace-mode)

    ("k s"   . space-to-newline)
    ("k d"   . delete-matching-lines)      ("k k"   . list-matching-lines)
    ("k f"   . delete-non-matching-lines)
    ("k r"   . quote-lines)                ("k u"   . escape-quotes)
    ("k t"   . delete-duplicate-lines)     ("k y"   . slash-to-double-backslash)
    ("k v"   . reformat-to-sentence-lines) ("k n"   . double-backslash-to-slash)
    ("k w"   . sort-lines-key-value)       ("k o"   . slash-to-backslash)
    ("k x"   . insert-column-a-z)          ("k ."   . sort-lines-block-or-region)
    ("k c"   . cycle-hyphen-lowline-space) ("k ,"   . sort-numeric-fields)
    ("k DEL" . ispell-word)                ("k SPC" . flyspell-buffer)

    ("l" . describe-foo-at-point)
    (";" . recentf-open-files)
    ("'" . sync)
    ("n" . switch-to-buffer)
    ("m" . downloads)
    ("," . open-last-closed)
    ("." . player)
    ("/" . goto-line)

    ("i ESC" . ignore)                     ("i <escape>" . ignore)
    ("j ESC" . ignore)                     ("j <escape>" . ignore)
    ("k ESC" . ignore)                     ("k <escape>" . ignore)

    ("<mouse-1>" . ignore)
    ("<mouse-2>" . ignore)
    ("<mouse-3>" . ignore)))

(keyamp--map (define-prefix-command 'keyamp-right-leader-map)
  '(("TAB" . eshell-split)                 ("<tab>"       . eshell-split)
    ("ESC" . ignore)                       ("<escape>"    . ignore)
    ("RET" . read-only-mode)               ("<return>"    . read-only-mode)
    ("DEL" . select-line)                  ("<backspace>" . select-line)
    ("SPC" . extend-selection)

    ;; right leader left half
    ("`" . find-next-dir-file)
    ("1" . view-lossage)
    ("2" . insert-kbd-macro)
    ("3" . config)
    ("4" . change-bracket-pairs)
    ("5" . json-pretty)

    ("q" . fill-or-unfill)
    ("w" . sun-moon)

    ("e e"   . insert-date)
    ("e DEL" . clock)                      ("e SPC" . calendar)

    ("r" . query-replace-regexp)
    ("t" . calc)
    ("a" . mark-whole-buffer)
    ("s" . clean-whitespace)

    ("D"     . repeat)                     ("В"     . repeat)
    ("d e"   . org-shiftup)
    ("d s"   . shell-command-on-region)    ("d l"   . elisp-eval-region-or-buffer)
    ("d d"   . eval-last-sexp)             ("d k"   . run-current-file)
    ("d f"   . shell-command)              ("d i"   . async-shell-command)
    ("d DEL" . stow)                       ("d SPC" . eval-defun)

    ("f e"   . insert-emacs-quote)         ("f i"   . insert-ascii-single-quote)
    ("f f"   . insert-char)                ("f j"   . insert-brace)
    ("f d"   . emoji-insert)               ("f k"   . insert-paren)
    ("f s"   . insert-formfeed)            ("f l"   . insert-square-bracket)
    ("f g"   . insert-double-angle-quote)  ("f h"   . insert-double-curly-quote)
    ("f DEL" . insert-backtick-quote)      ("f SPC" . insert-ascii-double-quote)

    ("g" . new-empty-buffer)
    ("z" . goto-char)
    ("x" . next-eww-buffer)
    ("c" . copy-all)
    ("v" . tasks)
    ("b" . title-case-region-or-line)

    ;; right leader right half
    ("6" . pass-generate)
    ("7" . increment-register)
    ("8" . insert-register)
    ("9" . org-insert-source-code)
    ("0" . toggle-theme)
    ("-" . snake)
    ("=" . toggle-input-source)

    ("y"  . find-text)
    ("u"  . backward-punct)
    ("i"  . copy-file-path)
    ("o"  . forward-punct)
    ("p"  . view-echo-area-messages)
    ("["  . open-last-closed)
    ("]"  . rename-visited-file)
    ("\\" . bookmark-delete)

    ("h" . half-page-backward)
    ("j" . toggle-truncate-lines)
    ("k" . make-backup-and-save)
    ("l" . display-line-numbers-mode)
    (";" . half-page-forward)
    ("'" . toggle-frame-maximized)

    ("n" . save-buffer)
    ("m" . dired-jump)
    ("," . save-close-cur-buf)
    ("." . mark-defun)
    ("/" . recenter-top-bottom)            ("*" . recenter-top-bottom)

    ("e ESC" . ignore)                     ("e <escape>" . ignore)
    ("d ESC" . ignore)                     ("d <escape>" . ignore)
    ("f ESC" . ignore)                     ("f <escape>" . ignore)

    ("<mouse-1>" . ignore)
    ("<mouse-2>" . ignore)
    ("<mouse-3>" . ignore)))


;; Core Remaps

;; Hold down ESC to post C-h (karabiner) and call `help-map'.
(keyamp--map-leaders help-map '(lookup-word-definition . translate))
(keyamp--map-tab help-map lookup-wikipedia)
(keyamp--map help-map
  '(("ESC" . ignore)             ("<escape>" . ignore)                                              ("C-h" . nil) ; unmap for use by which key
    ("RET" . lookup-web)         ("<return>" . lookup-web)
    ("e"   . describe-char)      ("i"        . info)
    ("s"   . info-lookup-symbol) ("j"        . describe-function)
    ("d"   . man)                ("k"        . describe-key)
    ("f"   . elisp-index-search) ("l"        . describe-variable)
    ("q"   . describe-syntax)    ("p"        . apropos-documentation)                               ("<f1>" . ignore) ("<help>" . ignore) ("C-w" . ignore) ("C-c" . ignore)
    ("w"   . describe-bindings)  ("o"        . lookup-all-dictionaries)                             ("C-o"  . ignore) ("C-\\"   . ignore) ("C-n" . ignore) ("C-f" . ignore)
    ("r"   . describe-mode)      ("u"        . lookup-all-synonyms)                                 ("C-s"  . ignore) ("C-e"    . ignore) ("'"   . ignore) ("6"   . ignore)
    ("a"   . describe-face)      (";"        . lookup-wiktionary)                                   ("9"    . ignore) ("L"      . ignore) ("n"   . ignore) ("p"   . ignore) ("v" . ignore)
    ("g"   . apropos-command)    ("h"        . describe-coding-system)                              ("?"    . ignore) ("A"      . ignore) ("U"   . ignore) ("S"   . ignore)
    ("z"   . apropos-variable)   ("."        . lookup-word-dict-org)
    ("x"   . apropos-value)      (","        . lookup-etymology)
    ("c"   . describe-text-properties)))

(keyamp--map global-map
  '(("C-r"     . open-file-at-cursor) ; hold down RET to post C-r (karabiner)
    ("C-t"     . hippie-expand)       ; hold down RET in insert mode
    ("<f13>"   . ignore)              ; special key not f13 really
    ("<next>"  . half-page-forward)   ("<prior>" . half-page-backward)
    ("<home>"  . scroll-down-command) ("<end>"   . scroll-up-command)))

(keyamp--map global-map
  '(("<double-mouse-1>" . extend-selection) ("<mouse-3>" . mouse-3)
    ("<header-line>  <mouse-1>" . novel)  ("<header-line>  <mouse-3>" . ignore)
    ("<left-fringe>  <mouse-1>" . ignore) ("<left-fringe>  <mouse-3>" . ignore)
    ("<right-fringe> <mouse-1>" . ignore) ("<right-fringe> <mouse-3>" . ignore)))

(with-eval-after-load 'lookup
  (advice-add 'keyamp-insert :around 'lookup-around))
(advice-add 'keyamp-insert :around 'translate-around)
(advice-add 'keyamp-insert :before 'delete-before)

(when (display-graphic-p)
  (advice-add 'mouse-set-point   :around 'lookup-around)
  (advice-add 'mouse-drag-region :before 'copy-selection)
  (advice-add 'mouse-set-point   :before 'scroll-one-pixel)
  (advice-add 'mouse-set-point   :after  'keyamp-command-if-insert)
  (advice-add 'mac-mwheel-scroll :before 'keyamp-command-if-insert)
  (advice-add 'mac-mwheel-scroll :before 'deactivate-mark-if-active))

;; Avoid karabiner mode sync lag. Hack.
(keyamp--remap keyamp-command-map '((hippie-expand . open-file-at-cursor)))

(with-sparse-keymap-x
 ;; Repeat using DEL/SPC or D. The concept widely used to form Repeat mode.
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . repeat)))
 (keyamp--set-map x '(repeat)))

(with-sparse-keymap-x
 ;; Hold down RET in insert mode to call `hippie-expand' with C-t.
 ;; Next RET press to insert a possible expansion. DEL to undo, SPC to confirm.
 (keyamp--map-leaders x '(hippie-expand-undo . insert-space-before))
 (keyamp--map x '(("RET" . hippie-expand) ("<return>" . hippie-expand)))
 (keyamp--set-map x '(hippie-expand)))

(with-sparse-keymap-x
 ;; After starting up an isearch press DEL to retreat to the previous
 ;; search string. Press SPC to pull string from kill ring into search string.
 (keyamp--map-leaders x '(isearch-ring-retreat . isearch-yank-kill))
 (keyamp--set-map-hook x '(isearch-mode-hook) nil nil :repeat))

;; Hit TAB to repeat after typing in search string and set following transient
;; map. Backtab of Shift TAB to repeat backward.
(keyamp--map isearch-mode-map
  '(("<escape>"  . isearch-cancel)          ("C-^"         . keyamp-left-leader-map)
    ("TAB"       . isearch-repeat-forward)  ("<tab>"       . isearch-repeat-forward)
    ("<backtab>" . isearch-repeat-backward) ("S-<tab>"     . isearch-repeat-backward)
    ("DEL"       . isearch-del-char)        ("<backspace>" . isearch-del-char)))
(keyamp--remap isearch-mode-map
  '((paste-from-register-1 . isearch-yank-register-1)))

(with-sparse-keymap-x
 ;; Find the occurrence of the current search string with J/L or DEL/SPC.
 ;; Press I/K to get search strings from the ring then DEL/SPC to repeat.
 ;; RET to search again.
 (keyamp--map-leaders x '(isearch-repeat-backward . isearch-repeat-forward))
 (keyamp--map x
   '(("i" . isearch-ring-retreat)    ("ш" . isearch-ring-retreat)
     ("j" . isearch-repeat-backward) ("о" . isearch-repeat-backward)
     ("k" . isearch-ring-advance)    ("л" . isearch-ring-advance)
     ("l" . isearch-repeat-forward)  ("д" . isearch-repeat-forward)))

 (keyamp--set-map x
   '(isearch-ring-retreat     isearch-ring-advance
     isearch-repeat-backward  isearch-repeat-forward
     isearch-cur-word-forward isearch-cur-word-backward
     isearch-yank-kill))

 (add-hook 'isearch-mode-hook
           (lambda (&rest r) "repeat after exit minibuffer"
             (if (or (eq real-this-command 'exit-minibuffer)
                     (eq real-this-command 'keyamp-minibuffer-insert))
                 (set-transient-map x))) 96))


;; Repeat mode. Screen commands.

(with-sparse-keymap-x
 ;; Leader layer to become transient main. Base map for next leaders adjustment
 ;; by transient maps which might be set by following target commands subsets.
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--map-tab x half-page-forward)
 (keyamp--map x '(("C-r" . delete-other-windows)))

 (keyamp--remap x
   '((delete-forward-char    . next-buffer)
     (make-frame-command     . delete-frame)
     (backward-del-word      . sun-moon)
     (undo                   . delete-or-split-window)
     (del-word               . ignore)
     (cut-text-block         . calc)
     (jump-mark              . view-echo-area-messages)
     (delete-backward        . alternate-buffer)
     (set-mark-command       . new-empty-buffer)
     (cut-line-or-selection  . prev-eww-buffer)
     (copy-line-or-selection . agenda)
     (paste-or-paste-prev    . tasks)
     (backward-left-bracket  . dired-jump)
     (forward-right-bracket  . player)
     (kmacro-helper          . config)
     (copy-to-register       . sql)
     (terminal               . terminal-split)
     (eshell                 . eshell-split)))

 (keyamp--set-map x
   '(prev-user-buffer           next-user-buffer
     delete-other-windows       split-window-below
     delete-window
     open-last-closed           save-close-cur-buf
     prev-proj-buffer           next-proj-buffer
     prev-eww-buffer            next-eww-buffer
     tasks                      config
     previous-buffer            next-buffer
     find-prev-dir-file         find-next-dir-file
     shrink-window              enlarge-window
     dired-jump                 downloads)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-user-buffer) (newline . next-user-buffer)))
 (keyamp--set-map x
   '(prev-user-buffer     next-user-buffer
     delete-other-windows delete-window
     split-window-below)))

(with-sparse-keymap-x
 ;; Hit RET right away to hide split window.
 (keyamp--remap x '((keyamp-insert . delete-other-windows)))
 (keyamp--set-map x '(split-window-below sync)))

(with-sparse-keymap-x
 (keyamp--remap x
   '((open-line       . prev-proj-buffer) (newline . next-proj-buffer)
     (delete-backward . speedbar)))
 (keyamp--set-map x '(prev-proj-buffer next-proj-buffer)))

(with-sparse-keymap-x
 (keyamp--map-tab x half-page-forward)
 (keyamp--remap x
   '((open-line       . prev-eww-buffer) (newline       . next-eww-buffer)
     (delete-backward . eww-reload)      (keyamp-insert . eww-reload)
     (undo            . justify-buffer)))
 (keyamp--set-map x '(prev-eww-buffer next-eww-buffer)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-user-buffer) (newline . tasks)))
 (keyamp--set-map x '(tasks)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-user-buffer) (newline . config)))
 (keyamp--set-map x '(config)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . previous-buffer) (newline . next-buffer)))
 (keyamp--set-map x '(previous-buffer next-buffer)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . find-prev-dir-file) (newline . find-next-dir-file)))
 (keyamp--set-map x '(find-prev-dir-file find-next-dir-file)))

(with-sparse-keymap-x
 ;; Easy dired jump repeat.
 (keyamp--remap x
   '((open-line . dired-jump) (newline . dired-jump)
     (backward-left-bracket . dired-jump)))
 (keyamp--set-map x
   '(dired-jump downloads dired-find-file ibuffer-visit-buffer open-last-closed
     bookmark-jump widget-button-press)))

(with-sparse-keymap-x
 ;; Hold down comma to call `save-close-cur-buf'. Then comma to repeat.
 (keyamp--remap x
   '((open-line            . prev-user-buffer)
     (newline              . next-user-buffer)
     (next-window-or-frame . save-close-cur-buf)
     (alternate-buffer     . save-close-cur-buf)
     (keyamp-insert        . delete-other-windows)))
 (keyamp--set-map x '(save-close-cur-buf)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . shrink-window) (newline . enlarge-window)))
 (keyamp--set-map x '(shrink-window enlarge-window) nil nil nil 2))

(with-sparse-keymap-x
 (keyamp--remap x '((make-frame-command . delete-frame)))
 (add-hook 'after-make-frame-functions
           (lambda (&rest r) "delete frame after make"
             (run-with-timer 0.5 nil (lambda () (set-transient-map x))))))

(with-sparse-keymap-x
 (keyamp--remap x '((make-frame-command . undelete-frame)))
 (advice-add 'delete-frame :after
             (lambda (&rest r) "undelete frame after delete"
               (run-with-timer 0.5 nil (lambda () (set-transient-map x))))))

(with-sparse-keymap-x
 (keyamp--remap x
   '((backward-left-bracket . dired-jump)
     (make-frame-command    . delete-frame)))
 (keyamp--set-map x '(alternate-buf-or-frame)))

(with-sparse-keymap-x
 ;; SPC to switch other window after split as a result of the commands.
 ;; DEL to delete other window.
 (keyamp--map-leaders x '(delete-other-windows . other-window))
 (keyamp--remap x '((keyamp-insert . delete-other-windows)))
 (keyamp--set-map x
   '(describe-foo-at-point   describe-variable
     describe-function       describe-key
     describe-mode           describe-char
     describe-face           list-matching-lines
     agenda
     run-current-file        exec-query
     view-echo-area-messages sun-moon
     clock                   async-shell-command)))

(defvar keyamp-escape-double-timer nil "Timer to manage ESC double hit.")

(defun keyamp-escape-double ()
  "Quick enough ESC double press calls `toggle-ibuffer'."
  (when (and (timerp keyamp-escape-double-timer)
             (eq last-command this-command))
    (cancel-timer keyamp-escape-double-timer)
    (setq this-command 'toggle-ibuffer)
    (toggle-ibuffer))
  (setq keyamp-escape-double-timer
        (run-with-timer (/ keyamp-double-press-timeout 1000.0) nil
                        (lambda () (setq keyamp-escape-double-timer t)))))

(advice-add 'keyamp-escape :after 'keyamp-escape-double)

(with-sparse-keymap-x
 (keyamp--remap x '((backward-left-bracket . dired-jump)))
 (keyamp--set-map x '(alternate-buffer)))


;; Repeat mode. View commands.

(with-sparse-keymap-x
 ;; Initiate by triple DEL/SPC (hold down).
 ;; I/K or DEL/SPC to move by lines. See `return-before'.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--map-tab x half-page-forward)
 (keyamp--remap x
   '((previous-line         . up-line)    (next-line           . down-line)
     (backward-left-bracket . dired-jump) (paste-or-paste-prev . tasks)))
 (keyamp--set-map x '(up-line down-line))
 (keyamp--set-map-hook x '(ibuffer-hook gnus-group-mode-hook) nil nil :repeat)
 (advice-add-macro
  '(other-window translate) :after
  (lambda (&rest r) "keymap move by lines"
    (when (memq major-mode '(org-agenda-mode gnus-group-mode ibuffer-mode eww-mode messages-buffer-mode))
      (set-transient-map x)
      (setq this-command 'down-line)))))

(with-sparse-keymap-x
 ;; Initiate by SPC then double DEL or DEL then SPC hold down. Press H then
 ;; second press H for beginning of the buffer, third for end of the buffer.
 ;; Similarly for ; in opposite direction. I/K or DEL/SPC to move by blocks.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--remap x
   '((previous-line . beg-of-line-or-block)  (next-line   . end-of-line-or-block)
     (beg-of-line   . beg-of-line-or-buffer) (end-of-lyne . end-of-line-or-buffer)))

 (advice-add 'beg-of-line :after
             (lambda (&rest r) "second press for beginning of the buffer"
               (when (eq last-command this-command)
                 (setq this-command 'beg-of-line-or-buffer)
                 (beg-of-line-or-buffer))))

 (advice-add 'end-of-lyne :after
             (lambda (&rest r) "second press for end of the buffer"
               (when (eq last-command this-command)
                 (setq this-command 'end-of-line-or-buffer)
                 (end-of-line-or-buffer))))

 ;; Hit sticky shift then I/K to move by blocks.
 (keyamp--map x
   '(("I" . previous-line) ("Ш" . previous-line)
     ("K" . next-line)     ("Л" . next-line)))

 (keyamp--set-map x
   '(beg-of-line-or-block  end-of-line-or-block
     beg-of-line-or-buffer end-of-line-or-buffer)))

(with-sparse-keymap-x
 ;; DEL DEL prefix command.
 ;; Triple DEL (hold down) to move lines up. Or hit DEL DEL SPC to
 ;; next user buffer and continue repeat by SPC.
 (keyamp--map-leaders x '(up-line . next-user-buffer))
 (keyamp--remap x
   '((previous-line       . beg-of-line-or-block)
     (next-line           . select-block)
     (keyamp-escape       . return-before)
     (open-file-at-cursor . exec-query)))
 (keyamp--set-map x '(select-block)))

(with-sparse-keymap-x
 ;; SPC SPC prefix command.
 ;; Double SPC to run `extend-selection', then next SPC press to
 ;; deactivate mark and run `down-line'. That is, hold down SPC to
 ;; start move down lines with SPC while DEL does up lines. The core
 ;; function deferred for a small amount of time and hold down move
 ;; works smoothly without actual activation of a region.
 (keyamp--map-leaders x '(prev-user-buffer . down-line))
 (keyamp--remap x '((keyamp-escape . return-before)))
 (keyamp--set-map x '(extend-selection)))

(with-sparse-keymap-x
 ;; DEL SPC prefix command.
 (keyamp--map-leaders x '(beg-of-line-or-block . end-of-line-or-block))
 (keyamp--remap x '((keyamp-escape . return-before)))
 (keyamp--set-map x '(select-text-in-quote)))

(with-sparse-keymap-x
 ;; SPC DEL prefix command.
 (keyamp--map-leaders x '(beg-of-line-or-block . end-of-line-or-block))
 (keyamp--remap x '((keyamp-escape . return-before)))
 (keyamp--set-map x '(select-line)))

(advice-add-macro
 ;; If region active deactivate mark and return to the point before selection.
 '(ibuffer-backward-filter-group   ibuffer-forward-filter-group
   gnus-topic-goto-prev-topic-line gnus-topic-goto-next-topic-line
   up-line                         down-line
   beg-of-line-or-block            end-of-line-or-block
   prev-user-buffer                next-user-buffer
   tasks                           prev-eww-buffer
   terminal                        eshell
   terminal-split                  eshell-split
   agenda                          calc)
 :before 'return-before)

(advice-add 'exec-query :after 'return-before) ; go back after query

(with-sparse-keymap-x
 ;; Left/right arrows repeat by DEL/SPC.
 (keyamp--map-leaders x '(backward-char . forward-char))
 (keyamp--remap x '((backward-char . left-char) (forward-char . right-char)))
 (keyamp--set-map x '(left-char right-char) nil nil nil 1))

(with-sparse-keymap-x
 ;; Repeat brackets move with DEL/SPC.
 (keyamp--map-leaders x '(backward-left-bracket . forward-right-bracket))
 (keyamp--set-map x '(backward-left-bracket forward-right-bracket)
                    nil nil nil 1))

(with-sparse-keymap-x
 ;; Repeat move by words with DEL/SPC.
 (keyamp--map-leaders x '(back-word . forw-word))
 (keyamp--remap x '((backward-char . back-word) (forward-char . forw-word)))
 ;; (keyamp--set-map x '(back-word forw-word) nil nil nil 0.5)
 (keyamp--set-map x '(back-word forw-word)))

(with-sparse-keymap-x
 ;; Repeat move by punct with U/O or DEL/SPC.
 (keyamp--map-leaders x '(back-word . forw-word))
 (keyamp--remap x '((back-word . backward-punct) (forw-word . forward-punct)))
 (keyamp--set-map x '(backward-punct forward-punct) nil nil nil 1))

(with-sparse-keymap-x
 ;; Hold down H/; to initiate half page up/down. Repeat with I/K or DEL/SPC.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--map-tab x scroll-up-command)
 (keyamp--remap x
   '((previous-line . half-page-backward) (next-line . half-page-forward)
     (down-line     . half-page-forward)  (up-line   . half-page-backward)))
 (unless (display-graphic-p) ; touch reader
   (keyamp--remap x '((down-line . half-page-backward) (up-line . half-page-forward))))
 (keyamp--set-map x '(half-page-backward half-page-forward)))

(advice-add 'half-page-forward :around
            (lambda (fun &rest r) "ibuffer exception"
              (if (eq major-mode 'ibuffer-mode)
                  (if (display-graphic-p)
                      (toggle-gnus)
                    (ibuffer-forward-filter-group))
                (apply fun r))))

(advice-add 'half-page-forward :around
            (lambda (fun &rest r) "gnus exception"
              (if (eq major-mode 'gnus-group-mode)
                  (if (display-graphic-p)
                      (toggle-ibuffer)
                    (gnus-topic-goto-next-topic-line))
                (apply fun r))))

(with-sparse-keymap-x
 ;; Initially TAB makes half page forward, following presses do full page.
 ;; Arrows always do half page and keep TAB transient, see previous keymap.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--map-tab x next-line)
 (keyamp--remap x
   '((previous-line . scroll-down-command) (next-line . scroll-up-command)
     (down-line     . half-page-forward)   (up-line   . half-page-backward)))
 (unless (display-graphic-p) ; touch reader
   (keyamp--remap x '((down-line . half-page-backward) (up-line . half-page-forward))))
 (keyamp--set-map x '(scroll-down-command scroll-up-command)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(next-line . next-line))
 (keyamp--remap x '((next-line . jump-mark)))
 (keyamp--set-map x '(jump-mark)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(text-scale-decrease . text-scale-increase))
 (keyamp--map-tab x text-scale-reset)
 (keyamp--set-map x '(text-scale-decrease text-scale-increase text-scale-reset)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(backward-button . forward-button))
 (keyamp--set-map x '(backward-button forward-button)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(next-line . next-line))
 (keyamp--remap x '((next-line . recenter-top-bottom)))
 (keyamp--set-map x '(recenter-top-bottom)) nil nil nil 2)


;; Repeat mode. Edit commands.

(with-sparse-keymap-x
 ;; After hit delete backward/forward char, shrink whitespaces or insert
 ;; space before while in command mode, DEL/SPC start to do delete/space.
 (keyamp--map-leaders x '(delete-forward-char . insert-space-before))
 (keyamp--set-map x '(delete-forward-char) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . insert-space-before))
 (keyamp--set-map x '(delete-backward insert-space-before) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--remap x '((open-line . backward-del-word) (newline . del-word)))
 (keyamp--set-map x '(backward-del-word del-word) nil nil nil 2))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(undo  . delete-backward))
 (keyamp--remap x '((delete-backward . undo-redo)))
 (keyamp--set-map x '(undo undo-redo)))

(with-sparse-keymap-x
 (keyamp--remap x '((delete-backward . cut-text-block)))
 (keyamp--set-map x '(cut-text-block)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . insert-space-before))
 (keyamp--remap x '((delete-backward . shrink-whitespaces)))
 (keyamp--set-map x '(shrink-whitespaces) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . toggle-comment)))
 (keyamp--set-map x '(toggle-comment) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . cut-line-or-selection)))
 (keyamp--set-map x '(kill-region) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . copy-line-or-selection)))
 (keyamp--set-map x '(copy-line-or-selection) nil nil nil 1))

(advice-add-macro
 '(copy-line-or-selection copy-to-register-1) :after
 (lambda (&rest r) "return to the point before selection after fun"
   (when (or (eq last-command 'extend-selection)
             (eq last-command 'select-text-in-quote))
     (set-mark-command t)
     (set-mark-command t))))

(with-sparse-keymap-x
 ;; Repeat in insert mode.
 (keyamp--map x '(("v" . paste-or-paste-prev) ("м" . paste-or-paste-prev)))
 (keyamp--set-map x '(paste-or-paste-prev)))

(with-sparse-keymap-x
 (keyamp--remap x '((delete-backward . toggle-letter-case)))
 (keyamp--set-map x '(toggle-letter-case) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . undo))
 (keyamp--remap x
   '((undo                   . org-shiftup)
     (delete-backward        . org-shiftdown)
     (copy-line-or-selection . agenda)))
 (keyamp--set-map x '(org-shiftup org-shiftdown) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(agenda . todo))
 (keyamp--remap x '((undo . todo) (copy-line-or-selection . agenda)))
 (keyamp--set-map x '(todo insert-date) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--remap x '((delete-backward . cycle-hyphen-lowline-space)))
 (keyamp--set-map x '(cycle-hyphen-lowline-space) nil nil nil 1))

(with-sparse-keymap-x
 ;; SPC S SPC to clean whitespaces and save the buffer or DEL to close.
 (keyamp--map-leaders x '(save-close-cur-buf . save-buffer))
 (keyamp--remap x '((backward-left-bracket . dired-jump)))
 (keyamp--set-map x '(clean-whitespace) nil nil nil 3))

(with-sparse-keymap-x
 (keyamp--remap x '((backward-left-bracket . dired-jump)))
 (keyamp--set-map x '(save-buffer) nil nil nil 3))


;; Modes Remaps.

(defun keyamp-minibuffer-insert ()
  "If minibuffer input not empty then confirm and exit instead
of insert mode activation."
  (interactive)
  (if (> (length (buffer-substring (minibuffer-prompt-end) (point))) 0)
      (exit-minibuffer)
    (keyamp-insert)))

(defun keyamp-minibuffer-escape ()
  "If minibuffer input not empty then activate command mode instead
of quit minibuffer."
  (interactive)
  (if (> (length (buffer-substring (minibuffer-prompt-end) (point))) 0)
      (keyamp-command)
    (abort-recursive-edit)))

(defun keyamp-insert-n ()
  "Insert N literally."
  (interactive)
  (keyamp-insert-init)
  (execute-kbd-macro (kbd "n")))

(defun keyamp-insert-y ()
  "Insert Y literally. Only if asked."
  (interactive)
  (if (and (minibufferp)
           (string-match "y, n, !\\|yn!q"
                         (buffer-substring (point-min) (point-max))))
      (progn
        (keyamp-insert-init)
        (execute-kbd-macro (kbd "y")))
    (backward-kill-word 1)))

(defun keyamp-insert-! ()
  "Insert ! literally."
  (interactive)
  (keyamp-insert-init)
  (execute-kbd-macro (kbd "!")))

(with-eval-after-load 'minibuffer
  (with-sparse-keymap-x
   ;; On minibuffer startup press DEL or I to list history backwards or
   ;; SPC or K to list completion candidates forward. After that
   ;; I/K or DEL/SPC to list either history or completion candidates
   ;; accordingly choice made. RET to confirm and exit, ESC to quit.
   ;; To switch from history to candidates listing press ESC then double
   ;; SPC `extend-selection' and DEL/SPC or I/K again to continue move
   ;; backward/forward. Similarly double DEL to activate history move.
   (keyamp--map-leaders x '(select-block . extend-selection))
   (keyamp--remap x
     '((end-of-lyne         . keyamp-insert-n) ; literal answers y or n
       (backward-del-word   . keyamp-insert-y) ; Engineer Engram layout
       (isearch-backward    . keyamp-insert-!) ; remap required for others
       (keyamp-insert       . keyamp-minibuffer-insert)
       (keyamp-escape       . keyamp-minibuffer-escape)
       (open-file-at-cursor . keyamp-exit-minibuffer)))
   ;; The hook is last one run during minibuffer setup and set the keymap.
   (keyamp--set-map-hook x '(minibuffer-setup-hook) :command nil :repeat))

  ;; Hit D/DEL for No, K/SPC for Yes to answer non-literal y or n.
  (keyamp--remap y-or-n-p-map
    '((select-block     . y-or-n-p-insert-n)
      (delete-backward  . y-or-n-p-insert-n)
      (extend-selection . y-or-n-p-insert-y)
      (next-line        . y-or-n-p-insert-y)))

  ;; Right after paste in minibuffer mostly confirm and exit follow.
  (advice-add 'paste-or-paste-prev :after
              (lambda (&rest r) "activate insert mode if in minibuffer"
                (when (and (minibufferp) (not keyamp-insert-p))
                  (keyamp-insert))))

  (keyamp--remap minibuffer-local-map
    '((previous-line . previous-line-or-history-element)
      (next-line     . next-line-or-history-element)
      (select-block  . previous-line-or-history-element)))

  (keyamp--map-tab minibuffer-local-completion-map minibuffer-complete)

  (keyamp--remap minibuffer-mode-map
    '((previous-line . previous-line-or-history-element)
      (next-line     . next-line-or-history-element)
      (select-block  . previous-line-or-history-element)))

  (advice-add 'next-line-or-history-element :before
              (lambda (&rest r) "move point to the end of line beforehand"
                (goto-char (point-max))))

  (with-sparse-keymap-x
   (keyamp--map-tab x minibuffer-next-completion)
   (keyamp--map-leaders x
     '(minibuffer-previous-completion . minibuffer-next-completion))
   (keyamp--remap x '((keyamp-escape . delete-completion-win)))
   (keyamp--map x
     '(("RET"      . minibuffer-choose-completion)
       ("<return>" . minibuffer-choose-completion)))
   (keyamp--set-map x
     '(completion-at-point
       minibuffer-previous-completion minibuffer-next-completion)))

  (advice-add 'completion-at-point :after
              (lambda (&rest r) "select candidate" (minibuffer-next-completion)))
  (advice-add-macro
   '(completion-at-point minibuffer-choose-completion delete-completion-win)
   :after 'keyamp-insert-init)

  (keyamp--remap minibuffer-inactive-mode-map '((view-echo-area-messages . player)))
  (keyamp--map minibuffer-inactive-mode-map
    '(("<mouse-3>" . toggle-ibuffer) ("<double-mouse-1>" . ignore))))

(with-eval-after-load 'icomplete
  (defun keyamp-exit-minibuffer ()
    "Exit if file completion. It means use content of minibuffer as it is,
  no select completion candidates. Else force complete and exit, that
  is, select and use first completion candidate. In case file
  completion, for most cases no need to complete, because there is NO
  right candidate. Otherwise, in all cases one MUST select a candidate.
  Simply hit TAB to minibuffer-complete file name if the name exists."
    (interactive)
    (if (eq (icomplete--category) 'file)
        (exit-minibuffer)
      (icomplete-force-complete-and-exit)))

  (add-hook 'minibuffer-setup-hook
            (lambda () "activate insert mode if file completion"
              (when (eq (icomplete--category) 'file)
                (keyamp-insert)
                (if keyamp--deactivate-repeat-mode-fun
                    (funcall keyamp--deactivate-repeat-mode-fun))
                (setq this-command 'keyamp-insert))) 96)

  (keyamp--map icomplete-minibuffer-map
    '(("RET" . keyamp-exit-minibuffer) ("<return>" . keyamp-exit-minibuffer)))

  (keyamp--remap icomplete-minibuffer-map
    '((previous-line    . icomplete-backward-completions)
      (next-line        . icomplete-forward-completions)
      (extend-selection . icomplete-forward-completions)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((keyamp-insert . keyamp-exit-minibuffer)
       (previous-line . icomplete-backward-completions)
       (next-line     . icomplete-forward-completions)))
   (keyamp--set-map x
     '(icomplete-backward-completions icomplete-forward-completions)))

  (with-sparse-keymap-x
   (keyamp--remap x
     '((previous-line . previous-line-or-history-element)
       (next-line     . icomplete-forward-completions)))
   (keyamp--set-map-hook x '(icomplete-minibuffer-setup-hook) nil nil :repeat))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((keyamp-insert . exit-minibuffer)
       (previous-line . previous-line-or-history-element)
       (next-line     . next-line-or-history-element)))
   (keyamp--set-map x
     '(previous-line-or-history-element next-line-or-history-element))))

(add-hook 'ido-setup-hook
  (lambda () "ido-completion-map created after ido setup only"
    (keyamp--remap ido-completion-map
      '((keyamp-insert    . ido-exit-minibuffer)
        (previous-line    . previous-line-or-history-element)
        (select-block     . previous-line-or-history-element)
        (next-line        . ido-next-match)
        (extend-selection . ido-next-match)))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--remap x '((previous-line . ido-prev-match) (next-line . ido-next-match)))
 (keyamp--set-map x '(ido-prev-match ido-next-match)))

(with-eval-after-load 'dired
  (keyamp--map dired-mode-map
    '(("C-h" . dired-do-delete) ("C-r" . delete-other-windows)
      ("<mouse-1>"        . mouse-set-point)
      ("<mouse-2>"        . mouse-set-point) ; mouse-2 really mouse-1
      ("<double-mouse-1>" . dired-find-file)))

  (keyamp--remap dired-mode-map
    '((keyamp-insert         . dired-find-file)
      (backward-left-bracket . dired-jump)
      (forward-right-bracket . open-in-external-app)
      (insert-space-before   . ignore)
      (del-word              . dired-unmark-all-marks)
      (backward-del-word     . dired-do-chmod)
      (shrink-whitespaces    . dired-hide-details-mode)
      (open-line             . prev-user-buffer)
      (delete-backward       . dired-toggle-mark)
      (newline               . next-user-buffer)
      (toggle-comment        . revert-buffer)
      (cut-line-or-selection . dired-kill-subdir)
      (cut-text-block        . dired-maybe-insert-subdir)
      (paste-or-paste-prev   . dired-create-directory)
      (toggle-letter-case    . dired-sort)
      (copy-to-register-1    . dired-do-copy)
      (paste-from-register-1 . dired-do-rename)
      (mark-whole-buffer     . dired-toggle-marks)
      (kmacro-helper         . config)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(dired-toggle-mark . dired-toggle-mark))
   (keyamp--set-map x '(dired-toggle-mark) nil nil nil 1))

  (advice-add 'dired-toggle-marks :before 'dired-unmark-all-marks))

(with-eval-after-load 'wdired
  (keyamp--map wdired-mode-map
    '(("C-h" . wdired-abort-changes) ("C-r" . wdired-finish-edit)
      ("C-q" . wdired-abort-changes) ("C-t" . wdired-finish-edit)))
  (advice-add-macro '(wdired-abort-changes wdired-finish-edit)
                    :after 'keyamp-command))

(with-eval-after-load 'dired-utils
  (keyamp--map-tab dired-mode-map dired-leader-map)
  (keyamp--map-tab (define-prefix-command 'dired-leader-map) dired-omit-mode)
  (keyamp--map dired-leader-map
    '(("e"  . dired-optimize-png)     ("u" . dired-2drawing)
      ("o"  . dired-rotate-img-right) ("p" . dired-rotate-img-left)
      ("a"  . dired-image-autocrop)   ("s" . dired-open-marked)
      ("d"  . dired-show-metadata)    ("h" . dired-rotate-img-180)
      ("l"  . dired-2png)             (";" . dired-scale-image)
      ("\'" . dired-zip-enc)          ("c" . dired-2jpg)
      ("/"  . dired-zip)              ("." . dired-unzip))))

(advice-add 'beg-of-line-or-buffer :after
            (lambda (&rest r) "stay on row with file in dired"
              (if (eq major-mode 'dired-mode) (dired-next-line 1))))

(advice-add 'beg-of-line-or-block :after
            (lambda (&rest r) "move by subdirs and stay on row with file in dired"
              (if (and (eq major-mode 'dired-mode)
                       (or (equal "D" (this-command-keys))
                           (equal "d" (this-command-keys))
                           (equal [backspace] (this-command-keys))
                           (= 127 (aref (this-command-keys) 0))))
                  (dired-previous-line 1))))

(advice-add 'end-of-line-or-block :after
            (lambda (&rest r) "move by subdirs and stay on row with file in dired"
              (if (and (eq major-mode 'dired-mode)
                       (or (equal "T" (this-command-keys))
                           (equal "t" (this-command-keys))
                           (equal " " (this-command-keys))))
                  (dired-next-line 1))))

(with-eval-after-load 'rect ; sane rectangle controls
  (keyamp--remap rectangle-mark-mode-map
    '((keyamp-insert          . string-rectangle)
      (insert-space-before    . open-rectangle)
      (copy-line-or-selection . copy-rectangle-as-kill)
      (delete-backward        . kill-rectangle)
      (paste-or-paste-prev    . yank-rectangle)
      (copy-to-register       . copy-rectangle-to-register)
      (toggle-comment         . rectangle-number-lines)
      (cut-line-or-selection  . clear-rectangle)
      (clean-whitespace       . delete-whitespace-rectangle))))

(with-eval-after-load 'ibuf-ext
  (keyamp--map-tab ibuffer-mode-map toggle-gnus)
  (unless (display-graphic-p)
    (keyamp--map-tab ibuffer-mode-map ibuffer-forward-filter-group))
  (keyamp--map ibuffer-mode-map
    '(("C-h" . ibuffer-do-delete) ("<double-mouse-1>" . ibuffer-visit-buffer)))

  ;; Same as base map for Screen, constantly available in ibuffer.
  (keyamp--remap ibuffer-mode-map
    '((previous-line          . up-line)
      (next-line              . down-line)
      (keyamp-insert          . ibuffer-visit-buffer)
      (end-of-lyne            . ibuffer-forward-filter-group)
      (beg-of-line            . ibuffer-backward-filter-group)
      (end-of-line-or-block   . ibuffer-forward-filter-group)
      (beg-of-line-or-block   . ibuffer-backward-filter-group)
      (backward-del-word      . sun-moon)
      (undo                   . delete-or-split-window)
      (del-word               . ignore)
      (cut-text-block         . calc)
      (jump-mark              . view-echo-area-messages)
      (open-line              . prev-user-buffer)
      (delete-backward        . alternate-buffer)
      (newline                . next-user-buffer)
      (set-mark-command       . new-empty-buffer)
      (cut-line-or-selection  . prev-eww-buffer)
      (copy-line-or-selection . agenda)
      (paste-or-paste-prev    . tasks)
      (backward-left-bracket  . downloads)
      (forward-right-bracket  . player)
      (kmacro-helper          . config)
      (copy-to-register       . sql)
      (left-char              . other-window)
      (right-char             . ibuffer-visit-buffer)))

  (keyamp--map ibuffer-mode-filter-group-map
    '(("C-h" . help-command) ("<mouse-1>" . ibuffer-toggle-filter-group)))

  (keyamp--remap ibuffer-mode-filter-group-map
    '((keyamp-insert . ibuffer-toggle-filter-group)
      (right-char    . ibuffer-toggle-filter-group)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x '((previous-line . up-line) (next-line . down-line)))
   (keyamp--set-map x '(ibuffer-toggle-filter-group)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . ibuffer-backward-filter-group)
       (next-line     . ibuffer-forward-filter-group)
       (beg-of-line   . beg-of-line-or-buffer)
       (end-of-lyne   . end-of-line-or-buffer)))
   (keyamp--set-map x
     '(ibuffer-backward-filter-group ibuffer-forward-filter-group)))

  (unless (display-graphic-p)
    (advice-add 'ibuffer-visit-buffer :before
                (lambda (&rest r) (unless (one-window-p) (delete-other-windows))))))

(with-eval-after-load 'ibuffer
  (keyamp--map ibuffer-name-map '(("<mouse-1>" . mouse-set-point))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . ibuffer-do-delete)))
 (keyamp--set-map x '(ibuffer-do-delete) nil nil nil 3))

(with-eval-after-load 'company
  (keyamp--map-tab company-active-map company-complete-common)

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((keyamp-escape   . company-abort)
       (keyamp-insert   . company-complete-selection)
       (isearch-forward . company-search-candidates)
       (previous-line   . company-select-previous)
       (next-line       . company-select-next)
       (beg-of-line     . company-previous-page)
       (end-of-lyne     . company-next-page)))

   (keyamp--set-map x
     '(company-select-previous company-select-next company-previous-page
       company-next-page company-show-doc-buffer company-search-abort
       company-manual-begin))

   (advice-add 'company-manual-begin :before 'keyamp-command)
   (add-hook 'keyamp-command-hook
             (lambda ()
               (when company-candidates
                 (setq keyamp--deactivate-repeat-mode-fun
                       (set-transient-map x))
                 (setq this-command 'keyamp--repeat-dummy)))))

  (with-sparse-keymap-x
   ;; Activate command mode after complete selection, but if next hit is SPC
   ;; then activate insert mode and insert SPC. DEL to undo the completion.
   (advice-add-macro '(company-search-abort company-complete-selection)
                     :after (lambda (&rest r) "`keyamp-command'"
                              (if keyamp--deactivate-repeat-mode-fun
                                  (funcall keyamp--deactivate-repeat-mode-fun))
                              (if keyamp-insert-p (keyamp-command))))

   (defun keyamp-insert-and-SPC ()
     "Activate insert mode and insert SPC."
     (interactive)
     (unless keyamp-insert-p (keyamp-insert))
     (insert " "))
   (keyamp--map-leaders x '(undo . keyamp-insert-and-SPC))
   (keyamp--set-map x '(company-search-abort company-complete-selection)))

  (advice-add 'company-search-candidates :after 'keyamp-insert-init)

  (keyamp--map-tab company-search-map company-search-repeat-forward)
  (keyamp--map company-search-map
    '(("<escape>"  . company-search-abort)
      ("<backtab>" . company-search-repeat-backward)
      ("S-<tab>"   . company-search-repeat-backward)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x
     '(company-search-repeat-backward . company-search-repeat-forward))
   (keyamp--set-map x
     '(company-search-repeat-backward company-search-repeat-forward))))

(with-eval-after-load 'transient
  (keyamp--map transient-base-map '(("<escape>" . transient-quit-one))))

(with-eval-after-load 'arc-mode
  (keyamp--remap archive-mode-map '((keyamp-insert . archive-extract))))

(with-eval-after-load 'bookmark
  (keyamp--remap bookmark-bmenu-mode-map
    '((keyamp-insert . bookmark-bmenu-this-window))))

(with-eval-after-load 'button
  (keyamp--remap button-map '((keyamp-insert . push-button))))

(with-eval-after-load 'compile
  (keyamp--remap compilation-button-map '((keyamp-insert . compile-goto-error))))

(with-eval-after-load 'flymake
  (keyamp--remap flymake-diagnostics-buffer-mode-map
    '((keyamp-insert . flymake-goto-diagnostic))))

(with-eval-after-load 'replace
  (keyamp--remap occur-mode-map
    '((keyamp-insert . occur-mode-goto-occurrence)))

  (keyamp--map query-replace-map
    '(("d" . skip) ("k" . act) ("в" . skip) ("л" . act))))

(with-eval-after-load 'shr
  (keyamp--remap shr-map '((keyamp-insert . shr-browse-url))))

(with-eval-after-load 'simple
  (keyamp--remap completion-list-mode-map '((keyamp-insert . choose-completion))))

(with-eval-after-load 'wid-edit
  (keyamp--remap widget-link-keymap '((keyamp-insert . widget-button-press))))

(with-eval-after-load 'org
  (keyamp--map-tab org-mode-map org-cycle)
  (keyamp--remap org-mode-map
    '((eval-last-sexp . insert-date) (insert-date . org-time-stamp))))

(with-eval-after-load 'org-agenda
  (keyamp--remap org-agenda-mode-map
    '((keyamp-insert         . org-agenda-switch-to)
      (undo                  . delete-or-split-window)
      (open-line             . prev-user-buffer)
      (delete-backward       . org-agenda-redo)
      (newline               . next-user-buffer)
      (paste-or-paste-prev   . tasks)
      (left-char             . other-window)
      (right-char            . org-agenda-switch-to)
      (previous-line         . up-line)
      (next-line             . down-line)
      (backward-del-word     . sun-moon)
      (cut-text-block        . calc)
      (jump-mark             . view-echo-area-messages)
      (set-mark-command      . new-empty-buffer)
      (cut-line-or-selection . prev-eww-buffer)
      (backward-left-bracket . downloads)
      (forward-right-bracket . player)
      (kmacro-helper         . config)
      (copy-to-register      . sql))))

(with-eval-after-load 'org-keys
  (keyamp--remap org-mouse-map '((org-open-at-mouse . mouse-set-point))))

(with-eval-after-load 'eww
  (keyamp--map-tab eww-mode-map half-page-forward)
  (keyamp--map eww-mode-map '(("<left-fringe> <mouse-1>" . half-page-forward)))
  (keyamp--remap eww-mode-map
    '((open-line             . eww-back-url)
      (newline               . eww-next-url)
      (delete-backward       . eww-reload)
      (del-word              . eww-reload-all)
      (undo                  . justify-buffer)
      (shrink-whitespaces    . eww-browse-with-external-browser)
      (backward-left-bracket . downloads)
      (forward-right-bracket . player)))
  (keyamp--remap eww-link-keymap '((keyamp-insert . eww-follow-link))))

(with-eval-after-load 'emms
  (with-sparse-keymap-x
   (keyamp--map-leaders x '(open-line . newline))
   (keyamp--remap x
     '((open-line             . emms-seek-backward-or-previous)
       (delete-backward       . emms-pause)
       (undo                  . emms-random)
       (newline               . emms-seek-forward-or-next)
       (backward-del-word     . emms-seek-backward)
       (del-word              . emms-seek-forward)
       (forward-right-bracket . player)))
   (keyamp--set-map x
     '(emms-seek-backward-or-previous emms-seek-forward-or-next
       emms-playlist-mode-play-smart  emms-pause
       emms-random)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(open-line . newline))
   (keyamp--remap x
     '((open-line . emms-seek-backward) (newline . emms-seek-forward)))
   (keyamp--set-map x '(emms-seek-backward emms-seek-forward))))

(with-eval-after-load 'emms-playlist-mode
  (keyamp--remap emms-playlist-mode-map
    '((keyamp-insert         . emms-playlist-mode-play-smart)
      (mouse-set-point       . emms-playlist-mode-play-smart)
      (open-line             . emms-seek-backward-or-previous)
      (newline               . emms-seek-forward-or-next)
      (undo                  . delete-or-split-window)
      (backward-del-word     . emms-seek-backward)
      (del-word              . emms-seek-forward)
      (delete-backward       . emms-playlist-mode-center-current)
      (backward-left-bracket . dired-jump)
      (forward-right-bracket . player))))

(with-eval-after-load 'flyspell
  (with-sparse-keymap-x
   (keyamp--map-leaders x '(open-line . newline))
   (keyamp--remap x
     '((delete-backward . ispell-word)
       (open-line       . flyspell-goto-prev-error)
       (newline         . flyspell-goto-next-error)))
   (keyamp--set-map x
     '(flyspell-buffer          ispell-word
       flyspell-goto-prev-error flyspell-goto-next-error))))

(with-eval-after-load 'doc-view
  (keyamp--map doc-view-mode-map '(("C-r" . delete-other-windows)))
  (keyamp--remap doc-view-mode-map
    '((previous-line  . doc-view-previous-line-or-previous-page)
      (next-line      . doc-view-next-line-or-next-page)
      (up-line        . doc-view-previous-line-or-previous-page)
      (down-line      . doc-view-next-line-or-next-page)
      (backward-char  . doc-view-previous-page)
      (forward-char   . doc-view-next-page)
      (enlarge-window . doc-view-enlarge)
      (beg-of-line    . doc-view-scroll-down-or-previous-page)
      (end-of-lyne    . doc-view-scroll-up-or-next-page)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(doc-view-shrink . doc-view-enlarge))
   (keyamp--set-map x '(doc-view-shrink doc-view-enlarge) nil nil nil 2))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
    (keyamp--remap x
      '((previous-line . doc-view-scroll-down-or-previous-page)
        (next-line     . doc-view-scroll-up-or-next-page)))
    (keyamp--set-map x
      '(doc-view-scroll-down-or-previous-page doc-view-scroll-up-or-next-page)))

  (with-sparse-keymap-x
     (keyamp--map-leaders x '(previous-line . next-line))
      (keyamp--remap x
        '((previous-line . doc-view-scroll-down-or-previous-page)
          (next-line     . doc-view-scroll-up-or-next-page)
          (up-line       . doc-view-scroll-down-or-previous-page)
          (down-line     . doc-view-scroll-up-or-next-page)))
      (keyamp--set-map x
        '(doc-view-previous-line-or-previous-page doc-view-next-line-or-next-page))))

(with-eval-after-load 'image-mode
  (keyamp--remap image-mode-map
    '((backward-char   . image-previous-file)
      (forward-char    . image-next-file)
      (previous-line   . image-decrease-size)
      (next-line       . image-increase-size)
      (undo            . image-dired)
      (delete-backward . image-rotate)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(backward-char . forward-char))
   (keyamp--set-map x '(image-previous-file image-next-file))
   (keyamp--set-map-hook x '(image-mode-hook)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--set-map x '(image-decrease-size image-increase-size))))

(with-eval-after-load 'esh-mode
  (keyamp--map-tab eshell-mode-map completion-at-point)
  (keyamp--map eshell-mode-map
    '(("C-h" . eshell-interrupt-process) ("C-t" . delete-other-windows)))
  (keyamp--remap eshell-mode-map
    '((cut-line-or-selection . eshell-clear-input)
      (next-eww-buffer       . eshell-clear)
      (select-block          . eshell-previous-input)
      (quoted-insert         . eshell-interrupt-process)
      (toggle-comment        . ignore)))

  (with-sparse-keymap-x
   (keyamp--map x '(("v" . paste-or-paste-prev) ("м" . paste-or-paste-prev)))
   (advice-add 'paste-or-paste-prev :after
               (lambda (&rest r) "activate insert mode in eshell"
                 (when (eq major-mode 'eshell-mode) ; vterm no
                   (keyamp-insert-init)
                   (setq keyamp--deactivate-repeat-mode-fun
                         (set-transient-map x))
                   (add-hook 'post-command-hook 'keyamp-start-input-timer)))))

  (advice-add 'paste-or-paste-prev :before
              (lambda (&rest r) "go to prompt before paste in eshell"
                (when (eq major-mode 'eshell-mode)
                  (unless (= (line-number-at-pos)
                             (count-lines (point-min) (point-max)))
                    (goto-char (point-max))))))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . eshell-previous-input)
       (next-line     . eshell-next-input)
       (keyamp-insert . eshell-send-input)))
   (keyamp--map-tab x change-wd)
   (keyamp--set-map x '(eshell-previous-input eshell-search-input) :command)
   (advice-add 'eshell-next-input :after
               (lambda (&rest r) "repeat"
                 (when (eq this-command 'eshell-next-input)
                   (if keyamp-insert-p (keyamp-command))
                   (set-transient-map x)))))

  (with-sparse-keymap-x
   ;; Insert mode is primary for eshell. The keymap ready after eshell start,
   ;; command submit or cancel. Use DEL/SPC to list history, V for paste and
   ;; other commands available in insert mode right after send input.
   ;; Useful to have an input timeout for insert mode in eshell.
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . eshell-previous-input)
       (next-line     . eshell-search-input)))

   (keyamp--map-tab x change-wd)
   (keyamp--map x
     '(("v" . paste-or-paste-prev)    ("м" . paste-or-paste-prev)
       ("'" . alternate-buf-or-frame) ("э" . alternate-buf-or-frame)
       ("[" . alternate-buffer)       ("х" . alternate-buffer)
       ("," . next-window-or-frame)   ("б" . next-window-or-frame)
       ("5" . terminal)               ("0" . eshell)))

   (keyamp--set-map x '(eshell-send-input eshell-interrupt-process) nil :insert)
   (keyamp--set-map-hook x '(eshell-mode-hook) nil :insert)

   (advice-add-macro '(end-of-line-or-buffer other-window) :after
                 (lambda (&rest r) "insert mode in eshell after come to prompt"
                   (when (and (eq major-mode 'eshell-mode)
                              (= (line-number-at-pos)
                                 (count-lines (point-min) (point-max))))
                     (keyamp-insert)
                     (set-transient-map x)
                     (add-hook 'post-command-hook 'keyamp-start-input-timer))))

   (advice-add 'keyamp-input-timer-payload :after
               (lambda () "eshell transient"
                 (when (eq major-mode 'eshell-mode)
                   (set-transient-map x)
                   (keyamp-indicate-repeat)
                   (setq this-command 'keyamp--repeat-dummy)))))

   (add-hook 'eshell-mode-hook
             (lambda (&rest r) "`keyamp-start-input-timer'"
               (add-hook 'post-command-hook 'keyamp-start-input-timer)))

   (advice-add-macro '(eshell-send-input eshell-interrupt-process)
    :after (lambda (&rest r) "`keyamp-start-input-timer'"
             (add-hook 'post-command-hook 'keyamp-start-input-timer))))

(advice-add-macro
 ;; Activate command mode after jump from insert. The commands might be run
 ;; by hold down a key or transient keymap from insert mode, mostly eshell.
 '(alternate-buf-or-frame  alternate-buffer
   delete-other-windows    delete-window
   split-window-below      other-window
   prev-user-buffer        next-user-buffer
   toggle-ibuffer          save-close-cur-buf
   dired-jump)
 :after (lambda (&rest r) "`keyamp-command'"
          (if keyamp-insert-p (keyamp-command))))

(with-eval-after-load 'vterm
  (keyamp--map-tab vterm-mode-map vterm-send-tab)
  (keyamp--map vterm-mode-map
    '(("C-h" . term-interrupt-subjob) ("C-q" . term-interrupt-subjob)
      ("C-r" . delete-other-windows)  ("C-t" . delete-other-windows)
      ("C-u" . vterm-send-next-key)))

  (keyamp--remap vterm-mode-map
    '((select-block          . vterm-up)
      (prev-eww-buffer       . vterm-clear)
      (paste-or-paste-prev   . vterm-yank)
      (paste-from-register-1 . vterm-yank-pop)
      (backward-del-word     . vterm-backward-kill-word)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . vterm-up)
       (next-line     . vterm-down)
       (keyamp-insert . vterm-send-return)))
   (keyamp--set-map x '(vterm-history-search) nil :insert)
   (keyamp--set-map x '(vterm-up vterm-down) :command))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . vterm-up)
       (next-line     . vterm-history-search)))
   (keyamp--map-tab x change-wd)
   (keyamp--map x
     '(("v" . paste-or-paste-prev)    ("м" . paste-or-paste-prev)
       ("'" . alternate-buf-or-frame) ("э" . alternate-buf-or-frame)
       ("[" . alternate-buffer)       ("х" . alternate-buffer)
       ("," . next-window-or-frame)   ("б" . next-window-or-frame)
       ("5" . terminal)               ("0" . eshell)))

   (keyamp--set-map x
     '(vterm-send-return term-interrupt-subjob) nil :insert)
   (keyamp--set-map-hook x '(vterm-mode-hook) nil :insert)

   (advice-add 'keyamp-input-timer-payload :after
               (lambda () "vterm transient"
                 (when (eq major-mode 'vterm-mode)
                   (set-transient-map x)
                   (keyamp-indicate-repeat)
                   (setq this-command 'keyamp--repeat-dummy))))

  (add-hook 'vterm-mode-hook
            (lambda (&rest r) "`keyamp-start-input-timer'"
              (add-hook 'post-command-hook 'keyamp-start-input-timer)))

  (advice-add-macro '(vterm-send-return term-interrupt-subjob)
   :after (lambda (&rest r) "`keyamp-start-input-timer'"
            (add-hook 'post-command-hook 'keyamp-start-input-timer)))))

(with-eval-after-load 'info
  (keyamp--remap Info-mode-map
    '((keyamp-insert   . Info-follow-nearest-node)
      (open-line       . Info-backward-node)
      (newline         . Info-forward-node)
      (undo            . Info-up)
      (delete-backward . Info-next-reference)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(open-line . newline))
   (keyamp--set-map x '(Info-backward-node Info-forward-node))))

(with-eval-after-load 'help-mode
  (keyamp--remap help-mode-map
    '((undo            . backward-button)
      (delete-backward . forward-button)
      (open-line       . help-go-back)
      (newline         . help-go-forward)))

  ;; DEL L and DEL/SPC to describe foo, next RET jump to source file or hit
  ;; DEL/SPC to continue move by buttons.
  (advice-add 'other-window :after
              (lambda (&rest r) "help mode"
                (when (and (eq major-mode 'help-mode) (= (point) 1))
                  (setq this-command 'forward-button)
                  (forward-button 1)))))

(with-eval-after-load 'gnus-topic
  (keyamp--map-tab gnus-topic-mode-map gnus-topic-goto-next-topic-line)
  (keyamp--map gnus-topic-mode-map '(("<double-mouse-1>" . gnus-topic-select-group)))

  (keyamp--remap gnus-topic-mode-map
    '((previous-line        . up-line)
      (next-line            . down-line)
      (keyamp-insert        . gnus-topic-select-group)
      (end-of-lyne          . gnus-topic-goto-next-topic-line)
      (beg-of-line          . gnus-topic-goto-prev-topic-line)
      (end-of-line-or-block . gnus-topic-goto-next-topic-line)
      (beg-of-line-or-block . gnus-topic-goto-prev-topic-line)
      (make-frame-command   . delete-frame)
      (left-char            . other-window)
      (right-char           . gnus-topic-select-group)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x '((previous-line . up-line) (next-line . down-line)))
   (keyamp--set-map x '(gnus-topic-select-group)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line        . gnus-topic-goto-prev-topic-line)
       (next-line            . gnus-topic-goto-next-topic-line)
       (beg-of-line          . gnus-beg-of-line-or-buffer)
       (end-of-lyne          . gnus-end-of-line-or-buffer)
       (end-of-line-or-block . gnus-beg-of-line-or-buffer)
       (beg-of-line-or-block . gnus-end-of-line-or-buffer)))

   (keyamp--set-map x
     '(gnus-topic-goto-prev-topic-line gnus-topic-goto-next-topic-line
       gnus-beg-of-line-or-buffer      gnus-end-of-line-or-buffer) nil nil nil 2)))

(with-eval-after-load 'gnus-group
  (keyamp--remap gnus-group-mode-map
    '((backward-del-word      . sun-moon)
      (undo                   . delete-or-split-window)
      (del-word               . gnus-group-enter-server-mode)
      (cut-text-block         . calc)
      (jump-mark              . view-echo-area-messages)
      (open-line              . prev-user-buffer)
      (delete-backward        . gnus-group-get-new-news)
      (newline                . next-user-buffer)
      (set-mark-command       . new-empty-buffer)
      (cut-line-or-selection  . prev-eww-buffer)
      (copy-line-or-selection . agenda)
      (paste-or-paste-prev    . tasks)
      (backward-left-bracket  . downloads)
      (forward-right-bracket  . player)
      (kmacro-helper          . config)
      (copy-to-register       . sql))))

(with-eval-after-load 'gnus-art
  (keyamp--remap gnus-mime-button-map
    '((keyamp-insert . gnus-article-press-button)))
  (keyamp--remap gnus-article-mode-map
    '((undo                . backward-button)
      (delete-backward     . forward-button)
      (make-frame-command  . delete-frame)))

  (advice-add 'other-window :after
              (lambda (&rest r)
                (when (and (eq major-mode 'gnus-article-mode) (= (point) 1))
                  (setq this-command 'forward-button)
                  (forward-button 1)))))

(with-eval-after-load 'gnus-sum
  (keyamp--map gnus-summary-mode-map
    '(("C-h"              . gnus-summary-delete-article)
      ("<double-mouse-1>" . gnus-summary-scroll-up)))

  (keyamp--remap gnus-summary-mode-map
    '((previous-line         . up-line)
      (next-line             . down-line)
      (keyamp-insert         . gnus-summary-scroll-up)
      (open-line             . gnus-summary-prev-group)
      (newline               . gnus-summary-next-group)
      (left-char             . gnus-summary-next-group) ; touch reader
      (right-char            . gnus-summary-prev-group) ; vise versa
      (paste-from-register-1 . gnus-summary-save-parts)
      (save-buffer           . gnus-summary-save-parts)
      (make-frame-command    . delete-frame)))

  (advice-add 'delete-frame :after
              (lambda (&rest r) "kill gnus along with frame"
                (if (get-buffer "*Group*") (kill-buffer "*Group*"))))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(up-line . down-line))
   (keyamp--remap x
     '((open-line . gnus-summary-prev-group) (newline . gnus-summary-next-group)))
   (keyamp--map-tab x half-page-forward)
   (keyamp--set-map x
     '(gnus-summary-prev-group gnus-summary-next-group gnus-delete-window-article))
   (keyamp--set-map-hook x '(gnus-summary-prepared-hook)))

  (with-sparse-keymap-x
   ;; Press RET to open an article then RET again to close it.
   ;; SPC/DEL to switch to the article then SPC/DEL to continue move by links.
   (keyamp--map-leaders x '(other-window . other-window))
   (keyamp--remap x '((keyamp-insert . gnus-delete-window-article)))
   (keyamp--set-map x '(gnus-summary-scroll-up))))

(with-eval-after-load 'gnus-srvr
  (keyamp--remap gnus-server-mode-map
    '((keyamp-insert   . gnus-server-read-server)
      (delete-backward . gnus-server-exit)))
  (keyamp--remap gnus-browse-mode-map
    '((keyamp-insert . gnus-browse-select-group))))

(with-eval-after-load 'recentf
  (keyamp--remap recentf-dialog-mode-map ; remap numbers to Engineer Engram
    '((keyamp-escape           . recentf-cancel-dialog)
      (copy-to-register        . recentf-open-most-recent-file-0)
      (kmacro-helper           . recentf-open-most-recent-file-1)
      (jump-to-register        . recentf-open-most-recent-file-2)
      (append-to-register-1    . recentf-open-most-recent-file-3)
      (toggle-case-fold-search . recentf-open-most-recent-file-4)
      (kmacro-play             . recentf-open-most-recent-file-5)
      (eshell                  . recentf-open-most-recent-file-6)
      (kmacro-record           . recentf-open-most-recent-file-7)
      (pass                    . recentf-open-most-recent-file-8)
      (terminal                . recentf-open-most-recent-file-9))))

(with-sparse-keymap-x
 (keyamp--remap x
   '((copy-to-register        . radio-channel-0)
     (kmacro-helper           . radio-channel-1)
     (jump-to-register        . radio-channel-2)
     (append-to-register-1    . radio-channel-3)
     (toggle-case-fold-search . radio-channel-4)
     (kmacro-play             . radio-channel-5)
     (eshell                  . radio-channel-6)
     (kmacro-record           . radio-channel-7)
     (pass                    . radio-channel-8)
     (terminal                . radio-channel-9)
     (open-line               . radio-prev)
     (newline                 . radio-next)))
 (keyamp--set-map x
   '(radio radio-next radio-prev radio-channel-0
     radio-channel-1 radio-channel-2 radio-channel-3
     radio-channel-4 radio-channel-5 radio-channel-6
     radio-channel-7 radio-channel-8 radio-channel-9)))

(with-eval-after-load 'snake
  (keyamp--remap snake-mode-map
    '((keyamp-escape   . snake-pause-game)
      (keyamp-insert   . snake-pause-game)
      (delete-backward . snake-move-up)
      (next-line       . snake-move-down)))
  (keyamp--remap snake-null-map
    '((keyamp-escape   . snake-start-game)
      (keyamp-insert   . snake-start-game)))
  (with-sparse-keymap-x
   (keyamp--map-leaders x '(snake-move-left . snake-move-right))
   (keyamp--set-map x
     '(snake-start-game snake-pause-game
       snake-move-left  snake-move-right
       snake-move-down  snake-move-up))
   (keyamp--set-map-hook x '(snake-mode-hook))))

(with-eval-after-load 'tetris
  (keyamp--remap tetris-mode-map
    '((keyamp-escape    . tetris-pause-game)
      (delete-backward  . tetris-rotate-prev)
      (alternate-buffer . tetris-rotate-prev)
      (newline          . tetris-rotate-next)
      (next-user-buffer . tetris-rotate-next)
      (next-line        . tetris-move-bottom)
      (backward-char    . tetris-move-down)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(tetris-move-left . tetris-move-right))
   (keyamp--set-map x
     '(tetris-start-game  tetris-pause-game
       tetris-move-left   tetris-move-right
       tetris-rotate-prev tetris-rotate-next
       tetris-move-bottom tetris-move-down))))

(with-eval-after-load 'js-mode
  (keyamp--map-tab js-mode-map js-leader-map)
  (keyamp--map-tab (define-prefix-command 'js-leader-map) js-complete-or-indent)
  (keyamp--map js-leader-map
    '(("h" . typescript-compile-file) ("," . js-eval-line) ("." . js-eval-region)))
  (keyamp--remap js-mode-map '((terminal-split . js-format-buffer))))

(with-eval-after-load 'css-mode
  (keyamp--map-tab css-mode-map css-leader-map)
  (keyamp--map-tab (define-prefix-command 'css-leader-map) css-complete-or-indent)
  (keyamp--map css-leader-map
    '(("'" . css-hex-color-to-hsl)     ("a" . css-complete-symbol)
      ("h" . css-format-compact)       ("p" . css-format-compact-buffer)
      ("o" . css-format-expand-buffer) ("k" . css-format-expand)))
  (keyamp--remap css-mode-map '((open-line . css-smart-newline))))

(with-eval-after-load 'html-mode
  (keyamp--map-tab html-mode-map html-leader-map)
  (keyamp--map html-mode-map
    '(("RET" . html-open-local-link) ("<return>" . html-open-local-link)
      ("C-r" . html-open-in-browser)))
  (keyamp--map-tab (define-prefix-command 'html-leader-map) html-insert-tag)
  (keyamp--map html-leader-map
    '(("RET" . html-insert-br-tag) ("<return>" . html-insert-br-tag)

      ("<left>"  . html-prev-opening-tag)
      ("<right>" . html-next-opening-tag)
      ("<down>"  . html-goto-matching-tag)

      ("@" . html-encode-ampersand-entity)
      ("$" . html-percent-decode-url)
      ("&" . html-decode-ampersand-entity)
      ("q" . html-make-citation)

      ("w"   . nil) ; required
      ("w ," . html-rename-source-file-path)
      ("w h" . html-resize-img)
      ("w q" . html-image-path-to-figure-tag)
      ("w j" . html-image-to-link)
      ("w w" . html-image-to-img-tag)
      ("w c" . html-convert-to-jpg)
      ("w o" . html-move-image-file)

      ("e" . html-remove-tag-pair)
      ("r" . html-mark-unicode)
      ("y" . html-lines-to-table)
      ("u" . html-emacs-to-windows-kbd-notation)
      ("i" . html-all-urls-to-link)
      ("o" . html-insert-pre-tag)
      ("[" . html-percent-encode-url)

      ("a i" . html-promote-header)
      ("a k" . html-demote-header)
      ("a e" . html-remove-tags)
      ("a q" . html-compact-def-list)
      ("a ." . html-remove-list-tags)
      ("a f" . html-remove-paragraph-tags)
      ("a ," . html-format-to-multi-lines)
      ("a l" . html-disable-script-tag)
      ("a a" . html-update-title-h1)
      ("a y" . html-remove-table-tags)
      ("a h" . html-change-current-tag)

      ("s" . html-html-to-text)
      ("d" . html-select-element)
      ("f" . html-blocks-to-paragraph)
      ("h" . html-lines-to-list)
      ("j" . html-any-to-link)

      ("k"   . nil)
      ("k e" . html-dehtmlize-pre-tags)
      ("k h" . html-bracket-to-markup)
      ("k j" . html-pre-tag-to-new-file)
      ("k ;" . html-htmlize-region)
      ("k ," . html-rehtmlize-precode-buffer)
      ("k k" . html-toggle-syntax-color-tags)

      ("l" . html-insert-date-section)
      ("x" . html-lines-to-def-list)
      ("c" . html-join-tags)
      ("v" . html-keyboard-shortcut-markup)
      ("b" . html-make-link-defunct)

      ("m i" . html-ampersand-chars-to-unicode)
      ("m h" . html-clone-file-in-link)
      ("m d" . html-url-to-dated-link)
      ("m w" . html-url-to-iframe-link)
      ("m j" . html-local-links-to-relative-path)
      ("m f" . html-pdf-path-to-embed)
      ("m ," . html-named-entity-to-char)
      ("m k" . html-local-links-to-fullpath)

      (","   . html-extract-url)
      ("."   . html-word-to-anchor-tag)
      ("/ ," . html-open-in-brave)
      ("/ l" . html-open-in-safari))))

(with-eval-after-load 'find-replace
  (keyamp--map-tab find-output-mode-map find-next-match)
  (keyamp--map find-output-mode-map
    '(("<backtab>" . find-previous-match) ("S-<tab>" . find-previous-match)))
  (keyamp--remap find-output-mode-map '((keyamp-insert . find--jump-to-place)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(backward-char . forward-char))
   (keyamp--remap x
     '((previous-line . find-previous-file)  (next-line    . find-next-file)
       (backward-char . find-previous-match) (forward-char . find-next-match)))
   (keyamp--set-map x
     '(find-next-match     find-previous-file
       find-previous-match find-next-file))))

(with-eval-after-load 'emacs-lisp-mode
  (keyamp--map-tab emacs-lisp-mode-map emacs-lisp-indent)
  (keyamp--remap emacs-lisp-mode-map
    '((reformat-lines . emacs-lisp-remove-paren-pair))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(backward-char . forward-char))
 (keyamp--remap x
   '((backward-char . flymake-goto-prev-error)
     (forward-char  . flymake-goto-next-error)
     (back-word     . flymake-goto-prev-error)
     (forw-word     . flymake-goto-next-error)))
 (keyamp--set-map x '(flymake-goto-prev-error flymake-goto-next-error)
                    nil nil nil 1))

(with-eval-after-load 'python-mode
  (keyamp--map-tab python-mode-map python-indent-or-complete)
  (keyamp--map python-mode-map
    '(("RET" . python-return-and-indent) ("<return>"  . python-return-and-indent)
      ("S-<tab>" . python-de-indent)     ("<backtab>" . python-de-indent)))

  (keyamp--remap python-mode-map
    '((newline        . python-return-and-indent)
      (terminal-split . python-format-buffer))))

(with-eval-after-load 'go-ts-mode
  (keyamp--map-tab go-ts-mode-map company-manual-begin)
  (keyamp--remap go-ts-mode-map
    '((describe-foo-at-point       . xref-find-definitions)
      (describe-variable           . xref-find-references)
      (mark-defun                  . go-mark-defun)
      (eval-defun                  . flymake-show-project-diagnostics)
      (elisp-eval-region-or-buffer . test)
      (eval-last-sexp              . server)
      (reformat-lines              . eglot-reconnect))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(xref-go-back . xref-find-definitions))
 (keyamp--set-map x '(xref-go-back xref-find-definitions)))

(with-eval-after-load 'xref
  (keyamp--remap xref--xref-buffer-mode-map
    '((keyamp-insert . xref-show-location-at-point))))

(with-eval-after-load 'sh-script
  (keyamp--map-tab bash-ts-mode-map indent-for-tab-command)
  (keyamp--map-tab sh-mode-map indent-for-tab-command))

(with-eval-after-load 'sqlite-mode
  (keyamp--remap sqlite-mode-map
    '((keyamp-insert   . sqlite-mode-list-data)
      (delete-backward . sqlite-mode-delete)
      (newline         . sqlite-mode-list-columns)
      (open-line       . sqlite-mode-list-tables))))

(with-eval-after-load 'sql
  (keyamp--remap sql-mode-map
    '((eval-defun . exec-query) (sql . toggle-sql-type)))
  (with-sparse-keymap-x
   (keyamp--remap x '((copy-to-register . toggle-sql-type)))
   (keyamp--set-map x '(sql toggle-sql-type exec-query))))

(with-eval-after-load 'speedbar
  (keyamp--remap speedbar-mode-map
    '((newline            . speedbar-refresh)))
  (keyamp--map speedbar-file-key-map
    '(("<mouse-2>"        . speedbar-toggle-line-expansion)
      ("<double-mouse-1>" . speedbar-edit-line)))
  (keyamp--remap speedbar-file-key-map
    '((keyamp-insert      . speedbar-toggle-line-expansion)
      (undo               . speedbar-up-directory)
      (delete-backward    . speedbar-edit-line))))

(with-eval-after-load 'calendar
  (keyamp--remap calendar-mode-map
    '((beg-of-line     . calendar-scroll-right)
      (end-of-lyne     . calendar-scroll-left)
      (delete-backward . calendar-goto-today)
      (keyamp-insert   . org-calendar-select)
      (undo            . delete-or-split-window)))
  (with-sparse-keymap-x
   (keyamp--map-leaders x '(calendar-scroll-right . calendar-scroll-left))
   (keyamp--map-tab x calendar-other-month)
   (keyamp--set-map x
     '(calendar-scroll-left calendar-scroll-right calendar-goto-today))))

(with-eval-after-load 'simple
  (keyamp--remap messages-buffer-mode-map
    '((undo                  . delete-or-split-window)
      (delete-backward       . alternate-buffer)
      (open-line             . prev-user-buffer)
      (newline               . next-user-buffer)
      (paste-or-paste-prev   . tasks)
      (previous-line         . up-line)
      (next-line             . down-line)
      (backward-del-word     . sun-moon)
      (cut-text-block        . calc)
      (cut-line-or-selection . prev-eww-buffer)
      (backward-left-bracket . downloads)
      (forward-right-bracket . player)
      (kmacro-helper         . config)
      (copy-to-register      . sql)))
  (keyamp--remap special-mode-map
    '((undo            . delete-or-split-window)
      (delete-backward . alternate-buffer)
      (open-line       . prev-user-buffer)
      (newline         . next-user-buffer))))

(with-eval-after-load 'calc
  (add-hook 'minibuffer-setup-hook
            (lambda () "activate insert mode for calc input"
              (when (or (eq real-this-command 'calcDigit-start)
                        (eq real-this-command 'calc-execute-extended-command)
                        (eq real-this-command 'calc-algebraic-entry))
                (keyamp-insert)
                (if keyamp--deactivate-repeat-mode-fun
                    (funcall keyamp--deactivate-repeat-mode-fun))
                (setq this-command 'keyamp-insert))) 96) ; very end
  (advice-add 'calcDigit-start :after 'keyamp-insert)
  (advice-add 'calcDigit-start :after
              (lambda (&rest r) "`keyamp-start-input-timer'"
                (add-hook 'post-command-hook 'keyamp-start-input-timer))))

  (advice-add-macro
    '(calc-plus  calc-minus
      calc-times calc-divide
      calc-mod   calc-inv
      calc-power calc-enter) :after 'keyamp-start-input-timer)

(with-eval-after-load 'calc-ext
  (keyamp--remap calc-mode-map
    '((delete-backward     . calc-pop)
      (undo                . calc-undo)
      (open-line           . calc-roll-down)
      (newline             . calc-algebraic-entry)
      (paste-or-paste-prev . calc-yank)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(undo . delete-backward))
   (keyamp--remap x '((delete-backward . calc-redo)))
   (keyamp--set-map x '(calc-undo calc-redo))
   (advice-add 'keyamp-input-timer-payload :after
               (lambda () "calc mode transient"
                 (when (eq major-mode 'calc-mode)
                   (set-transient-map x)
                   (keyamp-indicate-repeat)
                   (setq this-command 'keyamp--repeat-dummy))))))



(defconst keyamp-screen-commands-hash
  #s(hash-table test equal data
                (agenda                           t
                 async-shell-command              t
                 config                           t
                 delete-other-windows             t
                 delete-window                    t
                 downloads                        t
                 exec-query                       t
                 find-next-dir-file               t
                 find-prev-dir-file               t
                 next-buffer                      t
                 next-eww-buffer                  t
                 next-proj-buffer                 t
                 next-user-buffer                 t
                 open-file-at-cursor              t
                 previous-buffer                  t
                 prev-eww-buffer                  t
                 prev-proj-buffer                 t
                 prev-user-buffer                 t
                 run-current-file                 t
                 save-close-cur-buf               t
                 server                           t
                 split-window-below               t
                 sun-moon                         t
                 sync                             t
                 tasks                            t
                 test                             t
                 view-echo-area-messages          t)))

(defconst keyamp-edit-commands-hash
  #s(hash-table test equal data
                (clean-whitespace                 t
                 cut-line-or-selection            t
                 delete-backward                  t
                 delete-forward-char              t
                 backward-del-word                t
                 del-word                         t
                 dired-toggle-mark                t
                 ibuffer-do-delete                t
                 insert-date                      t
                 insert-space-before              t
                 kill-region                      t
                 org-shiftdown                    t
                 org-shiftup                      t
                 shrink-whitespaces               t
                 toggle-letter-case               t
                 toggle-comment                   t
                 todo                             t
                 undo                             t)))

(defconst keyamp-repeat-commands-hash
  #s(hash-table test equal data
                (backward-punct                   t
                 backward-button                  t
                 back-word                        t
                 beg-of-line-or-block             t
                 beg-of-line-or-buffer            t
                 backward-left-bracket            t
                 calc-undo                        t
                 calc-redo                        t
                 calendar-goto-today              t
                 calendar-scroll-left             t
                 calendar-scroll-right            t
                 company-manual-begin             t
                 company-next-page                t
                 company-select-previous          t
                 company-select-next              t
                 company-previous-page            t
                 copy-line-or-selection           t
                 dired-mark                       t
                 dired-unmark                     t
                 down-line                        t
                 emms-pause                       t
                 emms-seek-backward-or-previous   t
                 emms-seek-forward-or-next        t
                 emms-seek-backward               t
                 emms-seek-forward                t
                 emms-random                      t
                 emms-playlist-mode-play-smart    t
                 end-of-line-or-block             t
                 end-of-line-or-buffer            t
                 eshell-next-input                t
                 eshell-previous-input            t
                 eshell-search-input              t
                 extend-selection                 t
                 find-next-file                   t
                 find-next-match                  t
                 find-previous-file               t
                 find-previous-match              t
                 flymake-goto-next-error          t
                 flymake-goto-prev-error          t
                 forward-button                   t
                 forward-punct                    t
                 forward-right-bracket            t
                 forw-word                        t
                 gnus-delete-window-article       t
                 gnus-beg-of-line-or-buffer       t
                 gnus-end-of-line-or-buffer       t
                 gnus-summary-next-group          t
                 gnus-summary-prev-group          t
                 gnus-summary-scroll-up           t
                 gnus-topic-goto-next-topic-line  t
                 gnus-topic-goto-prev-topic-line  t
                 gnus-topic-select-group          t
                 ibuffer-backward-filter-group    t
                 ibuffer-forward-filter-group     t
                 ibuffer-toggle-filter-group      t
                 icomplete-backward-completions   t
                 icomplete-forward-completions    t
                 ido-next-match                   t
                 ido-prev-match                   t
                 isearch-repeat-backward          t
                 isearch-repeat-forward           t
                 isearch-ring-advance             t
                 isearch-ring-retreat             t
                 isearch-yank-kill                t
                 keyamp--repeat-dummy             t
                 left-char                        t
                 next-line-or-history-element     t
                 jump-mark                        t
                 previous-line-or-history-element t
                 recenter-top-bottom              t
                 right-char                       t
                 scroll-down-command              t
                 scroll-up-command                t
                 select-block                     t
                 isearch-cur-word-forward         t
                 isearch-cur-word-backward        t
                 radio-prev                       t
                 radio-next                       t
                 select-line                      t
                 select-text-in-quote             t
                 shrink-window                    t
                 enlarge-window                   t
                 text-scale-decrease              t
                 text-scale-increase              t
                 text-scale-reset                 t
                 toggle-ibuffer                   t
                 up-line                          t
                 half-page-backward               t
                 half-page-forward                t
                 vterm-down                       t
                 vterm-send-return                t
                 vterm-up                         t
                 xref-go-back                     t
                 xref-find-definitions            t)))



(defvar keyamp-insert-p t   "Non-nil means insert is on.")
(defvar keyamp-repeat-p nil "Non-nil means repeat is on.")

(defvar keyamp--deactivate-command-mode-fun nil)

(defun keyamp-command-init ()
  "Set command mode."
  (if keyamp-repeat-p
      (setq keyamp-repeat-p nil))
  (if keyamp--deactivate-repeat-mode-fun
      (funcall keyamp--deactivate-repeat-mode-fun))
  (when keyamp-insert-p
    (setq keyamp-insert-p nil)
    (push-mark (point) t))
  (if keyamp--deactivate-command-mode-fun
      (funcall keyamp--deactivate-command-mode-fun))
  (setq keyamp--deactivate-command-mode-fun
        (set-transient-map keyamp-command-map (lambda () t)))
  (keyamp-indicate-mode))

(defun keyamp-insert-init (&rest r)
  "Enter insert mode."
  (setq keyamp-insert-p t)
  (funcall keyamp--deactivate-command-mode-fun))

(defun keyamp-set-var-karabiner (var val)
  "Set karabiner VAR to VAL via shell command."
  (call-process keyamp-karabiner-cli nil 0 nil
                "--set-variables" (concat "{\"" var "\":" val "}")))

(defconst keyamp-karabiner-insert-mode "insert mode activated"
  "Karabiner keyamp insert mode variable.")

(when (file-exists-p keyamp-karabiner-cli)
  (add-hook 'keyamp-insert-hook
            (lambda () (keyamp-set-var-karabiner keyamp-karabiner-insert-mode "1")))
  (add-hook 'keyamp-command-hook
            (lambda () (keyamp-set-var-karabiner keyamp-karabiner-insert-mode "0"))))

(defun keyamp-command ()
  "Activate command mode."
  (interactive)
  (keyamp-command-init)
  (run-hooks 'keyamp-command-hook))

(defun keyamp-insert ()
  "Activate insert mode."
  (interactive)
  (keyamp-insert-init)
  (run-hooks 'keyamp-insert-hook))

(defun keyamp-command-if-insert (&rest r)
  "Activate command mode if insert mode."
  (if keyamp-insert-p (keyamp-command)))

(setq-default cursor-in-non-selected-windows nil)

(defvar keyamp-indicate-repeat-timer nil "Indicate repeat timer.")
(defconst keyamp-indicate-repeat-delay 0.5 "Cursor type change delay.")
(defconst keyamp-repeat-cursor-type  'hollow "Repeat cursor type.")
(defconst keyamp-command-cursor-type 'box "Command cursor type.")
(defconst keyamp-screen-cursor-type  nil "Screen cursor type.")
(defconst keyamp-edit-cursor-type    '(bar . 2)  "Edit cursor type.")
(defconst keyamp-insert-cursor-type  '(hbar . 2) "Insert cursor type.")

(defun keyamp-cancel-indicate-repeat-timer ()
  "Cancel timer which delays indicate repeat by changing cursor type."
  (when (timerp keyamp-indicate-repeat-timer)
    (cancel-timer keyamp-indicate-repeat-timer)
    (setq keyamp-indicate-repeat-timer t)))

(defun keyamp-change-cursor-type (Type)
  "Change cursor type."
  (when (display-graphic-p)
    (keyamp-cancel-indicate-repeat-timer)
    (modify-all-frames-parameters `((cursor-type . ,Type)))))

(defun keyamp-indicate-repeat ()
  "Indicate repeat transient is active. Cursor type change runs after first
repeat command exactly after a delay even if there more repeat commands follows."
  (setq mode-line-front-space keyamp-repeat-indicator)
  (when (display-graphic-p)
    (unless (memq (alist-get 'cursor-type default-frame-alist)
                  `(,keyamp-command-cursor-type ,keyamp-repeat-cursor-type))
      (keyamp-change-cursor-type keyamp-command-cursor-type))
    (unless (timerp keyamp-indicate-repeat-timer)
      (setq keyamp-indicate-repeat-timer
            (run-with-timer keyamp-indicate-repeat-delay nil
                            'keyamp-change-cursor-type keyamp-repeat-cursor-type)))))

(defun indicate-screen ()
  "Indicate screen repeat transient is active."
  (setq mode-line-front-space keyamp-screen-indicator)
  (keyamp-change-cursor-type keyamp-screen-cursor-type))

(defun keyamp-indicate-edit ()
  "Indicate edit repeat transient is active."
  (setq mode-line-front-space keyamp-insert-indicator)
  (keyamp-change-cursor-type keyamp-edit-cursor-type))

(defun keyamp-indicate-insert ()
  "Indicate insert is active."
  (setq mode-line-front-space keyamp-insert-indicator)
  (keyamp-change-cursor-type keyamp-insert-cursor-type))

(defun keyamp-indicate-command ()
  "Indicate command is active."
  (setq mode-line-front-space keyamp-command-indicator)
  (keyamp-change-cursor-type keyamp-command-cursor-type))

(defun keyamp-indicate-mode ()
  "Keyamp indicate mode. Run with `post-command-hook'."
  (cond
   ((equal prefix-arg (list 4)) ; universal-argument
    (keyamp-indicate-edit))
   ((gethash this-command keyamp-screen-commands-hash)
    (indicate-screen)
    (setq keyamp-repeat-p t))
   ((and (or (eq real-this-command 'repeat)
             (gethash this-command keyamp-repeat-commands-hash))
         (not keyamp-insert-p))
    (keyamp-indicate-repeat)
    (setq keyamp-repeat-p t))
   ((and (gethash this-command keyamp-edit-commands-hash)
         (not keyamp-insert-p))
    (keyamp-indicate-edit)
    (setq keyamp-repeat-p t))
   (keyamp-insert-p (keyamp-indicate-insert))
   (t
    (keyamp-indicate-command)
    (setq keyamp-repeat-p nil)))
  (when (and (not (eq this-command last-command))
             (not (display-graphic-p)))
    (force-mode-line-update t)))

(defun keyamp-escape ()
  "Return to command mode, clear selection or quit minibuffer."
  (interactive)
  (cond
   (keyamp-repeat-p   (keyamp-command))
   (keyamp-insert-p   (keyamp-command))
   ((region-active-p) (deactivate-mark))
   ((minibufferp)     (abort-recursive-edit))))



;;;###autoload
(define-minor-mode keyamp
  "Keyboard Amplifier."
  :global t
  :keymap keyamp-map
  (when keyamp
    (add-hook 'minibuffer-exit-hook  'keyamp-command)
    (add-hook 'isearch-mode-end-hook 'keyamp-command)
    (add-hook 'debugger-mode-hook    'keyamp-command)
    (add-hook 'post-command-hook     'keyamp-indicate-mode)
    (add-hook 'keyamp-insert-hook    'keyamp-cancel-repeat-mode-idle-timer)
    (add-hook 'keyamp-command-hook   'keyamp-cancel-repeat-mode-idle-timer)
    (add-hook 'isearch-mode-hook     'keyamp-cancel-repeat-mode-idle-timer)
    (add-function :after after-focus-change-function #'keyamp-command)
    (keyamp-catch-tty-ESC)
    (keyamp-command)
    (run-with-timer keyamp-defer-load-time nil
                    'keyamp-map-input-source 'russian-computer)
    (run-with-timer keyamp-defer-load-time nil
                    'keyamp-push-quail-keyboard-layout)
    (setq keyamp-idle-timer
          (run-with-idle-timer keyamp-idle-timeout t 'keyamp-escape))))

(provide 'keyamp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical unresolved)
;; End:
;;; keyamp.el ends here
