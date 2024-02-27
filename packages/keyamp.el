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
;; positioning, cursor move and editing. Point color indicates
;; transient remap is active. ESDF and IJKL are mostly used, DEL/ESC
;; and RET/SPC control EVERYTHING. Home row and thumb cluster only.
;; Repeat mode turned on/off *automatically* either by post command
;; hook or with idle timer.

;; DEL and SPC are two leader keys, RET activates insert mode, ESC does
;; command one. Holding down each of the keys posts control sequence
;; depending on mode. Keyboard has SYMMETRIC layout: left side for
;; editing, «No» and «Escape» while right side for moving, «Yes» and
;; «Enter». Any Emacs major or minor mode could be remapped to fit the
;; model, find examples in the package.

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
  "If region is active, return its boundary, else same as `get-bounds-of-block'."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (get-bounds-of-block)))


;; cursor movement

(defun up-line-wrap ()
  "On the first line go to the last line of buffer."
  (if (= (line-number-at-pos) 1)
      (forward-line (count-lines (point-min) (point-max)))))

(defun down-line-wrap ()
  "On the last line go to the first line of buffer."
  (if (>= (line-number-at-pos) (+ 1 (count-lines (point-min) (point-max))))
      (forward-line (- (count-lines (point-min) (point-max))))))

(defalias 'up-line 'previous-line "For transient keymaps.")
(defalias 'down-line 'next-line "For transient keymaps.")

(advice-add 'up-line   :before 'up-line-wrap)
(advice-add 'down-line :after  'down-line-wrap)
(advice-add 'up-line   :after (lambda (&rest r) (beginning-of-visual-line)))
(advice-add 'down-line :after (lambda (&rest r) (beginning-of-visual-line)))

(defun pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'."
  (interactive)
  (set-mark-command t)
  (setq this-command 'pop-local-mark-ring)
  (message "%s" "Mark pop")) ; same usual messages suppressed

(defun set-mark-deactivate-mark ()
  "Set the mark where point is, and deactivate it."
  (interactive)
  (set-mark-command nil)
  (deactivate-mark)
  (message "%s" "Mark set"))

(defun beg-of-line ()
  "Move cursor to beginning of line."
  (interactive)
  (let ((xp (point)))
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
          (beginning-of-line))))))

(defun beg-of-line-or-block ()
  "Move cursor to beginning of line or previous block.

• When called first time, move cursor to beginning of char in current
  line (if already, move to beginning of line);
• When called again, move cursor backward by jumping over any sequence
  of whitespaces containing 2 blank lines;
• If `visual-line-mode' is on, beginning of line means visual line."
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

(defalias 'end-of-lyne 'end-of-line "For use with transient keymaps.")

(defun end-of-line-or-block ()
  "Move cursor to end of line or next block.
• when called first time, move cursor to end of line;
• when called again, move cursor forward by jumping over any sequence
  of whitespaces containing 2 blank lines;
• if `visual-line-mode' is on, end of line means visual line."
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command)
          (equal "T" (this-command-keys)) ; shift k Engram
          (equal "t" (this-command-keys)) ; repeat
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
      (progn
        (if (not (eq major-mode 'eshell-mode))
            (progn
              (goto-char (point-max))
              (forward-line -1))))
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
    (progn
      (if (and (equal (point) (line-beginning-position))
               (> (current-column) 0))
          (progn
            (end-of-line-or-block))
        (progn
          (goto-char (point-max))
          (if (not (eq major-mode 'eshell-mode))
              (forward-line -1)))))))

(defvar keyext-brackets
  '("“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄"
    "‹›" "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "〘〙" "｢｣"
    "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩"
    "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞"
    "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧"
    "⸨⸩" "｟｠" "``")
"A list of strings, each element is a string of 2 chars, the left
bracket and a matching right bracket. Used by `select-text-in-quote'
and others.")

(defconst left-brackets
  (mapcar (lambda (x) (substring x 0 1)) keyext-brackets)
  "List of left bracket chars. Each element is a string.")

(defconst right-brackets
  (mapcar (lambda (x) (substring x 1 2)) keyext-brackets)
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
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

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

(defalias 'back-word 'backward-word "For use with transient keymaps.")
(defalias 'forw-word 'forward-word "For use with transient keymaps.")


;; editing commands

(defun copy-line-or-selection ()
  "Copy current line or selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer
(respects `narrow-to-region')."
  (interactive)
  (let ((inhibit-field-text-motion nil))
    (if current-prefix-arg
        (progn
          (copy-region-as-kill (point-min) (point-max)))
      (if (region-active-p)
          (progn
            (copy-region-as-kill (region-beginning) (region-end)))
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

(defun paste-or-paste-previous ()
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

(defconst show-kill-ring-separator
  "\n\n_____________________________________________________________________________\n\n"
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
          '(
            "\" double quote \""
            "' single quote '"
            "( paren )"
            "{ brace }"
            "[ square ]"
            "< greater >"
            "` emacs '"
            "` backtick `"
            "~ tilde ~"
            "= equal ="
            "[[ double square ]]"
            "“ curly double quote ”"
            "‘ curly single quote ’"
            "‹ french angle ›"
            "« french double angle »"
            "「 corner 」"
            "『 white corner 』"
            "【 lenticular 】"
            "〖 white lenticular 〗"
            "〈 angle 〉"
            "《 double angle 》"
            "〔 tortoise 〕"
            "〘 white tortoise 〙"
            "〚 white square 〛"
            "⦅ white paren ⦆"
            "⦃ WHITE CURLY BRACKET ⦄"
            "〈 pointing angle 〉"
            "⦑ ANGLE WITH DOT ⦒"
            "⧼ CURVED ANGLE ⧽"
            "⟦ math square ⟧"
            "⟨ math angle ⟩"
            "⟪ math DOUBLE ANGLE ⟫"
            "⟮ math FLATTENED PARENTHESIS ⟯"
            "⟬ math WHITE TORTOISE SHELL ⟭"
            "❛ HEAVY SINGLE QUOTATION MARK ORNAMENT ❜"
            "❝ HEAVY DOUBLE TURNED COMMA QUOTATION MARK ORNAMENT ❞"
            "❨ MEDIUM PARENTHESIS ORNAMENT ❩"
            "❪ MEDIUM FLATTENED PARENTHESIS ORNAMENT ❫"
            "❴ MEDIUM CURLY ORNAMENT ❵"
            "❬ MEDIUM POINTING ANGLE ORNAMENT ❭"
            "❮ HEAVY POINTING ANGLE QUOTATION MARK ORNAMENT ❯"
            "❰ HEAVY POINTING ANGLE ORNAMENT ❱"
            " none "
            )))
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
  (let* ((xskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕")
         (xp0 (point))
         (xp1 (if Begin
                  Begin
                (if (region-active-p)
                    (region-beginning)
                  (progn
                    (skip-chars-backward xskipChars
                                         (line-beginning-position)) (point)))))
         (xp2 (if End
                  End
                (if (region-active-p)
                    (region-end)
                  (progn (goto-char xp0)
                         (skip-chars-forward xskipChars
                                             (line-end-position)) (point)))))
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
        (progn
          (delete-blank-lines) ; (insert "\n")
          )))
     ((> xeol-count 2)
      (if xspace-neighbor-p
          (delete-spaces)
        (progn
          (goto-char xp2)
          (search-backward "\n")
          (delete-region xp1 (point))
          (insert "\n"))))
     (t (progn
          (message "Nothing done. Logic error 40873. Should not reach here"))))))

(defun fill-or-unfill ()
  "Reformat current block or selection to short/long line.
First call will break into multiple short lines. Repeated call toggles
between short and long lines.
This commands calls `fill-region' to do its work. Set `fill-column'
for short line length."
  (interactive)
  ;; This command symbol has a property “'longline-p”, the possible
  ;; values are t and nil. This property is used to easily determine
  ;; whether to compact or uncompact, when this command is called
  ;; again.
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
    ;; (setq (if MinLength MinLength))
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
  ;; This symbol has a property 'is-long-p, the possible values are t
  ;; and nil. This property is used to easily determine whether to
  ;; compact or uncompact, when this command is called again.
  (let (xisLong xp1 xp2)
    (setq xisLong (if (eq last-command this-command)
                      (get this-command 'is-long-p)
                    nil))
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

For example,

 cat
 dog
 cow

becomes

 \"cat\",
 \"dog\",
 \"cow\",

or

 (cat)
 (dog)
 (cow)

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
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "“curly double”"
            "‘curly single’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
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
        (let ((xskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
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
         (if (string-equal major-mode 'dired-mode)
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
         (progn
           (message "%s" (file-name-directory xfpath))
           (file-name-directory xfpath))
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
  (progn
    (copy-to-register ?1 (point-min) (point-min))
    (message "Clear register 1")))

(defun copy-to-register-1 ()
  "Copy current line or selection to register 1.
See also: `paste-from-register-1', `copy-to-register'."
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
         (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (copy-to-register ?1 xp1 xp2)
    (message "Copy to register 1")))

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
    (message "Append to register 1")))

(defun paste-from-register-1 ()
  "Paste text from register 1.
See also: `copy-to-register-1', `insert-register'."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun copy-rectangle-to-kill-ring (Begin End)
  "Copy region as column (rectangle region) to `kill-ring'.
See also: `kill-rectangle', `copy-to-register'."
  (interactive "r")
  (require 'rect)
  (kill-new (mapconcat #'identity (extract-rectangle Begin End) "\n")))


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
              '(
                "ISO date • 2018-04-12"
                "ISO full • 2018-04-12T22:46:11-07:00"
                "ISO space • 2018-04-12 22:46:11-07:00"
                "org mode <2018-04-12 Thu>"
                "all digits • 20180412224611"
                "date and digits • 2018-04-12_224611"
                "weekday • 2018-04-12 Thursday"
                "USA date + weekday • Thursday, April 12, 2018"
                "USA short + weekday • Thu, Apr 12, 2018"
                "USA mdy full • April 12, 2018"
                "USA mdy short • Apr 12, 2018"
                ))
           "org mode <2018-04-12 Thu>"
           )))
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
      ((string-match "^USA date + weekday" xstyle) (format-time-string "%A, %B %d, %Y"))
      ((string-match "^USA short + weekday" xstyle) (format-time-string "%a, %b %d, %Y"))
      ((string-match "^USA mdy full" xstyle) (format-time-string "%B %d, %Y"))
      ((string-match "^USA mdy short" xstyle) (format-time-string "%b %d, %Y"))
      (t (format-time-string "%Y-%m-%d"))))))

(defun insert-bracket-pair (LBracket RBracket &optional WrapMethod)
  "Insert brackets around selection, word, at point, and maybe move cursor
 in between.
LBracket and RBracket are strings. WrapMethod must be either `line' or
`block'. `block' means between empty lines.

• If there is a an active region, add brackets around region;
• if WrapMethod is `line', wrap around line;
• if WrapMethod is `block', wrap around block;
• if cursor is at beginning of line and its not empty line and contain
  at least 1 space, wrap around the line;
• if cursor is at end of a word or buffer, one of the following will happen:
  xyz▮ → xyz(▮)
  xyz▮ → (xyz▮)
• if in one of the lisp modes:
  wrap brackets around word if any. e.g. xy▮z → (xyz▮). Or just (▮)."
  (if (region-active-p)
      (progn
        (let ( (xp1 (region-beginning)) (xp2 (region-end)))
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
        (insert LBracket )
        (end-of-line)
        (insert  RBracket))
       ((and
         (or ; Cursor is at end of word or buffer i.e. xyz▮
          (looking-at "[^-_[:alnum:]]")
          (eq (point) (point-max)))
         (not (or
               (string-equal major-mode "elisp-mode")
               (string-equal major-mode "emacs-lisp-mode")
               (string-equal major-mode "lisp-mode")
               (string-equal major-mode "lisp-interaction-mode")
               (string-equal major-mode "common-lisp-mode")
               (string-equal major-mode "clojure-mode")
               (string-equal major-mode "clojure-mode")
               (string-equal major-mode "scheme-mode"))))
        (progn
          (setq xp1 (point) xp2 (point))
          (insert LBracket RBracket)
          (search-backward RBracket )))
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

(defun insert-paren ()                    (interactive) (insert-bracket-pair "(" ")"))
(defun insert-square-bracket ()           (interactive) (insert-bracket-pair "[" "]"))
(defun insert-brace ()                    (interactive) (insert-bracket-pair "{" "}"))

(defun insert-backtick-quote ()           (interactive) (insert-bracket-pair "`" "`"))
(defun insert-backtick-triple-quote ()    (interactive) (insert-bracket-pair "```\n" "\n```"))

(defun insert-double-curly-quote ()       (interactive) (insert-bracket-pair "“" "”"))
(defun insert-curly-single-quote ()       (interactive) (insert-bracket-pair "‘" "’"))
(defun insert-single-angle-quote ()       (interactive) (insert-bracket-pair "‹" "›"))
(defun insert-double-angle-quote ()       (interactive) (insert-bracket-pair "«" "»"))
(defun insert-ascii-double-quote ()       (interactive) (insert-bracket-pair "\"" "\""))
(defun insert-ascii-single-quote ()       (interactive) (insert-bracket-pair "'" "'"))
(defun insert-emacs-quote ()              (interactive) (insert-bracket-pair "`" "'"))
(defun insert-corner-bracket ()           (interactive) (insert-bracket-pair "「" "」"))
(defun insert-white-corner-bracket ()     (interactive) (insert-bracket-pair "『" "』"))
(defun insert-angle-bracket ()            (interactive) (insert-bracket-pair "〈" "〉"))
(defun insert-double-angle-bracket ()     (interactive) (insert-bracket-pair "《" "》"))
(defun insert-white-lenticular-bracket () (interactive) (insert-bracket-pair "〖" "〗"))
(defun insert-black-lenticular-bracket () (interactive) (insert-bracket-pair "【" "】"))
(defun insert-tortoise-shell-bracket ()   (interactive) (insert-bracket-pair "〔" "〕"))

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

(defun select-block ()
  "Select the current/next block plus 1 blank line.
If region is active, extend selection downward by block."
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
    (progn
      (push-mark (point) t nil)
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil 1)
        (goto-char (match-end 0)))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil 1))))

(defun select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line."
  (interactive)
  (push-mark (point) t nil)
  (if (region-active-p)
      (if visual-line-mode
          (let ((xp1 (point)))
            (end-of-visual-line 1)
            (when (eq xp1 (point))
              (end-of-visual-line 2)))
        (progn
          (forward-line 1)
          (end-of-line)))
    (if visual-line-mode
        (progn (beginning-of-visual-line)
               (push-mark (point) t t)
               (end-of-visual-line))
      (progn
        (push-mark (line-beginning-position) t t)
        (end-of-line)))))

(defun extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

When there is no selection,
• if cursor is on a any type of bracket (including parenthesis,
  quotation mark), select whole bracketed thing including bracket
• else, select current word.

When there is a selection, the selection extension behavior is still
experimental. But when cursor is on a any type of bracket
(parenthesis, quote), it extends selection to outer bracket."
  (interactive)
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
       ))))

(defun select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \" ` ' and anything in `brackets'.
Limit selection by current line.
This command ignores nesting. For example, if text is
    (a(b)c▮)
the selected char is “c”, not “a(b)c”."
  (interactive)
  (push-mark (point) t nil)
  (let ((xskipChars (concat "^\"`'" (mapconcat #'identity keyext-brackets ""))))
    (skip-chars-backward xskipChars)
    (push-mark (point) t t)
    (skip-chars-forward xskipChars)))


;; misc

(defun user-buffer-p ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it is not considered a user
buffer.
This function is used by buffer switching command and close buffer
command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”."
  (interactive)
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((string-equal major-mode "eww-mode") nil)
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "help-mode") nil)
   ((string-equal major-mode "doc-view-mode") nil)
   ((string-equal major-mode "diary-mode") nil)
   ((string-equal (buffer-name) "tetris-scores") nil)
   ((string-equal buffer-file-truename org-agenda-file-1) nil)
   ((string-equal buffer-file-truename org-agenda-file-2) nil)
   ((string-equal buffer-file-truename org-agenda-file-3) nil)
   ((string-match ".+em/project+." default-directory) nil)
   ((and buffer-file-truename
       (string-match ".sql" buffer-file-truename)) nil)
   (t t)))

(defun prev-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `user-buffer-p'."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (user-buffer-p))
          (progn
            (previous-buffer)
            (setq i (1+ i)))
        (progn
          (setq i 100))))))

(defun next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `user-buffer-p'."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (user-buffer-p))
          (progn
            (next-buffer)
            (setq i (1+ i)))
        (progn
          (setq i 100))))))

(defun project-buffer-p ()
  "Return t if current buffer is a project buffer, else nil."
  (interactive)
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((and (string-match ".+em/project+." default-directory)
         (not (string-equal major-mode "dired-mode"))) t)))

(defun prev-proj-buffer ()
  "Switch to the previous project buffer."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (project-buffer-p))
          (progn
            (previous-buffer)
            (setq i (1+ i)))
        (progn
          (setq i 100))))))

(defun next-proj-buffer ()
  "Switch to the next project buffer."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (project-buffer-p))
          (progn
            (next-buffer)
            (setq i (1+ i)))
        (progn
          (setq i 100))))))

(defun eww-reload-all ()
  "Reload all eww buffers."
  (interactive)
  (mapcar
   (lambda (xbuf)
     (with-current-buffer xbuf
       (when (eq major-mode 'eww-mode)
         (eww-reload))))
   (buffer-list)))

(defvar places-eww nil "Alist url to place.")

(defun eww-load-place-cache ()
  "Load places cache from file into `places-eww'."
  (setq places-eww
        (with-temp-buffer
          (insert-file-contents "~/.emacs.d/places-eww")
          (read (current-buffer)))))

(defun eww-load-place ()
  "Go to saved place in eww buffers."
  (interactive)
  (eww-load-place-cache)
  (when (eq major-mode 'eww-mode)
    (let ((xp1 (point))
          (xp2 (alist-get (replace-regexp-in-string
                           (getenv "HOME") "~"
                           (plist-get eww-data :url))
                          places-eww 1 nil 'string-equal)))
      (when (and (> (abs (- xp1 xp2)) 100) ; if position changed significantly
                 (> xp2 xp1))              ; allow offset forward only
        (goto-char xp2)))))

(defvar headers-eww nil "Alist url to header. Manual reference.")

(defun eww-update-header ()
  "Update header line for some buffers."
  (let ((xh (alist-get (replace-regexp-in-string (getenv "HOME") "~"
                                                 (plist-get eww-data :url))
                       headers-eww nil nil 'string-equal)))
    (when xh
      (setq header-line-format nil)
      (rename-buffer (concat "*" (string-trim xh) "*")))))

(defun eww-after-render ()
  "Make eww buffers readable. Run with `eww-after-render-hook'"
  (eww-readable)
  (eww-load-place)
  (eww-update-header)
  (if (and (display-graphic-p)
           (fboundp 'justify-buffer))
      (run-with-timer 0.1 nil 'justify-buffer)))

(defun eww-save-place-cache ()
  "Save places cache to file."
  (with-temp-buffer
    (if (> (length places-eww) 0) ; only if something
        (progn
          (pp places-eww (current-buffer))
          (write-region (point-min) (point-max)
                        "~/.emacs.d/places-eww" nil 'quiet)))))

(defun eww-save-place ()
  "Save place for eww buffer. Create new or update existing."
  (interactive)
  (when (eq major-mode 'eww-mode)
    (setq xurl (replace-regexp-in-string (getenv "HOME") "~"
                                         (plist-get eww-data :url)))
    (let ((xp1 (point))
          (xp2 (alist-get xurl places-eww 1 nil 'string-equal)))
      (if (assoc xurl places-eww)
          (if (and (> xp1 1000) ; only if point not in the beg
                   (> xp1 xp2)) ; allow offset forward only
              (setf (alist-get xurl
                               places-eww 1 nil 'string-equal) (point)))
        (push (cons xurl (point)) places-eww)))
    (eww-save-place-cache)
    (push-mark (point) t nil)))

(defun eww-buffer-p ()
  (interactive)
  (cond
   ((string-equal major-mode "eww-mode") t)
   (t nil)))

(defun prev-eww-buffer ()
  "Switch to the previous eww buffer."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (eww-buffer-p))
          (progn
            (previous-buffer)
            (setq i (1+ i)))
        (progn
          (setq i 100))))))

(defun next-eww-buffer ()
  "Switch to the next eww buffer."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (eww-buffer-p))
          (progn
            (next-buffer)
            (setq i (1+ i)))
        (progn
          (setq i 100))))))

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
                                (sort (directory-files (file-name-directory file) t nil t) 'string<)))
           (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                     (length files))))
      (find-file (nth pos files)))))

(defun find-prev-dir-file ()
  "Find the prev file (by name) in the current directory."
  (interactive)
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                (sort (directory-files (file-name-directory file) t nil t) 'string<)))
           (pos (mod (+ (cl-position file files :test 'equal) -1)
                     (length files))))
      (find-file (nth pos files)))))

(defun new-empty-buffer ()
  "Create a new empty buffer.
New buffer is named file, file<2>, etc.
On Emacs quit, if you want Emacs to prompt for save, set `buffer-offer-save'
to t.
It returns the buffer."
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

(declare-function minibuffer-keyboard-quit "delsel" ())
(declare-function org-edit-src-save "org-src" ())

(defun save-close-current-buffer ()
  "Save and close current buffer. If the buffer is not a file, save it
to `user-emacs-directory' temp and named
file_‹datetime›_‹randomhex›.txt."
  (interactive)
  (if (buffer-file-name)
      (when (buffer-modified-p) (save-buffer))
    (progn
      (when (user-buffer-p)
        (widen)
        (when (not (equal (point-max) 1))
          (write-file
           (format "%sfile-%s-%x.txt"
                   (concat user-emacs-directory "temp/")
                   (format-time-string "%Y%m%d-%H%M%S")
                   (random #xfffff)))))))
  (close-current-buffer))

(defun close-current-buffer ()
  "Close the current buffer.

Similar to `kill-buffer', with the following addition:
• Prompt user to save if the buffer has been modified even if the
  buffer is not associated with a file.
• If the buffer is editing a source code file in an `org-mode' file,
  prompt the user to save before closing.
• If the buffer is a file, add the path to the list `recently-closed-buffers'."
  (interactive)
  (let ((xisOrgModeSourceFile (string-match "^*Org Src" (buffer-name))))
    (if (active-minibuffer-window) ; if the buffer is minibuffer
        ;; (string-equal major-mode "minibuffer-inactive-mode")
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
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
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
                (cons (cons (buffer-name) (buffer-file-name)) recently-closed-buffers))
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

(defvar open-file-at-cursor-pre-hook nil
"Hook for `open-file-at-cursor'. Functions in the hook will be called
in order, each given the path as arg. The first return non-nil, its
value is given to `open-file-at-cursor' as input. This is useful for
transforming certain url into file path (your website url), so instead
of opening in browser, it opens in Emacs as file.")

(defun open-file-at-cursor ()
  "Open the file path under cursor.
If there is selection, use it for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›”
with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el”
for elisp files.

See also `open-file-at-cursor-pre-hook'.

This command is similar to `find-file-at-point' but without prompting
for confirmation."
  (interactive)
  (let* ((xinput
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ((xp0 (point)) xp1 xp2
                  (xpathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
              (skip-chars-backward xpathStops)
              (setq xp1 (point))
              (goto-char xp0)
              (skip-chars-forward xpathStops)
              (setq xp2 (point))
              (goto-char xp0)
              (buffer-substring-no-properties xp1 xp2))))
         xinput2 xpath
         )
    (setq xinput2
          (if (> (length open-file-at-cursor-pre-hook) 0)
              (let ((xx (run-hook-with-args-until-success 'open-file-at-cursor-pre-hook xinput)))
                (if xx xx xinput))
            xinput))
    (setq xpath (replace-regexp-in-string "^/C:/" "/" (replace-regexp-in-string "^file://" "" (replace-regexp-in-string ":\\'" "" xinput2))))
    (if (string-match-p "\\`https?://" xpath)
        (browse-url xpath)
      (progn ; not starting http://
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
                (progn ; open f.ts instead of f.js
                  (let ((xext (file-name-extension xpath))
                        (xfnamecore (file-name-sans-extension xpath)))
                    (if (and (string-equal xext "js")
                             (file-exists-p (concat xfnamecore ".ts")))
                        (find-file (concat xfnamecore ".ts"))
                      (if (> (length xpath) 0)
                          (find-file xpath)
                        (delete-other-windows)))))
              (if (file-exists-p (concat xpath ".el"))
                  (find-file (concat xpath ".el"))
                (delete-other-windows)))))))))

(defun open-http-at-cursor (fun &rest args)
  "Open http link at cursor."
  (let* ((xinput
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ((xp0 (point)) xp1 xp2
                  (xpathStops "^  \t\n\"`'|[]{}\\"))
              (skip-chars-backward xpathStops)
              (setq xp1 (point))
              (goto-char xp0)
              (skip-chars-forward xpathStops)
              (setq xp2 (point))
              (goto-char xp0)
              (buffer-substring-no-properties xp1 xp2))))
         xinput2 xpath)
    (setq xpath (replace-regexp-in-string
                 "^/C:/" "/" (replace-regexp-in-string
                              "^file://" "" (replace-regexp-in-string
                                             ":\\'" "" xinput))))
    (if (and (string-match-p "\\`https?://" xpath)
             (not (minibufferp)))
        (progn
          (if (y-or-n-p "Open url?")
              (browse-url xpath)))
      (apply fun args))))



(defun run-current-go-file ()
  "Run or build current Go file. To build, call `universal-argument' first."
  (interactive)
  (when (not buffer-file-name) (save-buffer))
  (when (buffer-modified-p) (save-buffer))
  (let* ((xoutputb "*run output*")
         (xfname buffer-file-name)
         (xprogName "go")
         (xcmdStr
          (concat xprogName " \""   xfname "\" &")))
    (setq xcmdStr (format (if current-prefix-arg
                              "%s build \"%s\" "
                            "%s run \"%s\" &")
                          xprogName xfname))
    (progn
      (message "Running %s" xfname)
      (message "%s" xcmdStr)
      (shell-command xcmdStr xoutputb))))

(defconst run-current-file-map
  '(("hs" . "runhaskell")
    ("pl" . "perl")
    ("py" . "python3")
    ("rb" . "ruby")
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
                (error "%s: Unknown file extension: %s" real-this-command xfExt)))))
        (setenv "NO_COLOR"))
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
    (progn
      (make-backup))))



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
  (interactive)
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
  "Forward `search-current-word'."
  (interactive)
  (isearch-cur-word)
  (isearch-repeat-forward))

(defun isearch-cur-word-backward ()
  "Backward `search-current-word'."
  (interactive)
  (isearch-cur-word)
  (isearch-repeat-backward))

(defun show-in-desktop ()
  "Show current file in desktop.
 (Mac Finder, File Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'."
  (interactive)
  (let ((xpath (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files))
                       default-directory
                     (car (dired-get-marked-files)))
                 (if (buffer-file-name) (buffer-file-name) default-directory))))
    (cond
     ((string-equal system-type "windows-nt")
      (shell-command (format "PowerShell -Command invoke-item '%s'"
                             (expand-file-name default-directory ))))
     ((string-equal system-type "darwin")
      (shell-command
       (concat "open -R " (shell-quote-argument xpath) " && echo Show in Finder")))
     ((string-equal system-type "gnu/linux")
      (call-process shell-file-name nil 0 nil
                    shell-command-switch
                    (format "%s %s"
                            "xdg-open"
                            (file-name-directory xpath)))))))

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
             (string-match "Sound" (dired-get-filename)))
        (emms-play-dired))
       ((string-equal system-type "darwin")
        (mapc (lambda (xfpath) (shell-command
                                (concat "open "
                                        (shell-quote-argument xfpath))
                                " && echo")) xfileList)
        (message ""))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (xfpath)
                (call-process shell-file-name nil 0 nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument xfpath))))
              xfileList))))))

(defun next-window-or-frame ()
  "Switch to next window or frame.
If current frame has only one window, switch to next frame."
  (interactive)
  (if (one-window-p)
      (other-frame 1)
    (other-window 1)))

(defun alternate-buf-or-frame ()
  "Switch to alternate buffer or frame.
If there more than one frame, switch to next frame."
  (interactive)
  (if (< 1 (length (frame-list)))
      (other-frame 1)
    (alternate-buffer)))

(defun kmacro-record ()
  "Start or stop macro recording."
  (interactive)
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-end-macro nil)
    (kmacro-start-macro nil)))

(defun kmacro-helper ()
  "Ad hoc function."
  (interactive)
  (message "Keyboard macro helper"))

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
    (beg-of-line-or-block)
    (kill-line))
  (setq this-command 'eshell-clear-input))

(defun screenshot ()
  "Take screenshot on macOS."
  (interactive)
  (when (string-equal system-type "darwin")
    (shell-command (concat "screencapture -W -U dummy"))))

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
  (message "%s" "Player not found")))

(defun text-scale-reset ()
  "Reset text scale."
  (interactive)
  (text-scale-adjust 0))

(defun toggle-ibuffer ()
  "Toggle ibuffer.
Force switch to current buffer to update `other-buffer'."
  (interactive)
  (let ((xbuf (buffer-name)) (xm major-mode))
    (if (string-equal major-mode "ibuffer-mode")
        (toggle-gnus)
      (progn
        (switch-to-buffer xbuf)
        (ibuffer)
        (unless (or (member (buffer-name (cadr (buffer-list)))
                            ibuffer-never-show-predicates)
                    (string-equal xm "eww-mode")
                    (string-equal xm "gnus-summary-mode"))
          (ibuffer-jump-to-buffer xbuf))))))

(defun flyspell-goto-prev-error ()
  "Go to prev error."
  (interactive)
  (flyspell-goto-next-error t))

(defun sun-moon ()
  "Show the Sun and the Moon schedule."
  (interactive)
  (lunar-phases)
  (sunrise-sunset))

(defun calculator ()
  "Run calculator."
  (interactive)
  (find-file "~/.calc.py")
  (rename-buffer "calculator"))

(defun weather ()
  "Show weather."
  (interactive)
  (let ((url "https://www.windy.com")
        (lat (number-to-string calendar-latitude))
        (lon (number-to-string calendar-longitude)))
    (browse-url (concat url "/?" lat "," lon ",9"))))

(defun weather-helper ()
  "Email weather forecast."
  (call-process "~/.weather/run.sh" nil 0 nil))

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
  (org-agenda arg "a")
  (other-window 1)
  (balance-windows))

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

(defun sudo ()
  "Use Tramp to `sudo' current file."
  (interactive)
  (when buffer-file-name
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

(defun sync (&optional Silent)
  "Sync private cloud."
  (interactive)
  (if Silent
      (call-process "~/.sync.sh" nil 0 nil)
    (async-shell-command "~/.sync.sh" "*sync output*")))

(defun gpg-agent-helper ()
  "GPG agent helper. Force restart."
  (call-process "~/.gpg-agent.sh" nil 0 nil))

(defun mu-sync ()
  "Mu sync."
  (call-process "~/.mbsync.sh" nil 0 nil))

(defun tmux-helper ()
  "Tmux helper. Force update display resolution."
  (call-process "~/.tmux.sh" nil 0 nil))

(defun mosh-helper ()
  "Mosh helper. Cleanup detached sessions."
  (interactive)
  (call-process "~/.mosh.sh" nil 0 nil))

(defun stow ()
  "Stow packages."
  (interactive)
  (shell-command "~/.stow.sh && echo")
  (message "%s" "Stow complete"))

(defun screensaver ()
  "Start screensaver in tmux."
  (let ((message-log-max nil)
        (inhibit-message t))
    (shell-command "tmux clock-mode")))

(defun toggle-gnus ()
  "Toggle gnus."
  (interactive)
  (if (get-buffer "*Group*")
      (switch-to-buffer "*Group*")
    (gnus)))

(defun server ()
  "Run project server. Switch to console if already running."
  (interactive)
  (let ((xpath (getenv "RUN_SERVER"))
        (xbuf "*run server*"))
    (if (get-buffer xbuf)
        (switch-to-buffer-other-window xbuf)
      (async-shell-command
       (if (> (length (getenv "CONNINFO")) 0)
           (format "cd %s && go run main.go -d $CONNINFO " xpath)
         (format "cd %s && go run main.go" xpath)) xbuf))))

(defun test ()
  "Run project tests."
  (interactive)
  (let ((xbuf "*run server*"))
    (if (get-buffer xbuf)
        (kill-buffer xbuf)))
  (async-shell-command "$RUN_TEST" "*run test*"))

(defun sql ()
  "Open SQL client."
  (interactive)
  (find-file "~/.sql"))

(defun exec-query ()
  "Execute potgres SQL statement separated by ;."
  (interactive)
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
    (goto-char (point-max))
    (push-mark (point) t nil)
    (insert (shell-command-to-string
             (format "psql %s -c \"%s\" -q" xconn xquery)))
    (insert (concat (make-string 79 45) "\n"))
    (set-mark-command t)
    (other-window 1)))

(defun who-called-defun (oldfun format &rest args)
  "Backtrace. For example, to find out who called `message':
(advice-add \='message :around #\='who-called-defun)"
  (let ((trace nil) (n 1) (frame nil))
    (while (setf frame (backtrace-frame n))
      (setf n (1+ n)
            trace (cons (cadr frame) trace)))
    (apply oldfun (concat "<<%S>>\n" format) (cons trace args))))

(defun suppress-messages (oldfun &rest args)
  "Suppress messages from function. For example:
(advice-add \='ibuffer-update :around \='suppress-messages)"
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
         (apply oldfun args)
      (advice-remove 'message #'silence))))

(defun command-error-function-silent (data context caller)
  "Ignore some signals; pass the rest to the default handler."
  (unless (memq (car data)
                '(buffer-read-only
                  text-read-only
                  beginning-of-buffer
                  end-of-buffer
                  quit))
    (command-error-default-function data context caller)))

(defun hide-messages (fun &rest args)
  "Inhibit messages from FUN. Run as advice."
  (let ((message-log-max nil)
        (inhibit-messages t))
    (apply fun args)))

(defun disable-kill-buffer-query-functions (kill-fun &rest args)
  "Disable confirmation conditionally before buffer kill."
  (let ((x kill-buffer-query-functions))
    (when (string-match "run server" (buffer-name))
      (setq kill-buffer-query-functions nil))
    (unwind-protect
        (apply kill-fun args)
      (setq kill-buffer-query-functions x))
    (setq kill-buffer-query-functions x)))

(defun disable-minibuffer-fun (fun &rest args)
  "Conditionally disable FUN. Run as advice."
  (unless (minibufferp) (apply fun args)))

(defun byte-compile-package ()
  "Byte compile current package."
  (when (string-match ".+emacs.d/packages+." (buffer-file-name))
    (clean-whitespace)
    (save-buffer)
    (byte-compile-file (expand-file-name (buffer-file-name)))))

(defun terminal ()
  "Run terminal emulator."
  (interactive)
  (vterm))

(defun novel ()
  "Read novel."
  (interactive)
  (switch-to-buffer "*Novel*"))

(defun hippie-expand-undo ()
  "Undo the expansion."
  (interactive)
  (hippie-expand -1))

(defun pass-generate ()
  "Generate and copy pass."
  (interactive)
  (let ((xpass (read-from-minibuffer "Generate pass path: ")))
    (shell-command (concat "pass generate -c " xpass))))

(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:
• for a function name where point is;
• for a variable name where point is."
  (interactive)
  (let (xsym)
    (cond ((setq xsym (ignore-errors
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
          ((setq xsym (variable-at-point)) (describe-variable xsym))))
  (setq this-command 'split-window-below))

(defun run-at-time-wrap (time fun &rest args)
  "Wrap for `run-at-time'."
  (if (and (equal (format-time-string "%H") (substring time 0 2))
           (equal (format-time-string "%M") (substring time 3 5)))
      (apply fun args)))

(defun dired-recursive-expand (dir)
  "Recursive expand dired dir."
  (interactive (list default-directory))
  (mapc #'dired-maybe-insert-subdir
        (seq-filter #'file-directory-p (directory-files-recursively dir "" t))))

(defun dired-trash-move-adjust (fun &rest args)
  "Disable move to trash if move to trash is impossible. Use as :around advice."
  (when (or (string-match "DiskO" (file-truename (dired-get-filename)))
            (file-remote-p (car args)))
    (setq delete-by-moving-to-trash nil))
  (unwind-protect
      (apply fun args)
    (setq delete-by-moving-to-trash t))
  (setq delete-by-moving-to-trash t))

(defvar dired-external-extensions
  '("mkv" "mp4" "avi" "mov" "mp3" "m4a" "ts" "flac" "MTS" "torrent")
  "Open these file extensions with `open-in-external-app'.")

(defun dired-find-file-adjust (fun &rest args)
  "Adjust open file method in dired. Use as :around advice."
  (let ((x large-file-warning-threshold))
    (if (and (display-graphic-p)
             (member (file-name-extension
                      (file-truename (dired-get-filename)))
                     dired-external-extensions))
        (progn
          (setq large-file-warning-threshold nil)
          (open-in-external-app))
      (apply fun args))
    (setq large-file-warning-threshold x)))

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
    (message "%s" "Not in org-mode")))

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

(defun lookup-around (fun &rest args)
  "Lookup selection if buffer is read only and last command `extend-selection'.
Use as around advice e.g. for mouse left click after double click."
  (if (and (eq last-command 'extend-selection)
           buffer-read-only)
      (progn
        (return-before)
        (lookup-web))
    (apply fun args)))

(defmacro advice-add-macro (SymList How Fun)
  "Map `advice-add' over a list SYMLIST to FUN."
  `(progn
     ,@(mapcar
        (lambda (xcmd)
          `(advice-add ,(list 'quote xcmd) ,How ,Fun))
        (cadr SymList))))

(defun quit ()
  "Confirm and quit. Because restart without confirm."
  (interactive)
  (if (y-or-n-p "Quit?")
      (save-buffers-kill-terminal)))

(defun toggle-ispell-dict ()
  "Toggle spell dictionaries.
Default dictionary is nil. First run with hook sets en."
  (if (string-equal ispell-current-dictionary "en")
      (progn
        (ispell-change-dictionary "ru")
        (setq ispell-personal-dictionary "~/.emacs.d/settings/.aspell.ru.pws"))
    (progn
      (ispell-change-dictionary "en")
      (setq ispell-personal-dictionary "~/.emacs.d/settings/.aspell.en.pws"))))

(defun turn-spell-checking-on ()
  "Turn flyspell mode on. Set dictionary."
  (flyspell-mode)
  (toggle-ispell-dict))

(defun set-location (Latitude Longtitude)
  "Set location."
  (setq calendar-location-name "Current"
        calendar-latitude  (string-to-number Latitude)
        calendar-longitude (string-to-number Longtitude)))

(defun toggle-frame-transparent ()
  "Toggle current frame transparency."
  (interactive)
  (when (display-graphic-p)
    (if (not (get 'toggle-frame-transparent 'state))
        (progn
          (put 'toggle-frame-transparent 'state t)
          (set-frame-parameter (selected-frame) 'alpha '(10 10)))
      (put 'toggle-frame-transparent 'state nil)
      (set-frame-parameter (selected-frame) 'alpha '(100 100)))))

(defun recentf-open-files-adjust ()
  "Remove unnecessary legends from the buffer."
  (with-current-buffer "*Open Recent*"
    (let ((inhibit-read-only t))
      (save-excursion
        (display-line-numbers-mode 0)
        (goto-char (point-min))
        (kill-region (line-beginning-position) (line-beginning-position 2))
        (kill-region (line-beginning-position) (line-beginning-position 2))
        (goto-char (point-max))
        (kill-region (line-beginning-position) (line-beginning-position 2))
        (setq this-command 'recentf-open-files))
      (forward-word)
      (backward-char))))

(defun mouse-3 ()
  "Mouse right click. If buffer read only then lookup word translation.
Otherwise call `paste-or-paste-previous'."
  (interactive)
  (if buffer-read-only
      (lookup-translation)
    (paste-or-paste-previous)))



(defgroup keyamp nil "Customization options for keyamp"
  :group 'help :prefix "keyamp-")

(defvar keyamp-command-hook nil "Hook for `keyamp-command'")
(defvar keyamp-insert-hook  nil "Hook for `keyamp-insert'")

(defconst keyamp-karabiner-cli
  "/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli"
  "Karabiner-Elements CLI executable. Optional for sync.")

(defconst keyamp-command-indicator "🟢" "Command mode.")
(defconst keyamp-insert-indicator  "🟠" "Insert mode or repeat edit.")
(defconst keyamp-repeat-indicator  "🔵" "Repeat view.")
(defconst keyamp-screen-indicator  "🟣" "Repeat screen.")

(defconst keyamp-command-cursor "LawnGreen"      "Color command.")
(defconst keyamp-insert-cursor  "Gold"           "Color insert.")
(defconst keyamp-repeat-cursor  "DeepSkyBlue"    "Color repeat view.")
(defconst keyamp-screen-cursor  "LightSlateBlue" "Color repeat screen.")

(defconst keyamp-idle-timeout (* 3 60) "Idle timeout.")
(defconst keyamp-hold-threshold (/ 200 1000.0) "Key hold down threshold.")



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
• Advice default HOW :after might be changed by specific HOW;
• Activate COMMANDMODE or INSERTMODE mode optionally;
• Deactivate repeat mode after idle for TIMEOUT seconds."
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       ,@(mapcar
          (lambda (xcmd)
            `(advice-add ,(list 'quote xcmd) (if ,How ,How :after)
                         (lambda (&rest r) "repeat"
                           (if (and ,CommandMode keyamp-insert-p)
                               (keyamp-command))
                           (setq keyamp--deactivate-repeat-mode-fun
                                 (set-transient-map ,xkeymapName))
                           (keyamp-cancel-repeat-mode-idle-timer)
                           (if (and ,TimeOut (not keyamp-insert-p))
                               (setq keyamp--repeat-mode-idle-timer
                                       (run-with-idle-timer
                                        ,TimeOut nil 'keyamp-escape)))
                           (if ,InsertMode (keyamp-insert)))))
          (cadr CmdList)))))

(defmacro keyamp--set-map-hook
    (KeymapName HookList &optional CommandMode InsertMode RepeatMode)
  "Map `set-transient-map' using `add-hook' over a list HOOKLIST.
Activate command, insert or repeat mode optionally."
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
                         (set-transient-map ,xkeymapName)
                         (if ,RepeatMode (setq this-command 'keyamp--repeat-dummy)))))
          (cadr HookList)))))

(defmacro keyamp--map-leaders (KeymapName CmdCons)
  "Map leader keys using `keyamp--map'."
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (keyamp--map ,xkeymapName
         '(("DEL" . ,(car (cadr CmdCons))) ("<backspace>" . ,(car (cadr CmdCons)))
           ("SPC" . ,(cdr (cadr CmdCons))))))))

(defmacro with-sparse-keymap-x (&rest body)
  "Make sparse keymap x for next use in BODY."
  `(let ((x (make-sparse-keymap))) ,@body))



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
  (let ((xto (alist-get From keyamp-engineer-engram-to-russian-computer
             nil nil 'string-equal)))
    (when (stringp xto)
      (string-to-char xto))))

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
                              (cdr x))))
          xl)))


;; keymaps

(defvar keyamp-map (make-sparse-keymap)
  "Parent keymap of `keyamp-command-map'.
Define keys that are available in both command and insert modes here.")

(defvar keyamp-command-map (cons 'keymap keyamp-map)
  "Keymap that takes precedence over all other keymaps in command mode.
Inherits bindings from `keyamp-map'.

In command mode, if no binding is found in this map
`keyamp-map' is checked, then if there is still no binding,
the other active keymaps are checked like normal. However, if a key is
explicitly bound to nil in this map, it will not be looked up in
`keyamp-map' and lookup will skip directly to the normally
active maps.

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
  '(("TAB" . keyamp-leader-map)          ("<tab>"    . keyamp-leader-map)
                                         ("<escape>" . keyamp-escape)             ("S-<escape>" . ignore)

    ;; Control sequences for leaders. Russian converted from Engineer Engram.
    ;; The sequences are prefixes for key hold down in Karabiner.
    ("C-^" . keyamp-left-leader-map)     ("C-+"      . keyamp-left-leader-map)
    ("C-_" . keyamp-right-leader-map)    ("C-И"      . keyamp-right-leader-map)))

;; Single keys mapping must double in Russian here. All prefix sequences mapped
;; automatically using `keyamp-map-input-source'. If missing then same.
(keyamp--map keyamp-command-map
  '(("RET" . keyamp-insert)              ("<return>"    . keyamp-insert)          ("S-<return>"    . ignore)
    ("DEL" . keyamp-left-leader-map)     ("<backspace>" . keyamp-left-leader-map) ("S-<backspace>" . prev-proj-buffer)
    ("SPC" . keyamp-right-leader-map)                                             ("S-SPC"         . next-proj-buffer)

    ;; left half
    ("`" . delete-forward-char)          ("ё" . delete-forward-char)              ("~" . keyamp-qwerty-to-current-layout)  ("Ë" . keyamp-qwerty-to-current-layout)
    ("1" . kmacro-record)                                                         ("!" . ignore)
    ("2" . kmacro-helper)                                                         ("@" . ignore)
    ("3" . kmacro-play)                                                           ("#" . ignore)                           ("№" . ignore)
    ("4" . append-to-register-1)                                                  ("$" . ignore)
    ("5" . terminal)                                                              ("%" . ignore)

    ("q" . insert-space-before)          ("й" . insert-space-before)              ("Q" . ignore)                           ("Й" . ignore)
    ("w" . backward-kill-word)           ("ц" . backward-kill-word)               ("W" . ignore)                           ("Ц" . ignore)
    ("e" . undo)                         ("у" . undo)                             ("E" . todo)                             ("У" . todo)
    ("r" . kill-word)                    ("к" . kill-word)                        ("R" . ignore)                           ("К" . ignore)
    ("t" . cut-text-block)               ("е" . cut-text-block)                   ("T" . ignore)                           ("Е" . ignore)

    ("a" . shrink-whitespaces)           ("ф" . shrink-whitespaces)               ("A" . ignore)                           ("Ф" . ignore)
    ("s" . open-line)                    ("ы" . open-line)                        ("S" . prev-proj-buffer)                 ("Ы" . prev-proj-buffer)
    ("d" . delete-backward)              ("в" . delete-backward)                  ("D" . repeat)                           ("В" . repeat)
    ("f" . newline)                      ("а" . newline)                          ("F" . next-proj-buffer)                 ("А" . next-proj-buffer)
    ("g" . set-mark-command)             ("п" . set-mark-command)                 ("G" . ignore)                           ("П" . ignore)

    ("z" . toggle-comment)               ("я" . toggle-comment)                   ("Z" . ignore)                           ("Я" . ignore)
    ("x" . cut-line-or-selection)        ("ч" . cut-line-or-selection)            ("X" . ignore)                           ("Ч" . ignore)
    ("c" . copy-line-or-selection)       ("с" . copy-line-or-selection)           ("C" . ignore)                           ("С" . ignore)
    ("v" . paste-or-paste-previous)      ("м" . paste-or-paste-previous)          ("V" . ignore)                           ("М" . ignore)
    ("b" . toggle-letter-case)           ("и" . toggle-letter-case)               ("B" . ignore)                           ("И" . ignore)

    ;; right half
    ("6" . pass)                                                                  ("^" . ignore)
    ("7" . number-to-register)                                                    ("&" . ignore)
    ("8" . copy-to-register)                                                      ("*" . goto-matching-bracket) ; QWERTY * → = Engineer Engram, QWERTY / → = RU PC Karabiner
    ("9" . toggle-case-fold-search)                                               ("(" . ignore)
    ("0" . eshell)                                                                (")" . ignore)
    ("-" . tetris)                                                                ("_" . ignore)
    ("=" . goto-matching-bracket)                                                 ("+" . ignore)

    ("y"  . isearch-cur-word-forward)    ("н" . isearch-cur-word-forward)         ("Y" . ignore)                           ("Н" . ignore)
    ("u"  . back-word)                   ("г" . back-word)                        ("U" . flymake-goto-prev-error)          ("Г" . flymake-goto-prev-error)
    ("i"  . previous-line)               ("ш" . previous-line)                    ("I" . beg-of-line-or-block)             ("Ш" . beg-of-line-or-block)
    ("o"  . forw-word)                   ("щ" . forw-word)                        ("O" . flymake-goto-next-error)          ("Щ" . flymake-goto-next-error)
    ("p"  . exchange-point-and-mark)     ("з" . exchange-point-and-mark)          ("P" . ignore)                           ("З" . ignore)
    ("["  . alternate-buf-or-frame)      ("х" . alternate-buf-or-frame)           ("{" . ignore)                           ("Х" . ignore)
    ("]"  . write-file)                  ("ъ" . write-file)                       ("}" . ignore)                           ("Ъ" . ignore)
    ("\\" . bookmark-set)                                                         ("|" . ignore)

    ("h" . beg-of-line)                  ("р" . beg-of-line)                      ("H"  . ignore)                          ("Р" . ignore)
    ("j" . backward-char)                ("о" . backward-char)                    ("J"  . isearch-cur-word-backward)       ("О" . isearch-cur-word-backward)
    ("k" . next-line)                    ("л" . next-line)                        ("K"  . end-of-line-or-block)            ("Л" . end-of-line-or-block)
    ("l" . forward-char)                 ("д" . forward-char)                     ("L"  . isearch-cur-word-forward)        ("Д" . isearch-cur-word-forward)
    (";" . end-of-lyne)                  ("ж" . end-of-lyne)                      (":"  . ignore)                          ("Ж" . ignore)
    ("'" . alternate-buffer)             ("э" . alternate-buffer)                 ("\"" . prev-frame)                      ("Э" . prev-frame)

    ("n" . isearch-forward)              ("т" . isearch-forward)                  ("N" . isearch-backward)                 ("Т" . isearch-backward)
    ("m" . backward-left-bracket)        ("ь" . backward-left-bracket)            ("M" . ignore)                           ("Ь" . ignore)
    ("," . next-window-or-frame)         ("б" . next-window-or-frame)             ("<" . ignore)                           ("Б" . ignore)
    ("." . forward-right-bracket)        ("ю" . forward-right-bracket)            (">" . ignore)                           ("Ю" . ignore)
    ("/" . goto-matching-bracket)                                                 ("?" . ignore)

    ("<left>" . left-char)               ("<right>" . right-char)
    ("<up>"   . up-line)                 ("<down>"  . down-line)))

;; TAB serves as leader key for insert mode or for specific major modes.
;; Hit TAB ESC/RET or DEL/SPC for move by lines or chars.
(keyamp--map (define-prefix-command 'keyamp-leader-map)
  '(("ESC" . previous-line)              ("<escape>"    . previous-line)
    ("RET" . next-line)                  ("<return>"    . next-line)
    ("DEL" . left-char)                  ("<backspace>" . left-char)
    ("SPC" . right-char)
    ("`"   . delete-forward-char)
    ("e"   . undo)
    ("v"   . paste-or-paste-previous)))

(keyamp--map (define-prefix-command 'keyamp-left-leader-map)
  '(("TAB" . toggle-ibuffer)             ("<tab>"       . toggle-ibuffer)
    ("ESC" . ignore)                     ("<escape>"    . ignore)
    ("RET" . execute-extended-command)   ("<return>"    . execute-extended-command)
    ("DEL" . select-block)               ("<backspace>" . select-block)
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
    ("e" . delete-window)
    ("r" . query-replace)
    ("t" . kill-line)

    ("a" . split-window-below)
    ("s" . prev-user-buffer)
    ("d" . delete-other-windows)
    ("f" . next-user-buffer)
    ("g" . rectangle-mark-mode)

    ("z" . universal-argument)
    ("x" . restart-emacs)
    ("c" . copy-to-register-1)
    ("v" . paste-from-register-1)
    ("b" . toggle-previous-letter-case)

    ;; left leader right half
    ("6" . quit)
    ("7" . jump-to-register)
    ("8" . sql)
    ("9" . screenshot)
    ("0" . proced)
    ("-" . ignore)
    ("=" . ignore)

    ("y" . find-name-dired)
    ("u" . bookmark-bmenu-list)

                                         ("i i"   . show-in-desktop)
    ("i DEL" . count-words)              ("i SPC" . count-matches)

    ("o"  . make-frame-command)
    ("p"  . view-echo-area-messages)
    ("["  . toggle-frame-maximized)
    ("]"  . find-file)
    ("\\" . bookmark-rename)

    ("h"  . bookmark-jump)

    ("j s" . glyphless-display-mode)     ("j l" . narrow-to-region-or-block)
                                         ("j k" . narrow-to-defun)
    ("j f" . toggle-word-wrap)           ("j j" . widen)
    ("j DEL" . hl-line-mode)             ("j SPC" . whitespace-mode)

    ("k s" . space-to-newline)
    ("k d" . delete-matching-lines)      ("k k" . list-matching-lines)
    ("k f" . delete-non-matching-lines)
    ("k r" . quote-lines)                ("k u" . escape-quotes)
    ("k t" . delete-duplicate-lines)     ("k y" . slash-to-double-backslash)
    ("k v" . reformat-to-sentence-lines) ("k n" . double-backslash-to-slash)
    ("k w" . sort-lines-key-value)       ("k o" . slash-to-backslash)
    ("k x" . insert-column-a-z)          ("k ." . sort-lines-block-or-region)
    ("k c" . cycle-hyphen-lowline-space) ("k ," . sort-numeric-fields)
    ("k DEL" . ispell-word)              ("k SPC" . flyspell-buffer)

    ("l" . describe-foo-at-point)
    (";" . recentf-open-files)
    ("'" . toggle-debug-on-error)
    ("n" . switch-to-buffer)
    ("m" . downloads)
    ("," . open-last-closed)
    ("." . player)
    ("/" . goto-line)

    ("i ESC" . ignore)                   ("i <escape>" . ignore)
    ("j ESC" . ignore)                   ("j <escape>" . ignore)
    ("k ESC" . ignore)                   ("k <escape>" . ignore)))

(keyamp--map (define-prefix-command 'keyamp-right-leader-map)
  '(("TAB" . toggle-gnus)                ("<tab>"       . toggle-gnus)
    ("ESC" . ignore)                     ("<escape>"    . ignore)
    ("RET" . read-only-mode)             ("<return>"    . read-only-mode)
    ("DEL" . select-line)                ("<backspace>" . select-line)
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
    ("e DEL" . clock)                    ("e SPC" . calendar)

    ("r" . query-replace-regexp)
    ("t" . calc)
    ("a" . mark-whole-buffer)
    ("s" . clean-whitespace)

    ("d e" . org-shiftup)
    ("d s" . shell-command-on-region)    ("d l" . elisp-eval-region-or-buffer)
    ("d d" . eval-last-sexp)             ("d k" . run-current-file)
    ("d f" . shell-command)
    ("d DEL" . stow)                     ("d SPC" . eval-defun)

    ("f e" . insert-emacs-quote)         ("f i" . insert-ascii-single-quote)
    ("f f" . insert-char)                ("f j" . insert-brace)
    ("f d" . emoji-insert)               ("f k" . insert-paren)
    ("f s" . insert-formfeed)            ("f l" . insert-square-bracket)
    ("f g" . insert-double-angle-quote)  ("f h" . insert-double-curly-quote)
    ("f DEL" . insert-backtick-quote)    ("f SPC" . insert-ascii-double-quote)

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
    ("=" . text-scale-increase)

    ("y"  . find-text)
    ("u"  . pop-local-mark-ring)
    ("i"  . copy-file-path)
    ("o"  . set-mark-deactivate-mark)
    ("p"  . show-kill-ring)
    ("["  . prev-frame)
    ("]"  . rename-visited-file)
    ("\\" . bookmark-delete)

    ("h" . View-scroll-half-page-backward)
    ("j" . toggle-truncate-lines)
    ("k" . make-backup-and-save)
    ("l" . display-line-numbers-mode)
    (";" . View-scroll-half-page-forward)
    ("'" . sync)

    ("n" . save-buffer)
    ("m" . dired-jump)
    ("," . save-close-current-buffer)
    ("." . mark-defun)
    ("/" . toggle-frame-transparent)     ("*" . toggle-frame-transparent)

    ("e ESC" . ignore)                   ("e <escape>" . ignore)
    ("d ESC" . ignore)                   ("d <escape>" . ignore)
    ("f ESC" . ignore)                   ("f <escape>" . ignore)))


;; Core Remaps

;; Hold down ESC to post C-h (karabiner) and call `help-map'.
;; See `keyamp-hold-indicate'.
(keyamp--map-leaders help-map '(lookup-word-definition . lookup-translation))
(keyamp--map help-map
  '(("ESC" . ignore)           ("<escape>" . ignore)           ("C-h" . nil) ; unmap for use by which key
    ("RET" . lookup-web)       ("<return>" . lookup-web)
    ("TAB" . lookup-wikipedia) ("<tab>"    . lookup-wikipedia)
    ("e" . describe-char)      ("i" . info)
    ("s" . info-lookup-symbol) ("j" . describe-function)
    ("d" . man)                ("k" . describe-key)
    ("f" . elisp-index-search) ("l" . describe-variable)
    ("q" . describe-syntax)    ("p" . apropos-documentation)                               ("<f1>" . ignore) ("<help>" . ignore) ("C-w" . ignore) ("C-c" . ignore)
    ("w" . describe-bindings)  ("o" . lookup-all-dictionaries)                             ("C-o"  . ignore) ("C-\\"   . ignore) ("C-n" . ignore) ("C-f" . ignore)
    ("r" . describe-mode)      ("u" . lookup-all-synonyms)                                 ("C-s"  . ignore) ("C-e"    . ignore) ("'"   . ignore) ("6"   . ignore)
    ("a" . describe-face)      (";" . lookup-wiktionary)                                   ("9"    . ignore) ("L"      . ignore) ("n"   . ignore) ("p"   . ignore) ("v" . ignore)
    ("g" . apropos-command)    ("h" . describe-coding-system)                              ("?"    . ignore) ("A"      . ignore) ("U"   . ignore) ("S"   . ignore)
    ("z" . apropos-variable)   ("." . lookup-word-dict-org)
    ("x" . apropos-value)      ("," . lookup-etymology)))

(keyamp--map global-map
  '(("C-r"                     . open-file-at-cursor) ; hold down RET to post C-r (karabiner)
    ("C-t"                     . hippie-expand)       ; hold down RET in insert mode
    ("<f13>"                   . ignore)              ; special key not f13 really
    ("<next>"                  . View-scroll-half-page-forward)
    ("<prior>"                 . View-scroll-half-page-backward)
    ("<home>"                  . scroll-down-command)
    ("<end>"                   . scroll-up-command)
    ("<double-mouse-1>"        . extend-selection)
    ("<mouse-3>"               . mouse-3)
    ("<header-line> <mouse-1>" . prev-frame)
    ("<header-line> <mouse-3>" . make-frame-command)
    ("<left-fringe> <mouse-1>" . ignore)))

(advice-add 'keyamp-insert :around 'open-http-at-cursor)
(advice-add 'keyamp-insert :around 'lookup-around)

(when (display-graphic-p)
  (advice-add 'mouse-set-point :around 'lookup-around)

  (advice-add 'mouse-drag-region :before
              (lambda (&rest r) "copy selection with left click"
                (if (region-active-p)
                    (copy-region-as-kill (region-beginning) (region-end)))))

  (advice-add 'mouse-set-point :after
              (lambda (&rest r) "activate command mode with left click"
                (if keyamp-insert-p (keyamp-command))))

  (advice-add 'mouse-set-point :before
              (lambda (&rest r) "no recenter after left click, hack"
                (if pixel-scroll-mode (pixel-scroll-pixel-up 1))))

  (advice-add 'mac-mwheel-scroll :before
              (lambda (&rest r) "activate command mode after wheel scroll"
                (if keyamp-insert-p (keyamp-command))))

  (advice-add 'mac-mwheel-scroll :before
              (lambda (&rest r) "deactivate selection before wheel scroll"
                (if (region-active-p) (deactivate-mark)))))

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
  '(("<escape>"  . isearch-cancel)          ("C-^"         . ignore) ; conflict
    ("TAB"       . isearch-repeat-forward)  ("<tab>"       . isearch-repeat-forward)
    ("<backtab>" . isearch-repeat-backward) ("S-<tab>"     . isearch-repeat-backward)
    ("DEL"       . isearch-del-char)        ("<backspace>" . isearch-del-char)))

(with-sparse-keymap-x
 ;; Find the occurrence of the current search string with J/L or DEL/SPC.
 ;; Press I/K to get search strings from the ring then DEL/SPC to repeat.
 ;; RET to search again.
 (keyamp--map-leaders x '(isearch-repeat-backward . isearch-repeat-forward))
 (keyamp--map x
   '(("i" . isearch-ring-retreat)     ("ш" . isearch-ring-retreat)
     ("j" . isearch-repeat-backward)  ("о" . isearch-repeat-backward)
     ("k" . isearch-ring-advance)     ("л" . isearch-ring-advance)
     ("l" . isearch-repeat-forward)   ("д" . isearch-repeat-forward)))

 (keyamp--set-map x
   '(isearch-ring-retreat    isearch-ring-advance
     isearch-repeat-backward isearch-repeat-forward
     search-current-word     isearch-yank-kill)))


;; Repeat mode. Screen commands.

(with-sparse-keymap-x
 ;; Leader layer to become transient main. Base map for next leaders adjustment
 ;; by transient maps which might be set by following target commands subsets.
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--map x
   '(("TAB" . alternate-buffer) ("<tab>" . alternate-buffer)
     ("C-h" . delete-window)    ("C-r"   . delete-other-windows)))

 (keyamp--remap x
   '((delete-forward-char     . next-buffer)
     (insert-space-before     . delete-frame)
     (backward-kill-word      . sun-moon)
     (undo                    . delete-window)
     (kill-word               . ignore)
     (cut-text-block          . calc)
     (exchange-point-and-mark . view-echo-area-messages)
     (shrink-whitespaces      . split-window-below)
     (delete-backward         . toggle-ibuffer)
     (set-mark-command        . new-empty-buffer)
     (cut-line-or-selection   . prev-eww-buffer)
     (copy-line-or-selection  . agenda)
     (paste-or-paste-previous . tasks)
     (backward-left-bracket   . dired-jump)
     (forward-right-bracket   . player)
     (kmacro-play             . config)))

 (keyamp--set-map x
   '(prev-user-buffer     next-user-buffer
     delete-other-windows split-window-below
     alternate-buffer     delete-window
     open-last-closed     save-close-current-buffer
     prev-proj-buffer     next-proj-buffer
     prev-eww-buffer      next-eww-buffer
     tasks                config
     previous-buffer      next-buffer
     prev-frame           forw-frame
     find-prev-dir-file   find-next-dir-file
     up-line              down-line
     dired-jump           downloads
     player)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-user-buffer) (newline . next-user-buffer)))
 (keyamp--set-map x
   '(prev-user-buffer     next-user-buffer
     delete-other-windows delete-window
     split-window-below   alternate-buffer) nil nil nil 10))

(with-sparse-keymap-x
 ;; Hit RET right away to hide split window.
 (keyamp--remap x '((keyamp-insert . delete-other-windows)))
 (keyamp--set-map x '(split-window-below) nil nil nil 3))

(with-sparse-keymap-x
 (keyamp--remap x
   '((open-line . prev-proj-buffer) (newline . next-proj-buffer)
     (delete-backward . speedbar)))
 (keyamp--set-map x '(prev-proj-buffer next-proj-buffer)))

(with-sparse-keymap-x
 (keyamp--map x
   '(("TAB"   . View-scroll-half-page-forward)
     ("<tab>" . View-scroll-half-page-forward)))
 (keyamp--remap x
   '((open-line       . prev-eww-buffer) (newline       . next-eww-buffer)
     (delete-backward . eww-reload)      (keyamp-insert . eww-reload)
     (undo            . eww)))
 (keyamp--set-map x '(prev-eww-buffer next-eww-buffer)))

(with-sparse-keymap-x
 (keyamp--map x '(("TAB" . tasks) ("<tab>" . tasks)))
 (keyamp--remap x '((open-line . prev-user-buffer) (newline . tasks)))
 (keyamp--set-map x '(tasks) nil nil nil 10))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-user-buffer) (newline . config)))
 (keyamp--set-map x '(config) nil nil nil 10))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--remap x '((open-line . previous-buffer) (newline . next-buffer)))
 (keyamp--set-map x '(previous-buffer next-buffer)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--remap x '((open-line . prev-frame) (newline . forw-frame)))
 (keyamp--set-map x '(prev-frame forw-frame)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--remap x '((open-line . find-prev-dir-file) (newline . find-next-dir-file)))
 (keyamp--set-map x '(find-prev-dir-file find-next-dir-file)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(backward-left-bracket . backward-left-bracket))
 (keyamp--remap x
   '((open-line             . prev-user-buffer) (newline . next-user-buffer)
     (backward-left-bracket . dired-jump)))
 (keyamp--set-map x '(dired-jump downloads player) nil nil nil 2))

(with-sparse-keymap-x
 ;; Hold down comma to call `save-close-current-buffer'. Then comma to repeat.
 (keyamp--remap x '((next-window-or-frame . save-close-current-buffer)))
 (keyamp--set-map x '(save-close-current-buffer) nil nil nil 2))


;; Repeat mode. View commands.

(with-sparse-keymap-x
 ;; Initiate by triple DEL/SPC (hold down). Transition to Screen mode.
 ;; I/K or DEL/SPC to move by lines. See `return-before'.
 ;; This keymap related both to Screen and to Repeat modes.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--map x
   '(("TAB"   . View-scroll-half-page-forward)
     ("<tab>" . View-scroll-half-page-forward)))
 (keyamp--remap x
   '((previous-line . up-line)          (next-line . down-line)
     (open-line     . prev-user-buffer) (newline   . next-user-buffer)))
 (keyamp--set-map x '(up-line down-line)))

(with-sparse-keymap-x
 ;; Initiate by SPC then double DEL or DEL then SPC hold down. Double H/; so
 ;; third press H for beginning of the buffer, 4th for end of the buffer.
 ;; Similarly for ; in opposite direction. I/K or DEL/SPC to move by blocks.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--remap x
   '((previous-line . beg-of-line-or-block)
     (next-line     . end-of-line-or-block)
     (beg-of-line   . beg-of-line-or-buffer)
     (end-of-lyne   . end-of-line-or-buffer)))

 (advice-add 'beg-of-line :after
             (lambda (&rest r) "second press activates repeat"
               (if (eq last-command this-command)
                   (progn
                     (setq this-command 'beg-of-line-or-block)
                     (set-transient-map x)))))

 (advice-add 'end-of-lyne :after
             (lambda (&rest r) "second press activates repeat"
               (if (eq last-command this-command)
                   (progn
                     (setq this-command 'end-of-line-or-block)
                     (set-transient-map x)))))

 ;; Hit sticky shift then hold down I/K to move by blocks.
 (keyamp--map x
   '(("I" . previous-line) ("Ш" . previous-line)
     ("K" . next-line)     ("Л" . next-line)))

 (keyamp--set-map x
   '(beg-of-line-or-block  end-of-line-or-block
     beg-of-line-or-buffer end-of-line-or-buffer)))

(with-sparse-keymap-x
 ;; Use I/K to move nodes in ibuffer and gnus. Target functions remapped
 ;; accordingly. Initiate also by SPC then double DEL or DEL then SPC hold down.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--remap x
   '((previous-line . beg-of-line-or-block)
     (next-line     . end-of-line-or-block)
     (beg-of-line   . beg-of-line-or-buffer)
     (end-of-lyne   . end-of-line-or-buffer)))
 (keyamp--set-map x
   '(ibuffer-backward-filter-group   ibuffer-forward-filter-group
     ibuffer-toggle-filter-group
     gnus-topic-goto-prev-topic-line gnus-topic-goto-next-topic-line
     gnus-topic-select-group)))

(with-sparse-keymap-x
 ;; SPC SPC prefix command.
 ;; Double SPC to run `extend-selection', then next SPC press to
 ;; deactivate mark and run `down-line'. That is, hold down SPC to start move
 ;; down lines with SPC while DEL does up lines. Similarly for `select-line'
 ;; and move by blocks. Hack.
 ;; Or hit SPC SPC DEL to scroll half page backward and repeat by DEL.
 (keyamp--map x '(("TAB" . tasks) ("<tab>" . tasks)))
 (keyamp--map-leaders x '(View-scroll-half-page-backward . down-line))
 (keyamp--remap x '((keyamp-escape . return-before)))
 (keyamp--set-map x '(extend-selection)))

;; Delete selection right before insertion.
(advice-add 'keyamp-insert :before 'delete-before)

(with-sparse-keymap-x
 ;; DEL DEL prefix command.
 ;; Triple DEL (hold down) to move lines up and activate Screen mode.
 ;; Or hit DEL DEL SPC to scroll half page forward and repeat by SPC.
 (keyamp--map x '(("TAB" . novel) ("<tab>" . novel)))
 (keyamp--map-leaders x '(up-line . View-scroll-half-page-forward))
 (keyamp--remap x
   '((previous-line . beg-of-line-or-block) (next-line . select-block)
     (keyamp-escape . return-before)
     (keyamp-insert . copy-line-or-selection)))
 (keyamp--set-map x '(select-block)))

(with-sparse-keymap-x
 ;; SPC DEL prefix command.
 ;; SPC DEL DEL to call `beg-of-line-or-block'. Hold down DEL to repeat.
 ;; Or hit SPC DEL SPC for `next-user-buffer' and activate screen keymap.
 (keyamp--map x '(("TAB" . terminal) ("<tab>" . terminal)))
 (keyamp--map-leaders x '(beg-of-line-or-block . next-user-buffer))
 (keyamp--remap x '((keyamp-escape . return-before)))
 (keyamp--set-map x '(select-line)))

(with-sparse-keymap-x
 ;; DEL SPC prefix command.
 ;; DEL SPC SPC to call `end-of-line-or-block'. Hold down SPC to repeat.
 ;; Or hit DEL SPC DEL for `prev-user-buffer' and activate screen keymap.
 (keyamp--map x '(("TAB" . eshell) ("<tab>" . eshell)))
 (keyamp--map-leaders x '(prev-user-buffer . end-of-line-or-block))
 (keyamp--remap x '((keyamp-escape . return-before)))
 (keyamp--set-map x '(select-text-in-quote)))

(advice-add-macro
 ;; If region active deactivate mark and return to the point before selection.
 '(ibuffer-backward-filter-group   ibuffer-forward-filter-group
   gnus-topic-goto-prev-topic-line gnus-topic-goto-next-topic-line
   up-line                         down-line
   beg-of-line-or-block            end-of-line-or-block
   prev-user-buffer                next-user-buffer
   View-scroll-half-page-backward  View-scroll-half-page-forward
   tasks                           novel
   terminal                        eshell) :before 'return-before)

(with-sparse-keymap-x
 ;; Left/right arrows repeat by DEL/SPC. Start with e.g. TAB DEL/SPC.
 (keyamp--map-leaders x '(backward-char . forward-char))
 (keyamp--remap x '((backward-char . left-char) (forward-char . right-char)))
 (keyamp--set-map x '(left-char right-char) nil nil nil 1))

(with-sparse-keymap-x
 ;; Repeat brackets move with DEL/SPC.
 (keyamp--map-leaders x '(backward-left-bracket . forward-right-bracket))
 (keyamp--set-map x '(backward-left-bracket forward-right-bracket)
                    nil nil nil 1))

(with-sparse-keymap-x
 ;; Repeat move by words with J/L or DEL/SPC. Second press word move key
 ;; calls punctuation move. Press O then hold down J or U/L for
 ;; convenient word move hold down, that is, start in opposite direction.
 (keyamp--map-leaders x '(backward-char . forward-char))
 (keyamp--remap x
   '((backward-char . back-word)      (forward-char . forw-word)
     (back-word     . backward-punct) (forw-word    . forward-punct)))
 (keyamp--set-map x '(back-word forw-word) nil nil nil 1))

(with-sparse-keymap-x
 ;; Repeat move by punct with J/L or DEL/SPC.
 (keyamp--map-leaders x '(backward-char . forward-char))
 (keyamp--remap x
   '((backward-char . backward-punct) (forward-char . forward-punct)))
 (keyamp--set-map x '(backward-punct forward-punct) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--remap x
   '((previous-line . scroll-down-command) (next-line . scroll-up-command)
     (down-line     . scroll-down-command) (up-line   . scroll-up-command)))
 (keyamp--set-map x '(scroll-down-command scroll-up-command)))

(with-sparse-keymap-x
 ;; Hold down H/; to initiate half page up/down. Repeat with I/K or DEL/SPC.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--map x '(("TAB" . next-line) ("<tab>" . next-line)))
 (keyamp--remap x
   '((previous-line . View-scroll-half-page-backward)
     (next-line     . View-scroll-half-page-forward)
     (down-line     . View-scroll-half-page-forward)
     (up-line       . View-scroll-half-page-backward)))
 (unless (display-graphic-p) ; reader touch
   (keyamp--remap x
     '((down-line . View-scroll-half-page-backward)
       (up-line   . View-scroll-half-page-forward))))
 (keyamp--set-map x
   '(View-scroll-half-page-backward View-scroll-half-page-forward)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(next-line . next-line))
 (keyamp--remap x '((next-line . pop-local-mark-ring)))
 (keyamp--set-map x '(pop-local-mark-ring)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(text-scale-decrease . text-scale-increase))
 (keyamp--map x '(("TAB" . text-scale-reset) ("<tab>" . text-scale-reset)))
 (keyamp--set-map x '(text-scale-decrease text-scale-increase text-scale-reset)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(backward-button . forward-button))
 (keyamp--set-map x '(backward-button forward-button)))


;; Repeat mode. Edit commands.

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-forward-char . delete-forward-char))
 (keyamp--set-map x '(delete-forward-char) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . insert-space-before))
 (keyamp--set-map x '(delete-backward insert-space-before) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--remap x '((open-line . backward-kill-word) (newline . kill-word)))
 (keyamp--set-map x '(backward-kill-word kill-word) nil nil nil 2))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(undo . delete-backward))
 (keyamp--remap x '((delete-backward . undo-redo)))
 (keyamp--set-map x '(undo undo-redo)))

(with-sparse-keymap-x
 (keyamp--remap x '((delete-backward . cut-text-block)))
 (keyamp--set-map x '(cut-text-block)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . shrink-whitespaces)))
 (keyamp--set-map x '(shrink-whitespaces) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . toggle-comment)))
 (keyamp--set-map x '(toggle-comment) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . cut-line-or-selection)))
 (keyamp--set-map x '(cut-line-or-selection) nil nil nil 1))

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
 (keyamp--map x '(("v" . paste-or-paste-previous) ("м" . paste-or-paste-previous)))
 (keyamp--set-map x '(paste-or-paste-previous)))

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
  "Insert n literally."
  (interactive)
  (keyamp-insert-init)
  (execute-kbd-macro (kbd "n")))

(defun keyamp-insert-y ()
  "Insert y literally."
  (interactive)
  (keyamp-insert-init)
  (execute-kbd-macro (kbd "y")))

(defun keyamp-insert-! ()
  "Insert ! literally."
  (interactive)
  (if (y-or-n-p "Confirm all?")
      (progn
        (keyamp-insert-init)
        (execute-kbd-macro (kbd "!")))
    (message "Abort")))

(with-eval-after-load 'minibuffer
  (with-sparse-keymap-x
   ;; On minibuffer start up press DEL or I to list history backwards or
   ;; SPC or K to list completion candidates forward. After that
   ;; I/K or DEL/SPC to list either history or completion candidates
   ;; accordingly choice made. RET to confirm and exit, ESC to quit.
   ;; To switch from history to candidates listing press ESC then double
   ;; SPC (`extend-selection') and DEL/SPC or I/K again to continue move
   ;; backward/forward. Similarly double DEL to activate history move.
   (keyamp--map-leaders x '(select-block . extend-selection))

   ;; Hit D or DEL for No, K or SPC for Yes right away to answer y or n.
   (keyamp--map x
     '(("d" . select-block)     ("в" . select-block)
       ("k" . extend-selection) ("л" . extend-selection)
       ;; Engineer Engram layout ! is QWERTY N and Russian Т
       ("N" . keyamp-insert-!)  ("Т" . keyamp-insert-!)))
   (keyamp--remap y-or-n-p-map
     '((select-block     . y-or-n-p-insert-n)
       (extend-selection . y-or-n-p-insert-y)))

   ;; The hook is last one run during minibuffer setup. Transient keymap x
   ;; gets highest priority.
   (keyamp--remap x
     '((end-of-lyne        . keyamp-insert-n) ; Engineer Engram layout,
       (backward-kill-word . keyamp-insert-y) ; remap required for others
       (keyamp-insert      . keyamp-minibuffer-insert)
       (keyamp-escape      . keyamp-minibuffer-escape)))
    (keyamp--set-map-hook x '(minibuffer-setup-hook) :command nil :repeat))

   ;; Right after paste in minibuffer mostly confirm and exit follow.
   (advice-add 'paste-or-paste-previous :after
               (lambda (&rest r) "activate insert mode if in minibuffer"
                 (when (and (minibufferp) (not keyamp-insert-p))
                   (keyamp-insert))))

  (keyamp--remap minibuffer-local-map
    '((previous-line . previous-line-or-history-element)
      (next-line     . next-line-or-history-element)
      (select-block  . previous-line-or-history-element)))

  (keyamp--map minibuffer-local-completion-map
    '(("TAB" . minibuffer-leader-map) ("<tab>" . minibuffer-leader-map)))
  (keyamp--map (define-prefix-command 'minibuffer-leader-map)
    '(("TAB" . minibuffer-complete)   ("<tab>" . minibuffer-complete)))

  (keyamp--remap minibuffer-mode-map
    '((previous-line . previous-line-or-history-element)
      (next-line     . next-line-or-history-element)
      (select-block  . previous-line-or-history-element)))

  (advice-add 'next-line-or-history-element :before
            (lambda (&rest r) "move point to the end of line beforehand"
              (goto-char (point-max)))))

(with-eval-after-load 'icomplete
  (defun keyamp-icomplete-exit ()
    "Exit if file completion. It means use content of minibuffer as it is,
  no select completion candidates. Else force complete and exit, that
  is, select and use first completion candidate. In case file
  completion, for most cases no need to complete, because there is NO
  right candidate. Otherwise, in all cases one MUST select a candidate.
  Simply hit TAB TAB to minibuffer-complete file name if the name exists."
    (interactive)
    (if (eq (icomplete--category) 'file)
        (exit-minibuffer)
      (icomplete-force-complete-and-exit)))

  (keyamp--map icomplete-minibuffer-map
    '(("RET" . keyamp-icomplete-exit) ("<return>" . keyamp-icomplete-exit)))

  (keyamp--remap icomplete-minibuffer-map
    '((previous-line    . icomplete-backward-completions)
      (next-line        . icomplete-forward-completions)
      (extend-selection . icomplete-forward-completions)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((keyamp-insert . keyamp-icomplete-exit)
       (previous-line . icomplete-backward-completions)
       (next-line     . icomplete-forward-completions)))
   (keyamp--set-map x
     '(icomplete-backward-completions icomplete-forward-completions)))

  (with-sparse-keymap-x
   (keyamp--remap x
     '((previous-line . previous-line-or-history-element)
       (next-line     . next-line-or-history-element)))
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
  (lambda () "because ido-completion-map created after ido setup only"
    (keyamp--remap ido-completion-map
      '((keyamp-insert . ido-exit-minibuffer)
        (previous-line . ido-prev-match) (select-block     . ido-prev-match)
        (next-line     . ido-next-match) (extend-selection . ido-next-match)))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(select-block . extend-selection))
 (keyamp--set-map x '(ido-prev-match ido-next-match)))

(with-eval-after-load 'dired
  (keyamp--map dired-mode-map
    '(("C-h" . dired-do-delete) ("C-r" . delete-other-windows)
      ("<mouse-1>"        . mouse-set-point)
      ("<mouse-2>"        . mouse-set-point) ; mouse-2 really mouse-1
      ("<double-mouse-1>" . dired-find-file)))

  (keyamp--remap dired-mode-map
    '((keyamp-insert           . dired-find-file)
      (insert-space-before     . dired-omit-mode)
      (kill-word               . dired-unmark-all-marks)
      (backward-kill-word      . dired-do-chmod)
      (shrink-whitespaces      . dired-hide-details-mode)
      (open-line               . dired-maybe-insert-subdir)
      (delete-backward         . dired-toggle-mark)
      (newline                 . dired-sort)
      (toggle-comment          . revert-buffer)
      (cut-line-or-selection   . dired-kill-subdir)
      (paste-or-paste-previous . dired-create-directory)
      (copy-to-register-1      . dired-do-copy)
      (paste-from-register-1   . dired-do-rename)
      (mark-whole-buffer       . dired-toggle-marks)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(dired-toggle-mark . dired-toggle-mark))
   (keyamp--set-map x '(dired-toggle-mark) nil nil nil 1)))

(with-eval-after-load 'wdired
  (keyamp--map wdired-mode-map
    '(("C-h" . wdired-abort-changes) ("C-r" . wdired-finish-edit)
      ("C-q" . wdired-abort-changes) ("C-t" . wdired-finish-edit)))
  (advice-add-macro '(wdired-abort-changes wdired-finish-edit)
                    :after 'keyamp-command))

(with-eval-after-load 'dired-utils
  (keyamp--map dired-mode-map
    '(("TAB" . dired-leader-map)               ("<tab>" . dired-leader-map)))

  (keyamp--map (define-prefix-command 'dired-leader-map)
    '(("TAB" . dired-omit-mode)                ("<tab>" . dired-omit-mode)
      ("q" . dired-image-remove-transparency)  ("e" . dired-optimize-png)
      ("u" . dired-2drawing)                   ("o" . dired-rotate-img-right)
      ("p" . dired-rotate-img-left)            ("a" . dired-image-autocrop)
      ("s" . dired-open-marked)                ("d" . dired-show-metadata)
      ("f" . dired-remove-all-metadata)        ("h" . dired-rotate-img-180)
      ("k" . dired-rename-space-to-underscore) ("l" . dired-2png)
      (";" . dired-scale-image)                ("\'" . dired-to-zip-encrypted)
      ("c" . dired-2jpg)                       ("/" . dired-to-zip))))

(advice-add 'dired-next-line :after
            (lambda (&rest r) "stay on row with file"
              (if (= (line-number-at-pos) (1+ (count-lines (point-min) (point-max))))
                  (dired-previous-line 1))))

(advice-add 'dired-previous-line :after
            (lambda (&rest r) "stay on row with file"
              (if (= (line-number-at-pos) 1) (dired-next-line 1))))

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
    '((keyamp-insert           . string-rectangle)
      (insert-space-before     . open-rectangle)
      (copy-line-or-selection  . copy-rectangle-as-kill)
      (delete-backward         . kill-rectangle)
      (paste-or-paste-previous . yank-rectangle)
      (copy-to-register        . copy-rectangle-to-register)
      (toggle-comment          . rectangle-number-lines)
      (cut-line-or-selection   . clear-rectangle)
      (clean-whitespace        . delete-whitespace-rectangle))))

(with-eval-after-load 'ibuf-ext
  (keyamp--map ibuffer-mode-map
    '(("C-h" . ibuffer-do-delete)  ("<double-mouse-1>" . ibuffer-visit-buffer)
      ("TAB" . ibuffer-leader-map) ("<tab>" . ibuffer-leader-map)))
  (keyamp--map (define-prefix-command 'ibuffer-leader-map)
    '(("TAB" . toggle-ibuffer)     ("<tab>" . toggle-ibuffer)))

  ;; Same as base map for Screen, always available in ibuffer.
  (keyamp--remap ibuffer-mode-map
    '((keyamp-insert           . ibuffer-visit-buffer)
      (end-of-lyne             . ibuffer-forward-filter-group)
      (beg-of-line             . ibuffer-backward-filter-group)
      (end-of-line-or-block    . ibuffer-forward-filter-group)
      (beg-of-line-or-block    . ibuffer-backward-filter-group)
      (insert-space-before     . delete-frame)
      (backward-kill-word      . sun-moon)
      (undo                    . delete-window)
      (kill-word               . ignore)
      (cut-text-block          . calc)

      (exchange-point-and-mark . view-echo-area-messages)
      (shrink-whitespaces      . split-window-below)
      (open-line               . prev-user-buffer)
      (delete-backward         . toggle-ibuffer)
      (newline                 . next-user-buffer)
      (set-mark-command        . new-empty-buffer)
      (cut-line-or-selection   . prev-eww-buffer)

      (copy-line-or-selection  . agenda)
      (paste-or-paste-previous . tasks)
      (backward-left-bracket   . downloads)
      (forward-right-bracket   . player)
      (kmacro-play             . config)))

  (keyamp--map ibuffer-mode-filter-group-map
    '(("C-h" . help-command) ("<mouse-1>" . mouse-set-point)
      ("<double-mouse-1>" . ibuffer-toggle-filter-group)))

  (keyamp--remap ibuffer-mode-filter-group-map
    '((keyamp-insert . ibuffer-toggle-filter-group)))

  (with-sparse-keymap-x
   (keyamp--remap x
     '((previous-line . ibuffer-backward-filter-group)
       (next-line     . ibuffer-forward-filter-group)))
   (keyamp--set-map x
     '(ibuffer-backward-filter-group ibuffer-forward-filter-group
       ibuffer-toggle-filter-group))))

(with-eval-after-load 'ibuffer
  (keyamp--map ibuffer-name-map '(("<mouse-1>" . mouse-set-point))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . ibuffer-do-delete)))
 (keyamp--set-map x '(ibuffer-do-delete) nil nil nil 1))

(with-eval-after-load 'company
  (keyamp--map company-active-map
    '(("TAB" . company-active-leader-map) ("<tab>" . company-active-leader-map)))
  (keyamp--map (define-prefix-command 'company-active-leader-map)
    '(("TAB" . company-complete-common)   ("<tab>" . company-complete-common)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((keyamp-escape   . company-abort)
       (keyamp-insert   . company-complete-selection)
       (isearch-forward . company-search-candidates)
       (previous-line   . company-select-previous)
       (next-line       . company-select-next)
       (backward-char   . company-previous-page)
       (forward-char    . company-next-page)))

   (keyamp--set-map x
     '(company-select-previous company-select-next company-previous-page
       company-next-page company-show-doc-buffer company-search-abort))

   (add-hook 'keyamp-command-hook
             (lambda ()
               (when company-candidates
                 (set-transient-map x)
                 (setq this-command 'keyamp--repeat-dummy)))))

  (with-sparse-keymap-x
   ;; Activate command mode after complete selection, but if next hit is SPC
   ;; then activate insert mode and insert SPC. DEL to undo the completion.
   (advice-add-macro '(company-search-abort company-complete-selection)
                     :after (lambda (&rest r) "`keyamp-command'"
                              (if keyamp-insert-p (keyamp-command))))

    (defun keyamp-insert-and-SPC ()
      "Activate insert mode and insert SPC."
      (interactive)
      (unless keyamp-insert-p (keyamp-insert))
      (insert " "))
    (keyamp--map-leaders x '(undo . keyamp-insert-and-SPC))
    (keyamp--set-map x '(company-search-abort company-complete-selection)))

  (advice-add 'company-search-candidates :after 'keyamp-insert-init)

  (keyamp--map company-search-map
    '(("<escape>"  . company-search-abort)
      ("TAB"       . company-search-repeat-forward)
      ("<tab>"     . company-search-repeat-forward)
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
  (keyamp--remap archive-mode-map
    '((keyamp-insert . archive-extract))))

(with-eval-after-load 'bookmark
  (keyamp--remap bookmark-bmenu-mode-map
    '((keyamp-insert . bookmark-bmenu-this-window))))

(with-eval-after-load 'button
  (keyamp--remap button-map
    '((keyamp-insert . push-button))))

(with-eval-after-load 'compile
  (keyamp--remap compilation-button-map
    '((keyamp-insert . compile-goto-error))))

(with-eval-after-load 'flymake
  (keyamp--remap flymake-diagnostics-buffer-mode-map
    '((keyamp-insert . flymake-goto-diagnostic))))

(with-eval-after-load 'org-agenda
  (keyamp--remap org-agenda-mode-map
    '((keyamp-insert . org-agenda-switch-to))))

(with-eval-after-load 'replace
  (keyamp--remap occur-mode-map
    '((keyamp-insert . occur-mode-goto-occurrence)))

  (keyamp--map query-replace-map
    '(("d" . skip) ("k" . act) ("в" . skip) ("л" . act))))

(with-eval-after-load 'shr
  (keyamp--remap shr-map
    '((keyamp-insert . shr-browse-url))))

(with-eval-after-load 'simple
  (keyamp--remap completion-list-mode-map
    '((keyamp-insert . choose-completion))))

(with-eval-after-load 'wid-edit
  (keyamp--remap widget-link-keymap
    '((keyamp-insert . widget-button-press))))

(with-eval-after-load 'emms-playlist-mode
  (keyamp--map emms-playlist-mode-map
    '(("<double-mouse-1>" . emms-playlist-mode-play-smart)))
  (keyamp--remap emms-playlist-mode-map
    '((keyamp-insert . emms-playlist-mode-play-smart))))

(with-eval-after-load 'org
  (keyamp--map org-mode-map
    '(("TAB" . org-leader-map) ("<tab>" . org-leader-map)))
  (keyamp--map (define-prefix-command 'org-leader-map)
    '(("TAB" . org-cycle)      ("<tab>" . org-cycle)))
  (keyamp--remap org-mode-map '((eval-last-sexp . insert-date)))

  (with-sparse-keymap-x
   (keyamp--map x '(("TAB" . org-cycle) ("<tab>" . org-cycle)))
   (keyamp--set-map x '(org-cycle))))

(with-eval-after-load 'eww
  (keyamp--map eww-mode-map
    '(("TAB" . eww-leader-map) ("<tab>" . eww-leader-map)))
  (keyamp--map (define-prefix-command 'eww-leader-map)
    '(("TAB"   . View-scroll-half-page-forward)
      ("<tab>" . View-scroll-half-page-forward)))

  (keyamp--remap eww-mode-map
    '((open-line       . eww-back-url) (newline   . eww-next-url)
      (delete-backward . eww-reload)   (kill-word . eww-reload-all)
      (keyamp-insert   . eww-reload)   (undo      . eww)
      (shrink-whitespaces . eww-browse-with-external-browser)))

  (keyamp--remap eww-link-keymap '((keyamp-insert . eww-follow-link)))

  (advice-add 'eww-reload      :around 'lookup-around))

(with-eval-after-load 'emms
  (with-sparse-keymap-x
   (keyamp--map-leaders x '(open-line . newline))
   (keyamp--remap x
     '((open-line       . emms-seek-backward-or-previous)
       (delete-backward . emms-pause)
       (newline         . emms-seek-forward-or-next)))

   (keyamp--set-map x
     '(emms-seek-backward-or-previous emms-seek-forward-or-next
       emms-pause))))

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
    '((previous-line   . doc-view-previous-line-or-previous-page)
      (next-line       . doc-view-next-line-or-next-page)
      (backward-char   . doc-view-previous-page)
      (forward-char    . doc-view-next-page)
      (back-word       . doc-view-shrink)
      (forw-word       . doc-view-enlarge)
      (beg-of-line     . doc-view-scroll-down-or-previous-page)
      (end-of-lyne     . doc-view-scroll-up-or-next-page)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
    (keyamp--remap x
      '((previous-line . doc-view-scroll-down-or-previous-page)
        (next-line     . doc-view-scroll-up-or-next-page)))
    (keyamp--set-map x
      '(doc-view-scroll-down-or-previous-page doc-view-scroll-up-or-next-page))))

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
  (keyamp--map eshell-mode-map
    '(("C-h" . eshell-interrupt-process)
      ("TAB" . eshell-leader-map)   ("<tab>" . eshell-leader-map)
      ("S-<tab>" . ignore)          ("<backtab>" . ignore)))
  (keyamp--map (define-prefix-command 'eshell-leader-map)
    '(("TAB" . completion-at-point) ("<tab>" . completion-at-point)))

  (keyamp--remap eshell-mode-map
    '((cut-line-or-selection . eshell-clear-input)
      (next-eww-buffer       . eshell-clear)
      (select-block          . eshell-previous-input)
      (quoted-insert         . eshell-interrupt-process)))

  (with-sparse-keymap-x
   (keyamp--map x '(("v" . paste-or-paste-previous) ("м" . paste-or-paste-previous)))
   (advice-add 'paste-or-paste-previous :after
               (lambda (&rest r) "activate insert mode in eshell"
                 (when (eq major-mode 'eshell-mode) ; vterm no
                   (keyamp-insert)
                   (set-transient-map x)))))

  (advice-add 'paste-or-paste-previous :before
              (lambda (&rest r) "go to input before paste if not in input"
                (when (eq major-mode 'eshell-mode)
                  (unless (= (line-number-at-pos)
                             (count-lines (point-min) (point-max)))
                    (goto-char (point-max))))))

  (with-sparse-keymap-x
   ;; Insert mode is primary for eshell. The keymap ready after eshell start,
   ;; command submit or cancel. Use DEL/SPC to list history, V for paste and
   ;; other commands available in insert mode right after send input.
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . eshell-previous-input)
       (next-line     . eshell-next-input)))

   (keyamp--map x
     '(("TAB"   . eshell-previous-matching-input)
       ("<tab>" . eshell-previous-matching-input)
       ("v" . paste-or-paste-previous) ("м" . paste-or-paste-previous)
       ("'" . alternate-buffer)        ("э" . alternate-buffer)
       ("[" . alternate-buf-or-frame)  ("х" . alternate-buf-or-frame)
       ("5" . terminal)))

    (keyamp--set-map x '(eshell-send-input eshell-interrupt-process))
    (keyamp--set-map x '(eshell-previous-input eshell-next-input) :command)
    (keyamp--set-map-hook x '(eshell-mode-hook) nil :insert)))

(advice-add-macro
 ;; Activate command mode after jump from insert. The commands might be run
 ;; by hold down a key or transient keymap from insert mode, mostly eshell.
 '(alternate-buf-or-frame  alternate-buffer
   delete-other-windows    delete-window
   split-window-below      dired-jump
   prev-user-buffer        next-user-buffer
   toggle-ibuffer          save-close-current-buffer)
 :after (lambda (&rest r) "`keyamp-command'"
          (if keyamp-insert-p (keyamp-command))))

;; Exception. Keep insert mode after split by `completion-at-point'.
(advice-add 'completion-at-point :after 'keyamp-insert-init)

(with-eval-after-load 'vterm
  (keyamp--map vterm-mode-map
    '(("C-h" . term-interrupt-subjob) ("C-q" . term-interrupt-subjob)
      ("C-r" . delete-other-windows)  ("C-t" . delete-other-windows)
      ("C-u" . vterm-send-next-key)
      ("TAB" . vterm-leader-map)      ("<tab>" . vterm-leader-map)
      ("<backtab>" . ignore)          ("S-<tab>" . ignore)))

  (keyamp--map (define-prefix-command 'vterm-leader-map)
    '(("TAB" . vterm-send-tab) ("<tab>" . vterm-send-tab)))

  (keyamp--remap vterm-mode-map
    '((select-block            . vterm-send-up)
      (prev-eww-buffer         . vterm-clear)
      (paste-or-paste-previous . vterm-yank)
      (paste-from-register-1   . vterm-yank-pop)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . vterm-send-up)
       (next-line     . vterm-send-down)))

   (keyamp--map x
     '(("v" . paste-or-paste-previous) ("м" . paste-or-paste-previous)
       ("'" . alternate-buffer)        ("э" . alternate-buffer)
       ("[" . alternate-buf-or-frame)  ("х" . alternate-buf-or-frame)
       ("9" . eshell)))

    (keyamp--set-map x '(vterm-send-return))
    (keyamp--set-map x '(vterm-send-up vterm-send-down) :command)
    (keyamp--set-map-hook x '(vterm-mode-hook) nil :insert)))

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
      (newline         . help-go-forward))))

(with-eval-after-load 'gnus-topic
  (keyamp--map gnus-topic-mode-map
    '(("<double-mouse-1>" . gnus-topic-select-group)
      ("TAB" . gnus-topic-leader-map) ("<tab>" . gnus-topic-leader-map)))
  (keyamp--map (define-prefix-command 'gnus-topic-leader-map)
    '(("TAB" . toggle-ibuffer)        ("<tab>" . toggle-ibuffer)))

  (keyamp--remap gnus-topic-mode-map
    '((keyamp-insert        . gnus-topic-select-group)
      (end-of-lyne          . gnus-topic-goto-next-topic-line)
      (beg-of-line          . gnus-topic-goto-prev-topic-line)
      (end-of-line-or-block . gnus-topic-goto-next-topic-line)
      (beg-of-line-or-block . gnus-topic-goto-prev-topic-line)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . gnus-topic-goto-prev-topic-line)
       (next-line     . gnus-topic-goto-next-topic-line)
       (beg-of-line   . gnus-beg-of-line-or-buffer)
       (end-of-lyne   . gnus-end-of-line-or-buffer)))

   (keyamp--set-map x
     '(gnus-topic-goto-prev-topic-line gnus-topic-goto-next-topic-line
       gnus-beg-of-line-or-buffer      gnus-end-of-line-or-buffer
       gnus-topic-select-group))))

(with-eval-after-load 'gnus-group
  (keyamp--remap gnus-group-mode-map
    '((undo            . gnus-group-enter-server-mode)
      (delete-backward . gnus-group-get-new-news)
      (open-line       . prev-user-buffer)
      (newline         . next-user-buffer))))

(with-eval-after-load 'gnus-art
  (keyamp--remap gnus-mime-button-map
    '((keyamp-insert . gnus-article-press-button)))
  (keyamp--remap gnus-article-mode-map
    '((undo             . backward-button)
      (delete-backward  . forward-button)))

(with-eval-after-load 'gnus-sum
  (keyamp--map gnus-summary-mode-map
    '(("C-h"              . gnus-summary-delete-article)
      ("<double-mouse-1>" . gnus-summary-scroll-up)))

  (keyamp--remap gnus-summary-mode-map
    '((keyamp-insert . gnus-summary-scroll-up)
      (open-line     . gnus-summary-prev-group)
      (newline       . gnus-summary-next-group)
      (save-buffer   . gnus-summary-save-parts))))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(open-line . newline))
   (keyamp--remap x
     '((open-line . gnus-summary-prev-group)
       (newline   . gnus-summary-next-group)))
   (keyamp--set-map x
     '(gnus-summary-prev-group gnus-summary-next-group))
   (keyamp--set-map-hook x '(gnus-summary-prepared-hook))))

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
      (kmacro-play             . recentf-open-most-recent-file-1)
      (number-to-register      . recentf-open-most-recent-file-2)
      (append-to-register-1    . recentf-open-most-recent-file-3)
      (toggle-case-fold-search . recentf-open-most-recent-file-4)
      (kmacro-helper           . recentf-open-most-recent-file-5)
      (eshell                  . recentf-open-most-recent-file-6)
      (kmacro-record           . recentf-open-most-recent-file-7)
      (pass                    . recentf-open-most-recent-file-8)
      (terminal                . recentf-open-most-recent-file-9))))

(with-eval-after-load 'snake
  (keyamp--remap snake-mode-map
    '((keyamp-insert        . snake-start-game)
      (keyamp-escape        . snake-pause-game)
      (delete-backward      . snake-move-up)
      (next-line            . snake-move-down)
      (delete-other-windows . snake-rotate-up)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(snake-move-left . snake-move-right))
   (keyamp--set-map x
     '(snake-start-game snake-pause-game
       snake-move-left  snake-move-right
       snake-move-down  snake-move-up))
    (keyamp--set-map-hook x '(snake-mode-hook))))

(with-eval-after-load 'tetris
  (keyamp--remap tetris-mode-map
    '((keyamp-escape   . tetris-pause-game)
      (delete-backward . tetris-rotate-prev) (delete-other-windows . tetris-rotate-prev)
      (newline         . tetris-rotate-next) (next-user-buffer     . tetris-rotate-next)
      (next-line       . tetris-move-bottom)
      (backward-char   . tetris-move-down)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(tetris-move-left . tetris-move-right))
   (keyamp--set-map x
     '(tetris-start-game  tetris-pause-game
       tetris-move-left   tetris-move-right
       tetris-rotate-prev tetris-rotate-next
       tetris-move-bottom tetris-move-down))))

(with-eval-after-load 'find-replace
  (keyamp--map find-output-mode-map
    '(("TAB"       . find-output-leader-map) ("<tab>"   . find-output-leader-map)
      ("<backtab>" . find-previous-match)    ("S-<tab>" . find-previous-match)))
  (keyamp--map (define-prefix-command 'find-output-leader-map)
    '(("TAB" . find-next-match) ("<tab>" . find-next-match)))

  (keyamp--remap find-output-mode-map
    '((keyamp-insert . find--jump-to-place)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(backward-char . forward-char))
   (keyamp--map x '(("TAB" . find-next-match) ("<tab>" . find-next-match)))
   (keyamp--remap x
     '((previous-line . find-previous-file)  (next-line    . find-next-file)
       (backward-char . find-previous-match) (forward-char . find-next-match)))
   (keyamp--set-map x
     '(find-next-match     find-previous-file
       find-previous-match find-next-file))))

(with-eval-after-load 'emacs-lisp-mode
  (keyamp--map emacs-lisp-mode-map
    '(("TAB" . emacs-lisp-leader-map) ("<tab>" . emacs-lisp-leader-map)
      ("S-<tab>" . ignore)            ("<backtab>" . ignore)))

  (keyamp--map (define-prefix-command 'emacs-lisp-leader-map)
    '(("TAB"   . emacs-lisp-complete-or-indent)
      ("<tab>" . emacs-lisp-complete-or-indent)
      ("d" . emacs-lisp-remove-paren-pair)
      ("k" . emacs-lisp-add-paren-around-symbol)
      ("f" . emacs-lisp-compact-parens))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(backward-char . forward-char))
 (keyamp--remap x
   '((backward-char . flymake-goto-prev-error)
     (forward-char  . flymake-goto-next-error)))
 (keyamp--set-map x '(flymake-goto-prev-error flymake-goto-next-error)
                    nil nil nil 1))

(with-eval-after-load 'python-mode
  (keyamp--map python-mode-map
    '(("TAB" . python-leader-map)         ("<tab>"     . python-leader-map)
      ("RET" . python-return-and-indent)  ("<return>"  . python-return-and-indent)
      ("S-<tab>" . python-de-indent)      ("<backtab>" . python-de-indent)))
  (keyamp--map (define-prefix-command 'python-leader-map)
    '(("TAB" . python-indent-or-complete) ("<tab>" . python-indent-or-complete)))

  (with-sparse-keymap-x
   (keyamp--map x
    '(("TAB" . python-indent-or-complete) ("<tab>" . python-indent-or-complete)))
   (keyamp--set-map x '(python-indent-or-complete python-de-indent)))

  (keyamp--remap python-mode-map
    '((newline     . python-return-and-indent)
      (toggle-gnus . python-format-buffer))))

(with-eval-after-load 'go-ts-mode
  (keyamp--map go-ts-mode-map
    '(("TAB" . go-ts-leader-map)     ("<tab>" . go-ts-leader-map)
      ("S-<tab>" . ignore)           ("<backtab>" . ignore)))
  (keyamp--map (define-prefix-command 'go-ts-leader-map)
    '(("TAB" . company-manual-begin) ("<tab>" . company-manual-begin)))

  (keyamp--remap go-ts-mode-map
    '((describe-foo-at-point . xref-find-definitions)
      (describe-variable     . xref-find-references)
      (mark-defun            . go-mark-defun)
      (eval-defun            . go-vet-project)
      (eval-last-sexp        . server))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(xref-go-back . xref-find-definitions))
 (keyamp--set-map x '(xref-go-back xref-find-definitions)))

(with-eval-after-load 'xref
  (keyamp--remap xref--xref-buffer-mode-map
    '((keyamp-insert . xref-show-location-at-point))))

(with-eval-after-load 'sh-script
  (keyamp--map bash-ts-mode-map
    '(("TAB" . bash-ts-mode-leader-map) ("<tab>" . bash-ts-mode-leader-map)))
  (keyamp--map (define-prefix-command 'bash-ts-mode-leader-map)
    '(("TAB" . indent-for-tab-command)  ("<tab>" . indent-for-tab-command)))
  (keyamp--map sh-mode-map ; shebang prevents ts mode auto load
    '(("TAB" . sh-mode-map-leader-map) ("<tab>" . sh-mode-map-leader-map)))
  (keyamp--map (define-prefix-command 'sh-mode-map-leader-map)
    '(("TAB" . indent-for-tab-command)  ("<tab>" . indent-for-tab-command))))

(with-eval-after-load 'sqlite-mode
  (keyamp--remap sqlite-mode-map
    '((keyamp-insert   . sqlite-mode-list-data)
      (delete-backward . sqlite-mode-delete)
      (newline         . sqlite-mode-list-columns)
      (open-line       . sqlite-mode-list-tables))))

(with-eval-after-load 'sql
  (keyamp--remap sql-mode-map '((eval-defun . exec-query))))

(with-eval-after-load 'speedbar
  (keyamp--remap speedbar-file-key-map
    '((keyamp-insert   . speedbar-edit-line)
      (newline         . speedbar-up-directory)
      (delete-backward . speedbar-toggle-line-expansion))))



(defconst keyamp-screen-commands-hash
  #s(hash-table test equal data
                (agenda                           t
                 alternate-buffer                 t
                 async-shell-command              t
                 config                           t
                 delete-other-windows             t
                 delete-window                    t
                 dired-jump                       t
                 downloads                        t
                 exec-query                       t

                 find-next-dir-file               t
                 find-prev-dir-file               t
                 forw-frame                       t
                 gnus-summary-next-group          t
                 gnus-summary-prev-group          t
                 next-buffer                      t
                 next-eww-buffer                  t
                 next-proj-buffer                 t
                 next-user-buffer                 t
                 open-file-at-cursor              t
                 player                           t
                 previous-buffer                  t
                 prev-frame                       t
                 prev-eww-buffer                  t
                 prev-proj-buffer                 t
                 prev-user-buffer                 t
                 proced                           t
                 run-current-file                 t
                 save-close-current-buffer        t

                 server                           t
                 split-window-below               t
                 sun-moon                         t
                 sync                             t
                 tasks                            t
                 test                             t
                 view-echo-area-messages          t)))

(defconst keyamp-edit-commands-hash
  #s(hash-table test equal data
                (delete-backward                  t
                 delete-forward-char              t
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
                 undo                             t
                 undo-redo                        t)))

(defconst keyamp-repeat-commands-hash
  #s(hash-table test equal data
                (backward-punct                   t
                 backward-button                  t
                 back-word                        t
                 beg-of-line-or-block             t
                 beg-of-line-or-buffer            t
                 backward-left-bracket            t
                 company-select-previous          t
                 company-select-next              t
                 company-next-page                t
                 company-previous-page            t

                 copy-line-or-selection           t
                 dired-mark                       t
                 dired-unmark                     t
                 down-line                        t
                 end-of-line-or-block             t
                 end-of-line-or-buffer            t
                 eshell-next-input                t
                 eshell-previous-input            t
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
                 gnus-beg-of-line-or-buffer       t
                 gnus-end-of-line-or-buffer       t
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

                 pop-local-mark-ring              t
                 previous-line-or-history-element t
                 right-char                       t
                 scroll-down-command              t
                 scroll-up-command                t
                 select-block                     t
                 isearch-cur-word-forward         t
                 isearch-cur-word-backward        t
                 select-line                      t
                 select-text-in-quote             t
                 text-scale-decrease              t
                 text-scale-increase              t
                 text-scale-reset                 t
                 up-line                          t
                 View-scroll-half-page-backward   t
                 View-scroll-half-page-forward    t
                 vterm-send-down                  t
                 vterm-send-up                    t)))



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
  (keyamp-indicate))

(defun keyamp-insert-init ()
  "Enter insert mode."
  (setq keyamp-insert-p t)
  (funcall keyamp--deactivate-command-mode-fun))

(defun keyamp-set-var-karabiner (key val)
  "Set karabiner variable KEY to value VAL via shell command."
  (call-process keyamp-karabiner-cli nil 0 nil
                "--set-variables" (concat "{\"" key "\":" val "}")))

(when (file-exists-p keyamp-karabiner-cli)
  (add-hook 'keyamp-insert-hook
            (lambda () (keyamp-set-var-karabiner "insert mode activated" "1")))
  (add-hook 'keyamp-command-hook
            (lambda () (keyamp-set-var-karabiner "insert mode activated" "0"))))

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

(defun keyamp-indicate ()
  "Indicate keyamp state. Run with `post-command-hook'."
  (cond
   ((gethash this-command keyamp-screen-commands-hash)
    (setq mode-line-front-space keyamp-screen-indicator)
    (set-face-background 'cursor keyamp-screen-cursor)
    (setq keyamp-repeat-p t))
   ((or (eq real-this-command 'repeat)
        (and (gethash this-command keyamp-repeat-commands-hash)
             (not keyamp-insert-p)))
    (setq mode-line-front-space keyamp-repeat-indicator)
    (set-face-background 'cursor keyamp-repeat-cursor)
    (setq keyamp-repeat-p t))
   ((gethash this-command keyamp-edit-commands-hash)
    (setq mode-line-front-space keyamp-insert-indicator)
    (set-face-background 'cursor keyamp-insert-cursor)
    (setq keyamp-repeat-p t))
   (keyamp-insert-p
    (setq mode-line-front-space keyamp-insert-indicator)
    (set-face-background 'cursor keyamp-insert-cursor)
    (modify-all-frames-parameters '((cursor-type . bar))))
   (t
    (setq mode-line-front-space keyamp-command-indicator)
    (set-face-background 'cursor keyamp-command-cursor)
    (setq keyamp-repeat-p nil)
    (modify-all-frames-parameters '((cursor-type . box)))))
  (unless (eq this-command last-command)
    (force-mode-line-update t)))

(defun keyamp-hold-down ()
  "Do something after key hold down. Run after idle time same as Karabiner
held down threshold. E.g. hold down ESC Karabiner posts C-h and Keyamp
indicates the event after been idle for the same period of time."
  (cond
   ((equal [8] (this-single-command-keys)) ; C-h
    (setq mode-line-front-space keyamp-screen-indicator)
    (set-face-background 'cursor keyamp-screen-cursor)
    (force-mode-line-update t))))

(defun keyamp-escape ()
  "Return to command mode, clear selection or quit minibuffer."
  (interactive)
  (cond
   ((or keyamp-repeat-p
        keyamp-insert-p) (keyamp-command))
   ((region-active-p)    (deactivate-mark))
   ((minibufferp)        (abort-recursive-edit))))



;;;###autoload
(define-minor-mode keyamp
  "Keyboard Amplifier."
  :global t
  :keymap keyamp-map
  (when keyamp
    (add-hook 'minibuffer-exit-hook  'keyamp-command)
    (add-hook 'isearch-mode-end-hook 'keyamp-command)
    (add-hook 'debugger-mode-hook    'keyamp-command)
    (add-hook 'post-command-hook     'keyamp-indicate)
    (add-hook 'keyamp-insert-hook    'keyamp-cancel-repeat-mode-idle-timer)
    (add-hook 'keyamp-command-hook   'keyamp-cancel-repeat-mode-idle-timer)
    (add-hook 'isearch-mode-hook     'keyamp-cancel-repeat-mode-idle-timer)
    (add-function :after after-focus-change-function #'keyamp-command)

    (keyamp-catch-tty-ESC)
    (keyamp-command)
    (run-with-timer 5 nil 'keyamp-map-input-source 'russian-computer)
    (run-with-timer 5 nil 'keyamp-push-quail-keyboard-layout)
    (setq keyamp-idle-timer
          (run-with-idle-timer keyamp-idle-timeout t 'keyamp-escape))
    (setq keyamp-hold-down-timer
          (run-with-idle-timer keyamp-hold-threshold t 'keyamp-hold-down))))

(provide 'keyamp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical unresolved)
;; End:
;;; keyamp.el ends here
