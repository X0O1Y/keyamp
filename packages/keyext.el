;;; keyext.el --- Key extensions -*- coding: utf-8; lexical-binding: t; -*-

;; This package is a fork of xah-fly-keys.

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

(defun up-line ()
  "Previous line. On the first line go to the last line of buffer."
  (interactive)
  (if (= (line-number-at-pos) 1)
      (forward-line (count-lines (point-min) (point-max)))
    (forward-line -1)))

(defun down-line ()
  "Next line. On the last line go to the first line of buffer."
  (interactive)
  (forward-line)
  (if (>= (line-number-at-pos) (+ 1 (count-lines (point-min) (point-max))))
      (forward-line (- (count-lines (point-min) (point-max))))))

(defun pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'."
  (interactive)
  (set-mark-command t)
  (setq this-command 'pop-local-mark-ring))

(defun set-mark-deactivate-mark ()
  "Set the mark where point is, and deactivate it."
  (interactive)
  (set-mark-command nil)
  (deactivate-mark))

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
            (equal [backspace] (this-command-keys))
            (= 127 (aref (this-command-keys) 0))) ; DEL repeat
        (when
            (re-search-backward "\n[\t\n ]*\n+" nil 1)
          (skip-chars-backward "\n\t ")
          (forward-char)
          (recenter))
      (if visual-line-mode
          (beginning-of-visual-line)
        (if (eq major-mode 'eshell-mode)
            (progn
              ;; custom eshell bol
              (if (= (line-number-at-pos) (count-lines (point-min) (point-max)))
                  (progn
                    (beginning-of-line)
                    (forward-word )
                    (forward-char ))
                (beginning-of-line)))
          (back-to-indentation)
          (when (eq xp (point))
            (beginning-of-line)))))))

(defun end-of-line-or-block ()
  "Move cursor to end of line or next block.

• When called first time, move cursor to end of line;
• When called again, move cursor forward by jumping over any sequence
  of whitespaces containing 2 blank lines;
• If `visual-line-mode' is on, end of line means visual line."
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command)
          (equal " " (this-command-keys))) ; SPC repeat
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil 1)
        (recenter))
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
     ((prog2 (backward-char) (looking-at (regexp-opt right-brackets)) (forward-char))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(defun sort-lines-block-or-region ()
  "Like `sort-lines' but if no region, do the current block."
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
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
    (let ((xbds (get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (narrow-to-region xp1 xp2)))

(defun back-word ()
  "Wrap for `backward-word' to use in transient remaps."
  (interactive)
  (backward-word))

(defun forw-word ()
  "Wrap for `forward-word' to use in transient remaps."
  (interactive)
  (forward-word))


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
                 (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                 nil)
                (progn
                  (end-of-line)
                  (forward-char))))
          (if (eobp)
              (if (eq (char-before) 10)
                  (progn)
                (progn
                  (copy-region-as-kill (line-beginning-position) (line-end-position))
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

;; (defun cut-all ()
;;   "Cut buffer content to `kill-ring'. Respects `narrow-to-region'."
;;   (interactive)
;;   (kill-new (buffer-string))
;;   (delete-region (point-min) (point-max)))

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
  (interactive)
  (progn
    (forward-sexp -1)
    (mark-sexp)
    (kill-region (region-beginning) (region-end))))

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
  (interactive)
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
  (interactive)
  (if (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end))
    (cond
     ((looking-back "\\s)" 1)
      (if current-prefix-arg
          (cut-bracket-pair)
        (cut-bracket-text)))
     ((looking-back "\\s(" 1)
      (message "Left of cursor is opening bracket")
      (let (xpOpenBracketLeft
            (xpOpenBracketRight (point)) xisComment)
        (backward-char)
        (setq xpOpenBracketLeft (point))
        (goto-char xpOpenBracketRight)
        (forward-char)
        (setq xisComment (nth 4 (syntax-ppss)))
        (if xisComment
            (progn
              (message "Cursor is in comment")
              (goto-char xpOpenBracketLeft)
              (if (forward-comment 1)
                  (kill-region (point) xpOpenBracketLeft)
                (message "Error hSnRp: parsing comment failed")))
          (progn
            (message "Right 1 char of cursor is not in comment")
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
  (let (xok)
    (unwind-protect
        (progn
          (cut-bracket)
          (setq xok t))
      (unless xok
        (if (looking-back "\\s)" 1)
            (progn
              (delete-char -1))
          (progn
            (delete-char 1)))))))

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
            "` markdown GRAVE ACCENT `"
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
    (let ((xbds (get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
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

(defun toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower."
  (interactive)
  (let ((deactivate-mark nil) xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq xp1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq xp2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
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

;; test case
;; test_case some
;; test-case
;; tes▮t-case

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
    (let ((xbds (get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
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
        (when
            (or
             (eq major-mode 'html-mode)
             (eq major-mode 'html-mode)
             (eq major-mode 'sgml-mode)
             (eq major-mode 'nxml-mode)
             (eq major-mode 'xml-mode)
             (eq major-mode 'mhtml-mode))
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
                    (skip-chars-backward xskipChars (line-beginning-position)) (point)))))
         (xp2 (if End
                  End
                (if (region-active-p)
                    (region-end)
                  (progn (goto-char xp0)
                         (skip-chars-forward xskipChars (line-end-position)) (point)))))
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
    (let ((xbds (get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
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
         (xspace-neighbor-p (or (eq xcharBefore 32) (eq xcharBefore 9) (eq xcharAfter 32) (eq xcharAfter 9))))
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
          (delete-blank-lines)
          (insert "\n"))))
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

(defun toggle-read-novel-mode ()
  "Setup current frame to be suitable for reading long novel/article text.

• Set frame width to 70;
• Line wrap at word boundaries;
• Line spacing is increased;
• Proportional width font is used.

Call again to toggle back."
  (interactive)
  (if (eq (frame-parameter (selected-frame) 'width) 80)
      (progn
        (set-frame-parameter (selected-frame) 'width 93)
        (variable-pitch-mode 0)
        (setq line-spacing nil)
        (setq word-wrap nil))
    (progn
      (set-frame-parameter (selected-frame) 'width 80)
      (variable-pitch-mode 1)
      (setq line-spacing 0.5)
      (setq word-wrap t)))
  (redraw-frame (selected-frame)))

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
    (let ((xbds (get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
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
      (while (re-search-forward "\\([A-Za-z0-9]+\\)[ \t]*\n[ \t]*\\([A-Za-z0-9]+\\)" nil t)
        (replace-match "\\1 \\2"))
      (goto-char (point-min))
      (while (re-search-forward "\\([,]\\)[ \t]*\n[ \t]*\\([A-Za-z0-9]+\\)" nil t)
        (replace-match "\\1 \\2"))
      (goto-char (point-min))
      (while (re-search-forward "  +" nil t) (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "\\([.?!]\\) +\\([(0-9A-Za-z]+\\)" nil t) (replace-match "\\1\n\\2"))
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
  ;; This function sets a property 'state. Possible values are 0 to length of xcharArray.
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
        (while (re-search-forward (elt xcharArray (% (+ xnowState 2) xn)) (point-max) 1)
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
           (message "Copy: %s" (file-name-directory xfpath))
           (file-name-directory xfpath))
       (progn
         (message "Copy: %s" xfpath)
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
    (message "Copy to register 1:\n%s" (buffer-substring-no-properties xp1 xp2))))

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
    (message "Append to register 1:\n%s" (buffer-substring-no-properties xp1 xp2))))

(defun paste-from-register-1 ()
  "Paste text from register 1.
See also: `copy-to-register-1', `insert-register'."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t)
  (message "Paste from register 1"))

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
• If WrapMethod is `line', wrap around line;
• If WrapMethod is `block', wrap around block;
• If cursor is at beginning of line and its not empty line and contain
  at least 1 space, wrap around the line;
• If cursor is at end of a word or buffer, one of the following will happen:
 xyz▮ → xyz(▮)
 xyz▮ → (xyz▮)
if in one of the lisp modes;
• Wrap brackets around word if any. e.g. xy▮z → (xyz▮). Or just (▮)."
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

(defun insert-string-assignment ()
  "Insert =\"\""
  (interactive)
  (progn (insert "=\"\"")
         (left-char)))

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
       ((looking-at "\\s(")
        ;; (message "left bracket")
        (mark-sexp)) ; left bracket
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
        (looking-back "[-_a-zA-Z0-9]" (max (- (point) 1) (point-min)))
        ;; (message "left is word or symbol")
        (skip-chars-backward "-_a-zA-Z0-9")
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (push-mark)
        (skip-chars-forward "-_a-zA-Z0-9")
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
        (push-mark (point)  t t)
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
This command ignores nesting. For example, if text is
    (a(b)c▮)
the selected char is “c”, not “a(b)c”.

Supports one extra level: second call for \"<f6▮>\" will select <f6>."
  (interactive)
  (let ((xskipChars (concat "^\"`'" (mapconcat #'identity keyext-brackets ""))))
    (if (eq last-command this-command)
        (progn
          (deactivate-mark)
          (skip-chars-backward xskipChars)
          (backward-char)
          (skip-chars-backward xskipChars)
          (push-mark (point) t t)
          (skip-chars-forward xskipChars)
          (forward-char)
          (skip-chars-forward xskipChars)
          (forward-char)
          (skip-chars-forward xskipChars))
      (progn
        (skip-chars-backward xskipChars)
        (push-mark (point) t t)
        (skip-chars-forward xskipChars)))))


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
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "eww-mode") nil)
   ((string-equal major-mode "help-mode") nil)
   ((string-equal major-mode "nov-mode") nil)
   ((string-equal major-mode "doc-view-mode") nil)
   ((string-equal major-mode "diary-mode") nil)
   ((string-equal (buffer-name) "tetris-scores") nil)
   ((string-equal buffer-file-truename org-agenda-file-1) nil)
   ((string-equal buffer-file-truename org-agenda-file-2) nil)
   ((string-equal buffer-file-truename org-agenda-file-3) nil)
   ((string-match ".+em/project+." default-directory) nil)
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
    (switch-to-buffer-other-window xbuf)
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
    (progn (message "No recently close buffer in this session"))))

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



(defvar run-current-file-before-hook nil "Hook for `run-current-file'. Before the file is run.")

(defvar run-current-file-after-hook nil "Hook for `run-current-file'. After the file is run.")

(defun run-current-go-file ()
  "Run or build current Go file.
To build, call `universal-argument' first."
  (interactive)
  (when (not buffer-file-name) (save-buffer))
  (when (buffer-modified-p) (save-buffer))
  (let* (
         (xoutputb "*run output*")
         ;; (resize-mini-windows nil)
         (xfname buffer-file-name)
         ;; (xfSuffix (file-name-extension xfname))
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
      (shell-command xcmdStr xoutputb )
      ;;
      )))

(defconst run-current-file-map
  '(("go" . "go run")
    ("hs" . "runhaskell")
    ("js" . "deno run")
    ("pl" . "perl")
    ("py" . "python3")
    ("rb" . "ruby")
    ("sh" . "bash")
    ("ts" . "deno run"))
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
  (catch 'confirm
    (if (y-or-n-p "Do you really want to run current file?")
        (progn
          (setenv "NO_COLOR" "1") ; 2022-09-10 for deno. Default color has yellow parts, hard to see
          (when (not buffer-file-name) (save-buffer))
          (let* ((xoutBuffer "*run output*")
                 ;; (resize-mini-windows nil)
                 (xextAppMap run-current-file-map)
                 (xfname buffer-file-name)
                 (xfExt (file-name-extension xfname))
                 (xappCmdStr (cdr (assoc xfExt xextAppMap)))
                 xcmdStr)
            ;; FIXME: Rather than `shell-command' with an `&', better use
            ;; `make-process' or `start-process' since we're not using the shell at all
            ;; (worse, we need to use `shell-quote-argument' to circumvent the shell).
            (setq xcmdStr
                  (when xappCmdStr
                    (format "%s %s &"
                            xappCmdStr
                            (shell-quote-argument xfname))))
            (when (buffer-modified-p) (save-buffer))
            (run-hooks 'run-current-file-before-hook)
            (cond
             ((string-equal xfExt "el")
              (load xfname))
             ((string-equal xfExt "go")
              (run-current-go-file))
             ((string-match "\\.\\(ws?l\\|m\\|nb\\)\\'" xfExt)
              (if (fboundp 'run-wolfram-script)
                  (progn
                    (run-wolfram-script nil current-prefix-arg))
                (if xappCmdStr
                    (progn
                      (message "Running")
                      (shell-command xcmdStr xoutBuffer))
                  (error "%s: Unknown file extension: %s" real-this-command xfExt))))
             ((string-equal xfExt "java")
              (progn
                (shell-command (format "javac %s" xfname) xoutBuffer)
                (shell-command (format "java %s" (file-name-sans-extension
                                                  (file-name-nondirectory xfname)))
                               xoutBuffer)))
             (t (if xappCmdStr
                    (progn
                      (message "Running %s" xcmdStr)
                      (shell-command xcmdStr xoutBuffer))
                  (error "%s: Unknown file extension: %s" real-this-command xfExt))))

            (run-hooks 'run-current-file-after-hook))
          (setenv "NO_COLOR")
          (throw 'confirm t))
      (progn
        (message "Abort run file")
        nil))))

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
        (while (eq (char-before) 32) (delete-char -1)))))
  (if (buffer-modified-p) (message "%s" "Cleanup whitespaces")))

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

(defun delete-current-file-make-backup ()
  "Delete current file, makes a backup~, close the buffer.
If buffer is not a file, copy content to `kill-ring', delete buffer.

Backup filename is “‹name›~‹dateTimeStamp›~”. Existing file of the
same name is overwritten. If buffer is not a file, the backup file
name starts with “xx_”.

Call `open-last-closed' to open the backup file."
  (interactive)
  (if (string-equal 'dired-mode major-mode)
      (message "In Dired. Nothing is done")
    (let* ((xfname (buffer-file-name))
           (xbackupPath
            (concat (if xfname xfname (format "%sxx" default-directory))
                    (format "~%s~" (format-time-string "%Y-%m-%d_%H%M%S")))))
      (if xfname
          (progn
            (save-buffer xfname)
            (copy-file xfname xbackupPath t)
            (when (boundp 'recently-closed-buffers)
              (push (cons nil xbackupPath) recently-closed-buffers))
            (message "Deleted. Backup at \n%s\nCall `open-last-closed' to open." xbackupPath)
            (delete-file xfname))
        (progn
          (widen)
          (kill-new  (buffer-string))))
      (kill-buffer (current-buffer)))))



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

(defun search-current-word ()
  "Call `isearch' on current word or “word” here is A to Z, a to z, and
hyphen [-] and lowline [_], independent of syntax table."
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq xp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq xp2 (point))))
    (setq mark-active nil)
    (when (< xp1 (point))
      (goto-char xp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties xp1 xp2))))

(declare-function w32-shell-execute "w32fns.c"
                  (operation document &optional parameters show-flag)) ; (w32-shell-execute "open" default-directory)

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
    (setq xdoIt (if (<= (length xfileList) 5) t (y-or-n-p "Open more than 5 files? ")))
    (when xdoIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (xfpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" "
                                  "'" (shell-quote-argument (expand-file-name xfpath)) "'")))
         xfileList))
       ((and (string-equal major-mode 'dired-mode)
             (string-match "Sound" (dired-get-filename)))
        (emms-play-dired))
       ((string-equal system-type "darwin")
        (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath))
                                              " && echo")) xfileList)
        (message ""))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (xfpath)
                (call-process shell-file-name nil 0 nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument xfpath))))
              xfileList))
       ((string-equal system-type "berkeley-unix")
        (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList))))))

(defun open-in-terminal ()
  "Open the current dir in a new terminal window.
On Microsoft Windows, it starts cross-platform PowerShell pwsh. You
need to have it installed."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (let ((xcmdstr
           (format "pwsh -Command Start-Process pwsh -WorkingDirectory %s" (shell-quote-argument default-directory))))
      (shell-command xcmdstr)))
   ((string-equal system-type "darwin")
    (shell-command (concat "open -a terminal "
                           (shell-quote-argument (expand-file-name default-directory))
                           " && echo Open in terminal")))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))
   ((string-equal system-type "berkeley-unix")
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))))

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
    (kill-line)))

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
        (toggle-gnus)
      (progn
        (switch-to-buffer xbuf)
        (ibuffer)
        (unless (member (buffer-name (cadr (buffer-list)))
                        ibuffer-never-show-predicates)
          (ibuffer-jump-to-buffer xbuf))))))

(defun flyspell-goto-prev-error ()
  "Go to prev error."
  (interactive)
  (flyspell-goto-next-error t))

(defun icomplete-exit-or-force-complete-and-exit ()
  "Exit if file completion. It means use content of minibuffer as it is, no
select completion candidates. Else force complete and exit, that is select
and use first completion candidate.
In case file completion, most cases no need to complete, because there is NO
right candidate. Otherwise, in almost all cases one MUST select a candidate."
  (interactive)
  (if (eq (icomplete--category) 'file)
      (exit-minibuffer)
    (icomplete-force-complete-and-exit)))

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
  "Send weather forecast."
  (call-process "~/.weather/run.sh" nil 0 nil))

(defun books ()
  "Read book."
  (interactive)
  (if (string-equal (buffer-name) current-book)
      (progn
        (switch-to-buffer (other-buffer))
        (toggle-theme))
    (progn
      (if (get-buffer current-book)
          (progn
            (switch-to-buffer current-book)
            (toggle-theme))
        (progn
          (find-file (concat "~/Books/" current-book))
          (toggle-theme))))))

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
  (org-todo)
  (unless (string-equal (buffer-name) "study")
    (beginning-of-line)
    (title-case-region-or-line)
    (beginning-of-line)))

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
    (progn
      (message "~/.sync.sh: started.")
      (async-shell-command "~/.sync.sh" "*sync output*"))))

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

(defun server-run ()
  "Run project server. Switch to console if already running."
  (interactive)
  (let ((xpath (getenv "SERVER_RUN"))
        (xbuf (concat "*run server*")))
    (if (get-buffer xbuf)
        (switch-to-buffer-other-window xbuf)
      (async-shell-command (format "cd %s && go run main.go" xpath) xbuf))))

(defun test-run ()
  "Run project tests."
  (interactive)
  (async-shell-command "$TEST_RUN"))

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
  (when (not (memq (car data) '(buffer-read-only
                                text-read-only
                                beginning-of-buffer
                                end-of-buffer
                                quit)))
    (command-error-default-function data context caller)))

(defun disable-kill-buffer-query-functions (kill-func &rest args)
  "Disable confirmation conditionally before buffer kill."
  (let ((x kill-buffer-query-functions))
    (when (string-match "run server" (buffer-name))
      (setq kill-buffer-query-functions nil))
    (unwind-protect
        (apply kill-func args)
      (setq kill-buffer-query-functions x))
    (setq kill-buffer-query-functions x)))

(defun disable-func (func &rest args)
  "Conditionally disable FUNC. Run as advice."
  (unless (minibufferp)
      (apply func args)))

(defun byte-compile-package ()
  "Byte compile current package."
  (if (string-match ".+emacs.d/packages+." (buffer-file-name))
      (byte-compile-file (expand-file-name (buffer-file-name)))))

(defun terminal ()
  "Run terminal emulator."
  (interactive)
  (vterm))

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

(defun run-at-time-wrap (time func &rest args)
  "Wrap for `run-at-time'."
  (if (and (equal (format-time-string "%H") (substring time 0 2))
           (equal (format-time-string "%M") (substring time 3 5)))
      (apply func args)))

(defun dired-recursive-expand (dir)
  "Recursive expand dired dir."
  (interactive (list default-directory))
  (mapc #'dired-maybe-insert-subdir
        (seq-filter #'file-directory-p (directory-files-recursively dir "" t))))

(defun json-pretty ()
  "Pretty buffer if json, echo message."
  (interactive)
  (if (string-equal (file-name-extension buffer-file-name) "json")
      (progn
        (json-pretty-print-buffer)
        (message "%s" "Pretty print json"))
    (message "%s" "Not json")))

(defun org-insert-source-code ()
  "Insert source code block."
  (interactive)
  (org-insert-structure-template "src")
  (newline))

(defun deactivate-mark-before-move (&rest r)
  "If region active deactivate mark conditionally and return to the line
before selection. This func to be run as before advice for move func."
  (interactive)
  (when (and (region-active-p)
             (or (eq last-command 'select-block)
                 (eq last-command 'extend-selection)
                 (eq last-command 'select-line)
                 (eq last-command 'select-text-in-quote)))
    (deactivate-mark)
    (when (eq last-command 'select-block)
      (set-mark-command t)
      (set-mark-command t))))

(defmacro advice-add-macro (SymList How Function)
  "Map `advice-add' over a list SYMLIST to FUNCTION."
  `(progn
     ,@(mapcar
        (lambda (xcmd)
          `(advice-add ,(list 'quote xcmd) ,How ,Function))
        (cadr SymList))))

(advice-add-macro '(scroll-down-command
                    scroll-up-command
                    isearch-repeat-backward
                    isearch-repeat-forward
                    prev-user-buffer
                    next-user-buffer
                    prev-proj-buffer
                    next-proj-buffer
                    find-previous-match
                    find-next-match)
                  :after (lambda (&rest r) "recenter" (recenter)))

(add-hook 'replace-update-post-hook 'recenter)

(provide 'keyext)

;; Local Variables:
;; byte-compile-warnings: (not lexical unresolved)
;; End:
;;; keyext.el ends here
