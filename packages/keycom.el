;;; keycom.el --- Keyboard commands -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; Commands for Keyboard Amplifier.

;;; Code:


;; Cursor movement

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

(defun up-line-wrap ()
  "On the first line go to the last line of buffer."
  (if (= (line-number-at-pos) 1) (forward-line (count-lines (point-min) (point-max)))))

(defun down-line-wrap (fun &rest r)
  "On the last line go to the first line of buffer."
  (if (= (line-number-at-pos) (+ 1 (count-lines (point-min) (point-max))))
      (forward-line (- (count-lines (point-min) (point-max))))
    (apply fun r)))

(defconst before-last-command-event-threshold (if (display-graphic-p) 0 70)
  "Threshold for `before-last-command-event'. Give time to paste into terminal.")
(defconst before-last-command-event-timeout 300
  "Timeout for `before-last-command-event'.")
(defvar before-last-command-event nil "Before `last-command-event'.")

(defconst before-last-command-timeout 200 "Timeout for `before-last-command' in ms.")
(defvar before-last-command nil "Command before last command.")

(defun before-last-command ()
  "Set and cancel `before-last-command'."
  (setq before-last-command last-command)
  (run-with-timer (/ before-last-command-timeout 1000.0) nil
                  (lambda () (setq before-last-command nil))))

(defvar last-command-keys nil "Last command keys.")

(defun up-line ()
  "Up line for transient use."
  (interactive)
  (if (equal before-last-command 'backward-char)
      (progn (setq this-command 'back-char) (back-char))
    (if (equal before-last-command this-command)
        (cond
         ((eq major-mode 'ibuffer-mode)
          (setq this-command 'ibuffer-backward-filter-group)
          (ibuffer-backward-filter-group))
         ((eq major-mode 'gnus-group-mode)
          (setq this-command 'gnus-topic-prev) (gnus-topic-prev))
         (t (if (equal last-command-keys "t") ; else SPC
                (progn (setq this-command 'page-up-half) (page-up-half))
              (setq this-command 'beg-of-block) (beg-of-block))))
      (command-execute 'previous-line)
      (if (eq last-command 'down-line) (before-last-command))))
  (setq last-command-keys (this-command-keys)))

(defun down-line ()
  "Down line for transient use."
  (interactive)
  (if (equal before-last-command 'forward-char)
      (progn (setq this-command 'forw-char) (forw-char))
    (if (equal before-last-command this-command)
        (cond
         ((eq major-mode 'ibuffer-mode)
          (setq this-command 'ibuffer-forward-filter-group)
          (ibuffer-forward-filter-group))
         ((eq major-mode 'gnus-group-mode)
          (setq this-command 'gnus-topic-next) (gnus-topic-next))
         (t (if (equal last-command-keys "d") ; else DEL
                (progn (setq this-command 'page-dn-half) (page-dn-half))
              (setq this-command 'end-of-block) (end-of-block))))
      (command-execute 'next-line)
      (if (and (eq major-mode 'gnus-summary-mode)
               (> (line-number-at-pos) 2))
          (command-execute 'next-line))
      (if (eq last-command 'up-line) (before-last-command))))
  (setq last-command-keys (this-command-keys)))

(defun beginning-of-visual-line-once (&rest _)
  "Call to `beginning-of-visual-line'."
  (unless (or (equal before-last-command 'backward-char)
              (equal before-last-command 'forward-char)
              (eq major-mode 'dired-mode))
    (beginning-of-visual-line)))

(advice-add 'up-line   :before 'up-line-wrap)
(advice-add 'down-line :around 'down-line-wrap)
(advice-add 'up-line   :after 'beginning-of-visual-line-once)
(advice-add 'down-line :after 'beginning-of-visual-line-once)

(defun goto-point-max (&rest _)
  "Go to point max if not there."
  (unless (eq (point) (point-max)) (goto-char (point-max))))

(advice-add 'hist-forw :before 'goto-point-max)

(defun comp-back ()
  "Completion backward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (setq this-command 'abort-recursive-edit) (abort-recursive-edit))
    (command-execute 'icomplete-backward-completions)
    (if (eq last-command 'comp-forw) (before-last-command))))

(defun comp-forw ()
  "Completion forward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (setq this-command 'abort-recursive-edit) (abort-recursive-edit))
    (command-execute 'icomplete-forward-completions)
    (if (eq last-command 'comp-back) (before-last-command))))

(defun hist-back ()
  "History backward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (setq this-command 'abort-recursive-edit) (abort-recursive-edit))
    (command-execute 'previous-line-or-history-element)
    (if (eq last-command 'hist-forw) (before-last-command))))

(defun hist-forw ()
  "History forward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (setq this-command 'abort-recursive-edit) (abort-recursive-edit))
    (command-execute 'next-line-or-history-element)
    (if (eq last-command 'hist-back) (before-last-command))))

(defun company-select-back ()
  "Company complete backward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (setq this-command 'company-abort) (company-abort))
    (command-execute 'company-select-previous)
    (if (eq last-command 'company-select-forw) (before-last-command))))

(defun company-select-forw ()
  "Company complete forward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (setq this-command 'company-abort) (company-abort))
    (command-execute 'company-select-next)
    (if (eq last-command 'company-select-back) (before-last-command))))

(defun button-back ()
  "Button backward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (setq this-command 'backward-button) (backward-button 1 nil nil t))
    (backward-button 1 nil nil t)
    (if (eq last-command 'button-forw) (before-last-command))))

(defun button-forw ()
  "Button forward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (setq this-command 'forward-button) (forward-button 1 nil nil t))
    (forward-button 1 nil nil t)
    (if (eq last-command 'button-back) (before-last-command))))

(defun isearch-back ()
  "Isearch backward for transient use."
  (interactive)
  (if (or (equal before-last-command this-command)
          (equal before-last-command 'isearch-cur-word-backward))
      (progn (setq this-command 'isearch-cancel) (isearch-cancel))
    (command-execute 'isearch-repeat-backward)
    (if (or (eq last-command 'isearch-forw)
            (eq last-command 'isearch-cur-word-forward))
        (before-last-command))))

(defun isearch-forw ()
  "Isearch forward for transient use."
  (interactive)
  (if (or (equal before-last-command this-command)
          (equal before-last-command 'isearch-cur-word-forward))
      (progn (setq this-command 'isearch-cancel) (isearch-cancel))
    (command-execute 'isearch-repeat-forward)
    (if (or (eq last-command 'isearch-back)
            (eq last-command 'isearch-cur-word-backward))
        (before-last-command))))

(defun jump-mark ()
  "Move cursor to last mark position of current buffer, but not point.
Call this repeatedly will cycle all positions in `mark-ring'.
Save point to register 6 before repeated call."
  (interactive)
  (unless (eq this-command last-command) (point-to-register ?6))
  (let ((xp (point)))
    (set-mark-command t) (if (eql xp (point)) (set-mark-command t))))

(defun jump-six ()   "Jump to register six."
       (interactive) (if (get-register ?6) (jump-to-register ?6)))
(defun jump-seven () "Jump to register seven."
       (interactive) (if (get-register ?7) (jump-to-register ?7)))
(defun jump-eight () "Jump to register eight."
       (interactive) (if (get-register ?8) (jump-to-register ?8)))

(defun beg-of-line ()
  "Move cursor to beginning of line."
  (interactive)
  (let ((xp (point)))
    (if visual-line-mode
        (beginning-of-visual-line)
      (if (eq major-mode 'eshell-mode) ; custom eshell bol
          (if (= (line-number-at-pos) (count-lines (point-min) (point-max)))
              (progn (beginning-of-line) (forward-char 4))
            (beginning-of-line))
        (back-to-indentation)
        (if (eq xp (point)) (beginning-of-line))))))

(defun end-of-lyne ()
  "End of line or visual line."
  (interactive)
  (if visual-line-mode (end-of-visual-line) (end-of-line)))

(defun back-block ()
  "Move cursor to the end of prev block."
  (interactive)
  (when (re-search-backward "\n[\t\n ]*\n+" nil 1)
    (skip-chars-backward "\n\t ") (forward-char))
  (if (eq last-command 'end-of-block)
      (when (re-search-backward "\n[\t\n ]*\n+" nil 1)
        (skip-chars-backward "\n\t ") (forward-char)))
  (if (eq major-mode 'dired-mode) (dired-previous-line 1)))

(defun forw-block ()
  "Move cursor to the beginning of next block."
  (interactive)
  (re-search-forward "\n[\t\n ]*\n+" nil 1)
  (if (eq major-mode 'dired-mode) (dired-next-line 1)))

(defun beg-of-block ()
  "Back block. Fast double direction switch to prev buf."
  (interactive)
  (if (equal before-last-command this-command)
      (if (equal last-command-keys "t") ; else SPC
          (progn (setq this-command 'page-up-half) (page-up-half))
        (command-execute 'back-block)
        (setq this-command 'dummy) (command-execute 'dummy))
    (command-execute 'back-block)
    (if (eq last-command 'end-of-block) (before-last-command)))
  (setq last-command-keys (this-command-keys)))

(defun end-of-block ()
  "Forw block."
  (interactive)
  (if (equal before-last-command this-command)
      (if (equal last-command-keys "d") ; else DEL
          (progn (setq this-command 'page-dn-half) (page-dn-half))
        (command-execute 'forw-block)
        (setq this-command 'dummy) (command-execute 'dummy))
    (command-execute 'forw-block)
    (if (eq last-command 'beg-of-block) (before-last-command)))
  (setq last-command-keys (this-command-keys)))

(defun beg-of-buf ()
  "Go to the beginning of buffer, next press to the end of buffer."
  (interactive)
  (if (= (point) (point-min))
      (unless (eq major-mode 'eshell-mode)
        (goto-char (point-max)) (forward-line -1))
    (goto-char (point-min)))
  (if (eq major-mode 'dired-mode) (dired-next-line 1)))

(defun end-of-buf ()
  "Go to the end of buffer, next call to the beginning of buffer."
  (interactive)
  (if (or (= (count-lines 1 (point)) (count-lines (point-min) (point-max)))
          (= (count-lines 1 (point)) (1- (count-lines (point-min) (point-max)))))
      (goto-char (point-min))
    (goto-char (point-max))
    (unless (eq major-mode 'eshell-mode) (forward-line -1))))

(defvar brackets '("()" "[]" "{}" "<>" "“”")
  "A list of strings, each element is a string of 2 chars, the left
bracket and a matching right bracket. Used by `select-quote'
and others.")

(defconst left-brackets (mapcar (lambda (x) (substring x 0 1)) brackets)
  "List of left bracket chars. Each element is a string.")

(defconst right-brackets (mapcar (lambda (x) (substring x 1 2)) brackets)
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

(defun backward-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `left-brackets'."
  (interactive)
  (re-search-backward (regexp-opt left-brackets) nil t))

(defun forward-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `right-brackets'."
  (interactive)
  (re-search-forward (regexp-opt right-brackets) nil t))

(defun goto-match-br ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `left-brackets'
and `right-brackets'."
  (interactive)
  (if (equal "%" (this-command-keys))
      ;; see equal sign mapping for russian, so because of conflict
      ;; while for Engram it is free and can be remapped:
      (progn (setq this-command 'text-scale-increase) (text-scale-increase 1))
    (if (nth 3 (syntax-ppss))
        (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
      (cond
       ((eq (char-after) ?\") (forward-sexp))
       ((eq (char-before) ?\") (backward-sexp))
       ((looking-at (regexp-opt left-brackets)) (forward-sexp))
       ((prog2 (backward-char)
            (looking-at (regexp-opt right-brackets)) (forward-char))
        (backward-sexp))
       (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING))))))

(defun sort-lines-block-or-region ()
  "Like `sort-lines' but if no region, do the current block."
  (interactive)
  (let* ((xbds (get-bounds-of-block-or-region)) (xp1 (car xbds)) (xp2 (cdr xbds)))
    (sort-lines current-prefix-arg xp1 xp2)))

(defun sort-lines-key-value (Beg End)
  "Sort key-values pairs by value."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region Beg End)
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

(defun dummy () "Dummy command." (interactive))

(defun back-word ()
  "Backward word. Fast double direction switch breaks beat to char move."
  (interactive)
  (if (member (this-command-keys) (list "l" [1075])) (push-mark (point) t)) ; virtual leader
  (if (equal before-last-command this-command)
      (progn (backward-word) (setq this-command 'dummy) (command-execute 'dummy))
    (command-execute 'backward-word)
    (if (eq last-command 'forw-word) (before-last-command))))

(defun forw-word ()
  "Forward word."
  (interactive)
    (if (member (this-command-keys) (list "w" [1097])) (push-mark (point) t)) ; virtual leader
  (if (equal before-last-command this-command)
      (progn (forward-word) (setq this-command 'dummy) (command-execute 'dummy))
    (command-execute 'forward-word)
    (if (eq last-command 'back-word) (before-last-command))))

(defun back-char ()
  "Backward char."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (command-execute 'backward-char)
             (setq this-command 'dummy) (command-execute 'dummy))
    (command-execute 'left-char)
    (if (eq last-command 'forw-char) (before-last-command))))

(defun forw-char ()
  "Forward char."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (command-execute 'forward-char)
             (setq this-command 'dummy) (command-execute 'dummy))
    (command-execute 'right-char)
    (if (eq last-command 'back-char) (before-last-command))))

(defun activate-region ()
  "Select region. If region active, then exchange point and mark."
  (interactive)
  (if (region-active-p)
      (if (eq (mark) (point))
          (progn (set-mark-command t) (exchange-point-and-mark))
        (exchange-point-and-mark))
    (set-mark-command nil)))

(defun deactivate-region ()
  "Command `deactivate-mark'."
  (interactive) (deactivate-mark))

(defun page-up-half ()
  "Page up half. Direction fast switch to cancel transient move."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (command-execute 'View-scroll-half-page-backward)
             (setq this-command 'dummy) (command-execute 'dummy))
    (command-execute 'View-scroll-half-page-backward)
    (if (eq last-command 'page-dn-half) (before-last-command)))
  (beginning-of-visual-line-once))

(defun page-dn-half ()
  "Page dn half."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (command-execute 'View-scroll-half-page-forward)
             (setq this-command 'dummy) (command-execute 'dummy))
    (command-execute 'View-scroll-half-page-forward)
    (if (eq last-command 'page-up-half) (before-last-command)))
  (beginning-of-visual-line-once))

(advice-add 'mark-defun :after 'exchange-point-and-mark)


;; Editing commands

(defun del-word ()
  "If next symbol not part of a word, delete the symbol. Otherwise kill
characters forward until encountering the end of the word."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (if (looking-at "[[:blank:][:alpha:][:digit:]]+")
        (kill-word 1)
      (kill-region (point) (progn (forward-char 1) (point))))))

(defun backward-del-word ()
  "If prev symbol not part of a word, delete the symbol. Otherwise kill
characters backward until encountering the end of the word."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (if (looking-back "[[:blank:][:alpha:][:digit:]]+" 1)
        (backward-kill-word 1)
      (kill-region (point) (progn (backward-char 1) (point))))))

(defun copy-text-block ()
  "Copy text block to register 1."
  (interactive)
  (if (fboundp 'uncentered-cursor) (uncentered-cursor))
  (select-block)
  (sit-for 0.1)
  (copy-to-register ?1 (region-beginning) (region-end))
  (double-jump-back)
  (if (fboundp 'centered-cursor) (centered-cursor)))

(defun copy-selection (&rest _)
  "Copy selection."
  (if (region-active-p) (copy-region-as-kill (region-beginning) (region-end))))

(defun copy-line ()
  "Copy current line or selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer
(respects `narrow-to-region')."
  (interactive)
  (let ((inhibit-field-text-motion nil))
    (if (region-active-p)
        (copy-region-as-kill (region-beginning) (region-end))
      (if (eq last-command this-command)
          (unless (eobp)
            (kill-append "\n" nil)
            (kill-append
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position)) nil)
            (end-of-line)
            (forward-char))
        (if (eobp)
            (unless (eq (char-before) 10)
              (copy-region-as-kill (line-beginning-position) (line-end-position))
              (end-of-line))
          (copy-region-as-kill (line-beginning-position) (line-end-position))
          (end-of-line)
          (forward-char)))))
  (return-before-copy))

(defun cut-line ()
  "Cut current line or selection."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (if (region-active-p)
        (kill-region (region-beginning) (region-end) t)
      (kill-region (line-beginning-position) (line-beginning-position 2)))))

(defun copy-all ()
  "Copy buffer content to `kill-ring'. Respects `narrow-to-region'."
  (interactive)
  (kill-new (buffer-string)) (message "Buffer content copy"))

(defun paste-or-prev ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.
When `universal-argument' is called first with a number arg,
paste that many times."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (progn
      (if (and delete-selection-mode (region-active-p))
          (delete-region (region-beginning) (region-end)))
      (if current-prefix-arg
          (progn (dotimes (_ (prefix-numeric-value current-prefix-arg)) (yank)))
        (if (eq real-last-command this-command) (yank-pop 1) (push-mark (point) t) (yank))))))

(defconst show-kill-ring-separator (concat "\n\n" (make-string 77 95) "\n\n")
  "A line divider for `show-kill-ring'.")

(defun show-kill-ring ()
  "Insert all `kill-ring' content in a new buffer named *copy stack*."
  (interactive)
  (let ((xbuf (generate-new-buffer "*copy stack*")) (inhibit-read-only t))
    (progn (switch-to-buffer xbuf)
           (mapc (lambda (x) (insert x show-kill-ring-separator)) kill-ring))
    (goto-char (point-min))))

(defun cut-bracket-text ()
  "Delete the matching brackets/quotes to the left of cursor,
including the inner text.
This command assumes the left of cursor is a right bracket, and there
is a matching one before it.
What char is considered bracket or quote is determined by current syntax table."
  (forward-sexp -1) (mark-sexp)
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
      (progn (mark-sexp) (kill-region (region-beginning) (region-end)))
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
      (if current-prefix-arg (cut-bracket-pair) (cut-bracket-text)))
     ((looking-back "\\s(" 1)
      ;; (message "Left of cursor is opening bracket")
      (let (xpOpenBracketLeft (xpOpenBracketRight (point)) xisComment)
        (backward-char)
        (setq xpOpenBracketLeft (point))
        (goto-char xpOpenBracketRight)
        (forward-char)
        (setq xisComment (nth 4 (syntax-ppss)))
        (if xisComment
            (progn ;; (message "Cursor is in comment")
              (goto-char xpOpenBracketLeft)
              (if (forward-comment 1)
                  (kill-region (point) xpOpenBracketLeft)
                (message "Error hSnRp: parsing comment failed")))
          (progn ;; (message "Right 1 char of cursor is not in comment")
            (goto-char xpOpenBracketLeft)
            (forward-sexp)
            (if current-prefix-arg (cut-bracket-pair) (cut-bracket-text))))))
     ((looking-back "\\s\"" 1)
      (if (nth 3 (syntax-ppss))
          (progn (backward-char) (cut-forward-bracket-pairs (not current-prefix-arg)))
        (if current-prefix-arg (cut-bracket-pair) (cut-bracket-text))))
     (t (kill-region (point) (progn (backward-char 1) (point)))))))

(defun del-back ()
  "Try cut bracket. If error, then delete char."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (let (ok)
      (unwind-protect
          (progn (cut-bracket) (push-mark (point) t) (setq ok t))
        (unless ok
          (if (looking-back "\\s)" 1)
              (kill-region (point) (progn (backward-char 1) (point)))
            (kill-region (point) (progn (forward-char 1) (point)))))))))

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
            " none ")))
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
                    (re-search-forward (format "%s\\([^%s]+?\\)%s" xx xx xx) nil t)
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
  (let ((case-fold-search nil)) (equal (upcase s) s)))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Cycle in this order: Init Caps, ALL CAPS, all lower. Calculates initial state."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (let ((deactivate-mark nil) xp1 xp2)
      (unless (eq last-command this-command) (put this-command 'state 0))
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
      (cond ((equal 0 (get this-command 'state))
             (upcase-initials-region xp1 xp2)
             (put this-command 'state 1))
            ((equal 1 (get this-command 'state))
             (upcase-region xp1 xp2)
             (put this-command 'state 2))
            ((equal 2 (get this-command 'state))
             (downcase-region xp1 xp2)
             (put this-command 'state 0))))))

(defun toggle-prev-letter-case ()
  "Toggle the letter case of the letter to the left of cursor."
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
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
         (xp1 (if Begin Begin
                (if (region-active-p)
                    (region-beginning)
                  (skip-chars-backward xskipChars (line-beginning-position))
                  (point))))
         (xp2 (if End End
                (if (region-active-p)
                    (region-end)
                  (goto-char xp0)
                  (skip-chars-forward xskipChars (line-end-position))
                  (point))))
         (xstrPairs [
                     [" A " " a "] [" An " " an "] [" And " " and "]
                     [" At " " at "] [" As " " as "] [" By " " by "]
                     [" Be " " be "] [" Into " " into "] [" In " " in "]
                     [" Is " " is "] [" It " " it "] [" For " " for "]
                     [" Of " " of "] [" Or " " or "] [" On " " on "]
                     [" Via " " via "] [" The " " the "] [" That " " that "]
                     [" To " " to "] [" Vs " " vs "] [" With " " with "]
                     [" From " " from "] ["'S " "'s "] ["'T " "'t "]

                     [" От " " от "] [" Для " " для "] [" И " " и "]
                     [" К " " к "] [" С " " с "] [" По " " по "]
                     [" В " " в "] [" На " " на "] [" Из " " из "]
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
             (while (search-forward (aref xx 0) nil t)
               (replace-match (aref xx 1) t t)))
           xstrPairs))))))

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
  "Delete space, tab around cursor."
  (interactive)
  (let (xp1 xp2)
    (skip-chars-forward " \t")
    (setq xp2 (point))
    (skip-chars-backward " \t")
    (setq xp1 (point))
    (delete-region xp1 xp2)))

(defun shrink-whitespaces ()
  "Remove whitespaces around cursor.
Shrink neighboring spaces, then newlines, then spaces again, leaving
one space or newline at each step, till no more white space."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (let* ((xeol-count 0) (xp0 (point))
           xp1 ; whitespace begin
           xp2 ; whitespace end
           (xcharBefore (char-before)) (xcharAfter (char-after))
           (xspace-neighbor-p (or (eq xcharBefore 32) (eq xcharBefore 9)
                                  (eq xcharAfter 32) (eq xcharAfter 9))))
      (skip-chars-backward " \n\t")
      (setq xp1 (point))
      (goto-char xp0)
      (skip-chars-forward " \n\t")
      (setq xp2 (point))
      (goto-char xp1)
      (while (search-forward "\n" xp2 t) (setq xeol-count (1+ xeol-count)))
      (goto-char xp0)
      (cond
       ((eq xeol-count 0)
        (if (> (- xp2 xp1) 1)
            (progn (delete-horizontal-space) (insert " "))
          (delete-horizontal-space)))
       ((eq xeol-count 1)
        (if xspace-neighbor-p
            (delete-spaces)
          (progn (delete-blank-lines) (insert " "))))
       ((eq xeol-count 2)
        (if xspace-neighbor-p (delete-spaces) (delete-blank-lines)))
       ((> xeol-count 2)
        (if xspace-neighbor-p
            (delete-spaces)
          (progn (goto-char xp2)
                 (search-backward "\n")
                 (delete-region xp1 (point))
                 (insert "\n"))))
       (t (message "Nothing done. Logic error 40873. Should not reach here"))))))

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
  (interactive) (let ((fill-column 90002000)) (fill-paragraph)))

(defun unfill-region (Begin End)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r") (let ((fill-column 90002000)) (fill-region Begin End)))

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
    (setq xisLong (if (eq last-command this-command) (get this-command 'is-long-p)))
    (let ((xbds (get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
    (if current-prefix-arg
        (reformat-to-multi-lines xp1 xp2)
      (if xisLong
          (reformat-to-multi-lines xp1 xp2)
        (reformat-whitespaces-to-one-space xp1 xp2)))
    (put this-command 'is-long-p (not xisLong))))

(defun reformat-to-sentence-lines ()
  "Reformat current block or selection into multiple lines by ending period.
Move cursor to the beginning of next text block."
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
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
      (while (re-search-forward "\\([.?!]\\) +\\([(0-9A-Za-zА-Яа-я]+\\)" nil t)
        (replace-match "\\1\n\\2"))
      (goto-char (point-max))
      (while (eq (char-before) 32) (delete-char -1))))
  (re-search-forward "\n+" nil 1))

(defun space-to-newline ()
  "Replace space sequence to a newline char in current block or selection."
  (interactive)
  (let* ((xbds (get-bounds-of-block-or-region)) (xp1 (car xbds)) (xp2 (cdr xbds)))
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
        (while (search-forward "/" nil t) (replace-match "\\\\"))))))

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
        (while (search-forward "\\" nil t) (replace-match "/"))))))

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
        (while (search-forward "\\\\"  nil t) (replace-match "\\\\"))))))

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
        (while (search-forward "/" nil t) (replace-match "\\\\\\\\"))))))

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
        (while (search-forward "\\\\" nil t) (replace-match "/"))))))

(defun toggle-comment ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (if (region-active-p)
        (comment-dwim nil)
      (let ((xlbp (line-beginning-position)) (xlep (line-end-position)))
        (if (eq xlbp xlep)
            (comment-dwim nil)
          (if (eq (point) xlep)
              (comment-dwim nil)
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
           '("\"double quote\""
             "'single quote'"
             "(paren)"
             "{brace}"
             "[square]"
             "`markdown`"
             "none"
             "other"))
          xbktChoice xsep xsepChoice xquoteL xquoteR)
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
            (if (eq (point) (point-max)) (throw 'EndReached t) (forward-char))))))))

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
      (while (search-forward "\"" nil t) (replace-match "\\\"" t t)))))

(defun unescape-quotes (Begin End)
  "Replace  \\\" by \" in current line or selection. See also: `escape-quotes'."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t) (replace-match "\"" t t)))))

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
  (let* (xp1 xp2
         (xcharArray ["-" "_" " "])
         (xn (length xcharArray))
         (xregionWasActive-p (region-active-p))
         (xnowState (if (eq last-command this-command)
                        (get 'cycle-hyphen-lowline-space 'state) 0))
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
             (let ((xresult (mapconcat #'identity
                                       (dired-get-marked-files) "\n")))
               (if (equal (length xresult) 0)
                   (progn default-directory)
                 (progn xresult)))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (file-name-directory xfpath)
       (progn (message "%s" xfpath) xfpath)))))

(defun cut-text-block ()
  "Cut text block plus blank lines or selection."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (let (xp1 xp2)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (if (re-search-backward "\n[ \t]*\n+" nil 1)
            (setq xp1 (goto-char (match-end 0)))
          (setq xp1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil 1)
            (setq xp2 (match-end 0))
          (setq xp2 (point-max))))
      (kill-region xp1 xp2))))

(defun clear-r1 ()
  "Clear register 1. See also: `paste-from-r1', `copy-to-register'."
  (interactive)
  (copy-to-register ?1 (point-min) (point-min)) (message "Clear register"))

(defun copy-to-r1 ()
  "Copy current line or selection to register 1.
See also: `paste-from-r1', `copy-to-register'."
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (copy-to-register ?1 xp1 xp2)
    (message "Copy register"))
  (return-before-copy))

(defun append-to-r1 ()
  "Append current line or selection to register 1.
When no selection, append current line, with newline char.
See also: `paste-from-r1', `copy-to-register'."
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
         (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (append-to-register ?1 xp1 xp2)
    (with-temp-buffer (insert "\n")
                      (append-to-register ?1 (point-min) (point-max)))
    (message "Append register")))

(defun paste-from-r1 ()
  "Paste text from register 1. See also: `copy-to-r1', `insert-register'."
  (interactive)
  (when (region-active-p) (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun isearch-yank-r1 ()
  "Pull string from register-1 into search string."
  (interactive)
  (unless isearch-mode (isearch-mode t))
  (with-temp-buffer
    (insert-register ?1 t)
    (isearch-yank-string
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun beg-of-buffer (fun &rest r)
  "Go to the beginning of buffer if no selection."
  (save-excursion
    (unless (use-region-p) (goto-char (point-min)))
    (apply fun r)))

(advice-add 'query-replace :around #'beg-of-buffer)
(advice-add 'query-replace-regexp :around #'beg-of-buffer)


;; Insertion commands

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
              '("ISO date: 2018-04-12"
                "ISO full: 2018-04-12T22:46:11-07:00"
                "ISO space: 2018-04-12 22:46:11-07:00"
                "org mode:  <2018-04-12 Thu>"
                "all digits:  20180412224611"
                "weekday:  2018-04-12 Thursday"))
           "org mode: <2018-04-12 Thu>")))
    (when (region-active-p) (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((string-match "^ISO date" xstyle) (format-time-string "%Y-%m-%d"))
      ((string-match "^org mode" xstyle)
       (concat " <" (let ((xmin (number-to-string
                                 (- (string-to-number (format-time-string "%M"))
                                    (mod (string-to-number
                                          (format-time-string "%M")) 5)))))
                      (concat (format-time-string "%Y-%m-%d %a %H:")
                              (if (= 1 (length xmin)) "0") xmin)) ">"))
      ((string-match "^all digits" xstyle) (format-time-string "%Y%m%d%H%M%S"))
      ((string-match "^ISO full" xstyle)
       (concat
        (format-time-string "%Y-%m-%dT%T")
        (funcall (lambda (xx) (format "%s:%s" (substring xx 0 3)
                                      (substring xx 3 5)))
                 (format-time-string "%z"))))
      ((string-match "^ISO space" xstyle)
       (concat
        (format-time-string "%Y-%m-%d %T")
        (funcall (lambda (xx) (format "%s:%s" (substring xx 0 3)
                                      (substring xx 3 5)))
                 (format-time-string "%z"))))
      ((string-match "^weekday" xstyle) (format-time-string "%Y-%m-%d %A"))
      (t (format-time-string "%Y-%m-%d")))))
  (when (eq major-mode 'org-mode)
    (beginning-of-line)
    (title-case-region-or-line)
    (end-of-line)))

(defun insert-bracket-pair (LBracket RBracket &optional WrapMethod)
  "Insert brackets around selection, word, at point, and maybe move cursor
 in between.
LBracket and RBracket are strings. WrapMethod must be either `line' or
`block'. `block' means between empty lines.

- if there is a an active region, add brackets around region;
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
      (let ((xp1 (region-beginning)) (xp2 (region-end)))
        (goto-char xp2) (insert RBracket)
        (goto-char xp1) (insert LBracket)
        (goto-char (+ xp2 2)))
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
          (let ((xbds (get-bounds-of-block-or-region)))
            (setq xp1 (car xbds) xp2 (cdr xbds)))
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

(defun insert-paren ()              (interactive) (insert-bracket-pair "(" ")"))
(defun insert-square-bracket ()     (interactive) (insert-bracket-pair "[" "]"))
(defun insert-brace ()              (interactive) (insert-bracket-pair "{" "}"))
(defun insert-backtick-quote ()     (interactive) (insert-bracket-pair "`" "`"))
(defun insert-double-curly-quote () (interactive) (insert-bracket-pair "“" "”"))
(defun insert-double-angle-quote () (interactive) (insert-bracket-pair "«" "»"))
(defun insert-ascii-double-quote () (interactive) (insert-bracket-pair "\"" "\""))
(defun insert-ascii-single-quote () (interactive) (insert-bracket-pair "'" "'"))
(defun insert-emacs-quote ()        (interactive) (insert-bracket-pair "`" "'"))

(defun insert-space-before ()
  "Insert space before cursor."
  (interactive)
  (if buffer-read-only (setq this-command 'ignore) (insert " ")))

(defun insert-formfeed ()
  "Insert a form feed char (codepoint 12)."
  (interactive) (insert "\n\u000c\n"))

(defun insert-column-a-z ()
  "Insert letters A to Z vertically, similar to `rectangle-number-lines'.
The commpand will prompt for a start char, and number of chars to insert.
The start char can be any char in Unicode."
  (interactive)
  (let ((xstartChar (string-to-char (read-string "Start char: " "a")))
        (xhowmany (string-to-number (read-string "How many: " "26")))
        (xcolpos (- (point) (line-beginning-position))))
    (dotimes (xi xhowmany)
      (progn (insert-char (+ xi xstartChar))
             (forward-line)
             (beginning-of-line)
             (forward-char xcolpos)))))


;; Text selection

(defun select-block ()
  "Select the current/next block plus 1 blank line.
If region is active, extend selection downward by block."
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
    (push-mark (point) t nil)
    (skip-chars-forward " \n\t")
    (when (re-search-backward "\n[ \t]*\n" nil 1) (goto-char (match-end 0)))
    (push-mark (point) t t)
    (re-search-forward "\n[ \t]*\n" nil 1)))

(defun select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line."
  (interactive)
  (if (bolp)
      (progn (setq this-command 'prev-buf) (prev-buf))
    (push-mark (point) t nil)
    (if (region-active-p)
        (if visual-line-mode
            (let ((xp1 (point)))
              (end-of-visual-line 1)
              (if (eq xp1 (point)) (end-of-visual-line 2)))
          (forward-line 1)
          (end-of-line))
      (if visual-line-mode
          (progn (beginning-of-visual-line)
                 (push-mark (point) t t)
                 (end-of-visual-line))
        (push-mark (line-beginning-position) t t)
        (end-of-line)))))

(defvar cur-hl-regexp nil "Current highlight regexp.")

(defun select-word ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.
If cursor is on a any type of bracket, select whole bracketed thing
including bracket, else select current word."
  (interactive)
  (push-mark (point) t nil)
  (cond
   ((= (point) (point-min))
    (push-mark (point) t t)
    (skip-chars-forward "-_a-zа-яA-ZА-Я0-9"))
   ((looking-at "\\s(") ; left bracket
    (mark-sexp))
   ((looking-back ")" (- (point) 1)) ; right bracket
    (goto-match-br)
    (mark-sexp))
   ((looking-at "\\s)") ; right bracket
    (backward-up-list)
    (mark-sexp))
   ((looking-back "\"" (max (- (point) 1) (point-min))) ; string quote back
    (goto-match-br)
    (mark-sexp))
   ((looking-at "\\s \"") ; string quote after space
    (mark-sexp))
   ((looking-back "[-_a-zа-яA-ZА-Я0-9]"
                  (max (- (point) 1) (point-min))) ; left is word or symbol
    (skip-chars-backward "-_a-zа-яA-ZА-Я0-9")
    (push-mark)
    (skip-chars-forward "-_a-zа-яA-ZА-Я0-9")
    (setq mark-active t))
   ((and (looking-at "[:blank:]")
         (prog2 (backward-char)
             (looking-at "[:blank:]") (forward-char))) ; left and right both space
    (skip-chars-backward "[:blank:]") (push-mark (point) t t)
    (skip-chars-forward "[:blank:]"))
   (t (mark-sexp) (exchange-point-and-mark)))
  (let ((xr (buffer-substring-no-properties (region-beginning) (region-end))))
    (when (and (region-active-p) (< 5 (length xr)))
      (setq cur-hl-regexp xr)
      (highlight-regexp cur-hl-regexp 'hl-select-word))))

(add-hook 'deactivate-mark-hook (lambda () (unhighlight-regexp cur-hl-regexp)))

(defvar pair-brackets '(("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">") ("“" . "”")))

(defun select-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \" ` ' and anything in `brackets'.
Limit selection by current line.
This command ignores nesting. For example, if text is
    (a(b)c▮)
the selected char is “c”, not “a(b)c”."
  (interactive)
  (if (bolp)
      (progn (setq this-command 'next-buf) (next-buf))
    (push-mark (point) t nil)
    (let ((xskipChars (concat "^\"`'" (mapconcat #'identity brackets ""))))
      (skip-chars-backward xskipChars)
      (setq xskipChar (buffer-substring-no-properties (- (point) 1) (point)))
      (if (member xskipChar left-brackets)
          (setq xskipChar (cdr (assoc xskipChar pair-brackets))))
      (push-mark (point) t t)
      (skip-chars-forward (concat "^" xskipChar)))))

(defun deactivate-mark-if-active (&rest _)
  "Deactivate mark if region active."
  (if (region-active-p) (deactivate-mark)))


;; Misc

(defun user-buffer-p ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it is not considered a user
buffer.
This function is used by buffer switching command and close buffer
command, so that next buffer shown is a user buffer."
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((memq major-mode '(eww-mode dired-mode help-mode doc-view-mode diary-mode)) nil)
   ((string-equal (buffer-name) "tetris-scores") nil)
   ((string-equal (buffer-name) "snake-scores") nil)
   ((string-equal buffer-file-truename org-agenda-file-1) nil)
   ((string-equal buffer-file-truename org-agenda-file-2) nil)
   ((string-equal buffer-file-truename org-agenda-file-3) nil)
   ((string-match ".+em/project+." default-directory) nil)
   ((and buffer-file-truename (string-match ".sql" buffer-file-truename)) nil)
   (t t)))

(defun back-buf ()
  "Switch to the previous buffer."
  (interactive)
  (if (project-buffer-p)
      (prev-proj-buf)
    (let ((i 0) (xbuf (current-buffer)))
      (previous-buffer)
      (while (< i 99)
        (if (not (user-buffer-p))
            (progn (previous-buffer)
                   (setq i (1+ i))
                   (when (= i 99)
                     (message "%s" "No prev buffer")
                     (switch-to-buffer xbuf)))
          (setq i 100))))))

(defun forw-buf ()
  "Switch to the next buffer."
  (interactive)
  (if (project-buffer-p)
      (next-proj-buf)
    (let ((i 0) (xbuf (current-buffer)))
      (next-buffer)
      (while (< i 99)
        (if (not (user-buffer-p))
            (progn (next-buffer)
                   (setq i (1+ i))
                   (when (= i 99)
                     (message "%s" "No next buffer")
                     (switch-to-buffer xbuf)))
          (setq i 100))))))

(defun prev-buf ()
  "Switch to the previous buffer."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (setq this-command 'toggle-ibuffer) (command-execute 'toggle-ibuffer))
    (command-execute 'back-buf)
    (if (eq last-command 'next-buf) (before-last-command))))

(defun next-buf ()
  "Switch to the next buffer."
  (interactive)
  (if (equal before-last-command this-command)
      (progn (setq this-command 'toggle-ibuffer) (command-execute 'toggle-ibuffer))
    (command-execute 'forw-buf)
    (if (eq last-command 'prev-buf) (before-last-command))))

(defun project-buffer-p ()
  "Return t if current buffer is a project buffer, else nil."
  (cond ((string-equal "*" (substring (buffer-name) 0 1)) nil)
        ((and (string-match ".+em/project+." default-directory)
              (not (string-equal major-mode "dired-mode"))) t)))

(defun prev-proj-buf ()
  "Switch to the previous project buffer."
  (interactive)
  (let ((i 0) (xbuf (current-buffer)))
    (previous-buffer)
    (while (< i 99)
      (if (not (project-buffer-p))
          (progn (previous-buffer)
                 (setq i (1+ i))
                 (when (= i 99)
                   (message "%s" "No prev proj buffer") (switch-to-buffer xbuf)))
        (setq i 100)))))

(defun next-proj-buf ()
  "Switch to the next project buffer."
  (interactive)
  (let ((i 0) (xbuf (current-buffer)))
    (next-buffer)
    (while (< i 99)
      (if (not (project-buffer-p))
          (progn (next-buffer)
                 (setq i (1+ i))
                 (when (= i 99)
                   (message "%s" "No next proj buffer") (switch-to-buffer xbuf)))
        (setq i 100)))))

(defun prev-eww-buffer ()
  "Switch to the previous eww buffer."
  (interactive)
  (let ((i 0) (xbuf (current-buffer)))
    (previous-buffer)
    (while (< i 99)
      (if (not (eq major-mode 'eww-mode))
          (progn (previous-buffer)
                 (setq i (1+ i))
                 (when (= i 99)
                   (message "%s" "No prev eww buffer") (switch-to-buffer xbuf)))
        (setq i 100)))))

(defun next-eww-buffer ()
  "Switch to the next eww buffer."
  (interactive)
  (let ((i 0) (xbuf (current-buffer)))
    (next-buffer)
    (while (< i 99)
      (if (not (eq major-mode 'eww-mode))
          (progn (next-buffer)
                 (setq i (1+ i))
                 (when (= i 99)
                   (message "%s" "No next eww buffer") (switch-to-buffer xbuf)))
        (setq i 100)))))

(defun alt-buf ()
  "Alternate buffer."
  (interactive)
  (if (string-equal (buffer-name (other-buffer)) "*Ibuffer*")
      (toggle-ibuffer)
    (switch-to-buffer (other-buffer))))

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

(defun save-close-buf ()
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
    (cond ((eq major-mode 'dired-mode) nil)
          ((eq major-mode 'ibuffer-mode) nil)
          ((string-equal xtype "proj")
           (unless (project-buffer-p) (prev-proj-buf)))
          ((string-equal xtype "user")
           (unless (user-buffer-p) (prev-buf))))))

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
        (abort-recursive-edit)
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
    (message "No recently closed buffers in this session")))

(defun open-file ()
  "Open the file path under cursor.
If there is selection, use it for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›”
with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el”
for elisp files."
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
                         (not (member xpath '("//" "/" "." ".." ":"))))
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
      (eww xurl) (setq last-command 'eww-follow-link))))



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
    (progn (message "%s" xcmdStr) (shell-command xcmdStr xoutputb))))

(defconst run-current-file-map '(("pl" . "perl") ("py" . "python3") ("sh" . "bash"))
  "Alist file extension to program name.")

(defun run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call [python x.py]
in a shell.
Output is printed to buffer “*run output*”.
File suffix is used to determine which program to run, set in the variable
`run-current-file-map'."
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
                (if xappCmdStr
                    (format "%s %s &" xappCmdStr (shell-quote-argument xfname))))
          (if (buffer-modified-p) (save-buffer))
          (cond ((string-equal xfExt "el")
                 (load xfname))
                ((string-equal xfExt "go")
                 (run-current-go-file))
                (t (if xappCmdStr
                       (progn (message "Running %s" xcmdStr)
                              (shell-command xcmdStr xoutBuffer))
                     (error "%s: Unknown extension: %s" real-this-command xfExt))))
          (setq run-output (concat "Run " xbuf))
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
The backup file name is in this format x.html~2018-05-15_133429~
The last part is hour, minutes, seconds.
If such a file already exist, it is overwritten.
If the current buffer is not associated with a file, nothing's done."
  (interactive)
  (let ((xfname (buffer-file-name))
        (xdateTimeFormat "%Y-%m-%d_%H%M%S"))
    (if xfname
        (let ((xbackupName
               (concat xfname "~" (format-time-string xdateTimeFormat) "~")))
          (copy-file xfname xbackupName t)
          (message "Backup"))
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
      (progn (make-backup) (when (buffer-modified-p) (save-buffer)))
    (make-backup)))



(defun cur-word ()
  "Get bounds of current word."
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
    (when (< xp1 (point)) (goto-char xp1))
    (buffer-substring-no-properties xp1 xp2)))

(defun isearch-cur-word ()
  "Call `isearch' on current word."
  (isearch-mode t) (isearch-yank-string (cur-word)))

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

(defvar occur-cur-word-defer-timer nil "Timer to defer `occur-cur-word'.")

(defun occur-cur-word-run ()
  "Call `occur' on current word."
  (interactive)
  (setq occur-cur-word-defer-timer nil)
  (occur (cur-word)) (enlarge-window-split))

(defun occur-cur-word ()
  "Defer in order to reuse double key press for another command."
  (interactive)
  (if (timerp occur-cur-word-defer-timer)
      (progn (cancel-timer occur-cur-word-defer-timer)
             (setq occur-cur-word-defer-timer nil))
    (setq occur-cur-word-defer-timer (run-with-timer 0.25 nil 'occur-cur-word-run))))

(defun search-string ()
  "Search string in all files of current directory."
  (interactive)
  (let ((xdefault (cur-word)))
    (find-text
     (read-string (format "Search string (default %s): " xdefault)
                  nil 'query-replace-history xdefault)
     (expand-file-name "") ".[A-Za-z0-9]+$" t t)))

(defun show-in-desktop ()
  "Show current file in desktop.
This command can be called when in a file buffer or in `dired'."
  (interactive)
  (let ((xpath (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files))
                       default-directory
                     (car (dired-get-marked-files)))
                 (if (buffer-file-name) (buffer-file-name) default-directory))))
    (cond ((string-equal system-type "darwin")
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

(defun alternate-frame ()
  "Switch to alternate buffer or frame.
If there more than one frame, switch to next frame."
  (interactive)
  (unless (minibufferp)
    (if (< 1 (length (frame-list))) (other-frame -1) (alt-buf))))

(defvar proced-defer-timer nil "Timer to defer `proced-defer'.")

(defun proced-run () "Run proced." (setq proced-defer-timer nil) (proced))

(defun proced-defer ()
  "Defer in order to reuse double key press for another command."
  (interactive)
  (if (timerp proced-defer-timer)
      (progn (cancel-timer proced-defer-timer) (setq proced-defer-timer nil))
    (setq proced-defer-timer (run-with-timer 0.25 nil 'proced-run))))

(defvar kmacro-record-timer nil "Timer to defer `kmacro-record'.")

(defun kmacro-start ()
  "Defer in order to reuse double key press for another command."
  (kmacro-start-macro nil) (setq kmacro-record-timer nil))

(defun kmacro-record ()
  "Start or stop macro recording."
  (interactive)
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-end-macro nil)
    (if (timerp kmacro-record-timer)
        (progn (cancel-timer kmacro-record-timer) (setq kmacro-record-timer nil))
      (setq kmacro-record-timer (run-with-timer 0.25 nil 'kmacro-start)))))

(defun terminal-prompt (Prompt)
  "Compare PROMPT and actual prompt."
  (string-equal Prompt (buffer-substring-no-properties
                         (line-beginning-position)
                         (+ (length Prompt) (line-beginning-position)))))

(defun terminal-input (Prompt)
  "Compare CAR kill-ring and actual terminal input."
  (string-equal (buffer-substring-no-properties
                 (+ (length Prompt) (line-beginning-position))
                 (line-end-position)) (car kill-ring)))

(defun change-wd-p ()
  "Ensure terminal input contains a command to change working directory
before actually send the cd command."
  (let ((xprompt "└ $ pushd . && ") (xprompt2 "└ $ % pushd . && "))
    (or (and (terminal-prompt xprompt) (terminal-input xprompt))
        (and (terminal-prompt xprompt2) (terminal-input xprompt2)))))

(defun get-wd ()
  "Get working directory."
  (if (one-window-p)
      (progn (switch-to-buffer (other-buffer))
             (copy-file-path t)
             (switch-to-buffer (other-buffer)))
    (other-window 1) (copy-file-path t) (other-window 1)))

(defun set-wd ()
  "Set working directory."
  (unless (string-equal (expand-file-name default-directory) (car kill-ring))
    (cond
     ((eq major-mode 'eshell-mode)
      (insert "pushd . && ") (yank) (if (change-wd-p) (eshell-send-input)))
     ((eq major-mode 'vterm-mode)
      (vterm-insert "pushd . && ") (vterm-yank) (if (change-wd-p) (vterm-send-return))))))

(defun change-wd () "Change working directory." (interactive) (get-wd) (set-wd))

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
  (if (one-window-p) (split-window-below))
  (enlarge-window-split) (other-window 1)
  (let ((inhibit-messages t) (message-log-max nil)) (command-execute 'eshell)))

(defun kmacro-helper ()
  "Keyboard macro helper. Ad hoc redefine."
  (interactive)
  (setq this-command 'config) (command-execute 'config))

(defalias 'kmacro-play 'call-last-kbd-macro)

(defun eshell-clear()
  "Clear screen eshell." (interactive) (eshell/clear t) (eshell-send-input))

(defun eshell-clear-input ()
  "Clear input eshell."
  (interactive) (if (region-active-p) (cut-line) (beg-of-line) (kill-line)))

(defun eshell-search-input ()
  "Eshell history ido complete."
  (interactive)
  (let ((xhist (delete-dups (ring-elements eshell-history-ring))))
    (push "" xhist) (insert (ido-completing-read "Search input: " xhist))))

(defun terminal-split ()
  "Split terminal window below."
  (interactive)
  (if (one-window-p) (split-window-below))
  (enlarge-window-split) (other-window 1)
  (command-execute 'vterm))

(defun vterm-up ()
  "Send `<up>' to the libvterm." (interactive) (vterm-send-key "<up>"))

(defun vterm-down ()
  "Send `<down>' to the libvterm." (interactive) (vterm-send-key "<down>"))

(defun vterm-backward-kill-word ()
  "Vterm backward kill word." (interactive) (vterm-send-key (kbd "C-w")))

(defun vterm-history-search ()
  "History search. Map C-a to history-incremental-search-backward in zshrc
and reverse-search-history in bashrc."
  (interactive) (vterm-send-key (kbd "C-a")))

(defun screenshot ()
  "Take screenshot on macOS."
  (interactive)
  (if (string-equal system-type "darwin")
      (call-process "screencapture" nil 0 nil "-W" "-U" "dummy")))

(defun clock () "World clock." (interactive) (world-clock) (other-window 1))

(defun player ()
  "Run player."
  (interactive) (if (fboundp 'emms-playlist) (emms-playlist) (message "No player")))

(defun text-scale-reset () "Reset text scale." (interactive) (text-scale-adjust 0))

(defun toggle-ibuffer ()
  "Toggle ibuffer. Force switch to current buffer to update `other-buffer'."
  (interactive)
  (let ((xbuf (buffer-name)))
    (if (string-equal major-mode "ibuffer-mode")
        (switch-to-buffer (other-buffer))
      (progn (switch-to-buffer xbuf) (ibuffer)
             (condition-case nil (ibuffer-jump-to-buffer xbuf) (error nil))))))

(defun ibuffer-select-group ()
  "Toggle filter group or visit buffer."
  (interactive)
  (let ((name (get-text-property (point) 'ibuffer-filter-group-name)))
    (if (stringp name) (ibuffer-toggle-filter-group) (ibuffer-visit-buffer))))

(defun flyspell-goto-prev-error ()
  "Go to prev error." (interactive) (flyspell-goto-next-error t))

(defun sun-moon ()
  "Show the Sun and the Moon."
  (interactive) (lunar-phases) (run-with-timer 1 nil 'sunrise-sunset))

(defun weather ()
  "Show weather."
  (interactive)
  (let ((url "https://www.windy.com")
        (lat (number-to-string calendar-latitude))
        (lon (number-to-string calendar-longitude)))
    (browse-url (concat url "/?" lat "," lon ",9"))))

(defun shopping () "Toggle shopping list." (interactive) (find-file shopping-list-file))

(defun downloads ()
  "Go to downloads or temp."
  (interactive)
  (if (equal (replace-regexp-in-string (getenv "HOME") "~" default-directory)
             downloads-dir)
      (find-file "~/.emacs.d/temp")
    (if (file-exists-p downloads-dir)
        (find-file downloads-dir)
      (find-file "~/.emacs.d/temp"))))

(defun org-agenda-tasks ()
  "Same as `org-agenda-switch-to', but call `tasks' in case of error."
  (interactive)
  (condition-case user-error (org-agenda-switch-to) (error (tasks))))

(defun todo ()
  "Modification of `org-todo'. Capitalize task."
  (interactive)
  (cond ((eq major-mode 'org-mode) (org-todo))
        ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))
  (when (eq major-mode 'org-mode)
    (beginning-of-line) (title-case-region-or-line)))

(defun toggle-gnus ()
  "Toggle gnus."
  (interactive)
  (if onlinep
      (progn (when (display-graphic-p)
               (setq xframe (make-frame-command)) (other-frame 1))
             (if (get-buffer "*Group*")
                 (switch-to-buffer "*Group*")
               (gnus) (unless (display-graphic-p) (gnus-nnews-inbox))))
    (message "%s" "Offline")))

(defun sql () "Open SQL client." (interactive) (find-file "~/.sql"))

(defvar sql-type "sqlite" "SQL type for client.")

(defun toggle-sql-type ()
  "Toggle `sql-type'."
  (interactive) (setq sql-type (if (equal sql-type "sqlite") "psql" "sqlite")))

(defun exec-query ()
  "Execute SQL statement separated by semicolon or selected region."
  (interactive)
  (unless (eq major-mode 'sql-mode) (error "Not SQL"))
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
    (other-window 1))
  (return-before))

(defun eval-region-or-sexp ()
  "Eval region or last sexp."
  (interactive)
  (if (use-region-p)
      (command-execute 'eval-region)
    (command-execute 'eval-last-sexp)))

(defun terminal () "Run terminal emulator." (interactive) (vterm))

(defun novel () "Read novel." (interactive) (switch-to-buffer "*novel*"))

(defun hippie-expand-undo () "Undo the expansion." (interactive) (he-reset-string))

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

(defvar video-extensions '("mkv" "mp4" "avi" "mov" "ts" "mts" "webm")
  "Open these video file extensions with `open-in-external-app'.")

(defvar external-extensions `("mp3" "m4a" "flac" "torrent" "app")
  "Open these file extensions with `open-in-external-app'.")

(setq external-extensions (append external-extensions video-extensions))

(defun json-pretty ()
  "Prettify buffer if json file."
  (interactive)
  (if (string-equal (file-name-extension buffer-file-name) "json")
      (progn (json-pretty-print-buffer) (message "%s" "Pretty print json"))
    (message "%s" "Not json")))

(defun org-insert-source-code ()
  "Insert source code block."
  (interactive)
  (if (eq major-mode 'org-mode)
      (progn (org-insert-structure-template "src") (insert "bash") (newline))
    (message "%s" "Not org")))

(defun dired-toggle-mark ()
  "Toggle mark for the current file."
  (interactive)
  (if (string-equal " "
                    (buffer-substring-no-properties
                     (line-beginning-position) (1+ (line-beginning-position))))
      (dired-mark 1)
    (dired-unmark 1)))

(defun double-jump-back ()
  "Double jump back."
  (set-mark-command t) (set-mark-command t))

(defun return-before-copy (&rest _)
  "Return to the point before copy selection."
  (when (memq last-command '(select-word select-quote))
    (double-jump-back) (setq this-command 'set-mark-command)))

(defun return-before (&rest _)
  "If region active deactivate mark conditionally and return to the point
before selection. This fun to be run as before advice for move fun."
  (interactive)
  (when (and (region-active-p) (memq last-command '(select-block select-word
                                                    select-line  select-quote)))
    (deactivate-mark) (double-jump-back)))

(defun delete-before (&rest _)
  "Delete selection right before insertion."
  (if (memq last-command '(select-word select-quote)) (del-back)))

(defun lookup-around (fun &rest r)
  "Lookup selection if buffer is read only and last command `select-word'.
Use as around advice e.g. for mouse left click after double click."
  (if (and (eq last-command 'select-word) buffer-read-only)
      (progn (return-before) (lookup-web))
    (apply fun r)))

(defun translate-around (fun &rest r)
  "Translate selection if buffer is read only and in eww."
  (if (and (eq major-mode 'eww-mode) buffer-read-only)
      (progn (translate) (setq this-command 'down-line))
    (apply fun r)))

(defun quit ()
  "Confirm and quit. Because restart without confirm."
  (interactive) (if (y-or-n-p-with-timeout "Quit?" 3 nil) (save-buffers-kill-terminal)))

(defun mouse-3 ()
  "Mouse right click. If buffer read only then lookup translation."
  (interactive) (if buffer-read-only (translate)))

(defun calendar-split () "Split calendar." (interactive) (calendar) (other-window 1))


;; Windows

(defun enlarge-window-around (fun &rest r)
  "If one window, then split window. Otherwise enlarge window."
  (if (one-window-p)
      (progn (setq this-command 'split-window-below) (split-window-below)
             (enlarge-window-split))
    (apply fun r)))

(advice-add 'enlarge-window :around 'enlarge-window-around)

(defun window-half-height-p ()
  "Return t if WINDOW is as half high as its containing frame."
  (equal 5 (round (fceiling
                   (* 10 (/ (float (window-height)) (float (frame-height))))))))

(defun enlarge-window-split ()
  "Enlarge window if frame splitted equally."
  (if (and (window-half-height-p)
           (< 16 (window-height)))
      (enlarge-window (round (fceiling (* 0.3 (window-height)))))))

(defun del-win ()
  "Split window if one window, otherwise delete window."
  (interactive)
  (if (and (not (null (frame-parameter nil 'fullscreen)))
           (display-graphic-p))
      (progn (setq this-command 'split-window-below)
             (split-window-below))
    (if (active-minibuffer-window)
        (abort-recursive-edit)
      (if (one-window-p)
          (progn
            (if (or (not (display-graphic-p))
                    (null (frame-parameter nil 'fullscreen)))
                (progn (setq this-command 'split-window-below)
                       (split-window-below))
              (setq this-command 'split-window-horizontally)
              (split-window-horizontally)))
        (setq this-command 'delete-window) (delete-window)))))

(defun other-win ()
  "Other window."
  (interactive) (setq this-command 'other-window) (other-window 1))

(defun shrink-win ()
  "Shrink window to fit rows count."
  (let ((xl (count-lines (point-min) (point-max))))
    (if (< xl (window-total-height)) (enlarge-window (- xl (window-total-height))))))

(defun shrink-completion-win ()
  "Shrink completion window."
  (when-let ((xwin (get-buffer-window "*Completions*")))
    (select-window xwin)
    (shrink-win)
    (select-window (get-buffer-window completion-reference-buffer))))

(defun completion-at-point-after (&rest _)
  "Setup after run completion at point."
  (when-let* ((xbuf "*Completions*") (xwin (get-buffer-window xbuf)))
    (with-current-buffer xbuf
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-min)) (kill-line 4))))
    (shrink-completion-win))
  (setq this-command 'completion-at-point))

(advice-add 'completion-at-point :after 'completion-at-point-after)

(defun delete-completion-win ()
  "Delete completion window."
  (interactive)
  (if-let ((xwin (get-buffer-window "*Completions*")))
      (progn (select-window xwin)
             (delete-window)
             (select-window (get-buffer-window completion-reference-buffer)))))

(defun toggle-pin-window ()
  "Toggle current window dedicated and not deletable."
  (interactive)
  (let ((x (not (window-dedicated-p (get-buffer-window (current-buffer))))))
    (set-window-dedicated-p nil x)
    (set-window-parameter nil 'no-delete-other-windows x)))

(defun scroll-one-pixel (&rest _)
  "Scroll one pixel up. Disables recentering cursor temporary."
  (if pixel-scroll-mode (pixel-scroll-pixel-up 1)))

(defalias 'view-messages 'view-echo-area-messages)

(defun save-all-unsaved () (interactive) (save-some-buffers t))

(defun save-buffer-isearch-cancel ()
  "Cancel isearch and save buffer."
  (interactive)
  (if (and (buffer-modified-p) (buffer-file-name)) (save-buffer))
  (if isearch-mode (isearch-cancel)))

(defun empty-trash ()
  "Empty trash on macOS."
  (interactive)
  (if (and (y-or-n-p-with-timeout "Empty trash?" 3 nil)
           (string-equal system-type "darwin"))
      (call-process "osascript" nil 0 nil "-e" "tell app \"Finder\" to empty")))

(provide 'keycom)

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical unresolved)
;; End:
;;; keycom.el ends here
