;;; keycom.el --- Keyboard Commands -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; Commands for Keyboard Amplifier.

;;; Code:



(require 'bookmark)
(require 'cl-lib)



;; Cursor movement

(defun get-bounds-of-block ()
  "Return the boundary (START . END) of current block."
  (let (p1 p2 (blankRegexp "\n[ \t]*\n"))
    (save-excursion
      (setq p1 (if (re-search-backward blankRegexp nil 1)
                   (goto-char (match-end 0))
                 (point)))
      (setq p2 (if (re-search-forward blankRegexp nil 1)
                   (match-beginning 0)
                 (point))))
    (cons p1 p2)))

(defun get-bounds-of-block-or-region ()
  "If region is active, return its boundary, else `get-bounds-of-block'."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (get-bounds-of-block)))

(defun up-line-wrap ()
  "On the first line go to the last line of buffer."
  (when (= (line-number-at-pos) 1)
    (forward-line (count-lines (point-min) (point-max)))))

(defun down-line-wrap (fun &rest r)
  "On the last line go to the first line of buffer."
  (if (= (line-number-at-pos) (1+ (count-lines (point-min) (point-max))))
      (forward-line (- (count-lines (point-min) (point-max))))
    (apply fun r)))

(defvar before-last-command-event nil "Before `last-command-event'.")

(defun before-last-command-event (Event)
  "Set and unset `before-last-command-event' to EVENT."
  (setq before-last-command-event Event)
  (run-with-timer before-last-command-timeout nil
                  (lambda () (setq before-last-command-event nil))))

(defconst before-last-command-timeout (/ 300 1000.0)
  "Timeout for `before-last-command' and `before-last-command-event'.")

(defvar before-last-command nil "Command before last command.")

(defun before-last-command ()
  "Set and cancel `before-last-command'."
  (setq before-last-command last-command)
  (run-with-timer before-last-command-timeout nil
                  (lambda () (setq before-last-command nil))))

(defvar last-command-keys nil "Last command keys.")

(defconst triple-press-direction-commands-list
  '(bchar
    fchar
    previous-line
    dired-previous-line
    back-word
    forw-word)
  "See `down-line' and `select-word'.
1. Before last command affects direction of a triple SPC and DEL press
2. Set right before execution of up and down line by the triple press
3. Up and down line run as around advice for select block and word.")

(defun up-line ()
  "Up line for transient use."
  (interactive)
  (cond
   ((equal before-last-command 'back-word)
    (setq this-command 'back-word-repeat)
    (back-word-repeat))
   ((equal before-last-command 'bchar)
    (setq this-command 'back-char)
    (back-char))
   ((equal before-last-command this-command)
    (cond
     ((eq major-mode 'ibuffer-mode)
      (setq this-command 'ibuffer-backward-filter-group)
      (ibuffer-backward-filter-group))
     ((eq major-mode 'gnus-group-mode)
      (setq this-command 'gnus-topic-prev)
      (gnus-topic-prev))
     (t (if (equal last-command-keys "t") ; else SPC
            (progn
              (setq this-command 'page-up-half)
              (page-up-half))
          (setq this-command 'beg-of-block)
          (beg-of-block)))))
   (t (command-execute 'previous-line)
      (when (eq last-command 'down-line)
        (before-last-command))))
  (setq last-command-keys (this-command-keys)))

(defun down-line ()
  "Down line for transient use."
  (interactive)
  (cond
   ((equal before-last-command 'forw-word)
    (setq this-command 'forw-word-repeat)
    (forw-word-repeat))
   ((or (equal before-last-command 'previous-line)
        (equal before-last-command 'dired-previous-line)) ; before select-word
    (setq this-command 'up-line-rev)
    (up-line-rev))
   ((equal before-last-command 'fchar)
    (setq this-command 'forw-char)
    (forw-char))
   ((equal before-last-command this-command)
    (cond
     ((eq major-mode 'ibuffer-mode)
      (setq this-command 'ibuffer-forward-filter-group)
      (ibuffer-forward-filter-group))
     ((eq major-mode 'gnus-group-mode)
      (setq this-command 'gnus-topic-next)
      (gnus-topic-next))
     (t (if (equal last-command-keys "d") ; else DEL
            (progn
              (setq this-command 'page-dn-half)
              (page-dn-half))
          (setq this-command 'end-of-block)
          (end-of-block)))))
   (t (command-execute 'next-line)
      (when (and (eq major-mode 'gnus-summary-mode)
                 (> (line-number-at-pos) 2))
        (command-execute 'next-line)) ; double next line
      (when (eq last-command 'up-line)
        (before-last-command))))
  (setq last-command-keys (this-command-keys)))

(defun up-line-rev ()
  "Up line for transient use. Reverse."
  (interactive)
  (if (equal before-last-command this-command)
      (cond
       ((eq major-mode 'ibuffer-mode)
        (setq this-command 'ibuffer-backward-filter-group)
        (ibuffer-backward-filter-group))
       ((eq major-mode 'gnus-group-mode)
        (setq this-command 'gnus-topic-prev)
        (gnus-topic-prev))
       (t (setq this-command 'up-line)
          (up-line)))
    (command-execute 'previous-line)
    (when (eq last-command 'down-line-rev)
      (before-last-command)))
  (setq last-command-keys (this-command-keys)))

(defun down-line-rev ()
  "Down line for transient use. Reverse. Generally both leaders do
down line if held down (triple press), but each leader might change direction,
and so does the other one."
  (interactive)
  (when (region-active-p) ; triple press
    (deactivate-mark)
    (double-jump-back)
    (command-execute 'next-line))
  (if (or (equal before-last-command 'previous-line)
          (equal before-last-command 'up-line))
      (progn
        (setq this-command 'up-line)
        (up-line))
    (if (equal before-last-command this-command)
        (cond
         ((eq major-mode 'ibuffer-mode)
          (setq this-command 'ibuffer-forward-filter-group)
          (ibuffer-forward-filter-group))
         ((eq major-mode 'gnus-group-mode)
          (setq this-command 'gnus-topic-next)
          (gnus-topic-next))
         (t (setq this-command 'down-line)
            (down-line)))
      (command-execute 'next-line)
      (when (and (eq major-mode 'gnus-summary-mode)
                 (> (line-number-at-pos) 2))
        (command-execute 'next-line))
      (when (eq last-command 'up-line-rev)
        (before-last-command))))
  (setq last-command-keys (this-command-keys)))

(defun beginning-of-visual-line-once (&rest _)
  "Call to `beginning-of-visual-line'."
  (unless (or (equal before-last-command 'backward-char)
              (equal before-last-command 'forward-char)
              (equal before-last-command 'forw-word)
              (equal before-last-command 'back-word)
              (eq major-mode 'dired-mode))
    (beginning-of-visual-line)))

(advice-add 'up-line       :before #'up-line-wrap)
(advice-add 'down-line     :around #'down-line-wrap)
(advice-add 'up-line       :after #'beginning-of-visual-line-once)
(advice-add 'down-line     :after #'beginning-of-visual-line-once)
(advice-add 'up-line-rev   :after #'beginning-of-visual-line-once)
(advice-add 'down-line-rev :after #'beginning-of-visual-line-once)

(defun goto-point-max (&rest _)
  "Go to point max if not there."
  (unless (eq (point) (point-max))
    (goto-char (point-max))))

(advice-add 'hist-forw :before #'goto-point-max)

(defun comp-back ()
  "Completion backward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (setq this-command 'abort-recursive-edit)
        (abort-recursive-edit))
    (command-execute 'icomplete-backward-completions)
    (when (eq last-command 'comp-forw)
      (before-last-command))))

(defun comp-forw ()
  "Completion forward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (setq this-command 'abort-recursive-edit)
        (abort-recursive-edit))
    (command-execute 'icomplete-forward-completions)
    (when (eq last-command 'comp-back)
      (before-last-command))))

(defun comp-back-rev ()
  "Completion backward for transient use. Reverse."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (setq this-command 'abort-recursive-edit)
        (abort-recursive-edit))
    (command-execute 'icomplete-backward-completions)
    (when (eq last-command 'comp-forw-rev)
      (before-last-command))))

(defun comp-forw-rev ()
  "Completion forward for transient use. Reverse."
  (interactive)
  (if (eq (icomplete--category) 'file)
      (isearch-backward)
    (if (equal before-last-command this-command)
        (progn
          (setq this-command 'abort-recursive-edit)
          (abort-recursive-edit))
      (command-execute 'icomplete-forward-completions)
      (when (eq last-command 'comp-back-rev)
        (before-last-command)))))

(defun hist-back ()
  "History backward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (isearch-cancel-clean-are)
    (command-execute 'previous-line-or-history-element)
    (when (eq last-command 'hist-forw)
      (before-last-command))))

(defun hist-forw ()
  "History forward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (isearch-cancel-clean-are)
    (command-execute 'next-line-or-history-element)
    (when (eq last-command 'hist-back)
      (before-last-command))))

(defun ido-next-match-rev ()
  "Custom for transient use, reverse."
  (interactive)
  (if ido-matches
      (let ((next (cadr ido-matches)))
	(setq ido-cur-list (ido-chop ido-cur-list next))
	(setq ido-matches (ido-chop ido-matches next))
	(setq ido-rescan nil))))

(defun ido-prev-match-rev ()
  "Custom for transient use, reverse."
  (interactive)
  (if ido-matches
      (let ((prev (car (last ido-matches))))
	(setq ido-cur-list (ido-chop ido-cur-list prev))
	(setq ido-matches (ido-chop ido-matches prev))
	(setq ido-rescan nil))))

(defun company-select-back ()
  "Company complete backward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (setq this-command 'company-abort)
        (company-abort))
    (command-execute 'company-select-previous)
    (when (eq last-command 'company-select-forw)
      (before-last-command))))

(defun company-select-forw ()
  "Company complete forward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (setq this-command 'company-abort)
        (company-abort))
    (command-execute 'company-select-next)
    (when (eq last-command 'company-select-back)
      (before-last-command))))

(defun button-back ()
  "Button backward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (setq this-command 'backward-button)
        (backward-button 1 nil nil t))
    (backward-button 1 nil nil t)
    (when (eq last-command 'button-forw)
      (before-last-command))))

(defun button-forw ()
  "Button forward for transient use."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (setq this-command 'forward-button)
        (forward-button 1 nil nil t))
    (forward-button 1 nil nil t)
    (when (eq last-command 'button-back)
      (before-last-command))))

(defun isearch-update-ring-force ()
  "Force push string to isearch ring."
  (unless (zerop (length isearch-string))
    ;; update the ring data
    (isearch-update-ring isearch-string isearch-regexp)))

(defun isearch-cancel-clean ()
  "Like `isearch-cancel' but no quit signal and clean up echo area."
  (isearch-update-ring-force)
  (if (and isearch-push-state-function isearch-cmds)
      ;; For defined push-state function, restore the first state.
      ;; This calls pop-state function and restores original point.
      (let ((isearch-cmds (last isearch-cmds)))
        (isearch--set-state (car isearch-cmds)))
    (goto-char isearch-opoint))
  (isearch-done t) ; Exit isearch..
  (isearch-clean-overlays)
  (message nil))

(defun isearch-cancel-clean-are ()
  "Defer `isearch-cancel-clean' and `abort-recursive-edit'."
  (interactive)
  (run-with-timer 0.05 nil 'isearch-cancel-clean)
  (setq this-command 'abort-recursive-edit)
  (abort-recursive-edit))

(defun isearch-minibuffer-prompt ()
  "Return t if minibufer is isearch minibuffer."
  (string-match "I-search" (minibuffer-prompt)))

(defun isearch-double-back ()
  "Isearch double backward for transient use."
  (interactive)
  (command-execute 'isearch-repeat-backward)
  (command-execute 'isearch-repeat-backward))

(defun isearch-direction-switch ()
  "Do direction switch."
  (interactive)
  (if isearch-regexp
      (if isearch-forward
          (isearch-backward-regexp)
        (isearch-forward-regexp))
    (if isearch-forward
        (isearch-backward)
      (isearch-forward))))

(defun isearch-back ()
  "Isearch backward for transient use."
  (interactive)
  (if (or (equal before-last-command this-command)
          (equal before-last-command 'isearch-wback))
      (progn
        (setq this-command 'isearch-cancel)
        (isearch-cancel-clean))
    (command-execute 'isearch-repeat-backward)
    (when (or (eq last-command 'isearch-forw)
              (eq last-command 'isearch-wforw))
      (before-last-command))))

(defun isearch-forw ()
  "Isearch forward for transient use."
  (interactive)
  (if (or (equal before-last-command this-command)
          (equal before-last-command 'isearch-wforw))
      (progn
        (setq this-command 'isearch-cancel)
        (isearch-cancel-clean))
    (command-execute 'isearch-repeat-forward)
    (when (or (eq last-command 'isearch-back)
              (eq last-command 'isearch-wback))
      (before-last-command))))

(defun jump-mark ()
  "Move cursor to last mark position of current buffer, but not point.
Call this repeatedly will cycle all positions in `mark-ring'.
Save point to register 6 before repeated call."
  (interactive)
  (if (region-active-p)
      (exchange-point-and-mark)
    (unless (eq this-command last-command)
      (point-to-register ?6))
    (let ((p (point)))
      (set-mark-command t)
      (when (eql p (point))
        (set-mark-command t)))))

(defun jump-6 ()
  "Jump to register 6."
  (interactive)
  (when (get-register ?6)
    (jump-to-register ?6)))

(defun jump-7 ()
  "Jump to register 7."
  (interactive)
  (when (get-register ?7)
    (jump-to-register ?7)))

(defun jump-8 ()
  "Jump to register 8."
  (interactive)
  (when (get-register ?8)
    (jump-to-register ?8)))

(defun beg-of-line-raw ()
  "Move cursor to beginning of line."
  (let ((p (point)))
    (if visual-line-mode
        (beginning-of-visual-line)
      (if (eq major-mode 'eshell-mode) ; custom eshell bol
          (if (= (line-number-at-pos) (count-lines (point-min) (point-max)))
              (progn
                (beginning-of-line)
                (forward-char 4))
            (beginning-of-line))
        (back-to-indentation)
        (when (eq p (point))
          (beginning-of-line))))))

(defun beg-of-line ()
  "Move cursor to beginning of line. Move to end of line if in beginning of line."
  (interactive)
  (if (bolp)
      (end-of-line)
    (beg-of-line-raw)))

(defun end-of-lyne ()
  "End of line or visual line."
  (interactive)
  (cond
   ((eolp)
    (beg-of-line-raw))
   (visual-line-mode
    (end-of-visual-line))
   (t (end-of-line))))

(defun back-block ()
  "Move cursor to the end of prev block."
  (interactive)
  (when (re-search-backward "\n[\t\n ]*\n+" nil 1)
    (skip-chars-backward "\n\t ")
    (forward-char))
  (when (eq last-command 'end-of-block)
    (when (re-search-backward "\n[\t\n ]*\n+" nil 1)
      (skip-chars-backward "\n\t ")
      (forward-char)))
  (when (eq major-mode 'dired-mode)
    (dired-previous-line 1)))

(defun forw-block ()
  "Move cursor to the beginning of next block."
  (interactive)
  (re-search-forward "\n[\t\n ]*\n+" nil 1)
  (when (eq major-mode 'dired-mode)
    (dired-next-line 1)))

(defun beg-of-block ()
  "Back block. Fast double direction switch via SPC to stop,
switch via K to half page move."
  (interactive)
  (if (equal before-last-command this-command)
      (if (equal last-command-keys "t") ; else SPC
          (progn
            (setq this-command 'page-up-half)
            (page-up-half))
        (command-execute 'back-block)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'back-block)
    (when (eq last-command 'end-of-block)
      (before-last-command)))
  (setq last-command-keys (this-command-keys)))

(defun beg-of-block-rev ()
  "Back block. For reverse transient."
  (interactive)
  (if (equal before-last-command this-command)
      (if (equal last-command-keys "t") ; else SPC
          (progn
            (setq this-command 'page-up-half)
            (page-up-half))
        (command-execute 'back-block)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'back-block)
    (when (eq last-command 'end-of-block-rev)
      (before-last-command)))
  (setq last-command-keys (this-command-keys)))

(defun end-of-block ()
  "Forw block. Fast double direction switch via DEL to stop,
switch via I to half page move."
  (interactive)
  (if (equal before-last-command this-command)
      (if (equal last-command-keys "d") ; else DEL
          (progn
            (setq this-command 'page-dn-half)
            (page-dn-half))
        (command-execute 'forw-block)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'forw-block)
    (when (eq last-command 'beg-of-block)
      (before-last-command)))
  (setq last-command-keys (this-command-keys)))

(defun end-of-block-rev ()
  "Forw block. For reverse transient."
  (interactive)
  (if (equal before-last-command this-command)
      (if (equal last-command-keys "d") ; else DEL
          (progn
            (setq this-command 'page-dn-half)
            (page-dn-half))
        (command-execute 'forw-block)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'forw-block)
    (when (eq last-command 'beg-of-block-rev)
      (before-last-command)))
  (setq last-command-keys (this-command-keys)))

(defun beg-of-buf ()
  "Go to the beginning of buffer, next press to the end of buffer."
  (interactive)
  (if (= (point) (point-min))
      (unless (eq major-mode 'eshell-mode)
        (goto-char (point-max))
        (forward-line -1))
    (goto-char (point-min)))
  (when (eq major-mode 'dired-mode)
    (dired-next-line 1)))

(defun end-of-buf ()
  "Go to the end of buffer, next call to the beginning of buffer."
  (interactive)
  (if (or (= (count-lines 1 (point)) (count-lines (point-min) (point-max)))
          (= (count-lines 1 (point)) (1- (count-lines (point-min) (point-max)))))
      (goto-char (point-min))
    (goto-char (point-max))
    (unless (eq major-mode 'eshell-mode)
      (forward-line -1))))

(defconst brackets '("()" "[]" "{}" "<>" "“”")
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
  (cond
   ;; see equal sign mapping for russian, so because of conflict
   ;; while for Engram it is free and can be remapped
   ((equal "%" (this-command-keys))
    (setq this-command 'text-scale-increase) (text-scale-increase 1))
   ;; next allows to call self insert if not russian
   ((and (equal "=" (this-command-keys))
         (boundp 'frame-title-ru) (null frame-title-ru))
    (self-insert-command 1))
   ;; base scenario
   ((nth 3 (syntax-ppss)) (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING))
   (t (cond
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
  (let* ((bds (get-bounds-of-block-or-region))
         (p1 (car bds))
         (p2 (cdr bds)))
    (sort-lines current-prefix-arg p1 p2)))

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
  (let* ((bds (get-bounds-of-block-or-region))
         (p1 (car bds))
         (p2 (cdr bds)))
    (narrow-to-region p1 p2)))

(defun back-word ()
  "Backward word. Fast double direction switch breaks beat to char move."
  (interactive)
  (when (and (member (this-command-keys) (list "l" [?г]))
             (not (or (eq last-command 'select-word)
                      (eq last-command 'back-word)
                      (eq last-command 'jump-mark))))
    (push-mark (point) t)) ; virtual leader
  (if (equal before-last-command this-command)
      (progn
        (backward-word)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'backward-word)
    (when (eq last-command 'forw-word)
      (before-last-command))))

(defun back-word-repeat ()
  "Backward word for leader repeat."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (backward-word)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'backward-word)
    (when (eq last-command 'forw-word-repeat)
      (before-last-command))))

(defun forw-word ()
  "Forward word."
  (interactive)
  (when (and (member (this-command-keys) (list "w" [?щ]))
             (not (or (eq last-command 'select-word)
                      (eq last-command 'forw-word))))
    (push-mark (point) t)) ; virtual leader
  (if (equal before-last-command this-command)
      (progn
        (forward-word)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'forward-word)
    (when (eq last-command 'back-word)
      (before-last-command))))

(defun forw-word-repeat ()
  "Forward word for leader repeat."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (forward-word)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'forward-word)
    (when (eq last-command 'back-word-repeat)
      (before-last-command))))

(defun back-char ()
  "Backward char."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (command-execute 'backward-char)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'left-char)
    (when (eq last-command 'forw-char)
      (before-last-command))))

(defun forw-char ()
  "Forward char."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (command-execute 'forward-char)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'right-char)
    (when (eq last-command 'back-char)
      (before-last-command))))

(defun bchar ()
  "Backward char."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (command-execute 'backward-char)
        (if (region-active-p)
            (progn
              (setq this-command 'deactivate-region)
              (command-execute 'deactivate-region))))
    (command-execute 'backward-char)
    (when (eq last-command 'fchar)
      (before-last-command))))

(defun fchar ()
  "Forward char."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (command-execute 'forward-char)
        (when (region-active-p)
          (setq this-command 'deactivate-region)
          (command-execute 'deactivate-region)))
    (command-execute 'forward-char)
    (when (eq last-command 'bchar)
      (before-last-command))))

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

(defun deactivate-region ()
  "Command to `deactivate-mark'."
  (interactive)
  (deactivate-mark))

(defun page-up-half ()
  "Page up half. Direction fast switch to cancel transient move."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (command-execute 'View-scroll-half-page-backward)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'View-scroll-half-page-backward)
    (when (eq last-command 'page-dn-half)
      (before-last-command)))
  (beginning-of-visual-line-once))

(defun page-dn-half ()
  "Page dn half."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (command-execute 'View-scroll-half-page-forward)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'View-scroll-half-page-forward)
    (when (eq last-command 'page-up-half)
      (before-last-command)))
  (beginning-of-visual-line-once))

(defun page-up-half-rev ()
  "Page up half. For reverse transient."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (command-execute 'View-scroll-half-page-backward)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'View-scroll-half-page-backward)
    (when (eq last-command 'page-dn-half-rev)
      (before-last-command)))
  (beginning-of-visual-line-once))

(defun page-dn-half-rev ()
  "Page dn half. For reverse transient."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (command-execute 'View-scroll-half-page-forward)
        (setq this-command 'ignore)
        (command-execute 'ignore))
    (command-execute 'View-scroll-half-page-forward)
    (when (eq last-command 'page-up-half-rev)
      (before-last-command)))
  (beginning-of-visual-line-once))

(advice-add 'mark-defun :after #'exchange-point-and-mark)


;; Editing commands

(defun del-word ()
  "Kill characters forward until encountering the end of the word."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (kill-word 1)))

(defun backward-del-word ()
  "Kill characters backward until encountering the end of the word."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (backward-kill-word 1)))

(defun copy-text-block ()
  "Copy text block to register 1."
  (interactive)
  (when (fboundp 'uncentered-cursor)
    (uncentered-cursor))
  (select-block)
  (sit-for 0.1)
  (copy-line)
  (double-jump-back)
  (when (fboundp 'centered-cursor)
    (centered-cursor)))

(defun copy-selection (&rest _)
  "Copy selection."
  (when (region-active-p)
    (copy-region-as-kill (region-beginning) (region-end))))

(defun copy-line ()
  "Copy current line or selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer
(respects `narrow-to-region')."
  (interactive)
  (let ((inhibit-field-text-motion nil))
    (if (region-active-p)
        (progn
          (copy-region-as-kill (region-beginning) (region-end))
          (setq this-command 'copy)) ; no repeat
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
          ;; animate line first selection
          (if (eq major-mode 'org-mode) ; TODO: exception, some issue
              (progn
                (copy-region-as-kill (line-beginning-position) (line-end-position))
                (end-of-line))
            (if visual-line-mode
                (progn
                  (beginning-of-visual-line)
                  (push-mark (point) t t)
                  (end-of-visual-line))
              (push-mark (line-beginning-position) t t)
              (end-of-line))
            (sit-for 0.1)
            (copy-region-as-kill (region-beginning) (region-end))
            (end-of-line))
          (unless (eobp)
            (forward-char))))))
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
  (kill-new (buffer-string))
  (message "Buffer content copy"))

(defun paste-or-prev ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.
When `universal-argument' is called first with a number arg,
paste that many times."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (when (and delete-selection-mode
               (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (dotimes (_ (prefix-numeric-value current-prefix-arg))
          (yank))
      (if (eq real-last-command this-command)
          (yank-pop 1)
        (push-mark (point) t)
        (yank)))))

(defconst show-kill-ring-separator (concat "\n\n" (make-string 77 95) "\n\n")
  "A line divider for `show-kill-ring'.")

(defun show-kill-ring ()
  "Insert all `kill-ring' content in a new buffer named *copy stack*."
  (interactive)
  (let ((buf (generate-new-buffer "*copy stack*"))
        (inhibit-read-only t))
    (switch-to-buffer buf)
    (mapc (lambda (x)
            (insert x show-kill-ring-separator))
          kill-ring)
    (goto-char (point-min))))

(defun cut-bracket-text ()
  "Delete the matching brackets/quotes to the left of cursor,
including the inner text.
This command assumes the left of cursor is a right bracket, and there
is a matching one before it.
What char is considered bracket or quote is determined by current syntax table."
  (if (and (eq major-mode 'python-ts-mode)
           (looking-back "\\s\"" 1))
      (progn ; crazy shit with quotes - go to prev quote and then del cause sexp moves wrong
        (backward-char 1)
        (let ((skipChars (concat "^\"`'" (mapconcat #'identity brackets ""))))
          (skip-chars-backward skipChars)
          (setq skipChar (buffer-substring-no-properties (- (point) 1) (point)))
          (when (member skipChar left-brackets)
            (setq skipChar (cdr (assoc skipChar pair-brackets)))))
        (del-back))
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
  (let ((p0 (point)) p1)
    (forward-sexp -1)
    (setq p1 (point))
    (goto-char p0)
    (delete-char -1)
    (goto-char p1)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- p0 2))))

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
    (let ((pt (point)))
      (forward-sexp)
      (delete-char -1)
      (push-mark (point) t)
      (goto-char pt)
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
      (let (pOpenBracketLeft (pOpenBracketRight (point)) isComment)
        (backward-char)
        (setq pOpenBracketLeft (point))
        (goto-char pOpenBracketRight)
        (forward-char)
        (setq isComment (nth 4 (syntax-ppss)))
        (if isComment
            (progn ;; (message "Cursor is in comment")
              (goto-char pOpenBracketLeft)
              (if (forward-comment 1)
                  (kill-region (point) pOpenBracketLeft)
                (message "Error hSnRp: parsing comment failed")))
          (progn ;; (message "Right 1 char of cursor is not in comment")
            (goto-char pOpenBracketLeft)
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
     (t (kill-region (point) (progn (backward-char 1) (point)))))))

(defun del-back ()
  "If delete chars in progress, then do delete char back.
Else try cut bracket. If error e.g. no match, then delete char."
  (interactive)
  (if (eq last-command this-command)
      (delete-char -1)
    (if buffer-read-only
        (setq this-command 'ignore)
      (condition-case nil
          (progn
            (cut-bracket)
            (push-mark (point) t))
        (error (if (looking-back "\\s)" 1)
                   (kill-region (point)
                                (progn
                                  (backward-char 1)
                                  (point)))
                 (kill-region (point)
                              (progn
                                (forward-char 1)
                                (point))))))))
  (setq this-command 'del-back))

(defun del-forw ()
  "Delete char forward."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (delete-char 1)))

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
   (let ((brackets
          '("\" double quote \""
            "' single quote '"
            "( paren )"
            "{ brace }"
            "[ square ]"
            "< greater >"
            "` backtick `"
            " none ")))
     (list
      (completing-read "Replace this:" brackets)
      (completing-read "To:" brackets))))
  (let (p1 p2 left right toL toR
           (ss1 (split-string FromChars " "))
           (ss2 (split-string ToChars " ")))
    (let ((bds (get-bounds-of-block-or-region)))
      (setq p1 (car bds) p2 (cdr bds)))
    (setq left (car ss1) right (car (last ss1)))
    (setq toL (car ss2) toR (car (last ss2)))
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (let ((case-fold-search nil))
          (if (string-equal left right)
              (let ((xx (regexp-quote left)))
                (goto-char (point-min))
                (while
                    (re-search-forward (format "%s\\([^%s]+?\\)%s" xx xx xx) nil t)
                  (overlay-put (make-overlay
                                (match-beginning 0) (match-end 0)) 'face 'highlight)
                  (replace-match (concat toL "\\1" toR) t)))
            (progn
              (progn
                (goto-char (point-min))
                (while (search-forward left nil t)
                  (overlay-put (make-overlay
                                (match-beginning 0) (match-end 0)) 'face 'highlight)
                  (replace-match toL t t)))
              (progn
                (goto-char (point-min))
                (while (search-forward right nil t)
                  (overlay-put (make-overlay
                                (match-beginning 0) (match-end 0)) 'face 'highlight)
                  (replace-match toR t t))))))))))

(defun string-is-capitalized (String)
  "Check if STRING capitalized."
  (let ((case-fold-search nil))
    (equal (upcase String) String)))

(defun toggle-case ()
  "Toggle the letter case of current word or selection.
Cycle in this order: Init Caps, ALL CAPS, all lower. Calculates initial state."
  (interactive)
  (if (or buffer-read-only (zerop (buffer-size)))
      (setq this-command 'ignore)
    (let ((deactivate-mark nil) p1 p2)
      (unless (eq last-command this-command)
        (put this-command 'state 0))
      (if (region-active-p)
          (setq p1 (region-beginning) p2 (region-end))
        (save-excursion
          (skip-chars-backward "[:alpha:]")
          (setq p1 (point))
          (when (string-is-capitalized (buffer-substring p1 (1+ p1)))
            (put this-command 'state 1))
          (skip-chars-forward "[:alpha:]")
          (setq p2 (point))
          (when (string-is-capitalized (buffer-substring p1 p2))
            (put this-command 'state 2))))
      (cond
       ((equal 0 (get this-command 'state))
        (upcase-initials-region p1 p2)
        (put this-command 'state 1))
       ((equal 1 (get this-command 'state))
        (upcase-region p1 p2)
        (put this-command 'state 2))
       ((equal 2 (get this-command 'state))
        (downcase-region p1 p2)
        (put this-command 'state 0))))))

(defun toggle-prev-case ()
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
  (let (p1 p2)
    (let ((bds (get-bounds-of-block-or-region)))
      (setq p1 (car bds) p2 (cdr bds)))
    (save-restriction
      (narrow-to-region p1 p2)
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
  (let* ((skipChars "^\"<>(){}[]")
         (p0 (point))
         (p1 (if Begin
                 Begin
               (if (region-active-p)
                   (region-beginning)
                 (skip-chars-backward skipChars (line-beginning-position))
                 (point))))
         (p2 (if End
                 End
               (if (region-active-p)
                   (region-end)
                 (goto-char p0)
                 (skip-chars-forward skipChars (line-end-position))
                 (point))))
         (strPairs [
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
                    [" Или " " или "] [" О " " о "]
                    ]))
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (xx)
             (goto-char (point-min))
             (while (search-forward (aref xx 0) nil t)
               (replace-match (aref xx 1) t t)))
           strPairs))))))

(defun delete-blank-lines ()
  "Delete all newline around cursor."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "\n")
    (setq p1 (point))
    (skip-chars-forward "\n")
    (setq p2 (point))
    (delete-region p1 p2)))

(defun delete-spaces ()
  "Delete space, tab around cursor."
  (interactive)
  (let (p1 p2)
    (skip-chars-forward " \t")
    (setq p2 (point))
    (skip-chars-backward " \t")
    (setq p1 (point))
    (delete-region p1 p2)))

(defun shrink-whitespaces ()
  "Remove whitespaces around cursor.
Shrink neighboring spaces, then newlines, then spaces again, leaving
one space or newline at each step, till no more white space."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (let* ((eol-count 0) (p0 (point))
           p1 ; whitespace begin
           p2 ; whitespace end
           (charBefore (char-before)) (charAfter (char-after))
           (space-neighbor-p (or (eq charBefore 32) (eq charBefore 9)
                                 (eq charAfter 32) (eq charAfter 9))))
      (skip-chars-backward " \n\t")
      (setq p1 (point))
      (goto-char p0)
      (skip-chars-forward " \n\t")
      (setq p2 (point))
      (goto-char p1)
      (while (search-forward "\n" p2 t) (setq eol-count (1+ eol-count)))
      (goto-char p0)
      (cond
       ((eq eol-count 0)
        (if (> (- p2 p1) 1)
            (progn
              (delete-horizontal-space)
              (insert " "))
          (delete-horizontal-space)))
       ((eq eol-count 1)
        (if space-neighbor-p
            (delete-spaces)
          (delete-blank-lines)
          (insert " ")))
       ((eq eol-count 2)
        (if space-neighbor-p
            (delete-spaces)
          (delete-blank-lines)))
       ((> eol-count 2)
        (if space-neighbor-p
            (delete-spaces)
          (goto-char p2)
          (search-backward "\n")
          (delete-region p1 (point))
          (insert "\n")))
       (t (message "Nothing done. Logic error 40873. Should not reach here"))))))

(defun fill-or-unfill ()
  "Reformat current block or selection to short/long line.
First call will break into multiple short lines. Repeated call toggles
between short and long lines.
This commands calls `fill-region' to do its work. Set `fill-column'
for short line length."
  (interactive)
  (let ((isLongline
         (if (eq last-command this-command)
             (get this-command 'longline-p)
           t))
        (deactivate-mark nil)
        p1 p2)
    (let ((bds (get-bounds-of-block-or-region)))
      (setq p1 (car bds) p2 (cdr bds)))
    (if isLongline
        (fill-region p1 p2)
      (let ((fill-column 99999))
        (fill-region p1 p2)))
    (put this-command 'longline-p (not isLongline))))

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
  (let (p1 p2 (minlen fill-column))
    (if (and Begin End)
        (setq p1 Begin p2 End)
      (let ((bds (get-bounds-of-block-or-region)))
        (setq p1 (car bds) p2 (cdr bds))))
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil 1)
          (when (> (- (point) (line-beginning-position)) minlen)
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
  (let (isLong p1 p2)
    (setq isLong (if (eq last-command this-command) (get this-command 'is-long-p)))
    (let ((bds (get-bounds-of-block-or-region)))
      (setq p1 (car bds) p2 (cdr bds)))
    (if current-prefix-arg
        (reformat-to-multi-lines p1 p2)
      (if isLong
          (reformat-to-multi-lines p1 p2)
        (reformat-whitespaces-to-one-space p1 p2)))
    (put this-command 'is-long-p (not isLong))))

(defun reformat-to-sentence-lines ()
  "Reformat current block or selection into multiple lines by ending period.
Move cursor to the beginning of next text block."
  (interactive)
  (let (p1 p2)
    (let ((bds (get-bounds-of-block-or-region)))
      (setq p1 (car bds) p2 (cdr bds)))
    (save-restriction
      (narrow-to-region p1 p2)
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
  (let* ((bds (get-bounds-of-block-or-region))
         (p1 (car bds))
         (p2 (cdr bds)))
    (save-restriction
      (narrow-to-region p1 p2)
      (goto-char (point-min))
      (while (re-search-forward " +" nil t)
        (replace-match "\n")))))

(defun slash-to-backslash (&optional Begin End)
  "Replace slash by backslash on current line or region."
  (interactive)
  (let (p1 p2)
    (if (and Begin End)
        (setq p1 Begin p2 End)
      (if (region-active-p)
          (setq p1 (region-beginning) p2 (region-end))
        (setq p1 (line-beginning-position) p2 (line-end-position))))
    (save-restriction
      (narrow-to-region p1 p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "/" nil t) (replace-match "\\\\"))))))

(defun backslash-to-slash (&optional Begin End)
  "Replace backslash by slash on current line or region."
  (interactive)
  (let (p1 p2)
    (if (and Begin End)
        (setq p1 Begin p2 End)
      (if (region-active-p)
          (setq p1 (region-beginning) p2 (region-end))
        (setq p1 (line-beginning-position) p2 (line-end-position))))
    (save-restriction
      (narrow-to-region p1 p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\" nil t) (replace-match "/"))))))

(defun double-backslash-to-single (&optional Begin End)
  "Replace double backslash by single backslash on current line or region."
  (interactive)
  (let (p1 p2)
    (if (and Begin End)
        (setq p1 Begin p2 End)
      (if (region-active-p)
          (setq p1 (region-beginning) p2 (region-end))
        (setq p1 (line-beginning-position) p2 (line-end-position))))
    (save-restriction
      (narrow-to-region p1 p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\\\"  nil t) (replace-match "\\\\"))))))

(defun slash-to-double-backslash (&optional Begin End)
  "Replace slash by double backslash on current line or region."
  (interactive)
  (let (p1 p2)
    (if (and Begin End)
        (setq p1 Begin p2 End)
      (if (region-active-p)
          (setq p1 (region-beginning) p2 (region-end))
        (setq p1 (line-beginning-position) p2 (line-end-position))))
    (save-restriction
      (narrow-to-region p1 p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "/" nil t) (replace-match "\\\\\\\\"))))))

(defun double-backslash-to-slash (&optional Begin End)
  "Replace double backslash by slash on current line or region."
  (interactive)
  (let (p1 p2)
    (if (and Begin End)
        (setq p1 Begin p2 End)
      (if (region-active-p)
          (setq p1 (region-beginning) p2 (region-end))
        (setq p1 (line-beginning-position) p2 (line-end-position))))
    (save-restriction
      (narrow-to-region p1 p2)
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
      (let ((lbp (line-beginning-position)) (lep (line-end-position)))
        (if (eq lbp lep)
            (comment-dwim nil)
          (if (eq (point) lep)
              (comment-dwim nil)
            (comment-or-uncomment-region lbp lep)
            (forward-line)))))))

(defun quote-lines (Begin End QuoteL QuoteR Sep)
  "Add quotes/brackets and separator (comma) to lines.
Act on current block or selection.
In lisp code, QuoteL QuoteR Sep are strings."
  (interactive
   (let* ((bds (get-bounds-of-block-or-region))
          (p1 (car bds))
          (p2 (cdr bds))
          (brackets
           '("\"double quote\""
             "'single quote'"
             "(paren)"
             "{brace}"
             "[square]"
             "`markdown`"
             "none"
             "other"))
          bktChoice sep sepChoice quoteL quoteR)
     (setq bktChoice (completing-read "Quote to use:" brackets))
     (setq sepChoice (completing-read "Line separator:" '("," ";" "none" "other")))
     (cond
      ((string-equal bktChoice "none")
       (setq quoteL "" quoteR ""))
      ((string-equal bktChoice "other")
       (let ((xx (read-string "Enter 2 chars, for begin/end quote:")))
         (setq quoteL (substring-no-properties xx 0 1)
               quoteR (substring-no-properties xx 1 2))))
      (t (setq quoteL (substring-no-properties bktChoice 0 1)
               quoteR (substring-no-properties bktChoice -1))))
     (setq sep
           (cond
            ((string-equal sepChoice "none") "")
            ((string-equal sepChoice "other") (read-string "Enter separator:"))
            (t sepChoice)))
     (list p1 p2 quoteL quoteR sep)))
  (let ((p1 Begin) (p2 End) (quoteL QuoteL) (quoteR QuoteR) (sep Sep))
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (goto-char (point-min))
        (catch 'EndReached
          (while t
            (skip-chars-forward "\t ")
            (insert quoteL)
            (end-of-line)
            (insert quoteR sep)
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
  ;; Possible values are 0 to length of charArr.
  (let* (p1 p2
            (charArr ["-" "_" " "])
            (n (length charArr))
            (regionWasActive-p (region-active-p))
            (nowState (if (eq last-command this-command)
                          (get 'cycle-hyphen-lowline-space 'state) 0))
            (changeTo (elt charArr nowState)))
    (if (and Begin End)
        (setq p1 Begin p2 End)
      (if (region-active-p)
          (setq p1 (region-beginning) p2 (region-end))
        (let ((skipChars "^\"<>(){}[]"))
          (skip-chars-backward skipChars (line-beginning-position))
          (setq p1 (point))
          (skip-chars-forward skipChars (line-end-position))
          (setq p2 (point))
          (push-mark p1))))
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (goto-char (point-min))
        (while (re-search-forward (elt charArr (% (+ nowState 2) n))
                                  (point-max) 1)
          (replace-match changeTo t t))))
    (when (or (string-equal changeTo " ") regionWasActive-p)
      (goto-char p2)
      (push-mark p1)
      (setq deactivate-mark nil))
    (put 'cycle-hyphen-lowline-space 'state (% (+ nowState 1) n))))

(defun copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
If in dired, copy the current or marked files.
If a buffer is not file and not dired, copy value of `default-directory'."
  (interactive "P")
  (let ((fpath
         (if (and (string-equal major-mode 'dired-mode)
                  (not DirPathOnlyQ))
             (let ((result (mapconcat #'identity
                                      (dired-get-marked-files) "\n")))
               (if (zerop (length result))
                   (progn default-directory)
                 (progn result)))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (file-name-directory fpath)
       (let* ((xx (if (eq major-mode 'dired-mode)
                      fpath
                    (concat fpath ":" (format-mode-line "%l"))))
              (x (if prefix-arg
                     xx
                   (replace-regexp-in-string (getenv "HOME") "~" xx))))
         (message "Copy %s" x)
         x)))))

(defun cut-text-block ()
  "Cut text block plus blank lines or selection."
  (interactive)
  (if buffer-read-only
      (setq this-command 'ignore)
    (let (p1 p2)
      (if (region-active-p)
          (setq p1 (region-beginning) p2 (region-end))
        (if (re-search-backward "\n[ \t]*\n+" nil 1)
            (setq p1 (goto-char (match-end 0)))
          (setq p1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil 1)
            (setq p2 (match-end 0))
          (setq p2 (point-max))))
      (kill-region p1 p2))))

(defun clear-r1 ()
  "Clear register 1. See also: `paste-from-r1', `copy-to-register'."
  (interactive)
  (copy-to-register ?1 (point-min) (point-min))
  (message "Clear register"))

(defun copy-to-r1 ()
  "Copy current line or selection to register 1.
See also: `paste-from-r1', `copy-to-register'."
  (interactive)
  (let (p1 p2)
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (setq p1 (line-beginning-position) p2 (line-end-position)))
    (copy-to-register ?1 p1 p2)
    (let ((message-log-max nil))
      (message "Copy register")))
  (return-before-copy))

(defun append-to-r1 ()
  "Append current line or selection to register 1.
When no selection, append current line, with newline char.
See also: `paste-from-r1', `copy-to-register'."
  (interactive)
  (let (p1 p2)
    (if (region-active-p)
         (setq p1 (region-beginning) p2 (region-end))
      (setq p1 (line-beginning-position) p2 (line-end-position)))
    (append-to-register ?1 p1 p2)
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
  (unless isearch-mode
    (isearch-mode t))
  (with-temp-buffer
    (insert-register ?1 t)
    (isearch-yank-string
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun beg-of-buffer (fun &rest r)
  "Go to the beginning of buffer if no selection."
  (save-excursion
    (unless (use-region-p)
      (goto-char (point-min)))
    (apply fun r)))

;; make replace from beginning of the buffer
(advice-add 'query-replace :around #'beg-of-buffer)
(advice-add 'query-replace-regexp :around #'beg-of-buffer)


;; Insertion commands

(defun insert-date ()
  "Insert current date time.
Insert date in this format: yyyy-mm-dd.
If `universal-argument' is called first, prompt for a format to use.
If there is selection, delete it first."
  (interactive)
  (let ((style
         (if current-prefix-arg
             (completing-read
              "Style:"
              '("ISO date: 2018-04-12"
                "ISO full: 2018-04-12T22:46:11-07:00"
                "ISO space: 2018-04-12 22:46:11-07:00"
                "org mode:  <2018-04-12 Thu>"
                "all digits:  20180412224611"
                "weekday:  2018-04-12 Thursday"))
           (if (eq major-mode 'org-mode)
               "org mode: <2018-04-12 Thu>"
             "ISO date: 2018-04-12"))))
    (when (region-active-p)
      (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((string-match "^ISO date" style) (format-time-string "%Y-%m-%d"))
      ((string-match "^org mode" style)
       (concat " <" (let ((mm (number-to-string
                                 (- (string-to-number (format-time-string "%M"))
                                    (mod (string-to-number
                                          (format-time-string "%M")) 5)))))
                      (concat (format-time-string "%Y-%m-%d %a %H:")
                              (if (= 1 (length mm)) "0") mm)) ">"))
      ((string-match "^all digits" style) (format-time-string "%Y%m%d%H%M%S"))
      ((string-match "^ISO full" style)
       (concat
        (format-time-string "%Y-%m-%dT%T")
        (funcall (lambda (xx) (format "%s:%s" (substring xx 0 3)
                                      (substring xx 3 5)))
                 (format-time-string "%z"))))
      ((string-match "^ISO space" style)
       (concat
        (format-time-string "%Y-%m-%d %T")
        (funcall (lambda (xx) (format "%s:%s" (substring xx 0 3)
                                      (substring xx 3 5)))
                 (format-time-string "%z"))))
      ((string-match "^weekday" style) (format-time-string "%Y-%m-%d %A"))
      (t (format-time-string "%Y-%m-%d"))))))

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
      (let ((p1 (region-beginning)) (p2 (region-end)))
        (goto-char p2) (insert RBracket)
        (goto-char p1) (insert LBracket)
        (goto-char (+ p2 2)))
    (let (p1 p2)
      (cond
       ((eq WrapMethod 'line)
        (setq p1 (line-beginning-position) p2 (line-end-position))
        (goto-char p2)
        (insert RBracket)
        (goto-char p1)
        (insert LBracket)
        (goto-char (+ p2 (length LBracket))))
       ((eq WrapMethod 'block)
        (save-excursion
          (let ((bds (get-bounds-of-block-or-region)))
            (setq p1 (car bds) p2 (cdr bds)))
          (goto-char p2)
          (insert RBracket)
          (goto-char p1)
          (insert LBracket)
          (goto-char (+ p2 (length LBracket)))))
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
          (setq p1 (point) p2 (point))
          (insert LBracket RBracket)
          (search-backward RBracket)))
       (t (progn
            ;; Wrap around “word”. Basically, want all alphanumeric,
            ;; plus hyphen and underscore, but don't want space or
            ;; punctuations. Also want chinese chars.
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq p1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq p2 (point))
            (goto-char p2)
            (insert RBracket)
            (goto-char p1)
            (insert LBracket)
            (goto-char (+ p2 (length LBracket)))))))))

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
  (if buffer-read-only
      (setq this-command 'ignore)
    (insert " ")))

(defun insert-formfeed ()
  "Insert a form feed char (codepoint 12)."
  (interactive)
  (insert "\n\u000c\n"))

(defun insert-column-a-z ()
  "Insert letters A to Z vertically, similar to `rectangle-number-lines'.
The commpand will prompt for a start char, and number of chars to insert.
The start char can be any char in Unicode."
  (interactive)
  (let ((startChar (string-to-char (read-string "Start char: " "a")))
        (howmany (string-to-number (read-string "How many: " "26")))
        (colpos (- (point) (line-beginning-position))))
    (dotimes (i howmany)
      (progn
        (insert-char (+ i startChar))
        (forward-line)
        (beginning-of-line)
        (forward-char colpos)))))


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
  (push-mark (point) t nil)
  (if (region-active-p)
      (if visual-line-mode
          (let ((xp1 (point)))
            (end-of-visual-line 1)
            (if (eq xp1 (point)) (end-of-visual-line 2)))
        (forward-line 1)
        (end-of-line))
    (if visual-line-mode
        (progn
          (beginning-of-visual-line)
          (push-mark (point) t t)
          (end-of-visual-line))
      (push-mark (line-beginning-position) t t)
      (end-of-line))))

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
   ((and (eq major-mode 'js-mode)
         (looking-back "}" (- (point) 1)))
    (bchar) ; no time to figure it out
    (select-word))
   (t
    (mark-sexp)
    (exchange-point-and-mark)))
  (let ((r (buffer-substring-no-properties (region-beginning) (region-end))))
    (when (and (region-active-p) (< 5 (length r)))
      (setq cur-hl-regexp r)
      (highlight-regexp cur-hl-regexp 'hl-select-word))))

(add-hook 'deactivate-mark-hook (lambda () (unhighlight-regexp cur-hl-regexp)))

(defconst pair-brackets '(("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">") ("“" . "”")))

(defun select-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \" ` ' and anything in `brackets'.
Limit selection by current line.
This command ignores nesting. For example, if text is
    (a(b)c▮)
the selected char is “c”, not “a(b)c”."
  (interactive)
  (push-mark (point) t nil)
  (let ((skipChars (concat "^\"`'" (mapconcat #'identity brackets ""))))
      (skip-chars-backward skipChars)
      (setq skipChar (buffer-substring-no-properties (- (point) 1) (point)))
      (if (member skipChar left-brackets)
          (setq skipChar (cdr (assoc skipChar pair-brackets))))
      (push-mark (point) t t)
      (skip-chars-forward (concat "^" skipChar))))

(defun deactivate-mark-if-active (&rest _)
  "Deactivate mark if region active."
  (if (region-active-p)
      (deactivate-mark)))


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
   ((string-equal (buffer-name) "scratch.el") nil)
   ((string-equal (buffer-name) "tetris-scores") nil)
   ((string-equal (buffer-name) "snake-scores") nil)
   ((string-equal (buffer-name) "ai") nil)
   ((string-equal buffer-file-truename org-agenda-file-1) nil)
   ((string-equal buffer-file-truename org-agenda-file-2) nil)
   ((string-equal buffer-file-truename org-agenda-file-3) nil)
   ((string-match ".+em/project+." default-directory) nil)
   ((and buffer-file-truename (string-match "^\.sql" (buffer-name))) nil)
   (t t)))

(defvar buf-count-limit 99 "Buffer count limit.")

(defun back-buf ()
  "Switch to the previous buffer."
  (interactive)
  (if (project-buffer-p)
      (prev-proj-buf)
    (let ((i 0)
          (buf (current-buffer))
          (err "No prev buffer"))
      (previous-buffer)
      (while (< i buf-count-limit)
        (if (not (user-buffer-p))
            (progn
              (previous-buffer)
              (setq i (1+ i))
              (when (= i buf-count-limit)
                (message "%s" err)
                (switch-to-buffer buf)))
          (setq i (1+ buf-count-limit))))
      (when (eq buf (current-buffer))
        (user-error "%s" err)))))

(defun forw-buf ()
  "Switch to the next buffer."
  (interactive)
  (if (project-buffer-p)
      (next-proj-buf)
    (let ((i 0)
          (buf (current-buffer))
          (err "No next buffer"))
      (next-buffer)
      (while (< i buf-count-limit)
        (if (not (user-buffer-p))
            (progn
              (next-buffer)
              (setq i (1+ i))
              (when (= i buf-count-limit)
                (message "%s" err)
                (switch-to-buffer buf)))
          (setq i (1+ buf-count-limit))))
      (when (eq buf (current-buffer))
        (user-error "%s" err)))))

(defun prev-buf ()
  "Switch to the previous buffer."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (setq this-command 'switch-to-buffer)
        (command-execute 'switch-to-buffer))
    (command-execute 'back-buf)))

(defun next-buf ()
  "Switch to the next buffer."
  (interactive)
  (if (equal before-last-command this-command)
      (progn
        (setq this-command 'switch-to-buffer)
        (command-execute 'switch-to-buffer))
    (command-execute 'forw-buf)))

(defun project-buffer-p ()
  "Return t if current buffer is a project buffer, else nil."
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1))
    nil)
   ((and (string-match ".+em/project+." default-directory)
         (not (string-equal major-mode "dired-mode")))
    t)))

(defun prev-proj-buf ()
  "Switch to the previous project buffer."
  (interactive)
  (let ((i 0)
        (buf (current-buffer))
        (err "No prev proj buffer"))
    (previous-buffer)
    (while (< i buf-count-limit)
      (if (not (project-buffer-p))
          (progn
            (previous-buffer)
            (setq i (1+ i))
            (when (= i buf-count-limit)
              (message "%s" err)
              (switch-to-buffer buf)))
        (setq i (1+ buf-count-limit))))))

(defun next-proj-buf ()
  "Switch to the next project buffer."
  (interactive)
  (let ((i 0)
        (buf (current-buffer))
        (err "No next proj buffer"))
    (next-buffer)
    (while (< i buf-count-limit)
      (if (not (project-buffer-p))
          (progn
            (next-buffer)
            (setq i (1+ i))
            (when (= i buf-count-limit)
              (message "%s" err)
              (switch-to-buffer buf)))
        (setq i (1+ buf-count-limit))))))

(defun next-eww-buf ()
  "Switch to the next eww buffer."
  (interactive)
  (let ((i 0)
        (buf (current-buffer))
        (err "No next eww buffer"))
    (next-buffer)
    (while (< i buf-count-limit)
      (if (not (eq major-mode 'eww-mode))
          (progn
            (next-buffer)
            (setq i (1+ i))
            (when (= i buf-count-limit)
              (message "%s" err)
              (switch-to-buffer buf)))
        (setq i (1+ buf-count-limit))))))

(defun prev-eww-buf ()
  "Switch to the previous eww buffer."
  (interactive)
  (let ((i 0)
        (buf (current-buffer))
        (err "No prev eww buffer"))
    (previous-buffer)
    (while (< i buf-count-limit)
      (if (not (eq major-mode 'eww-mode))
          (progn
            (previous-buffer)
            (setq i (1+ i))
            (when (= i buf-count-limit)
              (message "%s" err)
              (switch-to-buffer buf)))
        (setq i (1+ buf-count-limit))))))

(defun prev-vterm-buf ()
  "Switch to the previous vterm buffer."
  (interactive)
  (let ((i 0)
        (buf (current-buffer))
        (err "No prev vterm buffer"))
    (previous-buffer)
    (while (< i buf-count-limit)
      (if (or (not (eq major-mode 'vterm-mode))
              (string-equal (buffer-name) "*clock*"))
          (progn
            (previous-buffer)
            (setq i (1+ i))
            (when (= i buf-count-limit)
              (message "%s" err)
              (switch-to-buffer buf)))
        (setq i (1+ buf-count-limit))))))

(defun next-vterm-buf ()
  "Switch to the next vterm buffer."
  (interactive)
  (let ((i 0)
        (buf (current-buffer))
        (err "No next vterm buffer"))
    (next-buffer)
    (while (< i buf-count-limit)
      (if (or (not (eq major-mode 'vterm-mode))
              (string-equal (buffer-name) "*clock*"))
          (progn
            (next-buffer)
            (setq i (1+ i))
            (when (= i buf-count-limit)
              (switch-to-buffer buf)))
        (setq i (1+ buf-count-limit))))
    (if (and (eq buf (current-buffer))
             (not (eq major-mode 'vterm-mode)))
        (vterm)
      (if (or (eq (point) (point-min))
              (eq (point) (line-beginning-position)))
          (vterm-reset-cursor-point)))))

(defun prev-eshell-buf ()
  "Switch to the previous eshell buffer."
  (interactive)
  (let ((i 0)
        (buf (current-buffer))
        (err "No prev eshell buffer"))
    (previous-buffer)
    (while (< i buf-count-limit)
      (if (not (eq major-mode 'eshell-mode))
          (progn
            (previous-buffer)
            (setq i (1+ i))
            (when (= i buf-count-limit)
              (message "%s" err)
              (switch-to-buffer buf)))
        (setq i (1+ buf-count-limit))))))

(defun next-eshell-buf ()
  "Switch to the next eshell buffer."
  (interactive)
  (let ((i 0)
        (buf (current-buffer))
        (err "No next eshell buffer"))
    (next-buffer)
    (while (< i buf-count-limit)
      (if (not (eq major-mode 'eshell-mode))
          (progn
            (next-buffer)
            (setq i (1+ i))
            (when (= i buf-count-limit)
              (message "%s" err)
              (switch-to-buffer buf)))
        (setq i (1+ buf-count-limit))))))

(defun prev-dired-buf ()
  "Switch to the previous dired buffer."
  (interactive)
  (let ((i 0)
        (buf (current-buffer))
        (err "No prev dired buffer"))
    (previous-buffer)
    (while (< i buf-count-limit)
      (if (not (eq major-mode 'dired-mode))
          (progn
            (previous-buffer)
            (setq i (1+ i))
            (when (= i buf-count-limit)
              (message "%s" err)
              (switch-to-buffer buf)))
        (setq i (1+ buf-count-limit))))))

(defun next-dired-buf ()
  "Switch to the next dired buffer."
  (interactive)
  (let ((i 0)
        (buf (current-buffer))
        (err "No next dired buffer"))
    (next-buffer)
    (while (< i buf-count-limit)
      (if (not (eq major-mode 'dired-mode))
          (progn
            (next-buffer)
            (setq i (1+ i))
            (when (= i buf-count-limit)
              (message "%s" err)
              (switch-to-buffer buf)))
        (setq i (1+ buf-count-limit))))))

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

(defconst new-buffer-prefix "buffer" "New buffer prefix.")

(defun new-empty-buffer ()
  "Create a new empty buffer. New buffer is named buffer, buffer<2>, etc."
  (interactive)
  (let ((buf (generate-new-buffer new-buffer-prefix)))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    buf))

(defvar recently-closed-buffers nil
  "A alist of recently closed buffers. Each element is (bufferName . filePath).
The max number to track is controlled by the variable
`recently-closed-buffers-max'.")

(defconst recently-closed-buffers-max 40
  "The maximum length for `recently-closed-buffers'.")

(defun save-close-buf ()
  "Save and close current buffer. If the buffer is not a file, save it
to `user-emacs-directory' temp and named file_‹datetime›_‹randomhex›.txt.
Switch to the same buffer type after close, e.g. user or project."
  (interactive)
  (if (buffer-file-name)
      (when (buffer-modified-p)
        (save-buffer))
    (when (user-buffer-p)
      (widen)
      (when (not (equal (point-max) 1))
        (write-file
         (format "%s%s-%x.txt"
                 (concat user-emacs-directory "temp/")
                 (format-time-string "%Y%m%d-%H%M%S")
                 (random #xfffff))))))
  (let ((type (if (project-buffer-p) "proj" "user")))
    (close-current-buffer)
    (cond
     ((eq major-mode 'dired-mode) nil)
     ((eq major-mode 'ibuffer-mode) nil)
     ((eq major-mode 'vterm-mode)
      (prev-vterm-buf))
     ((string-equal type "proj")
      (unless (project-buffer-p)
        (prev-proj-buf)))
     ((string-equal type "user")
      (unless (user-buffer-p)
        (prev-buf))))))

(defun close-current-buffer ()
  "Close the current buffer.
Similar to `kill-buffer', with the following addition:
- Prompt user to save if the buffer has been modified even if the
  buffer is not associated with a file.
- If the buffer is editing a source code file in an `org-mode' file,
  prompt the user to save before closing.
- If the buffer is a file, add the path to the list `recently-closed-buffers'."
  (interactive)
  (let ((isOrgModeSourceFile (string-match "^*Org Src" (buffer-name))))
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
                   isOrgModeSourceFile)
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
  (if (zerop (length recently-closed-buffers))
      (message "No recently closed buffers in this session")
    (find-file (cdr (pop recently-closed-buffers)))))

(defun describe-foo-at-point-error ()
  "Ignore in case of error."
  (condition-case nil
      (progn
        (if (or (eq major-mode 'python-ts-mode)
                (eq major-mode 'go-ts-mode))
            (progn
              (setq this-command 'xref-find-definitions)
              (command-execute 'xref-find-definitions))
          (setq this-command 'describe-foo-at-point)
          (describe-foo-at-point)))
    (error
     (setq this-command 'ignore)
     (command-execute 'ignore))))

(defvar browse-url-ff-patterns nil "List of patterns to browse in Firefox.")

(defun open-file ()
  "Open the file path under cursor. Or do something depending on context.
If there is selection, use it for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›”
with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el”
for elisp files."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (progn
        (setq this-command 'open-in-external-app)
        (open-in-external-app))
    (let* ((input
            (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (let ((p0 (point)) p1 p2
                    (pathStops "^  \t\n\"`'“”|[]{}<>\\"))
                (skip-chars-backward pathStops)
                (setq p1 (point))
                (goto-char p0)
                (skip-chars-forward pathStops)
                (setq p2 (point))
                (goto-char p0)
                (buffer-substring-no-properties p1 p2))))
           path)
      (setq path
            (replace-regexp-in-string "^file://" ""
                                      (replace-regexp-in-string ":\\'" "" input)))
      (if (string-match-p "\\`https?://" path)
          (if (string-match-p "\\`https?://www.youtube.com" path)
              (movie path)
            (if (catch 'matched
                  (dolist (pattern browse-url-ff-patterns)
                    (when (string-match-p pattern path)
                      (throw 'matched t)))
                  nil)
                (let ((browse-url-browser-function
                       (lambda (url &optional _new-window)
                         (start-process "" nil "open" "-a" "Firefox" url))))
                  (browse-url path))
              (browse-url path)))
        (progn
          (if (string-match "#" path)
              (let ((fpath (substring path 0 (match-beginning 0)))
                    (fractPart (substring path (1+ (match-beginning 0)))))
                (if (file-exists-p fpath)
                    (progn
                      (find-file fpath)
                      (goto-char (point-min))
                      (search-forward fractPart))
                  (when (y-or-n-p (format "No file %s. Create?" fpath))
                    (find-file fpath))))
            (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" path)
                (let ((fpath (match-string-no-properties 1 path))
                      (lineNum (string-to-number (match-string-no-properties 2 path))))
                  (if (file-exists-p fpath)
                      (progn
                        (find-file fpath)
                        (goto-char (point-min))
                        (forward-line (1- lineNum)))
                    (when (y-or-n-p (format "No file %s. Create?" fpath))
                      (find-file fpath))))
              (if (file-exists-p path)
                  (if (and (> (length path) 0)
                           (not (member path '("//" "/" "." ".." ":"))))
                      (find-file path)
                    (describe-foo-at-point-error))
                (if (file-exists-p (concat path ".el"))
                    (find-file (concat path ".el"))
                  (describe-foo-at-point-error))))))))))

(defun url-paste-and-go ()
  "Go to URL from system clipboard."
  (let ((url (funcall interprogram-paste-function)))
    (if (string-match-p "\\`https?://www.youtube.com" url)
        (movie url)
      (eww url)
      (setq last-command 'eww-follow-link))))



(defvar run-output nil "Display name for output buffer.")

(defun run-current-go-file ()
  "Run, build or test current Go file. To build, call `universal-argument'
first. To test, file name must contain _test suffix."
  (interactive)
  (when (not buffer-file-name)
    (save-buffer))
  (when (buffer-modified-p)
    (save-buffer))
  (let* ((outputb "*run output*")
         (fname buffer-file-name)
         (buf (buffer-name))
         (progName "go")
         (cmdStr (concat progName " \"" fname "\" &")))
    (setq cmdStr (format
                  (cond
                   (current-prefix-arg "%s build \"%s\" ")
                   ((string-match  "_test" fname) "%s test \"%s\" ")
                   (t "%s run \"%s\" &"))
                  progName fname))
    (progn
      (message "%s" cmdStr)
      (shell-command cmdStr outputb))))

(defconst pyvenv "~/pyvenv" "Python virtual environment.")

(defconst run-current-file-map
  `(("pl" . "perl")
    ("py" . ,(if (file-exists-p pyvenv)
                (concat "source " pyvenv "/bin/activate && python")
              "python3"))
    ("sh" . "bash")
    (nil  . "bash"))
  "Alist file extension to program name.")

(defun run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call [python x.py]
in a shell.
Output is printed to buffer “*run output*”.
File suffix is used to determine which program to run, set in the variable
`run-current-file-map'."
  (interactive)
  (if (and (memq major-mode '(emacs-lisp-mode
                              python-ts-mode
                              bash-ts-mode
                              sh-mode
                              go-ts-mode))
           (y-or-n-p (concat "Run? " (buffer-file-name))))
      (progn
        (when (not buffer-file-name) (save-buffer))
        (let* ((outBuffer "*run output*")
               (extAppMap run-current-file-map)
               (fname buffer-file-name)
               (buf (buffer-name))
               (fExt (file-name-extension fname))
               (appCmdStr (cdr (assoc fExt extAppMap)))
               cmdStr)
          (setq cmdStr
                (when appCmdStr
                  (format "%s %s &" appCmdStr (shell-quote-argument fname))))
          (when (buffer-modified-p)
            (save-buffer))
          (cond
           ((string-equal fExt "el")
            (load fname))
           ((string-equal fExt "go")
            (run-current-go-file))
           (t (if appCmdStr
                  (progn
                    (message "Running %s" cmdStr)
                    (shell-command cmdStr outBuffer))
                (error "%s: Unknown extension: %s" real-this-command fExt))))
          (setq run-output (concat "Run " buf))
          (enlarge-window-split)))
    (message "Cancel run file")))

(defun clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or selection, respects `narrow-to-region'."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (point-min) end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
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
  (let ((fname (buffer-file-name))
        (dateTimeFormat "%Y-%m-%d_%H%M%S"))
    (if fname
        (let ((backupName
               (concat fname "~" (format-time-string dateTimeFormat) "~")))
          (copy-file fname backupName t)
          (message "Backup %s" (replace-regexp-in-string (getenv "HOME") "~" fname)))
      (if (eq major-mode 'dired-mode)
          (progn
            (mapc (lambda (xx)
                    (let ((backupName
                           (concat xx "~" (format-time-string dateTimeFormat) "~")))
                      (copy-file xx backupName t)))
                  (dired-get-marked-files))
            (revert-buffer))
        (user-error "%s: buffer not file nor dired" real-this-command)))))

(defun make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `make-backup'.
If the current buffer is not associated with a file nor dired, nothing's done."
  (interactive)
  (when (and (buffer-file-name)
             (buffer-modified-p))
    (save-buffer))
  (make-backup))

(defun backup-and-copy ()
  "Make backup and copy file path."
  (interactive)
  (copy-file-path)
  (make-backup-and-save))

(defun save-buffer-silent ()
  "Save buffer without message."
  (when-let ((inhibit-message t)
             ((not (minibufferp)))
             ((buffer-file-name)))
    (save-buffer)))

(defun save-buffer-silent-defer ()
  "Defer `save-buffer-silent'."
  (unless (eq major-mode 'emacs-lisp-mode)
    (run-with-timer nil nil 'save-buffer-silent)))



(defun cur-word ()
  "Get bounds of current word."
  (let (p1 p2)
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (save-excursion
        (skip-chars-backward "-_A-ZА-Яa-zа-я0-9")
        (setq p1 (point))
        (right-char)
        (skip-chars-forward "-_A-ZА-Яa-zа-я0-9")
        (setq p2 (point))))
    (setq mark-active nil)
    (when (< p1 (point)) (goto-char p1))
    (buffer-substring-no-properties p1 p2)))

(defun isearch-cur-word ()
  "Call `isearch' on current word."
  (isearch-mode t)
  (isearch-yank-string (cur-word)))

(defun isearch-wforw ()
  "Forward `isearch-cur-word'."
  (interactive)
  (isearch-cur-word)
  (isearch-repeat-forward)
  (setq this-command 'isearch-wforw))

(defun isearch-wback ()
  "Backward `isearch-cur-word'."
  (interactive)
  (isearch-cur-word)
  (isearch-repeat-backward)
  (setq this-command 'isearch-wback))

(defun occur-cur-word-run ()
  "Call `occur' on current word."
  (interactive)
  (setq defer-timer nil)
  (occur (cur-word))
  (enlarge-window-split))

(defun occur-cur-word ()
  "Defer in order to reuse double key press for another command."
  (interactive)
  (if (timerp defer-timer)
      (progn
        (cancel-timer defer-timer)
        (setq defer-timer nil))
    (setq defer-timer (run-with-timer defer-timeout nil 'occur-cur-word-run))))

(defun search-string ()
  "Search string in all files of current directory."
  (interactive)
  (let ((default (cur-word)))
    (find-text
     (read-string (format "Search (%s): " default) nil 'query-replace-history default)
     (expand-file-name "") ".[A-Za-z0-9]+$" t t)))

(defun show-in-desktop ()
  "Show current file in desktop.
This command can be called when in a file buffer or in `dired'."
  (interactive)
  (let ((path (if (eq major-mode 'dired-mode)
                  (if (eq nil (dired-get-marked-files))
                      default-directory
                    (car (dired-get-marked-files)))
                (if (buffer-file-name)
                    (buffer-file-name)
                  default-directory))))
    (cond
     ((eq system-type 'windows-nt)
      (let ((cmd (format "Explorer /select,%s"
                         (replace-regexp-in-string "/" "\\" path t t)))
            (inhibit-message t)
            (message-log-max nil))
        (shell-command cmd)))
     ((string-equal system-type "darwin")
      (call-process "open" nil 0 nil "-R" path)))))

(defun open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in Emacs Lisp, if Fname is given, open that."
  (interactive)
  (let (fileList doIt)
    (setq fileList
          (if Fname
              (list Fname)
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
    (setq doIt (if (<= (length fileList) 50)
                   t
                 (y-or-n-p (format "Open %s files?" (length fileList)))))
    (when doIt
      (cond
       ((and (string-equal major-mode 'dired-mode)
             (= 1 (length fileList)) ; single video file
             (member (file-name-extension
                      (downcase (file-truename (nth 0 fileList))))
                     video-extensions))
        (mapc (lambda (fpath) (movie fpath)) fileList))
       ((or (and (string-equal major-mode 'dired-mode)
                 (string-match (concat (getenv "HOME") "/Sound") (dired-get-filename)))
            (string-equal "mp3"
                          (file-name-extension
                           (downcase (file-truename (nth 0 fileList))))))
        (if (get-buffer emms-playlist-buffer-name)
            (emms-add-dired)
          (emms-play-dired)))
       ((eq system-type 'windows-nt)
        (let ((outBuf (get-buffer-create "*open in external app*"))
              (cmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
          (mapc
           (lambda (x)
             (apply 'start-process (append (list "open in external app" outBuf)
                                           cmdlist
                                           (list (format "'%s'" (if (string-match "'" x)
                                                                    (replace-match "`'" t t x)
                                                                  x))) nil)))
           fileList)))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fpath) (call-process "open" nil 0 nil fpath))
         fileList))))))

(defun alternate-frame ()
  "Switch to alternate buffer or frame.
If there more than one frame, switch to next frame."
  (interactive)
  (if (< 1 (length (frame-list)))
      (other-frame -1)
    (unless (minibufferp)
      (alt-buf))))

(defvar defer-timer nil "Defer timer.")
(defconst defer-timeout (/ 200 1000.0) "Defer timeout.")

(defun proced-run ()
  "Run proced."
  (setq defer-timer nil)
  (proced))

(defun proced-defer ()
  "Defer in order to reuse double key press for another command."
  (interactive)
  (if (timerp defer-timer)
      (progn
        (cancel-timer defer-timer)
        (setq defer-timer nil))
    (setq defer-timer (run-with-timer defer-timeout nil 'proced-run))))

(defun kmacro-start ()
  "Defer in order to reuse double key press for another command."
  (setq defer-timer nil)
  (kmacro-start-macro nil))

(defun kmacro-record ()
  "Start or stop macro recording."
  (interactive)
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-end-macro nil)
    (if (timerp defer-timer)
        (progn
          (cancel-timer defer-timer)
          (setq defer-timer nil))
      (setq defer-timer
            (run-with-timer defer-timeout nil 'kmacro-start)))))

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
  (let ((prompt "└ $ pushd . && ") (prompt2 "└ $ % pushd . && "))
    (or (and (terminal-prompt prompt) (terminal-input prompt))
        (and (terminal-prompt prompt2) (terminal-input prompt2)))))

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
  (unless (string-equal (expand-file-name default-directory) (car kill-ring))
    (cond
     ((eq major-mode 'eshell-mode)
      (insert "pushd . && ")
      (yank)
      (when (change-wd-p)
        (eshell-send-input)))
     ((eq major-mode 'vterm-mode)
      (vterm-insert "pushd . && ")
      (vterm-yank)
      (when (change-wd-p)
        (vterm-send-return))))))

(defun change-wd ()
  "Change working directory."
  (interactive)
  (get-wd)
  (set-wd))

(advice-add 'completion-at-point :around
            (lambda (fun &rest r) "no need complete empty command"
              (if (and (eq major-mode 'eshell-mode)
                       (zerop (length (buffer-substring-no-properties
                                       (+ (length "└ $ ") (line-beginning-position))
                                       (line-end-position)))))
                  (change-wd)
                (apply fun r))))

(defun sh ()
  "Split eshell window below if not one window."
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (let ((current-prefix-arg '-)
            (inhibit-messages t)
            (message-log-max nil))
        (call-interactively 'eshell))
    (if (one-windowp)
        (progn
          (split-window-below)
          (other-window 1)))
    (let ((inhibit-messages t)
          (message-log-max nil))
      (command-execute 'eshell))))

(defun sh-run ()
  "Run sh."
  (setq defer-timer nil)
  (command-execute 'sh))

(defun sh-defer ()
  "Defer in order to reuse double key press for another command."
  (interactive)
  (if (timerp defer-timer)
      (progn
        (cancel-timer defer-timer)
        (setq defer-timer nil))
    (setq defer-timer (run-with-timer defer-timeout nil 'sh-run))))

(defun kmacro-helper ()
  "Keyboard macro helper. Ad hoc redefine."
  (interactive)
  (setq this-command 'config)
  (command-execute 'config))

(defvar kmacro-playp nil "Keyboard macro playback predicate.")
(defvar kmacro-play-timer nil "Keyboard macro playback timer.")
(defconst kmacro-play-timeout 60 "Keyboard macro playback timeout.")

(defun kmacro-play-toggle ()
  "Toggle keyboard macro playback. Auto disable."
  (interactive)
  (setq kmacro-playp (if kmacro-playp nil t))
  (let ((message-log-max nil))
    (if kmacro-playp
        (progn
          (message "%s" "Macro playback on")
          (when (timerp kmacro-play-timer)
            (cancel-timer kmacro-play-timer))
          (setq kmacro-play-timer
                (run-with-idle-timer kmacro-play-timeout nil
                                     (lambda ()
                                       (setq kmacro-playp nil)
                                       (setq kmacro-play-timer nil)))))
      (message "%s" "Macro playback off")
      (when (timerp kmacro-play-timer)
        (cancel-timer kmacro-play-timer)))))

(defun kmacro-play ()
  "Play keyboard macro."
  (interactive)
  (if kmacro-playp
      (call-last-kbd-macro)
    (let ((message-log-max nil))
      (message "%s" "Enable macro playback")
      (setq this-command 'ignore)
      (command-execute 'ignore))))

(defun eshell-clear()
  "Clear screen eshell."
  (interactive)
  (eshell/clear t)
  (eshell-send-input))

(defun eshell-clear-input ()
  "Clear input eshell."
  (interactive)
  (if (region-active-p)
      (cut-line)
    (beg-of-line)
    (kill-line)))

(defun eshell-search-input ()
  "Eshell history ido complete."
  (interactive)
  (let ((hist (delete-dups (ring-elements eshell-history-ring))))
    (push "" hist)
    (insert (ido-completing-read "Search input: " hist))))

(defun one-windowp ()
  "Custom one window predicate."
  (or (> (window-total-height) 30)
      (one-window-p)))

(defun terminal ()
  "Split terminal window below. Switch to terminal if already split.
Open new terminal if already in terminal."
  (interactive)
  (if (fboundp 'vterm)
      (if (eq major-mode 'vterm-mode)
          (let ((current-prefix-arg '-))
            (call-interactively 'next-vterm-buf))
        (if (and (boundp 'tt-buffer)
                 tt-buffer
                 (get-buffer tt-buffer))
            (progn
              (switch-to-buffer tt-buffer)
              (if (or (eq (point) (point-min))
                      (eq (point) (line-beginning-position)))
                  (vterm-reset-cursor-point)))
          (call-interactively 'next-vterm-buf)))
    (shell)))

(defvar vterm-last-command "" "Vterm last command.")
(defconst vterm-prompt-regexp "└" "Vterm prompt regexp.")

(defun vterm-capture-command ()
  "Advice for `vterm-send-return' to capture the command."
  (let (p1 p2)
    (save-excursion
      (goto-char (point-max))
      (re-search-backward vterm-prompt-regexp nil t 1)
      (when (< (point) (point-max))
        (setq p1 (1+ (point)))
        (end-of-line)
        (setq p2 (point))))
    (if (and p1 p2)
        (setq vterm-last-command
              (string-trim
               (buffer-substring-no-properties p1 p2))))))

(advice-add 'vterm-send-return :before #'vterm-capture-command)

(defun vterm-read-send-key ()
  "Read next input event and send it to the libvterm.
Custom, added prompt on event read."
  (interactive)
  (dolist (key (vterm--translate-event-to-args
                (read-event "Press key to send...")))
    (apply #'vterm-send-key key)))

(defun vterm-up-vi-cmd ()
  "Send `<up>' to the libvterm. Activate shell vi cmd mode."
  (interactive)
  (vterm-send-key "<up>")
  (vterm-shell-vi-cmd))

(defun vterm-up ()
  "Send `<up>' to the libvterm."
  (interactive)
  (vterm-send-key "<up>"))

(defun vterm-down ()
  "Send `<down>' to the libvterm."
  (interactive)
  (vterm-send-key "<down>"))

(defun vterm-left ()
  "Send `<left>' to the libvterm."
  (interactive)
  (vterm-send-key "<left>"))

(defun vterm-right ()
  "Send `<right>' to the libvterm."
  (interactive)
  (vterm-send-key "<right>"))

(defun vterm-send-backtab ()
  "Send `<backtab>' to the libvterm. Keep custom."
  (interactive)
  (vterm-send-key "<backtab>"))

(defun vterm-history-search ()
  "History search. Map C-o to history-incremental-search-backward in zshrc
and reverse-search-history in bashrc."
  (interactive)
  (vterm-send-key (kbd "C-o")))

(defun vterm-tmux-prefix ()
  "Send tmux prefix key."
  (vterm-send-key (kbd "C-b")))

(defun vterm-tmux-copy ()
  "Activate copy mode in tmux. Prefix + ] to paste."
  (interactive)
  (vterm-tmux-prefix)
  (vterm-send-key (kbd "["))
  (vterm-reset-cursor-point))

(defun vterm-tmux-copy-hpu ()
  "Activate copy mode in tmux and half page up immediately."
  (interactive)
  (vterm-tmux-copy)
  (sit-for vterm-timer-delay)
  (vterm-send-key (kbd "SPC"))
  (vterm-send-key (kbd "."))
  (vterm-reset-cursor-point))

(defun vterm-tmux-copy-hpd ()
  "Activate copy mode in tmux and half page down immediately."
  (interactive)
  (vterm-tmux-copy)
  (sit-for vterm-timer-delay)
  (vterm-send-key (kbd "DEL"))
  (vterm-send-key (kbd "."))
  (vterm-reset-cursor-point))

(defun vterm-tmux-split-pane ()
  "Split pane in tmux."
  (interactive)
  (vterm-tmux-prefix)
  (vterm-send-key (kbd "%")))

(defun vterm-tmux-copy-self-insert ()
  "Send key to tmux copy mode."
  (interactive)
  (if (eq last-command-event 127)
      (vterm-send-key (kbd "DEL")) ; vterm-module.c:996 missing DEL
    (vterm--self-insert)))

(defun vterm-tmux-new-window ()
  "Tmux create window."
  (interactive)
  (vterm-tmux-prefix)
  (vterm-send-key (kbd "c")))

(defun vterm-tmux-close-window ()
  "Tmux close window."
  (interactive)
  (vterm-tmux-prefix)
  (vterm-send-key (kbd "&")))

(defun vterm-tmux-next-window ()
  "Tmux next window."
  (interactive)
  (vterm-tmux-prefix)
  (vterm-send-key (kbd "n")))

(defun vterm-tmux-prev-window ()
  "Tmux prev window."
  (interactive)
  (vterm-tmux-prefix)
  (vterm-send-key (kbd "p")))

(defun vterm-shell-vi-cmd ()
  "Activate vi cmd mode in shell prompt. Deactivate tmux copy mode."
  (interactive)
  (when (and (eq major-mode 'vterm-mode)
             (vterm-reset-cursor-point))
    (vterm-send-key (kbd "^["))))

(defun vterm-shell-vi-insert ()
  "Activate vi insert mode in shell prompt."
  (interactive)
  (when (and (eq major-mode 'vterm-mode)
             (string-match vterm-prompt-regexp
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
             (not (eq this-command 'term-interrupt-subjob))
             (not (eq this-command 'vterm-send-return))
             (not (eq this-command 'vterm-history-search)))
    (vterm-send-key (kbd "^["))
    (vterm-send-key (kbd "C-m"))))

(defun vterm-shell-vi-self-insert ()
  "Send key to shell prompt vi cmd mode."
  (interactive)
  (vterm--self-insert))

(defun vterm-shell-vi-push-right (Key)
  "Send key to shell prompt vi cmd mode."
  (let ((x (point)))
    (vterm-send-key Key)        ; workaround vi go after last char
    (sit-for vterm-timer-delay) ; delay to sync
    (when (and (eq x (point))   ; point did not move
               (if-let ((p1 (1+ (point))) ; all blank after next char until eol
                        (p2 (line-end-position))
                        ((>= p2 p1)))
                   (string-blank-p (buffer-substring-no-properties p1 p2))
                 t))
      (vterm-send-key (kbd "C-m"))   ; so activate insert
      (vterm-send-key "<right>")     ; go after last char
      (vterm-send-key (kbd "SPC"))   ; add space and back to cmd mode
      (vterm-send-key (kbd "^[")))))

(defun vterm-shell-vi-s ()
  "Send key to shell prompt vi cmd mode."
  (interactive)
  (vterm-shell-vi-push-right "s"))

(defun vterm-shell-vi-l ()
  "Send key to shell prompt vi cmd mode."
  (interactive)
  (vterm-send-key "l"))

(defun vterm-shell-vi-w ()
  "Send key to shell prompt vi cmd mode."
  (interactive)
  (vterm-shell-vi-push-right "w"))

(defun vterm-shell-vi-e ()
  "Send key to shell prompt vi cmd mode."
  (interactive)
  (vterm-send-key "e"))

(defun vterm-shell-vi-a ()
  "Send key to shell prompt vi cmd mode."
  (interactive)
  (vterm-send-key "a"))

(defun vterm-vi ()
  "Activate vi mode transient."
  (interactive)
  (vterm-reset-cursor-point))

(defun vterm-vi-self-insert ()
  "Send key to vi mode."
  (interactive)
  (cond
   ((eq last-command-event 127)
    (vterm-send-key (kbd "DEL")))
   ((eq last-command-event 13)
    (vterm-send-return))
   ((equal (this-command-keys) (kbd "<return>"))
    (vterm-send-return))
   (t (vterm--self-insert))))

(defun vterm-vi-escape ()
  "Send key to vi mode. Specialized."
  (interactive)
  (vterm--self-insert))

(defun vterm-vi-quit ()
  "Quit vi without save."
  (interactive)
  (when (and (y-or-n-p "Quit vi?")
             (eq major-mode 'vterm-mode))
    (vterm-send-key (kbd "^["))
    (vterm-send-key "Z")
    (vterm-send-key "Q")))

(defun vterm-vi-save-quit ()
  "Save and quit vi."
  (interactive)
  (when (and (y-or-n-p "Save and quit vi?")
             (eq major-mode 'vterm-mode))
    (vterm-send-key (kbd "^["))
    (vterm-send-key "Z")
    (vterm-send-key "Z")))

(defun vterm-reset-cursor-shape ()
  "Kill local cursor type variable in order to restore cursor change shape."
  (when (eq major-mode 'vterm-mode) ; sometimes binds and hinders indicate
    (kill-local-variable 'cursor-type)))

(advice-add 'vterm-reset-cursor-point :after #'vterm-reset-cursor-shape)

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
    (message "No player")))

(defun text-scale-reset ()
  "Reset text scale."
  (interactive)
  (text-scale-adjust 0))

(defun toggle-ibuffer ()
  "Toggle ibuffer. Force switch to current buffer to update `other-buffer'."
  (interactive)
  (if (string-equal major-mode "ibuffer-mode")
      (switch-to-buffer (other-buffer))
    (let ((buf (buffer-name))
          (ibuf "*Ibuffer*"))
      (switch-to-buffer buf)
      (if (get-buffer ibuf)
          (switch-to-buffer ibuf)
        (let ((default-directory (expand-file-name "~/")))
          (ibuffer)))
      (condition-case nil
          (ibuffer-jump-to-buffer buf)
        ;; magic number
        (error (ibuffer-forward-filter-group 4))))))

(defun ibuffer-select-group ()
  "Toggle filter group or visit buffer."
  (interactive)
  (let ((name (get-text-property (point) 'ibuffer-filter-group-name)))
    (if (stringp name)
        (ibuffer-toggle-filter-group)
      (ibuffer-visit-buffer))))

(defun flyspell-goto-prev-error ()
  "Go to prev error."
  (interactive)
  (flyspell-goto-next-error t))

(defun sun-moon ()
  "Show the Sun and the Moon."
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
  (condition-case user-error
      (org-agenda-switch-to)
    (error (toggle-ibuffer))))

(defun todo ()
  "Modification of `org-todo'. Capitalize task."
  (interactive)
  (cond
   ((eq major-mode 'org-mode) (org-todo))
   ((eq major-mode 'org-agenda-mode)
    (if (org-get-at-bol 'org-marker) ; avoid error
        (org-agenda-todo)
      (novel)))
   (t (setq this-command 'ignore)
      (command-execute 'ignore)))
  (when (eq major-mode 'org-mode)
    (beginning-of-line)
    (title-case-region-or-line)))

(defun toggle-gnus ()
  "Toggle gnus."
  (interactive)
  (if (and (boundp onlinep) onlinep) ; need own online predicate
      (progn
        (when (display-graphic-p)
          (make-frame-command)
          (other-frame 1))
        (if (get-buffer "*Group*")
            (switch-to-buffer "*Group*")
          (gnus)
          (unless (display-graphic-p)
            (gnus-nnews-inbox))))
    (message "%s" "Offline")))

(defun password-store ()
  "Go to password store."
  (interactive)
  (if-let ((store "~/.password-store")
           ((file-exists-p store)))
      (find-file store)
    (message "%s %s" store "not found")))

(defun sql ()
  "Open SQL client or toggle sql type."
  (interactive)
  (let ((file ".sql"))
    (if (string-match (concat "^\\" file) (buffer-name))
        (toggle-sql-type)
      (if-let ((sql (concat "~/" file))
               ((file-exists-p sql)))
          (find-file sql)
        (message "%s %s" sql "not found")))))

(defvar sql-type "async" "SQL type for client.")

(defconst sql-type-list '("async" "MS SQL" "Postgres" "SQLite") "List of SQL types.")

(defun toggle-sql-type ()
  "Toggle `sql-type'."
  (interactive)
  (setq sql-type
        (nth (mod (1+ (cl-position sql-type sql-type-list :test 'string-equal))
                  (length sql-type-list))
             sql-type-list)))

(defun exec-query ()
  "Execute SQL statement separated by semicolon or selected region.
This is toy version. Target is async version over comint not included here."
  (interactive)
  (unless (eq major-mode 'sql-mode)
    (error "Not SQL"))
  (let ((conn (getenv "CONNINFO"))
        (buf (concat "*exec query*"))
        query p1 p2 (sepRegex ";"))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (save-excursion
        (setq p1 (if (re-search-backward sepRegex nil 1)
                     (goto-char (match-end 0))
                   (point)))
        (setq p2 (if (re-search-forward sepRegex nil 1)
                     (match-beginning 0)
                   (point)))))
    (setq query (string-trim
                 (concat (buffer-substring-no-properties p1 p2))))
    (switch-to-buffer-other-window (get-buffer-create buf))
    (with-current-buffer buf
      (read-only-mode))
    (goto-char (point-max))
    (push-mark (point) t nil)
    (let ((inhibit-read-only t))
      (cond
       ((string-equal sql-type "MS SQL")
        (let ((conn (getenv "SQLCMDSERVER")))
          (insert
           (shell-command-to-string (format "sqlcmd -S %s -Q \"%s\"" conn query)))))
       ((string-equal sql-type "Postgres")
        (if (eq system-type 'windows-nt) ; use temp file
            (let* ((localapp (string-replace "\\" "/" (getenv "localappdata")))
                   (file (format "%s%s-%x.sql" (concat localapp "/Temp/")
                                 (format-time-string "%Y%m%d-%H%M%S")
                                 (random #xfffff))))
              (with-temp-buffer
                (prin1 query (current-buffer)) ; with quotes
                (write-region (1+ (point-min)) (1- (point-max)) file nil 'quiet))
              (insert
               (shell-command-to-string (format "psql --file \"%s\" --quiet %s" file conn))))
          (setq query (string-replace  "\"" "\\\"" (string-replace "$" "\\$" query)))
          (insert
           (shell-command-to-string (format "psql --command \"%s\" --quiet %s" query conn)))))
       ((string-equal sql-type "SQLite")
        ;; attach database can't parse ~
        (setq query (replace-regexp-in-string "~" (getenv "HOME") query))
        ;; way to send attach and dot commands along with sql query
        (let ((res (shell-command-to-string
                    (format "sqlite3 <<EOF\n%s\nEOF" query))))
          (if (zerop (length res))
              (insert "No rows\n")
            (insert res)))))
      (insert (concat (make-string 200 45) "\n")))
    (set-mark-command t)
    (other-window 1)
    (enlarge-window-split))
  (return-before))

(defun eval-region-or-sexp ()
  "Eval region or last sexp."
  (interactive)
  (if (use-region-p)
      (command-execute 'eval-region)
    (command-execute 'eval-last-sexp)))

(defun novel ()
  "Read novel."
  (interactive)
  (switch-to-buffer "*novel*"))

(defun hippie-expand-reset ()
  "Reset hippie expand."
  (interactive)
  (hippie-expand -1))

(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:
- for a function name where point is;
- for a variable name where point is."
  (interactive)
  (let (sym)
    (cond
     ((setq sym (ignore-errors
                  (with-syntax-table emacs-lisp-mode-syntax-table
                    (save-excursion
                      (or (not (zerop (skip-syntax-backward "_w")))
                          (eq (char-syntax (char-after (point))) ?w)
                          (eq (char-syntax (char-after (point))) ?_)
                          (forward-sexp -1))
                      (skip-chars-forward "`'")
                      (let ((obj (read (current-buffer))))
                        (and (symbolp obj)
                             (fboundp obj)
                             obj))))))
      (describe-function sym))
     ((setq sym (variable-at-point))
      (describe-variable sym)))))

(defconst video-extensions '("mkv" "mp4" "avi" "mov" "ts" "mts" "m2ts" "webm" "vob" "aiff")
  "Open these video file extensions with `open-in-external-app'.")

(defconst external-extensions `("mp3" "m4a" "flac" "torrent" "exe" "xlsx" "docx" "dmg")
  "Open these file extensions with `open-in-external-app'.")

(setq external-extensions (append external-extensions video-extensions))

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
  (cond
   ((eq major-mode 'org-mode)
    (org-insert-structure-template "src")
    (insert "bash")
    (newline))
   ((eq major-mode 'vterm-mode)
    (setq this-command 'vterm-vi-save-quit)
    (vterm-vi-save-quit))
   (t
    (message "%s" "Not org"))))

(defun repeat-command ()
  "Custom `repeat-complex-command'."
  (interactive)
  (cond
   ((eq major-mode 'vterm-mode)
    (setq this-command 'vterm-vi-quit)
    (vterm-vi-quit))
   (t
    (setq this-command 'repeat-complex-command)
    (call-interactively 'repeat-complex-command))))

(defun dired-toggle-mark ()
  "Toggle mark for the current file."
  (interactive)
  (if (string-equal
       " " (buffer-substring
            (line-beginning-position)
            (1+ (line-beginning-position))))
      (dired-mark 1)
    (dired-unmark 1)))

(defun double-jump-back ()
  "Double jump back."
  (set-mark-command t)
  (set-mark-command t))

(defun return-before-copy (&rest _)
  "Return to the point before copy selection."
  (when (memq last-command '(select-word select-quote mark-whole-buffer))
    (double-jump-back)
    (setq this-command 'set-mark-command)))

(defun return-before (&rest _)
  "If region active deactivate mark conditionally and return to the point
before selection. This fun to be run as before advice for move fun."
  (interactive)
  (when (and (region-active-p)
             (memq last-command
                   '(select-block select-word select-line
                     select-quote mark-whole-buffer)))
    (deactivate-mark)
    (double-jump-back)))

(defun delete-before (&rest _)
  "Delete selection right before insertion."
  (when (memq last-command '(select-word select-quote))
    (del-back)))

(defun lookup-around (fun &rest r)
  "Lookup selection if buffer is read only and last command `select-word'.
Use as around advice e.g. for mouse left click after double click."
  (if (and (eq last-command 'select-word) buffer-read-only)
      (progn
        (return-before)
        (lookup-web))
    (apply fun r)))

(defun translate-around (fun &rest r)
  "Translate selection if buffer is read only and in eww."
  (if (and (eq major-mode 'eww-mode) buffer-read-only)
      (progn
        (translate)
        (setq this-command 'down-line))
    (apply fun r)))

(defun quit ()
  "Confirm and quit. Because restart without confirm."
  (interactive)
  (if (y-or-n-p-with-timeout "Quit?" 3 nil)
      (save-buffers-kill-terminal)))

(defun mouse-3 (e)
  "Mouse right click. Select word or if eww buffer then lookup translation."
  (interactive "e")
  (mouse-set-point e)
  (if (eq major-mode 'eww-mode)
      (translate)
    (when (use-region-p)
      (deactivate-mark))
    (select-word)))

(defun calendar-split ()
  "Split calendar."
  (interactive)
  (calendar)
  (other-window 1))


;; Windows

(defun enlarge-window-around (fun &rest r)
  "If one window, then split window. Otherwise enlarge window."
  (if (one-window-p)
      (progn
        (setq this-command 'split-window-below)
        (split-window-below)
        (enlarge-window-split))
    (apply fun r)))

(advice-add 'enlarge-window :around #'enlarge-window-around)

(defun window-half-height-p ()
  "Return t if WINDOW is as half high as its containing frame."
  (equal 5 (round (fceiling
                   (* 10 (/ (float (window-height)) (float (frame-height))))))))

(defun enlarge-window-split ()
  "Enlarge window if frame splitted equally."
  (when (and (window-half-height-p)
             (< 16 (window-height)))
    (enlarge-window (round (fceiling (* 0.3 (window-height)))))))

(defun other-win ()
  "Other window."
  (interactive)
  (setq this-command 'other-window)
  (other-window 1))

(defun shrink-win ()
  "Shrink window to fit rows count."
  (if-let ((num (count-lines (point-min) (point-max)))
           ((< num (window-total-height))))
      (enlarge-window (- num (window-total-height)))))

(defun shrink-completion-win ()
  "Shrink completion window."
  (when-let ((win (get-buffer-window "*Completions*")))
    (select-window win)
    (shrink-win)
    (select-window (get-buffer-window completion-reference-buffer))))

(defun completion-at-point-after (&rest _)
  "Setup after run completion at point."
  (when-let ((buf "*Completions*")
             (win (get-buffer-window buf)))
    (with-current-buffer buf
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (kill-line 4))))
    (shrink-completion-win))
  (setq this-command 'completion-at-point))

(advice-add 'completion-at-point :after #'completion-at-point-after)

(defun delete-completion-win ()
  "Delete completion window."
  (interactive)
  (when-let ((win (get-buffer-window "*Completions*")))
    (select-window win)
    (delete-window)
    (select-window (get-buffer-window completion-reference-buffer))))

(defun toggle-pin-window ()
  "Toggle current window dedicated and not deletable."
  (interactive)
  (let ((x (not (window-dedicated-p (get-buffer-window (current-buffer))))))
    (set-window-dedicated-p nil x)
    (set-window-parameter nil 'no-delete-other-windows x)))

(defun scroll-one-pixel (&rest _)
  "Scroll one pixel up. Disables recentering cursor temporary."
  (when pixel-scroll-mode
    (pixel-scroll-pixel-up 1)))

(defalias 'view-messages 'view-echo-area-messages)

(defun save-all-unsaved ()
  (interactive)
  (save-some-buffers t))

(defun save-buffer-isearch-cancel ()
  "Cancel isearch and save buffer."
  (interactive)
  (isearch-cancel-clean)
  (cond
   ((buffer-file-name)
    (if (buffer-modified-p)
        (save-buffer)
      (setq this-command 'ignore) ; indicate only if modified
      (command-execute 'ignore)))
   ((string-match (concat "^" new-buffer-prefix "*.") (buffer-name))
    (command-execute 'write-file))
   (t (setq this-command 'ignore)
      (command-execute 'ignore))))

(defun empty-bin ()
  "Empty bin on macOS."
  (interactive)
  (when (and (y-or-n-p-with-timeout "Empty bin?" 3 nil)
             (string-equal system-type "darwin"))
    (call-process "osascript" nil 0 nil "-e" "tell app \"Finder\" to empty")))

(defun rectangle ()
  "Rectangle mark mode."
  (interactive)
  (rectangle-mark-mode))

(defun quoted-insert-custom (arg)
  "Same as original `quoted-insert' but escape key ignored."
  (interactive "*p")
  (let* ((char
          ;; Avoid "obsolete" warnings for translation-table-for-input.
          (with-no-warnings
            (let (translation-table-for-input input-method-function)
              (if (or (not overwrite-mode)
                      (eq overwrite-mode 'overwrite-mode-binary))
                  (read-quoted-char)
                (read-char))))))
    ;; This used to assume character codes 0240 - 0377 stand for
    ;; characters in some single-byte character set, and converted them
    ;; to Emacs characters.  But in 23.1 this feature is deprecated
    ;; in favor of inserting the corresponding Unicode characters.
    ;; (if (and enable-multibyte-characters
    ;;          (>= char ?\240)
    ;;          (<= char ?\377))
    ;;     (setq char (unibyte-char-to-multibyte char)))
    (unless (characterp char)
      (user-error "%s is not a valid character"
                  (key-description (vector char))))
    (if (eq char 27) ; escape not inserted
        (ignore)
      (if (> arg 0)
          (if (eq overwrite-mode 'overwrite-mode-binary)
              (delete-char arg)))
      (while (> arg 0)
        (insert-and-inherit char)
        (setq arg (1- arg))))))

(defvar hscroll-columns 4 "Scroll selected window display columns.")

(defun hscroll-left ()
  "Horizontal scroll left."
  (interactive)
  (scroll-left hscroll-columns))

(defun hscroll-right ()
  "Horizontal scroll right."
  (interactive)
  (scroll-right hscroll-columns))

(defun eval-defun-visual ()
  "Same as `eval-defun' but highlight defun."
  (interactive)
  (when (fboundp 'uncentered-cursor)
    (uncentered-cursor))
  (command-execute 'eval-defun)
  (push-mark (point) t nil)
  (mark-defun)
  (sit-for 0.1)
  (set-mark-command t)
  (set-mark-command t)
  (when (fboundp 'centered-cursor)
    (centered-cursor)))

(defun copy-char ()
  "Prompt for a character and copy it to the kill-ring."
  (interactive)
  (let* ((char (read-char-by-name "Copy char: "))
         (charName (or (get-char-code-property char 'name) "")))
    (kill-new (char-to-string char))
    (message "Copied: %c (%s)" char charName)))

(defconst prog-commands
  '(("python-new" . "New Python file.")
    ("go-new" . "New Go file.")
    ("bash-new" . "New Bash file."))
  "Alist of commands and their descriptions.")

(defun prog-new (Cmd)
  "New prog file, commands defined in `prog-commands'.
Marginalia annotation support."
  (interactive
   (list (completing-read
          "New prog file: "
          (lambda (str pred action)
            (if (eq action 'metadata)
                `(metadata
                  (annotation-function
                   . ,(lambda (key)
                        (format " %s" (cdr (assoc key prog-commands))))))
              (complete-with-action action (mapcar #'car prog-commands) str pred))))))
  (if-let ((cmd (intern Cmd))
           ((fboundp cmd)))
      (funcall cmd)
    (message "No command %s" Cmd)))

(defun jump-buffer-or-bookmark ()
  (interactive)
  (let* ((buffers (mapcar #'buffer-name
                          (cl-remove-if (lambda (b)
                                          (with-current-buffer b
                                            (derived-mode-p 'dired-mode)))
                                        (buffer-list))))
         (bookmarks (cl-remove-if (lambda (bm)
                                    (let ((bm-file (bookmark-get-filename bm)))
                                      (cl-some (lambda (buf)
                                                 (string= (buffer-file-name buf) bm-file))
                                               (buffer-list))))
                                  (bookmark-all-names)))
         (candidates (append buffers bookmarks))
         (collection (lambda (string pred action)
                       (if (eq action 'metadata)
                           `(metadata (category . bookmark))
                         (complete-with-action action candidates string pred)))))
    (let ((choice (minibuffer-with-setup-hook
                      (lambda ()
                        (setq-local completion-ignore-case t))
                    (completing-read "M-x buffer: " collection nil t))))
      (if (member choice buffers)
          (switch-to-buffer choice)
        (bookmark-jump choice)))))

(defun split-window-r ()
  "Delete other windows then split window right"
  (interactive)
  (delete-other-windows)
  (split-window-right))

(provide 'keycom)

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical unresolved)
;; End:
;;; keycom.el ends here
