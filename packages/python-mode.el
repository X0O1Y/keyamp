;;; python-mode.el --- major mode for editing Python code -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Egor Maltsev <x0o1@ya.ru>
;; URL: https://github.com/xEgorka/keymap/
;; Version: 0.7
;; Created: Mar 2023

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing Python code. Based on GNU Emacs Python and
;; xah-python-mode. Implements sane indentation and integration with
;; elpy. Installation:
;;
;; (require 'python-mode)


;; code

(defvar python-mode-hook nil "Standard hook for `python-mode'.")


;; commands

(defvar python-formatter-tool "black"
  "Value should be string, either “black” or “yapf”.")

(defvar python-formatter-black-path "black --skip-string-normalization"
  "Full path or name of python code formatter “black”.")

(defvar python-formatter-yapf-path "yapf"
  "Full path or name of python code formatter yapf.")

(defun python-format-buffer ()
  "Format the current buffer file by calling `python-formatter-tool'.
If buffer is a file, it is saved first."
  (interactive)
  (let ((xbuffFileName (buffer-file-name)))
    (if xbuffFileName
        (progn
          (save-buffer)
          (cond
           ((string-equal python-formatter-tool "black")
            (shell-command
             (format "%s %s -q" python-formatter-black-path xbuffFileName))
            (message ""))
           ((string-equal python-formatter-tool "yapf")
            (shell-command
             (format "%s -i %s" python-formatter-yapf-path xbuffFileName)))
           (t
            (user-error "`python-formatter-tool' should be “black” or “yapf”.")))
          (revert-buffer t t t))
      (python-format-region (point-min) (point-max)))))

(defun python-format-region (Begin End)
  "Format the current region file using `python-formatter-tool'.
The region must be a valid python code. File is saved first."
  (interactive "r")
  (cond
   ((string-equal python-formatter-tool "black")
    (shell-command-on-region
     Begin End
     (format "%s - -q" python-formatter-black-path)
     nil
     t))))

(defun run-pdb ()
  "Debug current file with pdb."
  (interactive)
  (pdb (concat "python3 -m pdb " (file-name-nondirectory buffer-file-name))))

(setq python-shell-completion-native-enable nil)

(defun run-python ()
  "Run and show Python shell, do not select other window."
  (interactive)
  (python-shell-make-comint
          (python-shell-calculate-command)
          (python-shell-get-process-name nil) t)
  (other-window 1))


;; keyword completion

(defcustom python-indent-offset 4
  "Default indentation offset for Python.")

(defun python-indent-shift-left (start end)
  "Shift lines contained in region START END by `python-indent-offset'
to the left. If region isn't active, the current line is shifted. The
shifted region includes the lines in which START and END lie. An error
is signaled if any lines in the region are indented less than
`python-indent-offset' columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (setq count python-indent-offset)
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (user-error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(defun python-indent-shift-right (start end)
  "Shift lines contained in region START END by `python-indent-offset'
columns to the right. If region isn't active, the current line is
shifted. The shifted region includes the lines in which START and END
lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count python-indent-offset)
    (indent-rigidly start end count)))

(defun python-complete-symbol ()
  "Perform keyword completion on current symbol."
  (interactive)
  (let* ((xbds (bounds-of-thing-at-point 'symbol))
         (xp1 (car xbds))
         (xp2 (cdr xbds))
         (xcurrent-sym
          (if (or (null xp1) (null xp2) (equal xp1 xp2))
              ""
            (buffer-substring-no-properties xp1 xp2)))
         xresult-sym)
    (when (not xcurrent-sym) (setq xcurrent-sym ""))
    (setq xresult-sym
          (completing-read "" python-keywords nil nil xcurrent-sym))
    (delete-region xp1 xp2)
    (insert xresult-sym)))

(defun python-indent-or-complete ()
  "If cursor is after a word, call `python-complete-symbol', else do indent."
  (interactive)
  (if (region-active-p)
      (progn
        (python-indent-shift-right (region-beginning) (region-end)))
    (progn
      (let ((xcurChar (char-before)))
        (if (or (eq xcurChar 32) (eq xcurChar 10))
            (insert-char 32 python-indent-offset)
          (python-complete-symbol))))))

(defun python-de-indent ()
  "Do de-indent."
  (interactive)
  (if (region-active-p)
      (progn
        (python-indent-shift-left (region-beginning) (region-end)))
    (progn
      (let ((xcurChar (char-before)))
        (if (eq xcurChar 32)
            (delete-char (- python-indent-offset)))))))

(defun python-return-and-indent ()
  "If cursor after colon, do return and `python-indent-or-complete',
else do return. Always keep indentation."
  (interactive)
  (let ((xcurChar (char-before)))
    (if (eq xcurChar 58)
        (progn
          (electric-indent-just-newline 1)
          (indent-relative-first-indent-point)
          (python-indent-or-complete))
      (progn
        (electric-indent-just-newline 1)
        (indent-relative-first-indent-point)))))


;; syntax coloring

(setq python-keywords
      '("and" "del" "from" "not" "while" "as" "elif" "global"
        "or" "with" "assert" "else" "if" "pass" "yield" "break"
        "except" "import" "class" "in" "raise" "continue" "finally" "is"
        "return" "def" "for" "lambda" "try" "def" "for" "with"
        "await" "self" "abs" "all" "any" "bin" "bool" "callable"
        "chr" "classmethod" "compile" "complex" "delattr" "dict" "dir" "divmod"
        "enumerate" "eval" "filter" "float" "format" "frozenset" "getattr" "globals"
        "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
        "issubclass" "iter" "len" "list" "locals" "map" "max" "memoryview"
        "min" "next" "object" "oct" "open" "ord" "pow" "print"
        "property" "range" "repr" "reversed" "round" "set" "setattr" "slice"
        "sorted" "staticmethod" "str" "sum" "super" "tuple" "type" "vars"
        "zip" "ascii" "breakpoint" "bytearray" "bytes" "exec"))

(setq python-dunder_words
      '("__import__" "__annotations__" "__closure__" "__code__"
        "__defaults__" "__dict__" "__doc__" "__globals__"
        "__kwdefaults__" "__name__" "__module__" "__package__" "__qualname__"))

(setq python-highlights
      `((,(regexp-opt python-keywords 'symbols) . 'font-lock-keyword-face)
        (,(regexp-opt python-dunder_words 'symbols) . 'font-lock-builtin-face)))


;; syntax table

(defvar python-mode-syntax-table nil "Syntax table for `python-mode'.")

(setq python-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table))


;; abbrev table, templates

(defun python-abbrev-enable-function ()
  "Return t if not in string or comment. Else nil.
This is for abbrev table property `:enable-function'."
  (let ((xsyntaxState (syntax-ppss)))
    (not (or (nth 3 xsyntaxState) (nth 4 xsyntaxState)))))

(defun python-expand-abbrev ()
  "Expand the symbol before cursor, if cursor is not in string or comment.
Returns the abbrev symbol if there's a expansion, else nil."
  (interactive)
  (when (python-abbrev-enable-function) ; abbrev property :enable-function doesn't seem to work, so check here instead
    (let ( xp1 xp2
               xabrStr
               xabrSymbol
               )

      (save-excursion
        (forward-symbol -1)
        (setq xp1 (point))
        (forward-symbol 1)
        (setq xp2 (point)))

      (setq xabrStr (buffer-substring-no-properties xp1 xp2))
      (setq xabrSymbol (abbrev-symbol xabrStr))
      (if xabrSymbol
          (progn
            (abbrev-insert xabrSymbol xabrStr xp1 xp2 )
            (python--abbrev-position-cursor xp1)
            xabrSymbol)
        nil))))

(defun python--abbrev-position-cursor (&optional Pos)
  "Move cursor back to ▮ if exist, else put at end.
Return true if found, else false."
  (interactive)
  (let ((xfoundQ (search-backward "▮" (if Pos Pos (max (point-min) (- (point) 100))) t )))
    (when xfoundQ (delete-char 1))
    xfoundQ
    ))

(defun python--ahf ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
 The “ahf” stand for abbrev hook function."
  t)

(put 'python--ahf 'no-self-insert t)

(add-hook 'python-mode-hook
          (lambda ()
            (make-local-variable 'abbrev-expand-function)
            (setq abbrev-expand-function 'python-expand-abbrev)))

(setq python-mode-abbrev-table nil)

(define-abbrev-table 'python-mode-abbrev-table
  '(("ml" "def main() -> int:\n    print(Solution().▮())\n    return 0\n\n\nif __name__ == '__main__':\n    main()" python--ahf)
    ("mm" "def main() -> int:\n    ▮\n    return 0\n\n\nif __name__ == '__main__':\n    main()" python--ahf)

    ("pp" "print(▮)"                                               python--ahf)
    ("len" "len(▮)"                                                python--ahf)
    ("min" "min(▮)"                                                python--ahf)
    ("max" "max(▮)"                                                python--ahf)
    ("ran" "range(▮)"                                              python--ahf)
    ("app" "append(▮)"                                             python--ahf)
    ("append" "append(▮)"                                          python--ahf)
    ("add" "add(▮)"                                                python--ahf)
    ("find" "find(▮)"                                              python--ahf)
    ("div" "divmod(▮)"                                             python--ahf)
    ("list" "list(▮)"                                              python--ahf)

    ("rev" "reverse()▮"                                            python--ahf)
    ("pop" "pop()▮"                                                python--ahf)
    ("popl" "popleft()▮"                                           python--ahf)
    ("set" "set()▮"                                                python--ahf)
    ("str" "str(▮)"                                                python--ahf)
    ("abs" "abs(▮)"                                                python--ahf)
    ("deque" "deque(▮)"                                            python--ahf)
    ("sort" "sort()▮"                                              python--ahf)

    ("lower" "lower()▮"                                            python--ahf)
    ("upp" "upper()▮"                                              python--ahf)
    ("upper" "upper()▮"                                            python--ahf)
    ("join" "join(▮)"                                              python--ahf)
    ("split" "split(' ')▮"                                         python--ahf)
    ("Counter" "Counter(▮)"                                        python--ahf)
    ("enumerate" "enumerate(▮)"                                    python--ahf)
    ("items" "items()▮"                                            python--ahf)
    ("ll" "from typing import List, Optional\n▮"                   python--ahf)
    ("cc" "from collections import Counter\n▮"                     python--ahf)

    ("ListNode" "ListNode(▮)"                                      python--ahf)

    ("float" "float('inf')")
    ("rr" "return")
    ("imp" "import")
    ("br" "break")
    ("nn" "None")
    ("tt" "True▮"                                                  python--ahf)
    ("ff" "False▮"                                                 python--ahf))

  "Abbrev table for `python-mode'")

(abbrev-table-put python-mode-abbrev-table :regexp "\\([_*0-9A-Za-z]+\\)")
(abbrev-table-put python-mode-abbrev-table :case-fixed t)
(abbrev-table-put python-mode-abbrev-table :system t)


;; keybinding

(defvar python-mode-map nil "Keymap for `python-mode'")
(progn
  (setq python-mode-map (make-sparse-keymap))
  (define-prefix-command 'python-leader-map)
  (define-key python-mode-map
    (if (boundp 'emacs-major-mode-leader-key)
        emacs-major-mode-leader-key
      (kbd "TAB"))
    python-leader-map)
  (define-key python-mode-map (kbd "<backtab>") 'python-de-indent)
  (define-key python-mode-map (kbd "<return>")  'python-return-and-indent)

  (define-key python-leader-map (kbd "TAB")     'python-indent-or-complete)
  (define-key python-leader-map (kbd "a")       'python-format-buffer))


;; flymake integration

(defgroup python-flymake nil
  "Integration between Python and Flymake."
  :group 'python
  :link '(custom-group-link :tag "Flymake" flymake)
  :version "26.1")

(defcustom python-flymake-command '("pyflakes")
  "The external tool that will be used to perform the syntax check.
This is a non empty list of strings, the checker tool possibly followed by
required arguments.  Once launched it will receive the Python source to be
checked as its standard input.
To use `flake8' you would set this to (\"flake8\" \"-\")."
  :version "26.1"
  :group 'python-flymake
  :type '(repeat string))

;; The default regexp accommodates for older pyflakes, which did not
;; report the column number, and at the same time it's compatible with
;; flake8 output, although it may be redefined to explicitly match the
;; TYPE
(defcustom python-flymake-command-output-pattern
  (list
   "^\\(?:<?stdin>?\\):\\(?1:[0-9]+\\):\\(?:\\(?2:[0-9]+\\):\\)? \\(?3:.*\\)$"
   1 2 nil 3)
  "Specify how to parse the output of `python-flymake-command'.
The value has the form (REGEXP LINE COLUMN TYPE MESSAGE): if
REGEXP matches, the LINE'th subexpression gives the line number,
the COLUMN'th subexpression gives the column number on that line,
the TYPE'th subexpression gives the type of the message and the
MESSAGE'th gives the message text itself.

If COLUMN or TYPE are nil or that index didn't match, that
information is not present on the matched line and a default will
be used."
  :version "26.1"
  :group 'python-flymake
  :type '(list regexp
               (integer :tag "Line's index")
               (choice
                (const :tag "No column" nil)
                (integer :tag "Column's index"))
               (choice
                (const :tag "No type" nil)
                (integer :tag "Type's index"))
               (integer :tag "Message's index")))

(defcustom python-flymake-msg-alist
  '(("\\(^redefinition\\|.*unused.*\\|used$\\)" . :warning))
  "Alist used to associate messages to their types.
Each element should be a cons-cell (REGEXP . TYPE), where TYPE
should be a diagnostic type symbol like `:error', `:warning' or
`:note'.  For example, when using `flake8' a possible
configuration could be:

  ((\"\\(^redefinition\\|.*unused.*\\|used$\\)\" . :warning)
   (\"^E999\" . :error)
   (\"^[EW][0-9]+\" . :note))

By default messages are considered errors."
  :version "26.1"
  :group 'python-flymake
  :type '(alist :key-type (regexp)
                :value-type (symbol)))

(defcustom python-forward-sexp-function #'python-nav-forward-sexp
  "Function to use when navigating between expressions."
  :version "28.1"
  :type '(choice (const :tag "Python blocks" python-nav-forward-sexp)
                 (const :tag "CC-mode like" nil)
                 function))

(defvar-local python--flymake-proc nil)

(defun python--flymake-parse-output (source proc report-fn)
  "Collect diagnostics parsing checker tool's output line by line."
  (let ((rx (nth 0 python-flymake-command-output-pattern))
        (lineidx (nth 1 python-flymake-command-output-pattern))
        (colidx (nth 2 python-flymake-command-output-pattern))
        (typeidx (nth 3 python-flymake-command-output-pattern))
        (msgidx (nth 4 python-flymake-command-output-pattern)))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (cl-loop
       while (search-forward-regexp rx nil t)
       for msg = (match-string msgidx)
       for (beg . end) = (flymake-diag-region
                          source
                          (string-to-number
                           (match-string lineidx))
                          (and colidx
                               (match-string colidx)
                               (string-to-number
                                (match-string colidx))))
       for type = (or (and typeidx
                           (match-string typeidx)
                           (assoc-default
                            (match-string typeidx)
                            python-flymake-msg-alist
                            #'string-match))
                      (assoc-default msg
                                     python-flymake-msg-alist
                                     #'string-match)
                      :error)
       collect (flymake-make-diagnostic
                source beg end type msg)
       into diags
       finally (funcall report-fn diags)))))

(defun python-flymake (report-fn &rest _args)
  "Flymake backend for Python.
This backend uses `python-flymake-command' (which see) to launch a process
that is passed the current buffer's content via stdin.
REPORT-FN is Flymake's callback function."
  (unless (executable-find (car python-flymake-command))
    (error "Cannot find a suitable checker"))

  (when (process-live-p python--flymake-proc)
    (kill-process python--flymake-proc))

  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq python--flymake-proc
            (make-process
             :name "python-flymake"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer " *python-flymake*")
             :command python-flymake-command
             :sentinel
             (lambda (proc _event)
               (when (eq 'exit (process-status proc))
                 (unwind-protect
                     (when (with-current-buffer source
                             (eq proc python--flymake-proc))
                       (python--flymake-parse-output source proc report-fn))
                   (kill-buffer (process-buffer proc)))))))
      (process-send-region python--flymake-proc (point-min) (point-max))
      (process-send-eof python--flymake-proc))))



(define-derived-mode
  python-mode
  prog-mode
  "Python"
  "Major mode for editing Python language code.

\\{python-mode-map}"

  (setq-local comment-start "# ")
  (setq-local comment-end "")

  (set-syntax-table python-mode-syntax-table)
  (setq font-lock-defaults '(python-highlights))
  (use-local-map python-mode-map)

  (abbrev-mode 1)
  :abbrev-table python-mode-abbrev-table

  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (add-hook 'flymake-diagnostic-functions #'python-flymake nil t))

(add-to-list 'auto-mode-alist '("\\.py[3]*\\'" . python-mode))

(provide 'python-mode)

;;; python-mode.el ends here
