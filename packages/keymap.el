;;; keymap.el --- modal keybinding minor mode -*- coding: utf-8; lexical-binding: t; -*-


;;; Code:

(require 'dired)
(require 'dired-x)
(require 'ido)



(defgroup keymap nil
  "Modal keybinding minor mode."
  :group 'keyboard)

(defvar keymap-command-mode-activate-hook nil "Hook for `keymap-command-mode-activate'")

(defvar keymap-insert-mode-activate-hook nil "Hook for `keymap-insert-mode-activate'")

(defvar keymap-karabiner-cli "/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli"
  "Karabiner-Elements CLI executable")

(defun keymap-escape-key ()
  "Escape key for command mode."
  (interactive)
  (if (active-minibuffer-window)
      (abort-recursive-edit)
    (if (region-active-p)
        (deactivate-mark)
      (toggle-ibuffer))))

(progn
  (defvar keymap-fast-keyseq-timeout 50)

  (defun keymap-tty-ESC-filter (map)
    (if (and (equal (this-single-command-keys) [?\e])
             (sit-for (/ keymap-fast-keyseq-timeout 1000.0)))
        [escape] map))

  (defun keymap-lookup-key (map key)
    (catch 'found
      (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

  (defun keymap-catch-tty-ESC ()
    "Setup key mappings of current terminal to turn a tty's ESC into
  `escape'."
    (when (memq (terminal-live-p (frame-terminal)) '(t pc))
      (let ((esc-binding (keymap-lookup-key input-decode-map ?\e)))
        (define-key input-decode-map
          [?\e] `(menu-item "" ,esc-binding :filter keymap-tty-ESC-filter)))))

  (keymap-catch-tty-ESC)

  ;; catched tty ESC translated to <escape>
  (define-key key-translation-map (kbd "ESC") (kbd "<escape>")))


;; layout lookup tables for key conversion

(defvar keymap-layouts nil "A alist. Key is layout name, string type.
Value is a alist, each element is of the form (\"e\" . \"d\").
First char is Qwerty, second is corresponding char of the destination layout.
When a char is not in this alist, they are assumed to be the same.")

(push '("qwerty" . nil) keymap-layouts)

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
    (">" . "F") ("?" . "P") ("1" . "9")  ("2" . "7") ("3" . "1") ("4" . "3")
    ("5" . "5") ("6" . "4") ("7" . "2")  ("8" . "0") ("9" . "6") ("0" . "8")
    ("!" . "@") ("@" . "&") ("#" . "/")  ("$" . "$") ("%" . "<") ("^" . ">")
    ("&" . "*") ("*" . "=") ("(" . "+")  (")" . "\\")))
 keymap-layouts)

(defvar keymap-current-layout nil
  "The current keyboard layout. Value is a key in `keymap-layouts'.
Do not set this variable manually. Use `keymap-set-layout' to set it.
If the value is nil, it is automatically set to \"qwerty\".
When this variable changes, suitable change must also be done to `keymap--convert-table'.")

(if keymap-current-layout nil (setq keymap-current-layout "qwerty"))

(defvar keymap--convert-table nil
  "A alist that's the conversion table from qwerty to current layout.
Value structure is one of the key's value of `keymap-layouts'.
Value is programtically set from value of `keymap-current-layout'.
Do not manually set this variable.")

(setq keymap--convert-table
      (cdr (assoc keymap-current-layout keymap-layouts)))

(defun keymap--convert-kbd-str (Charstr)
  "Return the corresponding char Charstr according to
`keymap--convert-table'. Charstr must be a string that is the argument to `kbd'. e.g. \"a\" and \"a b c\"
Each space separated token is converted according to `keymap--convert-table'."
  (interactive)
  (mapconcat
   'identity
   (mapcar
    (lambda (x)
      (let ((xresult (assoc x keymap--convert-table)))
        (if xresult (cdr xresult) x)))
    (split-string Charstr " +"))
   " "))

(defmacro keymap--define-keys (KeymapName KeyCmdAlist &optional Direct-p)
  "Map `define-key' over a alist KeyCmdAlist, with key layout remap.
The key is remapped from Dvorak to the current keyboard layout by `keymap--convert-kbd-str'.
If Direct-p is t, do not remap key to current keyboard layout.
Example usage:
;; (keymap--define-keys
;;  (define-prefix-command \\='xyz-map)
;;  \\='(
;;    (\"h\" . highlight-symbol-at-point)
;;    (\".\" . isearch-forward-symbol-at-point)
;;    (\"w\" . isearch-forward-word)))"
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName , KeymapName))
       ,@(mapcar
          (lambda (xpair)
            `(define-key
               ,xkeymapName
               (kbd (,(if Direct-p #'identity #'keymap--convert-kbd-str) ,(car xpair)))
               ,(list 'quote (cdr xpair))))
          (cadr KeyCmdAlist)))))

(defmacro keymap--define-keys-translation (KeyKeyAlist State-p)
  "Map `define-key' for `key-translation-map' over a alist KeyKeyAlist.
If State-p is nil, remove the mapping."
  (let ((xstate (make-symbol "keyboard-state")))
    `(let ((,xstate , State-p))
       ,@(mapcar
          (lambda (xpair)
            `(define-key key-translation-map
               (kbd ,(car xpair))
               (if ,xstate (kbd ,(cdr xpair)) nil)))
          (cadr KeyKeyAlist)))))

(defmacro keymap--define-keys-remap (KeymapName CmdCmdAlist)
  "Map `define-key' remap over a alist CmdCmdAlist."
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName , KeymapName))
       ,@(mapcar
          (lambda (xpair)
            `(define-key
               ,xkeymapName
               [remap ,(list (car xpair))]
               ,(list 'quote (cdr xpair))))
          (cadr CmdCmdAlist)))))


;; keymaps

(defvar keymap-map (make-sparse-keymap)
  "Backward-compatibility map for `keymap' minor mode. If
`keymap-insert-state-p' is true, point to `keymap-insert-map', else,
point to points to `keymap-command-map'.")

(make-obsolete-variable
 'keymap-map
 "Put bindings for command mode in `keymap-command-map', bindings for
insert mode in `keymap-insert-map' and bindings that are common to both
command and insert modes in `keymap-shared-map'." "2020-04-16")

(defvar keymap-shared-map (make-sparse-keymap)
  "Parent keymap of `keymap-command-map' and `keymap-insert-map'.

Define keys that are available in both command and insert modes here, like
`keymap-mode-toggle'.")

;; (cons 'keymap keymap-shared-map) makes a new keymap with `keymap-shared-map'
;; as its parent. See info node (elisp)Inheritance and Keymaps
(defvar keymap-command-map (cons 'keymap keymap-shared-map)
  "Keymap that takes precedence over all other keymaps in command mode.

Inherits bindings from `keymap-shared-map'.

In command mode, if no binding is found in this map
`keymap-shared-map' is checked, then if there is still no binding,
the other active keymaps are checked like normal. However, if a key is
explicitly bound to nil in this map, it will not be looked up in
`keymap-shared-map' and lookup will skip directly to the normally
active maps.

In this way, bindings in `keymap-shared-map' can be disabled by this map.

Effectively, this map takes precedence over all others when command mode
is enabled.")

(defvar keymap-insert-map (cons 'keymap keymap-shared-map)
  "Keymap for bindings that will be checked in insert mode. Active whenever
`keymap' is non-nil.

Inherits bindings from `keymap-shared-map'. In insert mode, if no binding
is found in this map `keymap-shared-map' is checked, then if there is
still no binding, the other active keymaps are checked like normal. However,
if a key is explicitly bound to nil in this map, it will not be looked
up in `keymap-shared-map' and lookup will skip directly to the normally
active maps. In this way, bindings in `keymap-shared-map' can be disabled
by this map.

Keep in mind that this acts like a normal global minor mode map, so other
minor modes loaded later may override bindings in this map.")

(defvar keymap--deactivate-command-mode-func nil)


;; setting keys

(defun keymap-define-keys ()
  "Define the keys for keymap.
Used by `keymap-set-layout' for changing layout."
  (let ()

    (keymap--define-keys
     keymap-shared-map
     '(("<escape>" . keymap-command-mode-activate)
       ("C-_"      . keymap-leader-key-map)
       ("C-И"      . keymap-leader-key-map)
       ("C-t"      . autocomplete))
     :direct)

    (keymap--define-keys
     keymap-command-map
     '(("<escape>" . keymap-escape-key)
       ("SPC"      . keymap-leader-key-map)
       ("DEL"      . keymap-insert-mode-activate)

       ("a" . shrink-whitespaces)         ("ф" . shrink-whitespaces)
       ("b" . toggle-letter-case)         ("и" . toggle-letter-case)
       ("c" . copy-line-or-region)        ("с" . copy-line-or-region)
       ("d" . delete-backward-smart)      ("в" . delete-backward-smart)
       ("e" . undo)                       ("у" . undo)
       ("f" . repeat)                     ("а" . repeat)
       ("g" . delete-current-text-block)  ("п" . delete-current-text-block)
       ("h" . beginning-of-line-or-block) ("р" . beginning-of-line-or-block)
       ("i" . previous-line)              ("ш" . previous-line)
       ("j" . backward-char)              ("о" . backward-char)
       ("k" . next-line)                  ("л" . next-line)
       ("l" . forward-char)               ("д" . forward-char)
       ("m" . backward-left-bracket)      ("ь" . backward-left-bracket)
       ("n" . isearch-forward)            ("т" . isearch-forward)
       ("o" . forward-word)               ("щ" . forward-word)
       ("p" . insert-space-before)        ("з" . insert-space-before)
       ("q" . reformat-lines)             ("й" . reformat-lines)
       ("r" . kill-word)                  ("к" . kill-word)
       ("s" . open-line)                  ("ы" . open-line)
       ("t" . set-mark-command)           ("е" . set-mark-command)
       ("u" . backward-word)              ("г" . backward-word)
       ("v" . paste-or-paste-previous)    ("м" . paste-or-paste-previous)
       ("w" . backward-kill-word)         ("ц" . backward-kill-word)
       ("x" . cut-line-or-region)         ("ч" . cut-line-or-region)
       ("y" . search-current-word)        ("н" . search-current-word)
       ("z" . toggle-comment)             ("я" . toggle-comment)
       ("," . next-window-or-frame)       ("б" . next-window-or-frame)
       ("`" . other-frame)                ("ё" . other-frame)
       (";" . end-of-line-or-block)       ("ж" . end-of-line-or-block)
       ("[" . backward-punct)             ("х" . backward-punct)
       ("]" . forward-punct)              ("ъ" . forward-punct)
       ("'" . alternate-buffer)           ("э" . alternate-buffer)
       ("." . forward-right-bracket)      ("ю" . forward-right-bracket)
       ("/" . goto-matching-bracket)      ("=" . goto-matching-bracket) ; remap 「.」
       ("\\" . kmacro)

       ("1" . previous-user-buffer)
       ("2" . next-user-buffer)
       ("3" . delete-other-windows)
       ("4" . split-window-below)
       ("5" . alternate-buffer)
       ("6" . pop-local-mark-ring)
       ("7" . select-line)
       ("8" . extend-selection)
       ("9" . select-text-in-quote)
       ("0" . exchange-point-and-mark)

       ("<f3>"  . pass)
       ("<f4>"  . tasks)
       ("<f5>"  . rss)
       ("<f6>"  . books)
       ("<f7>"  . email)

       ;;
       ))

    (keymap--define-keys
     (define-prefix-command 'keymap-leader-key-map)
     '(("<escape>" . keyboard-quit)
       ("ESC"      . keyboard-quit)
       ("SPC"      . repeat)
       ("DEL"      . keymap-insert-mode-activate-ru)
       ("RET"      . execute-extended-command)
       ("TAB"      . indent-for-tab-command)

       ("a" . mark-whole-buffer)
       ("b" . toggle-previous-letter-case)
       ("c" . copy-all-or-region)

       ("d ," . insert-angle-bracket)
       ("d ." . insert-markdown-triple-quote)
       ("d ;" . insert-string-assignment)
       ("d a" . insert-double-angle-bracket)
       ("d c" . insert-unicode)
       ("d d" . insert-date)
       ("d f" . insert-char)
       ("d g" . insert-curly-single-quote)
       ("d h" . insert-double-curly-quote)
       ("d i" . insert-ascii-single-quote)
       ("d j" . insert-brace)
       ("d k" . insert-paren)
       ("d l" . insert-square-bracket)
       ("d m" . insert-corner-bracket)
       ("d n" . insert-black-lenticular-bracket)
       ("d o" . insert-tortoise-shell-bracket)
       ("d p" . insert-formfeed)
       ("d r" . insert-single-angle-quote)
       ("d t" . insert-double-angle-quote)
       ("d u" . insert-ascii-double-quote)
       ("d v" . insert-markdown-quote)
       ("d y" . insert-emacs-quote)
       ("d <escape>" . keyboard-quit)
       ("d ESC"      . keyboard-quit)

       ("e d" . todo)
       ("e e" . agenda)
       ("e f" . org-time-stamp)
       ("e i" . org-up)
       ("e r" . tasks)
       ("e s" . backlog)
       ("e w" . rss)
       ("e <escape>" . keyboard-quit)
       ("e ESC"      . keyboard-quit)

       ("f" . previous-user-buffer)
       ("g" . kill-line)
       ("h" . beginning-of-buffer)

       ("i /" . revert-buffer-with-coding-system)
       ("i ;" . write-file)
       ("i b" . set-buffer-file-coding-system)
       ("i c" . copy-file-path)
       ("i d" . downloads)
       ("i f" . find-file)
       ("i h" . screenshot)
       ("i i" . bookmark-jump)
       ("i j" . recentf-open-files)
       ("i l" . new-empty-buffer)
       ("i o" . bookmark-bmenu-list)
       ("i p" . bookmark-set)
       ("i r" . open-last-closed)
       ("i s" . show-in-desktop)
       ("i t" . list-recently-closed)
       ("i u" . open-in-terminal)
       ("i w" . open-in-external-app)
       ("i y" . open-recently-closed)
       ("i <escape>" . keyboard-quit)
       ("i ESC"      . keyboard-quit)

       ("j a" . apropos-command)
       ("j b" . describe-bindings)
       ("j c" . describe-char)
       ("j d" . apropos-documentation)
       ("j e" . man)
       ("j f" . describe-face)
       ("j g" . info-lookup-symbol)
       ("j h" . view-echo-area-messages)
       ("j i" . info)
       ("j j" . describe-function)
       ("j k" . describe-key)
       ("j l" . describe-variable)
       ("j m" . describe-mode)
       ("j n" . view-lossage)
       ("j o" . describe-language-environment)
       ("j r" . apropos-variable)
       ("j s" . describe-syntax)
       ("j t" . list-timers)
       ("j u" . elisp-index-search)
       ("j v" . apropos-value)
       ("j x" . describe-command)
       ("j z" . describe-coding-system)
       ("j <escape>" . keyboard-quit)
       ("j ESC"      . keyboard-quit)

       ("k <up>"   . move-block-up)
       ("k <down>" . move-block-down)
       ("k '" . reformat-to-sentence-lines)
       ("k ," . sort-numeric-fields)
       ("k ." . sort-lines-block-or-region)
       ("k /" . slash-to-double-backslash)
       ("k 3" . number-to-register)
       ("k 4" . increment-register)
       ("k 7" . clear-register-1)
       ("k 8" . append-to-register-1)
       ("k a" . goto-char)
       ("k b" . title-case-region-or-line)
       ("k c" . copy-to-register)
       ("k d" . list-matching-lines)
       ("k e" . json-pretty-print-buffer)
       ("k f" . delete-matching-lines)
       ("k g" . delete-non-matching-lines)
       ("k h" . mark-defun)
       ("k i" . move-row-up)
       ("k j" . repeat-complex-command)
       ("k k" . switch-to-buffer)
       ("k l" . kmacro-name-last-macro)
       ("k m" . make-backup-and-save)
       ("k n" . double-backslash-to-slash)
       ("k o" . slash-to-backslash)
       ("k p" . escape-quotes)
       ("k q" . sort-lines-key-value)
       ("k r" . quote-lines)
       ("k s" . space-to-newline)
       ("k t" . delete-duplicate-lines)
       ("k u" . move-to-column)
       ("k v" . change-bracket-pairs)
       ("k w" . sort-lines-key-value)
       ("k x" . insert-column-a-z)
       ("k y" . goto-line)
       ("k z" . insert-kbd-macro)
       ("k <escape>" . keyboard-quit)
       ("k ESC"      . keyboard-quit)

       ("l ," . eww)
       ("l ." . visual-line-mode)
       ("l /" . abort-recursive-edit)
       ("l 0" . shell-command-on-region)
       ("l 1" . shopping)
       ("l 2" . global-hl-line-mode)
       ("l 3" . wclock)
       ("l 4" . display-line-numbers-mode)
       ("l 5" . weather)
       ("l 6" . calendar)
       ("l 7" . calculator)
       ("l 8" . sun-moon)
       ("l 9" . shell-command)
       ("l -" . async-shell-command)
       ("l ;" . count-matches)
       ("l a" . text-scale-adjust)
       ("l b" . save-some-buffers)
       ("l c" . flyspell-buffer)
       ("l d" . whitespace-mode)
       ("l e" . toggle-frame-maximized)
       ("l f" . shell)
       ("l g" . make-frame-command)
       ("l h" . narrow-to-page)
       ("l i" . toggle-case-fold-search)
       ("l j" . narrow-to-region-or-block)
       ("l k" . narrow-to-defun)
       ("l l" . describe-variable)
       ("l m" . jump-to-register)
       ("l n" . toggle-debug-on-error)
       ("l o" . count-words)
       ("l p" . toggle-word-wrap)
       ("l r" . read-only-mode)
       ("l s" . variable-pitch-mode)
       ("l t" . toggle-truncate-lines)
       ("l u" . widen)
       ("l v" . menu-bar-open)
       ("l w" . abbrev-mode)
       ("l <escape>" . keyboard-quit)
       ("l ESC"      . keyboard-quit)

       ("m" . dired-jump)
       ("n" . save-buffer)
       ("o" . recenter-top-bottom)
       ("p" . show-kill-ring)
       ("q" . fill-or-unfill)
       ("r" . query-replace)
       ("s" . clean-whitespace)
       ("t" . rectangle-mark-mode)
       ("u" . save-close-current-buffer)
       ("v" . insert-register)
       ("x" . save-buffers-kill-terminal)
       ("y" . find-text)
       ("z" . universal-argument)

       (", ," . run-current-file)
       (", c" . delete-frame)
       (", d" . eval-defun)
       (", e" . eval-buffer)
       (", f" . eval-region)
       (", h" . delete-current-file-make-backup)
       (", m" . eval-last-sexp)
       (", r" . eval-expression)
       (", <escape>" . keyboard-quit)
       (", ESC"      . keyboard-quit)

       ("." . toggle-eshell)
       ("'" . cycle-hyphen-lowline-space)
       (";" . last-line-of-buffer)
       ("/" . sync) ("*" . sync) ; remap 「.」 russian-computer
       ("\\" . call-last-kbd-macro)

       ("3" . delete-window)
       ("4" . split-window-right)
       ("6" . set-mark-deactivate-mark)
       ("8" . select-block)
       ("9" . ispell-word)

       ("<f4>" . agenda)
       ("<f8>" . player)

       ;; leader and hold down
       ("C-_ c"   . copy-to-register-1)          ("C-И c"   . copy-to-register-1)
       ("C-_ d d" . delete-other-windows)        ("C-И d d" . delete-other-windows)
       ("C-_ e e" . split-window-below)          ("C-И e e" . split-window-below)
       ("C-_ f"   . open-file-at-cursor)         ("C-И f"   . open-file-at-cursor)
       ("C-_ g"   . make-frame-command)          ("C-И g"   . make-frame-command)
       ("C-_ i i" . select-block)                ("C-И i i" . select-block)
       ("C-_ j j" . select-line)                 ("C-И j j" . select-line)
       ("C-_ k k" . extend-selection)            ("C-И k k" . extend-selection)
       ("C-_ l l" . select-text-in-quote)        ("C-И l l" . select-text-in-quote)
       ("C-_ m"   . set-mark-deactivate-mark)    ("C-И m"   . set-mark-deactivate-mark)
       ("C-_ n"   . save-some-buffers)           ("C-И n"   . save-some-buffers)
       ("C-_ r"   . query-replace-regexp)        ("C-И r"   . query-replace-regexp)
       ("C-_ s"   . delete-window)               ("C-И s"   . delete-window)
       ("C-_ u"   . open-last-closed)            ("C-И u"   . open-last-closed)
       ("C-_ v"   . paste-from-register-1)       ("C-И v"   . paste-from-register-1)
       ("C-_ y"   . find-name-dired)             ("C-И y"   . find-name-dired)
       ("C-_ ."   . pop-local-mark-ring)         ("C-И ."   . pop-local-mark-ring)
       ("C-_ , ," . exchange-point-and-mark)     ("C-И , ," . exchange-point-and-mark)
       ("C-_ \\"  . apply-macro-to-region-lines) ("C-И \\"  . apply-macro-to-region-lines)))

    (keymap--define-keys
     query-replace-map
     '(("C-h" . skip)
       ("C-r" . act))
     :direct)

    (keymap--define-keys
     global-map
     '(("C-r" . info))
     :direct)

    (keymap--define-keys
     isearch-mode-map
     '(("<escape>" . isearch-abort)
       ("<up>"     . isearch-ring-retreat)
       ("C-_ i i"  . isearch-ring-retreat)    ("C-И i i"  . isearch-ring-retreat)
       ("<left>"   . isearch-repeat-backward)
       ("C-_ j j"  . isearch-repeat-backward) ("C-И j j"  . isearch-repeat-backward)
       ("<down>"   . isearch-ring-advance)
       ("C-_ k k"  . isearch-ring-advance)    ("C-И k k"  . isearch-ring-advance)
       ("<right>"  . isearch-repeat-forward)
       ("C-_ l l"  . isearch-repeat-forward)  ("C-И l l"  . isearch-repeat-forward)
       ("C-_ v"    . isearch-yank-kill)       ("C-И v"    . isearch-yank-kill)))

    (keymap--define-keys
     minibuffer-local-isearch-map
     '(("<left>"  . isearch-reverse-exit-minibuffer)
       ("<right>" . isearch-forward-exit-minibuffer))
     :direct)

    (with-eval-after-load 'ibuf-ext
      (keymap--define-keys
       ibuffer-mode-map
       '(("C-h" . ibuffer-do-delete))
       :direct)

      (keymap--define-keys-remap
       ibuffer-mode-map
       '((end-of-line-or-block       . ibuffer-forward-filter-group)
         (beginning-of-line-or-block . ibuffer-backward-filter-group)
         (previous-line              . previous-line-ibuffer)
         (next-line                  . next-line-ibuffer))))

    (with-eval-after-load 'icomplete
      (keymap--define-keys
       icomplete-fido-mode-map
       '(("C-r" . icomplete-fido-delete-char))
       :direct)

      (keymap--define-keys-remap
       icomplete-fido-mode-map
       '((previous-line . icomplete-backward-completions)
         (next-line     . icomplete-forward-completions)
         (autocomplete  . icomplete-fido-delete-char))))

    (add-hook 'ido-setup-hook (lambda ()
                                (keymap--define-keys-remap
                                 ido-completion-map
                                 '((previous-line . ido-prev-match)
                                   (next-line     . ido-next-match)))))

    (with-eval-after-load 'dired
      (keymap--define-keys
       dired-mode-map
       '(("C-r" . open-in-external-app)
         ("C-h" . dired-do-delete))
       :direct)

      (keymap--define-keys-remap
       dired-mode-map
       '((backward-left-bracket . dired-mark)
         (forward-right-bracket . dired-unmark)
         (toggle-comment        . revert-buffer)
         (copy-all-or-region    . dired-do-copy)
         (insert-register       . dired-do-rename)
         (mark-whole-buffer     . dired-toggle-marks)
         (reformat-lines        . dired-hide-details-mode)
         (fill-or-unfill        . dired-toggle-read-only))))

    (with-eval-after-load 'wdired
      (keymap--define-keys
       wdired-mode-map
       '(("C-r" . wdired-finish-edit)
         ("C-h" . wdired-abort-changes))
       :direct))

    (with-eval-after-load 'doc-view
      (keymap--define-keys-remap
       doc-view-mode-map
       '((previous-line . doc-view-previous-line-or-previous-page)
         (next-line     . doc-view-next-line-or-next-page)
         (backward-char . doc-view-previous-page)
         (forward-char  . doc-view-next-page)
         (backward-word . doc-view-shrink)
         (forward-word  . doc-view-enlarge))))

    (with-eval-after-load 'mu4e-update
      (keymap--define-keys
       mu4e-update-minor-mode-map
       '(("C-r" . mu4e-update-mail-and-index))
       :direct)

      (keymap--define-keys-remap
       mu4e-update-minor-mode-map
       '((cut-line-or-region    . mu4e-mark-execute-all)
         (delete-backward-smart . mu4e-headers-mark-for-trash)
         (open-line             . mu4e-view-mark-for-read)
         (backward-left-bracket . mu4e-headers-mark-for-move)
         (backward-kill-word    . mu4e-headers-mark-for-refile))))

    (with-eval-after-load 'mu4e-view
      (keymap--define-keys-remap
       mu4e-view-mode-map
       '((paste-or-paste-previous . mu4e-view-save-attachments))))

    (with-eval-after-load 'image-mode
      (keymap--define-keys-remap
       image-mode-map
       '((forward-char  . image-next-file)
         (backward-char . image-previous-file))))

    (with-eval-after-load 'minibuffer
      (keymap--define-keys-remap
       minibuffer-local-map
       '((previous-line . previous-line-or-history-element)
         (next-line     . next-line-or-history-element))))

    (with-eval-after-load 'esh-mode
      (keymap--define-keys
       eshell-mode-map
       '(("C-h" . eshell-interrupt-process))
       :direct)

      (keymap--define-keys-remap
       eshell-mode-map
       '((save-buffers-kill-terminal . eshell-clear)
         (bookmark-jump              . eshell-previous-matching-input-from-input)
         (switch-to-buffer           . eshell-next-matching-input-from-input))))

    (with-eval-after-load 'rect
      (keymap--define-keys-remap
       rectangle-mark-mode-map
       '((copy-line-or-region          . copy-rectangle-as-kill)
         (delete-backward-smart        . kill-rectangle)
         (keymap-insert-mode-activate . replace-rectangle)
         (paste-or-paste-previous      . yank-rectangle)
         (copy-to-register             . copy-rectangle-to-register)
         (toggle-comment               . rectangle-number-lines)
         (cut-line-or-region           . clear-rectangle)
         (insert-space-before          . open-rectangle)
         (clean-whitespace             . delete-whitespace-rectangle))))

    (with-eval-after-load 'info
      (keymap--define-keys-remap
       Info-mode-map
       '((backward-left-bracket . Info-backward-node)
         (forward-right-bracket . Info-forward-node)
         (delete-backward-smart . Info-next-reference)
         (backward-kill-word    . Info-up))))

    (with-eval-after-load 'help-mode
      (keymap--define-keys-remap
       help-mode-map
       '((delete-backward-smart . forward-button)
         (undo                  . backward-button)
         (backward-left-bracket . help-go-back)
         (forward-right-bracket . help-go-forward))))

    (with-eval-after-load 'newst-plainview
      (keymap--define-keys
       newsticker--url-keymap
       '(("C-r" . newsticker-browse-url)
         ("RET" . newsticker-show-entry))
       :direct)

      (keymap--define-keys-remap
       newsticker-mode-map
       '((end-of-line-or-block       . newsticker-next-feed)
         (beginning-of-line-or-block . newsticker-previous-feed)
         (backward-left-bracket      . newsticker-previous-item)
         (forward-right-bracket      . newsticker-next-item)
         (shrink-whitespaces         . newsticker-hide-all-desc))))

    (with-eval-after-load 'nov
      (keymap--define-keys-remap
       nov-mode-map
       '((goto-matching-bracket . nov-goto-toc)
         (backward-left-bracket . nov-previous-document)
         (forward-right-bracket . nov-next-document))))

    (with-eval-after-load 'org-agenda
      (keymap--define-keys
       org-agenda-mode-map
       '(("<backspace>" . nil))
       :direct))

    (with-eval-after-load 'tetris
      (keymap--define-keys-remap
       tetris-mode-map
       '((repeat         . tetris-move-bottom)
         (newline        . tetris-start-game)
         (undo           . tetris-pause-game)
         (reformat-lines . tetris-end-game)
         (next-line      . tetris-move-down)
         (backward-char  . tetris-move-left)
         (forward-char   . tetris-move-right)
         (previous-line  . tetris-rotate-prev))))

    ;;
    ))

(keymap-define-keys)



(require 'quail)

(defconst quail-keyboard-layout-engineer-engram
  "\
                              \
  9@7&1/3$5<4>2*0=6+8\\#|%^`~  \
  bByYoOuU'(\")lLdDwWvVzZ{[    \
  cCiIeEaA,;.:hHtTsSnNqQ}]    \
  gGxXjJkK-_?!rRmMfFpP        \
                              "
  "Engineer Engram keyboard layout for Quail, e.g. for input method.")

(push (cons "engineer-engram" quail-keyboard-layout-engineer-engram)
      quail-keyboard-layout-alist)

(quail-set-keyboard-layout "engineer-engram")


;; russian-computer

(setq keymap-engineer-engram-to-russian-computer
      '(("a" . "а") ("b" . "й")  ("c" . "ф") ("d" . "ш") ("e" . "в")
        ("f" . "ю") ("g" . "я")  ("h" . "о") ("i" . "ы") ("j" . "с")
        ("k" . "м") ("l" . "г")  ("m" . "б") ("n" . "ж") ("o" . "у")
        ("q" . "э") ("r" . "ь")  ("s" . "д") ("t" . "л") ("u" . "к")
        ("v" . "з") ("w" . "щ")  ("x" . "ч") ("y" . "ц") ("z" . "х")
        ("." . "р") ("\"" . "н") ("?" . "т") ("-" . "и") ("," . "п")
        ("'" . "е") ("`" . "ё")  ("{" . "ъ")))

(defun keymap-quail-get-translation (from)
  "Get translation Engineer Engram to russian-computer.
From symbol to character code."
  (interactive)
  (let ((to (alist-get from keymap-engineer-engram-to-russian-computer
             nil nil 'string-equal)))
    (when (stringp to)
      (string-to-char to))))

(defun keymap-define-input-source (input-method)
  "Build reverse mapping for `input-method'.
Use Russian input source for command mode. Respects Engineer Engram layout."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (if (string-equal keymap-current-layout "engineer-engram")
                         (keymap-quail-get-translation (char-to-string to))
                       (quail-get-translation (cadr map) (char-to-string to) 1))))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))


;; pocket qwerty keyboard

(defun toggle-qwerty-to-engineer-engram ()
  "Toggle translate Qwerty layout to Engineer Engram on Emacs level.
Useful when Engineer Engram layout not available on OS or keyboard level."
  (interactive)
  (if (get 'toggle-qwerty-to-engineer-engram 'state)
      (progn
        (put 'toggle-qwerty-to-engineer-engram 'state nil)
        (message "Translation Qwerty to Engineer Engram deactivated"))
    (progn
      (put 'toggle-qwerty-to-engineer-engram 'state t)
      (message "Translation Qwerty to Engineer Engram activated")))
  (let ()
    (keymap--define-keys-translation
     '(("-" . "#") ("=" . "%") ("`" . "`")  ("q" . "b") ("w" . "y") ("e" . "o")
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
       (">" . "F") ("?" . "P") ("1" . "9")  ("2" . "7") ("3" . "1") ("4" . "3")
       ("5" . "5") ("6" . "4") ("7" . "2")  ("8" . "0") ("9" . "6") ("0" . "8")
       ("!" . "@") ("@" . "&") ("#" . "/")  ("$" . "$") ("%" . "<") ("^" . ">")
       ("&" . "*") ("*" . "=") ("(" . "+")  (")" . "\\"))
     (get 'toggle-qwerty-to-engineer-engram 'state))))

(global-set-key (kbd "C-f") 'toggle-qwerty-to-engineer-engram)



(defvar keymap-insert-state-p t "Non-nil means insertion mode is on.")

(defvar keymap-command-mode-indicator   "🟢"
  "Character indicating command mode is active.")
(defvar keymap-insert-mode-indicator    "🟠"
  "Character indicating insert mode is active.")
(defvar keymap-repeat-command-indicator "🔵"
  "Character indicating repeat command is active.")

(defun keymap-mode-indicator-update ()
  "Update mode indicator."
  (if (eq real-this-command 'repeat)
      (setq mode-line-front-space keymap-repeat-command-indicator)
    (progn
      (if keymap-insert-state-p
          (setq mode-line-front-space keymap-insert-mode-indicator)
        (setq mode-line-front-space keymap-command-mode-indicator)))))

(add-hook 'post-command-hook 'keymap-mode-indicator-update)

(defun keymap--update-key-map ()
  (setq keymap-map (if keymap-insert-state-p
                            keymap-insert-map
                          keymap-command-map)))

(defun keymap-set-layout (Layout)
  "Set a keyboard layout.
Argument must be one of the key name in `keymap-layouts'."
  (interactive "sType a layout: ")
  (let ((xnewlout
         (cond
          ((stringp Layout) Layout)
          ((symbolp Layout) (symbol-name Layout))
          (t (user-error "Layout %s must be a string." Layout))))
        (xoldlout keymap-current-layout))
    (setq keymap-current-layout xnewlout)
    (setq keymap--convert-table
          (cdr (assoc keymap-current-layout keymap-layouts)))
    (when (and (featurep 'keymap)
               (not (string-equal xoldlout xnewlout)))
      (keymap-define-keys))))

(defun keymap-command-mode-init ()
  "Set command mode keys."
  (interactive)
  (setq keymap-insert-state-p nil)
  (keymap--update-key-map)
  (when keymap--deactivate-command-mode-func
    (funcall keymap--deactivate-command-mode-func))
  (setq keymap--deactivate-command-mode-func
        (set-transient-map keymap-command-map (lambda () t)))
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (set-face-background 'cursor "PaleGreen")
  (setq mode-line-front-space keymap-command-mode-indicator)
  (force-mode-line-update)
  (deactivate-input-method))

(defun keymap-insert-mode-init (&optional no-indication)
  "Enter insertion mode."
  (interactive)
  (setq keymap-insert-state-p t)
  (keymap--update-key-map)
  (funcall keymap--deactivate-command-mode-func)
  (unless no-indication
    (modify-all-frames-parameters '((cursor-type . bar)))
    (set-face-background 'cursor "white")
    (setq mode-line-front-space keymap-insert-mode-indicator))
  (force-mode-line-update))

(defun keymap-command-mode-init-karabiner ()
  "Karabiner integration.
Init command mode with `keymap-command-mode-activate-hook'."
  (call-process keymap-karabiner-cli nil 0 nil
                "--set-variables" "{\"insert mode activated\":0}"))

(defun keymap-insert-mode-init-karabiner ()
  "Karabiner integration.
Init insert mode with `keymap-insert-mode-activate-hook'."
  (call-process keymap-karabiner-cli nil 0 nil
                "--set-variables" "{\"insert mode activated\":1}"))

(when (and (string-equal system-type "darwin")
           (file-exists-p keymap-karabiner-cli))
  (add-hook 'keymap-insert-mode-activate-hook  'keymap-insert-mode-init-karabiner)
  (add-hook 'keymap-command-mode-activate-hook 'keymap-command-mode-init-karabiner))

(defun keymap-command-mode-activate ()
  "Activate command mode and run `keymap-command-mode-activate-hook'."
  (interactive)
  (keymap-command-mode-init)
  (run-hooks 'keymap-command-mode-activate-hook))

(when terminal-p
  (setq keymap-timer-idle-timer
        (run-with-idle-timer 120 t 'keymap-command-mode-activate)))

(defun keymap-command-mode-activate-no-hook ()
  "Activate command mode. Does not run `keymap-command-mode-activate-hook'."
  (interactive)
  (keymap-command-mode-init))

(defun keymap-insert-mode-activate ()
  "Activate insertion mode."
  (interactive)
  (keymap-insert-mode-init)
  (run-hooks 'keymap-insert-mode-activate-hook))

(defun keymap-insert-mode-activate-ru ()
  "Activate insertion mode, set russian input method."
  (interactive)
  (keymap-insert-mode-init)
  (run-hooks 'keymap-insert-mode-activate-hook)
  (set-input-method 'russian-computer))

(defun keymap-insert-mode-activate-minibuffer ()
  "Activate insertion mode, unless ido active."
  (interactive)
  (unless (ido-active)
    (keymap-insert-mode-init)
    (run-hooks 'keymap-insert-mode-activate-hook)))



;;;###autoload
(define-minor-mode keymap
  "A modal keybinding set."
  :global t
  :lighter "Keymap"
  :keymap keymap-insert-map
  (delete-selection-mode 1)
  (setq shift-select-mode nil)

  (if keymap
      ;; Construction:
      (progn
        (add-hook 'minibuffer-setup-hook           'keymap-insert-mode-activate-minibuffer)
        (add-hook 'icomplete-minibuffer-setup-hook 'keymap-command-mode-activate)
        (add-hook 'minibuffer-exit-hook            'keymap-command-mode-activate)
        (add-hook 'isearch-mode-end-hook           'keymap-command-mode-activate)
        (when (and (keymapp keymap-map)
                   (not (memq keymap-map (list keymap-command-map keymap-insert-map))))
          (set-keymap-parent keymap-map keymap-shared-map)
          (setq keymap-shared-map keymap-map))
        (keymap-command-mode-activate))
    (progn
      ;; Teardown:
      (remove-hook 'minibuffer-setup-hook           'keymap-insert-mode-activate-minibuffer)
      (remove-hook 'icomplete-minibuffer-setup-hook 'keymap-command-mode-activate)
      (remove-hook 'minibuffer-exit-hook            'keymap-command-mode-activate)
      (remove-hook 'isearch-mode-end-hook           'keymap-command-mode-activate)
      (keymap-insert-mode-init :no-indication)
      (setq mode-line-front-space '(:eval (if (display-graphic-p) " " "-")))

      ;;
      )))

(provide 'keymap)



;; Local Variables:
;; byte-compile-docstring-max-column: 999
;; End:

;;; keymap.el ends here
