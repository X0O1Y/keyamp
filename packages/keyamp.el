;;; keyamp.el --- Keyboard «Amplifier» -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Egor Maltsev <x0o1@ya.ru>
;; Version: 1.0 2023-09-13

;; This package is part of input model.
;; Follow the link: https://github.com/xEgorka/keyamp

;;; Commentary:

;; Keyamp provides 3 modes based on transient keymaps: insert, command
;; and «ampable».

;; Command and insert modes are persistent transient keymaps.

;; Ampable mode pushes transient remaps to keymap stack on top of
;; command mode for easy repeat of commands chains during screen
;; positioning, cursor move and editing. Point color indicates
;; transient remap is active. ESDF and IJKL are mostly used, DEL/ESC
;; and RET/SPC control EVERYTHING. Home row and thumb cluster only.

;; DEL and SPC are two leader keys, RET activates insert mode, ESC for
;; command one. Holding down each of the keys posts control sequence
;; depending on mode. Keyboard has SYMMETRIC layout: left side for
;; editing, «No» and «Escape» while right side for moving, «Yes» and
;; «Enter». Any Emacs major or minor mode could be remaped to fit the
;; model, find examples in the package.

;; Karabiner integration allows to post control or leader sequences by
;; holding down a key. NO need to have any modifier or arrows keys at
;; ALL. Holding down posts leader layer. The same symmetric layout
;; might be configured on ANSI keyboard, ergonomic split and virtual
;; keyboards. See the link for layouts and karabiner config.

;;; Code:



(defgroup keyamp nil
  "Customization options for keyamp"
  :group 'help
  :prefix "keyamp-")

(defvar keyamp-command-mode-activate-hook nil "Hook for `keyamp-command-mode-activate'")
(defvar keyamp-insert-mode-activate-hook  nil "Hook for `keyamp-insert-mode-activate'")

(defconst keyamp-karabiner-cli
  "/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli"
  "Karabiner-Elements CLI executable")

(defconst keyamp-command-mode-indicator "🟢" "Character indicating command mode is active.")
(defconst keyamp-insert-mode-indicator  "🟠" "Character indicating insert mode  is active.")
(defconst keyamp-ampable-mode-indicator "🔵" "Character indicating ampable mode is active.")

(defconst keyamp-command-mode-cursor "lawngreen"
  "Cursor color indicating command mode is active.")
(defconst keyamp-insert-mode-cursor  "gold"
  "Cursor color indicating insert  mode is active.")
(defconst keyamp-ampable-mode-cursor "deepskyblue"
  "Cursor color indicating ampable mode is active.")

(defconst keyamp-idle-timeout 120 "Mode activation timeout.")


;; layout lookup tables for key conversion

(defvar keyamp-layouts nil "A alist. Key is layout name, string type.
Value is a alist, each element is of the form (\"e\" . \"d\").
First char is qwerty, second is corresponding char of the destination layout.
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
  "A alist that's the conversion table from qwerty to current layout.
Value structure is one of the key's value of `keyamp-layouts'.
Value is programtically set from value of `keyamp-current-layout'.
Do not manually set this variable.")

(setq keyamp--convert-table
      (cdr (assoc keyamp-current-layout keyamp-layouts)))

(defun keyamp--convert-kbd-str (Charstr)
  "Return the corresponding char Charstr according to
`keyamp--convert-table'. Charstr must be a string that is the argument
to `kbd'. E.g. \"a\" and \"a b c\". Each space separated token is
converted according to `keyamp--convert-table'."
  (interactive)
  (mapconcat
   'identity
   (mapcar
    (lambda (x)
      (let ((xresult (assoc x keyamp--convert-table)))
        (if xresult (cdr xresult) x)))
    (split-string Charstr " +"))
   " "))

(defmacro keyamp--define-keys (KeymapName KeyCmdAlist &optional Direct-p)
  "Map `define-key' over a alist KeyCmdAlist, with key layout remap.
The key is remapped from qwerty to the current keyboard layout by
`keyamp--convert-kbd-str'.
If Direct-p is t, do not remap key to current keyboard layout.

Example usage:
(keyamp--define-keys
 (define-prefix-command \\='xyz-map)
 \\='(
   (\"h\" . highlight-symbol-at-point)
   (\".\" . isearch-forward-symbol-at-point)
   (\"w\" . isearch-forward-word)))"
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName , KeymapName))
       ,@(mapcar
          (lambda (xpair)
            `(define-key
               ,xkeymapName
               (kbd (,(if Direct-p #'identity #'keyamp--convert-kbd-str) ,(car xpair)))
               ,(list 'quote (cdr xpair))))
          (cadr KeyCmdAlist)))))

(defmacro keyamp--define-keys-translation (KeyKeyAlist State-p)
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

(defmacro keyamp--define-keys-remap (KeymapName CmdCmdAlist)
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



(defconst keyamp-engineer-engram-to-russian-computer
  '(("a" . "а") ("b" . "й") ("c" . "ф") ("d" . "ш") ("e" . "в")
    ("f" . "ю") ("g" . "я") ("h" . "о") ("i" . "ы") ("j" . "с")
    ("k" . "м") ("l" . "г") ("m" . "б") ("n" . "ж") ("o" . "у")
    ("q" . "э") ("r" . "ь") ("s" . "д") ("t" . "л") ("u" . "к")
    ("v" . "з") ("w" . "щ") ("x" . "ч") ("y" . "ц") ("z" . "х")
    ("." . "р") ("?" . "т") ("-" . "и") ("," . "п") ("'" . "е")
    ("`" . "ё") ("{" . "ъ") ("\"" . "н"))
  "Mapping for `keyamp-define-input-source'")

(defun keyamp-quail-get-translation (from)
  "Get translation Engineer Engram to russian-computer.
From character to character code."
  (interactive)
  (let ((to (alist-get from keyamp-engineer-engram-to-russian-computer
             nil nil 'string-equal)))
    (when (stringp to)
      (string-to-char to))))

(defun keyamp-define-input-source (input-method)
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
               (from (if (string-equal keyamp-current-layout "engineer-engram")
                         (keyamp-quail-get-translation (char-to-string to))
                       (quail-get-translation (cadr map) (char-to-string to) 1))))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))



(defconst quail-keyboard-layout-engineer-engram
  "\
                              \
  7@5&1/3$9<8>2*0=4+6\\#|%^`~  \
  bByYoOuU'(\")lLdDwWvVzZ{[    \
  cCiIeEaA,;.:hHtTsSnNqQ}]    \
  gGxXjJkK-_?!rRmMfFpP        \
                              "
  "Engineer Engram keyboard layout for Quail, e.g. for input method.")

(require 'quail)
(push (cons "engineer-engram" quail-keyboard-layout-engineer-engram)
      quail-keyboard-layout-alist)

(defun keyamp-qwerty-to-engineer-engram ()
  "Toggle translate qwerty layout to engineer engram on Emacs level.
Useful when Engineer Engram layout not available on OS or keyboard level."
  (interactive)
  (if (get 'keyamp-qwerty-to-engineer-engram 'state)
      (progn
        (put 'keyamp-qwerty-to-engineer-engram 'state nil)
        (quail-set-keyboard-layout "standard")
        (message "Translation deactivated"))
    (progn
      (put 'keyamp-qwerty-to-engineer-engram 'state t)
      (quail-set-keyboard-layout "engineer-engram")
      (message "Translation activated")))
  (let ()
    (keyamp--define-keys-translation
     '(("-" . "#") ("=" . "%") ("`" . "`")    ("q" . "b") ("w" . "y") ("e" . "o")
       ("r" . "u") ("t" . "'") ("y" . "\"")   ("u" . "l") ("i" . "d") ("o" . "w")
       ("p" . "v") ("[" . "z") ("]" . "C-_")  ("a" . "c") ("s" . "i") ("d" . "e")
       ("f" . "a") ("g" . ",") ("h" . ".")    ("j" . "h") ("k" . "t") ("l" . "s")
       (";" . "n") ("'" . "q") ("\\" . "C-^") ("z" . "g") ("x" . "x") ("c" . "j")
       ("v" . "k") ("b" . "-") ("n" . "?")    ("m" . "r") ("," . "m") ("." . "f")
       ("/" . "p") ("_" . "|") ("+" . "^")    ("~" . "~") ("Q" . "B") ("W" . "Y")
       ("E" . "O") ("R" . "U") ("T" . "(")    ("Y" . ")") ("U" . "L") ("I" . "D")
       ("O" . "W") ("P" . "V") ("{" . "Z")    ("}" . "[") ("A" . "C") ("S" . "I")
       ("D" . "E") ("F" . "A") ("G" . ";")    ("H" . ":") ("J" . "H") ("K" . "T")
       ("L" . "S") (":" . "N") ("\"" . "Q")   ("|" . "]") ("Z" . "G") ("X" . "X")
       ("C" . "J") ("V" . "K") ("B" . "_")    ("N" . "!") ("M" . "R") ("<" . "M")
       (">" . "F") ("?" . "P") ("1" . "7")    ("2" . "5") ("3" . "1") ("4" . "3")
       ("5" . "9") ("6" . "8") ("7" . "2")    ("8" . "0") ("9" . "4") ("0" . "6")
       ("!" . "@") ("@" . "&") ("#" . "/")    ("$" . "$") ("%" . "<") ("^" . ">")
       ("&" . "*") ("*" . "=") ("(" . "+")    (")" . "\\"))
     (get 'keyamp-qwerty-to-engineer-engram 'state))))


;; keymaps

(defvar keyamp-shared-map (make-sparse-keymap)
  "Parent keymap of `keyamp-command-map' and `keyamp-insert-map'.
Define keys that are available in both command and insert modes here.")

(defvar keyamp-command-map (cons 'keymap keyamp-shared-map)
  "Keymap that takes precedence over all other keymaps in command mode.
Inherits bindings from `keyamp-shared-map'.

In command mode, if no binding is found in this map
`keyamp-shared-map' is checked, then if there is still no binding,
the other active keymaps are checked like normal. However, if a key is
explicitly bound to nil in this map, it will not be looked up in
`keyamp-shared-map' and lookup will skip directly to the normally
active maps.

In this way, bindings in `keyamp-shared-map' can be disabled by this map.
Effectively, this map takes precedence over all others when command mode
is enabled.")

(defvar keyamp-insert-map (cons 'keymap keyamp-shared-map)
  "Keymap for bindings that will be checked in insert mode. Active whenever
`keyamp' is non-nil.

Inherits bindings from `keyamp-shared-map'. In insert mode, if no binding
is found in this map `keyamp-shared-map' is checked, then if there is
still no binding, the other active keymaps are checked like normal. However,
if a key is explicitly bound to nil in this map, it will not be looked
up in `keyamp-shared-map' and lookup will skip directly to the normally
active maps. In this way, bindings in `keyamp-shared-map' can be disabled
by this map.

Keep in mind that this acts like a normal global minor mode map, so other
minor modes loaded later may override bindings in this map.")

(defvar keyamp--deactivate-command-mode-func nil)

(defvar keyamp-ampable-commands-hash nil
  "Hash table with commands which set various transient keymaps,
so-called “ampable” commands.")


;; setting keys

(defun keyamp-escape ()
  "Return to command mode. Escape everything."
  (interactive)
  (if (or (eq last-repeatable-command 'repeat)
          (gethash last-command keyamp-ampable-commands-hash))
      (message "")
    (progn
      (if (active-minibuffer-window)
          (abort-recursive-edit)
        (when (region-active-p) (deactivate-mark))))))

(progn
  (defconst keyamp-tty-seq-timeout 30
    "Timeout in ms to wait sequence after ESC sent in tty.")

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
        (define-key input-decode-map
          [?\e] `(menu-item "" ,esc-binding :filter keyamp-tty-ESC-filter)))))

  (define-key key-translation-map (kbd "ESC") (kbd "<escape>")))



(keyamp--define-keys
 keyamp-shared-map
 '(("<escape>"   . keyamp-command-mode-activate)
   ("C-<escape>" . keyamp-ampable-mode-indicate)
   ("C-^" . keyamp-leader-left-key-map)  ("C-+" . keyamp-leader-left-key-map)
   ("C-_" . keyamp-leader-right-key-map) ("C-И" . keyamp-leader-right-key-map)))

(keyamp--define-keys
 keyamp-command-map
 '(("<escape>" . keyamp-escape)                                                        ("S-<escape>"    . ignore)
   ("RET" . keyamp-insert-mode-activate) ("<return>"    . keyamp-insert-mode-activate) ("S-<return>"    . ignore)
   ("DEL" . keyamp-leader-left-key-map)  ("<backspace>" . keyamp-leader-left-key-map)  ("S-<backspace>" . ignore)
   ("SPC" . keyamp-leader-right-key-map)

   ;; left half
   ("`" . other-frame)                  ("ё" . other-frame)                ("~" . keyamp-qwerty-to-engineer-engram) ("Ë" . keyamp-qwerty-to-engineer-engram)
   ("1" . kmacro-play)                                                     ("!" . ignore)
   ("2" . kmacro-helper)                                                   ("@" . ignore)
   ("3" . kmacro-record)                                                   ("#" . ignore) ("№" . ignore)
   ("4" . append-to-register-1)                                            ("$" . ignore)
   ("5" . delete-forward-char)                                             ("%" . ignore)

   ("q" . insert-space-before)          ("й" . insert-space-before)        ("Q" . ignore) ("Й" . ignore)
   ("w" . backward-kill-word)           ("ц" . backward-kill-word)         ("W" . ignore) ("Ц" . ignore)
   ("e" . undo)                         ("у" . undo)                       ("E" . ignore) ("У" . ignore)
   ("r" . kill-word)                    ("к" . kill-word)                  ("R" . ignore) ("К" . ignore)
   ("t" . cut-text-block)               ("е" . cut-text-block)             ("T" . ignore) ("Е" . ignore)

   ("a" . shrink-whitespaces)           ("ф" . shrink-whitespaces)         ("A" . ignore) ("Ф" . ignore)
   ("s" . open-line)                    ("ы" . open-line)                  ("S" . ignore) ("Ы" . ignore)
   ("d" . cut-bracket-or-delete)        ("в" . cut-bracket-or-delete)      ("D" . ignore) ("В" . ignore)
   ("f" . newline)                      ("а" . newline)                    ("F" . ignore) ("А" . ignore)
   ("g" . set-mark-command)             ("п" . set-mark-command)           ("G" . ignore) ("П" . ignore)

   ("z" . toggle-comment)               ("я" . toggle-comment)             ("Z" . ignore) ("Я" . ignore)
   ("x" . cut-line-or-selection)        ("ч" . cut-line-or-selection)      ("X" . ignore) ("Ч" . ignore)
   ("c" . copy-line-or-selection)       ("с" . copy-line-or-selection)     ("C" . ignore) ("С" . ignore)
   ("v" . paste-or-paste-previous)      ("м" . paste-or-paste-previous)    ("V" . ignore) ("М" . ignore)
   ("b" . toggle-letter-case)           ("и" . toggle-letter-case)         ("B" . ignore) ("И" . ignore)

   ;; right half
   ("6" . pass)                                                            ("^" . ignore)
   ("7" . number-to-register)                                              ("&" . ignore)
   ("8" . copy-to-register)                                                ("*" . goto-matching-bracket) ; qwerty「*」→「=」engram, qwerty「/」→「=」ru pc karabiner
   ("9" . eperiodic)                                                       ("(" . ignore)
   ("0" . terminal)                                                        (")" . ignore)
   ("-" . works)                                                           ("_" . ignore)
   ("=" . post-task)                                                       ("+" . ignore)

   ("y"  . search-current-word)         ("н" . search-current-word)        ("Y" . ignore) ("Н" . ignore)
   ("u"  . backward-word)               ("г" . backward-word)              ("U" . ignore) ("Г" . ignore)
   ("i"  . previous-line)               ("ш" . previous-line)              ("I" . ignore) ("Ш" . ignore)
   ("o"  . forward-word)                ("щ" . forward-word)               ("O" . ignore) ("Щ" . ignore)
   ("p"  . exchange-point-and-mark)     ("з" . exchange-point-and-mark)    ("P" . ignore) ("З" . ignore)
   ("["  . alternate-buffer)            ("х" . alternate-buffer)           ("{" . ignore) ("Х" . ignore)
   ("]"  . keyamp-leader-left-key-map)  ("ъ" . keyamp-leader-left-key-map) ("}" . ignore) ("Ъ" . ignore)
   ("\\" . keyamp-leader-right-key-map)                                    ("|" . ignore)

   ("h" . beginning-of-line-or-block)   ("р" . beginning-of-line-or-block) ("H"  . ignore) ("Р" . ignore)
   ("j" . backward-char)                ("о" . backward-char)              ("J"  . ignore) ("О" . ignore)
   ("k" . next-line)                    ("л" . next-line)                  ("K"  . ignore) ("Л" . ignore)
   ("l" . forward-char)                 ("д" . forward-char)               ("L"  . ignore) ("Д" . ignore)
   (";" . end-of-line-or-block)         ("ж" . end-of-line-or-block)       (":"  . ignore) ("Ж" . ignore)
   ("'" . alternate-buffer)             ("э" . alternate-buffer)           ("\"" . ignore) ("Э" . ignore)

   ("n" . isearch-forward)              ("т" . isearch-forward)            ("N" . ignore) ("Т" . ignore)
   ("m" . backward-left-bracket)        ("ь" . backward-left-bracket)      ("M" . ignore) ("Ь" . ignore)
   ("," . next-window-or-frame)         ("б" . next-window-or-frame)       ("<" . ignore) ("Б" . ignore)
   ("." . forward-right-bracket)        ("ю" . forward-right-bracket)      (">" . ignore) ("Ю" . ignore)
   ("/" . goto-matching-bracket)                                           ("?" . ignore)

   ("<up>"    . up-line)
   ("<down>"  . down-line)
   ("<left>"  . left-char)
   ("<right>" . right-char)))

(keyamp--define-keys
 (define-prefix-command 'keyamp-leader-left-key-map)
 '(("SPC" . repeat)
   ("DEL" . repeat)                   ("<backspace>" . repeat)
   ("RET" . execute-extended-command) ("<return>"    . execute-extended-command)
   ("TAB" . toggle-ibuffer)           ("<tab>"       . toggle-ibuffer)
   ("ESC" . ignore)                   ("<escape>"    . ignore)

   ;; left leader left half
   ("`" . screenshot)
   ("1" . apply-macro-to-region-lines)
   ("2" . kmacro-name-last-macro)
   ("3" . ignore)
   ("4" . clear-register-1)
   ("5" . ignore)

   ("q" . reformat-lines)
   ("w" . ignore) ; C-c C-c
   ("e" . split-window-below)
   ("r" . query-replace)
   ("t" . kill-line)

   ("a" . delete-window)
   ("s" . previous-user-buffer)
   ("d" . delete-other-windows)
   ("f" . next-user-buffer)
   ("g" . rectangle-mark-mode)

   ("z" . universal-argument) ; C-u
   ("x" . save-buffers-kill-terminal)
   ("c" . copy-to-register-1)
   ("v" . paste-from-register-1)
   ("b" . toggle-previous-letter-case)

   ;; left leader right half
   ("6" . ignore)
   ("7" . jump-to-register)
   ("8" . ignore)
   ("9" . ignore)
   ("0" . ignore)
   ("-" . ignore)
   ("=" . ignore)

   ("y" . find-name-dired)
   ("u" . switch-to-buffer)

   ("i e" . open-recently-closed)          ("i i" . copy-file-path)
   ("i d" . show-in-desktop)               ("i k" . write-file)
   ("i f" . find-file)
   ("i t" . list-recently-closed)
   ("i b" . set-buffer-file-coding-system) ("i n" . revert-buffer-with-coding-system)
                                           ("i p" . bookmark-set)
   ("i r" . rename-visited-file)           ("i u" . open-in-terminal)

   ("o"  . bookmark-jump)
   ("p"  . view-echo-area-messages)
   ("["  . ignore)
   ("]"  . ignore)
   ("\\" . ignore)

   ("h" . recentf-open-files)

   ("j e" . man)                           ("j i" . info)
   ("j s" . describe-syntax)               ("j l" . describe-variable)
   ("j d" . apropos-documentation)         ("j k" . describe-key)
   ("j f" . describe-face)                 ("j j" . describe-function)
   ("j g" . info-lookup-symbol)
   ("j c" . describe-char)                 ("j m" . describe-mode)
   ("j a" . apropos-command)               ("j ;" . describe-bindings)
   ("j r" . apropos-variable)              ("j u" . elisp-index-search)
   ("j v" . apropos-value)                 ("j n" . view-lossage)
   ("j w" . describe-coding-system)        ("j o" . describe-language-environment)

   ("k e" . json-pretty-print-buffer)      ("k i" . move-to-column)
   ("k s" . space-to-newline)              ("k l" . make-backup-and-save)
   ("k d" . list-matching-lines)           ("k k" . ispell-word)
   ("k f" . delete-matching-lines)         ("k j" . repeat-complex-command)
   ("k g" . delete-non-matching-lines)     ("k h" . reformat-to-sentence-lines)
   ("k r" . quote-lines)                   ("k u" . escape-quotes)
   ("k t" . delete-duplicate-lines)        ("k y" . slash-to-double-backslash)
   ("k v" . change-bracket-pairs)          ("k n" . double-backslash-to-slash)
   ("k w" . sort-lines-key-value)          ("k o" . slash-to-backslash)
   ("k x" . insert-column-a-z)             ("k ." . sort-lines-block-or-region)
   ("k c" . cycle-hyphen-lowline-space)    ("k ," . sort-numeric-fields)

   ("l e" . global-hl-line-mode)           ("l i" . widen)
   ("l s" . display-line-numbers-mode)     ("l l" . read-only-mode)
   ("l d" . whitespace-mode)               ("l k" . narrow-to-defun)
   ("l f" . toggle-case-fold-search)       ("l j" . narrow-to-region-or-block)
   ("l g" . toggle-word-wrap)              ("l h" . narrow-to-page)
   ("l a" . text-scale-adjust)             ("l ;" . count-matches)
   ("l t" . toggle-truncate-lines)         ("l y" . visual-line-mode)
   ("l c" . flyspell-buffer)               ("l ," . eww)
   ("l w" . abbrev-mode)                   ("l o" . count-words)
   ("l q" . glyphless-display-mode)

   (";" . bookmark-bmenu-list)
   ("'" . toggle-debug-on-error)

   ("n" . describe-foo-at-point)
   ("m" . downloads)
   ("," . open-last-closed)
   ("." . player)
   ("/" . goto-line)

   ("i <escape>" . ignore) ("i ESC" . ignore)
   ("j <escape>" . ignore) ("j ESC" . ignore)
   ("k <escape>" . ignore) ("k ESC" . ignore)
   ("l <escape>" . ignore) ("l ESC" . ignore)))

(keyamp--define-keys
 (define-prefix-command 'keyamp-leader-right-key-map)
 '(("SPC" . repeat)
   ("DEL" . repeat)                   ("<backspace>" . repeat)
   ("RET" . execute-extended-command) ("<return>"    . execute-extended-command)
   ("TAB" . toggle-ibuffer)           ("<tab>"       . toggle-ibuffer)
   ("ESC" . ignore)                   ("<escape>"    . ignore)

   ;; right leader left half
   ("`" . toggle-frame-maximized)
   ("1" . ignore)
   ("2" . insert-kbd-macro)
   ("3" . ignore)
   ("4" . ignore)
   ("5" . ignore)

   ("q" . fill-or-unfill)
   ("w" . ignore)

   ("e e" . todo)
                                         ("e l" . weather)
   ("e d" . calendar)                    ("e k" . pass)
   ("e f" . org-time-stamp)              ("e j" . clock)
   ("e r" . shopping)                    ("e u" . calculator)

   ("r" . query-replace-regexp)
   ("t" . toggle-eshell)

   ("a" . mark-whole-buffer)

   ("s s" . clean-whitespace)
   ("s e" . move-row-up)
   ("s d" . move-row-down)

   ("d e" . org-shiftup)                 ("d i" . eval-defun)
   ("d s" . shell-command-on-region)     ("d l" . delete-frame)
   ("d d" . insert-date)                 ("d k" . run-current-file)
   ("d f" . shell-command)               ("d j" . eval-last-sexp)
   ("d r" . async-shell-command)         ("d u" . elisp-eval-region-or-buffer)
   ("d t" . proced)
                                         ("d n" . stow)
                                         ("d ;" . undelete-frame)
                                         ("d ," . elisp-byte-compile-file)

                                         ("f i" . insert-ascii-single-quote)
                                         ("f l" . insert-square-bracket)
   ("f d" . emoji-insert)                ("f k" . insert-paren)
   ("f f" . insert-char)                 ("f j" . insert-brace)
   ("f a" . insert-double-angle-bracket) ("f ;" . insert-string-assignment)
                                         ("f ," . insert-angle-bracket)
   ("f g" . insert-curly-single-quote)   ("f h" . insert-double-curly-quote)
   ("f r" . insert-single-angle-quote)   ("f u" . insert-ascii-double-quote)
   ("f t" . insert-double-angle-quote)   ("f y" . insert-emacs-quote)
   ("f v" . insert-markdown-quote)       ("f m" . insert-corner-bracket)
                                         ("f n" . insert-black-lenticular-bracket)
                                         ("f ." . insert-markdown-triple-quote)
                                         ("f o" . insert-tortoise-shell-bracket)
                                         ("f p" . insert-formfeed)

   ("g" . new-empty-buffer)

   ("z" . goto-char)
   ("x" . cut-all)
   ("c" . copy-all)
   ("v" . tasks)
   ("b" . title-case-region-or-line)

   ;; right leader right half
   ("6" . ignore)
   ("7" . increment-register)
   ("8" . insert-register)
   ("9" . ignore)
   ("0" . ignore)
   ("-" . ignore)
   ("=" . ignore)

   ("y"  . find-text)
   ("u"  . pop-local-mark-ring)
   ("i"  . select-block)
   ("o"  . set-mark-deactivate-mark)
   ("p"  . show-kill-ring)
   ("["  . ignore)
   ("]"  . ignore)
   ("\\" . ignore)

   ("h" . scroll-down-command)
   ("j" . select-bracket)
   ("k" . extend-selection)
   ("l" . select-text-in-quote)
   (";" . scroll-up-command)
   ("'" . sync)

   ("n" . save-buffer)
   ("m" . dired-jump)
   ("," . save-close-current-buffer)
   ("." . select-line)
   ("/" . recenter-top-bottom) ("*" . recenter-top-bottom)

   ("e <escape>" . ignore) ("e ESC" . ignore)
   ("s <escape>" . ignore) ("s ESC" . ignore)
   ("d <escape>" . ignore) ("d ESC" . ignore)
   ("f <escape>" . ignore) ("f ESC" . ignore)))


;; DEL ESC RET SPC

(keyamp--define-keys query-replace-map '(("C-h" . skip) ("C-r" . act)))
(keyamp--define-keys global-map '(("C-r" . open-file-at-cursor) ("C-t" . hippie-expand)))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . repeat)))
  (keyamp--define-keys x '(("DEL" . repeat) ("<backspace>" . repeat) ("SPC" . repeat)))
  (advice-add 'repeat :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys
   x
   '(("RET" . hippie-expand)      ("<return>"    . hippie-expand)
     ("DEL" . hippie-expand-undo) ("<backspace>" . hippie-expand-undo)))
  (advice-add 'hippie-expand :after (lambda (&rest r) "Repeat." (set-transient-map x))))


;; screen

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap
   x
   '((insert-space-before         . sun-moon)                ; q
     (undo                        . split-window-below)      ; e
     (kill-word                   . make-frame-command)      ; r
     (cut-text-block              . toggle-eshell)           ; t
     (open-line                   . previous-user-buffer)    ; s
     (cut-bracket-or-delete       . delete-other-windows)    ; d
     (newline                     . next-user-buffer)        ; f
     (set-mark-command            . new-empty-buffer)        ; g
     (cut-line-or-selection       . works)                   ; x
     (copy-line-or-selection      . agenda)                  ; c
     (paste-or-paste-previous     . tasks)                   ; v
     (exchange-point-and-mark     . view-echo-area-messages) ; p
     (backward-left-bracket       . dired-jump)              ; m
     (forward-right-bracket       . player)))                ; .
  (keyamp--define-keys
   x
   '(("TAB" . toggle-ibuffer)       ("<tab>"       . toggle-ibuffer)
     ("DEL" . previous-user-buffer) ("<backspace>" . previous-user-buffer)
     ("SPC" . next-user-buffer)))

  (advice-add 'delete-other-windows      :after (lambda (&rest r) "Screen positioning." (set-transient-map x)))
  (advice-add 'next-user-buffer          :after (lambda (&rest r) "Screen positioning." (set-transient-map x)))
  (advice-add 'previous-user-buffer      :after (lambda (&rest r) "Screen positioning." (set-transient-map x)))
  (advice-add 'save-close-current-buffer :after (lambda (&rest r) "Screen positioning." (set-transient-map x)))
  (advice-add 'split-window-below        :after (lambda (&rest r) "Screen positioning." (set-transient-map x)))
  (add-hook 'help-mode-hook (lambda () "Screen positioning." (set-transient-map x) (setq this-command 'split-window-below)))
  (add-hook 'ibuffer-hook   (lambda () "Screen positioning." (set-transient-map x) (setq this-command 'ibuffer-forward-filter-group)))
  (advice-add 'ibuffer-forward-filter-group  :after (lambda (&rest r) "Screen positioning." (set-transient-map x)))
  (advice-add 'ibuffer-backward-filter-group :after (lambda (&rest r) "Screen positioning." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((backward-left-bracket . dired-jump)))
  (keyamp--define-keys x '(("DEL" . dired-jump) ("<backspace>" . dired-jump) ("SPC" . dired-jump)))
  (advice-add 'dired-jump :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'downloads  :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'player     :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((next-window-or-frame . save-close-current-buffer)))
  (advice-add 'save-close-current-buffer :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . tasks)))
  (keyamp--define-keys x '(("DEL" . previous-user-buffer) ("<backspace>" . previous-user-buffer) ("SPC" . tasks)))
  (advice-add 'tasks :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . works)))
  (keyamp--define-keys x '(("DEL" . previous-user-buffer) ("<backspace>" . previous-user-buffer) ("SPC" . works)))
  (advice-add 'works :after (lambda (&rest r) "Repeat." (set-transient-map x))))


;; edit

(let ((x (make-sparse-keymap))) ; q d
  (keyamp--define-keys x '(("DEL" . cut-bracket-or-delete) ("<backspace>" . cut-bracket-or-delete) ("SPC" . insert-space-before)))
  (advice-add 'insert-space-before   :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'cut-bracket-or-delete :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys x '(("DEL" . delete-forward-char) ("<backspace>" . delete-forward-char) ("SPC" . delete-forward-char)))
  (advice-add 'delete-forward-char :after (lambda (&rest r) "Repeat." (unless keyamp-insert-state-p (set-transient-map x)))))

(let ((x (make-sparse-keymap))) ; w r
  (keyamp--define-keys-remap x '((open-line . backward-kill-word) (newline . kill-word)))
  (keyamp--define-keys x '(("DEL" . open-line) ("<backspace>" . open-line) ("SPC" . newline)))
  (advice-add 'backward-kill-word :after (lambda (&rest r) "Repeat." (set-transient-map x) (setq this-command 'kill-region)))
  (advice-add 'kill-word :after (lambda (&rest r) "Repeat." (set-transient-map x) (setq this-command 'kill-region))))

(let ((x (make-sparse-keymap))) ; e d
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . undo-redo)))
  (keyamp--define-keys x '(("DEL" . undo) ("<backspace>" . undo) ("SPC" . undo-redo)))
  (advice-add 'undo      :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'undo-redo :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap))) ; t
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . cut-text-block)))
  (advice-add 'cut-text-block :after (lambda (&rest r) "Repeat." (set-transient-map x) (setq this-command 'kill-region))))

(let ((x (make-sparse-keymap))) ; a
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . shrink-whitespaces)))
  (keyamp--define-keys x '(("DEL" . shrink-whitespaces) ("<backspace>" . shrink-whitespaces) ("SPC" . shrink-whitespaces)))
  (advice-add 'shrink-whitespaces :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap))) ; g
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . rectangle-mark-mode)))
  (advice-add 'set-mark-command :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap))) ; z
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . toggle-comment)))
  (keyamp--define-keys x '(("DEL" . toggle-comment) ("<backspace>" . toggle-comment) ("SPC" . toggle-comment)))
  (advice-add 'toggle-comment :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap))) ; x
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . cut-line-or-selection)))
  (keyamp--define-keys x '(("DEL" . cut-line-or-selection) ("<backspace>" . cut-line-or-selection) ("SPC" . cut-line-or-selection)))
  (advice-add 'cut-line-or-selection :after (lambda (&rest r) "Repeat." (set-transient-map x) (setq this-command 'kill-region))))

(let ((x (make-sparse-keymap))) ; c
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . copy-line-or-selection)))
  (keyamp--define-keys x '(("DEL" . copy-line-or-selection) ("<backspace>" . copy-line-or-selection) ("SPC" . copy-line-or-selection)))
  (advice-add 'copy-line-or-selection :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap))) ; v
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . paste-or-paste-previous)))
  (keyamp--define-keys x '(("DEL" . undo) ("<backspace>" . undo) ("SPC" . paste-or-paste-previous)))
  (advice-add 'paste-or-paste-previous :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap))) ; b
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . toggle-letter-case)))
  (keyamp--define-keys x '(("DEL" . toggle-letter-case) ("<backspace>" . toggle-letter-case) ("SPC" . toggle-letter-case)))
  (advice-add 'toggle-letter-case :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((undo . move-row-up) (cut-bracket-or-delete . move-row-down)))
  (keyamp--define-keys x '(("DEL" . move-row-up) ("<backspace>" . move-row-up) ("SPC" . move-row-down)))
  (advice-add 'move-row-up   :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'move-row-down :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((undo . org-shiftup) (cut-bracket-or-delete  . org-shiftdown) (copy-line-or-selection . agenda)))
  (advice-add 'org-shiftup   :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'org-shiftdown :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((undo . todo) (copy-line-or-selection . agenda)))
  (advice-add 'todo        :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'insert-date :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((cut-bracket-or-delete . cycle-hyphen-lowline-space)))
  (advice-add 'cycle-hyphen-lowline-space :after (lambda (&rest r) "Repeat." (set-transient-map x))))


;; move

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys x '(("DEL" . previous-line) ("<backspace>" . previous-line) ("SPC" . next-line)))
  (advice-add 'previous-line :after (lambda (&rest r) "Repeat." (unless keyamp-insert-state-p (set-transient-map x))))
  (advice-add 'next-line     :after (lambda (&rest r) "Repeat." (unless keyamp-insert-state-p (set-transient-map x)))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys x '(("DEL" . previous-line-cycle) ("<backspace>" . previous-line-cycle) ("SPC" . next-line-cycle)))
  (advice-add 'previous-line-cycle :after (lambda (&rest r) "Repeat." (unless keyamp-insert-state-p (set-transient-map x))))
  (advice-add 'next-line-cycle     :after (lambda (&rest r) "Repeat." (unless keyamp-insert-state-p (set-transient-map x)))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys x '(("DEL" . up-line) ("<backspace>" . up-line) ("SPC" . down-line)))
  (advice-add 'up-line   :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'down-line :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap
   x
   '((previous-line              . beginning-of-line-or-block)
     (next-line                  . end-of-line-or-block)
     (beginning-of-line-or-block . beginning-of-line-or-buffer)
     (end-of-line-or-block       . end-of-line-or-buffer)))
  (keyamp--define-keys x '(("DEL" . previous-line) ("<backspace>" . previous-line) ("SPC" . next-line)))
  (advice-add 'beginning-of-line-or-block  :after (lambda (&rest r) "Cursor positioning." (set-transient-map x)))
  (advice-add 'end-of-line-or-block        :after (lambda (&rest r) "Cursor positioning." (set-transient-map x)))
  (advice-add 'beginning-of-line-or-buffer :after (lambda (&rest r) "Cursor positioning." (set-transient-map x)))
  (advice-add 'end-of-line-or-buffer       :after (lambda (&rest r) "Cursor positioning." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap
   x
   '((backward-char . backward-word)  (forward-char  . forward-word)
     (backward-word . backward-punct) (forward-word  . forward-punct)))
  (keyamp--define-keys x '(("DEL" . backward-word) ("<backspace>" . backward-word) ("SPC" . forward-word)))
  (advice-add 'backward-word           :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'forward-word            :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'backward-punct          :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'forward-punct           :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'set-mark-command        :after (lambda (&rest r) "Cursor positioning." (set-transient-map x)))
  (advice-add 'exchange-point-and-mark :after (lambda (&rest r) "Cursor positioning." (set-transient-map x)))
  (advice-add 'rectangle-mark-mode     :after (lambda (&rest r) "Cursor positioning." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((previous-line . backward-left-bracket) (next-line . forward-right-bracket)))
  (keyamp--define-keys x '(("DEL" . backward-left-bracket) ("<backspace>" . backward-left-bracket) ("SPC" . forward-right-bracket)))
  (advice-add 'backward-left-bracket :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'forward-right-bracket :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'goto-matching-bracket :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((previous-line . scroll-down-line) (next-line . scroll-up-line)))
  (keyamp--define-keys x '(("DEL" . scroll-down-line) ("<backspace>" . scroll-down-line) ("SPC" . scroll-up-line)))
  (advice-add 'scroll-down-line :before (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'scroll-up-line   :before (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((previous-line . scroll-down-command) (next-line . scroll-up-command)))
  (keyamp--define-keys x '(("DEL" . scroll-down-command) ("<backspace>" . scroll-down-command) ("SPC" . scroll-up-command)))
  (advice-add 'scroll-down-command :before (lambda (&rest r) "Repeat." (set-transient-map x) (recenter))) ; advice after not working
  (advice-add 'scroll-up-command   :before (lambda (&rest r) "Repeat." (set-transient-map x) (recenter))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((next-line . pop-local-mark-ring)))
  (advice-add 'pop-local-mark-ring :after (lambda (&rest r) "Repeat." (set-transient-map x) (recenter))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((next-line . recenter-top-bottom)))
  (advice-add 'recenter-top-bottom :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((previous-line . beginning-of-line-or-block) (next-line . select-block)))
  (keyamp--define-keys x '(("DEL" . previous-line) ("<backspace>" . previous-line) ("SPC" . next-line)))
  (advice-add 'select-block :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap
   x
   '((next-line     . extend-selection)
     (backward-char . backward-word)
     (forward-char  . forward-word)))
  (advice-add 'extend-selection :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((next-line . select-line)))
  (keyamp--define-keys x '(("DEL" . previous-line) ("<backspace>" . previous-line) ("SPC" . next-line)))
  (advice-add 'select-line :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys-remap x '((next-line . select-text-in-quote)))
  (advice-add 'select-text-in-quote :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(keyamp--define-keys
 isearch-mode-map
 '(("<escape>" . isearch-abort)           ("C-h"   . isearch-repeat-backward) ("C-r"   . isearch-repeat-forward)
   ("<up>"     . isearch-ring-retreat)    ("C-_ i" . isearch-ring-retreat)    ("C-И i" . isearch-ring-retreat)
   ("<left>"   . isearch-repeat-backward) ("C-_ j" . isearch-repeat-backward) ("C-И j" . isearch-repeat-backward)
   ("<down>"   . isearch-ring-advance)    ("C-_ k" . isearch-ring-advance)    ("C-И k" . isearch-ring-advance)
   ("<right>"  . isearch-repeat-forward)  ("C-_ l" . isearch-repeat-forward)  ("C-И l" . isearch-repeat-forward)
   ("C-^"      . ignore)                  ("C-_ n" . isearch-yank-kill)       ("C-И n" . isearch-yank-kill)))

(let ((x (make-sparse-keymap)))
  (keyamp--define-keys
   x
   '(("i"   . isearch-ring-retreat)    ("ш"   . isearch-ring-retreat)
     ("j"   . isearch-repeat-backward) ("о"   . isearch-repeat-backward)
     ("k"   . isearch-ring-advance)    ("л"   . isearch-ring-advance)
     ("l"   . isearch-repeat-forward)  ("д"   . isearch-repeat-forward)
     ("d"   . repeat)                  ("в"   . repeat)
     ("DEL" . isearch-repeat-backward) ("SPC" . isearch-repeat-forward)))
  (advice-add 'isearch-ring-retreat    :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'isearch-repeat-backward :after (lambda (&rest r) "Repeat." (set-transient-map x) (recenter)))
  (advice-add 'isearch-ring-advance    :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'isearch-repeat-forward  :after (lambda (&rest r) "Repeat." (set-transient-map x) (recenter)))
  (advice-add 'search-current-word     :after (lambda (&rest r) "Repeat." (set-transient-map x)))
  (advice-add 'isearch-yank-kill       :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(with-eval-after-load 'help
  (keyamp--define-keys
   help-map
   '(("i" . lookup-word-definition)  ("u" . lookup-all-synonyms)
     ("j" . lookup-web)              ("o" . lookup-all-dictionaries)
     ("k" . lookup-google-translate) ("m" . lookup-word-dict-org)
     ("l" . lookup-wikipedia)        ("," . lookup-etymology) ("." . lookup-wiktionary)
     ("ESC"  . ignore) ("<escape>" . ignore) ("<f1>" . ignore) ("<help>" . ignore) ("C-w" . ignore) ("C-c" . ignore) ("C-o" . ignore) ("C-\\" . ignore)
     ("C-n"  . ignore) ("C-f"      . ignore) ("C-s"  . ignore) ("C-e"    . ignore) ("RET" . ignore) ("'"   . ignore) ("6"   . ignore) ("?"    . ignore)
     ("9"    . ignore) ("L"        . ignore) ("n"    . ignore) ("d"      . ignore) ("z"   . ignore) ("p"   . ignore))))


;; modes

(with-eval-after-load 'minibuffer
  (keyamp--define-keys-remap
   minibuffer-local-map
   '((previous-line . previous-line-or-history-element)
     (next-line     . next-line-or-history-element)
     (select-block  . previous-line-or-history-element)))
  (keyamp--define-keys-remap
   minibuffer-mode-map
   '((previous-line       . previous-line-or-history-element)
     (next-line           . next-line-or-history-element)
     (open-file-at-cursor . exit-minibuffer)
     (select-block        . previous-line-or-history-element))))

(with-eval-after-load 'icomplete
  (keyamp--define-keys
   icomplete-minibuffer-map
   '(("C-r" . icomplete-force-complete-and-exit)
     ("RET" . icomplete-exit-or-force-complete-and-exit) ("<return>" . icomplete-exit-or-force-complete-and-exit)))

  (keyamp--define-keys-remap
   icomplete-minibuffer-map
   '((previous-line    . icomplete-backward-completions)
     (next-line        . icomplete-forward-completions)
     (select-block     . previous-line-or-history-element)
     (extend-selection . next-line-or-history-element)))

  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys-remap
     x
     '((previous-line               . previous-line-or-history-element)
       (next-line                   . next-line-or-history-element)
       (keyamp-insert-mode-activate . exit-minibuffer)))
    (keyamp--define-keys x '(("DEL" . previous-line) ("<backspace>" . previous-line) ("SPC" . next-line)))
    (advice-add 'previous-line-or-history-element :after (lambda (&rest r) "History search." (set-transient-map x)))
    (advice-add 'next-line-or-history-element     :after (lambda (&rest r) "History search." (set-transient-map x)))))

  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys-remap
     x
     '((previous-line . icomplete-backward-completions)
       (next-line     . icomplete-forward-completions)))
    (keyamp--define-keys x '(("DEL" . previous-line) ("<backspace>" . previous-line) ("SPC" . next-line)))
    (advice-add 'icomplete-backward-completions :after (lambda (&rest r) "Repeat." (set-transient-map x)))
    (advice-add 'icomplete-forward-completions  :after (lambda (&rest r) "Repeat." (set-transient-map x))))

(add-hook 'ido-setup-hook
          (lambda ()
            (keyamp--define-keys ido-completion-map '(("C-r" . ido-exit-minibuffer)))
            (keyamp--define-keys-remap
             ido-completion-map
             '((previous-line . ido-prev-match)
               (next-line     . ido-next-match)))
            (let ((x (make-sparse-keymap)))
              (keyamp--define-keys
               x
               '(("DEL" . previous-line) ("<backspace>" . previous-line) ("SPC" . next-line)))
              (advice-add 'ido-prev-match :after (lambda (&rest r) "Repeat." (set-transient-map x)))
              (advice-add 'ido-next-match :after (lambda (&rest r) "Repeat." (set-transient-map x))))))

(progn ; dired
  (with-eval-after-load 'dired
    (keyamp--define-keys dired-mode-map '(("C-h" . dired-do-delete) ("C-r" . open-in-external-app)))
    (keyamp--define-keys-remap
     dired-mode-map
     '((keyamp-insert-mode-activate . dired-find-file)
       (backward-left-bracket       . dired-mark)
       (forward-right-bracket       . dired-unmark)
       (toggle-comment              . revert-buffer)
       (copy-to-register-1          . dired-do-copy)
       (paste-from-register-1       . dired-do-rename)
       (mark-whole-buffer           . dired-toggle-marks)
       (reformat-lines              . dired-create-directory)
       (insert-space-before         . dired-hide-details-mode)))

    (let ((x (make-sparse-keymap)))
      (keyamp--define-keys x '(("DEL" . dired-previous-line) ("<backspace>" . dired-previous-line) ("SPC" . dired-next-line)))
      (advice-add 'dired-previous-line :after (lambda (&rest r) "Repeat." (set-transient-map x)))
      (advice-add 'dired-next-line     :after (lambda (&rest r) "Repeat." (set-transient-map x))))

    (let ((x (make-sparse-keymap)))
      (keyamp--define-keys x '(("DEL" . dired-unmark) ("<backspace>" . dired-unmark) ("SPC" . dired-mark)))
      (advice-add 'dired-unmark :after (lambda (&rest r) "Repeat." (set-transient-map x)))
      (advice-add 'dired-mark   :after (lambda (&rest r) "Repeat." (set-transient-map x)))))

  (with-eval-after-load 'wdired
    (keyamp--define-keys wdired-mode-map '(("C-h" . wdired-abort-changes) ("C-r" . wdired-finish-edit)))))

(with-eval-after-load 'rect
  (keyamp--define-keys-remap
   rectangle-mark-mode-map
   '((copy-line-or-selection      . copy-rectangle-as-kill)
     (cut-bracket-or-delete       . kill-rectangle)
     (keyamp-insert-mode-activate . string-rectangle)
     (paste-or-paste-previous     . yank-rectangle)
     (copy-to-register            . copy-rectangle-to-register)
     (toggle-comment              . rectangle-number-lines)
     (cut-line-or-selection       . clear-rectangle)
     (insert-space-before         . open-rectangle)
     (clean-whitespace            . delete-whitespace-rectangle))))

(progn ; ibuffer
  (with-eval-after-load 'ibuf-ext
    (keyamp--define-keys
     ibuffer-mode-map
     '(("C-h" . ibuffer-do-delete) ("C-r" . ibuffer-diff-with-file)
       ("TAB" . news)              ("<tab>" . news)))

    (keyamp--define-keys-remap
     ibuffer-mode-map
     '((keyamp-insert-mode-activate . ibuffer-visit-buffer)
       (end-of-line-or-block        . ibuffer-forward-filter-group)
       (beginning-of-line-or-block  . ibuffer-backward-filter-group)
       (previous-line               . previous-line-cycle)
       (next-line                   . next-line-cycle)))
    (keyamp--define-keys-remap ibuffer-mode-filter-group-map '((keyamp-insert-mode-activate . ibuffer-toggle-filter-group)))

    (let ((x (make-sparse-keymap)))
      (keyamp--define-keys-remap
       x
       '((previous-line              . ibuffer-backward-filter-group)
         (next-line                  . ibuffer-forward-filter-group)
         (beginning-of-line-or-block . beginning-of-line-or-buffer)
         (end-of-line-or-block       . end-of-line-or-buffer)))
      (advice-add 'ibuffer-backward-filter-group :after (lambda (&rest r) "Cursor positioning." (set-transient-map x)))
      (advice-add 'ibuffer-forward-filter-group  :after (lambda (&rest r) "Cursor positioning." (set-transient-map x)))
      (advice-add 'ibuffer-toggle-filter-group   :after (lambda (&rest r) "Cursor positioning." (set-transient-map x)))))

  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys-remap x '((cut-bracket-or-delete . ibuffer-do-delete)))
    (keyamp--define-keys x       '(("DEL" . ibuffer-do-delete) ("<backspace>" . ibuffer-do-delete) ("SPC" . ibuffer-do-delete)))
    (advice-add 'ibuffer-do-delete :after (lambda (&rest r) "Repeat." (set-transient-map x)))))

(with-eval-after-load 'transient
  (keyamp--define-keys transient-base-map '(("<escape>" . transient-quit-one))))

(progn ; remap RET
  (with-eval-after-load 'arc-mode (keyamp--define-keys-remap archive-mode-map '((keyamp-insert-mode-activate . archive-extract))))
  (with-eval-after-load 'bookmark (keyamp--define-keys-remap bookmark-bmenu-mode-map '((keyamp-insert-mode-activate . bookmark-bmenu-this-window))))
  (with-eval-after-load 'button (keyamp--define-keys-remap button-map '((keyamp-insert-mode-activate . push-button))))
  (with-eval-after-load 'compile (keyamp--define-keys-remap compilation-button-map '((keyamp-insert-mode-activate . compile-goto-error))))
  (with-eval-after-load 'gnus-art (keyamp--define-keys-remap gnus-mime-button-map '((keyamp-insert-mode-activate . gnus-article-press-button))))
  (with-eval-after-load 'emms-playlist-mode (keyamp--define-keys-remap emms-playlist-mode-map '((keyamp-insert-mode-activate . emms-playlist-mode-play-smart))))
  (with-eval-after-load 'org-agenda (keyamp--define-keys-remap org-agenda-mode-map '((keyamp-insert-mode-activate . org-agenda-switch-to))))
  (with-eval-after-load 'replace (keyamp--define-keys-remap occur-mode-map '((keyamp-insert-mode-activate . occur-mode-goto-occurrence))))
  (with-eval-after-load 'shr (keyamp--define-keys-remap shr-map '((keyamp-insert-mode-activate . shr-browse-url))))
  (with-eval-after-load 'simple (keyamp--define-keys-remap completion-list-mode-map '((keyamp-insert-mode-activate . choose-completion))))
  (with-eval-after-load 'wid-edit (keyamp--define-keys-remap widget-link-keymap '((keyamp-insert-mode-activate . widget-button-press)))))

(with-eval-after-load 'doc-view
  (keyamp--define-keys-remap
   doc-view-mode-map
   '((previous-line              . doc-view-previous-line-or-previous-page)
     (next-line                  . doc-view-next-line-or-next-page)
     (backward-char              . doc-view-previous-page)
     (forward-char               . doc-view-next-page)
     (backward-word              . doc-view-shrink)
     (forward-word               . doc-view-enlarge)
     (beginning-of-line-or-block . doc-view-scroll-down-or-previous-page)
     (end-of-line-or-block       . doc-view-scroll-up-or-next-page)))

  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys-remap
     x
     '((previous-line . doc-view-scroll-down-or-previous-page)
       (next-line     . doc-view-scroll-up-or-next-page)))
    (keyamp--define-keys x '(("DEL" . doc-view-scroll-down-or-previous-page) ("<backspace>" . doc-view-scroll-down-or-previous-page) ("SPC" . doc-view-scroll-up-or-next-page)))
    (advice-add 'doc-view-scroll-down-or-previous-page :after (lambda (&rest r) "Repeat." (set-transient-map x)))
    (advice-add 'doc-view-scroll-up-or-next-page       :after (lambda (&rest r) "Repeat." (set-transient-map x)))))

(with-eval-after-load 'image-mode
  (keyamp--define-keys-remap image-mode-map '((backward-char . image-previous-file) (forward-char . image-next-file)))
  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys x '(("DEL" . backward-char) ("<backspace>" . backward-char) ("SPC" . forward-char)))
    (advice-add 'image-previous-file :after (lambda (&rest r) "Repeat." (set-transient-map x)))
    (advice-add 'image-next-file     :after (lambda (&rest r) "Repeat." (set-transient-map x)))))

(with-eval-after-load 'esh-mode
  (keyamp--define-keys eshell-mode-map '(("C-h" . eshell-interrupt-process) ("C-r" . eshell-send-input)))
  (keyamp--define-keys-remap
   eshell-mode-map
   '((cut-line-or-selection . eshell-clear-input)
     (cut-all               . eshell-clear)
     (select-block          . eshell-previous-input)))
  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys-remap
     x
     '((previous-line . eshell-previous-input)
       (next-line     . eshell-next-input)))
    (keyamp--define-keys x '(("DEL" . previous-line) ("<backspace>" . previous-line) ("SPC" . next-line)))
    (advice-add 'eshell-previous-input  :after (lambda (&rest r) "History search." (set-transient-map x)))
    (advice-add 'eshell-next-input      :after (lambda (&rest r) "History search." (set-transient-map x)))
    (add-hook 'eshell-post-command-hook (lambda () "History search."
                                          (keyamp-command-mode-activate)
                                          (set-transient-map x)
                                          (setq this-command 'eshell-previous-input)
                                          (set-face-background 'cursor keyamp-ampable-mode-cursor)))))

(with-eval-after-load 'shell
  (keyamp--define-keys shell-mode-map '(("C-h" . comint-interrupt-subjob) ("C-r" . comint-send-input)))
  (keyamp--define-keys-remap
   shell-mode-map
   '((cut-all      . comint-clear-buffer)
     (select-block . comint-previous-input)))
  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys-remap x '((previous-line . comint-previous-input) (next-line     . comint-next-input)))
    (advice-add 'comint-previous-input :after (lambda (&rest r) "History search." (set-transient-map x)))
    (advice-add 'comint-next-input     :after (lambda (&rest r) "History search." (set-transient-map x)))
    (add-hook 'comint-output-filter-functions (lambda (&rest r) "History search."
                                                (set-transient-map x)
                                                (keyamp-command-mode-activate)
                                                (setq this-command 'comint-previous-input)))))

(with-eval-after-load 'term
  (keyamp--define-keys
   term-raw-map
   '(("C-h" . term-interrupt-subjob) ("C-r" . term-send-input) ("C-c C-c" . term-line-mode)))
  (keyamp--define-keys
   term-mode-map
   '(("C-h" . term-interrupt-subjob) ("C-r" . term-send-input) ("C-c C-c" . term-char-mode)))
  (keyamp--define-keys-remap term-mode-map '((select-block . term-send-up)))
  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys-remap x '((previous-line . term-send-up) (next-line . term-send-down)))
    (advice-add 'term-send-up   :after (lambda (&rest r) "History search." (set-transient-map x)))
    (advice-add 'term-send-down :after (lambda (&rest r) "History search." (set-transient-map x)))
    (add-hook 'term-mode-hook (lambda () "History search." (set-transient-map x) (setq this-command 'term-send-up)))
    (add-hook 'term-input-filter-functions (lambda (&rest r) "History search."
                                             (set-transient-map x)
                                             (keyamp-command-mode-activate)
                                             (setq this-command 'term-send-up)))))

(with-eval-after-load 'info
  (keyamp--define-keys-remap
   Info-mode-map
   '((open-line                   . Info-backward-node)
     (newline                     . Info-forward-node)
     (cut-bracket-or-delete       . Info-next-reference)
     (undo                        . Info-up)
     (keyamp-insert-mode-activate . Info-follow-nearest-node)
     (down-line                   . scroll-down-line)
     (up-line                     . scroll-up-line)
     (right-char                  . Info-backward-node)
     (left-char                   . Info-forward-node)))
  (keyamp--define-keys Info-mode-map '(("TAB" . scroll-up-command) ("<tab>" . scroll-up-command)))
  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys x '(("DEL" . open-line) ("<backspace>" . open-line) ("SPC" . newline)))
    (advice-add 'Info-backward-node :after (lambda (&rest r) "Screen positioning." (set-transient-map x)))
    (advice-add 'Info-forward-node  :after (lambda (&rest r) "Screen positioning." (set-transient-map x)))))

(with-eval-after-load 'help-mode
  (keyamp--define-keys-remap
   help-mode-map
   '((cut-bracket-or-delete . forward-button)
     (undo                  . backward-button)
     (open-line             . help-go-back)
     (newline               . help-go-forward))))

(progn ; gnus
  (with-eval-after-load 'gnus-topic
    (keyamp--define-keys gnus-topic-mode-map '(("TAB" . toggle-ibuffer) ("<tab>" . toggle-ibuffer)))
    (keyamp--define-keys-remap
     gnus-topic-mode-map
     '((keyamp-insert-mode-activate . gnus-topic-select-group)
       (beginning-of-line-or-block  . gnus-topic-goto-previous-topic-line)
       (end-of-line-or-block        . gnus-topic-goto-next-topic-line)
       (previous-line               . previous-line-cycle)
       (next-line                   . next-line-cycle)))
    (let ((x (make-sparse-keymap)))
      (keyamp--define-keys-remap
       x
       '((previous-line              . gnus-topic-goto-previous-topic-line)
         (next-line                  . gnus-topic-goto-next-topic-line)
         (beginning-of-line-or-block . beginning-of-line-or-buffer)
         (end-of-line-or-block       . end-of-line-or-buffer)))
      (keyamp--define-keys x '(("DEL" . previous-line) ("<backspace>" . previous-line) ("SPC" . next-line)))
      (advice-add 'gnus-topic-goto-previous-topic-line :after (lambda (&rest r) "Cursor positioning." (set-transient-map x)))
      (advice-add 'gnus-topic-goto-next-topic-line     :after (lambda (&rest r) "Cursor positioning." (set-transient-map x)))
      (advice-add 'beginning-of-line-or-buffer         :after (lambda (&rest r) "Cursor positioning." (when gnus-topic-mode (set-transient-map x))))
      (advice-add 'end-of-line-or-buffer               :after (lambda (&rest r) "Cursor positioning." (when gnus-topic-mode (set-transient-map x))))))

  (with-eval-after-load 'gnus-group
    (keyamp--define-keys-remap
     gnus-group-mode-map
     '((undo                  . gnus-group-enter-server-mode)
       (cut-bracket-or-delete . gnus-group-get-new-news))))

  (with-eval-after-load 'gnus-sum
    (keyamp--define-keys
     gnus-summary-mode-map
     '(("C-h" . gnus-summary-delete-article)
       ("TAB" . scroll-up-command) ("<tab>" . scroll-up-command)))
    (keyamp--define-keys-remap
     gnus-summary-mode-map
     '((keyamp-insert-mode-activate . gnus-summary-scroll-up)
       (open-file-at-cursor         . keyamp-insert-mode-activate)
       (undo                        . gnus-summary-prev-article)
       (cut-bracket-or-delete       . gnus-summary-next-article)
       (open-line                   . gnus-summary-prev-group)
       (newline                     . gnus-summary-next-group)
       (kill-word                   . gnus-summary-save-parts)
       (down-line                   . scroll-down-line)
       (up-line                     . scroll-up-line)
       (right-char                  . gnus-summary-prev-group)
       (left-char                   . gnus-summary-next-group)))

    (let ((x (make-sparse-keymap)))
      (keyamp--define-keys-remap
       x
       '((undo                  . gnus-summary-prev-article)
         (cut-bracket-or-delete . gnus-summary-next-article)
         (open-line             . gnus-summary-prev-group)
         (newline               . gnus-summary-next-group)
         (down-line             . scroll-down-line)
         (up-line               . scroll-up-line)
         (right-char            . gnus-summary-prev-group)
         (left-char             . gnus-summary-next-group)))
      (keyamp--define-keys x '(("DEL" . gnus-summary-prev-group) ("<backspace>" . gnus-summary-prev-group) ("SPC" . gnus-summary-next-group)))
      (add-hook 'gnus-summary-prepared-hook (lambda () "Repeat." (set-transient-map x)))
      (advice-add 'gnus-summary-prev-group :after (lambda (&rest r) "Repeat." (set-transient-map x)))
      (advice-add 'gnus-summary-next-group :after (lambda (&rest r) "Repeat." (set-transient-map x))))

    (let ((x (make-sparse-keymap)))
      (keyamp--define-keys x '(("DEL" . gnus-summary-prev-article) ("<backspace>" . gnus-summary-prev-article) ("SPC" . gnus-summary-next-article)))
      (advice-add 'gnus-summary-prev-article :after (lambda (&rest r) "Repeat." (set-transient-map x)))
      (advice-add 'gnus-summary-next-article :after (lambda (&rest r) "Repeat." (set-transient-map x)))))

  (with-eval-after-load 'gnus-srvr
    (keyamp--define-keys-remap
     gnus-server-mode-map
     '((keyamp-insert-mode-activate . gnus-server-read-server)
       (open-file-at-cursor         . keyamp-insert-mode-activate)
       (cut-bracket-or-delete       . gnus-server-exit)))
    (keyamp--define-keys-remap
     gnus-browse-mode-map
     '((keyamp-insert-mode-activate . gnus-browse-select-group)
       (open-file-at-cursor         . keyamp-insert-mode-activate)))))

(with-eval-after-load 'snake
  (keyamp--define-keys-remap
   snake-mode-map
   '((keyamp-insert-mode-activate . snake-start-game)
     (keyamp-escape               . snake-pause-game)
     (next-line                   . snake-move-down) (extend-selection     . snake-move-down)
     (cut-bracket-or-delete       . snake-move-up)   (delete-other-windows . snake-rotate-up)))

  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys x '(("DEL" . snake-move-left) ("<backspace>" . snake-move-left) ("SPC" . snake-move-right)))
    (add-hook 'snake-mode-hook (lambda () (set-transient-map x)))
    (advice-add 'snake-start-game :after (lambda (&rest r) (set-transient-map x)))
    (advice-add 'snake-pause-game :after (lambda (&rest r) (set-transient-map x)))
    (advice-add 'snake-move-left  :after (lambda (&rest r) (set-transient-map x)))
    (advice-add 'snake-move-right :after (lambda (&rest r) (set-transient-map x)))
    (advice-add 'snake-move-down  :after (lambda (&rest r) (set-transient-map x)))
    (advice-add 'snake-move-up    :after (lambda (&rest r) (set-transient-map x)))))

(with-eval-after-load 'tetris
  (keyamp--define-keys-remap
   tetris-mode-map
   '((keyamp-escape               . tetris-pause-game)
     (keyamp-insert-mode-activate . tetris-start-game)
     (cut-bracket-or-delete       . tetris-rotate-prev) (delete-other-windows . tetris-rotate-prev)
     (next-line                   . tetris-move-bottom) (extend-selection     . tetris-move-bottom)))

  (let ((x (make-sparse-keymap)))
    (keyamp--define-keys x '(("DEL" . tetris-move-left) ("<backspace>" . tetris-move-left) ("SPC" . tetris-move-right)))
    (advice-add 'tetris-start-game  :after (lambda (&rest r) (set-transient-map x)))
    (advice-add 'tetris-pause-game  :after (lambda (&rest r) (set-transient-map x)))
    (advice-add 'tetris-move-left   :after (lambda (&rest r) (set-transient-map x)))
    (advice-add 'tetris-move-right  :after (lambda (&rest r) (set-transient-map x)))
    (advice-add 'tetris-rotate-prev :after (lambda (&rest r) (set-transient-map x)))
    (advice-add 'tetris-move-bottom :after (lambda (&rest r) (set-transient-map x)))))

(with-eval-after-load 'nov
  (keyamp--define-keys-remap
   nov-mode-map
   '((undo                        . nov-goto-toc)
     (open-line                   . nov-previous-document)
     (newline                     . nov-next-document)
     (keyamp-insert-mode-activate . nov-browse-url))))



(setq keyamp-ampable-commands-hash
      #s(hash-table
         size 100
         test equal
         data (Info-backward-node                        t
               Info-forward-node                         t
               agenda                                    t
               backward-kill-word                        t
               backward-left-bracket                     t
               backward-punct                            t
               backward-word                             t
               beginning-of-line-or-block                t
               beginning-of-line-or-buffer               t
               comint-next-input                         t
               comint-previous-input                     t
               copy-line-or-selection                    t
               cut-bracket-or-delete                     t
               cycle-hyphen-lowline-space                t
               delete-forward-char                       t
               delete-other-windows                      t
               dired-jump                                t
               dired-mark                                t
               dired-next-line                           t
               dired-previous-line                       t
               dired-unmark                              t
               down-line                                 t
               downloads                                 t
               end-of-line-or-block                      t
               end-of-line-or-buffer                     t
               eshell-next-input                         t
               eshell-previous-input                     t
               exchange-point-and-mark                   t
               extend-selection                          t
               forward-punct                             t
               forward-right-bracket                     t
               forward-word                              t
               gnus-summary-next-article                 t
               gnus-summary-next-group                   t
               gnus-summary-prev-article                 t
               gnus-summary-prev-group                   t
               gnus-topic-goto-next-topic-line           t
               gnus-topic-goto-previous-topic-line       t
               goto-matching-bracket                     t
               ibuffer-backward-filter-group             t
               ibuffer-forward-filter-group              t
               icomplete-backward-completions            t
               icomplete-forward-completions             t
               ido-next-match                            t
               ido-prev-match                            t
               insert-date                               t
               insert-space-before                       t
               isearch-repeat-backward                   t
               isearch-repeat-forward                    t
               isearch-ring-advance                      t
               isearch-ring-retreat                      t
               isearch-yank-kill                         t
               kill-region                               t
               kill-word                                 t
               move-row-down                             t
               move-row-up                               t
               next-line                                 t
               next-line-cycle                           t
               next-line-or-history-element              t
               next-user-buffer                          t
               org-shiftdown                             t
               org-shiftup                               t
               pop-local-mark-ring                       t
               previous-line                             t
               previous-line-cycle                       t
               previous-line-or-history-element          t
               previous-user-buffer                      t
               recenter-top-bottom                       t
               rectangle-mark-mode                       t
               save-close-current-buffer                 t
               scroll-down-command                       t
               scroll-up-command                         t
               search-current-word                       t
               select-line                               t
               select-text-in-quote                      t
               set-mark-command                          t
               shrink-whitespaces                        t
               split-window-below                        t
               tasks                                     t
               term-send-down                            t
               term-send-up                              t
               todo                                      t
               toggle-comment                            t
               toggle-letter-case                        t
               undo                                      t
               undo-redo                                 t
               up-line                                   t
               works                                     t
               yank                                      t
               yank-pop                                  t)))



(defvar keyamp-insert-state-p t "Non-nil means insertion mode is on.")

(defvar keyamp-insert-mode-idle-timer  nil "Idle timer for exit insert mode.")
(defvar keyamp-ampable-mode-idle-timer nil "Idle timer for exit ampable mode.")

(defun keyamp-command-mode-init ()
  "Set command mode keys."
  (setq keyamp-insert-state-p nil)
  (when keyamp--deactivate-command-mode-func
    (funcall keyamp--deactivate-command-mode-func))
  (setq keyamp--deactivate-command-mode-func
        (set-transient-map keyamp-command-map (lambda () t)))
  (set-face-background 'cursor keyamp-command-mode-cursor)
  (setq mode-line-front-space keyamp-command-mode-indicator)
  (force-mode-line-update)
  (when (timerp keyamp-insert-mode-idle-timer)
    (cancel-timer keyamp-insert-mode-idle-timer)))

(defun keyamp-insert-mode-init ()
  "Enter insertion mode."
  (setq keyamp-insert-state-p t)
  (funcall keyamp--deactivate-command-mode-func)
  (set-face-background 'cursor keyamp-insert-mode-cursor)
  (setq mode-line-front-space keyamp-insert-mode-indicator)
  (force-mode-line-update)
  (setq keyamp-insert-mode-idle-timer
        (run-with-idle-timer keyamp-idle-timeout nil 'keyamp-command-mode-activate)))

(defun keyamp-command-mode-init-karabiner ()
  "Karabiner integration.
Init command mode with `keyamp-command-mode-activate-hook'."
  (call-process keyamp-karabiner-cli nil 0 nil
                "--set-variables" "{\"insert mode activated\":0}"))

(defun keyamp-insert-mode-init-karabiner ()
  "Karabiner integration.
Init insert mode with `keyamp-insert-mode-activate-hook'."
  (call-process keyamp-karabiner-cli nil 0 nil
                "--set-variables" "{\"insert mode activated\":1}"))

(defun keyamp-command-mode-activate ()
  "Activate command mode."
  (interactive)
  (keyamp-command-mode-init)
  (run-hooks 'keyamp-command-mode-activate-hook))

(defun keyamp-insert-mode-activate ()
  "Activate insert mode."
  (interactive)
  (keyamp-insert-mode-init)
  (run-hooks 'keyamp-insert-mode-activate-hook))

(defun keyamp-ampable-mode-indicate ()
  "Indicate ampable mode. Run with `post-command-hook'.
Also run by `keyamp-clear-transient-map' as non-ampable command."
  (interactive)
  (if (or (eq real-this-command 'repeat)
          (gethash this-command keyamp-ampable-commands-hash))
      (progn
        (setq mode-line-front-space keyamp-ampable-mode-indicator)
        (set-face-background 'cursor keyamp-ampable-mode-cursor))
    (progn
      (unless keyamp-insert-state-p
        (setq mode-line-front-space keyamp-command-mode-indicator)
        (set-face-background 'cursor keyamp-command-mode-cursor)))
    (force-mode-line-update)))

(defun keyamp-clear-transient-map ()
  "Emulate keyboard press to run non-ampable command that clears
transient keymaps. Run with `keyamp-ampable-mode-idle-timer'."
  (unless (string-equal major-mode "ibuffer-mode")
    (execute-kbd-macro (kbd "C-<escape>"))))



;;;###autoload
(define-minor-mode keyamp
  "Keyboard «Amplifier»."
  :global t
  :keymap keyamp-insert-map

  (when keyamp
    (add-hook 'minibuffer-setup-hook    'keyamp-command-mode-activate)
    (add-hook 'minibuffer-exit-hook     'keyamp-command-mode-activate)
    (add-hook 'isearch-mode-end-hook    'keyamp-command-mode-activate)
    (add-hook 'post-command-hook        'keyamp-ampable-mode-indicate)
    (when (file-exists-p keyamp-karabiner-cli)
      (add-hook 'keyamp-insert-mode-activate-hook  'keyamp-insert-mode-init-karabiner)
      (add-hook 'keyamp-command-mode-activate-hook 'keyamp-command-mode-init-karabiner))
    (keyamp-catch-tty-ESC)
    (keyamp-define-input-source 'russian-computer)
    (keyamp-command-mode-activate)
    (setq keyamp-ampable-mode-idle-timer
          (run-with-idle-timer keyamp-idle-timeout t 'keyamp-clear-transient-map))))

(provide 'keyamp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical)
;; End:
;;; keyamp.el ends here
