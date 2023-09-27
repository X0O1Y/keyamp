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
        (message "Translation qwerty to engineer engram deactivated"))
    (progn
      (put 'keyamp-qwerty-to-engineer-engram 'state t)
      (quail-set-keyboard-layout "engineer-engram")
      (message "Translation qwerty to engineer engram activated")))
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
  "Return to command mode. Escape everything then toggle ibuffer."
  (interactive)
  (if (or (eq last-repeatable-command 'repeat)
          (gethash last-command keyamp-ampable-commands-hash))
      (message "")
    (progn
      (if (active-minibuffer-window)
          (abort-recursive-edit)
        (if (region-active-p)
            (deactivate-mark)
          (toggle-ibuffer))))))

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
   ("SPC" . keyamp-leader-right-key-map)                                               ("S-SPC"         . ignore)
                                                                                       ("S-<tab>"       . ignore)

   ;; left half
   ("`" . other-frame)                ("ё" . other-frame)                ("~" . ignore) ("Ë" . ignore)
   ("1" . kmacro-play)                                                   ("!" . ignore)
   ("2" . kmacro-helper)                                                 ("@" . ignore)
   ("3" . kmacro-record)                                                 ("#" . ignore) ("№" . ignore)
   ("4" . append-to-register-1)                                          ("$" . ignore)
   ("5" . clear-register-1)                                              ("%" . ignore)

   ("q" . insert-space-before)        ("й" . insert-space-before)        ("Q" . ignore) ("Й" . ignore)
   ("w" . backward-kill-word)         ("ц" . backward-kill-word)         ("W" . ignore) ("Ц" . ignore)
   ("e" . undo)                       ("у" . undo)                       ("E" . ignore) ("У" . ignore)
   ("r" . kill-word)                  ("к" . kill-word)                  ("R" . ignore) ("К" . ignore)
   ("t" . cut-text-block)             ("е" . cut-text-block)             ("T" . ignore) ("Е" . ignore)

   ("a" . shrink-whitespaces)         ("ф" . shrink-whitespaces)         ("A" . ignore) ("Ф" . ignore)
   ("s" . open-line)                  ("ы" . open-line)                  ("S" . ignore) ("Ы" . ignore)
   ("d" . cut-bracket-or-delete)      ("в" . cut-bracket-or-delete)      ("D" . ignore) ("В" . ignore)
   ("f" . newline)                    ("а" . newline)                    ("F" . ignore) ("А" . ignore)
   ("g" . set-mark-command)           ("п" . set-mark-command)           ("G" . ignore) ("П" . ignore)

   ("z" . toggle-comment)             ("я" . toggle-comment)             ("Z" . ignore) ("Я" . ignore)
   ("x" . cut-line-or-selection)      ("ч" . cut-line-or-selection)      ("X" . ignore) ("Ч" . ignore)
   ("c" . copy-line-or-selection)     ("с" . copy-line-or-selection)     ("C" . ignore) ("С" . ignore)
   ("v" . paste-or-paste-previous)    ("м" . paste-or-paste-previous)    ("V" . ignore) ("М" . ignore)
   ("b" . toggle-letter-case)         ("и" . toggle-letter-case)         ("B" . ignore) ("И" . ignore)

   ("<f1>" . ignore)
   ("<f2>" . ignore)
   ("<f3>" . pass)
   ("<f4>" . ignore)
   ("<f5>" . eperiodic)

   ;; right half
   ("6" . ignore)                                                        ("^" . ignore)
   ("7" . number-to-register)                                            ("&" . ignore)
   ("8" . copy-to-register)                                              ("*" . goto-matching-bracket) ; qwerty「*」→「=」engram, qwerty「/」→「=」ru pc karabiner
   ("9" . ignore)                                                        ("(" . ignore)
   ("0" . terminal)                                                      (")" . ignore)
   ("-" . ignore)                                                        ("_" . ignore)
   ("=" . ignore)                                                        ("+" . ignore)

   ("y"  . search-current-word)       ("н" . search-current-word)        ("Y" . ignore) ("Н" . ignore)
   ("u"  . backward-word)             ("г" . backward-word)              ("U" . ignore) ("Г" . ignore)
   ("i"  . previous-line)             ("ш" . previous-line)              ("I" . ignore) ("Ш" . ignore)
   ("o"  . forward-word)              ("щ" . forward-word)               ("O" . ignore) ("Щ" . ignore)
   ("p"  . exchange-point-and-mark)   ("з" . exchange-point-and-mark)    ("P" . ignore) ("З" . ignore)
   ("["  . alternate-buffer)          ("х" . alternate-buffer)           ("{" . ignore) ("Х" . ignore)
   ("]"  . ignore)                    ("ъ" . ignore)                     ("}" . ignore) ("Ъ" . ignore)
   ("\\" . ignore)                                                       ("|" . ignore)

   ("h" . beginning-of-line-or-block) ("р" . beginning-of-line-or-block) ("H"  . ignore) ("Р" . ignore)
   ("j" . backward-char)              ("о" . backward-char)              ("J"  . ignore) ("О" . ignore)
   ("k" . next-line)                  ("л" . next-line)                  ("K"  . ignore) ("Л" . ignore)
   ("l" . forward-char)               ("д" . forward-char)               ("L"  . ignore) ("Д" . ignore)
   (";" . end-of-line-or-block)       ("ж" . end-of-line-or-block)       (":"  . ignore) ("Ж" . ignore)
   ("'" . alternate-buffer)           ("э" . alternate-buffer)           ("\"" . ignore) ("Э" . ignore)

   ("n" . isearch-forward)            ("т" . isearch-forward)            ("N" . ignore) ("Т" . ignore)
   ("m" . backward-left-bracket)      ("ь" . backward-left-bracket)      ("M" . ignore) ("Ь" . ignore)
   ("," . next-window-or-frame)       ("б" . next-window-or-frame)       ("<" . ignore) ("Б" . ignore)
   ("." . forward-right-bracket)      ("ю" . forward-right-bracket)      (">" . ignore) ("Ю" . ignore)
   ("/" . goto-matching-bracket)                                         ("?" . ignore)

   ("<up>"    . up-line)                                                 ("S-<up>"    . ignore)
   ("<down>"  . down-line)                                               ("S-<down>"  . ignore)
   ("<left>"  . left-char)                                               ("S-<left>"  . ignore)
   ("<right>" . right-char)                                              ("S-<right>" . ignore)
   ("<prior>" . works)                                                   ("S-<prior>" . ignore)
   ("<next>"  . agenda)                                                  ("S-<next>"  . ignore)

   ("<f6>"  . books)
   ("<f7>"  . ignore)
   ("<f8>"  . player)
   ("<f9>"  . ignore)
   ("<f10>" . ignore)
   ("<f11>" . ignore)
   ("<f12>" . ignore)))

(keyamp--define-keys
 (define-prefix-command 'keyamp-leader-left-key-map)
 '(("SPC" . repeat)
   ("DEL" . repeat)                   ("<backspace>" . repeat)
   ("RET" . execute-extended-command) ("<return>"    . execute-extended-command)
   ("TAB" . ignore)                   ("<tab>"       . ignore)

   ;; left leader left half
   ("`" . screenshot)
   ("1" . apply-macro-to-region-lines)
   ("2" . kmacro-name-last-macro)
   ("3" . ignore)
   ("4" . ignore)
   ("5" . ignore)

   ("q" . reformat-lines)
   ("e" . split-window-below)
   ("r" . query-replace)
   ("t" . kill-line)

   ("a" . delete-window)
   ("s" . previous-user-buffer)
   ("d" . delete-other-windows)
   ("f" . next-user-buffer)
   ("g" . rectangle-mark-mode)

   ("z" . universal-argument)
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
   ("i f" . find-file)                     ("i j" . recentf-open-files)
   ("i t" . list-recently-closed)          ("i o" . bookmark-bmenu-list)
   ("i b" . set-buffer-file-coding-system) ("i n" . revert-buffer-with-coding-system)
                                           ("i p" . bookmark-set)
   ("i r" . rename-visited-file)           ("i u" . open-in-terminal)

   ("o"  . bookmark-jump)
   ("p"  . view-echo-area-messages)
   ("["  . ignore)
   ("]"  . ignore)
   ("\\" . ignore)

   ("h" . scroll-down-command)

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

   (";" . scroll-up-command)
   ("'" . toggle-debug-on-error)

   ("n" . save-some-buffers)
   ("m" . downloads)
   ("," . open-last-closed)
   ("." . mark-defun)
   ("/" . goto-line)

   ("<escape>"   . ignore) ("ESC"   . ignore)
   ("i <escape>" . ignore) ("i ESC" . ignore)
   ("j <escape>" . ignore) ("j ESC" . ignore)
   ("k <escape>" . ignore) ("k ESC" . ignore)
   ("l <escape>" . ignore) ("l ESC" . ignore)))

(keyamp--define-keys
 (define-prefix-command 'keyamp-leader-right-key-map)
 '(("SPC" . repeat)
   ("DEL" . repeat)                   ("<backspace>" . repeat)
   ("RET" . execute-extended-command) ("<return>"    . execute-extended-command)
   ("TAB" . indent-for-tab-command)   ("<tab>"       . indent-for-tab-command)

   ;; right leader left half
   ("`" . toggle-frame-maximized)
   ("1" . ignore)
   ("2" . insert-kbd-macro)
   ("3" . ignore)
   ("4" . ignore)
   ("5" . ignore)

   ("q" . fill-or-unfill)
   ("w" . news)

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

   ("h" . backward-punct)
   ("j" . select-bracket)
   ("k" . extend-selection)
   ("l" . select-text-in-quote)
   (";" . forward-punct)
   ("'" . sync)

   ("n" . save-buffer)
   ("m" . dired-jump)
   ("," . save-close-current-buffer)
   ("." . recenter-top-bottom)
   ("/" . select-line)            ("*" . select-line)

   ("<f8>" . player)

   ("<escape>"   . ignore) ("ESC"   . ignore)
   ("e <escape>" . ignore) ("e ESC" . ignore)
   ("s <escape>" . ignore) ("s ESC" . ignore)
   ("d <escape>" . ignore) ("d ESC" . ignore)
   ("f <escape>" . ignore) ("f ESC" . ignore)))

(keyamp--define-keys query-replace-map '(("C-h" . skip) ("C-r" . act)))

(keyamp--define-keys
 global-map
 '(("C-r" . open-file-at-cursor)
   ("C-f" . keyamp-qwerty-to-engineer-engram)
   ("C-t" . autocomplete)))

(with-eval-after-load 'help
  (keyamp--define-keys
   help-map
   '(("i" . lookup-word-definition)
     ("j" . lookup-web)
     ("k" . lookup-google-translate)
     ("l" . lookup-wikipedia)
     ("u" . lookup-all-synonyms)
     ("o" . lookup-all-dictionaries)
     ("m" . lookup-word-dict-org)
     ("," . lookup-etymology)
     ("." . lookup-wiktionary)

     ("ESC"  . ignore) ("<escape>" . ignore)
     ("<f1>" . ignore) ("<help>"   . ignore)
     ("C-w"  . ignore) ("C-c"      . ignore)
     ("C-o"  . ignore) ("C-\\"     . ignore)
     ("C-n"  . ignore) ("C-f"      . ignore)
     ("C-s"  . ignore) ("C-e"      . ignore)
     ("RET"  . ignore) ("'"        . ignore)
     ("6"    . ignore) ("?"        . ignore)
     ("9"    . ignore) ("L"        . ignore)
     ("n"    . ignore) ("d"        . ignore)
     ("z"    . ignore) ("p"        . ignore))))


;; repeat

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((cut-bracket-or-delete . repeat)))
  (keyamp--define-keys
   xk
   '(("DEL" . repeat) ("<backspace>" . repeat) ("SPC" . repeat)))
  (advice-add 'repeat :after (lambda (&rest r) "Repeat." (set-transient-map xk))))


;; screen

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((insert-space-before     . sun-moon)                ; q
     (backward-kill-word      . news)                    ; w
     (undo                    . split-window-below)      ; e
     (kill-word               . make-frame-command)      ; r
     (cut-text-block          . toggle-eshell)           ; t
     (shrink-whitespaces      . delete-window)           ; a
     (open-line               . previous-user-buffer)    ; s
     (cut-bracket-or-delete   . alternate-buffer)        ; d
     (newline                 . next-user-buffer)        ; f
     (set-mark-command        . new-empty-buffer)        ; g
     (copy-line-or-selection  . agenda)                  ; c
     (paste-or-paste-previous . tasks)                   ; v
     (backward-word           . switch-to-buffer)        ; u
     (forward-word            . bookmark-jump)           ; o
     (exchange-point-and-mark . view-echo-area-messages) ; p
     (backward-left-bracket   . downloads)               ; m
     (forward-right-bracket   . player)                  ; .
     ))
  (keyamp--define-keys
   xk
   '(("DEL" . previous-user-buffer) ("<backspace>" . previous-user-buffer)
     ("SPC" . next-user-buffer)))

  (advice-add 'agenda                    :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'alternate-buffer          :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'delete-other-windows      :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'delete-window             :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'downloads                 :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'next-user-buffer          :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'player                    :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'previous-user-buffer      :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'save-close-current-buffer :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'split-window-below        :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'sun-moon                  :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'tasks                     :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'toggle-eshell             :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'view-echo-area-messages   :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))

  (add-hook 'help-mode-hook
            (lambda () "Screen positioning."
              (set-transient-map xk) (setq this-command 'split-window-below)))
  (add-hook 'run-current-file-after-hook
            (lambda () "Screen positioning."
              (set-transient-map xk) (setq this-command 'split-window-below)))

  (add-hook 'ibuffer-hook
            (lambda () "Screen positioning."
              (set-transient-map xk) (setq this-command 'ibuffer-next-line)))
  (advice-add 'ibuffer-forward-filter-group  :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'ibuffer-backward-filter-group :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'ibuffer-previous-line         :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'ibuffer-next-line             :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'ibuffer-toggle-filter-group   :after (lambda (&rest r) "Screen positioning." (set-transient-map xk)))
  (advice-add 'ibuffer-do-delete             :after (lambda (&rest r) "Screen positioning." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((backward-left-bracket . dired-jump)))
  (keyamp--define-keys xk       '(("DEL" . dired-jump) ("<backspace>" . dired-jump) ("SPC" . dired-jump)))
  (advice-add 'dired-jump :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'downloads  :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((next-window-or-frame . save-close-current-buffer)))
  (advice-add 'save-close-current-buffer :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((cut-bracket-or-delete . tasks)))
  (keyamp--define-keys xk       '(("DEL" . tasks) ("<backspace>" . tasks) ("SPC" . tasks)))
  (advice-add 'tasks :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((cut-bracket-or-delete . works)))
  (keyamp--define-keys xk       '(("DEL" . works) ("<backspace>" . works) ("SPC" . works)))
  (advice-add 'works :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((agenda . post-task)))
  (advice-add 'agenda :after (lambda (&rest r) "Repeat." (set-transient-map xk))))


;; edit

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((cut-bracket-or-delete . insert-space-before))) ; q
  (keyamp--define-keys xk       '(("DEL" . insert-space-before) ("<backspace>" . insert-space-before) ("SPC" . insert-space-before)))
  (advice-add 'insert-space-before :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((open-line . backward-kill-word) ; w
     (newline   . kill-word)))        ; r
  (advice-add 'backward-kill-word
              :after (lambda (&rest r) "Repeat."
                       (set-transient-map xk) (setq this-command 'kill-region)))
  (advice-add 'kill-word
              :after (lambda (&rest r) "Repeat."
                       (set-transient-map xk) (setq this-command 'kill-region))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((cut-bracket-or-delete . shrink-whitespaces))) ; a
  (keyamp--define-keys xk       '(("DEL" . shrink-whitespaces) ("<backspace>" . shrink-whitespaces) ("SPC" . shrink-whitespaces)))
  (advice-add 'shrink-whitespaces :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((cut-bracket-or-delete . cut-text-block))) ; g
  (advice-add 'cut-text-block
              :after (lambda (&rest r) "Repeat."
                       (set-transient-map xk) (setq this-command 'kill-region))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((cut-bracket-or-delete . toggle-comment))) ; z
  (keyamp--define-keys xk       '(("DEL" . toggle-comment) ("<backspace>" . toggle-comment) ("SPC" . toggle-comment)))
  (advice-add 'toggle-comment :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((cut-bracket-or-delete . cut-line-or-selection))) ; x
    (keyamp--define-keys xk     '(("DEL" . cut-line-or-selection) ("<backspace>" . cut-line-or-selection) ("SPC" . cut-line-or-selection)))
  (advice-add 'cut-line-or-selection
              :after (lambda (&rest r) "Repeat."
                       (set-transient-map xk) (setq this-command 'kill-region))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((cut-bracket-or-delete . copy-line-or-selection))) ; c
  (keyamp--define-keys xk       '(("DEL" . copy-line-or-selection) ("<backspace>" . copy-line-or-selection) ("SPC" . copy-line-or-selection)))
  (advice-add 'copy-line-or-selection :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((cut-bracket-or-delete . toggle-letter-case))) ; b
    (keyamp--define-keys xk     '(("DEL" . toggle-letter-case) ("<backspace>" . toggle-letter-case) ("SPC" . toggle-letter-case)))
  (advice-add 'toggle-letter-case :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((undo                  . move-row-up)
     (cut-bracket-or-delete . move-row-down)))
  (keyamp--define-keys
   xk
   '(("DEL" . move-row-up) ("<backspace>" . move-row-up)
     ("SPC" . move-row-down)))
  (advice-add 'move-row-up   :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'move-row-down :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((undo                   . org-shiftup)
     (cut-bracket-or-delete  . org-shiftdown)
     (copy-line-or-selection . agenda)))
  (advice-add 'org-shiftup   :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'org-shiftdown :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((undo                   . todo)
     (copy-line-or-selection . agenda)))
  (advice-add 'todo        :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'insert-date :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((cut-bracket-or-delete . cycle-hyphen-lowline-space)))
  (advice-add 'cycle-hyphen-lowline-space :after (lambda (&rest r) "Repeat." (set-transient-map xk))))


;; move

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((previous-line              . beginning-of-line-or-block)
     (next-line                  . end-of-line-or-block)
     (backward-char              . backward-word)
     (forward-char               . forward-word)
     (beginning-of-line-or-block . beginning-of-line-or-buffer)
     (end-of-line-or-block       . end-of-line-or-buffer)))
  (keyamp--define-keys
   xk
   '(("DEL" . previous-line) ("<backspace>" . previous-line)
     ("SPC" . next-line)))

  (advice-add 'beginning-of-line-or-block  :after (lambda (&rest r) "Cursor positioning." (set-transient-map xk)))
  (advice-add 'end-of-line-or-block        :after (lambda (&rest r) "Cursor positioning." (set-transient-map xk)))
  (advice-add 'set-mark-command            :after (lambda (&rest r) "Cursor positioning." (set-transient-map xk)))
  (advice-add 'exchange-point-and-mark     :after (lambda (&rest r) "Cursor positioning." (set-transient-map xk)))
  (advice-add 'beginning-of-line-or-buffer :after (lambda (&rest r) "Cursor positioning." (set-transient-map xk)))
  (advice-add 'end-of-line-or-buffer       :after (lambda (&rest r) "Cursor positioning." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((backward-char . backward-punct)
     (forward-char  . forward-punct)))
  (keyamp--define-keys
   xk
   '(("DEL" . backward-punct) ("<backspace>" . backward-punct)
     ("SPC" . forward-punct)))
  (advice-add 'backward-punct :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'forward-punct  :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((backward-char . backward-word)
     (forward-char  . forward-word)))
  (keyamp--define-keys
   xk
   '(("DEL" . backward-word) ("<backspace>" . backward-word)
     ("SPC" . forward-word)))
  (advice-add 'backward-word :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'forward-word  :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((previous-line . backward-left-bracket)
     (next-line     . forward-right-bracket)))
  (keyamp--define-keys
   xk
   '(("DEL" . backward-left-bracket) ("<backspace>" . backward-left-bracket)
     ("SPC" . forward-right-bracket)))
  (advice-add 'backward-left-bracket :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'forward-right-bracket :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'goto-matching-bracket :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((previous-line . scroll-down-command)
     (next-line     . scroll-up-command)))
  (keyamp--define-keys
   xk
   '(("DEL" . scroll-down-command) ("<backspace>" . scroll-down-command)
     ("SPC" . scroll-up-command)))
  (advice-add 'scroll-down-command :after (lambda (&rest r) "Repeat." (set-transient-map xk) (recenter)))
  (advice-add 'scroll-up-command   :after (lambda (&rest r) "Repeat." (set-transient-map xk) (recenter))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((next-line . pop-local-mark-ring)))
  (advice-add 'pop-local-mark-ring :after (lambda (&rest r) "Repeat." (set-transient-map xk) (recenter))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((next-line . recenter-top-bottom)))
  (advice-add 'recenter-top-bottom :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((next-line . select-block)))
  (advice-add 'select-block :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap
   xk
   '((next-line     . extend-selection)
     (backward-char . backward-word)
     (forward-char  . forward-word)))
  (advice-add 'extend-selection :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((next-line . select-line)))
  (advice-add 'select-line :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys-remap xk '((next-line . select-text-in-quote)))
  (advice-add 'select-text-in-quote :after (lambda (&rest r) "Repeat." (set-transient-map xk))))

(keyamp--define-keys
 isearch-mode-map
 '(("<escape>" . isearch-abort)
   ("<up>"     . isearch-ring-retreat)    ("C-_ i" . isearch-ring-retreat)    ("C-И i" . isearch-ring-retreat)
   ("<left>"   . isearch-repeat-backward) ("C-_ j" . isearch-repeat-backward) ("C-И j" . isearch-repeat-backward)
   ("<down>"   . isearch-ring-advance)    ("C-_ k" . isearch-ring-advance)    ("C-И k" . isearch-ring-advance)
   ("<right>"  . isearch-repeat-forward)  ("C-_ l" . isearch-repeat-forward)  ("C-И l" . isearch-repeat-forward)
   ("C-^"      . ignore)                  ("C-_ n" . isearch-yank-kill)       ("C-И n" . isearch-yank-kill)))

(let ((xk (make-sparse-keymap)))
  (keyamp--define-keys
   xk
   '(("i" . isearch-ring-retreat)      ("ш" . isearch-ring-retreat)
     ("j" . isearch-repeat-backward)   ("о" . isearch-repeat-backward)
     ("k" . isearch-ring-advance)      ("л" . isearch-ring-advance)
     ("l" . isearch-repeat-forward)    ("д" . isearch-repeat-forward)
     ("d" . repeat)                    ("у" . repeat)
     ("DEL" . isearch-repeat-backward) ("SPC" . isearch-repeat-forward)))

  (advice-add 'isearch-ring-retreat    :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'isearch-repeat-backward :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'isearch-ring-advance    :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'isearch-repeat-forward  :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'search-current-word     :after (lambda (&rest r) "Repeat." (set-transient-map xk)))
  (advice-add 'isearch-yank-kill       :after (lambda (&rest r) "Repeat." (set-transient-map xk))))


;; modes

(with-eval-after-load 'minibuffer
  (keyamp--define-keys-remap
   minibuffer-local-map
   '((previous-line . previous-line-or-history-element)
     (next-line     . next-line-or-history-element)))

  (keyamp--define-keys-remap
   minibuffer-mode-map
   '((previous-line . previous-line-or-history-element)
     (next-line     . next-line-or-history-element))))

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

  (let ((xk (make-sparse-keymap)))
    (keyamp--define-keys-remap
     xk
     '((previous-line               . previous-line-or-history-element)
       (next-line                   . next-line-or-history-element)
       (keyamp-insert-mode-activate . exit-minibuffer)))
    (advice-add 'previous-line-or-history-element :after (lambda (&rest r) "History search." (set-transient-map xk)))
    (advice-add 'next-line-or-history-element     :after (lambda (&rest r) "History search." (set-transient-map xk)))))

(add-hook 'ido-setup-hook (lambda ()
                            (keyamp--define-keys
                             ido-completion-map
                             '(("C-r" . ido-exit-minibuffer)))
                            (keyamp--define-keys-remap
                             ido-completion-map
                             '((previous-line . ido-prev-match)
                               (next-line     . ido-next-match)))))

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
       (insert-space-before         . dired-hide-details-mode))))

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
    (keyamp--define-keys ibuffer-mode-map '(("C-h" . ibuffer-do-delete) ("C-r" . ibuffer-diff-with-file)))

    (keyamp--define-keys-remap
     ibuffer-mode-map
     '((keyamp-insert-mode-activate . ibuffer-visit-buffer)
       (end-of-line-or-block        . ibuffer-forward-filter-group)
       (beginning-of-line-or-block  . ibuffer-backward-filter-group)
       (previous-line               . ibuffer-previous-line)
       (next-line                   . ibuffer-next-line)))

    (keyamp--define-keys-remap ibuffer-mode-filter-group-map '((keyamp-insert-mode-activate . ibuffer-toggle-filter-group)))

    (let ((xk (make-sparse-keymap)))
      (keyamp--define-keys-remap
       xk
       '((previous-line              . ibuffer-backward-filter-group)
         (next-line                  . ibuffer-forward-filter-group)
         (beginning-of-line-or-block . beginning-of-line-or-buffer)
         (end-of-line-or-block       . end-of-line-or-buffer)))
      (advice-add 'ibuffer-backward-filter-group :after (lambda (&rest r) "Cursor positioning." (set-transient-map xk)))
      (advice-add 'ibuffer-forward-filter-group  :after (lambda (&rest r) "Cursor positioning." (set-transient-map xk)))
      (advice-add 'ibuffer-toggle-filter-group   :after (lambda (&rest r) "Cursor positioning." (set-transient-map xk)))))

  (let ((xk (make-sparse-keymap)))
    (keyamp--define-keys-remap xk '((cut-bracket-or-delete . ibuffer-do-delete)))
      (keyamp--define-keys xk '(("DEL" . ibuffer-do-delete) ("<backspace>" . ibuffer-do-delete) ("SPC" . ibuffer-do-delete)))
    (advice-add 'ibuffer-do-delete :after (lambda (&rest r) "Repeat." (set-transient-map xk)))))

(with-eval-after-load 'transient
    (keyamp--define-keys transient-base-map '(("<escape>" . transient-quit-one))))

(progn ; remap RET
  (with-eval-after-load 'button
    (keyamp--define-keys-remap button-map '((keyamp-insert-mode-activate . push-button))))

  (with-eval-after-load 'simple
    (keyamp--define-keys-remap completion-list-mode-map '((keyamp-insert-mode-activate . choose-completion))))

  (with-eval-after-load 'org-agenda
    (keyamp--define-keys-remap org-agenda-mode-map '((keyamp-insert-mode-activate . org-agenda-switch-to))))

  (with-eval-after-load 'shr
    (keyamp--define-keys-remap shr-map '((keyamp-insert-mode-activate . shr-browse-url))))

  (with-eval-after-load 'arc-mode
    (keyamp--define-keys-remap archive-mode-map '((keyamp-insert-mode-activate . archive-extract))))

  (with-eval-after-load 'wid-edit
    (keyamp--define-keys-remap widget-link-keymap '((keyamp-insert-mode-activate . widget-button-press))))

  (with-eval-after-load 'bookmark
    (keyamp--define-keys-remap bookmark-bmenu-mode-map '((keyamp-insert-mode-activate . bookmark-bmenu-this-window))))

  (with-eval-after-load 'replace
    (keyamp--define-keys-remap occur-mode-map '((keyamp-insert-mode-activate . occur-mode-goto-occurrence))))

  (with-eval-after-load 'compile
    (keyamp--define-keys-remap compilation-button-map '((keyamp-insert-mode-activate . compile-goto-error)))))

(with-eval-after-load 'doc-view
  (keyamp--define-keys-remap
   doc-view-mode-map
   '((previous-line . doc-view-previous-line-or-previous-page)
     (next-line     . doc-view-next-line-or-next-page)
     (backward-char . doc-view-previous-page)
     (forward-char  . doc-view-next-page)
     (backward-word . doc-view-shrink)
     (forward-word  . doc-view-enlarge))))

(with-eval-after-load 'image-mode
  (keyamp--define-keys-remap
   image-mode-map
   '((backward-char . image-previous-file) (forward-char . image-next-file))))

(with-eval-after-load 'esh-mode
  (keyamp--define-keys eshell-mode-map '(("C-h" . eshell-interrupt-process) ("C-r" . eshell-send-input)))

  (keyamp--define-keys-remap
   eshell-mode-map
   '((cut-line-or-selection . eshell-clear-input)
     (cut-all               . eshell-clear)
     (select-block          . eshell-previous-matching-input-from-input)))

  (let ((xk (make-sparse-keymap)))
    (keyamp--define-keys-remap
     xk
     '((previous-line . eshell-previous-matching-input-from-input)
       (next-line     . eshell-next-matching-input-from-input)))
    (advice-add 'eshell-previous-matching-input-from-input  :after (lambda (&rest r) "History search." (set-transient-map xk)))
    (advice-add 'eshell-next-matching-input-from-input      :after (lambda (&rest r) "History search." (set-transient-map xk)))
    (add-hook 'eshell-post-command-hook (lambda () "History search."
                                          (set-transient-map xk)
                                          (set-face-background 'cursor keyamp-ampable-mode-cursor)))))

(with-eval-after-load 'shell
  (keyamp--define-keys shell-mode-map '(("C-h" . comint-interrupt-subjob) ("C-r" . comint-send-input)))

  (keyamp--define-keys-remap
   shell-mode-map
   '((cut-all      . comint-clear-buffer)
     (select-block . comint-previous-matching-input-from-input)))

  (let ((xk (make-sparse-keymap)))
    (keyamp--define-keys-remap
     xk
     '((previous-line . comint-previous-matching-input-from-input)
       (next-line     . comint-next-matching-input-from-input)))
    (advice-add 'comint-previous-matching-input-from-input  :after (lambda (&rest r) "History search." (set-transient-map xk)))
    (advice-add 'comint-next-matching-input-from-input      :after (lambda (&rest r) "History search." (set-transient-map xk)))
    (add-hook 'comint-output-filter-functions (lambda (&rest r) "History search."
                                                (set-transient-map xk)
                                                (keyamp-command-mode-activate)
                                                (setq this-command 'comint-previous-matching-input-from-input)))))

(with-eval-after-load 'term
  (keyamp--define-keys
   term-raw-map
   '(("C-h" . term-interrupt-subjob) ("C-r" . term-send-input) ("C-c C-c" . term-line-mode)))

  (keyamp--define-keys
   term-mode-map
   '(("C-h" . term-interrupt-subjob) ("C-r" . term-send-input) ("C-c C-c" . term-char-mode)))

  (keyamp--define-keys-remap term-mode-map '((select-block . term-send-up)))

  (let ((xk (make-sparse-keymap)))
    (keyamp--define-keys-remap
     xk
     '((previous-line . term-send-up)
       (next-line     . term-send-down)))
    (advice-add 'term-send-up   :after (lambda (&rest r) "History search." (set-transient-map xk)))
    (advice-add 'term-send-down :after (lambda (&rest r) "History search." (set-transient-map xk)))
    (add-hook 'term-mode-hook (lambda (&rest r) "History search."
                                (set-transient-map xk)
                                (setq this-command 'term-send-up)))
    (add-hook 'term-input-filter-functions (lambda (&rest r) "History search."
                                             (set-transient-map xk)
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
     (up-line                     . scroll-down-command)
     (left-char                   . Info-backward-node)
     (down-line                   . scroll-up-command)
     (right-char                  . Info-forward-node))))

(with-eval-after-load 'help-mode
  (keyamp--define-keys-remap
   help-mode-map
   '((cut-bracket-or-delete . forward-button)
     (undo                  . backward-button)
     (open-line             . help-go-back)
     (newline               . help-go-forward))))

(progn ; gnus
  (with-eval-after-load 'gnus-group
    (keyamp--define-keys-remap
     gnus-group-mode-map
     '((newline                     . gnus-group-enter-server-mode)
       (undo                        . gnus-group-get-new-news)
       (beginning-of-line-or-block  . gnus-topic-goto-previous-topic)
       (end-of-line-or-block        . gnus-topic-goto-next-topic))))

  (with-eval-after-load 'gnus-sum
    (keyamp--define-keys gnus-summary-mode-map '(("C-h" . gnus-summary-delete-article)))
    (keyamp--define-keys-remap
     gnus-summary-mode-map
     '((keyamp-insert-mode-activate . gnus-summary-scroll-up)
       (open-file-at-cursor         . keyamp-insert-mode-activate)
       (cut-bracket-or-delete       . news))))

  (with-eval-after-load 'gnus-srvr
    (keyamp--define-keys-remap
     gnus-server-mode-map
     '((keyamp-insert-mode-activate . gnus-server-read-server)
       (open-file-at-cursor         . keyamp-insert-mode-activate)
       (cut-bracket-or-delete       . gnus-server-exit)))
    (keyamp--define-keys-remap
     gnus-browse-mode-map
     '((keyamp-insert-mode-activate . gnus-browse-select-group)
       (open-file-at-cursor         . keyamp-insert-mode-activate))))

  (with-eval-after-load 'gnus-art
    (keyamp--define-keys-remap gnus-mime-button-map '((keyamp-insert-mode-activate . gnus-article-press-button))))

  (with-eval-after-load 'gnus-topic
    (keyamp--define-keys-remap
     gnus-topic-mode-map
     '((keyamp-insert-mode-activate . gnus-topic-select-group)))))

(with-eval-after-load 'snake
  (keyamp--define-keys-remap
   snake-mode-map
   '((keyamp-insert-mode-activate . snake-start-game)
     (shrink-whitespaces          . snake-pause-game)
     (reformat-lines              . snake-end-game)
     (next-line                   . snake-move-down)
     (cut-bracket-or-delete       . snake-move-down)
     (backward-char               . snake-move-left)
     (forward-char                . snake-move-right)
     (previous-line               . snake-move-up)
     (undo                        . snake-move-up))))

(with-eval-after-load 'tetris
  (keyamp--define-keys-remap
   tetris-mode-map
   '((keyamp-insert-mode-activate . tetris-start-game)
     (newline                     . tetris-move-bottom)
     (next-user-buffer            . tetris-move-bottom)
     (undo                        . tetris-pause-game)
     (undo                        . tetris-pause-game)
     (reformat-lines              . tetris-end-game)
     (next-line                   . tetris-move-down)
     (backward-char               . tetris-move-left)
     (forward-char                . tetris-move-right)
     (previous-line               . tetris-rotate-prev))))

(with-eval-after-load 'emms-playlist-mode
  (keyamp--define-keys-remap
   emms-playlist-mode-map
   '((keyamp-insert-mode-activate . emms-playlist-mode-play-smart))))

(with-eval-after-load 'nov
  (keyamp--define-keys-remap
   nov-mode-map
   '((undo                        . nov-goto-toc)
     (open-line                   . nov-previous-document)
     (newline                     . nov-next-document)
     (keyamp-insert-mode-activate . nov-browse-url))))



(setq keyamp-ampable-commands-hash
      #s(hash-table
         size 80
         test equal
         data (agenda                                    t
               alternate-buffer                          t
               backward-kill-word                        t
               backward-left-bracket                     t
               backward-punct                            t
               backward-word                             t
               beginning-of-line-or-block                t
               beginning-of-line-or-buffer               t
               comint-previous-matching-input-from-input t
               comint-next-matching-input-from-input     t
               copy-line-or-selection                    t
               cycle-hyphen-lowline-space                t
               delete-other-windows                      t
               delete-window                             t
               dired-jump                                t
               downloads                                 t
               end-of-line-or-block                      t
               end-of-line-or-buffer                     t
               eshell-previous-matching-input-from-input t
               eshell-next-matching-input-from-input     t
               exchange-point-and-mark                   t
               extend-selection                          t
               forward-punct                             t
               forward-right-bracket                     t
               forward-word                              t
               goto-matching-bracket                     t
               kill-region                               t
               ibuffer-backward-filter-group             t
               ibuffer-do-delete                         t
               ibuffer-forward-filter-group              t
               ibuffer-next-line                         t
               ibuffer-previous-line                     t
               ibuffer-toggle-filter-group               t
               Info-backward-node                        t
               Info-forward-node                         t
               insert-date                               t
               insert-space-before                       t
               isearch-repeat-backward                   t
               isearch-repeat-forward                    t
               isearch-ring-advance                      t
               isearch-ring-retreat                      t
               isearch-yank-kill                         t
               kill-word                                 t
               move-row-down                             t
               move-row-up                               t
               next-line-or-history-element              t
               next-user-buffer                          t
               org-shiftdown                             t
               org-shiftup                               t
               player                                    t
               pop-local-mark-ring                       t
               previous-line-or-history-element          t
               previous-user-buffer                      t
               recenter-top-bottom                       t
               save-close-current-buffer                 t
               scroll-down-command                       t
               scroll-up-command                         t
               search-current-word                       t
               select-line                               t
               select-text-in-quote                      t
               set-mark-command                          t
               shrink-whitespaces                        t
               split-window-below                        t
               sun-moon                                  t
               tasks                                     t
               term-send-up                              t
               term-send-down                            t
               todo                                      t
               toggle-comment                            t
               toggle-eshell                             t
               toggle-letter-case                        t
               view-echo-area-messages                   t
               works                                     t)))



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
    (add-hook 'minibuffer-setup-hook   'keyamp-command-mode-activate)
    (add-hook 'minibuffer-exit-hook    'keyamp-command-mode-activate)
    (add-hook 'isearch-mode-end-hook   'keyamp-command-mode-activate)
    (add-hook 'eshell-pre-command-hook 'keyamp-command-mode-activate)
    (add-hook 'post-command-hook       'keyamp-ampable-mode-indicate)
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
