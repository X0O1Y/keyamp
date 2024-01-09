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

;; Keyamp provides 3 modes: insert, command and repeat. Command mode
;; based on persistent transient keymap.

;; Repeat mode pushes transient remaps to keymap stack on top of
;; command mode for easy repeat of command chains during screen
;; positioning, cursor move and editing. Point color indicates
;; transient remap is active. ESDF and IJKL are mostly used, DEL/ESC
;; and RET/SPC control EVERYTHING. Home row and thumb cluster only.

;; DEL and SPC are two leader keys, RET activates insert mode, ESC does
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

;; This package is a fork of xah-fly-keys.

;;; Code:



(require 'keyext)
(require 'quail)

(defgroup keyamp nil "Customization options for keyamp"
  :group 'help :prefix "keyamp-")

(defvar keyamp-command-hook nil "Hook for `keyamp-command'")
(defvar keyamp-insert-hook  nil "Hook for `keyamp-insert'")

(defconst keyamp-karabiner-cli
  "/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli"
  "Karabiner-Elements CLI executable")

(defconst keyamp-command-indicator "🟢" "Character indicating command is active.")
(defconst keyamp-insert-indicator  "🟠" "Character indicating insert is active.")
(defconst keyamp-repeat-indicator  "🔵" "Character indicating repeat is active.")

(defconst keyamp-command-cursor "lawngreen"   "Cursor color command.")
(defconst keyamp-insert-cursor  "gold"        "Cursor color insert.")
(defconst keyamp-repeat-cursor  "deepskyblue" "Cursor color repeat.")

(defconst keyamp-idle-timeout 60 "Idle timeout.")



(defvar keyamp-layouts nil "A alist. Key is layout name, string type.
Value is a alist, each element is of the form (\"e\" . \"d\").
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

(defmacro keyamp--map-translation (KeyKeyAlist State-p)
  "Map `keymap-set' for `key-translation-map' over a alist KEYKEYALIST.
If State-p is nil, remove the mapping."
  `(let ()
     ,@(mapcar
        (lambda (xpair)
          `(keymap-set key-translation-map ,(car xpair) (if ,State-p ,(cdr xpair))))
        (cadr KeyKeyAlist))))

(defmacro keyamp--remap (KeymapName CmdCmdAlist)
 "Map `keymap-set' remap over a alist CMDCMDALIST."
  (let ((xkeymapName (make-symbol "keymap-name")))
   `(let ((,xkeymapName ,KeymapName))
      ,@(mapcar
         (lambda (xpair)
           `(keymap-set
             ,xkeymapName ,(concat "<remap> <" (format "%s" (car xpair)) ">") ,(list 'quote (cdr xpair))))
         (cadr CmdCmdAlist)))))

(defmacro keyamp--set-map (KeymapName CmdList &optional CommandMode InsertMode How)
  "Map `set-transient-map' using `advice-add' over a list CMDLIST.
Advice default HOW :after might be changed by specific HOW.
Activate command or insert mode optionally."
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       ,@(mapcar
          (lambda (xcmd)
            `(advice-add ,(list 'quote xcmd) (if ,How ,How :after)
                         (lambda (&rest r) "repeat"
                           (if (and ,CommandMode keyamp-insert-p) (keyamp-command))
                           (set-transient-map ,xkeymapName)
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
                         (if (and ,CommandMode keyamp-insert-p) (keyamp-command))
                         (if (and ,InsertMode (not keyamp-insert-p)) (keyamp-insert))
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

(defun keyamp-quail-get-translation (Xfrom)
  "Get translation Engineer Engram to russian-computer.
From character to character code."
  (let ((xto (alist-get Xfrom keyamp-engineer-engram-to-russian-computer
             nil nil 'string-equal)))
    (when (stringp xto)
      (string-to-char xto))))

(defun keyamp-map-input-source (input-method)
  "Build reverse mapping for `input-method'.
Use Russian input source for command mode. Respect Engineer Engram layout."
  (let ((xinput (symbol-name input-method))
        (xmods '(nil (control))))
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

(defun keyamp-qwerty-to-engineer-engram ()
  "Toggle translate QWERTY layout to Engineer Engram on Emacs level.
Useful when Engineer Engram layout not available on OS or keyboard level."
  (interactive)
  (if (get 'keyamp-qwerty-to-engineer-engram 'state)
      (progn
        (put 'keyamp-qwerty-to-engineer-engram 'state nil)
        (quail-set-keyboard-layout "standard")
        (message "QWERTY layout deactivated"))
    (progn
      (put 'keyamp-qwerty-to-engineer-engram 'state t)
      (quail-set-keyboard-layout "engineer-engram")
      (message "QWERTY layout activated")))
  (let ()
    (keyamp--map-translation
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
        (">" . "F") ("?" . "P") ("1" . "7")  ("2" . "5") ("3" . "1") ("4" . "3")
        ("5" . "9") ("6" . "8") ("7" . "2")  ("8" . "0") ("9" . "4") ("0" . "6")
        ("!" . "@") ("@" . "&") ("#" . "/")  ("$" . "$") ("%" . "<") ("^" . ">")
        ("&" . "*") ("*" . "=") ("(" . "+")  (")" . "\\"))
     (get 'keyamp-qwerty-to-engineer-engram 'state))))


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
        (keymap-set input-decode-map
                    "ESC" `(menu-item "" ,esc-binding :filter keyamp-tty-ESC-filter)))))

  ;; Map terminal ESC to <escape>.
  (keymap-set key-translation-map "ESC" "<escape>"))


;; setting keys

(keyamp--map keyamp-map
  '(("<escape>" . keyamp-escape)         ("S-<escape>" . ignore)
    ("C-^" . keyamp-left-leader-map)     ("C-+" . keyamp-left-leader-map)
    ("C-_" . keyamp-right-leader-map)    ("C-И" . keyamp-right-leader-map)))

;; Single keys mapping must double in Russian here. All prefix sequences mapped
;; automatically using `keyamp-map-input-source'.
(keyamp--map keyamp-command-map
  '(("RET" . keyamp-insert)              ("<return>"    . keyamp-insert)          ("S-<return>"    . ignore)
    ("DEL" . keyamp-left-leader-map)     ("<backspace>" . keyamp-left-leader-map) ("S-<backspace>" . ignore)
    ("SPC" . keyamp-right-leader-map)                                             ("S-SPC"         . ignore)

    ;; left half
    ("`" . delete-forward-char)          ("ё" . delete-forward-char)     ("~" . keyamp-qwerty-to-engineer-engram) ("Ë" . keyamp-qwerty-to-engineer-engram)
    ("1" . kmacro-record)                                                ("!" . ignore)
    ("2" . kmacro-helper)                                                ("@" . ignore)
    ("3" . kmacro-play)                                                  ("#" . ignore) ("№" . ignore)
    ("4" . append-to-register-1)                                         ("$" . ignore)
    ("5" . repeat)                                                       ("%" . ignore)

    ("q" . insert-space-before)          ("й" . insert-space-before)     ("Q" . ignore) ("Й" . ignore)
    ("w" . backward-kill-word)           ("ц" . backward-kill-word)      ("W" . ignore) ("Ц" . ignore)
    ("e" . undo)                         ("у" . undo)                    ("E" . ignore) ("У" . ignore)
    ("r" . kill-word)                    ("к" . kill-word)               ("R" . ignore) ("К" . ignore)
    ("t" . cut-text-block)               ("е" . cut-text-block)          ("T" . ignore) ("Е" . ignore)

    ("a" . shrink-whitespaces)           ("ф" . shrink-whitespaces)      ("A" . ignore) ("Ф" . ignore)
    ("s" . open-line)                    ("ы" . open-line)               ("S" . ignore) ("Ы" . ignore)
    ("d" . delete-backward)              ("в" . delete-backward)         ("D" . ignore) ("В" . ignore)
    ("f" . newline)                      ("а" . newline)                 ("F" . ignore) ("А" . ignore)
    ("g" . set-mark-command)             ("п" . set-mark-command)        ("G" . ignore) ("П" . ignore)

    ("z" . toggle-comment)               ("я" . toggle-comment)          ("Z" . ignore) ("Я" . ignore)
    ("x" . cut-line-or-selection)        ("ч" . cut-line-or-selection)   ("X" . ignore) ("Ч" . ignore)
    ("c" . copy-line-or-selection)       ("с" . copy-line-or-selection)  ("C" . ignore) ("С" . ignore)
    ("v" . paste-or-paste-previous)      ("м" . paste-or-paste-previous) ("V" . ignore) ("М" . ignore)
    ("b" . toggle-letter-case)           ("и" . toggle-letter-case)      ("B" . ignore) ("И" . ignore)

    ;; right half
    ("6" . pass)                                                         ("^" . ignore)
    ("7" . number-to-register)                                           ("&" . ignore)
    ("8" . copy-to-register)                                             ("*" . goto-matching-bracket) ; qwerty「*」→「=」engram, qwerty「/」→「=」ru pc karabiner
    ("9" . eshell)                                                       ("(" . ignore)
    ("0" . terminal)                                                     (")" . ignore)
    ("-" . tetris)                                                       ("_" . ignore)
    ("=" . goto-matching-bracket)                                        ("+" . ignore)

    ("y"  . search-current-word)         ("н" . search-current-word)     ("Y" . ignore) ("Н" . ignore)
    ("u"  . back-word)                   ("г" . back-word)               ("U" . ignore) ("Г" . ignore)
    ("i"  . previous-line)               ("ш" . previous-line)           ("I" . ignore) ("Ш" . ignore)
    ("o"  . forw-word)                   ("щ" . forw-word)               ("O" . ignore) ("Щ" . ignore)
    ("p"  . exchange-point-and-mark)     ("з" . exchange-point-and-mark) ("P" . ignore) ("З" . ignore)
    ("["  . alternate-buf-or-frame)      ("х" . alternate-buf-or-frame)  ("{" . ignore) ("Х" . ignore)
    ("]"  . write-file)                  ("ъ" . write-file)              ("}" . ignore) ("Ъ" . ignore)
    ("\\" . bookmark-set)                                                ("|" . ignore)

    ("h" . beg-of-line-or-block)         ("р" . beg-of-line-or-block)    ("H"  . ignore) ("Р" . ignore)
    ("j" . backward-char)                ("о" . backward-char)           ("J"  . ignore) ("О" . ignore)
    ("k" . next-line)                    ("л" . next-line)               ("K"  . ignore) ("Л" . ignore)
    ("l" . forward-char)                 ("д" . forward-char)            ("L"  . ignore) ("Д" . ignore)
    (";" . end-of-line-or-block)         ("ж" . end-of-line-or-block)    (":"  . ignore) ("Ж" . ignore)
    ("'" . alternate-buffer)             ("э" . alternate-buffer)        ("\"" . ignore) ("Э" . ignore)

    ("n" . isearch-forward)              ("т" . isearch-forward)         ("N" . ignore) ("Т" . ignore)
    ("m" . backward-left-bracket)        ("ь" . backward-left-bracket)   ("M" . ignore) ("Ь" . ignore)
    ("," . next-window-or-frame)         ("б" . next-window-or-frame)    ("<" . ignore) ("Б" . ignore)
    ("." . forward-right-bracket)        ("ю" . forward-right-bracket)   (">" . ignore) ("Ю" . ignore)
    ("/" . goto-matching-bracket)                                        ("?" . ignore)

    ("<left>" . left-char) ("<right>" . right-char)
    ("<up>"   . up-line)   ("<down>"  . down-line)))

(keyamp--map (define-prefix-command 'keyamp-left-leader-map)
  '(("SPC" . select-text-in-quote)
    ("DEL" . select-block)               ("<backspace>" . select-block)
    ("RET" . execute-extended-command)   ("<return>"    . execute-extended-command)
    ("TAB" . toggle-ibuffer)             ("<tab>"       . toggle-ibuffer)
    ("ESC" . ignore)                     ("<escape>"    . ignore)

    ;; left leader left half
    ("`" . find-next-dir-file)
    ("1" . periodic-chart)
    ("2" . kmacro-name-last-macro)
    ("3" . apply-macro-to-region-lines)
    ("4" . clear-register-1)
    ("5" . repeat-complex-command)

    ("q" . reformat-lines)
    ("w" . org-ctrl-c-ctrl-c)
    ("e" . split-window-below)
    ("r" . query-replace)
    ("t" . kill-line)

    ("a" . delete-window)
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
    ("6" . save-buffers-kill-terminal)
    ("7" . jump-to-register)
    ("8" . test)
    ("9" . server)
    ("0" . abbrev-mode)
    ("-" . move-to-column)
    ("=" . screenshot)
    ("y" . find-name-dired)
    ("u" . switch-to-buffer)

                                            ("i i"   . show-in-desktop)
    ("i DEL" . count-words)                 ("i SPC" . count-matches)

    ("o"  . recentf-open-files)
    ("p"  . view-echo-area-messages)
    ("["  . toggle-frame-maximized)
    ("]"  . find-file)
    ("\\" . bookmark-rename)
    ("h"  . bookmark-jump)

    ("j s" . glyphless-display-mode)        ("j l" . narrow-to-region-or-block)
    ("j d" . toggle-case-fold-search)       ("j k" . narrow-to-defun)
    ("j f" . toggle-word-wrap)              ("j j" . widen)
    ("j DEL" . hl-line-mode)                ("j SPC" . whitespace-mode)

    ("k s" . space-to-newline)
    ("k d" . delete-matching-lines)         ("k k" . list-matching-lines)
    ("k f" . delete-non-matching-lines)
    ("k r" . quote-lines)                   ("k u" . escape-quotes)
    ("k t" . delete-duplicate-lines)        ("k y" . slash-to-double-backslash)
    ("k v" . reformat-to-sentence-lines)    ("k n" . double-backslash-to-slash)
    ("k w" . sort-lines-key-value)          ("k o" . slash-to-backslash)
    ("k x" . insert-column-a-z)             ("k ." . sort-lines-block-or-region)
    ("k c" . cycle-hyphen-lowline-space)    ("k ," . sort-numeric-fields)
    ("k DEL" . ispell-word)                 ("k SPC" . flyspell-buffer)

    ("l" . describe-foo-at-point)
    (";" . bookmark-bmenu-list)
    ("'" . toggle-debug-on-error)
    ("n" . proced)
    ("m" . downloads)
    ("," . open-last-closed)
    ("." . player)
    ("/" . goto-line)

    ("i ESC" . ignore) ("i <escape>" . ignore)
    ("j ESC" . ignore) ("j <escape>" . ignore)
    ("k ESC" . ignore) ("k <escape>" . ignore)))

(keyamp--map (define-prefix-command 'keyamp-right-leader-map)
  '(("SPC" . extend-selection)
    ("DEL" . select-line)              ("<backspace>" . select-line)
    ("RET" . read-only-mode)           ("<return>"    . read-only-mode)
    ("TAB" . toggle-gnus)              ("<tab>"       . toggle-gnus)
    ("ESC" . ignore)                   ("<escape>"    . ignore)

    ;; right leader left half
    ("`" . next-buffer)
    ("1" . view-lossage)
    ("2" . insert-kbd-macro)
    ("3" . config)
    ("4" . change-bracket-pairs)
    ("5" . json-pretty)

    ("q" . fill-or-unfill)
    ("w" . sun-moon)

    ("e e" . todo)
    ("e DEL" . clock)                   ("e SPC" . calendar)

    ("r" . query-replace-regexp)
    ("t" . calc)
    ("a" . mark-whole-buffer)
    ("s" . clean-whitespace)

    ("d e" . org-shiftup)
    ("d s" . shell-command-on-region)   ("d l" . elisp-eval-region-or-buffer)
    ("d d" . insert-date)               ("d k" . run-current-file)
    ("d f" . shell-command)             ("d j" . eval-last-sexp)
    ("d DEL" . stow)                    ("d SPC" . eval-defun)

    ("f e" . insert-emacs-quote)        ("f i" . insert-ascii-single-quote)
    ("f f" . insert-char)               ("f j" . insert-brace)
    ("f d" . emoji-insert)              ("f k" . insert-paren)
    ("f s" . insert-formfeed)           ("f l" . insert-square-bracket)
    ("f g" . insert-double-angle-quote) ("f h" . insert-double-curly-quote)
    ("f DEL" . insert-backtick-quote)   ("f SPC" . insert-ascii-double-quote)

    ("g" . new-empty-buffer)
    ("z" . goto-char)
    ("x" . next-proj-buffer)
    ("c" . copy-all)
    ("v" . tasks)
    ("b" . title-case-region-or-line)

    ;; right leader right half
    ("6" . pass-generate)
    ("7" . increment-register)
    ("8" . insert-register)
    ("9" . toggle-theme)
    ("0" . org-insert-source-code)
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

    ("h" . scroll-down-command)
    ("j" . toggle-truncate-lines)
    ("k" . make-backup-and-save)
    ("l" . display-line-numbers-mode)
    (";" . scroll-up-command)
    ("'" . sync)

    ("n" . save-buffer)
    ("m" . dired-jump)
    ("," . save-close-current-buffer)
    ("." . recenter-top-bottom)
    ("/" . mark-defun) ("*" . mark-defun)

    ("e ESC" . ignore) ("e <escape>" . ignore)
    ("d ESC" . ignore) ("d <escape>" . ignore)
    ("f ESC" . ignore) ("f <escape>" . ignore)))


;; Core Remaps

;; Hold down ESC (karabiner) to post C-h and call `help-map'.
(keyamp--map-leaders help-map '(lookup-word-definition . lookup-google-translate))
(keyamp--map help-map
  '(("ESC" . ignore)           ("<escape>" . ignore)
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
  '(("C-r" . open-file-at-cursor) ; Hold down RET to post C-r (karabiner).
    ("C-t" . hippie-expand)       ; Hold down RET in insert mode.
    ("<down-mouse-1>"   . keyamp-command)
    ("<double-mouse-1>" . extend-selection)
    ("<mouse-3>"        . paste-or-paste-previous)
    ("<header-line> <mouse-1>" . prev-frame)
    ("<header-line> <mouse-3>" . make-frame-command)))

;; Avoid karabiner sync mode lag.
(keyamp--remap keyamp-command-map '((hippie-expand . open-file-at-cursor)))

;; Repeat using DEL/SPC or D. The concept widely used to form Repeat mode.
(with-sparse-keymap-x
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
  '(("<escape>"  . isearch-cancel)          ("C-^" . ignore) ; conflict
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


;; Repeat View Screen

(with-sparse-keymap-x
 ;; Leader layer to become transient main. Base map for next leaders adjustment
 ;; by transient maps which might be set by following target commands subsets.
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--map x
   '(("TAB" . toggle-ibuffer) ("<tab>" . toggle-ibuffer)
     ("C-h" . delete-window)))
 (keyamp--remap x
   '((insert-space-before     . delete-frame)
     (backward-kill-word      . sun-moon)
     (undo                    . split-window-below)
     (kill-word               . make-frame-command)
     (cut-text-block          . calc)
     (exchange-point-and-mark . view-echo-area-messages)
     (shrink-whitespaces      . delete-window)
     (delete-backward         . delete-other-windows)
     (set-mark-command        . new-empty-buffer)
     (cut-line-or-selection   . next-proj-buffer)
     (copy-line-or-selection  . agenda)
     (paste-or-paste-previous . tasks)
     (backward-left-bracket   . downloads)
     (forward-right-bracket   . player)
     (kmacro-play             . config)))
 (keyamp--set-map x
   '(prev-user-buffer     next-user-buffer
     delete-other-windows split-window-below
     alternate-buffer     delete-window
     open-last-closed     save-close-current-buffer
     prev-proj-buffer     next-proj-buffer
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
     split-window-below   alternate-buffer
     open-last-closed     save-close-current-buffer)))

(with-sparse-keymap-x
 ;; Hit RET to hide split window.
 (keyamp--remap x '((keyamp-insert . delete-other-windows)))
 (keyamp--set-map x '(split-window-below)))

(with-sparse-keymap-x
 (keyamp--remap x
   '((open-line . prev-proj-buffer) (newline . next-proj-buffer)
     (eshell    . server)))
 (keyamp--set-map x '(prev-proj-buffer next-proj-buffer)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-user-buffer) (newline . tasks)))
 (keyamp--set-map x '(tasks)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-user-buffer) (newline . config)))
 (keyamp--set-map x '(config)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--remap x '((open-line . previous-buffer) (newline . next-buffer)))
 (keyamp--set-map x '(previous-buffer next-buffer)))

(with-sparse-keymap-x
 ;; When a lot of frames hold down [ to activate.
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
  (keyamp--set-map x '(dired-jump downloads player)))


;; Repeat Edit

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-forward-char . delete-forward-char))
  (keyamp--set-map x '(delete-forward-char)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . insert-space-before))
 (keyamp--set-map x '(delete-backward insert-space-before)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--remap x '((open-line . backward-kill-word) (newline . kill-word)))
 (keyamp--set-map x '(backward-kill-word kill-word)))

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
 (keyamp--set-map x '(shrink-whitespaces)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . toggle-comment)))
 (keyamp--set-map x '(toggle-comment)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . cut-line-or-selection)))
 (keyamp--set-map x '(cut-line-or-selection)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . copy-line-or-selection)))
 (keyamp--set-map x '(copy-line-or-selection)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . paste-or-paste-previous)))
 (keyamp--set-map x '(paste-or-paste-previous)))

(with-sparse-keymap-x
 (keyamp--remap x '((delete-backward . toggle-letter-case)))
 (keyamp--set-map x '(toggle-letter-case)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . undo))
 (keyamp--remap x
   '((undo                   . org-shiftup)
     (delete-backward        . org-shiftdown)
     (copy-line-or-selection . agenda)))
 (keyamp--set-map x '(org-shiftup org-shiftdown)))

(with-sparse-keymap-x
 (keyamp--remap x '((undo . todo) (copy-line-or-selection . agenda)))
 (keyamp--set-map x '(todo insert-date)))

(with-sparse-keymap-x
 (keyamp--remap x '((delete-backward . cycle-hyphen-lowline-space)))
 (keyamp--set-map x '(cycle-hyphen-lowline-space)))


;; Repeat View

(with-sparse-keymap-x
 ;; Initiate by triple DEL/SPC (hold down). Transition to View Screen.
 ;; I/K or DEL/SPC to move by lines. See deactivate-mark-before-move.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--remap x
   '((previous-line . up-line)              (next-line . down-line)
     (open-line     . previous-user-buffer) (newline . next-user-buffer)))
 (keyamp--set-map x '(up-line down-line)))

(with-sparse-keymap-x
 ;; Initiate by SPC then double DEL or DEL then SPC hold down. Or H/; while
 ;; second press H for beginning of the buffer, third for end of the buffer.
 ;; Similarly for ; in opposite direction. I/K or DEL/SPC to move by blocks.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--remap x
   '((previous-line        . beg-of-line-or-block)
     (next-line            . end-of-line-or-block)
     (beg-of-line-or-block . beg-of-line-or-buffer)
     (end-of-line-or-block . end-of-line-or-buffer)))
 (keyamp--set-map x
   '(beg-of-line-or-block  end-of-line-or-block
     beg-of-line-or-buffer end-of-line-or-buffer)))

(with-sparse-keymap-x
 ;; Use I/K to move nodes in ibuffer and gnus. Target functions remapped
 ;; accordingly. Initiate also by SPC then double DEL or DEL then SPC hold down.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--remap x
   '((previous-line . beg-of-line-or-block)
     (next-line     . end-of-line-or-block)))
 (keyamp--set-map x
   '(ibuffer-backward-filter-group   ibuffer-forward-filter-group
     gnus-topic-goto-prev-topic-line gnus-topic-goto-next-topic-line)))

(advice-add 'ibuffer-backward-filter-group   :before 'deactivate-mark-before-move)
(advice-add 'ibuffer-forward-filter-group    :before 'deactivate-mark-before-move)
(advice-add 'gnus-topic-goto-prev-topic-line :before 'deactivate-mark-before-move)
(advice-add 'gnus-topic-goto-next-topic-line :before 'deactivate-mark-before-move)

(with-sparse-keymap-x
 ;; Double SPC to run `extend-selection', then next SPC press to
 ;; deactivate mark and run `down-line'. That is, hold down SPC to start move
 ;; down lines with SPC while DEL does up lines. Similarly for `select-line'
 ;; and move by blocks.
 (keyamp--map-leaders x '(up-line . down-line))
 (keyamp--set-map x '(extend-selection)))

(advice-add 'up-line   :before 'deactivate-mark-before-move)
(advice-add 'down-line :before 'deactivate-mark-before-move)

(with-sparse-keymap-x
 ;; Triple DEL (hold down) to move lines up and activate View Screen.
 (keyamp--map-leaders x '(up-line . down-line))
 (keyamp--remap x
   '((previous-line . beg-of-line-or-block) (next-line . select-block)))
 (keyamp--set-map x '(select-block)))

(with-sparse-keymap-x
 ;; SPC DEL DEL to call `beg-of-line-or-block'. Hold down DEL to repeat.
 (keyamp--map-leaders x '(beg-of-line-or-block . end-of-line-or-block))
 (keyamp--remap x '((next-line . select-line)))
 (keyamp--set-map x '(select-line)))

(advice-add 'beg-of-line-or-block :before 'deactivate-mark-before-move)
(advice-add 'end-of-line-or-block :before 'deactivate-mark-before-move)

(with-sparse-keymap-x
 ;; DEL SPC SPC to call `end-of-line-or-block'. Hold down SPC to repeat.
 (keyamp--map-leaders x '(beg-of-line-or-block . end-of-line-or-block))
 (keyamp--remap x '((next-line . select-text-in-quote)))
 (keyamp--set-map x '(select-text-in-quote)))

(with-sparse-keymap-x
 ;; Hold down , (comma) to call `save-close-current-buffer'. Then , to repeat.
 (keyamp--remap x '((next-window-or-frame . save-close-current-buffer)))
 (keyamp--set-map x '(save-close-current-buffer)))

(with-sparse-keymap-x
 ;; Left/right arrows repeat by DEL/SPC.
 (keyamp--map-leaders x '(backward-char . forward-char))
 (keyamp--remap x '((backward-char . left-char) (forward-char . right-char)))
 (keyamp--set-map x '(left-char right-char)))

(with-sparse-keymap-x
 ;; Repeat brackets move with DEL/SPC.
 (keyamp--map-leaders x '(backward-left-bracket . forward-right-bracket))
 (keyamp--set-map x '(backward-left-bracket forward-right-bracket)))

(with-sparse-keymap-x
 ;; Repeat move by words with J/L or DEL/SPC. Second press word move key
 ;; calls punctuation move. Press O then hold down J or U/L for
 ;; convenient word move hold down, that is, start in opposite direction.
 (keyamp--map-leaders x '(backward-char . forward-char))
 (keyamp--remap x
   '((backward-char . back-word)      (forward-char . forw-word)
     (back-word     . backward-punct) (forw-word    . forward-punct)))
 (keyamp--set-map x '(back-word forw-word backward-punct forward-punct)))

(with-sparse-keymap-x
 ;; Hold down H/; to initiate page up/down. Repeat with I/K.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--remap x
   '((previous-line . scroll-down-command) (next-line . scroll-up-command)))
 (keyamp--set-map x '(scroll-down-command scroll-up-command)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(next-line . next-line))
 (keyamp--remap x '((next-line . pop-local-mark-ring)))
 (keyamp--set-map x '(pop-local-mark-ring)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(next-line . next-line))
 (keyamp--remap x '((next-line . recenter-top-bottom)))
 (keyamp--set-map x '(recenter-top-bottom)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(text-scale-decrease . text-scale-increase))
 (keyamp--map x '(("TAB" . text-scale-reset) ("<tab>" . text-scale-reset)))
 (keyamp--set-map x '(text-scale-decrease text-scale-increase text-scale-reset)))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(backward-button . forward-button))
 (keyamp--set-map x '(backward-button forward-button)))


;; Modes Remaps

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
      (keyamp-escape)
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
       ("k" . extend-selection) ("л" . extend-selection)))
   (keyamp--remap y-or-n-p-map
     '((select-block     . y-or-n-p-insert-n)
       (extend-selection . y-or-n-p-insert-y)))

   ;; The hook is last one run during minibuffer setup.
   (keyamp--remap x
     '((end-of-line-or-block . keyamp-insert-n) ; Engineer Engram layout,
       (backward-kill-word   . keyamp-insert-y) ; remap required for other
       (keyamp-insert        . keyamp-minibuffer-insert)
       (keyamp-escape        . keyamp-minibuffer-escape)))
    (keyamp--set-map-hook x '(minibuffer-setup-hook) :command nil :repeat))

  (with-sparse-keymap-x
   ;; Right after paste in minibuffer mostly confirm and exit follow.
   ;; Repeat V to paste previous while stay in insert mode ready for RET.
   (keyamp--map x '(("v" . paste-or-paste-previous) ("м" . paste-or-paste-previous)))
   (advice-add 'paste-or-paste-previous :after
               (lambda (&rest r) "activate insert mode if in minibuffer"
                 (when (minibufferp)
                   (keyamp-insert)
                   (set-transient-map x)))))

  (keyamp--remap minibuffer-local-map
    '((previous-line . previous-line-or-history-element)
      (next-line     . next-line-or-history-element)
      (select-block  . previous-line-or-history-element)))

  (keyamp--remap minibuffer-mode-map
    '((previous-line . previous-line-or-history-element)
      (next-line     . next-line-or-history-element)
      (select-block  . previous-line-or-history-element))))

(with-eval-after-load 'icomplete
  ;; Exit if file completion. It means use content of minibuffer as it is, no
  ;; select completion candidates. Else force complete and exit, that is, select
  ;; and use first completion candidate.
  ;; In case file completion, for most cases no need to complete, because there
  ;; is NO right candidate. Otherwise, in all cases one MUST select a candidate.
  (keyamp--map icomplete-minibuffer-map
    '(("RET"      . icomplete-exit-or-force-complete-and-exit)
      ("<return>" . icomplete-exit-or-force-complete-and-exit)))

  (keyamp--remap icomplete-minibuffer-map
    '((previous-line    . icomplete-backward-completions)
      (next-line        . icomplete-forward-completions)
      (extend-selection . icomplete-forward-completions)))

  (with-sparse-keymap-x
    (keyamp--map-leaders x '(previous-line . next-line))
    (keyamp--remap x
      '((keyamp-insert . icomplete-exit-or-force-complete-and-exit)
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
  (lambda () "ido-completion-map created after ido setup only"
    (keyamp--remap ido-completion-map
      '((keyamp-insert . ido-exit-minibuffer)
        (previous-line . ido-prev-match) (select-block     . ido-prev-match)
        (next-line     . ido-next-match) (extend-selection . ido-next-match)))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(select-block . extend-selection))
 (keyamp--set-map x '(ido-prev-match ido-next-match)))

(with-eval-after-load 'dired
  (keyamp--map dired-mode-map
    '(("C-h" . dired-do-delete)       ("C-r" . open-in-external-app)
      ("<mouse-1>" . mouse-set-point) ("<mouse-2>" . mouse-set-point)
      ("<double-mouse-1>" . dired-find-file)))

  (keyamp--remap dired-mode-map
    '((keyamp-insert         . dired-find-file)
      (newline               . dired-sort)
      (backward-left-bracket . dired-mark)
      (forward-right-bracket . dired-unmark)
      (toggle-comment        . revert-buffer)
      (insert-space-before   . dired-create-directory)
      (shrink-whitespaces    . dired-hide-details-mode)
      (copy-to-register-1    . dired-do-copy)
      (paste-from-register-1 . dired-do-rename)
      (mark-whole-buffer     . dired-toggle-marks)))

  (with-sparse-keymap-x
    (keyamp--map-leaders x '(dired-unmark . dired-mark))
    (keyamp--set-map x '(dired-unmark dired-mark))))

(with-eval-after-load 'wdired
  (keyamp--map wdired-mode-map
    '(("C-h" . wdired-abort-changes) ("C-r" . wdired-finish-edit))))

(with-eval-after-load 'dired-utils
  (keyamp--map dired-mode-map
    '(("TAB" . dired-leader-map) ("<tab>" . dired-leader-map)))
  (keyamp--map dired-leader-map
    '(("ESC" . ignore)                         ("<escape>" . ignore)
      ("TAB" . dired-omit-mode)                ("<tab>"    . dired-omit-mode)
      ("q" . dired-image-remove-transparency)  ("e" . dired-optimize-png)
      ("u" . dired-2drawing)                   ("o" . dired-rotate-img-right)
      ("p" . dired-rotate-img-left)            ("a" . dired-image-autocrop)
      ("s" . dired-open-marked)                ("d" . dired-show-metadata)
      ("f" . dired-remove-all-metadata)        ("h" . dired-rotate-img-180)
      ("k" . dired-rename-space-to-underscore) ("l" . dired-2png)
      (";" . dired-scale-image)                ("\'" . dired-to-zip-encrypted)
      ("c" . dired-2jpg)                       ("/" . dired-to-zip))))

(with-eval-after-load 'rect
  ;; Sane rectangle controls.
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
    '(("C-h" . ibuffer-do-delete) ("<double-mouse-1>" . ibuffer-visit-buffer)
      ("TAB" . toggle-gnus)       ("<tab>" . toggle-gnus)))

  ;; Same as base map for View Screen, always available in ibuffer.
  (keyamp--remap ibuffer-mode-map
    '((keyamp-insert           . ibuffer-visit-buffer)
      (end-of-line-or-block    . ibuffer-forward-filter-group)
      (beg-of-line-or-block    . ibuffer-backward-filter-group)
      (insert-space-before     . delete-frame)
      (backward-kill-word      . sun-moon)
      (undo                    . split-window-below)
      (kill-word               . make-frame-command)
      (cut-text-block          . calc)
      (back-word               . switch-to-buffer)
      (forw-word               . recentf-open-files)
      (exchange-point-and-mark . view-echo-area-messages)
      (shrink-whitespaces      . delete-window)
      (open-line               . prev-user-buffer)
      (delete-backward         . delete-other-windows)
      (newline                 . next-user-buffer)
      (set-mark-command        . new-empty-buffer)
      (cut-line-or-selection   . next-proj-buffer)
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
     '((previous-line        . ibuffer-backward-filter-group)
       (next-line            . ibuffer-forward-filter-group)
       (undo                 . ibuffer-backward-filter-group)
       (delete-backward      . ibuffer-forward-filter-group)
       (beg-of-line-or-block . beg-of-line-or-buffer)
       (end-of-line-or-block . end-of-line-or-buffer)))
   (keyamp--set-map x
     '(ibuffer-backward-filter-group ibuffer-forward-filter-group
       ibuffer-toggle-filter-group))))

(with-eval-after-load 'ibuffer
  (keyamp--map ibuffer-name-map '(("<mouse-1>" . mouse-set-point))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . ibuffer-do-delete)))
 (keyamp--set-map x '(ibuffer-do-delete)))

(with-eval-after-load 'company
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
   (keyamp--set-map x
     '(company-search-abort company-complete-selection) :command)
   (keyamp--set-map x
     '(company-search-candidates) nil :insert))

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
    '((keyamp-insert . occur-mode-goto-occurrence))))

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

(with-eval-after-load 'emms
  (with-sparse-keymap-x
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
     '(flyspell-buffer
       flyspell-goto-prev-error
       flyspell-goto-next-error
       ispell-word))))

(with-eval-after-load 'doc-view
  (keyamp--remap doc-view-mode-map
    '((previous-line        . doc-view-previous-line-or-previous-page)
      (next-line            . doc-view-next-line-or-next-page)
      (backward-char        . doc-view-previous-page)
      (forward-char         . doc-view-next-page)
      (undo                 . doc-view-previous-line-or-previous-page)
      (delete-backward      . doc-view-next-line-or-next-page)
      (open-line            . doc-view-previous-page)
      (newline              . doc-view-next-page)
      (back-word            . doc-view-shrink)
      (forw-word            . doc-view-enlarge)
      (backward-char        . doc-view-scroll-down-or-previous-page)
      (end-of-line-or-block . doc-view-scroll-up-or-next-page)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
    (keyamp--remap x
      '((previous-line   . doc-view-scroll-down-or-previous-page)
        (next-line       . doc-view-scroll-up-or-next-page)
        (undo            . doc-view-scroll-down-or-previous-page)
        (delete-backward . doc-view-scroll-up-or-next-page)))
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
   (keyamp--set-map-hook x '(image-mode-hook))))

(with-eval-after-load 'esh-mode
  (advice-add 'mac-mwheel-scroll :before
              (lambda (&rest r) "activate command mode in eshell"
                (if (and (eq major-mode 'eshell-mode)
                           keyamp-insert-p)
                  (keyamp-command))))

  (keyamp--map eshell-mode-map '(("C-h" . eshell-interrupt-process)))
  (keyamp--remap eshell-mode-map
    '((cut-line-or-selection . eshell-clear-input)
      (next-proj-buffer      . eshell-clear)
      (select-block          . eshell-previous-input)
      (quoted-insert         . eshell-interrupt-process)))

  (with-sparse-keymap-x
   (keyamp--map x '(("v" . paste-or-paste-previous) ("м" . paste-or-paste-previous)))
   (advice-add 'paste-or-paste-previous :after
               (lambda (&rest r) "activate insert mode in eshell"
                 (when (eq major-mode 'eshell-mode) ; vterm no
                   (keyamp-insert)
                   (set-transient-map x)))))

  (with-sparse-keymap-x
   ;; Insert mode is primary for eshell. The keymap ready after eshell start,
   ;; command submit or cancel. Use DEL/SPC to list history, V for paste.
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . eshell-previous-input)
       (next-line     . eshell-next-input)))
   (keyamp--map x
     '(("v" . paste-or-paste-previous) ("м" . paste-or-paste-previous)
       ("'" . alternate-buffer)        ("э" . alternate-buffer)
       ("[" . alternate-buf-or-frame)  ("х" . alternate-buf-or-frame)))
    (keyamp--set-map x '(eshell-send-input eshell-interrupt-process))
    (keyamp--set-map x '(eshell-previous-input eshell-next-input) :command)
    (keyamp--set-map-hook x '(eshell-mode-hook) nil :insert)))

(with-sparse-keymap-x
 ;; Command mode when jump from insert. No keymap. The commands might be run
 ;; by hold down a key or transient keymap from insert mode, mostly eshell.
 (keyamp--set-map x
   '(alternate-buf-or-frame  alternate-buffer
     delete-other-windows    delete-window
     split-window-below      dired-jump
     prev-user-buffer        next-user-buffer
     toggle-ibuffer          save-close-current-buffer)
   :command))

(with-eval-after-load 'vterm
  (keyamp--map vterm-mode-map
    '(("C-h" . term-interrupt-subjob) ("C-q" . term-interrupt-subjob)
      ("C-r" . delete-other-windows)  ("C-t" . delete-other-windows)
      ("C-u" . vterm-send-next-key)))
  (keyamp--remap vterm-mode-map
    '((select-block            . vterm-send-up)
      (next-proj-buffer        . vterm-clear)
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
       ("[" . alternate-buf-or-frame)  ("х" . alternate-buf-or-frame)))
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
    '(("TAB" . toggle-ibuffer) ("<tab>" . toggle-ibuffer)
      ("<double-mouse-1>" . gnus-topic-select-group)))
  (keyamp--remap gnus-topic-mode-map
    '((keyamp-insert        . gnus-topic-select-group)
      (beg-of-line-or-block . gnus-topic-goto-prev-topic-line)
      (end-of-line-or-block . gnus-topic-goto-next-topic-line)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line        . gnus-topic-goto-prev-topic-line)
       (next-line            . gnus-topic-goto-next-topic-line)
       (beg-of-line-or-block . gnus-beg-of-line-or-buffer)
       (end-of-line-or-block . gnus-end-of-line-or-buffer)))
   (keyamp--set-map x
     '(gnus-topic-goto-prev-topic-line gnus-topic-goto-next-topic-line
       gnus-beg-of-line-or-buffer      gnus-end-of-line-or-buffer))))

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
    '((undo            . backward-button)
      (delete-backward . forward-button)
      (save-buffer     . gnus-summary-save-parts)
      (proced          . gnus-mime-save-part)))

(with-eval-after-load 'gnus-sum
  (keyamp--map gnus-summary-mode-map
    '(("C-h" . gnus-summary-delete-article)
      ("TAB" . ignore) ("<tab>" . ignore)
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
  ;; Remap numbers to Engineer Engram layout
  (keyamp--remap recentf-dialog-mode-map
    '((keyamp-escape        . recentf-cancel-dialog)
      (copy-to-register     . recentf-open-most-recent-file-0)
      (kmacro-play          . recentf-open-most-recent-file-1)
      (number-to-register   . recentf-open-most-recent-file-2)
      (append-to-register-1 . recentf-open-most-recent-file-3)
      (eshell               . recentf-open-most-recent-file-4)
      (repeat               . recentf-open-most-recent-file-5)
      (kmacro-helper        . recentf-open-most-recent-file-6)
      (kmacro-record        . recentf-open-most-recent-file-7)
      (pass                 . recentf-open-most-recent-file-8)
      (eshell               . recentf-open-most-recent-file-9))))

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
    '((keyamp-escape        . tetris-pause-game)
      (delete-backward      . tetris-rotate-prev)
      (delete-other-windows . tetris-rotate-prev)
      (newline              . tetris-rotate-next)
      (next-user-buffer     . tetris-rotate-next)
      (next-line            . tetris-move-bottom)
      (backward-char        . tetris-move-down)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(tetris-move-left . tetris-move-right))
   (keyamp--set-map x
     '(tetris-start-game  tetris-pause-game
       tetris-move-left   tetris-move-right
       tetris-rotate-prev tetris-rotate-next
       tetris-move-bottom tetris-move-down))))

(with-eval-after-load 'find-replace
  (keyamp--map find-output-mode-map
    '(("TAB"       . find-next-match)     ("<tab>"   . find-next-match)
      ("<backtab>" . find-previous-match) ("S-<tab>" . find-previous-match)))
  (keyamp--remap find-output-mode-map
    '((keyamp-insert . find--jump-to-place)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(backward-char . forward-char))
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
  (keyamp--map emacs-lisp-leader-map
    '(("ESC" . ignore) ("<escape>" . ignore)
      ("TAB"   . emacs-lisp-complete-or-indent)
      ("<tab>" . emacs-lisp-complete-or-indent)
      ("d" . emacs-lisp-remove-paren-pair)
      ("k" . emacs-lisp-add-paren-around-symbol)
      ("f" . emacs-lisp-compact-parens))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(flymake-goto-prev-error . flymake-goto-next-error))
 (keyamp--set-map x '(flymake-goto-prev-error flymake-goto-next-error)))

(with-eval-after-load 'python-mode
  (keyamp--map python-mode-map
    '(("TAB" . python-indent-or-complete) ("<tab>"    . python-indent-or-complete)
      ("RET" . python-return-and-indent)  ("<return>" . python-return-and-indent)
      ("S-<tab>" . python-de-indent)      ("<backtab>" . python-de-indent)))
  (keyamp--remap python-mode-map
    '((open-line      . python-return-and-indent)
      (reformat-lines . flymake-goto-next-error)
      (toggle-gnus    . python-format-buffer))))

(with-eval-after-load 'go-ts-mode
  (keyamp--map go-ts-mode-map
    '(("TAB" . go-format-buffer) ("<tab>" . go-format-buffer)))
  (keyamp--remap go-ts-mode-map
    '((describe-foo-at-point . xref-find-definitions)
      (describe-variable     . xref-find-references)
      (reformat-lines        . flymake-goto-next-error)
      (mark-defun            . go-mark-defun)))

  (with-sparse-keymap-x
   (keyamp--map-leaders x '(xref-go-back . xref-find-definitions))
   (keyamp--set-map x '(xref-go-back xref-find-definitions))))

(with-eval-after-load 'html-mode
  (keyamp--map html-mode-map
    '(("TAB" . html-leader-map)      ("<tab>" . html-leader-map)
      ("RET" . html-open-local-link) ("<return>" . html-open-local-link)
      ("C-r" . html-open-in-browser)))

  (keyamp--map html-leader-map
    '(("TAB"    . html-insert-tag)                   ("<tab>"    . html-insert-tag)
      ("RET"    . html-insert-br-tag)                ("<return>" . html-insert-br-tag)
      ("<left>" . html-prev-opening-tag)             ("<right>"  . html-next-opening-tag)
      ("<down>" . html-goto-matching-tag)            ("@"        . html-encode-ampersand-entity)
      ("$"      . html-percent-decode-url)           ("&"        . html-decode-ampersand-entity)
      ("q"      . html-make-citation)                ("w"        . nil)
      ("w ,"    . html-rename-source-file-path)

      ("w h"    . html-resize-img)                   ("w q"      . html-image-path-to-figure-tag)
      ("w j"    . html-image-to-link)                ("w w"      . html-image-to-img-tag)
      ("w c"    . html-convert-to-jpg)               ("w o"      . html-move-image-file)
      ("e"      . html-remove-tag-pair)              ("r"        . html-mark-unicode)
      ("y"      . html-lines-to-table)               ("u"        . html-emacs-to-windows-kbd-notation)
      ("i"      . html-all-urls-to-link)             ("o"        . html-insert-pre-tag)
      ("["      . html-percent-encode-url)

      ("a <up>" . html-promote-header)               ("a <down>" . html-demote-header)
      ("a e"    . html-remove-tags)                  ("a q"      . html-compact-def-list)
      ("a ."    . html-remove-list-tags)             ("a f"      . html-remove-paragraph-tags)
      ("a ,"    . html-format-to-multi-lines)        ("a l"      . html-disable-script-tag)
      ("a k"    . html-markup-ruby)                  ("a a"      . html-update-title-h1)
      ("a y"    . html-remove-table-tags)            ("a h"      . html-change-current-tag)
      ("s"      . html-html-to-text)                 ("d"        . html-select-element)
      ("f"      . html-blocks-to-paragraph)          ("h"        . html-lines-to-list)
      ("j"      . html-any-to-link)                  ("k"        . nil)

      ("k e"    . html-dehtmlize-pre-tags)           ("k h"      . html-bracket-to-markup)
      ("k j"    . html-pre-tag-to-new-file)          ("k ;"      . html-htmlize-region)
      ("k ,"    . html-rehtmlize-precode-buffer)     ("k k"      . html-toggle-syntax-color-tags)
      ("l"      . html-insert-date-section)          ("x"        . html-lines-to-def-list)
      ("c"      . html-join-tags)                    ("v"        . html-keyboard-shortcut-markup)
      ("b"      . html-make-link-defunct)

      ("m i"    . html-ampersand-chars-to-unicode)   ("m h"      . html-clone-file-in-link)
      ("m d"    . html-url-to-dated-link)            ("m w"      . html-url-to-iframe-link)
      ("m j"    . html-local-links-to-relative-path) ("m f"      . html-pdf-path-to-embed)
      ("m ,"    . html-named-entity-to-char)         ("m k"      . html-local-links-to-fullpath)
      (","      . html-extract-url)                  ("."        . html-word-to-anchor-tag)
      ("/ h"    . html-open-in-chrome)               ("/ q"      . html-open-in-firefox)
      ("/ ,"    . html-open-in-brave)                ("/ l"      . html-open-in-safari))))

(with-eval-after-load 'js-mode
  (keyamp--map js-mode-map
    '(("TAB" . js-leader-map)         ("<tab>" . js-leader-map)))
  (keyamp--map js-leader-map
    '(("TAB" . js-complete-or-indent) ("<tab>" . js-complete-or-indent)
      ("h" . typescript-compile-file)
      ("," . js-eval-line) ("." . js-eval-region)))
  (keyamp--remap js-mode-map '((toggle-gnus . js-format-buffer))))

(with-eval-after-load 'css-mode
  (keyamp--map css-mode-map
    '(("TAB" . css-leader-map) ("<tab>" . css-leader-map)))
  (keyamp--map css-leader-map
    '(("," . css-insert-random-color-hsl)
      ("TAB" . css-complete-or-indent) ("<tab>" . css-complete-or-indent)
      ("'" . css-hex-color-to-hsl)     ("a" . css-complete-symbol)
      ("h" . css-format-compact)       ("p" . css-format-compact-buffer)
      ("o" . css-format-expand-buffer) ("k" . css-format-expand)))
  (keyamp--remap css-mode-map '((open-line . css-smart-newline))))



(defconst keyamp-screen-commands-hash
  #s(hash-table test equal data
                (agenda                           t
                 alternate-buffer                 t
                 async-shell-command              t
                 delete-other-windows             t
                 find-next-dir-file               t
                 find-prev-dir-file               t
                 dired-jump                       t
                 downloads                        t
                 next-proj-buffer                 t
                 next-user-buffer                 t
                 new-empty-buffer                 t
                 player                           t
                 prev-proj-buffer                 t
                 prev-user-buffer                 t
                 save-close-current-buffer        t
                 split-window-below               t
                 sun-moon                         t
                 tasks                            t
                 view-echo-area-messages          t
                 xref-find-definitions            t
                 xref-go-back                     t)))

(defconst keyamp-edit-commands-hash
  #s(hash-table test equal data
                (delete-backward                  t
                 delete-forward-char              t
                 insert-space-before              t
                 kill-region                      t
                 shrink-whitespaces               t
                 toggle-comment                   t
                 undo                             t
                 undo-redo                        t
                 yank                             t
                 yank-pop                         t)))

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
                 ibuffer-backward-filter-group    t
                 ibuffer-forward-filter-group     t
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
                 recenter-top-bottom              t
                 right-char                       t
                 scroll-down-command              t
                 scroll-up-command                t
                 select-block                     t
                 search-current-word              t
                 select-line                      t
                 select-text-in-quote             t
                 up-line                          t
                 vterm-send-down                  t
                 vterm-send-up                    t)))



(defvar keyamp--deactivate-command-mode-func nil)
(defvar keyamp-insert-p t "Non-nil means insert is on.")
(defvar keyamp-repeat-p nil "Non-nil means repeat is on.")

(defun keyamp-command-init ()
  "Set command mode."
  (setq keyamp-insert-p nil)
  (if keyamp--deactivate-command-mode-func
      (funcall keyamp--deactivate-command-mode-func))
  (setq keyamp--deactivate-command-mode-func
        (set-transient-map keyamp-command-map (lambda () t))))

(defun keyamp-insert-init ()
  "Enter insert mode."
  (setq keyamp-insert-p t)
  (funcall keyamp--deactivate-command-mode-func))

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
    (setq mode-line-front-space keyamp-command-indicator)
    (set-face-background 'cursor keyamp-command-cursor)
    (blink-cursor-mode 1))
   ((gethash this-command keyamp-repeat-commands-hash)
    (setq keyamp-repeat-p t)
    (setq mode-line-front-space keyamp-repeat-indicator)
    (set-face-background 'cursor keyamp-repeat-cursor)
    (blink-cursor-mode 0))
   ((or (gethash this-command keyamp-edit-commands-hash)
        (eq real-this-command 'repeat) keyamp-insert-p)
    (setq mode-line-front-space keyamp-insert-indicator)
    (set-face-background 'cursor keyamp-insert-cursor)
    (if keyamp-insert-p (blink-cursor-mode 1) (blink-cursor-mode 0)))
   (t (setq keyamp-repeat-p nil)
      (setq mode-line-front-space keyamp-command-indicator)
      (set-face-background 'cursor keyamp-command-cursor)
      (blink-cursor-mode 0))))

(defun keyamp-escape (&optional Keyamp-idle-p)
  "Return to command mode or escape everything.
If run by idle timer then emulate escape keyboard press."
  (interactive)
  (cond
   (Keyamp-idle-p     (execute-kbd-macro (kbd "<escape>")))
   (keyamp-insert-p   (keyamp-command))
   (keyamp-repeat-p   (keyamp-command))
   ((region-active-p) (deactivate-mark))
   ((minibufferp)     (abort-recursive-edit))
   (t                 (keyamp-command))))



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
    (add-function :after after-focus-change-function #'keyamp-command)
    (keyamp-catch-tty-ESC)
    (keyamp-command)
    (run-with-timer 1 nil 'keyamp-map-input-source 'russian-computer)
    (run-with-timer 2 nil 'keyamp-push-quail-keyboard-layout)
    (setq keyamp-idle-timer
          (run-with-idle-timer keyamp-idle-timeout t 'keyamp-escape t))))

(provide 'keyamp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical)
;; End:
;;; keyamp.el ends here
