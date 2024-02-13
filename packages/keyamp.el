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

;; Keyamp provides 4 modes: insert, command, repeat, screen. Command
;; mode based on persistent transient keymap.

;; Repeat mode pushes transient remaps to keymap stack on top of
;; command mode for easy repeat of command chains during screen
;; positioning, cursor move and editing. Point color indicates
;; transient remap is active. ESDF and IJKL are mostly used, DEL/ESC
;; and RET/SPC control EVERYTHING. Home row and thumb cluster only.
;; Screen mode is similar to repeat with separate color indicator.

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
  "Karabiner-Elements CLI executable. Optional for mode sync.")

(defconst keyamp-command-indicator "🟢" "Command mode.")
(defconst keyamp-insert-indicator  "🟠" "Insert mode.")
(defconst keyamp-repeat-indicator  "🔵" "Repeat mode.")
(defconst keyamp-screen-indicator  "🟣" "Screen mode.")

(defconst keyamp-command-cursor "LawnGreen"      "Color command.")
(defconst keyamp-insert-cursor  "Gold"           "Color insert.")
(defconst keyamp-repeat-cursor  "DeepSkyBlue"    "Color repeat.")
(defconst keyamp-screen-cursor  "LightSlateBlue" "Color screen.")

(defconst keyamp-idle-timeout (* 3 60) "Idle timeout.")



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

    ("h" . beg-of-line-or-block)         ("р" . beg-of-line-or-block)             ("H"  . ignore)                          ("Р" . ignore)
    ("j" . backward-char)                ("о" . backward-char)                    ("J"  . isearch-cur-word-backward)       ("О" . isearch-cur-word-backward)
    ("k" . next-line)                    ("л" . next-line)                        ("K"  . end-of-line-or-block)            ("Л" . end-of-line-or-block)
    ("l" . forward-char)                 ("д" . forward-char)                     ("L"  . isearch-cur-word-forward)        ("Д" . isearch-cur-word-forward)
    (";" . end-of-line-or-block)         ("ж" . end-of-line-or-block)             (":"  . ignore)                          ("Ж" . ignore)
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

    ("h" . View-scroll-half-page-backward)
    ("j" . toggle-truncate-lines)
    ("k" . make-backup-and-save)
    ("l" . display-line-numbers-mode)
    (";" . View-scroll-half-page-forward)
    ("'" . sync)

    ("n" . save-buffer)
    ("m" . dired-jump)
    ("," . save-close-current-buffer)
    ("." . recenter-top-bottom)
    ("/" . mark-defun) ("*" . mark-defun)

    ("e ESC" . ignore)                   ("e <escape>" . ignore)
    ("d ESC" . ignore)                   ("d <escape>" . ignore)
    ("f ESC" . ignore)                   ("f <escape>" . ignore)))


;; Core Remaps

;; Hold down ESC (karabiner) to post C-h and call `help-map'.
;; See `keyamp-hold-indicate'.
(keyamp--map-leaders help-map '(lookup-word-definition . lookup-google-translate))
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
  '(("C-r" . open-file-at-cursor) ; hold down RET to post C-r (karabiner)
    ("C-t" . hippie-expand)       ; hold down RET in insert mode
    ("<f13>" . ignore)            ; Kinesis advantage special key
    ("<double-mouse-1>" . extend-selection)
    ("<down-mouse-1>"   . keyamp-command)          ; left click for command mode
    ("<mouse-3>"        . paste-or-paste-previous) ; paste with right click

    ("<header-line> <mouse-1>" . prev-frame)
    ("<header-line> <mouse-3>" . make-frame-command)
    ("<left-fringe> <mouse-1>" . ignore)))

(advice-add 'mouse-set-point :before
            (lambda (&rest r) "save point and copy selection with left click"
              (push-mark (point) t)
              (if (region-active-p)
                  (copy-region-as-kill (region-beginning) (region-end)))))

;; Avoid karabiner sync mode lag. Hack.
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


;; Screen mode.

(with-sparse-keymap-x
 ;; Leader layer to become transient main. Base map for next leaders adjustment
 ;; by transient maps which might be set by following target commands subsets.
 (keyamp--map-leaders x '(open-line . newline))
 (keyamp--map x
   '(("TAB" . toggle-ibuffer) ("<tab>" . toggle-ibuffer)
     ("C-h" . delete-window)  ("C-r"   . delete-other-windows)))

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
     split-window-below   alternate-buffer
     open-last-closed     save-close-current-buffer)))

(with-sparse-keymap-x
 ;; Hit RET to hide split window.
 (keyamp--remap x '((keyamp-insert . delete-other-windows)))
 (keyamp--set-map x '(split-window-below)))

(with-sparse-keymap-x
 (keyamp--remap x
   '((open-line . prev-proj-buffer) (newline . next-proj-buffer)
     (delete-backward . main-proj-buffer)))
 (keyamp--set-map x '(prev-proj-buffer next-proj-buffer main-proj-buffer)))

(with-sparse-keymap-x
 (keyamp--map x
   '(("TAB"   . View-scroll-half-page-forward)
     ("<tab>" . View-scroll-half-page-forward)))
 (keyamp--remap x
   '((open-line       . prev-eww-buffer) (newline         . next-eww-buffer)
     (delete-backward . eww-reload)      (keyamp-insert   . eww-reload)
     (undo            . eww)))
 (keyamp--set-map x '(prev-eww-buffer next-eww-buffer)))

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
 ;; When a lot of frames hold down [ to activate frame selection with DEL/SPC.
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
  (keyamp--set-map x '(dired-jump dired-find-file downloads player)))


;; Repeat mode. View commands.

(with-sparse-keymap-x
 ;; Initiate by triple DEL/SPC (hold down). Transition to Screen mode.
 ;; I/K or DEL/SPC to move by lines. See deactivate-mark-before-move.
 ;; This keymap related both to Screen and to Repeat modes.
 (keyamp--map-leaders x '(previous-line . next-line))
 (keyamp--map x
   '(("TAB" . View-scroll-half-page-forward)
     ("<tab>" . View-scroll-half-page-forward)))
 (keyamp--remap x
   '((previous-line . up-line)              (next-line . down-line)
     (open-line     . prev-user-buffer)     (newline   . next-user-buffer)))
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
     beg-of-line-or-buffer end-of-line-or-buffer))

 ;; Hit sticky shift then hold down I/K to move by blocks.
 (keyamp--map x
   '(("I" . previous-line) ("Ш" . previous-line)
     ("K" . next-line)     ("Л" . next-line))))

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

(with-sparse-keymap-x
 ;; Double SPC to run `extend-selection', then next SPC press to
 ;; deactivate mark and run `down-line'. That is, hold down SPC to start move
 ;; down lines with SPC while DEL does up lines. Similarly for `select-line'
 ;; and move by blocks. Hack.
 (keyamp--map-leaders x '(up-line . down-line))
 (keyamp--set-map x '(extend-selection)))

(with-sparse-keymap-x
 ;; Triple DEL (hold down) to move lines up and activate Screen mode.
 (keyamp--map-leaders x '(up-line . down-line))
 (keyamp--remap x
   '((previous-line . beg-of-line-or-block) (next-line . select-block)))
 (keyamp--set-map x '(select-block)))

(with-sparse-keymap-x
 ;; SPC DEL DEL to call `beg-of-line-or-block'. Hold down DEL to repeat.
 (keyamp--map-leaders x '(beg-of-line-or-block . end-of-line-or-block))
 (keyamp--remap x '((next-line . select-line)))
 (keyamp--set-map x '(select-line)))

;; If region active deactivate mark conditionally and return to the line
;; before selection.
(advice-add-macro
 '(ibuffer-backward-filter-group   ibuffer-forward-filter-group
   gnus-topic-goto-prev-topic-line gnus-topic-goto-next-topic-line
   up-line                         down-line
   beg-of-line-or-block            end-of-line-or-block)
 :before 'deactivate-mark-before-move)

(with-sparse-keymap-x
 ;; DEL SPC SPC to call `end-of-line-or-block'. Hold down SPC to repeat.
 (keyamp--map-leaders x '(beg-of-line-or-block . end-of-line-or-block))
 (keyamp--remap x '((next-line . select-text-in-quote)))
 (keyamp--set-map x '(select-text-in-quote)))

(with-sparse-keymap-x
 ;; Hold down comma to call `save-close-current-buffer'. Then comma to repeat.
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
 (keyamp--set-map x '(View-scroll-half-page-backward
                      View-scroll-half-page-forward)))

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


;; Repeat mode. Edit commands.

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
 (keyamp--map-leaders x '(delete-backward . insert-space-before))
 (keyamp--map x '(("v" . delete-backward) ("м" . delete-backward))) ; repeat in insert mode
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
 (keyamp--map-leaders x '(agenda . todo))
 (keyamp--remap x '((undo . todo) (copy-line-or-selection . agenda)))
 (keyamp--set-map x '(todo insert-date)))

(with-sparse-keymap-x
 (keyamp--remap x '((delete-backward . cycle-hyphen-lowline-space)))
 (keyamp--set-map x '(cycle-hyphen-lowline-space)))


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
       ("N" . keyamp-insert-!)  ("Т" . keyamp-insert-!))) ; Engineer Engram layout ! is QWERTY N and Russian Т
   (keyamp--remap y-or-n-p-map
     '((select-block     . y-or-n-p-insert-n)
       (extend-selection . y-or-n-p-insert-y)))

   ;; The hook is last one run during minibuffer setup. Transient keymap x
   ;; gets highest priority.
   (keyamp--remap x
     '((end-of-line-or-block . keyamp-insert-n) ; Engineer Engram layout,
       (backward-kill-word   . keyamp-insert-y) ; remap required for others
       (keyamp-insert        . keyamp-minibuffer-insert)
       (keyamp-escape        . keyamp-minibuffer-escape)))
    (keyamp--set-map-hook x '(minibuffer-setup-hook) :command nil :repeat))

   ;; Right after paste in minibuffer mostly confirm and exit follow.
   (advice-add 'paste-or-paste-previous :after
               (lambda (&rest r) "activate insert mode if in minibuffer"
                 (when (and (minibufferp)
                            (not keyamp-insert-p))
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
    '(("C-h" . dired-do-delete) ("C-r" . delete-other-windows)
      ("<mouse-1>" . mouse-set-point)
      ("<mouse-2>" . mouse-set-point) ; mouse-2 really mouse-1
      ("<double-mouse-1>" . dired-find-file)
      ("<mouse-3>" . open-in-external-app)))

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
    (keyamp--set-map x '(dired-toggle-mark))))

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
            (lambda (&rest r)
              (if (= (line-number-at-pos) (1+ (count-lines (point-min) (point-max))))
                  (dired-previous-line 1))))

(advice-add 'dired-previous-line :after
            (lambda (&rest r) (if (= (line-number-at-pos) 1) (dired-next-line 1))))

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
     '((previous-line        . ibuffer-backward-filter-group)
       (next-line            . ibuffer-forward-filter-group)
       (undo                 . ibuffer-backward-filter-group)
       (delete-backward      . ibuffer-forward-filter-group)
       (beg-of-line-or-block . beg-of-line-or-buffer)
       (end-of-line-or-block . end-of-line-or-buffer)))
   (keyamp--set-map x
     '(ibuffer-backward-filter-group ibuffer-forward-filter-group))))

(with-eval-after-load 'ibuffer
  (keyamp--map ibuffer-name-map '(("<mouse-1>" . mouse-set-point))))

(with-sparse-keymap-x
 (keyamp--map-leaders x '(delete-backward . delete-backward))
 (keyamp--remap x '((delete-backward . ibuffer-do-delete)))
 (keyamp--set-map x '(ibuffer-do-delete)))

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
      (insert-space-before))
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
    '((open-line       . eww-back-url) (newline . eww-next-url)
      (delete-backward . eww-reload)   (kill-word . eww-reload-all)
      (keyamp-insert   . eww-reload)   (undo            . eww)
      (shrink-whitespaces . eww-browse-with-external-browser)))

  (keyamp--remap eww-link-keymap
    '((keyamp-insert . eww-follow-link))))

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
     '(flyspell-buffer
       flyspell-goto-prev-error
       flyspell-goto-next-error
       ispell-word))))

(with-eval-after-load 'doc-view
  (keyamp--map doc-view-mode-map
    '(("C-r" . delete-other-windows)))
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

  (keyamp--map eshell-mode-map
    '(("C-h" . eshell-interrupt-process)
      ("TAB" . eshell-leader-map) ("<tab>" . eshell-leader-map)
      ("S-<tab>" . ignore)        ("<backtab>" . ignore)))
  (keyamp--map (define-prefix-command 'eshell-leader-map)
    '(("TAB" . completion-at-point) ("<tab>" . completion-at-point)))

  (keyamp--remap eshell-mode-map
    '((cut-line-or-selection . eshell-clear-input)
      (cut-text-block        . eshell-clear)
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

;; Command mode when jump from insert. The commands might be run
;; by hold down a key or transient keymap from insert mode, mostly eshell.
(advice-add-macro
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
    '((undo             . backward-button)
      (delete-backward  . forward-button)))

(with-eval-after-load 'gnus-sum
  (keyamp--map gnus-summary-mode-map
    '(("C-h"              . gnus-summary-delete-article)
      ("<double-mouse-1>" . gnus-summary-scroll-up)))

  (keyamp--remap gnus-summary-mode-map
    '((keyamp-insert    . gnus-summary-scroll-up)
      (open-line        . gnus-summary-prev-group)
      (newline          . gnus-summary-next-group)
      (save-buffer      . gnus-summary-save-parts))))

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
    '((keyamp-escape        . tetris-pause-game)
      (delete-backward      . tetris-rotate-prev) (delete-other-windows . tetris-rotate-prev)
      (newline              . tetris-rotate-next) (next-user-buffer     . tetris-rotate-next)
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
 (keyamp--set-map x '(flymake-goto-prev-error flymake-goto-next-error)))

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
      (eval-defun            . test)
      (eval-last-sexp        . server))))

(with-sparse-keymap-x
   (keyamp--map-leaders x '(xref-go-back . xref-find-definitions))
   (keyamp--set-map x '(xref-go-back xref-find-definitions)))

(with-eval-after-load 'sh-script
  (keyamp--map bash-ts-mode-map
    '(("TAB" . bash-ts-mode-leader-map) ("<tab>" . bash-ts-mode-leader-map)
      ("S-<tab>" . ignore)              ("<backtab>" . ignore)))
  (keyamp--map (define-prefix-command 'bash-ts-mode-leader-map)
    '(("TAB" . indent-for-tab-command) ("<tab>" . indent-for-tab-command))))

(with-eval-after-load 'sqlite-mode
  (keyamp--remap sqlite-mode-map
    '((keyamp-insert   . sqlite-mode-list-data)
      (delete-backward . sqlite-mode-delete)
      (newline         . sqlite-mode-list-columns)
      (open-line       . sqlite-mode-list-tables))))

(with-eval-after-load 'sql
  (keyamp--remap sql-mode-map '((eval-defun . exec-query))))



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
                 main-proj-buffer                 t
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



(defvar keyamp-insert-p t   "Non-nil insert is on.")
(defvar keyamp-repeat-p nil "Non-nil means repeat is on.")
(defvar keyamp-screen-p nil "Non-nil means screen is on.")

(defvar keyamp--deactivate-command-mode-func nil)

(defun keyamp-command-init ()
  "Set command mode."
  (when keyamp-insert-p
    (setq keyamp-insert-p nil)
    (push-mark (point) t))
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
    (setq mode-line-front-space keyamp-screen-indicator)
    (set-face-background 'cursor keyamp-screen-cursor)
    (setq keyamp-repeat-p nil)
    (setq keyamp-screen-p t))
   ((or (eq real-this-command 'repeat)
        (and (gethash this-command keyamp-repeat-commands-hash)
             (not keyamp-insert-p)))
    (setq mode-line-front-space keyamp-repeat-indicator)
    (set-face-background 'cursor keyamp-repeat-cursor)
    (setq keyamp-repeat-p t)
    (setq keyamp-screen-p nil))
   ((or (gethash this-command keyamp-edit-commands-hash)
        keyamp-insert-p)
    (setq mode-line-front-space keyamp-insert-indicator)
    (set-face-background 'cursor keyamp-insert-cursor))
   (t
    (setq mode-line-front-space keyamp-command-indicator)
    (set-face-background 'cursor keyamp-command-cursor)
    (setq keyamp-repeat-p nil)
    (setq keyamp-screen-p nil)))
  (unless (eq this-command last-command)
    (force-mode-line-update t)))

(defun keyamp-hold-indicate ()
  "Indicate prefix key hold down. Run after idle time same as Karabiner
held down threshold. E.g. hold down ESC Karabiner posts C-h in 0.2 s and
Keyamp indicates the event after idle 0.2 s."
  (cond
   ((equal [8] (this-single-command-keys)) ; C-h
    (setq mode-line-front-space keyamp-screen-indicator)
    (set-face-background 'cursor keyamp-screen-cursor)
    (force-mode-line-update t))))

(defun keyamp-escape (&optional Keyamp-idle-p)
  "Return to command mode, clear selection or quit minibuffer.
If run by idle timer then emulate escape keyboard press which required to
deactivate transient map."
  (interactive)
  (cond
   (Keyamp-idle-p        (execute-kbd-macro (kbd "<escape>")))
   ((or keyamp-insert-p
        keyamp-repeat-p
        keyamp-screen-p) (keyamp-command))
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
    (add-function :after after-focus-change-function #'keyamp-command)

    (keyamp-catch-tty-ESC)
    (keyamp-command)
    (run-with-timer 5 nil 'keyamp-map-input-source 'russian-computer)
    (run-with-timer 5 nil 'keyamp-push-quail-keyboard-layout)
    (setq keyamp-idle-timer
          (run-with-idle-timer keyamp-idle-timeout t 'keyamp-escape t))
    (setq keyamp-indicate-timer
          (run-with-idle-timer 0.2 t 'keyamp-hold-indicate))))

(provide 'keyamp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical)
;; End:
;;; keyamp.el ends here
