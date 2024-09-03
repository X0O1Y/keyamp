;;; keyamp.el --- Keyboard Amplifier -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Egor Maltsev <x0o1@ya.ru>
;; Version: 1.1 2024-07-15 Breakbeat
;;          __   _____   __
;;         |__| |_____| |__|
;;
;; DEL SPC RET keys provide IDE workflow using ordinary on-screen keyboard.
;;
;; This package is part of input model.
;; Follow the link: https://github.com/xEgorka/keyamp

;;; Commentary:

;; Keyamp provides 3 modes: insert, command and repeat.
;; Command mode based on persistent transient keymap.

;; Repeat mode pushes transient remaps to keymap stack on top of command
;; mode for easy repeat of command chains during screen positioning,
;; cursor move and editing. Cursor shape indicates active transient
;; keymap. Repeat mode turned on/off automatically either by post command
;; hook or with idle timer.

;; DEL and SPC are two leader keys, RET activates insert mode, ESC does
;; command one. Holding down each of the keys posts control sequence
;; depending on mode. Keyboard has symmetric layout: left side for
;; editing and NO while right side for moving and YES. Any Emacs major
;; or minor mode could be remapped to fit the model, see the package.

;; Karabiner integration allows to post control or leader sequences by
;; holding down a key. No need to have any modifier or arrows keys at
;; all. Holding down posts leader layer. The same symmetric layout might
;; be configured on ANSI keyboard, ergonomic split and virtual keyboards.
;; See the link for layouts and karabiner config.

;; This package is a fork of xah-fly-keys.

;;; Code:



(require 'commands)


;; Macros

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
    (">" . "F") ("?" . "P") ("1" . "1")  ("2" . "2") ("3" . "3") ("4" . "4")
    ("5" . "5") ("6" . "6") ("7" . "7")  ("8" . "8") ("9" . "9") ("0" . "0")
    ("!" . "@") ("@" . "&") ("#" . "/")  ("$" . "$") ("%" . "<") ("^" . ">")
    ("&" . "*") ("*" . "=") ("(" . "+")  (")" . "\\"))) keyamp-layouts)

(defvar keyamp-current-layout "engineer-engram"
  "The current keyboard layout. Value is a key in `keyamp-layouts'.")

(defvar keyamp--convert-table (cdr (assoc keyamp-current-layout keyamp-layouts))
  "A alist that's the conversion table from QWERTY to current layout.
Value structure is one of the key's value of `keyamp-layouts'.
Value is programmatically set from value of `keyamp-current-layout'.
Do not manually set this variable.")

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
  (declare (indent defun))
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
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
   `(let ((,xkeymapName ,KeymapName))
      ,@(mapcar
         (lambda (xpair)
           `(keymap-set ,xkeymapName
                        ,(concat "<remap> <" (format "%s" (car xpair)) ">")
                        ,(list 'quote (cdr xpair))))
         (cadr CmdCmdAlist)))))

(defmacro keyamp--set-map
    (KeymapName CmdList &optional CommandMode InsertMode How TimeOut)
  "Map `set-transient-map' using `advice-add' over a list CMDLIST.
- Advice default HOW :after might be changed by specific HOW;
- Activate COMMANDMODE or INSERTMODE mode optionally;
- Deactivate repeat mode after idle for TIMEOUT seconds."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       ,@(mapcar
          (lambda (xcmd)
            `(advice-add ,(list 'quote xcmd) (if ,How ,How :after)
                         (lambda (&rest r) "auto repeat"
                           (when (or (eq real-this-command 'repeat)
                                     (eq this-command 'kill-region) ; exception
                                     (eq this-command 'undo)        ; exception
                                     (eq this-command ,(list 'quote xcmd)))
                             (if (and ,CommandMode keyamp-insert-p)
                                 (keyamp-command))
                             (keyamp-set-deactivate-repeat-fun ,xkeymapName)
                             (keyamp-cancel-repeat-idle-timer)
                             (if (and ,TimeOut (not keyamp-insert-p))
                                 (setq keyamp--repeat-idle-timer
                                       (run-with-idle-timer
                                        ,TimeOut nil 'keyamp-escape)))
                             (if ,InsertMode (keyamp-insert))))))
          (cadr CmdList)))))

(defmacro keyamp--set-map-hook
    (KeymapName HookList &optional CommandMode InsertMode RepeatMode)
  "Map `set-transient-map' using `add-hook' over a list HOOKLIST.
Activate command, insert or repeat mode optionally."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       ,@(mapcar
          (lambda (xhook)
            `(add-hook ,(list 'quote xhook)
                       (lambda () "auto repeat"
                         (if (and ,CommandMode keyamp-insert-p) (keyamp-command))
                         (if (and ,InsertMode (not keyamp-insert-p))
                             (keyamp-insert))
                         (keyamp-set-deactivate-repeat-fun ,xkeymapName)
                         (if ,RepeatMode
                             (keyamp-command-execute 'keyamp--repeat-dummy)))))
          (cadr HookList)))))

(defun keyamp--repeat-dummy () (interactive))

(defun keyamp-command-execute (Command)
  "Change this command to COMMAND and execute it."
  (setq this-command Command) (command-execute Command))

(defmacro keyamp--map-leader (KeymapName CmdCons)
  "Map leader keys using `keyamp--map'."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (if (display-graphic-p)
           (keyamp--map ,xkeymapName
             '(("<backspace>" . ,(car (cadr CmdCons)))
               ("SPC" . ,(cdr (cadr CmdCons)))))
         (keyamp--map ,xkeymapName
           '(("DEL" . ,(car (cadr CmdCons))) ("SPC" . ,(cdr (cadr CmdCons)))))))))

(defmacro keyamp--map-tab (KeymapName Cmd)
  "Map TAB or <tab> keys to CMD using `keyamp--map'."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (if (display-graphic-p)
           (keyamp--map ,xkeymapName '(("<tab>" . ,Cmd)))
         (keyamp--map ,xkeymapName '(("TAB" . ,Cmd)))))))

(defmacro keyamp--map-backtab (KeymapName Cmd)
  "Map S-<tab> or <backtab> keys to CMD using `keyamp--map'."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (if (display-graphic-p)
           (keyamp--map ,xkeymapName '(("S-<tab>" . ,Cmd)))
         (keyamp--map ,xkeymapName '(("<backtab>" . ,Cmd)))))))

(defmacro keyamp--map-return (KeymapName Cmd)
  "Map RET and <return> keys to CMD using `keyamp--map'."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (if (display-graphic-p)
           (keyamp--map ,xkeymapName '(("<return>" . ,Cmd)))
         (keyamp--map ,xkeymapName '(("RET" . ,Cmd)))))))

(defmacro keyamp--map-escape (KeymapName Cmd)
  "Map ESC and <escape> keys to CMD using `keyamp--map'."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (keyamp--map ,xkeymapName '(("ESC" . ,Cmd) ("<escape>" . ,Cmd))))))

(defmacro with-sparse-keymap-x (&rest body)
  "Make sparse keymap x for next use in BODY."
  `(let ((x (make-sparse-keymap))) ,@body))

(defmacro advice-add-macro (SymList How Fun)
  "Map `advice-add' HOW over a list SYMLIST to FUN."
  `(progn ,@(mapcar (lambda (xcmd) `(advice-add ,(list 'quote xcmd) ,How ,Fun))
                    (cadr SymList))))


;; Double remap

(defun keyamp-exe-remap (Key From To)
  "Remap key from FROM command to TO command by default, but if remapped
already, then use existing remap instead. Execute resulting command."
  (let ((x (keymap-lookup overriding-local-map (keyamp--convert-kbd-str Key))))
    (keyamp-command-execute (cond ((or (equal x From) (not x)) To) (x x)))))


;; Double press

(defconst keyamp-double-press-timeout 250 "Double key press timeout in ms.")
(defvar keyamp-double-press-timer nil "Double key press timer.")

(defun keyamp-double-press (Cmd)
  "Execute COMMAND after second command call during `keyamp-double-press-timeout'."
  (if (and (timerp keyamp-double-press-timer) (eq this-command last-command))
      (keyamp-command-execute Cmd))
  (setq keyamp-double-press-timer
        (run-with-timer (/ keyamp-double-press-timeout 1000.0) nil
                        (lambda () (setq keyamp-double-press-timer nil)))))

(defmacro keyamp--map-double (CmdCmdAlist)
  "Map over alist CMDCMDALIST double press of CAR CMDCONS to CDR CMDCONS."
  (declare (indent defun))
  `(progn ,@(mapcar
             (lambda (xpair)
               `(advice-add ,(list 'quote (car xpair)) :after
                            (lambda (&rest r) "double press"
                              (keyamp-double-press ,(list 'quote (cdr  xpair))))))
             (cadr CmdCmdAlist))))


;; Triple press

(defvar keyamp-defer-command-timer nil "Defer command timer.")
(defconst keyamp-key-repeat-delay (if (display-graphic-p) 30 90)
  "Key repeat delay in ms. Higher value for network access.")

(defun keyamp-defer-command (Defer Command)
  "Defer execution of COMMAND for DEFER ms."
  (setq keyamp-defer-command-timer (run-with-timer (/ Defer 1000.0) nil Command)))

(defun keyamp-cancel-defer-command-timer ()
  "Cancel `keyamp-defer-command-timer'."
  (when (timerp keyamp-defer-command-timer)
    (cancel-timer keyamp-defer-command-timer) (setq keyamp-defer-command-timer nil)))

(defun keyamp-defer-command-around (fun &rest r)
  "Run `keyamp-defer-command' as around advice."
  (if (memq last-command '(backward-char forward-char)) (before-last-command))
  (keyamp-defer-command keyamp-key-repeat-delay fun))


;; Terminal escape

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

(unless (display-graphic-p) ;; map terminal ESC to <escape>
  (keyamp-catch-tty-ESC)
  (keymap-set key-translation-map "ESC" "<escape>"))


;; Input source

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
  (let ((to (alist-get From keyamp-engineer-engram-to-russian-computer
                       nil nil 'string-equal)))
    (if (stringp to) (string-to-char to))))

(defun keyamp-map-input-source (Input-method)
  "Build reverse mapping for INPUT-METHOD.
Use Russian input source for command mode. Respect Engineer Engram layout."
  (require 'quail)
  (let ((xinput (symbol-name Input-method)) (xmods '(nil (control)))
        (message-log-max nil) (inhibit-message t))
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

(defun input-source (Source)
  "Activate input method CDR SOURCE, message CAR SOURCE."
  (activate-input-method (cdr Source)) (message "%s" (car Source)))

(defun toggle-input-source ()
  "Toggle input method."
  (interactive)
  (require 'quail)
  (if current-input-method
      (input-source '("ABC" . nil))
    (input-source '("АБВ" . russian-computer))))


;; Quail layout

(defconst quail-keyboard-layout-engineer-engram
  "\
                              \
  1@2&3/4$5<6>7*8=9+0\\#|%^`~  \
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
      (progn (put 'keyamp-qwerty-to-current-layout 'state nil)
             (quail-set-keyboard-layout "standard")
             (message "QWERTY keyboard to %s deactivated" keyamp-current-layout))
    (put 'keyamp-qwerty-to-current-layout 'state t)
    (quail-set-keyboard-layout keyamp-current-layout)
    (message "QWERTY keyboard to %s activated" keyamp-current-layout))
  (let ((xl (alist-get keyamp-current-layout keyamp-layouts nil nil 'string-equal)))
    (mapc #'(lambda (x)
              (keymap-set key-translation-map
                          (car x)
                          (if (get 'keyamp-qwerty-to-current-layout 'state) (cdr x))))
          xl)))


;; Keymaps

(defvar keyamp-map (make-sparse-keymap)
  "Parent keymap of `keyamp-command-map'.
Define keys that are available in both command and insert modes here.")

(defvar keyamp-command-map (cons 'keymap keyamp-map)
  "Keymap that takes precedence over all other keymaps in command mode.
Inherits bindings from `keyamp-map'.

  In command mode, if no binding is found in this map `keyamp-map' is
checked, then if there is still no binding, the other active keymaps
are checked like normal. However, if a key is explicitly bound to nil
in this map, it will not be looked up in `keyamp-map' and lookup will
skip directly to the normally active maps.

  In this way, bindings in `keyamp-map' can be disabled by this map.
Effectively, this map takes precedence over all others when command mode
is enabled.")

(define-prefix-command 'keyamp-lleader-map)
(define-prefix-command 'keyamp-rleader-map)

(keyamp--map keyamp-map
  '(("<escape>" . keyamp-escape)
    ;; Control sequences for leaders. Russian converted from Engineer Engram.
    ;; The sequences are prefixes for key hold down in Karabiner.
    ("C-^" . keyamp-lleader-map) ("C-+" . keyamp-lleader-map)
    ("C-_" . keyamp-rleader-map) ("C-И" . keyamp-rleader-map)

    ("S-<backspace>" . backward-char) ("S-SPC" . forward-char)))

(keyamp--map-leader keyamp-command-map '(keyamp-lleader-map . keyamp-rleader-map))
(keyamp--map-return keyamp-command-map keyamp-insert)
;; Single keys mapping must double in Russian here. All prefix sequences mapped
;; automatically using `keyamp-map-input-source'. If missing then same.
(keyamp--map keyamp-command-map
  '(;; left half
    ("`" . make-frame-command)             ("ё" . make-frame-command)               ("~" . keyamp-qwerty-to-current-layout)  ("Ë" . keyamp-qwerty-to-current-layout)
    ("1" . kmacro-record)                                                           ("!" . ignore)
    ("2" . kmacro-play)                                                             ("@" . ignore)
    ("3" . kmacro-helper)                                                           ("#" . ignore)                           ("№" . ignore)
    ("4" . append-to-r1)                                                            ("$" . ignore)
    ("5" . terminal-split)                                                                ("%" . ignore)

    ("q" . insert-space-before)            ("й" . insert-space-before)              ("Q" . ignore)                           ("Й" . ignore)
    ("w" . backward-del-word)              ("ц" . backward-del-word)                ("W" . ignore)                           ("Ц" . ignore)
    ("e" . undo)                           ("у" . undo)                             ("E" . ignore)                           ("У" . ignore)
    ("r" . del-word)                       ("к" . del-word)                         ("R" . ignore)                           ("К" . ignore)
    ("t" . cut-text-block)                 ("е" . cut-text-block)                   ("T" . ignore)                           ("Е" . ignore)

    ("a" . shrink-whitespaces)             ("ф" . shrink-whitespaces)               ("A" . ignore)                           ("Ф" . ignore)
    ("s" . open-line)                      ("ы" . open-line)                        ("S" . ignore)                           ("Ы" . ignore)
    ("d" . del-back)                       ("в" . del-back)                         ("D" . ignore)                           ("В" . ignore)
    ("f" . newline)                        ("а" . newline)                          ("F" . ignore)                           ("А" . ignore)
    ("g" . activate-region)                ("п" . activate-region)                  ("G" . ignore)                           ("П" . ignore)

    ("z" . toggle-comment)                 ("я" . toggle-comment)                   ("Z" . ignore)                           ("Я" . ignore)
    ("x" . cut-line)                       ("ч" . cut-line)                         ("X" . ignore)                           ("Ч" . ignore)
    ("c" . copy-line)                      ("с" . copy-line)                        ("C" . ignore)                           ("С" . ignore)
    ("v" . paste-or-prev)                  ("м" . paste-or-prev)                    ("V" . ignore)                           ("М" . ignore)
    ("b" . toggle-letter-case)             ("и" . toggle-letter-case)               ("B" . ignore)                           ("И" . ignore)

    ;; right half
    ("6" . pass)                                                                    ("^" . ignore)
    ("7" . jump-to-register)                                                        ("&" . ignore)
    ("8" . copy-to-register)                                                        ("*" . goto-match-br) ; QWERTY * → = Engineer Engram, QWERTY / → = RU PC Karabiner
    ("9" . proced)                                                                  ("(" . ignore)
    ("0" . eshell-split)                                                                  (")" . ignore)
    ("-" . enlarge-window)                                                          ("_" . ignore)
    ("=" . goto-match-br)                                                           ("+" . ignore)

    ("y"  . occur-cur-word)                ("н" . occur-cur-word)                   ("Y" . ignore)                           ("Н" . ignore)
    ("u"  . back-word)                     ("г" . back-word)                        ("U" . ignore)                           ("Г" . ignore)
    ("i"  . previous-line)                 ("ш" . previous-line)                    ("I" . ignore)                           ("Ш" . ignore)
    ("o"  . forw-word)                     ("щ" . forw-word)                        ("O" . ignore)                           ("Щ" . ignore)
    ("p"  . jump-mark)                     ("з" . jump-mark)                        ("P" . ignore)                           ("З" . ignore)
    ("["  . alt-buf)                       ("х" . alt-buf)                          ("{" . ignore)                           ("Х" . ignore)
    ("]"  . write-file)                    ("ъ" . write-file)                       ("}" . ignore)                           ("Ъ" . ignore)
    ("\\" . bookmark-set)                                                           ("|" . ignore)

    ("h" . beg-of-line)                    ("р" . beg-of-line)                      ("H"  . ignore)                          ("Р" . ignore)
    ("j" . backward-char)                  ("о" . backward-char)                    ("J"  . ignore)                          ("О" . ignore)
    ("k" . next-line)                      ("л" . next-line)                        ("K"  . ignore)                          ("Л" . ignore)
    ("l" . forward-char)                   ("д" . forward-char)                     ("L"  . ignore)                          ("Д" . ignore)
    (";" . end-of-lyne)                    ("ж" . end-of-lyne)                      (":"  . ignore)                          ("Ж" . ignore)
    ("'" . alternate-frame)                ("э" . alternate-frame)                  ("\"" . ignore)                          ("Э" . ignore)

    ("n" . isearch-forward)                ("т" . isearch-forward)                  ("N" . toggle-case-fold-search)          ("Т" . toggle-case-fold-search)
    ("m" . backward-bracket)               ("ь" . backward-bracket)                 ("M" . ignore)                           ("Ь" . ignore)
    ("," . other-win)                      ("б" . other-win)                        ("<" . ignore)                           ("Б" . ignore)
    ("." . forward-bracket)                ("ю" . forward-bracket)                  (">" . ignore)                           ("Ю" . ignore)
    ("/" . goto-match-br)                                                           ("?" . ignore)

    ("<left>" . back-char)                 ("<right>" . forw-char)
    ("<up>"   . up-line)                   ("<down>"  . down-line)

    ("S-<escape>"    . ignore)             ("S-<return>" . speedbar)
    ("S-<backspace>" . ignore)             ("S-SPC"      . ignore)))

(keyamp--map-leader keyamp-lleader-map '(select-block . select-quote))
(keyamp--map-return keyamp-lleader-map execute-extended-command)
(keyamp--map-escape keyamp-lleader-map ignore)
(keyamp--map-tab keyamp-lleader-map read-only-mode)
(keyamp--map keyamp-lleader-map
  '(;; left leader left half
    ("`" . next-buffer)
    ("1" . periodic-chart)
    ("2" . kmacro-name-last-macro)
    ("3" . apply-macro-to-region-lines)
    ("4" . clear-r1)
    ("5" . repeat-complex-command)

    ("q" . reformat-lines)
    ("w" . org-ctrl-c-ctrl-c)
    ("e" . del-win)
    ("r" . query-replace)
    ("t" . copy-text-block)

    ("a" . kill-line)
    ("s" . prev-buf)
    ("d" . alt-buf)
    ("f" . next-buf)
    ("g" . rectangle-mark-mode)

    ("z" . universal-argument)
    ("x" . restart-emacs)
    ("c" . copy-to-r1)
    ("v" . paste-from-r1)
    ("b" . toggle-prev-letter-case)

    ;; left leader right half
    ("6" . quit)
    ("7" . number-to-register)
    ("8" . sql)
    ("9" . screenshot)
    ("0" . eww)
    ("-" . ignore)
    ("=" . ignore)

    ("y" . find-name-dired)
    ("u" . flymake-goto-prev-error)

    ("i i"   . copy-file-path)
    ("i DEL" . count-words)                ("i SPC" . count-matches)
    ("i ESC" . ignore)                     ("i RET" . show-in-desktop)

    ("o"  . flymake-goto-next-error)
    ("p"  . show-kill-ring)
    ("["  . save-close-buf)
    ("]"  . find-file)
    ("\\" . bookmark-rename)

    ("h"  . switch-to-buffer)

    ("j s"   . glyphless-display-mode)     ("j l"   . narrow-to-region-or-block)
    ("j d"   . display-line-numbers-mode)  ("j k"   . narrow-to-defun)
    ("j f"   . toggle-word-wrap)           ("j j"   . widen)
    ("j DEL" . hl-line-mode)               ("j SPC" . whitespace-mode)
    ("j ESC" . ignore)                     ("j RET" . toggle-truncate-lines)

    ("k s"   . space-to-newline)
    ("k d"   . delete-matching-lines)      ("k k"   . list-matching-lines)
    ("k f"   . delete-non-matching-lines)
    ("k r"   . quote-lines)                ("k u"   . escape-quotes)
    ("k t"   . delete-duplicate-lines)     ("k y"   . slash-to-double-backslash)
    ("k v"   . reformat-to-sentence-lines) ("k n"   . double-backslash-to-slash)
    ("k w"   . sort-lines-key-value)       ("k o"   . slash-to-backslash)
    ("k x"   . insert-column-a-z)          ("k ."   . sort-lines-block-or-region)
    ("k c"   . cycle-hyphen-lowline-space) ("k ,"   . sort-numeric-fields)
    ("k DEL" . ispell-word)                ("k SPC" . flyspell-buffer)
    ("k ESC" . ignore)                     ("k RET" . make-backup-and-save)

    ("l" . describe-foo-at-point)
    (";" . recentf-open-files)
    ("'" . sync)
    ("n" . isearch-backward)
    ("m" . downloads)
    ("," . player)
    ("." . open-last-closed)
    ("/" . goto-line)))

(if (display-graphic-p)
    (keyamp--map keyamp-lleader-map
      '(("i DEL" . nil)        ("i <backspace>" . count-words)
        ("i RET" . nil)        ("i <return>"    . show-in-desktop)
        ("i ESC" . nil)        ("i <escape>"    . ignore)
        ("j DEL" . nil)        ("j <backspace>" . hl-line-mode)
        ("j RET" . nil)        ("j <return>"    . toggle-truncate-lines)
        ("j ESC" . nil)        ("j <escape>"    . ignore)
        ("k DEL" . nil)        ("k <backspace>" . ispell-word)
        ("k RET" . nil)        ("k <return>"    . make-backup-and-save)
        ("k ESC" . nil)        ("k <escape>"    . ignore)
        ("<mouse-1>" . ignore) ("<mouse-2>" . ignore) ("<mouse-3>" . ignore))))

(keyamp--map-leader keyamp-rleader-map '(select-line . select-word))
(keyamp--map-return keyamp-rleader-map toggle-ibuffer)
(keyamp--map-escape keyamp-rleader-map ignore)
(keyamp--map-tab keyamp-rleader-map help-command)
(keyamp--map keyamp-rleader-map
  '(;; right leader left half
    ("`" . find-next-dir-file)
    ("1" . view-lossage)
    ("2" . insert-kbd-macro)
    ("3" . config)
    ("4" . change-bracket-pairs)
    ("5" . json-pretty)

    ("q" . fill-or-unfill)
    ("w" . sun-moon)

    ("e e"   . todo)                       ("e k"   . weather)
    ("e DEL" . clock)                      ("e SPC" . calendar)
    ("e ESC" . ignore)                     ("e RET" . insert-date)

    ("r" . query-replace-regexp)
    ("t" . calc)
    ("a" . mark-whole-buffer)
    ("s" . clean-whitespace)

    ("D"     . repeat)                     ("В"     . repeat)
    ("d e"   . org-shiftup)                ("d i"   . async-shell-command)
    ("d d"   . eval-region-or-sexp)        ("d k"   . run-current-file)
    ("d DEL" . stow)                       ("d SPC" . eval-defun)
    ("d ESC" . ignore)                     ("d RET" . shell-command)

    ("f e"   . insert-emacs-quote)         ("f i"   . insert-ascii-single-quote)
    ("f f"   . insert-char)                ("f j"   . insert-brace)
                                           ("f k"   . insert-paren)
    ("f s"   . insert-formfeed)            ("f l"   . insert-square-bracket)
    ("f g"   . insert-double-angle-quote)  ("f h"   . insert-double-curly-quote)
    ("f DEL" . insert-backtick-quote)      ("f SPC" . insert-ascii-double-quote)
    ("f ESC" . ignore)                     ("f RET" . emoji-insert)

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
    ("=" . toggle-input-source)

    ("y"  . search-string)
    ("u"  . backward-punct)
    ("i"  . beg-of-block)
    ("o"  . forward-punct)
    ("p"  . recenter-top-bottom)
    ("["  . open-last-closed)
    ("]"  . rename-visited-file)
    ("\\" . bookmark-delete)

    ("h" . page-up-half)
    ("j" . isearch-cur-word-backward)
    ("k" . end-of-block)
    ("l" . isearch-cur-word-forward)
    (";" . page-dn-half)
    ("'" . toggle-frame-maximized)

    ("n" . save-buffer)
    ("m" . dired-jump)
    ("," . open-file)
    ("." . save-close-buf)
    ("/" . view-messages)
    ("*" . view-messages)))

(if (display-graphic-p)
    (keyamp--map keyamp-rleader-map
      '(("e DEL" . nil)        ("e <backspace>" . clock)
        ("e RET" . nil)        ("e <return>"    . insert-date)
        ("e ESC" . nil)        ("e <escape>"    . ignore)
        ("d DEL" . nil)        ("d <backspace>" . stow)
        ("d RET" . nil)        ("d <return>"    . shell-command)
        ("d ESC" . nil)        ("d <escape>"    . ignore)
        ("f DEL" . nil)        ("f <backspace>" . insert-backtick-quote)
        ("f RET" . nil)        ("f <return>"    . emoji-insert)
        ("f ESC" . nil)        ("f <escape>"    . ignore)
        ("<mouse-1>" . ignore) ("<mouse-2>" . ignore) ("<mouse-3>" . ignore))))

(keyamp--map-double
  '((keyamp-escape . alt-buf)       (activate-region . deactivate-region)
    (beg-of-line   . bookmark-jump) (end-of-lyne     . end-of-buf)
    (kmacro-record . keyamp-C-h)    (other-win       . open-file)))


;; Core Remaps

(setq help-map (make-sparse-keymap)) (fset 'help-command help-map)

(keyamp--map-leader help-map '(lookup-word-definition . translate))
(keyamp--map-return help-map lookup-web)
(keyamp--map-tab help-map lookup-wikipedia)
(keyamp--map help-map
  '(("e" . describe-char)      ("i" . info)
    ("s" . info-lookup-symbol) ("j" . describe-function)
    ("d" . man)                ("k" . describe-key)
    ("f" . elisp-index-search) ("l" . describe-variable)
    ("q" . describe-syntax)    ("p" . apropos-documentation)
    ("w" . describe-bindings)  ("o" . lookup-all-dictionaries)
    ("r" . describe-mode)      ("u" . lookup-all-synonyms)
    ("a" . describe-face)      (";" . lookup-wiktionary)
    ("g" . apropos-command)    ("h" . describe-coding-system)
    ("z" . apropos-variable)   ("." . lookup-word-dict-org)
    ("x" . apropos-value)      ("," . lookup-etymology)))

(keyamp--map global-map '(("C-r" . open-file) ("C-t" . hippie-expand)))
(when (display-graphic-p)
  (keyamp--map global-map
    '(("<prior>" . page-up-half) ("<next>" . page-dn-half)
      ("<double-mouse-1>"        . select-word)
      ("<mouse-3>"               . mouse-3)
      ("<header-line> <mouse-1>" . novel)))
  ;; Avoid karabiner mode sync lag.
  (keyamp--remap keyamp-command-map '((hippie-expand . open-file)))
  (advice-add 'mouse-set-point   :around 'lookup-around)
  (advice-add 'mouse-set-point   :before 'scroll-one-pixel)
  (advice-add 'mouse-set-point   :after  'keyamp-command-if-insert)
  (advice-add 'mouse-drag-region :before 'copy-selection)
  (advice-add 'mac-mwheel-scroll :before 'keyamp-command-if-insert)
  (advice-add 'mac-mwheel-scroll :before 'deactivate-mark-if-active))

(advice-add 'keyamp-insert :before 'delete-before)
(advice-add 'keyamp-insert :around 'lookup-around)
(advice-add 'keyamp-insert :around 'translate-around)

(with-sparse-keymap-x
 ;; Repeat using DEL/SPC or D. The concept widely used to form Repeat mode.
 (keyamp--map-leader x '(del-back . del-back))
 (keyamp--remap x '((del-back . repeat))) (keyamp--set-map x '(repeat)))

(with-sparse-keymap-x
 ;; Hold down RET in insert mode to call `hippie-expand' with C-t.
 ;; Next RET press to insert a possible expansion. DEL to undo, SPC to continue.
 (keyamp--map-leader x '(hippie-expand-undo . self-insert-command))
 (keyamp--map-return x hippie-expand) (keyamp--set-map x '(hippie-expand)))

(defun save-buffer-isearch-cancel ()
  "Cancel isearch and save buffer."
  (interactive)
  (if (and (buffer-modified-p) (buffer-file-name)) (save-buffer))
  (if isearch-mode (isearch-cancel)))

(with-sparse-keymap-x
 ;; After starting up an isearch press DEL to retreat to the previous
 ;; search string. Press SPC to pull string from kill ring into search string.
 (keyamp--map-leader x '(isearch-ring-retreat . isearch-yank-kill))
 ;; Double press N to save modified file.
 (keyamp--map x '(("n" . save-buffer-isearch-cancel) ("т" . save-buffer-isearch-cancel)))
 (keyamp--set-map-hook x '(isearch-mode-hook) nil nil :repeat))

;; Hit TAB to repeat after typing in search string and set following transient
;; map. Backtab to repeat backward.
(keyamp--map-leader isearch-mode-map '(isearch-del-char . isearch-printing-char))
(keyamp--map-tab isearch-mode-map isearch-forw)
(keyamp--map-backtab isearch-mode-map isearch-back)
(keyamp--map isearch-mode-map '(("<escape>" . isearch-cancel) ("C-^" . keyamp-lleader-map)))
(keyamp--remap isearch-mode-map '((paste-from-r1 . isearch-yank-r1)))

(with-sparse-keymap-x
 ;; Find the occurrence of the current search string with J/L or DEL/SPC.
 ;; Press I/K to get search strings from the ring then DEL/SPC to repeat.
 ;; RET to search again.
 (keyamp--map-leader x '(isearch-back . isearch-forw))
 (keyamp--map x
   '(("i" . isearch-ring-retreat) ("ш" . isearch-ring-retreat)
     ("j" . isearch-back)         ("о" . isearch-back)
     ("k" . isearch-ring-advance) ("л" . isearch-ring-advance)
     ("l" . isearch-forw)         ("д" . isearch-forw)))
 (keyamp--set-map x
   '(isearch-ring-retreat     isearch-ring-advance
     isearch-back             isearch-forw
     isearch-cur-word-forward isearch-cur-word-backward isearch-yank-kill))

 (defun isearch-mode-exit-minibuffer ()
   "Reset isearch transient after choice from the ring."
  (if (memq real-this-command '(exit-minibuffer keyamp-minibuffer-insert))
      (keyamp-set-deactivate-repeat-fun x)))
 (add-hook 'isearch-mode-hook 'isearch-mode-exit-minibuffer 96))



(defun keyamp-screen-TAB ()
  "Tab key command for transient use."
  (interactive)
  (cond
   ((eq major-mode 'org-agenda-mode) (keyamp-command-execute 'todo))
   ((eq major-mode 'ibuffer-mode) (keyamp-command-execute 'ibuffer-select-group))
   ((eq major-mode 'gnus-group-mode) (keyamp-command-execute 'gnus-topic-select-group))
   (t (keyamp-command-execute 'page-dn-half))))

(defun keyamp-RET ()
  "Return key command for transient use."
  (interactive)
  (if (eq major-mode 'eww-mode)
      (keyamp-command-execute 'keyamp-insert) ; do translate
    (if (display-graphic-p)
        (keyamp-exe-remap "<return>" 'keyamp-insert 'keyamp-escape)
      (keyamp-exe-remap "RET" 'keyamp-insert 'keyamp-escape))))

(defun keyamp-lleader-E ()
  (interactive) (keyamp-exe-remap "e" 'undo 'del-win))
(defun keyamp-lleader-S ()
  (interactive) (keyamp-exe-remap "s" 'open-line 'prev-buf))
(defun keyamp-lleader-D ()
  (interactive) (keyamp-exe-remap "d" 'del-back 'alt-buf))
(defun keyamp-lleader-F ()
  (interactive) (keyamp-exe-remap "f" 'newline 'next-buf))

(defun keyamp-C-h ()
  "Maybe substitute for C-h."
  (interactive)
  (cond ((eq major-mode 'dired-mode) (keyamp-command-execute 'dired-do-delete))
        ((eq major-mode 'ibuffer-mode) (keyamp-command-execute 'ibuffer-do-delete))
        (t (keyamp-command-execute 'deactivate-region))))


;; Repeat mode screen commands

(with-sparse-keymap-x
 ;; Leader layer to become transient main. Base map for next leaders adjustment
 ;; by transient maps which might be set by following target commands subsets.
 (keyamp--map-leader x '(open-line . newline))
 (keyamp--map-tab x keyamp-screen-TAB)
 (keyamp--remap x
   '((keyamp-insert       . keyamp-escape)
     (make-frame-command  . delete-frame)
     (insert-space-before . ignore)
     (backward-del-word   . sun-moon)
     (undo                . del-win)
     (del-word            . toggle-gnus)
     (cut-text-block      . calc)
     (goto-match-br       . view-messages)
     (shrink-whitespaces  . calendar-split)
     (del-back            . alt-buf)
     (activate-region     . new-empty-buffer)
     (toggle-comment      . ignore)
     (cut-line            . prev-eww-buffer)
     (copy-line           . screen-idle)
     (paste-or-prev       . tasks)
     (backward-bracket    . dired-jump)
     (forward-bracket     . save-close-buf)
     (kmacro-helper       . config)
     (copy-to-register    . sql)
     (up-line             . view-messages)
     (down-line           . screen-idle)
     (back-char           . next-buf)
     (forw-char           . prev-buf)))

 (keyamp--set-map x
   '(prev-buf             next-buf
     delete-other-windows split-window-below
     delete-window        save-close-buf
     prev-proj-buf        next-proj-buf
     prev-eww-buffer      next-eww-buffer
     tasks                config
     previous-buffer      next-buffer
     find-prev-dir-file   find-next-dir-file
     shrink-window        enlarge-window
     org-agenda-switch-to-task)))

;; save-close-buf indicate modify, but set screen keymap and defer screen indicate
(defvar save-close-buf-timer nil "Timer to indicate `save-close-buf'.")

(defun save-close-buf-timer-cancel ()
  "Cancel timer and remove post command hook."
  (remove-hook 'post-command-hook 'save-close-buf-timer-cancel)
  (unless (eq this-command last-command)
    (if (timerp save-close-buf-timer) (cancel-timer save-close-buf-timer))))

(defun save-close-buf-timer-init ()
  "Init timer with post command hook."
  (remove-hook 'post-command-hook 'save-close-buf-timer-init)
  (add-hook 'post-command-hook 'save-close-buf-timer-cancel))

(defun save-close-buf-screen-indicate ()
  "Defer screen indicator after `save-close-buf'."
  (add-hook 'post-command-hook 'save-close-buf-timer-init)
  (setq save-close-buf-timer
        (run-with-timer 0.5 nil 'keyamp-indicate keyamp-screen-indicator keyamp-edit-cursor keyamp-screen-color)))

(advice-add 'save-close-buf :after 'save-close-buf-screen-indicate)

(with-sparse-keymap-x
 ;; DEL/SPC to switch other window after split as a result of the commands.
 (keyamp--map-leader x '(other-window . other-window))
 (keyamp--remap x
   '((keyamp-insert . delete-other-windows) (down-line . delete-other-windows)))
 (keyamp--set-map x
   '(describe-foo-at-point   describe-variable
     describe-function       describe-key
     describe-mode           describe-char
     describe-face           list-matching-lines
                             occur-cur-word
     run-current-file        exec-query
     view-messages           sun-moon
     clock                   async-shell-command
     open-in-external-app    player
     sync                    calendar-split)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-buf) (newline . next-buf)))
 (keyamp--set-map x
   '(prev-buf next-buf delete-other-windows delete-window split-window-below)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-proj-buf) (newline . next-proj-buf)))
 (keyamp--set-map x '(prev-proj-buf next-proj-buf)))

(with-sparse-keymap-x
 (keyamp--map-tab x keyamp-screen-TAB)
 (keyamp--remap x
   '((open-line . prev-eww-buffer) (newline . next-eww-buffer)
     (del-back  . eww-reload)      (undo    . justify-buffer)))
 (keyamp--set-map x '(prev-eww-buffer next-eww-buffer)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-buf) (newline . tasks)))
 (keyamp--set-map x '(tasks org-agenda-switch-to-task)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . prev-buf) (newline . config)))
 (keyamp--set-map x '(config)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . previous-buffer) (newline . next-buffer)))
 (keyamp--set-map x '(previous-buffer next-buffer)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . find-prev-dir-file) (newline . find-next-dir-file)))
 (keyamp--set-map x '(find-prev-dir-file find-next-dir-file)))

(with-sparse-keymap-x
 (keyamp--remap x '((backward-bracket . dired-jump)))
 (keyamp--set-map x
   '(dired-jump downloads dired-find-file ibuffer-visit-buffer open-last-closed
     bookmark-jump widget-button-press)))

(with-sparse-keymap-x
 (keyamp--remap x
   '((open-line       . prev-buf)       (newline       . next-buf)
     (forward-bracket . save-close-buf) (keyamp-insert . keyamp-escape)))
 (keyamp--set-map x '(save-close-buf)))

(with-sparse-keymap-x
 (keyamp--remap x '((open-line . shrink-window) (newline . enlarge-window)))
 (keyamp--set-map x '(shrink-window enlarge-window) nil nil nil 2))

(with-sparse-keymap-x
 (keyamp--remap x '((make-frame-command . delete-frame)))
 (keyamp--set-map x '(alternate-frame)))


;; Repeat mode view commands

(with-sparse-keymap-x
 (keyamp--remap x '((keyamp-insert . screen-idle)))
 (keyamp--set-map-hook x '(ibuffer-hook)))

(with-sparse-keymap-x
 ;; Initiate by triple DEL/SPC (hold down).
 ;; I/K or DEL/SPC to move by lines. See `return-before'.
 (keyamp--map-leader x '(previous-line . next-line))
 (keyamp--map-tab x keyamp-screen-TAB)
 (keyamp--remap x
   '((previous-line . up-line) (next-line . down-line) (keyamp-insert . keyamp-RET)))
 (keyamp--set-map x '(up-line down-line))
 (keyamp--set-map-hook x '(ibuffer-hook gnus-group-mode-hook) nil nil :repeat)

 (defun keyamp-lines-move (&rest r)
   "Repeat move by lines."
   (if (memq major-mode keyamp-lines-move-modes)
       (keyamp-set-deactivate-repeat-fun x)))
 (advice-add-macro '(other-window translate dired-find-file dired-jump)
                   :after 'keyamp-lines-move))

 (defvar keyamp-lines-move-modes
  '(occur-mode         org-agenda-mode  gnus-group-mode
    ibuffer-mode       eww-mode         messages-buffer-mode
    emms-playlist-mode fundamental-mode dired-mode)
  "List of modes using lines move.")

(with-sparse-keymap-x
 (keyamp--map-leader x '(previous-line . next-line))
 (keyamp--remap x
   '((previous-line . beg-of-block)     (next-line . end-of-block)
     (up-line       . beg-of-block)     (down-line . end-of-block)
     (keyamp-insert . keyamp-RET)
     (undo          . keyamp-lleader-E) (del-back  . keyamp-lleader-D)
     (open-line     . keyamp-lleader-S) (newline   . keyamp-lleader-F)))
 (keyamp--set-map x '(beg-of-block end-of-block beg-of-buf end-of-buf)))

(with-sparse-keymap-x
 (keyamp--remap x '((beg-of-line . beg-of-buf) (end-of-lyne . end-of-buf)))
 (keyamp--set-map x '(beg-of-buf end-of-buf)))

;; In case triple DEL received during `keyamp-key-repeat-delay',
;; `select-block' would be ignored. Must call before following transient maps.
;; Same for triple SPC and `select-word'.
(advice-add-macro '(select-block select-word) :around 'keyamp-defer-command-around)
(advice-add-macro '(up-line down-line) :before 'keyamp-cancel-defer-command-timer)

(with-sparse-keymap-x
 (keyamp--map-leader x '(up-line . end-of-block))
 (keyamp--remap x
   '((previous-line . beg-of-block)  (next-line . select-block)
     (keyamp-escape . return-before) (open-file . exec-query)))
 (keyamp--set-map x '(select-block)))

(with-sparse-keymap-x
 (keyamp--map-leader x '(beg-of-block . down-line))
 (keyamp--remap x '((keyamp-escape . return-before)))
 (keyamp--set-map x '(select-word)))

(with-sparse-keymap-x
 (keyamp--map-leader x '(back-word . forw-char))
 (keyamp--remap x '((keyamp-escape . return-before)))
 (keyamp--set-map x '(select-quote)))

(with-sparse-keymap-x
 (keyamp--map-leader x '(back-char . forw-word))
 (keyamp--remap x '((keyamp-escape . return-before) (keyamp-insert . keyamp-C-h)))
 (keyamp--set-map x '(select-line)))

(advice-add-macro
 ;; If region active deactivate mark and return to the point before selection.
 '(up-line                       down-line
   beg-of-block                  end-of-block
   ibuffer-backward-filter-group ibuffer-forward-filter-group
   gnus-topic-prev               gnus-topic-next
   page-up-half                  page-dn-half
   prev-buf                      next-buf
   back-word                     forw-word
   back-char                     forw-char
   dired-do-delete               deactivate-region)
:before 'return-before)

(with-sparse-keymap-x
 ;; Left/right arrows repeat by DEL/SPC.
 (keyamp--map-leader x '(back-char . forw-char))
 (keyamp--set-map x '(back-char forw-char)))

(with-sparse-keymap-x
 (keyamp--map-leader x '(backward-char . forward-char))
 (keyamp--remap x '((backward-char . back-word) (forward-char . forw-word)))
 (keyamp--set-map x '(back-word forw-word)))

(with-sparse-keymap-x
 (keyamp--map-leader x '(back-word . forw-word))
 (keyamp--remap x '((back-word . backward-punct) (forw-word . forward-punct)))
 (keyamp--set-map x '(backward-punct forward-punct)))

(with-sparse-keymap-x
 ;; Hold down H/; to initiate half page up/down. Repeat with I/K or DEL/SPC.
 (keyamp--map-leader x '(previous-line . next-line))
 (keyamp--map-tab x scroll-up-command)
 (keyamp--remap x
   '((previous-line . page-up-half)  (next-line . page-dn-half)
     (down-line     . page-dn-half)  (up-line   . page-up-half)
     (keyamp-insert . keyamp-RET)
     (undo          . keyamp-lleader-E)   (del-back  . keyamp-lleader-D)
     (open-line     . keyamp-lleader-S)   (newline   . keyamp-lleader-F)))
 (unless (display-graphic-p) ; touch reader
   (keyamp--remap x '((down-line . page-up-half) (up-line . page-dn-half))))
 (keyamp--set-map x '(page-up-half page-dn-half)))

(with-sparse-keymap-x
 ;; Initially TAB makes half page forward, following presses do full page.
 ;; Arrows always do half page and keep TAB transient, see previous keymap.
 (keyamp--map-leader x '(previous-line . next-line))
 (keyamp--map-tab x next-line)
 (keyamp--remap x
   '((previous-line . scroll-down-command) (next-line . scroll-up-command)
     (down-line     . page-dn-half)        (up-line   . page-up-half)
     (keyamp-insert . keyamp-RET)
     (undo          . keyamp-lleader-E)    (del-back  . keyamp-lleader-D)
     (open-line     . keyamp-lleader-S)    (newline   . keyamp-lleader-F)))
 (unless (display-graphic-p) ; touch reader
   (keyamp--remap x '((down-line . page-up-half) (up-line . page-dn-half))))
 (keyamp--set-map x '(scroll-down-command scroll-up-command)))

(with-sparse-keymap-x
 (keyamp--map-leader x '(next-line . next-line))
 (keyamp--remap x '((next-line . jump-mark))) (keyamp--set-map x '(jump-mark)))

(with-sparse-keymap-x
 (keyamp--map-leader x '(text-scale-decrease . text-scale-increase))
 (keyamp--map-tab x text-scale-reset)
 (keyamp--remap x '((keyamp-insert . keyamp-escape)))
 (keyamp--set-map x '(text-scale-decrease text-scale-increase text-scale-reset)))

(with-sparse-keymap-x
 (keyamp--map-leader x '(button-back . button-forw))
 (keyamp--set-map x '(button-back button-forw)))

(defvar keyamp-button-move-modes nil "List of modes using button move.")

(defun keyamp-button-move (&rest r)
  "Continue move by buttons."
  (if (and (memq major-mode keyamp-button-move-modes) (= (point) (point-min)))
      (keyamp-command-execute 'button-forw)))
(advice-add 'other-window :after 'keyamp-button-move)

(with-sparse-keymap-x
 (keyamp--map-leader x '(next-line . next-line))
 (keyamp--remap x '((next-line . recenter-top-bottom)))
 (keyamp--set-map x '(recenter-top-bottom)) nil nil nil 2)


;; Repeat mode edit commands

(with-sparse-keymap-x
 ;; After hit delete backward/forward char, shrink whitespaces or insert
 ;; space before while in command mode, DEL/SPC start to do delete/space.
 (keyamp--map-leader x '(delete-forward-char . insert-space-before))
 (keyamp--set-map x '(delete-forward-char) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leader x '(del-back . insert-space-before))
 (keyamp--set-map x '(del-back insert-space-before) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leader x '(open-line . newline))
 (keyamp--remap x '((open-line . backward-del-word) (newline . del-word)))
 (keyamp--set-map x '(backward-del-word del-word) nil nil nil 2))

(with-sparse-keymap-x
 (keyamp--map-leader x '(undo-only . undo-redo))
 (keyamp--set-map x '(undo undo-redo)))

(with-sparse-keymap-x
 (keyamp--remap x '((del-back . cut-text-block)))
 (keyamp--set-map x '(cut-text-block)))

(with-sparse-keymap-x
 (keyamp--map-leader x '(del-back . insert-space-before))
 (keyamp--remap x '((del-back . shrink-whitespaces)))
 (keyamp--set-map x '(shrink-whitespaces) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leader x '(del-back . del-back ))
 (keyamp--remap x '((del-back . toggle-comment)))
 (keyamp--set-map x '(toggle-comment) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leader x '(del-back . del-back ))
 (keyamp--remap x '((del-back . cut-line)))
 (keyamp--set-map x '(cut-line) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leader x '(del-back . del-back ))
 (keyamp--remap x '((del-back . copy-line))) (keyamp--set-map x '(copy-line)))

(with-sparse-keymap-x
 (keyamp--remap x '((del-back . toggle-letter-case)))
 (keyamp--set-map x '(toggle-letter-case) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leader x '(del-back . undo))
 (keyamp--remap x
   '((undo . org-shiftup) (del-back . org-shiftdown) (copy-line . screen-idle)))
 (keyamp--set-map x '(org-shiftup org-shiftdown) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--map-leader x '(screen-idle . todo))
 (keyamp--remap x '((undo . todo) (copy-line . screen-idle)))
 (keyamp--set-map x '(todo insert-date) nil nil nil 1))

(with-sparse-keymap-x
 (keyamp--remap x '((del-back . cycle-hyphen-lowline-space)))
 (keyamp--set-map x '(cycle-hyphen-lowline-space) nil nil nil 1))

(with-sparse-keymap-x
 ;; SPC S SPC to clean whitespaces and save the buffer or DEL to close.
 (keyamp--map-leader x '(save-close-buf . save-buffer))
 (keyamp--remap x '((backward-bracket . dired-jump)))
 (keyamp--set-map x '(clean-whitespace) nil nil nil 3))

(with-sparse-keymap-x ; use dummy for modify indication purpose
 (keyamp--set-map x
   '(apply-macro-to-region-lines cycle-hyphen-lowline-space
     delete-duplicate-lines      delete-matching-lines
     delete-non-matching-lines   emoji-insert
     eval-defun                  eval-region-or-sexp
     eshell-clear-input
     fill-or-unfill              increment-register
     insert-ascii-double-quote   insert-ascii-single-quote
     insert-backtick-quote       insert-brace
     insert-column-a-z           insert-double-angle-quote
     insert-double-curly-quote   insert-emacs-quote
     insert-formfeed             insert-paren
     insert-square-bracket       json-pretty
     new-empty-buffer            newline
     open-line                   org-insert-source-code
     org-open-line               quote-lines
     reformat-lines              reformat-to-sentence-lines
     run-current-file            save-buffer
     sort-lines-key-value        space-to-newline
     title-case-region-or-line   toggle-prev-letter-case
     yank-pop                    yank)
   nil nil nil 0.5))


;; Modes Remaps

(with-eval-after-load 'minibuffer
  (with-sparse-keymap-x
   ;; On minibuffer startup press DEL or I to list history backwards or
   ;; SPC or K to list completion candidates forward. After that
   ;; I/K or DEL/SPC to list either history or completion candidates
   ;; accordingly choice made. RET to confirm and exit, ESC to quit.
   ;; To switch from history to candidates listing press ESC then double
   ;; SPC `select-word' and DEL/SPC or I/K again to continue move
   ;; backward/forward. Similarly double DEL to activate history move.
   (keyamp--map-leader x '(select-block . select-word))
   (keyamp--remap x
     '((end-of-lyne             . keyamp-insert-n) ; literal answers Y N !
       (backward-del-word       . keyamp-insert-y) ; but not for `read-event'
       (toggle-case-fold-search . keyamp-insert-!)
       (keyamp-insert           . keyamp-minibuffer-insert)
       (keyamp-escape           . keyamp-minibuffer-escape)
       (open-file               . keyamp-exit-minibuffer)))
   ;; The hook is last one run during minibuffer setup and set the keymap.
   (keyamp--set-map-hook x '(minibuffer-setup-hook) :command nil :repeat)
   (add-hook 'minibuffer-setup-hook 'keyamp-minibuffer-setup-insert 96))

  ;; Hit D/DEL for No, K/SPC for Yes to answer non-literal Y or N.
  (keyamp--remap y-or-n-p-map
    '((select-block . y-or-n-p-insert-n) (del-back  . y-or-n-p-insert-n)
      (select-word  . y-or-n-p-insert-y) (next-line . y-or-n-p-insert-y)))

  (keyamp--remap minibuffer-local-map
    '((previous-line . hist-back) (next-line . hist-forw)
      (select-block  . hist-back)))
  (keyamp--map-tab minibuffer-local-completion-map minibuffer-complete)
  (keyamp--remap minibuffer-mode-map
    '((previous-line . hist-back) (next-line . hist-forw)
      (select-block  . hist-back)))

  (with-sparse-keymap-x
   (keyamp--map-tab x minibuffer-next-completion)
   (keyamp--map-leader x
     '(minibuffer-previous-completion . minibuffer-next-completion))
   (keyamp--remap x '((keyamp-escape . delete-completion-win)))
   (keyamp--map-return x minibuffer-choose-completion)
   (keyamp--set-map x
     '(completion-at-point
       minibuffer-previous-completion minibuffer-next-completion)))

  (advice-add 'completion-at-point :after
              (lambda (&rest r) "select candidate" (minibuffer-next-completion)))
  (advice-add-macro
   '(completion-at-point minibuffer-choose-completion delete-completion-win)
   :after 'keyamp-insert-init)

  (keyamp--map minibuffer-inactive-mode-map
    '(("<mouse-1>" . player) ("<double-mouse-1>" . ignore))))

(with-eval-after-load 'icomplete
  (keyamp--map-return icomplete-minibuffer-map keyamp-exit-minibuffer)
  (keyamp--remap icomplete-minibuffer-map
    '((previous-line . comp-back) (next-line . comp-forw)
      (select-word   . comp-forw)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x
     '((keyamp-escape . abort-recursive-edit) (keyamp-insert . keyamp-exit-minibuffer)
       (previous-line . comp-back)            (next-line     . comp-forw)))
   (keyamp--set-map x '(comp-back comp-forw)))

  (with-sparse-keymap-x
   (keyamp--remap x '((previous-line . hist-back) (next-line . comp-forw)))
   (keyamp--set-map-hook x '(icomplete-minibuffer-setup-hook) nil nil :repeat))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x
     '((keyamp-escape . abort-recursive-edit) (keyamp-insert . exit-minibuffer)
       (previous-line . hist-back)            (next-line     . hist-forw)))
   (keyamp--set-map x '(hist-back hist-forw))))

(add-hook 'ido-setup-hook
  (lambda () "ido-completion-map created after ido setup only"
    (keyamp--remap ido-completion-map
      '((keyamp-insert . ido-exit-minibuffer)
        (previous-line . hist-back)      (select-block  . hist-back)
        (next-line     . ido-next-match) (select-word   . ido-next-match)))))

(with-sparse-keymap-x
 (keyamp--map-leader x '(previous-line . next-line))
 (keyamp--remap x '((previous-line . ido-prev-match) (next-line . ido-next-match)))
 (keyamp--set-map x '(ido-prev-match ido-next-match)))

(with-eval-after-load 'dired
  (keyamp--map dired-mode-map
    '(("C-h" . dired-do-delete)       ("C-r" . delete-other-windows)
      ("<mouse-1>" . dired-find-file) ("<mouse-2>" . dired-find-file)))

  (keyamp--remap dired-mode-map
    '((keyamp-insert       . dired-find-file)
      (backward-bracket    . dired-jump)
      (insert-space-before . ignore)
      (del-word            . dired-unmark-all-marks)
      (backward-del-word   . dired-do-chmod)
      (shrink-whitespaces  . dired-hide-details-mode)
      (open-line           . prev-buf)
      (del-back            . dired-toggle-mark)
      (newline             . next-buf)
      (toggle-comment      . revert-buffer)
      (cut-line            . dired-kill-subdir)
      (cut-text-block      . dired-maybe-insert-subdir)
      (paste-or-prev       . dired-create-directory)
      (toggle-letter-case  . dired-sort)
      (copy-to-r1          . dired-do-copy)
      (paste-from-r1       . dired-do-rename)
      (mark-whole-buffer   . dired-toggle-marks)
      (kmacro-helper       . config)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(dired-toggle-mark . dired-toggle-mark))
   (keyamp--set-map x '(dired-toggle-mark) nil nil nil 1))
  (advice-add 'dired-toggle-marks :before 'dired-unmark-all-marks))

(with-eval-after-load 'wdired
  (keyamp--map wdired-mode-map
    '(("C-h" . wdired-abort-changes) ("C-r" . wdired-finish-edit)
      ("C-q" . wdired-abort-changes) ("C-t" . wdired-finish-edit)))
  (advice-add-macro '(wdired-abort-changes wdired-finish-edit)
                    :after 'keyamp-command))

(with-eval-after-load 'dired-utils
  (keyamp--map-tab dired-mode-map dired-leader-map)
  (keyamp--map-tab (define-prefix-command 'dired-leader-map) dired-omit-mode)
  (keyamp--map dired-leader-map
    '(("e"  . dired-optimize-png)     ("u" . dired-2drawing)
      ("o"  . dired-rotate-img-right) ("p" . dired-rotate-img-left)
      ("a"  . dired-image-autocrop)   ("s" . dired-open-marked)
      ("d"  . dired-show-metadata)    ("h" . dired-rotate-img-180)
      ("l"  . dired-2png)             (";" . dired-scale-image)
      ("\'" . dired-zip-enc)          ("c" . dired-2jpg)
      ("/"  . dired-zip)              ("." . dired-unzip))))

(with-eval-after-load 'rect ; sane rectangle controls
  (keyamp--remap rectangle-mark-mode-map
    '((keyamp-insert       . string-rectangle)
      (insert-space-before . open-rectangle)
      (copy-line           . copy-rectangle-as-kill)
      (del-back            . kill-rectangle)
      (paste-or-prev       . yank-rectangle)
      (copy-to-register    . copy-rectangle-to-register)
      (toggle-comment      . rectangle-number-lines)
      (cut-line            . clear-rectangle)
      (clean-whitespace    . delete-whitespace-rectangle))))

(with-eval-after-load 'ibuf-ext
  (keyamp--map-tab ibuffer-mode-map ibuffer-forward-filter-group)
  (keyamp--map ibuffer-mode-map
    '(("C-h" . ibuffer-do-delete) ("<double-mouse-1>" . ibuffer-visit-buffer)))

  (keyamp--map-tab ibuffer-mode-map ibuffer-visit-buffer)
  (keyamp--remap ibuffer-mode-map
    '((previous-line       . up-line)
      (next-line           . down-line)
      (keyamp-insert       . ibuffer-visit-buffer)
      (end-of-lyne         . ibuffer-forward-filter-group)
      (beg-of-line         . ibuffer-backward-filter-group)
      (end-of-block        . ibuffer-forward-filter-group)
      (beg-of-block        . ibuffer-backward-filter-group)
      (insert-space-before . ignore)
      (backward-del-word   . sun-moon)
      (undo                . del-win)
      (cut-text-block      . calc)
      (goto-match-br       . view-messages)
      (shrink-whitespaces  . calendar-split)
      (open-line           . prev-buf)
      (del-back            . alt-buf)
      (newline             . next-buf)
      (activate-region     . new-empty-buffer)
      (toggle-comment      . ignore)
      (cut-line            . prev-eww-buffer)
      (copy-line           . screen-idle)
      (paste-or-prev       . tasks)
      (backward-bracket    . downloads)
      (forward-bracket     . save-close-buf)
      (kmacro-helper       . config)
      (copy-to-register    . sql)
      (del-word            . toggle-gnus)
      (forw-char           . screen-idle-return)
      (back-char           . screen-idle)))

  (keyamp--map ibuffer-mode-filter-group-map
    '(("C-h" . help-command) ("<mouse-1>" . ibuffer-toggle-filter-group)))

  (keyamp--map-tab ibuffer-mode-filter-group-map ibuffer-toggle-filter-group)
  (keyamp--remap ibuffer-mode-filter-group-map
    '((keyamp-insert . ibuffer-toggle-filter-group)
      (forw-char     . screen-idle-return)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x '((previous-line . up-line) (next-line . down-line)))
   (keyamp--set-map x '(ibuffer-toggle-filter-group)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . ibuffer-backward-filter-group)
       (next-line     . ibuffer-forward-filter-group)
       (beg-of-line   . beg-of-buf)
       (end-of-lyne   . end-of-buf)))
   (keyamp--set-map x
     '(ibuffer-backward-filter-group ibuffer-forward-filter-group))))

(with-eval-after-load 'ibuffer
  (keyamp--map ibuffer-name-map '(("<mouse-1>" . mouse-set-point))))

(with-sparse-keymap-x
 (keyamp--map-leader x '(del-back . del-back))
 (keyamp--remap x '((del-back . ibuffer-do-delete)))
 (keyamp--set-map x '(ibuffer-do-delete) nil nil nil 3))

(with-eval-after-load 'company
  (keyamp--map-tab company-active-map company-complete-common)

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x
     '((keyamp-escape   . company-abort)
       (keyamp-insert   . company-complete-selection)
       (isearch-forward . company-search-candidates)
       (previous-line   . company-select-back)
       (next-line       . company-select-forw)
       (beg-of-line     . company-previous-page)
       (end-of-lyne     . company-next-page)))

   (keyamp--set-map x
     '(company-select-back company-select-forw company-previous-page
       company-next-page company-show-doc-buffer company-search-abort
       company-manual-begin))

   (advice-add 'company-manual-begin :before 'keyamp-command)
   (defun keyamp-command-company ()
     (if company-candidates (keyamp-set-deactivate-repeat-fun x)))
   (add-hook 'keyamp-command-hook 'keyamp-command-company))

  (with-sparse-keymap-x
   ;; Activate command mode after complete selection, but if next hit is SPC
   ;; then activate insert mode and insert SPC. DEL to undo the completion.
   (advice-add-macro '(company-search-abort company-complete-selection)
                     :after 'keyamp-command-if-insert)
   (keyamp--map-leader x '(undo . keyamp-insert-and-SPC))
   (keyamp--set-map x '(company-search-abort company-complete-selection)))

  (advice-add 'company-search-candidates :after 'keyamp-insert-init)
  (keyamp--map-tab company-search-map company-search-repeat-forward)
  (keyamp--map-backtab company-search-map company-search-repeat-backward)
  (keyamp--map company-search-map '(("<escape>" . company-search-abort)))

  (with-sparse-keymap-x
   (keyamp--map-leader x
     '(company-search-repeat-backward . company-search-repeat-forward))
   (keyamp--set-map x
     '(company-search-repeat-backward company-search-repeat-forward))))

(with-eval-after-load 'transient
  (keyamp--map transient-base-map '(("<escape>" . transient-quit-one))))

(with-eval-after-load 'arc-mode
  (keyamp--remap archive-mode-map '((keyamp-insert . archive-extract))))

(with-eval-after-load 'bookmark
  (keyamp--remap bookmark-bmenu-mode-map
    '((keyamp-insert . bookmark-bmenu-this-window))))

(with-eval-after-load 'button
  (keyamp--remap button-map '((keyamp-insert . push-button))))

(with-eval-after-load 'compile
  (keyamp--remap compilation-button-map '((keyamp-insert . compile-goto-error))))

(with-eval-after-load 'flymake
  (keyamp--remap flymake-diagnostics-buffer-mode-map
    '((keyamp-insert . flymake-goto-diagnostic))))

(with-eval-after-load 'replace
  (keyamp--remap occur-mode-map '((keyamp-insert . occur-mode-goto-occurrence)))
  (keyamp--map query-replace-map
    '(("d" . skip) ("в" . skip)  ("k" . act) ("л" . act)
      ("RET" . edit-replacement) ("<return>" . edit-replacement)
      ("ESC" . exit)             ("<escape>" . exit))))

(with-eval-after-load 'shr
  (keyamp--remap shr-map '((keyamp-insert . shr-browse-url))))

(with-eval-after-load 'simple
  (keyamp--remap completion-list-mode-map '((keyamp-insert . choose-completion))))

(with-eval-after-load 'wid-edit
  (keyamp--remap widget-link-keymap '((keyamp-insert . widget-button-press)))
  (with-sparse-keymap-x
   (keyamp--map-leader x '(widget-backward . widget-forward))
   (keyamp--set-map x '(widget-backward widget-forward))))

(with-eval-after-load 'org
  (keyamp--map-tab org-mode-map org-cycle)
  (keyamp--remap org-mode-map
    '((eval-region-or-sexp . insert-date) (insert-date . org-time-stamp))))

(with-eval-after-load 'org-agenda
  (keyamp--map org-agenda-mode-map
    '(("<double-mouse-1>" . org-agenda-switch-to-task)))
  (keyamp--map-tab org-agenda-mode-map todo)
  (keyamp--remap org-agenda-mode-map
    '((keyamp-insert       . org-agenda-switch-to-task)
      (keyamp-escape       . screen-idle-escape)
      (make-frame-command  . delete-frame)
      (insert-space-before . ignore)
      (backward-del-word   . sun-moon)
      (undo                . del-win)
      (del-word            . toggle-gnus)
      (cut-text-block      . calc)
      (goto-match-br       . view-messages)
      (shrink-whitespaces  . calendar-split)
      (open-line           . prev-buf)
      (del-back            . alt-buf)
      (newline             . next-buf)
      (activate-region     . new-empty-buffer)
      (previous-line       . up-line)
      (next-line           . down-line)
      (toggle-comment      . ignore)
      (cut-line            . prev-eww-buffer)
      (paste-or-prev       . tasks)
      (backward-bracket    . downloads)
      (forward-bracket     . save-close-buf)
      (kmacro-helper       . config)
      (copy-to-register    . sql)
      (forw-char           . screen-idle-escape)
      (back-char           . screen-idle-return)
      (beg-of-line         . bookmark-jump)
      (end-of-line         . recentf-open-files))))

(defvar screen-idle-keymap (make-sparse-keymap))
(keyamp--map-leader screen-idle-keymap '(up-line . down-line))
(keyamp--map-tab screen-idle-keymap novel)
(keyamp--map screen-idle-keymap
  '(("<right>" . screen-idle-escape) ("<left>" . screen-idle-return)
    ("<up>"    . view-messages)      ("<down>" . down-line)))

(advice-add 'delete-other-windows :after
            (lambda (&rest r)
              (if (eq major-mode 'org-agenda-mode)
                  (set-transient-map screen-idle-keymap))))

(defvar screen-idle-escape-keymap (make-sparse-keymap))
(keyamp--map-tab screen-idle-escape-keymap keyamp-screen-TAB)
(keyamp--map screen-idle-escape-keymap
  '(("<left>" . screen-idle) ("<right>" . screen-idle-return)
    ("<up>"   . toggle-ibuffer)))
(keyamp--set-map screen-idle-escape-keymap '(screen-idle-escape novel))

(with-eval-after-load 'org-keys
  (keyamp--remap org-mouse-map '((org-open-at-mouse . mouse-set-point))))

(with-eval-after-load 'eww
  (keyamp--map-tab eww-mode-map page-dn-half)
  (keyamp--map eww-mode-map '(("<left-fringe> <mouse-1>" . page-dn-half)))
  (keyamp--remap eww-mode-map
    '((open-line                . eww-back-url)
      (newline                  . eww-next-url)
      (del-back                 . eww-reload)
      (del-word                 . eww-reload-all)
      (undo                     . justify-buffer)
      (cut-text-block           . eww-copy-page-url)
      (shrink-whitespaces       . eww-browse-with-external-browser)
      (backward-bracket         . downloads)
      (forward-bracket          . ignore)
      (isearch-cur-word-forward . what-cursor-position)
      (backward-char            . back-word)
      (forward-char             . forw-word)
      (previous-line            . up-line)
      (next-line                . down-line)))
  (keyamp--remap eww-link-keymap '((keyamp-insert . eww-follow-link))))

(with-eval-after-load 'emms
  (with-sparse-keymap-x
   (keyamp--map-leader x '(open-line . newline))
   (keyamp--remap x
     '((open-line         . emms-seek-backward-or-previous)
       (del-back          . emms-pause)
       (cut-line          . emms-random)
       (newline           . emms-seek-forward-or-next)
       (backward-del-word . emms-seek-backward)
       (del-word          . emms-seek-forward)
       (forward-bracket   . ignore)))
   (keyamp--set-map x
     '(emms-seek-backward-or-previous emms-seek-forward-or-next
       emms-playlist-mode-play-smart  emms-pause emms-random)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(open-line . newline))
   (keyamp--remap x
     '((open-line . emms-seek-backward) (newline . emms-seek-forward)))
   (keyamp--set-map x '(emms-seek-backward emms-seek-forward))))

(with-eval-after-load 'emms-playlist-mode
  (keyamp--remap emms-playlist-mode-map
    '((keyamp-insert     . emms-playlist-mode-play-smart)
      (mouse-set-point   . emms-playlist-mode-play-smart)
      (open-line         . emms-seek-backward-or-previous)
      (newline           . emms-seek-forward-or-next)
      (undo              . del-win)
      (backward-del-word . emms-seek-backward)
      (del-word          . emms-seek-forward)
      (del-back          . emms-playlist-mode-center-current)
      (backward-bracket  . dired-jump)
      (forward-bracket   . ignore))))

(with-eval-after-load 'flyspell
  (with-sparse-keymap-x
   (keyamp--map-leader x '(open-line . newline))
   (keyamp--remap x
     '((del-back  . ispell-word)
       (open-line . flyspell-goto-prev-error)
       (newline   . flyspell-goto-next-error)))
   (keyamp--set-map x
     '(flyspell-buffer          ispell-word
       flyspell-goto-prev-error flyspell-goto-next-error))))

(with-eval-after-load 'doc-view
  (keyamp--map doc-view-mode-map '(("C-r" . delete-other-windows)))
  (keyamp--remap doc-view-mode-map
    '((keyamp-insert  . keyamp-escape)
      (previous-line  . doc-view-previous-line-or-previous-page)
      (next-line      . doc-view-next-line-or-next-page)
      (up-line        . doc-view-previous-line-or-previous-page)
      (down-line      . doc-view-next-line-or-next-page)
      (backward-char  . doc-view-previous-page)
      (forward-char   . doc-view-next-page)
      (enlarge-window . doc-view-enlarge)
      (beg-of-line    . doc-view-scroll-down-or-previous-page)
      (end-of-lyne    . doc-view-scroll-up-or-next-page)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(doc-view-shrink . doc-view-enlarge))
   (keyamp--set-map x '(doc-view-shrink doc-view-enlarge) nil nil nil 2))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . doc-view-scroll-down-or-previous-page)
       (next-line     . doc-view-scroll-up-or-next-page)))
   (keyamp--set-map x
     '(doc-view-scroll-down-or-previous-page doc-view-scroll-up-or-next-page)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . doc-view-scroll-down-or-previous-page)
       (next-line     . doc-view-scroll-up-or-next-page)
       (up-line       . doc-view-scroll-down-or-previous-page)
       (down-line     . doc-view-scroll-up-or-next-page)))
   (keyamp--set-map x
     '(doc-view-previous-line-or-previous-page doc-view-next-line-or-next-page))))

(with-eval-after-load 'image-mode
  (keyamp--remap image-mode-map
    '((keyamp-insert . keyamp-escape)
      (backward-char . image-previous-file) (forward-char  . image-next-file)
      (previous-line . image-decrease-size) (next-line     . image-increase-size)
      (undo          . image-dired)         (del-back      . image-rotate)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(backward-char . forward-char))
   (keyamp--set-map x '(image-previous-file image-next-file))
   (keyamp--set-map-hook x '(image-mode-hook)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--set-map x '(image-decrease-size image-increase-size))))

(with-eval-after-load 'esh-mode
  (keyamp--map-tab eshell-mode-map completion-at-point)
  (keyamp--map-backtab eshell-mode-map eshell-search-input)
  (keyamp--map eshell-mode-map
    '(("C-h" . eshell-interrupt-process) ("C-t" . delete-other-windows)))
  (keyamp--remap eshell-mode-map
    '((cut-line        . eshell-clear-input)
      (next-eww-buffer . eshell-clear)
      (select-block    . eshell-previous-input)
      (quoted-insert   . eshell-interrupt-process)
      (eshell-split    . delete-other-windows)
      (open-line       . ignore)
      (newline         . ignore)
      (toggle-comment  . ignore)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . eshell-previous-input) (next-line     . eshell-next-input)
       (undo          . delete-window)         (keyamp-insert . eshell-send-input)))
   (keyamp--set-map x
     '(eshell-previous-input eshell-next-input eshell-search-input) :command))

  (with-sparse-keymap-x
   ;; Insert mode primary for eshell. The keymap ready after eshell start,
   ;; command submit or cancel. DEL to list history, SPC to paste.
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--map-tab x change-wd)
   (keyamp--map-backtab x eshell-search-input)
   (keyamp--remap x
     '((previous-line . eshell-previous-input)
       (next-line     . paste-or-prev)
       (undo          . delete-window)
       (eshell-split  . delete-other-windows)))
   (keyamp--set-map x '(eshell-send-input eshell-interrupt-process) nil :insert)
   (defun keyamp-input-timer-payload-eshell ()
     (if (eq major-mode 'eshell-mode) (keyamp-set-deactivate-repeat-fun x)))
   (advice-add 'keyamp-input-timer-payload :after 'keyamp-input-timer-payload-eshell)
   (keyamp--set-map-hook x '(eshell-mode-hook) nil :insert))
  (add-hook 'eshell-mode-hook 'keyamp-input-timer-post-command)
  (advice-add-macro '(eshell-send-input eshell-interrupt-process)
                    :after 'keyamp-input-timer-post-command))

(advice-add-macro
 ;; Activate command mode after jump from insert.
 '(alt-buf  delete-other-windows delete-window split-window-below
   prev-buf next-buf             save-close-buf dired-jump)
 :after 'keyamp-command-if-insert)

(with-eval-after-load 'vterm
  (keyamp--map-tab vterm-mode-map vterm-send-tab)
  (keyamp--map vterm-mode-map
    '(("C-h" . term-interrupt-subjob) ("C-q" . term-interrupt-subjob)
      ("C-r" . delete-other-windows)  ("C-t" . delete-other-windows)))

  (keyamp--remap vterm-mode-map
    '((select-block        . vterm-up)
      (prev-eww-buffer     . vterm-clear)
      (paste-or-prev       . vterm-yank)
      (paste-from-r1       . vterm-yank-pop)
      (backward-del-word   . vterm-backward-kill-word)
      (toggle-comment      . vterm-send-next-key)
      (open-line           . ignore)
      (newline             . ignore)
      (cut-line            . ignore)
      (insert-space-before . ignore)
      (del-back            . vterm-send-backspace)
      (terminal-split      . delete-other-windows)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(vterm-send-backspace . keyamp-insert-and-SPC))
   (keyamp--set-map x '(vterm-send-backspace) nil nil nil 1))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . vterm-up)      (next-line     . vterm-down)
       (undo          . delete-window) (keyamp-insert . vterm-send-return)))
   (keyamp--set-map x '(vterm-history-search) nil :insert)
   (keyamp--set-map x '(vterm-up vterm-down) :command))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--map-tab x change-wd)
   (keyamp--map-backtab x vterm-history-search)
   (keyamp--remap x
     '((previous-line . vterm-up) (next-line . vterm-yank) (undo . delete-window)
       (terminal-split . delete-other-windows)))
   (keyamp--set-map-hook x '(vterm-mode-hook) nil :insert)
   (keyamp--set-map x '(vterm-send-return term-interrupt-subjob) nil :insert)
   (defun keyamp-input-timer-payload-vterm ()
     (if (eq major-mode 'vterm-mode) (keyamp-set-deactivate-repeat-fun x)))
   (advice-add 'keyamp-input-timer-payload :after 'keyamp-input-timer-payload-vterm))
  (add-hook 'vterm-mode-hook 'keyamp-input-timer-post-command)
  (advice-add-macro '(vterm-send-return term-interrupt-subjob)
                    :after 'keyamp-input-timer-post-command))

(with-eval-after-load 'info
  (keyamp--remap Info-mode-map
    '((keyamp-insert . Info-follow-nearest-node)
      (open-line     . Info-backward-node)
      (newline       . Info-forward-node)
      (undo          . Info-up)
      (del-back      . Info-next-reference)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(open-line . newline))
   (keyamp--set-map x '(Info-backward-node Info-forward-node)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(Info-prev-reference . Info-next-reference))
   (keyamp--set-map x '(Info-prev-reference Info-next-reference))))

(with-eval-after-load 'help-mode
  (push 'help-mode keyamp-button-move-modes)
  (keyamp--remap help-mode-map
    '((undo          . button-back)  (del-back . button-forw)
      (open-line     . help-go-back) (newline  . help-go-forward)
      (keyamp-insert . keyamp-escape))))

(with-eval-after-load 'gnus-topic
  (keyamp--map-tab gnus-topic-mode-map gnus-topic-select-group)
  (keyamp--map gnus-topic-mode-map '(("<mouse-1>" . gnus-topic-select-group)))
  (keyamp--remap gnus-topic-mode-map
    '((keyamp-insert . gnus-topic-select-group)
      (previous-line . up-line)            (next-line     . down-line)
      (beg-of-line   . gnus-topic-prev)    (end-of-lyne   . gnus-topic-next)
      (beg-of-block  . gnus-topic-prev)    (end-of-block  . gnus-topic-next)
      (back-char     . screen-idle-escape) (forw-char     . screen-idle)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x '((previous-line . up-line) (next-line . down-line)))
   (keyamp--set-map x '(gnus-topic-select-group)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(previous-line . next-line))
   (keyamp--remap x
     '((previous-line . gnus-topic-prev) (next-line    . gnus-topic-next)
       (beg-of-line   . gnus-beg-of-buf) (end-of-lyne  . gnus-end-of-buf)
       (end-of-block  . gnus-beg-of-buf) (beg-of-block . gnus-end-of-buf)))
   (keyamp--set-map x
     '(gnus-topic-prev gnus-topic-next
       gnus-beg-of-buf gnus-end-of-buf) nil nil nil 2)))

(with-eval-after-load 'gnus-group
  (keyamp--remap gnus-group-mode-map
    '((back-char         . screen-idle-escape)
      (forw-char         . screen-idle)
      (backward-del-word . sun-moon)
      (undo              . del-win)
      (del-word          . gnus-group-enter-server-mode)
      (cut-text-block    . calc)
      (goto-match-br     . view-messages)
      (open-line         . prev-buf)
      (del-back          . gnus-group-get-new-news)
      (newline           . next-buf)
      (activate-region   . new-empty-buffer)
      (cut-line          . prev-eww-buffer)
      (copy-line         . screen-idle)
      (paste-or-prev     . tasks)
      (backward-bracket  . downloads)
      (forward-bracket   . save-close-buf)
      (kmacro-helper     . config)
      (copy-to-register  . sql))))

(with-eval-after-load 'gnus-art
  (push 'gnus-article-mode keyamp-lines-move-modes)
  (keyamp--remap gnus-mime-button-map '((keyamp-insert . gnus-article-press-button)))
  (keyamp--remap gnus-article-mode-map
    '((undo . button-back) (del-back . button-forw))))

(with-eval-after-load 'gnus-sum
  (push 'gnus-summary-mode keyamp-lines-move-modes)
  (keyamp--map gnus-summary-mode-map
    '(("C-h" . gnus-summary-delete-article) ("<mouse-1>" . gnus-summary-scroll-up)))
  (keyamp--map-tab gnus-summary-mode-map page-dn-half)
  (keyamp--remap gnus-summary-mode-map
    '((previous-line . up-line)
      (next-line     . down-line)
      (back-char     . screen-idle-escape)
      (forw-char     . screen-idle)
      (keyamp-insert . gnus-summary-scroll-up)
      (open-line     . gnus-summary-prev-group)
      (newline       . gnus-summary-next-group)
      (paste-or-prev . tasks)
      (paste-from-r1 . gnus-summary-save-parts)
      (save-buffer   . gnus-summary-save-parts)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(up-line . down-line))
   (keyamp--map-tab x keyamp-screen-TAB)
   (keyamp--remap x
     '((open-line . gnus-summary-prev-group) (newline . gnus-summary-next-group)
       (up-line  . toggle-gnus)))
   (keyamp--set-map x
     '(gnus-summary-prev-group gnus-summary-next-group gnus-delete-window-article
       screen-idle-return))
   (keyamp--set-map-hook x '(gnus-summary-prepared-hook) nil nil :repeat))

  (with-sparse-keymap-x
   ;; Press RET to open an article then RET again to close it.
   ;; SPC/DEL to switch to the article then SPC/DEL to continue move by links.
   (keyamp--map-leader x '(other-window . other-window))
   (keyamp--remap x '((keyamp-insert . gnus-delete-window-article)))
   (keyamp--set-map x '(gnus-summary-scroll-up))))

(with-eval-after-load 'gnus-srvr
  (keyamp--remap gnus-server-mode-map
    '((keyamp-insert . gnus-server-read-server) (del-back . gnus-server-exit)))
  (keyamp--remap gnus-browse-mode-map
    '((keyamp-insert . gnus-browse-select-group))))

(with-eval-after-load 'recentf
  (keyamp--remap recentf-dialog-mode-map
    '((keyamp-escape    . recentf-cancel-dialog)
      (eshell-split     . recentf-open-most-recent-file-0)
      (kmacro-record    . recentf-open-most-recent-file-1)
      (kmacro-play      . recentf-open-most-recent-file-2)
      (kmacro-helper    . recentf-open-most-recent-file-3)
      (append-to-r1     . recentf-open-most-recent-file-4)
      (terminal         . recentf-open-most-recent-file-5)
      (pass             . recentf-open-most-recent-file-6)
      (jump-to-register . recentf-open-most-recent-file-7)
      (copy-to-register . recentf-open-most-recent-file-8)
      (proced           . recentf-open-most-recent-file-9)))
  (with-sparse-keymap-x
   (keyamp--map-leader x '(widget-backward . widget-forward))
   (keyamp--set-map x '(recentf-open-files))))

(with-sparse-keymap-x
 (keyamp--remap x
   '((eshell-split     . radio-channel-0) (kmacro-record    . radio-channel-1)
     (kmacro-play      . radio-channel-2) (kmacro-helper    . radio-channel-3)
     (append-to-r1     . radio-channel-4) (terminal         . radio-channel-5)
     (pass             . radio-channel-6) (jump-to-register . radio-channel-7)
     (copy-to-register . radio-channel-8) (proced           . radio-channel-9)))
 (keyamp--set-map x
   '(radio radio-next radio-prev radio-channel-0
     radio-channel-1 radio-channel-2 radio-channel-3
     radio-channel-4 radio-channel-5 radio-channel-6
     radio-channel-7 radio-channel-8 radio-channel-9)))

(with-eval-after-load 'snake
  (keyamp--remap snake-mode-map
    '((keyamp-escape . snake-pause-game) (keyamp-insert . snake-pause-game)
      (del-back      . snake-move-up)    (next-line     . snake-move-down)))
  (keyamp--remap snake-null-map
    '((keyamp-escape . snake-start-game) (keyamp-insert . snake-start-game)))
  (with-sparse-keymap-x
   (keyamp--map-leader x '(snake-move-left . snake-move-right))
   (keyamp--set-map x
     '(snake-start-game snake-pause-game snake-move-left  snake-move-right
       snake-move-down  snake-move-up))
   (keyamp--set-map-hook x '(snake-mode-hook))))

(with-eval-after-load 'tetris
  (keyamp--remap tetris-mode-map
    '((keyamp-escape . tetris-pause-game)
      (del-back      . tetris-rotate-prev) (alt-buf       . tetris-rotate-prev)
      (newline       . tetris-rotate-next) (next-buf      . tetris-rotate-next)
      (next-line     . tetris-move-bottom) (backward-char . tetris-move-down)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(tetris-move-left . tetris-move-right))
   (keyamp--set-map x
     '(tetris-start-game  tetris-pause-game tetris-move-left   tetris-move-right
       tetris-rotate-prev tetris-rotate-next tetris-move-bottom tetris-move-down))))

(with-eval-after-load 'js-mode
  (keyamp--map-tab js-mode-map js-leader-map)
  (keyamp--map-tab (define-prefix-command 'js-leader-map) js-complete-or-indent)
  (keyamp--map js-leader-map
    '(("h" . typescript-compile-file) ("," . js-eval-line) ("." . js-eval-region))))

(with-eval-after-load 'css-mode
  (keyamp--map-tab css-mode-map css-leader-map)
  (keyamp--map-tab (define-prefix-command 'css-leader-map) css-complete-or-indent)
  (keyamp--map css-leader-map
    '(("'" . css-hex-color-to-hsl)     ("a" . css-complete-symbol)
      ("h" . css-format-compact)       ("p" . css-format-compact-buffer)
      ("o" . css-format-expand-buffer) ("k" . css-format-expand)))
  (keyamp--remap css-mode-map '((open-line . css-smart-newline))))

(with-eval-after-load 'html-mode
  (keyamp--map-return html-mode-map html-open-local-link)
  (keyamp--map html-mode-map '(("C-r" . html-open-in-browser)))
  (keyamp--map-tab html-mode-map html-leader-map)
  (keyamp--map-tab (define-prefix-command 'html-leader-map) html-insert-tag)
  (keyamp--map-return html-leader-map html-insert-br-tag)
  (keyamp--map html-leader-map
    '(("<left>"  . html-prev-opening-tag)
      ("<right>" . html-next-opening-tag)
      ("<down>"  . html-goto-matching-tag)

      ("@" . html-encode-ampersand-entity)
      ("$" . html-percent-decode-url)
      ("&" . html-decode-ampersand-entity)
      ("q" . html-make-citation)

      ("w"   . nil) ; required
      ("w ," . html-rename-source-file-path)
      ("w h" . html-resize-img)
      ("w q" . html-image-path-to-figure-tag)
      ("w j" . html-image-to-link)
      ("w w" . html-image-to-img-tag)
      ("w c" . html-convert-to-jpg)
      ("w o" . html-move-image-file)

      ("e" . html-remove-tag-pair)
      ("r" . html-mark-unicode)
      ("y" . html-lines-to-table)
      ("u" . html-emacs-to-windows-kbd-notation)
      ("i" . html-all-urls-to-link)
      ("o" . html-insert-pre-tag)
      ("[" . html-percent-encode-url)

      ("a i" . html-promote-header)
      ("a k" . html-demote-header)
      ("a e" . html-remove-tags)
      ("a q" . html-compact-def-list)
      ("a ." . html-remove-list-tags)
      ("a f" . html-remove-paragraph-tags)
      ("a ," . html-format-to-multi-lines)
      ("a l" . html-disable-script-tag)
      ("a a" . html-update-title-h1)
      ("a y" . html-remove-table-tags)
      ("a h" . html-change-current-tag)

      ("s" . html-html-to-text)
      ("d" . html-select-element)
      ("f" . html-blocks-to-paragraph)
      ("h" . html-lines-to-list)
      ("j" . html-any-to-link)

      ("k"   . nil)
      ("k e" . html-dehtmlize-pre-tags)
      ("k h" . html-bracket-to-markup)
      ("k j" . html-pre-tag-to-new-file)
      ("k ;" . html-htmlize-region)
      ("k ," . html-rehtmlize-precode-buffer)
      ("k k" . html-toggle-syntax-color-tags)

      ("l" . html-insert-date-section)
      ("x" . html-lines-to-def-list)
      ("c" . html-join-tags)
      ("v" . html-keyboard-shortcut-markup)
      ("b" . html-make-link-defunct)

      ("m i" . html-ampersand-chars-to-unicode)
      ("m h" . html-clone-file-in-link)
      ("m d" . html-url-to-dated-link)
      ("m w" . html-url-to-iframe-link)
      ("m j" . html-local-links-to-relative-path)
      ("m f" . html-pdf-path-to-embed)
      ("m ," . html-named-entity-to-char)
      ("m k" . html-local-links-to-fullpath)

      (","   . html-extract-url)
      ("."   . html-word-to-anchor-tag)
      ("/ ," . html-open-in-brave)
      ("/ l" . html-open-in-safari))))

(with-eval-after-load 'find-replace
  (keyamp--map-tab find-output-mode-map find-next-match)
  (keyamp--map-backtab find-output-mode-map find-previous-match)
  (keyamp--remap find-output-mode-map '((keyamp-insert . find--jump-to-place)))

  (with-sparse-keymap-x
   (keyamp--map-leader x '(backward-char . forward-char))
   (keyamp--remap x
     '((previous-line . find-previous-file)  (next-line    . find-next-file)
       (backward-char . find-previous-match) (forward-char . find-next-match)))
   (keyamp--set-map x
     '(find-next-match find-previous-file find-previous-match find-next-file))
   (keyamp--set-map-hook x '(find-output-mode-hook) nil nil :repeat)))

(with-eval-after-load 'emacs-lisp-mode
  (keyamp--map-tab emacs-lisp-mode-map emacs-lisp-indent)
  (keyamp--remap emacs-lisp-mode-map
    '((reformat-lines . emacs-lisp-remove-paren-pair))))

(with-sparse-keymap-x
 (keyamp--map-leader x '(backward-char . forward-char))
 (keyamp--remap x
   '((backward-char . flymake-goto-prev-error)
     (forward-char  . flymake-goto-next-error)
     (back-word     . flymake-goto-prev-error)
     (forw-word     . flymake-goto-next-error)))
 (keyamp--set-map x '(flymake-goto-prev-error flymake-goto-next-error)))

(with-eval-after-load 'python-mode
  (keyamp--map-tab python-mode-map python-indent-or-complete)
  (keyamp--map-backtab python-mode-map python-de-indent)
  (keyamp--map-return python-mode-map python-return-and-indent)
  (keyamp--remap python-mode-map '((newline . python-return-and-indent))))

(with-eval-after-load 'go-ts-mode
  (keyamp--map-tab go-ts-mode-map company-manual-begin)
  (keyamp--remap go-ts-mode-map
    '((describe-foo-at-point . xref-find-definitions)
      (describe-variable     . xref-find-references)
      (mark-defun            . go-mark-defun)
      (eval-defun            . flymake-show-project-diagnostics)
      (eval-region-or-sexp   . server)
      (reformat-lines        . eglot-reconnect))))

(with-sparse-keymap-x
 (keyamp--map-leader x '(xref-go-back . xref-find-definitions))
 (keyamp--set-map x '(xref-go-back xref-find-definitions)))

(with-eval-after-load 'xref
  (keyamp--remap xref--xref-buffer-mode-map
    '((keyamp-insert . xref-show-location-at-point))))

(with-eval-after-load 'sh-script
  (keyamp--map-tab bash-ts-mode-map indent-for-tab-command)
  (keyamp--map-tab sh-mode-map indent-for-tab-command))

(with-eval-after-load 'sqlite-mode
  (keyamp--remap sqlite-mode-map
    '((keyamp-insert . sqlite-mode-list-data) (del-back . sqlite-mode-delete)
      (newline . sqlite-mode-list-columns) (open-line . sqlite-mode-list-tables))))

(with-eval-after-load 'sql
  (keyamp--remap sql-mode-map '((eval-defun . exec-query) (sql . toggle-sql-type)))
  (with-sparse-keymap-x
   (keyamp--remap x '((copy-to-register . toggle-sql-type)))
   (keyamp--set-map x '(sql toggle-sql-type exec-query))))

(with-eval-after-load 'speedbar
  (keyamp--remap speedbar-mode-map
    '((newline            . speedbar-refresh)))
  (keyamp--map speedbar-file-key-map
    '(("<mouse-2>"        . speedbar-toggle-line-expansion)
      ("<double-mouse-1>" . speedbar-edit-line)))
  (keyamp--remap speedbar-file-key-map
    '((keyamp-insert      . speedbar-toggle-line-expansion)
      (undo               . speedbar-up-directory)
      (del-back           . speedbar-edit-line))))

(with-eval-after-load 'calendar
  (keyamp--remap calendar-mode-map
    '((beg-of-line . calendar-scroll-right) (end-of-lyne   . calendar-scroll-left)
      (del-back    . calendar-goto-today)   (keyamp-insert . org-calendar-select)
      (undo        . del-win)))
  (with-sparse-keymap-x
   (keyamp--map-leader x '(calendar-scroll-right . calendar-scroll-left))
   (keyamp--map-tab x calendar-other-month)
   (keyamp--set-map x '(calendar-scroll-left calendar-scroll-right calendar-goto-today))))

(with-eval-after-load 'simple
  (keyamp--remap messages-buffer-mode-map
    '((keyamp-insert     . keyamp-escape)
      (undo              . del-win)
      (del-back          . alt-buf)
      (open-line         . prev-buf)
      (newline           . next-buf)
      (paste-or-prev     . tasks)
      (previous-line     . up-line)
      (next-line         . down-line)
      (backward-del-word . sun-moon)
      (cut-text-block    . calc)
      (cut-line          . prev-eww-buffer)
      (backward-bracket  . downloads)
      (forward-bracket   . save-close-buf)
      (kmacro-helper     . config)
      (copy-to-register  . sql)))
  (keyamp--remap special-mode-map
    '((undo              . del-win)
      (del-back          . alt-buf)
      (open-line         . prev-buf)
      (newline           . next-buf))))

(with-eval-after-load 'calc
  (setq keyamp-minibuffer-insert-commands `(,keyamp-minibuffer-insert-commands
          calcDigit-start calc-execute-extended-command calc-algebraic-entry))
  (advice-add 'calcDigit-start :after 'keyamp-insert)
  (advice-add 'calcDigit-start :after 'keyamp-input-timer-post-command))
  (advice-add-macro
    '(calc-plus calc-minus calc-times calc-divide
      calc-mod  calc-inv   calc-power calc-enter) :after 'keyamp-start-input-timer)

(with-eval-after-load 'calc-ext
  (keyamp--remap calc-mode-map
    '((del-back      . calc-pop)       (undo    . calc-undo)
      (open-line     . calc-roll-down) (newline . calc-algebraic-entry)
      (paste-or-prev . calc-yank)))
  (with-sparse-keymap-x
   (keyamp--map-leader x '(undo . del-back))
   (keyamp--remap x '((del-back . calc-redo)))
   (keyamp--set-map x '(calc-undo calc-redo))
   (defun keyamp-input-timer-payload-calc ()
     (if (eq major-mode 'calc-mode) (keyamp-set-deactivate-repeat-fun x)))
   (advice-add 'keyamp-input-timer-payload :after 'keyamp-input-timer-payload-calc)))



(defconst keyamp-screen-commands-hash
  #s(hash-table test equal data
                (async-shell-command              t
                 calendar-split                   t
                 clock                            t
                 config                           t
                 delete-other-windows             t
                 delete-window                    t
                 describe-char                    t
                 describe-face                    t
                 describe-foo-at-point            t
                 describe-function                t
                 describe-key                     t
                 describe-mode                    t
                 describe-variable                t
                 exec-query                       t
                 find-next-dir-file               t
                 find-prev-dir-file               t
                 list-matching-lines              t
                 next-buffer                      t
                 next-eww-buffer                  t
                 next-proj-buf                    t
                 next-buf                         t
                 occur-cur-word                   t
                 open-in-external-app             t
                 org-agenda-switch-to-task        t
                 player                           t
                 prev-eww-buffer                  t
                 prev-proj-buf                    t
                 prev-buf                         t
                 previous-buffer                  t
                 server                           t
                 split-window-below               t
                 sun-moon                         t
                 sync                             t
                 tasks                            t
                 test                             t
                 view-messages                    t)))

(defconst keyamp-edit-commands-hash
  #s(hash-table test equal data
                (apply-macro-to-region-lines      t
                 backward-del-word                t
                 clean-whitespace                 t
                 cut-line                         t
                 cycle-hyphen-lowline-space       t
                 del-back                         t
                 del-word                         t
                 delete-duplicate-lines           t
                 delete-forward-char              t
                 delete-matching-lines            t
                 delete-non-matching-line         t
                 dired-toggle-mark                t
                 emoji-insert                     t
                 eval-defun                       t
                 eval-region-or-sexp              t
                 fill-or-unfil                    t
                 ibuffer-do-delete                t
                 increment-register               t
                 insert-ascii-double-quote        t
                 insert-ascii-single-quote        t
                 insert-backtick-quote            t
                 insert-brace                     t
                 insert-char                      t
                 insert-column-a-z                t
                 insert-date                      t
                 insert-double-angle-quote        t
                 insert-double-curly-quote        t
                 insert-emacs-quote               t
                 insert-formfeed                  t
                 insert-paren                     t
                 insert-space-before              t
                 insert-square-bracket            t
                 json-pretty                      t
                 kill-region                      t
                 new-empty-buffer                 t
                 newline                          t
                 open-line                        t
                 org-insert-source-code           t
                 org-open-line                    t
                 org-shiftdown                    t
                 org-shiftup                      t
                 quote-lines                      t
                 reformat-lines                   t
                 reformat-to-sentence-lines       t
                 run-current-file                 t
                 save-buffer                      t
                 save-close-buf                   t
                 shrink-whitespaces               t
                 sort-lines-key-value             t
                 space-to-newline                 t
                 title-case-region-or-line        t
                 todo                             t
                 toggle-comment                   t
                 toggle-letter-case               t
                 toggle-prev-letter-case          t
                 undo                             t
                 vterm-send-backspace             t
                 yank                             t)))

(defconst keyamp-repeat-commands-hash
  #s(hash-table test equal data
                (back-word                        t
                 button-back                      t
                 back-char                        t
                 backward-punct                   t
                 beg-of-block                     t
                 beg-of-buf                       t
                 calc-redo                        t
                 calc-undo                        t
                 calendar-goto-today              t
                 calendar-scroll-left             t
                 calendar-scroll-right            t
                 company-manual-begin             t
                 company-next-page                t
                 company-previous-page            t
                 company-select-forw              t
                 company-select-back              t
                 comp-back                        t
                 comp-forw                        t
                 copy-line                        t
                 dired-jump                       t
                 dired-mark                       t
                 dired-unmark                     t
                 down-line                        t
                 emms-pause                       t
                 emms-playlist-mode-play-smart    t
                 emms-random                      t
                 emms-seek-backward               t
                 emms-seek-backward-or-previous   t
                 emms-seek-forward                t
                 emms-seek-forward-or-next        t
                 end-of-block                     t
                 end-of-buf                       t
                 enlarge-window                   t
                 eshell-next-input                t
                 eshell-previous-input            t
                 eshell-search-input              t
                 select-word                      t
                 find-next-file                   t
                 find-next-match                  t
                 find-previous-file               t
                 find-previous-match              t
                 flymake-goto-next-error          t
                 flymake-goto-prev-error          t
                 forw-char                        t
                 forw-word                        t
                 button-forw                      t
                 forward-punct                    t
                 gnus-beg-of-buf                  t
                 gnus-delete-window-article       t
                 gnus-end-of-buf                  t
                 gnus-summary-next-group          t
                 gnus-summary-prev-group          t
                 gnus-summary-scroll-up           t
                 gnus-topic-next                  t
                 gnus-topic-prev                  t
                 gnus-topic-select-group          t
                 page-up-half                     t
                 page-dn-half                     t
                 hist-forw                        t
                 hist-back                        t
                 ibuffer-backward-filter-group    t
                 ibuffer-forward-filter-group     t
                 ibuffer-toggle-filter-group      t
                 ido-next-match                   t
                 ido-prev-match                   t
                 image-next-file                  t
                 image-previous-file              t
                 Info-backward-node               t
                 Info-forward-node                t
                 Info-prev-reference              t
                 Info-next-reference              t
                 isearch-cur-word-backward        t
                 isearch-cur-word-forward         t
                 isearch-back                     t
                 isearch-forw                     t
                 isearch-ring-advance             t
                 isearch-ring-retreat             t
                 isearch-yank-kill                t
                 jump-mark                        t
                 keyamp--repeat-dummy             t
                 radio-next                       t
                 radio-prev                       t
                 recenter-top-bottom              t
                 recentf-open-files               t
                 screen-idle                      t
                 screen-idle-return               t
                 scroll-down-command              t
                 scroll-up-command                t
                 select-block                     t
                 select-line                      t
                 select-quote                     t
                 shrink-window                    t
                 text-scale-decrease              t
                 text-scale-increase              t
                 text-scale-reset                 t
                 translate                        t
                 up-line                          t
                 vterm-down                       t
                 vterm-send-return                t
                 vterm-up                         t
                 widget-backward                  t
                 widget-forward                   t
                 xref-find-definitions            t
                 xref-go-back                     t)))



(defgroup keyamp nil "Customization options for keyamp"
  :group 'help :prefix "keyamp-")

(defvar keyamp-command-hook nil "Hook for `keyamp-command'")
(defvar keyamp-insert-hook  nil "Hook for `keyamp-insert'")

(defconst keyamp-karabiner-cli
  "/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli"
  "Karabiner-Elements CLI executable. Optional for mode sync.")

(defconst keyamp-idle-indicator    "●︎" "Idle.")
(defconst keyamp-screen-indicator  "●︎" "Screen.")
(defconst keyamp-repeat-indicator  "●︎" "Read.")
(defconst keyamp-command-indicator "●︎" "Command.")
(defconst keyamp-io-indicator      "●︎" "IO.")
(defconst keyamp-insert-indicator  "●︎" "Insert.")
(defconst keyamp-edit-indicator    "●︎" "Modify.")

(defconst keyamp-idle-color    "#ab82ff" "Idle.")
(defconst keyamp-screen-color  "#0000ff" "Screen.")
(defconst keyamp-repeat-color  "#00bfff" "Read.")
(defconst keyamp-command-color "#7cfc00" "Command.")
(defconst keyamp-io-color      "#ffd700" "IO.")
(defconst keyamp-insert-color  "#ff8c00" "Insert.")
(defconst keyamp-edit-color    "#ff0000" "Modify.")

(defface mode-line-front-space-face `((t :foreground ,keyamp-command-color))
  "Mode line front space face.")

(defconst keyamp-command-cursor 'box        "Command cursor.")
(defconst keyamp-insert-cursor  '(hbar . 2) "Insert cursor.")
(defconst keyamp-repeat-cursor  'hollow     "Repeat cursor.")
(defconst keyamp-screen-cursor  nil         "Screen cursor.")
(defconst keyamp-edit-cursor    '(bar . 2)  "Modify cursor.")

(defvar keyamp-indicate-repeat-timer nil "Indicate repeat timer.")
(defconst keyamp-indicate-repeat-delay 1 "Repeat cursor change delay.")

(defconst keyamp-idle-timeout 60
  "Idle timeout for keymaps without self timeout.")
(defconst keyamp-defer-load 5 "Defer load second priority features.")


;; Input timer

(defconst keyamp-input-timeout 3 "Input timeout.")
(defvar keyamp-input-timer nil
  "Timer activates repeat mode if no command follows. Any command or self
insert cancels the timer.")

(defun keyamp-cancel-input-timer ()
  "Cancel `keyamp-input-timer'."
  (remove-hook 'post-command-hook 'keyamp-cancel-input-timer)
  (remove-hook 'post-self-insert-hook 'keyamp-cancel-input-timer)
  (if (timerp keyamp-input-timer) (cancel-timer keyamp-input-timer)))

(defun keyamp-input-timer-payload ()
  "Payload for `keyamp-input-timer'."
  (keyamp-cancel-input-timer) (keyamp-command) (keyamp-indicate-repeat))

(defun keyamp-start-input-timer (&rest r)
  "Start `keyamp-input-timer'."
  (keyamp-cancel-input-timer)
  (remove-hook 'post-command-hook 'keyamp-start-input-timer)
  (add-hook 'post-command-hook 'keyamp-cancel-input-timer)
  (add-hook 'post-self-insert-hook 'keyamp-cancel-input-timer)
  (setq keyamp-input-timer
        (run-with-timer keyamp-input-timeout nil 'keyamp-input-timer-payload)))

(defun keyamp-input-timer-post-command (&rest r)
  "Add `keyamp-start-input-timer' to `post-command-hook'."
  (add-hook 'post-command-hook 'keyamp-start-input-timer))


;; Karabiner

(defun keyamp-set-var-karabiner (var val)
  "Set karabiner VAR to VAL via shell command."
  (call-process keyamp-karabiner-cli nil 0 nil
                "--set-variables" (concat "{\"" var "\":" val "}")))

(defconst keyamp-karabiner-insert-mode "insert mode activated"
  "Karabiner keyamp insert mode variable.")

(defun keyamp-insert-karabiner ()
  "Sync insert mode with karabiner."
  (keyamp-set-var-karabiner keyamp-karabiner-insert-mode "1"))

(defun keyamp-command-karabiner ()
  "Sync command mode with karabiner."
  (keyamp-set-var-karabiner keyamp-karabiner-insert-mode "0"))

(defun keyamp-karabiner-init ()
  "Init karabiner."
  (when (executable-find keyamp-karabiner-cli)
    (add-hook 'keyamp-insert-hook 'keyamp-insert-karabiner)
    (add-hook 'keyamp-command-hook 'keyamp-command-karabiner)))


;; Modes

(defvar keyamp-insert-p t "Non-nil means insert is on.")
(defvar keyamp--deactivate-command-fun nil)

(defun keyamp-command-init ()
  "Set command mode."
  (keyamp-repeat-deactivate)
  (when keyamp-insert-p (setq keyamp-insert-p nil) (push-mark (point) t))
  (if keyamp--deactivate-command-fun (funcall keyamp--deactivate-command-fun))
  (setq keyamp--deactivate-command-fun
        (set-transient-map keyamp-command-map (lambda () t)))
  (keyamp-indicate-command))

(defun keyamp-insert-init (&rest r)
  "Enter insert mode."
  (setq keyamp-insert-p t) (funcall keyamp--deactivate-command-fun))

(defun keyamp-command ()
  "Activate command mode."
  (interactive) (keyamp-command-init) (run-hooks 'keyamp-command-hook))

(defun keyamp-insert ()
  "Activate insert mode."
  (interactive) (keyamp-insert-init) (run-hooks 'keyamp-insert-hook))

(defconst before-last-command-event-threshold (if (display-graphic-p) 0 70)
  "Threshold for `before-last-command-event'. Give time to paste into terminal.")
(defconst before-last-command-event-timeout 300
  "Timeout for `before-last-command-event'.")
(defvar before-last-command-event nil "Before `last-command-event'.")

(defun keyamp-SPC-SPC (&rest r)
  "Insert SPC SPC to activate command mode."
  (if (and (eq before-last-command-event 32) (eq last-command-event 32))
      (if isearch-mode (isearch-cancel) (delete-char -2) (keyamp-escape))
    (if (eq last-command-event 32)
        (progn
          (run-with-timer (if (and (minibufferp) (not (display-graphic-p))) 0
                            (/ before-last-command-event-threshold 1000.0)) nil
                            (lambda () (setq before-last-command-event 32)))
          (run-with-timer (/ before-last-command-event-timeout 1000.0) nil
                          (lambda () (setq before-last-command-event nil))))
      (if before-last-command-event (setq before-last-command-event nil)))))
(add-hook 'post-self-insert-hook 'keyamp-SPC-SPC)
(advice-add-macro '(isearch-printing-char minibuffer-complete-word)
                  :after 'keyamp-SPC-SPC)

(defun keyamp-SPC-DEL (&rest r)
  "Insert SPC DEL to activate command mode."
  (if (eq before-last-command-event 32)
      (if isearch-mode (isearch-cancel) (keyamp-command))))
(advice-add-macro '(delete-backward-char isearch-del-char) :after 'keyamp-SPC-DEL)

(defun keyamp-command-if-insert (&rest r)
  "Activate command mode if insert mode." (if keyamp-insert-p (keyamp-command)))

(defun keyamp-insert-and-SPC ()
  "Activate insert mode and insert SPC."
  (interactive) (unless keyamp-insert-p (keyamp-insert)) (insert " "))

(defun keyamp-minibuffer-insert ()
  "If minibuffer input not empty then confirm and exit instead
of insert mode activation."
  (interactive)
  (if (> (length (buffer-substring (minibuffer-prompt-end) (point))) 0)
      (exit-minibuffer) (keyamp-insert)))

(defun keyamp-minibuffer-escape ()
  "If minibuffer input not empty then activate command mode instead
of quit minibuffer. Answer q to literal confirmation."
  (interactive)
  (let ((xq (buffer-substring (point-min) (point-max))))
    (if (and (minibufferp) (string-match "y, n, !\\|yn!q" xq))
        (progn (keyamp-insert-init) (execute-kbd-macro (kbd "q")))
      (if (> (length (buffer-substring (minibuffer-prompt-end) (point))) 0)
          (keyamp-command) (abort-recursive-edit)))))

(defun keyamp-insert-n ()
  "Insert N literally."
  (interactive) (keyamp-insert-init) (execute-kbd-macro (kbd "n")))

(defun keyamp-insert-y ()
  "Insert Y literally. Only if asked."
  (interactive)
  (let ((xq (buffer-substring (point-min) (point-max))))
    (if (and (minibufferp) (string-match "y, n, !\\|yn!q" xq))
        (progn (keyamp-insert-init) (execute-kbd-macro (kbd "y")))
      (backward-kill-word 1))))

(defun keyamp-insert-! ()
  "Insert ! literally."
  (interactive) (keyamp-insert-init) (execute-kbd-macro (kbd "!")))

(defun keyamp-exit-minibuffer ()
  "Exit if file completion. It means use content of minibuffer as it is,
no select completion candidates. Else force complete and exit, that
is, select and use first completion candidate. In case file
completion, for most cases no need to complete, because there is NO
right candidate. Otherwise, in all cases one MUST select a candidate.
Simply hit TAB to minibuffer-complete file name if the name exists."
  (interactive)
  (if (eq (icomplete--category) 'file)
      (exit-minibuffer) (icomplete-force-complete-and-exit)))

(defvar keyamp-minibuffer-insert-commands nil
  "List of commands activating insert mode in minibuffer.")

(defun keyamp-minibuffer-setup-insert ()
  "Activate insert mode for list of commands after minibuffer setup.
Set up input timer to activate command mode."
  (when (or (eq (icomplete--category) 'file)
            (memq real-this-command keyamp-minibuffer-insert-commands))
    (keyamp-repeat-deactivate) (keyamp-command-execute 'keyamp-insert)
    (keyamp-input-timer-post-command)))

(setq-default cursor-in-non-selected-windows nil)

(defun keyamp-cancel-indicate-repeat-timer ()
  "Cancel indicate repeat timer."
  (when (timerp keyamp-indicate-repeat-timer)
    (cancel-timer keyamp-indicate-repeat-timer)
    (setq keyamp-indicate-repeat-timer nil)))

(defun keyamp-change-cursor-type (Cursor)
  "Set cursor type."
  (keyamp-cancel-indicate-repeat-timer)
  (modify-all-frames-parameters `((cursor-type . ,Cursor))))

(defun keyamp-mode-line-front-space (Indicator)
  "Set `mode-line-front-space' to INDICATOR."
  (setq mode-line-front-space Indicator)
  (unless (eq this-command last-command) (force-mode-line-update t)))

(defun keyamp-mode-line-front-space-color (Color)
  "Set `mode-line-front-space-face' COLOR."
  (set-face-attribute 'mode-line-front-space-face nil :foreground Color))

(defun keyamp-indicate (Indicator Cursor Color)
  "Indicate mode with INDICATOR, CURSOR and COLOR."
  (keyamp-mode-line-front-space Indicator) (keyamp-change-cursor-type Cursor)
  (keyamp-mode-line-front-space-color Color))

(defun keyamp-indicate-repeat ()
  "Indicate repeat view is active. Runs after first repeat command exactly
after a delay even if there more repeat commands follow."
  (unless (memq mode-line-front-space
                `(,keyamp-command-indicator ,keyamp-repeat-indicator))
    (keyamp-indicate-command))
  (unless (timerp keyamp-indicate-repeat-timer)
    (setq keyamp-indicate-repeat-timer
          (run-with-timer keyamp-indicate-repeat-delay nil
                          'keyamp-indicate keyamp-repeat-indicator
                          keyamp-repeat-cursor keyamp-repeat-color))))

(defvar keyamp-repeat-p nil "Non-nil means repeat is on.")
(defvar keyamp--deactivate-repeat-fun nil "Repeat mode deactivate function.")
(defvar keyamp--repeat-idle-timer nil "Repeat mode idle timer.")

(defun keyamp-set-deactivate-repeat-fun (Keymap)
  "Set `keyamp--deactivate-repeat-fun'."
  (setq keyamp--deactivate-repeat-fun (set-transient-map Keymap)))

(defun keyamp-cancel-repeat-idle-timer ()
  "Cancel `keyamp--repeat-idle-timer'."
  (if (timerp keyamp--repeat-idle-timer) (cancel-timer keyamp--repeat-idle-timer)))

(defun keyamp-repeat-deactivate ()
  "Deactivate repeat."
  (if keyamp-repeat-p (setq keyamp-repeat-p nil))
  (if keyamp--deactivate-repeat-fun (funcall keyamp--deactivate-repeat-fun)))

(defun keyamp-indicate-idle ()
  "Indicate idle."
  (keyamp-indicate keyamp-idle-indicator keyamp-command-cursor keyamp-idle-color))

(defun keyamp-indicate-screen ()
  "Indicate screen."
  (keyamp-indicate keyamp-screen-indicator keyamp-screen-cursor keyamp-screen-color))

(defun keyamp-indicate-edit ()
  "Indicate edit."
  (keyamp-indicate keyamp-edit-indicator keyamp-edit-cursor keyamp-edit-color))

(defun keyamp-indicate-insert ()
  "Indicate insert."
  (keyamp-indicate keyamp-insert-indicator keyamp-insert-cursor keyamp-insert-color))

(defun keyamp-indicate-command ()
  "Indicate command."
  (keyamp-indicate keyamp-command-indicator keyamp-command-cursor keyamp-command-color))

(defvar keyamp-user-error nil
  "True if previous command signaled `user-error'. See `command-error-function'.")

(defun keyamp-repeat ()
  "Set repeat mode. Run with `post-command-hook'."
  (if keyamp-user-error
      (progn (keyamp-command) (setq keyamp-user-error nil))
    (cond
     ((gethash this-command keyamp-screen-commands-hash)
      (keyamp-indicate-screen) (setq keyamp-repeat-p t))
     ((and (or (eq real-this-command 'repeat)
               (gethash this-command keyamp-repeat-commands-hash))
           (not keyamp-insert-p))
      (keyamp-indicate-repeat) (setq keyamp-repeat-p t))
     ((and (gethash this-command keyamp-edit-commands-hash) (not keyamp-insert-p))
      (keyamp-indicate-edit) (setq keyamp-repeat-p t))
     (keyamp-insert-p (keyamp-indicate-insert) (setq keyamp-repeat-p nil))
     (t (keyamp-indicate-command) (setq keyamp-repeat-p nil)))))

(defconst keyamp-indicate-io-duration (if (display-graphic-p) 0.3 0.6)
  "Indicate IO duration. Higher value for network access.")
(defvar keyamp-indicate-io-timer nil "Indicate IO timer.")

(defun keyamp-indicate-io-cancel (Indicator Cursor Color)
  "Cancel indicate IO."
  (if (eq keyamp-io-indicator mode-line-front-space)
      (keyamp-indicate Indicator Cursor Color)))

(defun keyamp-indicate-io (&rest r)
  "Indicate IO feedback from emacsclient evals or processes calls.
Cancel after `keyamp-indicate-io-duration'."
  (let* ((xInd mode-line-front-space) (xCur (frame-parameter nil 'cursor-type))
         (xCol (face-attribute 'mode-line-front-space-face :foreground)))
    (unless (eq xCol keyamp-io-color)
      (keyamp-indicate keyamp-io-indicator xCur keyamp-io-color)
      (if (timerp keyamp-indicate-io-timer) (cancel-timer keyamp-indicate-io-timer))
      (setq keyamp-indicate-io-timer
            (run-with-timer keyamp-indicate-io-duration nil
                            'keyamp-indicate-io-cancel xInd xCur xCol)))))

(defun keyamp-escape (&optional Idle)
  "Return to command mode, clear selection or quit minibuffer. Indicate idle."
  (interactive)
  (cond ((or keyamp-repeat-p keyamp-insert-p) (keyamp-command))
        ((region-active-p) (deactivate-mark)) ((minibufferp) (abort-recursive-edit)))
  (if Idle (keyamp-indicate-idle)))

;;;###autoload
(define-minor-mode keyamp
  "Keyboard Amplifier."
  :global t
  :keymap keyamp-map
  (when keyamp
    (keyamp-command)
    (add-hook 'minibuffer-exit-hook  'keyamp-command)
    (add-hook 'isearch-mode-end-hook 'keyamp-command)
    (add-hook 'debugger-mode-hook    'keyamp-command)
    (add-function :after after-focus-change-function #'keyamp-command-if-insert)
    (add-hook 'post-command-hook     'keyamp-repeat)
    (add-hook 'keyamp-insert-hook    'keyamp-cancel-repeat-idle-timer)
    (add-hook 'keyamp-command-hook   'keyamp-cancel-repeat-idle-timer)
    (add-hook 'isearch-mode-hook     'keyamp-cancel-repeat-idle-timer)
    (run-with-timer keyamp-defer-load nil 'keyamp-karabiner-init)
    (run-with-timer keyamp-defer-load nil 'keyamp-map-input-source 'russian-computer)
    (run-with-timer keyamp-defer-load nil 'keyamp-push-quail-keyboard-layout)
    (run-with-idle-timer keyamp-idle-timeout t 'keyamp-escape t)))

(provide 'keyamp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical unresolved)
;; End:
;;; keyamp.el ends here
