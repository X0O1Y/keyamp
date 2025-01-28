;;; keyamp.el --- Keyboard Amplifier -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Egor Maltsev <x0o1@ya.ru>
;; Version: 1.2 2025-01-07 Spectrum
;;          __   _____   __
;;         |__| |_____| |__|
;;
;; DEL SPC RET keys provide IDE workflow for ordinary on-screen keyboard.
;; This package is part of input model.

;;; Commentary:

;; Keyamp provides 3 modes: insert, command and repeat. Command mode
;; based on persistent transient keymap. Repeat mode adds transient
;; remaps on top of command mode for easy repeat of command chains
;; during screen positioning, cursor move and editing. Mode line front
;; space color indicates active transient keymap. Repeat mode turned
;; on/off automatically either by advice or with timer.

;; Try Keyboard Amplifier out with qwerty layout:
;; (setq keyamp-current-layout "qwerty")
;; (require 'keyamp)
;; (keyamp)

;;; Code:



(require 'keycom)


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

(defmacro keyamp--set (KeymapName CmdList &optional CommandMode InsertMode How TimeOut)
  "Map `set-transient-map' using `advice-add' over a list CMDLIST.
- Advice default HOW :after might be changed by specific HOW;
- Activate COMMANDMODE or INSERTMODE mode optionally;
- Deactivate repeat mode after idle for TIMEOUT seconds;
- Ignore advice if defining or executing kbd macro."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       ,@(mapcar
          (lambda (xcmd)
            `(advice-add ,(list 'quote xcmd) (if ,How ,How :after)
                         (lambda (&rest _) "auto repeat"
                           (when (and (not (or defining-kbd-macro executing-kbd-macro))
                                      (or (eq real-this-command 'repeat)
                                          (eq this-command 'kill-region) ; exception
                                          (eq this-command 'undo) ; exception
                                          (eq this-command ,(list 'quote xcmd))))
                             (if (and ,CommandMode keyamp-insert-p)
                                 (keyamp-command))
                             (keyamp-repeat-init ,xkeymapName)
                             (keyamp-cancel-repeat-idle-timer)
                             (if (and ,TimeOut
                                      (not keyamp-insert-p))
                                 (setq keyamp--repeat-idle-timer
                                       (run-with-idle-timer ,TimeOut nil 'keyamp-escape)))
                             (if ,InsertMode
                                 (keyamp-insert))))))
          (cadr CmdList)))))

(defmacro keyamp--hook (KeymapName HookList &optional CommandMode InsertMode RepeatMode)
  "Map `set-transient-map' using `add-hook' over a list HOOKLIST.
Activate command, insert or repeat mode optionally."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       ,@(mapcar
          (lambda (xhook)
            `(add-hook ,(list 'quote xhook)
                       (lambda () "auto repeat"
                         (if (and ,CommandMode keyamp-insert-p)
                             (keyamp-command))
                         (if (and ,InsertMode (not keyamp-insert-p))
                             (keyamp-insert))
                         (keyamp-repeat-init ,xkeymapName)
                         (if ,RepeatMode
                             (keyamp-command-execute 'keyamp--read-dummy)))))
          (cadr HookList)))))

(defun keyamp--read-dummy () "Dummy for indication." (interactive))

(defun keyamp-command-execute (Command)
  "Change this command to COMMAND and execute it. Indicate when not idle."
  (setq this-command Command)
  (command-execute Command)
  (if (or (null (current-idle-time))
          (< (time-convert (current-idle-time) 'integer) keyamp-idle-timeout))
      (keyamp-transient)))

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
  "Map S-<tab> and <backtab> keys to CMD using `keyamp--map'."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (if (display-graphic-p)
           (keyamp--map ,xkeymapName '(("S-<tab>" . ,Cmd))))
       (keyamp--map ,xkeymapName '(("<backtab>" . ,Cmd))))))

(defmacro keyamp--map-return (KeymapName Cmd)
  "Map RET or <return> keys to CMD using `keyamp--map'."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (if (display-graphic-p)
           (keyamp--map ,xkeymapName '(("<return>" . ,Cmd)))
         (keyamp--map ,xkeymapName '(("RET" . ,Cmd)))))))

(defmacro keyamp--map-escape (KeymapName Cmd)
  "Map <escape> key to CMD using `keyamp--map'."
  (declare (indent defun))
  (let ((xkeymapName (make-symbol "keymap-name")))
    `(let ((,xkeymapName ,KeymapName))
       (keyamp--map ,xkeymapName '(("<escape>" . ,Cmd))))))

(defmacro with-sparse-keymap (&rest body)
  "Make sparse keymap for next use in BODY."
  (declare (indent defun))
  `(let ((keymap (make-sparse-keymap)))
     ,@body))

(defmacro advice-add-macro (SymList How Fun)
  "Map `advice-add' HOW over a list SYMLIST to FUN."
  `(progn
     ,@(mapcar (lambda (xcmd) `(advice-add ,(list 'quote xcmd) ,How ,Fun))
               (cadr SymList))))


;; Double remap

(defun keyamp-exe-remap (Key From To)
  "Remap key from FROM command to TO command by default, but if remapped
already, then use existing remap instead. Execute resulting command."
  (let ((x (keymap-lookup overriding-local-map (keyamp--convert-kbd-str Key))))
    (keyamp-command-execute (cond ((or (equal x From) (not x)) To) (x x)))))


;; Double press

(defconst keyamp-double-press-timeout (/ 300 1000.0) "Double key press timeout.")
(defvar keyamp-double-press-timer nil "Double key press timer.")

(defun keyamp-double-press (Cmd)
  "Execute COMMAND after second command call during `keyamp-double-press-timeout'."
  (if (and (timerp keyamp-double-press-timer)
           (eq this-command last-command)
           (not (or defining-kbd-macro executing-kbd-macro)))
      (keyamp-command-execute Cmd))
  (setq keyamp-double-press-timer
        (run-with-timer keyamp-double-press-timeout nil
                        (lambda () (setq keyamp-double-press-timer nil)))))

(defmacro keyamp--map-double (CmdCmdAlist)
  "Map over alist CMDCMDALIST double press of CAR CMDCONS to CDR CMDCONS."
  (declare (indent defun))
  `(progn ,@(mapcar
             (lambda (xpair)
               `(advice-add ,(list 'quote (car xpair)) :after
                            (lambda (&rest _) "double press"
                              (keyamp-double-press ,(list 'quote (cdr  xpair))))))
             (cadr CmdCmdAlist))))


;; Triple press (hold down)

(defvar keyamp-defer-command-timer nil "Defer command timer.")
(defconst keyamp-key-repeat-delay (/ (if (display-graphic-p) 30 90) 1000.0)
  "Key repeat delay. Higher value for network access.")

(defun keyamp-defer-command (Defer Command)
  "Defer execution of COMMAND for DEFER seconds."
  (setq keyamp-defer-command-timer
        (run-with-timer Defer nil 'keyamp-command-execute Command)))

(defun keyamp-cancel-defer-command-timer ()
  "Cancel `keyamp-defer-command-timer'."
  (when (timerp keyamp-defer-command-timer)
    (cancel-timer keyamp-defer-command-timer)
    (setq keyamp-defer-command-timer nil)))

(defun keyamp-defer-command-around (fun &rest _)
  "Run `keyamp-defer-command' as around advice."
  (if (memq last-command triple-press-direction-commands-list)
      (before-last-command))
  (keyamp-defer-command keyamp-key-repeat-delay fun))


;; Terminal ESC to <escape>

(defconst keyamp-tty-seq-timeout (/ 30 1000.0)
  "Timeout to wait key sequence after ESC sent in tty.")

(defun keyamp-tty-ESC-filter (map)
  "Map last ESC key from this single command keys to <escape>."
  (if (and (let ((tty-seq (this-single-command-keys)))
             (= ?\e (aref tty-seq (1- (length tty-seq)))))
           (sit-for keyamp-tty-seq-timeout))
      [escape] map))

(defun keyamp-lookup-key (map key)
  (catch 'found
    (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

(defun keyamp-catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into <escape>."
  (when (memq (terminal-live-p (frame-terminal)) '(t pc))
    (let ((esc-binding (keyamp-lookup-key input-decode-map ?\e)))
      (keymap-set input-decode-map
                  "ESC" `(menu-item "" ,esc-binding :filter keyamp-tty-ESC-filter)))
    (keymap-set key-translation-map "ESC" "<escape>")))


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
             (if (eq 'keyamp-qwerty-to-current-layout this-command)
                 (message "Deactivated QWERTY keyboard to %s" keyamp-current-layout)))
    (put 'keyamp-qwerty-to-current-layout 'state t)
    (quail-set-keyboard-layout keyamp-current-layout)
    (if (eq 'keyamp-qwerty-to-current-layout this-command)
        (message "Activated QWERTY keyboard to %s" keyamp-current-layout)))
  (let ((xl (alist-get keyamp-current-layout keyamp-layouts nil nil 'string-equal)))
    (mapc (lambda (x)
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
    ("C-^" . keyamp-lleader-map)           ("C-+" . keyamp-lleader-map)
    ("C-_" . keyamp-rleader-map)           ("C-И" . keyamp-rleader-map)))

(keyamp--map-leader keyamp-command-map '(keyamp-lleader-map . keyamp-rleader-map))
(keyamp--map-return keyamp-command-map keyamp-insert)
;; Single keys mapping must double in Russian here. All prefix sequences mapped
;; automatically using `keyamp-map-input-source'. If missing then same.
(keyamp--map keyamp-command-map
  '(;; left half
    ("`" . make-frame-command)             ("ё" . make-frame-command)               ("~" . keyamp-qwerty-to-current-layout) ("Ë" . keyamp-qwerty-to-current-layout)
    ("1" . kmacro-record)                                                           ("!" . self-insert-command)
    ("2" . kmacro-play)                                                             ("@" . self-insert-command)
    ("3" . kmacro-helper)                                                           ("#" . self-insert-command)             ("№" . self-insert-command)
    ("4" . append-to-r1)                                                            ("$" . self-insert-command)
    ("5" . terminal-split)                                                          ("%" . self-insert-command)

    ("q" . insert-space-before)            ("й" . insert-space-before)              ("Q" . keyamp-self-insert-and-insert)   ("Й" . keyamp-self-insert-and-insert)
    ("w" . backward-del-word)              ("ц" . backward-del-word)                ("W" . keyamp-self-insert-and-insert)   ("Ц" . keyamp-self-insert-and-insert)
    ("e" . undo)                           ("у" . undo)                             ("E" . keyamp-self-insert-and-insert)   ("У" . keyamp-self-insert-and-insert)
    ("r" . del-word)                       ("к" . del-word)                         ("R" . keyamp-self-insert-and-insert)   ("К" . keyamp-self-insert-and-insert)
    ("t" . cut-text-block)                 ("е" . cut-text-block)                   ("T" . self-insert-command)             ("Е" . keyamp-self-insert-and-insert)

    ("a" . shrink-whitespaces)             ("ф" . shrink-whitespaces)               ("A" . keyamp-self-insert-and-insert)   ("Ф" . keyamp-self-insert-and-insert)
    ("s" . open-line)                      ("ы" . open-line)                        ("S" . keyamp-self-insert-and-insert)   ("Ы" . keyamp-self-insert-and-insert)
    ("d" . del-back)                       ("в" . del-back)                         ("D" . keyamp-self-insert-and-insert)   ("В" . keyamp-self-insert-and-insert)
    ("f" . newline)                        ("а" . newline)                          ("F" . keyamp-self-insert-and-insert)   ("А" . keyamp-self-insert-and-insert)
    ("g" . activate-region)                ("п" . activate-region)                  ("G" . self-insert-command)             ("П" . keyamp-self-insert-and-insert)

    ("z" . toggle-comment)                 ("я" . toggle-comment)                   ("Z" . keyamp-self-insert-and-insert)   ("Я" . keyamp-self-insert-and-insert)
    ("x" . cut-line)                       ("ч" . cut-line)                         ("X" . keyamp-self-insert-and-insert)   ("Ч" . keyamp-self-insert-and-insert)
    ("c" . copy-line)                      ("с" . copy-line)                        ("C" . keyamp-self-insert-and-insert)   ("С" . keyamp-self-insert-and-insert)
    ("v" . paste-or-prev)                  ("м" . paste-or-prev)                    ("V" . keyamp-self-insert-and-insert)   ("М" . keyamp-self-insert-and-insert)
    ("b" . toggle-case)                    ("и" . toggle-case)                      ("B" . self-insert-command)             ("И" . keyamp-self-insert-and-insert)

    ;; right half
    ("6" . pass)                                                                    ("^" . self-insert-command)
    ("7" . jump-to-register)                                                        ("&" . keyamp-self-insert-and-insert)
    ("8" . point-to-register)                                                       ("*" . goto-match-br) ; QWERTY * → = Engineer Engram, QWERTY / → = RU PC Karabiner
    ("9" . proced-defer)                                                            ("(" . self-insert-command)
    ("0" . eshell-split)                                                            (")" . self-insert-command)
    ("-" . enlarge-window)                                                          ("_" . self-insert-command)
    ("=" . goto-match-br)                                                           ("+" . self-insert-command)

    ("y"  . occur-cur-word)                ("н" . occur-cur-word)                   ("Y" . self-insert-command)             ("Н" . keyamp-self-insert-and-insert)
    ("u"  . back-word)                     ("г" . back-word)                        ("U" . keyamp-self-insert-and-insert)   ("Г" . keyamp-self-insert-and-insert)
    ("i"  . previous-line)                 ("ш" . previous-line)                    ("I" . keyamp-self-insert-and-insert)   ("Ш" . keyamp-self-insert-and-insert)
    ("o"  . forw-word)                     ("щ" . forw-word)                        ("O" . keyamp-self-insert-and-insert)   ("Щ" . keyamp-self-insert-and-insert)
    ("p"  . jump-mark)                     ("з" . jump-mark)                        ("P" . keyamp-self-insert-and-insert)   ("З" . keyamp-self-insert-and-insert)
    ("["  . alt-buf)                       ("х" . alt-buf)                          ("{" . keyamp-self-insert-and-insert)   ("Х" . keyamp-self-insert-and-insert)
    ("]"  . bookmark-bmenu-list)           ("ъ" . bookmark-bmenu-list)              ("}" . keyamp-self-insert-and-insert)   ("Ъ" . keyamp-self-insert-and-insert)
    ("\\" . bookmark-set)                                                           ("|" . keyamp-self-insert-and-insert)

    ("h" . beg-of-line)                    ("р" . beg-of-line)                      ("H"  . self-insert-command)            ("Р" . keyamp-self-insert-and-insert)
    ("j" . backward-char)                  ("о" . backward-char)                    ("J"  . keyamp-self-insert-and-insert)  ("О" . keyamp-self-insert-and-insert)
    ("k" . next-line)                      ("л" . next-line)                        ("K"  . keyamp-self-insert-and-insert)  ("Л" . keyamp-self-insert-and-insert)
    ("l" . forward-char)                   ("д" . forward-char)                     ("L"  . keyamp-self-insert-and-insert)  ("Д" . keyamp-self-insert-and-insert)
    (";" . end-of-lyne)                    ("ж" . end-of-lyne)                      (":"  . keyamp-self-insert-and-insert)  ("Ж" . keyamp-self-insert-and-insert)
    ("'" . alternate-frame)                ("э" . alternate-frame)                  ("\"" . keyamp-self-insert-and-insert)  ("Э" . keyamp-self-insert-and-insert)

    ("n" . isearch-forward)                ("т" . isearch-forward)                  ("N" . keyamp-insert-N)                 ("Т" . keyamp-self-insert-and-insert)
    ("m" . backward-bracket)               ("ь" . backward-bracket)                 ("M" . keyamp-self-insert-and-insert)   ("Ь" . keyamp-self-insert-and-insert)
    ("," . other-win)                      ("б" . other-win)                        ("<" . keyamp-self-insert-and-insert)   ("Б" . keyamp-self-insert-and-insert)
    ("." . forward-bracket)                ("ю" . forward-bracket)                  (">" . keyamp-self-insert-and-insert)   ("Ю" . keyamp-self-insert-and-insert)
    ("/" . goto-match-br)                                                           ("?" . keyamp-self-insert-and-insert)

    ("<left>" . back-char)                 ("<right>" . forw-char)
    ("<up>"   . up-line)                   ("<down>"  . down-line)))

(keyamp--map-leader keyamp-lleader-map '(select-block . select-quote))
(keyamp--map-return keyamp-lleader-map execute-extended-command)
(keyamp--map keyamp-lleader-map '(("C-t" . display-line-numbers-mode)))
(keyamp--map-escape keyamp-lleader-map ignore)
(keyamp--map-backtab keyamp-lleader-map prev-proj-buf)
(keyamp--map-tab keyamp-lleader-map read-only-mode)
(keyamp--map keyamp-lleader-map
  '(;; left leader left half
    ("`" . revert-buffer)
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
    ("g" . new-empty-buffer)

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
    ("-" . snake)
    ("=" . tetris)

    ("y" . find-name-dired)
    ("u" . flymake-goto-prev-error)

    ("i i"   . copy-file-path)
    ("i DEL" . count-words)                ("i SPC" . count-matches)
    ("i <escape>" . ignore)                ("i RET" . show-in-desktop)

    ("o"  . flymake-goto-next-error)
    ("p"  . show-kill-ring)
    ("["  . toggle-frame-maximized)
    ("]"  . ignore)
    ("\\" . ignore)

    ("h"  . go-new)

                                           ("j l"   . narrow-to-region-or-block)
                                           ("j k"   . narrow-to-defun)
                                           ("j j"   . widen)
    ("j DEL" . hl-line-mode)               ("j SPC" . whitespace-mode)
    ("j <escape>" . ignore)                ("j RET" . toggle-word-wrap)

    ("k s"   . space-to-newline)
    ("k d"   . delete-matching-lines)      ("k k"   . find-file)
    ("k f"   . delete-non-matching-lines)
    ("k r"   . quote-lines)                ("k u"   . escape-quotes)
    ("k t"   . delete-duplicate-lines)     ("k y"   . slash-to-double-backslash)
    ("k v"   . reformat-to-sentence-lines) ("k n"   . double-backslash-to-slash)
    ("k w"   . sort-lines-key-value)       ("k o"   . slash-to-backslash)
    ("k x"   . insert-column-a-z)          ("k ."   . sort-lines-block-or-region)
    ("k c"   . cycle-hyphen-lowline-space) ("k ,"   . sort-numeric-fields)
    ("k DEL" . ispell-word)                ("k SPC" . flyspell-buffer)
    ("k <escape>" . ignore)                ("k RET" . list-matching-lines)

    ("l" . screen-idle)
    (";" . recentf-open-files)
    ("'" . sync)
    ("n" . list-timers)
    ("m" . yt-dlp)
    ("," . list-processes)
    ("." . open-last-closed)
    ("/" . goto-line)))

(if (display-graphic-p)
    (keyamp--map keyamp-lleader-map
      '(("i DEL" . nil)        ("i <backspace>" . count-words)
        ("i RET" . nil)        ("i <return>"    . show-in-desktop)
        ("j DEL" . nil)        ("j <backspace>" . hl-line-mode)
        ("j RET" . nil)        ("j <return>"    . toggle-truncate-lines)
        ("k DEL" . nil)        ("k <backspace>" . ispell-word)
        ("k RET" . nil)        ("k <return>"    . find-file)
        ("<mouse-1>" . ignore)
        ("<mouse-2>" . ignore)
        ("<mouse-3>" . ignore)
        ("<down-mouse-1>" . mouse-drag-region-rectangle))))

(keyamp--map-leader keyamp-rleader-map '(select-line . select-word))
(keyamp--map-return keyamp-rleader-map open-file)
(keyamp--map keyamp-rleader-map '(("C-t" . toggle-truncate-lines)))
(keyamp--map-escape keyamp-rleader-map ignore)
(keyamp--map-backtab keyamp-rleader-map speedbar)
(keyamp--map-tab keyamp-rleader-map next-proj-buf)
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
    ("e <escape>" . ignore)                ("e RET" . insert-date)

    ("r" . query-replace-regexp)
    ("t" . calc)
    ("a" . mark-whole-buffer)
    ("s" . clean-whitespace)

    ("D"     . repeat)                     ("В"     . repeat)
    ("d e"   . org-shiftup)                ("d i"   . async-shell-command)
    ("d d"   . eval-region-or-sexp)        ("d k"   . run-current-file)
    ("d DEL" . stow)                       ("d SPC" . eval-defun)
    ("d <escape>" . ignore)                ("d RET" . shell-command)

    ("f e"   . insert-emacs-quote)         ("f i"   . insert-ascii-single-quote)
    ("f f"   . insert-char)                ("f j"   . insert-brace)
    ("f k"   . insert-paren)
    ("f s"   . insert-formfeed)            ("f l"   . insert-square-bracket)
    ("f g"   . insert-double-angle-quote)  ("f h"   . insert-double-curly-quote)
    ("f DEL" . insert-backtick-quote)      ("f SPC" . insert-ascii-double-quote)
    ("f <escape>" . ignore)                ("f RET" . emoji-insert)

    ("g" . python-new)
    ("z" . goto-char)
    ("x" . next-eww-buffer)
    ("c" . copy-all)
    ("v" . tasks)
    ("b" . title-case-region-or-line)

    ;; right leader right half
    ("6" . pass-generate)
    ("7" . copy-to-register)
    ("8" . insert-register)
    ("9" . org-insert-source-code)
    ("0" . toggle-theme)
    ("-" . enlarge-window-horizontally)
    ("=" . toggle-input-source)

    ("y"  . search-string)
    ("u"  . backward-punct)
    ("i"  . beg-of-block)
    ("o"  . forward-punct)
    ("p"  . mark-defun)
    ("["  . ignore)
    ("]"  . ignore)
    ("\\" . ignore)

    ("h" . page-up-half)
    ("j" . isearch-wback)
    ("k" . end-of-block)
    ("l" . isearch-wforw)
    (";" . page-dn-half)
    ("'" . toggle-frame-maximized)

    ("n" . toggle-case-fold-search)
    ("m" . dired-jump)
    ("," . delete-window)
    ("." . save-close-buf)
    ("/" . view-messages)
    ("*" . view-messages)))

(if (display-graphic-p)
    (keyamp--map keyamp-rleader-map
      '(("e DEL" . nil)        ("e <backspace>" . clock)
        ("e RET" . nil)        ("e <return>"    . insert-date)
        ("d DEL" . nil)        ("d <backspace>" . stow)
        ("d RET" . nil)        ("d <return>"    . shell-command)
        ("f DEL" . nil)        ("f <backspace>" . insert-backtick-quote)
        ("f RET" . nil)        ("f <return>"    . emoji-insert)
        ("<mouse-1>" . ignore)
        ("<mouse-2>" . ignore)
        ("<mouse-3>" . ignore))))

(keyamp--map-double
  '((keyamp-escape  . alternate-frame) (activate-region . deactivate-region)
    (beg-of-line    . bookmark-jump)   (end-of-lyne     . switch-to-buffer)
    (kmacro-record  . keyamp-delete)   (other-win       . delete-other-windows)
    (proced-defer   . save-close-buf)  (append-to-r1    . clear-r1)
    (occur-cur-word . search-string)))


;; Remaps

(when (display-graphic-p)
  (keymap-set global-map          "<tab>" 'indent-for-tab-command) ; /lisp/indent.el.gz:816
  (keymap-set key-translation-map "S-SPC"         "<tab>")
  (keymap-set key-translation-map "S-<backspace>" "<backtab>")
  (keymap-set key-translation-map "S-<escape>"    "C-h")
  (keymap-set key-translation-map "S-<return>"    "C-t"))

(keymap-set global-map "<home>" 'beg-of-buf)
(keymap-set global-map "<end>" 'end-of-buf)

(setq help-map (make-sparse-keymap))
(fset 'help-command help-map)

(keyamp--map-leader help-map '(lookup-word-definition . translate))
(keyamp--map-escape help-map ignore)
(keyamp--map-return help-map lookup-web)
(keyamp--map-backtab help-map lookup-etymology)
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
    ("z" . apropos-variable)   ("x" . apropos-value)))

(keyamp--map global-map '(("C-r" . delete-other-windows) ("C-t" . hippie-expand)))

(when (display-graphic-p)
  (keyamp--map global-map
    '(("<prior>"          . page-up-half) ("<next>"    . page-dn-half)
      ("<double-mouse-1>" . select-word)  ("<mouse-3>" . mouse-3)))
  (advice-add 'mouse-set-point   :around 'lookup-around)
  (advice-add 'mouse-set-point   :before 'scroll-one-pixel)
  (advice-add 'mouse-set-point   :after  'keyamp-command-if-insert)
  (advice-add 'mouse-drag-region :before 'copy-selection))

(advice-add 'keyamp-insert :before 'delete-before)
(advice-add 'keyamp-insert :around 'lookup-around)
(advice-add 'keyamp-insert :around 'translate-around)

(with-sparse-keymap
  ;; Repeat using DEL/SPC or D. The concept widely used to form Repeat mode.
  (keyamp--map-leader keymap '(del-back . del-back))
  (keyamp--remap keymap '((del-back . repeat)))
  (keyamp--set keymap '(repeat)))

(with-sparse-keymap
  ;; S-RET to call `hippie-expand'. Press RET to insert a possible expansion.
  (keyamp--map-leader keymap '(hippie-expand-undo . self-insert-command))
  (keyamp--map-return keymap hippie-expand)
  (keyamp--set keymap '(hippie-expand)))


;; I-search

(with-sparse-keymap
  ;; After starting up an isearch press DEL to retreat to the previous
  ;; search string. Press SPC to pull string from kill ring into search string.
  (keyamp--map-leader keymap '(isearch-ring-retreat . isearch-yank-kill))
  ;; Double press N to save modified file.
  (keyamp--map keymap '(("n" . save-buffer-isearch-cancel) ("т" . save-buffer-isearch-cancel)))
  (keyamp--hook keymap '(isearch-mode-hook) nil nil :repeat))

;; Hit TAB to repeat after typing in search string and set following transient
;; map. Backtab to repeat backward. S-DEL/S-SPC for Backtab/TAB.
(keyamp--map-leader isearch-mode-map '(isearch-del-char . isearch-printing-char))
(keyamp--map-escape isearch-mode-map isearch-cancel)
(keyamp--map-backtab isearch-mode-map isearch-back)
(keyamp--map-tab isearch-mode-map isearch-forw)
(keyamp--map isearch-mode-map '(("C-^" . keyamp-lleader-map)))
(keyamp--remap isearch-mode-map '((paste-from-r1 . isearch-yank-r1)))

(with-sparse-keymap
  ;; Find the occurrence of the current search string with J/L or DEL/SPC.
  ;; Press I/K or DEL/SPC to get search strings from the ring.
  ;; S-SPC to find the occurrence of the last search string.
  (keyamp--map-leader keymap '(isearch-back . isearch-forw))
  (keyamp--map keymap
    '(("i" . isearch-ring-retreat) ("ш" . isearch-ring-retreat)
      ("j" . isearch-back)         ("о" . isearch-back)
      ("k" . isearch-ring-advance) ("л" . isearch-ring-advance)
      ("l" . isearch-forw)         ("д" . isearch-forw)))
  (keyamp--set keymap
    '(isearch-ring-retreat     isearch-ring-advance
      isearch-back             isearch-forw
      isearch-wforw            isearch-wback
      isearch-yank-kill))

 (defun isearch-mode-exit-minibuffer ()
   "Setup isearch transient after choice from the ring and exit minibuffer."
   (when (eq real-this-command 'exit-minibuffer)
     (keyamp-repeat-deactivate-init keymap)
     (setq this-command 'isearch-forw)))

 (add-hook 'isearch-mode-hook 'isearch-mode-exit-minibuffer 96))

(with-sparse-keymap
  ;; Press I/K or DEL/SPC to get search strings from the ring
  ;; then S-DEL/S-SPC to find the occurrence of the search string.
  (keyamp--map-leader keymap '(hist-back . hist-forw))
  (keyamp--map-escape keymap isearch-cancel-clean-are)
  (keyamp--map-backtab keymap exit-minibuffer)
  (keyamp--map-tab keymap exit-minibuffer)

 (defun isearch-mode-setup-minibuffer ()
   "Setup isearch transient in minibuffer before choice from the ring."
   (if (isearch-minibuffer-prompt)
       (keyamp-repeat-deactivate-init keymap)))

 (add-hook 'minibuffer-setup-hook 'isearch-mode-setup-minibuffer 96)
 (advice-add-macro '(hist-back hist-forw) :after 'isearch-mode-setup-minibuffer))



(defun keyamp-TAB ()
  "Tab key command for transient use."
  (interactive)
  (cond ((eq major-mode 'org-agenda-mode) (keyamp-command-execute 'todo))
        ((eq major-mode 'ibuffer-mode)    (keyamp-command-execute 'ibuffer-select-group))
        ((eq major-mode 'gnus-group-mode) (keyamp-command-execute 'gnus-topic-select-group))
        ((eq last-command 'up-line)       (keyamp-command-execute 'del-win)) ; see below default for down-line
        (t (keyamp-command-execute 'page-dn-half))))

(defun keyamp-RET ()
  "Return key command for transient use."
  (interactive)
  (if (eq major-mode 'eww-mode)
      (keyamp-command-execute 'keyamp-insert) ; do translate
    (if (display-graphic-p)
        (keyamp-exe-remap "<return>" 'keyamp-insert 'keyamp-escape)
      (keyamp-exe-remap "RET" 'keyamp-insert 'keyamp-escape))))

(defun keyamp-delete ()
  "Keyamp do delete."
  (interactive)
  (cond ((eq major-mode 'dired-mode)   (keyamp-command-execute 'dired-do-delete))
        ((eq major-mode 'ibuffer-mode) (keyamp-command-execute 'ibuffer-do-delete))
        ((eq major-mode 'eshell-mode)  (keyamp-command-execute 'eshell-interrupt-process))
        ((eq major-mode 'vterm-mode)   (keyamp-command-execute 'term-interrupt-subjob))
        (t (keyamp-command-execute 'ignore))))


;; Repeat mode - screen commands

(defvar keyamp-delay-1 1 "Delay 1 second.")
(defvar keyamp-delay-2 2 "Delay 2 seconds.")
(defvar keyamp-delay-3 3 "Delay 3 seconds.")

(with-sparse-keymap
  ;; Leader layer to become transient main. Base map for next leaders adjustment
  ;; by transient maps which might be set by following target commands subsets.
  (keyamp--map-leader keymap '(open-line . newline))
  (keyamp--map-return keymap keyamp-escape)
  (keyamp--map-backtab keymap page-up-half)
  (keyamp--map-tab keymap keyamp-TAB)
  (keyamp--remap keymap
    '((make-frame-command  . delete-frame)
      (insert-space-before . clock)
      (backward-del-word   . sun-moon)
      (undo                . del-win)
      (del-word            . toggle-gnus)
      (cut-text-block      . calc)
      (goto-match-br       . view-messages)
      (shrink-whitespaces  . calendar-split)
      (del-back            . alt-buf)
      (toggle-comment      . view-messages)
      (cut-line            . prev-eww-buffer)
      (copy-line           . screen-idle)
      (paste-or-prev       . tasks)
      (toggle-case         . downloads)
      (backward-bracket    . dired-jump)
      (forward-bracket     . save-close-buf)
      (kmacro-helper       . config)
      (point-to-register   . sql)
      (up-line             . view-messages)
      (down-line           . screen-idle)
      (back-char           . next-buf)
      (forw-char           . prev-buf)
      (proced-defer        . save-close-buf)
      (kmacro-play         . save-close-buf)
      (append-to-r1        . recentf-open-files)))

  (keyamp--set keymap
    '(prev-buf                   next-buf
      delete-other-windows       split-window-below
      delete-window              save-close-buf
      prev-proj-buf              next-proj-buf
      prev-eww-buffer            next-eww-buffer
      prev-eshell-buffer         next-eshell-buffer
      prev-vterm-buffer          next-vterm-buffer
      tasks                      config
      previous-buffer            next-buffer
      find-prev-dir-file         find-next-dir-file
      shrink-window              enlarge-window
      shrink-window-horizontally enlarge-window-horizontally
      org-agenda-tasks           split-window-horizontally)))

(with-sparse-keymap
  ;; DEL/SPC to switch other window after split as a result of the commands.
  (keyamp--map-leader keymap '(other-window . other-window))
  (keyamp--map-return keymap delete-other-windows)
  (keyamp--remap keymap '((down-line . delete-other-windows)))
  (keyamp--set keymap
    '(describe-foo-at-point   describe-variable
      describe-function       describe-key
      describe-mode           describe-char
      describe-face           list-matching-lines
      player                  occur-cur-word
      run-current-file        exec-query
      view-messages           sun-moon
      clock                   async-shell-command
      sync                    calendar-split)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . prev-buf) (newline . next-buf)))
  (keyamp--set keymap
    '(prev-buf next-buf delete-other-windows delete-window split-window-below
      split-window-horizontally)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . prev-proj-buf) (newline . next-proj-buf)))
  (keyamp--set keymap '(prev-proj-buf next-proj-buf)))

(with-sparse-keymap
  (keyamp--map-backtab keymap page-up-half)
  (keyamp--map-tab keymap keyamp-TAB)
  (keyamp--remap keymap
    '((open-line . prev-eww-buffer) (newline . next-eww-buffer)
      (del-back  . eww-reload)      (undo    . justify-buffer)))
  (keyamp--set keymap '(prev-eww-buffer next-eww-buffer)))

(with-sparse-keymap
  (keyamp--map-backtab keymap page-up-half)
  (keyamp--map-tab keymap keyamp-TAB)
  (keyamp--remap keymap '((open-line . prev-eshell-buffer) (newline . next-eshell-buffer)))
  (keyamp--set keymap '(prev-eshell-buffer next-eshell-buffer)))

(with-sparse-keymap
  (keyamp--map-backtab keymap page-up-half)
  (keyamp--map-tab keymap keyamp-TAB)
  (keyamp--remap keymap '((open-line . prev-vterm-buffer) (newline . next-vterm-buffer)))
  (keyamp--set keymap '(prev-vterm-buffer next-vterm-buffer)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . prev-buf) (newline . tasks)))
  (keyamp--set keymap '(tasks org-agenda-tasks)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . prev-buf) (newline . config)))
  (keyamp--set keymap '(config)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . previous-buffer) (newline . next-buffer)))
  (keyamp--set keymap '(previous-buffer next-buffer)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . find-prev-dir-file) (newline . find-next-dir-file)))
  (keyamp--set keymap '(find-prev-dir-file find-next-dir-file)))

(with-sparse-keymap
  (keyamp--remap keymap
    '((backward-bracket . dired-jump) (forward-bracket . save-close-buf)))
  (keyamp--set keymap
    '(dired-jump downloads dired-find-file ibuffer-visit-buffer open-last-closed
      bookmark-jump widget-button-press alt-buf)))

(with-sparse-keymap
  (keyamp--remap keymap
    '((open-line       . prev-buf)       (newline       . next-buf)
      (forward-bracket . save-close-buf) (keyamp-insert . keyamp-escape)
      (proced-defer    . save-close-buf)))
  (keyamp--set keymap '(save-close-buf)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . shrink-window) (newline . enlarge-window)))
  (keyamp--set keymap '(shrink-window enlarge-window) nil nil nil keyamp-delay-2))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . shrink-window-horizontally) (newline . enlarge-window-horizontally)))
  (keyamp--set keymap '(enlarge-window-horizontally shrink-window-horizontally)
    nil nil nil keyamp-delay-2))

(with-sparse-keymap
  (keyamp--remap keymap '((make-frame-command . delete-frame)))
  (keyamp--set keymap '(alternate-frame)))


;; Repeat mode - read commands

(with-sparse-keymap
  ;; Initiate by triple DEL/SPC (hold down).
  ;; I/K or DEL/SPC to move by lines. See `return-before'.
  (keyamp--map-leader keymap '(up-line . down-line))
  (keyamp--map-return keymap keyamp-RET)
  (keyamp--map-backtab keymap page-up-half)
  (keyamp--map-tab keymap keyamp-TAB)
  (keyamp--map keymap '(("<up>" . up-line-rev)))
  (keyamp--remap keymap '((previous-line . up-line-rev) (next-line . down-line)))
  (keyamp--set keymap '(up-line down-line))
  (keyamp--hook keymap '(ibuffer-hook gnus-group-mode-hook) nil nil :repeat)

  (defvar keyamp-lines-move-modes
    '(occur-mode         gnus-group-mode  emms-playlist-mode
      ibuffer-mode       eww-mode         messages-buffer-mode )
    "List of modes using lines move.")

  (defun keyamp-lines-move (&rest _)
    "Repeat move by lines."
    (when (and this-command
               (memq major-mode keyamp-lines-move-modes))
      (keyamp-repeat-deactivate-init keymap)
      (run-at-time nil nil 'keyamp-indicate-read)))

  (advice-add-macro '(other-window translate dired-find-file dired-jump)
   :after 'keyamp-lines-move))

(with-sparse-keymap ; swap leaders up/down
  (keyamp--map-leader keymap '(down-line-rev . up-line-rev))
  (keyamp--map-return keymap keyamp-RET)
  (keyamp--remap keymap '((previous-line . up-line) (next-line . down-line)))
  (keyamp--set keymap '(up-line-rev down-line-rev)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(up-line . down-line))
  (keyamp--map-return keymap keyamp-RET)
  (keyamp--remap keymap '((previous-line . beg-of-block) (next-line . end-of-block)))
  (keyamp--set keymap '(beg-of-buf end-of-buf)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(previous-line . next-line))
  (keyamp--map-return keymap keyamp-RET)
  (keyamp--remap keymap '((previous-line . beg-of-block) (next-line . end-of-block)))
  (keyamp--set keymap '(beg-of-block end-of-block)))

(with-sparse-keymap
  (keyamp--remap keymap '((beg-of-line . beg-of-buf) (end-of-lyne . end-of-buf)))
  (keyamp--set keymap '(beg-of-buf end-of-buf)))

(defvar keyamp-beg-of-buf-timer nil "Beg of buf timer.")
(defvar keyamp-end-of-buf-timer nil "End of buf timer.")

(advice-add 'end-of-lyne :after
            (lambda () "keyamp-beg-of-buf-timer"
              (setq keyamp-beg-of-buf-timer
                    (run-with-timer keyamp-double-press-timeout nil
                                    (lambda () (setq keyamp-beg-of-buf-timer nil))))))

(advice-add 'beg-of-line :after
            (lambda () "keyamp-end-of-buf-timer"
              (setq keyamp-end-of-buf-timer
                    (run-with-timer keyamp-double-press-timeout nil
                                    (lambda () (setq keyamp-end-of-buf-timer nil))))))

(advice-add 'beg-of-line :around
           (lambda (fun &rest r) "keyamp-beg-of-buf-timer"
             (if (timerp keyamp-beg-of-buf-timer)
                 (keyamp-command-execute 'beg-of-buf)
               (apply fun r))))

(advice-add 'end-of-lyne :around
           (lambda (fun &rest r) "keyamp-end-of-buf-timer"
             (if (timerp keyamp-end-of-buf-timer)
                 (keyamp-command-execute 'end-of-buf)
               (apply fun r))))

;; In case triple DEL received during `keyamp-key-repeat-delay',
;; `select-block' would be ignored. Must call before following transient maps.
;; Same for triple SPC and `select-word'.
(advice-add-macro '(select-block select-word) :around 'keyamp-defer-command-around)
(advice-add-macro '(up-line down-line) :before 'keyamp-cancel-defer-command-timer)

(with-sparse-keymap
  (keyamp--map-leader keymap '(up-line . end-of-block))
  (keyamp--remap keymap
    '((previous-line . beg-of-block)  (next-line     . select-block)
      (keyamp-escape . return-before) (hippie-expand . exec-query)))
  (keyamp--set keymap '(select-block)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(beg-of-block . down-line))
  (keyamp--remap keymap '((keyamp-escape . return-before)))
  (keyamp--set keymap '(select-word)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(back-word . forw-char))
  (keyamp--map-escape keymap return-before)
  (keyamp--set keymap '(select-quote)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(back-char . forw-word))
  (keyamp--map-escape keymap return-before)
  (keyamp--map-return keymap deactivate-region)
  (keyamp--set keymap '(select-line)))

(advice-add-macro
 ;; If region active deactivate mark and return to the point before selection.
 '(up-line                       down-line
   beg-of-block                  end-of-block
   ibuffer-backward-filter-group ibuffer-forward-filter-group
   gnus-topic-prev               gnus-topic-next
   page-up-half                  page-dn-half
   prev-buf                      next-buf
   back-char                     forw-char
   dired-do-delete               deactivate-region)
:before 'return-before)

(with-sparse-keymap ; Left/right arrows repeat by DEL/SPC.
  (keyamp--map-leader keymap '(back-char . forw-char))
  (keyamp--set keymap '(back-char forw-char)))

;; touch screen
(with-sparse-keymap
  (keyamp--map-tab keymap next-buf)
  (keyamp--set keymap '(forw-char)))

(with-sparse-keymap
  (keyamp--map-tab keymap other-win)
  (keyamp--set keymap '(back-char)))

(with-sparse-keymap
  (keyamp--remap keymap '((backward-char . back-word) (forward-char . forw-word)))
  (keyamp--set keymap '(back-word forw-word)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(backward-char . forward-char))
  (keyamp--remap keymap '((backward-char . back-word-repeat) (forward-char . forw-word-repeat)))
  (keyamp--set keymap '(back-word-repeat forw-word-repeat)))

(with-sparse-keymap
  (keyamp--remap keymap '((back-word . backward-punct) (forw-word . forward-punct)))
  (keyamp--set keymap '(backward-punct forward-punct)))

;; U and O act as leader keys.
(defvar keyamp--deactivate-leader-fun nil "Virtual leader deactivate function.")
(defvar keyamp-leader-timer nil "Virtual leader deactivate timer.")

(defun keyamp--leader-deactivate ()
  "Deactivate virtual leader."
  (if keyamp--deactivate-leader-fun
      (funcall keyamp--deactivate-leader-fun))
  (setq keyamp-leader-timer nil))

(defun keyamp-leader-init (Keymap)
  "Set virtual leader transient KEYMAP."
  (when (member (this-command-keys)
                (list (keyamp--convert-kbd-str "u") (keyamp--convert-kbd-str "o")
                      [1075] [1097])) ; г 1075 щ 1097
    (setq keyamp--deactivate-leader-fun (set-transient-map Keymap))
    (if (timerp keyamp-leader-timer)
        (cancel-timer keyamp-leader-timer))
    (setq keyamp-leader-timer
          (run-with-timer keyamp-double-press-timeout nil 'keyamp--leader-deactivate))))

(define-prefix-command 'keyamp-lleader-i-map)
(define-prefix-command 'keyamp-lleader-k-map)

(with-sparse-keymap
  (keyamp--map keymap '(("i" . keyamp-lleader-i-map) ("k" . keyamp-lleader-k-map)))
  (keyamp--map keymap '(("ш" . keyamp-lleader-i-map) ("л" . keyamp-lleader-k-map)))
  (keyamp--map keyamp-lleader-i-map '(("i" . make-backup-and-copy)))
  (keyamp--map keyamp-lleader-k-map '(("k" . make-backup-and-save)))
  (keyamp--remap keymap '((back-word . select-block) (forw-word . select-quote)))
  (advice-add 'back-word :after
              (lambda () "virtual left leader transient"
                (keyamp-leader-init keymap))))

(with-sparse-keymap
  (keyamp--remap keymap
    '((back-word     . select-line)   (forw-word        . select-word)
      (previous-line . end-of-buf)    (next-line        . end-of-block)
      (backward-char . isearch-wforw) (backward-bracket . dired-jump)))
  (advice-add 'forw-word :after
              (lambda () "virtual right leader transient"
                (keyamp-leader-init keymap))))

(defun keyamp-leader-return-before (&rest _)
  "Return before, that is, compensate word move."
  (if (timerp keyamp-leader-timer)
      (set-mark-command t)))

(advice-add-macro '(select-block               select-quote
                    make-backup-and-copy       make-backup-and-save
                    select-line                select-word
                    isearch-wforw)
                  :before 'keyamp-leader-return-before)

;; G acts as leader key.
(with-sparse-keymap
  (keyamp--map-leader keymap '(open-line . newline))
  (keyamp--map-escape keymap deactivate-region)
  (keyamp--map-return keymap toggle-ibuffer)
  (keyamp--map-backtab keymap describe-variable)
  (keyamp--map-tab keymap ignore)
  (keyamp--remap keymap
    '((undo               . todo)
      (shrink-whitespaces . screen-idle)
      (open-line          . prev-buf)
      (del-back           . del-forw)
      (newline            . tasks)
      (activate-region    . rectangle)
      (toggle-case        . downloads)
      (other-win          . jump-7)
      (isearch-forward    . jump-8)
      (alternate-frame    . player)
      (kmacro-helper      . toggle-pin-window)))

  (advice-add 'activate-region :after
              (lambda () "virtual leader G transient"
                (if (eq (mark) (point))
                    (set-transient-map keymap)))))

(defun keyamp-deactivate-region (&rest _)
  "Deactivate region if mark equal point."
  (if (eq (mark) (point))
      (deactivate-region)))

(advice-add-macro
 '(tasks                 open-in-external-app
   delete-window         screen-idle
   del-forw              next-proj-buf
   jump-7                jump-8
   screen-idle           downloads
   toggle-pin-window     toggle-ibuffer
   describe-variable     player)
 :before 'keyamp-deactivate-region)

(with-sparse-keymap
  ;; Repeat half page up/down with I/K or DEL/SPC.
  (keyamp--map-leader keymap '(previous-line . next-line))
  (keyamp--map-return keymap keyamp-RET)
  (keyamp--map-backtab keymap scroll-down-command)
  (keyamp--map-tab keymap scroll-up-command)
  (keyamp--remap keymap
    '((previous-line . page-up-half) (next-line . page-dn-half)
      (down-line     . page-dn-half) (up-line   . page-up-half)))
  (unless (display-graphic-p) ; touch reader
    (keyamp--remap keymap '((down-line . page-up-half) (up-line . page-dn-half))))
  (keyamp--set keymap '(page-up-half page-dn-half)))

(with-sparse-keymap
  ;; Initially TAB makes half page forward, following presses do full page.
  ;; Arrows always do half page and keep TAB transient, see previous keymap.
  (keyamp--map-leader keymap '(previous-line . next-line))
  (keyamp--map-return keymap keyamp-RET)
  (keyamp--map-backtab keymap previous-line)
  (keyamp--map-tab keymap next-line)
  (keyamp--remap keymap
    '((previous-line . scroll-down-command) (next-line . scroll-up-command)
      (down-line     . page-dn-half) (up-line   . page-up-half)))
  (unless (display-graphic-p) ; touch reader
    (keyamp--remap keymap '((down-line . page-up-half) (up-line . page-dn-half))))
  (keyamp--set keymap '(scroll-down-command scroll-up-command)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(next-line . next-line))
  (keyamp--remap keymap '((next-line . jump-mark)))
  (keyamp--set keymap '(jump-mark)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(text-scale-decrease . text-scale-increase))
  (keyamp--map-return keymap keyamp-escape)
  (keyamp--map-tab keymap text-scale-reset)
  (keyamp--remap keymap '((goto-match-br . text-scale-reset)))
  (keyamp--set keymap '(text-scale-decrease text-scale-increase text-scale-reset)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(up-line . down-line))
  (keyamp--remap keymap '((undo . button-back) (del-back . button-forw)))
  (keyamp--set keymap '(button-back button-forw)))

(defvar keyamp-button-move-modes nil "List of modes using button move.")

(defun keyamp-button-move (&rest _)
  "Continue move by buttons."
  (if (and (memq major-mode keyamp-button-move-modes)
           (= (point) (point-min)))
      (keyamp-command-execute 'button-forw)))

(advice-add 'other-window :after 'keyamp-button-move)

(with-sparse-keymap
  (keyamp--map-leader keymap '(next-line . next-line))
  (keyamp--remap keymap '((next-line . recenter-top-bottom)))
  (keyamp--set keymap '(recenter-top-bottom)) nil nil nil keyamp-delay-2)

(with-sparse-keymap
  (keyamp--map-return keymap toggle-truncate-lines)
  (keyamp--map keymap '(("C-t" . toggle-truncate-lines)))
  (keyamp--set keymap '(toggle-truncate-lines)))


;; Repeat mode - modify commands

(with-sparse-keymap
  ;; After hit delete backward/forward char, shrink whitespaces or insert
  ;; space before while in command mode, DEL/SPC start to do delete/space.
  (keyamp--map-leader keymap '(delete-forward-char . insert-space-before))
  (keyamp--set keymap '(delete-forward-char) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--map-leader keymap '(del-back . insert-space-before))
  (keyamp--set keymap '(del-back insert-space-before) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--map-leader keymap '(undo . del-forw))
  (keyamp--remap keymap '((del-back . del-forw)))
  (keyamp--set keymap '(del-forw) nil nil nil keyamp-delay-2))

(with-sparse-keymap
  (keyamp--map-leader keymap '(open-line . newline))
  (keyamp--remap keymap '((open-line . backward-del-word) (newline . del-word)))
  (keyamp--set keymap '(backward-del-word del-word) nil nil nil keyamp-delay-2))

(with-sparse-keymap
  (keyamp--map-leader keymap '(undo-only . undo-redo))
  (keyamp--set keymap '(undo undo-redo)))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . cut-text-block)))
  (keyamp--set keymap '(cut-text-block)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(del-back . insert-space-before))
  (keyamp--remap keymap '((del-back . shrink-whitespaces)))
  (keyamp--set keymap '(shrink-whitespaces) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--map-leader keymap '(del-back . del-back))
  (keyamp--remap keymap '((del-back . toggle-comment)))
  (keyamp--set keymap '(toggle-comment) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--map-leader keymap '(del-back . del-back))
  (keyamp--remap keymap '((del-back . cut-line)))
  (keyamp--set keymap '(cut-line) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--map-leader keymap '(del-back . del-back))
  (keyamp--remap keymap '((del-back . copy-line)))
  (keyamp--set keymap '(copy-line)))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . toggle-case)))
  (keyamp--set keymap '(toggle-case) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--map-leader keymap '(del-back . undo))
  (keyamp--remap keymap '((undo . org-shiftup) (del-back . org-shiftdown)))
  (keyamp--set keymap '(org-shiftup org-shiftdown) nil nil nil keyamp-delay-3))

(with-sparse-keymap
  (keyamp--map-leader keymap '(screen-idle . todo))
  (keyamp--remap keymap '((undo . todo) (copy-line . screen-idle)))
  (keyamp--set keymap '(todo insert-date) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . cycle-hyphen-lowline-space)))
  (keyamp--set keymap '(cycle-hyphen-lowline-space) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  ;; SPC S then SPC to clean whitespaces and save the buffer or DEL to close.
  (keyamp--map-leader keymap '(save-close-buf . save-buffer))
  (keyamp--remap keymap '((backward-bracket . dired-jump)))
  (keyamp--set keymap '(clean-whitespace) nil nil nil keyamp-delay-1))

(with-sparse-keymap ; dummy indication modify
  (keyamp--set keymap
    '(apply-macro-to-region-lines cycle-hyphen-lowline-space
      delete-duplicate-lines      delete-matching-lines
      delete-non-matching-lines   emoji-insert
      eval-defun                  eval-region-or-sexp
      eshell-clear-input          fill-or-unfill
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
      save-buffer                 space-to-newline
      title-case-region-or-line   toggle-prev-letter-case)
    nil nil nil keyamp-blink-blink))

(with-sparse-keymap ; dummy indication idle
  (keyamp--set keymap '(ignore dummy monitor) nil nil nil keyamp-blink-blink))

(with-sparse-keymap ; easy undo with DEL
  (keyamp--map-leader keymap '(undo . nil))
  (keyamp--set keymap '(newline python-return-and-indent open-line)))


;; Modes Remaps

(with-eval-after-load 'minibuffer
  (with-sparse-keymap
    ;; On minibuffer startup press DEL to list history backwards or
    ;; SPC to list completion candidates forward or paste. After that
    ;; I/K or DEL/SPC to list either history or completion candidates
    ;; accordingly choice made. RET to confirm and exit, ESC to quit.
    ;; To switch from history to candidates listing press ESC then double
    ;; SPC `select-word' and DEL/SPC or I/K again to continue move
    ;; backward/forward. Similarly double DEL to activate history move.
    ;; Fast history or completion candidates direction switch to quit.
    (keyamp--map-leader keymap '(select-block . select-word))
    (keyamp--map-escape keymap keyamp-minibuffer-escape)
    (keyamp--map-return keymap keyamp-minibuffer-return)
    (keyamp--map-backtab keymap keyamp-minibuffer-backtab)
    (keyamp--map-tab keymap keyamp-minibuffer-tab)
    (keyamp--remap keymap
      '((kmacro-record       . keyamp-insert-1)
        (kmacro-play         . keyamp-insert-2)
        (kmacro-helper       . keyamp-insert-3)
        (append-to-r1        . keyamp-insert-4)
        (terminal-split      . keyamp-insert-5)
        (pass                . keyamp-insert-6)
        (jump-to-register    . keyamp-insert-7)
        (point-to-register   . keyamp-insert-8)
        (proced-defer        . keyamp-insert-9)
        (eshell-split        . keyamp-insert-0)

        (insert-space-before . keyamp-insert-q)
        (backward-del-word   . keyamp-insert-w)
        (undo                . keyamp-insert-e)
        (del-word            . keyamp-insert-r)
        (cut-text-block      . keyamp-insert-t)
        (occur-cur-word      . keyamp-insert-y)
        (back-word           . keyamp-insert-u)
        (previous-line       . keyamp-insert-i)
        (forw-word           . keyamp-insert-o)
        (jump-mark           . keyamp-insert-p)
        (alt-buf             . keyamp-insert-bracket)

        (shrink-whitespaces  . keyamp-insert-a)
        (open-line           . keyamp-insert-s)
        (del-back            . keyamp-insert-d)
        (newline             . keyamp-insert-f)
        (activate-region     . keyamp-insert-g)
        (beg-of-line         . keyamp-insert-h)
        (backward-char       . keyamp-insert-j)
        (next-line           . keyamp-insert-k)
        (forward-char        . keyamp-insert-l)
        (end-of-lyne         . keyamp-insert-semicolon)
        (alternate-frame     . keyamp-insert-quote)

        (toggle-comment      . keyamp-insert-z)
        (cut-line            . keyamp-insert-x)
        (copy-line           . keyamp-insert-c)
        (paste-or-prev       . keyamp-insert-v)
        (toggle-case         . keyamp-insert-b)
        (isearch-forward     . keyamp-insert-n)
        (backward-bracket    . keyamp-insert-m)
        (other-win           . keyamp-insert-comma)
        (forward-bracket     . keyamp-insert-dot)
        (goto-match-br       . keyamp-insert-slash)))

    ;; The hook is last one run during minibuffer setup and set the keymap.
    (keyamp--hook keymap '(minibuffer-setup-hook) :command nil :repeat))

  (defun keyamp-select-word (fun &rest r)
    "Execute `paste-or-prev' in empty minibuffer."
    (if (keyamp-minibuffer-empty)
        (keyamp-command-execute 'paste-or-prev)
      (apply fun r)))

  (advice-add 'select-word :around 'keyamp-select-word)

  ;; Hit D/DEL for No, K/SPC for Yes to answer non-literal Y or N.
  (keyamp--remap y-or-n-p-map
    '((select-block . y-or-n-p-insert-n) (del-back  . y-or-n-p-insert-n)
      (select-word  . y-or-n-p-insert-y) (next-line . y-or-n-p-insert-y)))

  (keyamp--remap minibuffer-local-map
    '((previous-line . hist-back) (next-line . hist-forw)
      (select-block  . hist-back)))

  (keyamp--map-backtab minibuffer-local-completion-map keyamp-minibuffer-backtab)
  (keyamp--map-tab minibuffer-local-completion-map keyamp-minibuffer-tab)
  (keyamp--map minibuffer-local-completion-map '(("C-t" . minibuffer-complete)))
  (keyamp--remap minibuffer-mode-map
    '((previous-line . hist-back) (next-line . hist-forw)
      (select-block  . hist-back)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(minibuffer-previous-completion . minibuffer-next-completion))
    (keyamp--map-escape keymap delete-completion-win)
    (keyamp--map-return keymap minibuffer-choose-completion)
    (keyamp--map-backtab keymap minibuffer-previous-completion)
    (keyamp--map-tab keymap minibuffer-next-completion)
    (keyamp--set keymap
      '(completion-at-point minibuffer-previous-completion minibuffer-next-completion)))

  (advice-add 'completion-at-point :after
              (lambda (&rest _) "select candidate" (minibuffer-next-completion)))
  (advice-add-macro '(completion-at-point minibuffer-choose-completion delete-completion-win)
                    :after 'keyamp-insert-init)

  (keyamp--map minibuffer-inactive-mode-map
    '(("<mouse-1>" . radio) ("<double-mouse-1>" . ignore)))
  (keyamp--remap minibuffer-inactive-mode-map '((mouse-3 . radio-next))))

(with-eval-after-load 'icomplete
  (keyamp--map-return icomplete-minibuffer-map keyamp-minibuffer-return)
  (keyamp--remap icomplete-minibuffer-map
    '((previous-line . comp-back) (next-line . comp-forw) (select-word . comp-forw)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--map-escape keymap abort-recursive-edit)
    (keyamp--map-return keymap keyamp-minibuffer-return)
    (keyamp--remap keymap '((previous-line . comp-back) (next-line . comp-forw)))
    (keyamp--set keymap '(comp-back comp-forw)))

  (with-sparse-keymap
    (keyamp--remap keymap '((previous-line . hist-back) (next-line . comp-forw)))
    (keyamp--hook keymap '(icomplete-minibuffer-setup-hook) nil nil :repeat))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--map-escape keymap abort-recursive-edit)
    (keyamp--map-return keymap exit-minibuffer)
    (keyamp--remap keymap '((previous-line . hist-back) (next-line . hist-forw)))
    (keyamp--set keymap '(hist-back hist-forw))))

(add-hook 'ido-setup-hook
  (lambda () "ido-completion-map created after ido setup only"
    (keyamp--remap ido-completion-map
      '((keyamp-insert . ido-exit-minibuffer)
        (previous-line . hist-back)      (select-block . hist-back)
        (next-line     . ido-next-match) (select-word  . ido-next-match)))))

(with-sparse-keymap
  (keyamp--map-leader keymap '(previous-line . next-line))
  (keyamp--remap keymap '((previous-line . ido-prev-match) (next-line . ido-next-match)))
  (keyamp--set keymap '(ido-prev-match ido-next-match)))

(with-eval-after-load 'dired
  (keyamp--map dired-mode-map
    '(("<double-mouse-1>" . dired-find-file)
      ("<mouse-1>" . mouse-set-point) ("<mouse-2>" . mouse-set-point)))
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
      (toggle-case         . dired-sort)
      (copy-to-r1          . dired-do-copy)
      (paste-from-r1       . dired-do-rename)
      (mark-whole-buffer   . dired-toggle-marks)
      (kmacro-helper       . config)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(dired-toggle-mark . dired-toggle-mark))
    (keyamp--set keymap '(dired-toggle-mark) nil nil nil keyamp-delay-1))

  (advice-add 'dired-toggle-marks :before 'dired-unmark-all-marks))

(with-eval-after-load 'wdired
  (keyamp--map wdired-mode-map '(("C-h" . wdired-abort-changes) ("C-t" . wdired-finish-edit)))
  (advice-add-macro '(wdired-abort-changes wdired-finish-edit) :after 'keyamp-command))

(with-eval-after-load 'dired-utils
  (keyamp--map-backtab dired-mode-map dired-lleader-map)
  (keyamp--map-backtab (define-prefix-command 'dired-lleader-map) dired-omit-mode)
  (keyamp--map-leader dired-lleader-map '(dired-size . nil))

  (keyamp--map-tab dired-mode-map dired-rleader-map)
  (keyamp--map-tab (define-prefix-command 'dired-rleader-map) dired-omit-mode)
  (keyamp--map-leader dired-rleader-map '(nil . dired-size))
  (keyamp--map dired-rleader-map
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
  (keyamp--map ibuffer-mode-map '(("<double-mouse-1>" . ibuffer-visit-buffer)))
  (keyamp--map-backtab ibuffer-mode-map ibuffer-backward-filter-group)
  (keyamp--map-tab ibuffer-mode-map ibuffer-visit-buffer)
  (keyamp--remap ibuffer-mode-map
    '((previous-line       . up-line)
      (next-line           . down-line)
      (keyamp-insert       . ibuffer-visit-buffer)
      (end-of-block        . ibuffer-forward-filter-group)
      (beg-of-block        . ibuffer-backward-filter-group)
      (insert-space-before . clock)
      (backward-del-word   . sun-moon)
      (undo                . del-win)
      (cut-text-block      . calc)
      (goto-match-br       . view-messages)
      (shrink-whitespaces  . calendar-split)
      (open-line           . prev-buf)
      (del-back            . alt-buf)
      (newline             . next-buf)
      (toggle-comment      . view-messages)
      (cut-line            . prev-eww-buffer)
      (copy-line           . screen-idle)
      (paste-or-prev       . tasks)
      (toggle-case         . downloads)
      (forward-bracket     . save-close-buf)
      (kmacro-helper       . config)
      (point-to-register   . sql)
      (del-word            . toggle-gnus)
      (forw-char           . screen-idle-return)
      (back-char           . screen-idle)
      (append-to-r1        . recentf-open-files)
      (kmacro-play         . save-close-buf)))

  (keyamp--map ibuffer-mode-filter-group-map '(("<mouse-1>" . ibuffer-toggle-filter-group)))
  (keyamp--map-tab ibuffer-mode-filter-group-map ibuffer-toggle-filter-group)
  (keyamp--remap ibuffer-mode-filter-group-map
    '((keyamp-insert . ibuffer-toggle-filter-group)
      (forw-char     . screen-idle-return)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--remap keymap '((previous-line . up-line-rev) (next-line . down-line)))
    (keyamp--set keymap '(ibuffer-toggle-filter-group)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(ibuffer-backward-filter-group . ibuffer-forward-filter-group))
    (keyamp--set keymap '(ibuffer-backward-filter-group ibuffer-forward-filter-group))))

(with-eval-after-load 'ibuffer
  (keyamp--map ibuffer-name-map '(("<mouse-1>" . mouse-set-point))))

(with-sparse-keymap
  (keyamp--map-leader keymap '(del-back . del-back))
  (keyamp--remap keymap '((del-back . ibuffer-do-delete) (kmacro-record . ibuffer-do-delete)))
  (keyamp--set keymap '(ibuffer-do-delete) nil nil nil keyamp-delay-3))

(with-eval-after-load 'company
  (keyamp--map-tab company-active-map company-complete-common)

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--map-escape keymap company-abort)
    (keyamp--map-return keymap company-complete-selection)
    (keyamp--remap keymap
      '((isearch-forward . company-search-candidates)
        (previous-line   . company-select-back)
        (next-line       . company-select-forw)
        (beg-of-line     . company-previous-page)
        (end-of-lyne     . company-next-page)))

    (keyamp--set keymap
      '(company-select-back company-select-forw company-previous-page
                            company-next-page company-show-doc-buffer company-search-abort
                            company-manual-begin))

    (advice-add 'company-manual-begin :before 'keyamp-command)

    (defun keyamp-command-company ()
      "Set transient keymap if company candidates."
      (if company-candidates
          (keyamp-repeat-deactivate-init keymap)))

    (add-hook 'keyamp-command-hook 'keyamp-command-company))

  (with-sparse-keymap
    ;; Activate command mode after complete selection, but if next hit is SPC
    ;; then activate insert mode and insert SPC. DEL to undo the completion.
    (advice-add-macro '(company-search-abort company-complete-selection)
                      :after 'keyamp-command-if-insert)
    (keyamp--map-leader keymap '(undo . keyamp-insert-and-SPC))
    (keyamp--set keymap '(company-search-abort company-complete-selection)))

  (advice-add 'company-search-candidates :after 'keyamp-insert-init)
  (keyamp--map-escape company-search-map company-search-abort)
  (keyamp--map-backtab company-search-map company-search-repeat-backward)
  (keyamp--map-tab company-search-map company-search-repeat-forward)

  (with-sparse-keymap
    (keyamp--map-leader keymap '(company-search-repeat-backward . company-search-repeat-forward))
    (keyamp--set keymap '(company-search-repeat-backward company-search-repeat-forward))))

(with-eval-after-load 'transient
  (keyamp--map-escape transient-base-map transient-quit-one))

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
  (keyamp--remap occur-mode-map
    '((keyamp-insert . occur-mode-goto-occurrence)
      (del-back      . next-match)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(nil . next-match))
    (keyamp--set keymap '(next-match)))

  (keyamp--map-return query-replace-map edit-replacement)
  (keyamp--map-escape query-replace-map exit)
  (keyamp--map query-replace-map '(("d" . skip) ("в" . skip) ("k" . act) ("л" . act))))

(with-eval-after-load 'shr
  (keyamp--remap shr-map '((keyamp-insert . shr-browse-url))))

(with-eval-after-load 'simple
  (keyamp--remap completion-list-mode-map '((keyamp-insert . choose-completion))))

(with-eval-after-load 'wid-edit
  (keyamp--remap widget-link-keymap '((keyamp-insert . widget-button-press)))
  (with-sparse-keymap
    (keyamp--map-leader keymap '(widget-backward . widget-forward))
    (keyamp--set keymap '(widget-backward widget-forward))))

(with-eval-after-load 'org
  (keyamp--map-backtab org-mode-map undo)
  (keyamp--map-tab org-mode-map org-cycle)
  (keyamp--remap org-mode-map
    '((eval-region-or-sexp . insert-date) (insert-date . org-time-stamp))))

(with-eval-after-load 'org-agenda
  (keyamp--map-backtab org-agenda-mode-map ignore)
  (keyamp--map-tab org-agenda-mode-map todo)
  (keyamp--map org-agenda-mode-map
    '(("<double-mouse-1>" . org-agenda-tasks) ("<mouse-3>" . mouse-3)))
  (keyamp--remap org-agenda-mode-map
    '((keyamp-insert       . org-agenda-tasks)
      (make-frame-command  . delete-frame)
      (insert-space-before . clock)
      (backward-del-word   . sun-moon)
      (undo                . del-win)
      (del-word            . toggle-gnus)
      (cut-text-block      . calc)
      (goto-match-br       . view-messages)
      (shrink-whitespaces  . calendar-split)
      (open-line           . prev-buf)
      (del-back            . alt-buf)
      (newline             . next-buf)
      (previous-line       . up-line-rev)
      (next-line           . down-line)
      (toggle-comment      . org-agenda-redo)
      (cut-line            . prev-eww-buffer)
      (paste-or-prev       . tasks)
      (toggle-case         . downloads)
      (backward-bracket    . downloads)
      (forward-bracket     . save-close-buf)
      (kmacro-helper       . config)
      (forw-char           . screen-idle-escape)
      (back-char           . screen-idle-return)
      (append-to-r1        . recentf-open-files)
      (kmacro-play         . save-close-buf)
      (kmacro-record       . alarm)
      (pass                . stopwatch-lap)
      (jump-to-register    . stopwatch)
      (point-to-register   . timer)
      (insert-register     . timer-stop)
      (proced-defer        . timer-display))))

(defvar screen-idle-keymap (make-sparse-keymap))
(keyamp--map-tab screen-idle-keymap novel)
(keyamp--map screen-idle-keymap
  '(("<right>" . screen-idle-escape) ("<left>" . screen-idle-return)
    ("<up>"    . view-messages)      ("<down>" . down-line)))

(advice-add 'delete-other-windows :after
            (lambda (&rest _) "screen-idle-keymap"
              (if (eq major-mode 'org-agenda-mode)
                  (set-transient-map screen-idle-keymap))))

(defvar screen-idle-escape-keymap (make-sparse-keymap))
(keyamp--map-tab screen-idle-escape-keymap keyamp-TAB)
(keyamp--map screen-idle-escape-keymap
  '(("<left>" . screen-idle)    ("<right>" . screen-idle-return)
    ("<up>"   . toggle-ibuffer)))
(keyamp--set screen-idle-escape-keymap '(screen-idle-escape novel))

(defvar tmux-clock-keymap (make-sparse-keymap) "Tmux clock transient keymap.")
(defvar tmux-clock-keymap-deactivate-fun nil
  "Tmux clock transient deactivate fun.")

(keyamp--map-leader tmux-clock-keymap '(alt-buf . tasks))
(keyamp--map-escape tmux-clock-keymap alt-buf)
(keyamp--map-return tmux-clock-keymap alt-buf)
(keyamp--remap tmux-clock-keymap
  '((del-back      . alt-buf) (mouse-3      . alt-buf)
    (previous-line . alt-buf) (next-line    . alt-buf)
    (backward-char . alt-buf) (forward-char . alt-buf)))

(with-eval-after-load 'org-keys
  (keyamp--remap org-mouse-map '((org-open-at-mouse . mouse-set-point))))

(with-eval-after-load 'eww
  (keyamp--map-tab eww-mode-map page-dn-half)
  (keyamp--map eww-mode-map '(("<left-fringe> <mouse-1>" . page-dn-half)))
  (keyamp--remap eww-mode-map
    '((open-line          . eww-back-url)
      (newline            . eww-next-url)
      (del-back           . eww-reload)
      (del-word           . eww-reload-all)
      (undo               . justify-buffer)
      (cut-text-block     . eww-copy-page-url)
      (shrink-whitespaces . eww-browse-with-external-browser)
      (backward-bracket   . downloads)
      (forward-bracket    . recenter-top-bottom)
      (isearch-wforw      . what-cursor-position)
      (backward-char      . back-word)
      (forward-char       . forw-word)
      (previous-line      . up-line-rev)
      (next-line          . down-line)))
  (keyamp--remap eww-link-keymap '((keyamp-insert . eww-follow-link))))

(with-eval-after-load 'emms
  (with-sparse-keymap
    (keyamp--map-leader keymap '(open-line . newline))
    (keyamp--remap keymap
      '((open-line         . emms-seek-backward-or-previous)
        (del-back          . emms-pause)
        (cut-line          . emms-random)
        (newline           . emms-seek-forward-or-next)
        (backward-del-word . emms-seek-backward)
        (del-word          . emms-seek-forward)
        (forward-bracket   . ignore)))
    (keyamp--set keymap
      '(emms-seek-backward-or-previous emms-seek-forward-or-next
        emms-playlist-mode-play-smart  emms-pause emms-random)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(open-line . newline))
    (keyamp--remap keymap '((open-line . emms-seek-backward) (newline . emms-seek-forward)))
    (keyamp--set keymap '(emms-seek-backward emms-seek-forward))))

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
  (with-sparse-keymap
    (keyamp--map-leader keymap '(open-line . newline))
    (keyamp--remap keymap
      '((del-back  . ispell-word)
        (open-line . flyspell-goto-prev-error)
        (newline   . flyspell-goto-next-error)))
    (keyamp--set keymap
      '(flyspell-buffer          ispell-word
        flyspell-goto-prev-error flyspell-goto-next-error))))

(with-eval-after-load 'doc-view
  (keyamp--map doc-view-mode-map '(("C-r" . delete-other-windows)))
  (keyamp--remap doc-view-mode-map
    '((keyamp-insert  . keyamp-escape)
      (select-block   . doc-view-scroll-down-or-previous-page)
      (select-word    . doc-view-scroll-up-or-next-page)
      (previous-line  . doc-view-scroll-down-or-previous-page)
      (next-line      . doc-view-scroll-up-or-next-page)
      (up-line        . doc-view-scroll-down-or-previous-page)
      (down-line      . doc-view-scroll-up-or-next-page)
      (backward-char  . doc-view-previous-page)
      (forward-char   . doc-view-next-page)
      (enlarge-window . doc-view-enlarge)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(doc-view-shrink . doc-view-enlarge))
    (keyamp--set keymap '(doc-view-shrink doc-view-enlarge) nil nil nil keyamp-delay-2))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--remap keymap
      '((previous-line . doc-view-scroll-down-or-previous-page)
        (next-line     . doc-view-scroll-up-or-next-page)
        (up-line       . doc-view-scroll-down-or-previous-page)
        (down-line     . doc-view-scroll-up-or-next-page)))
    (keyamp--set keymap
      '(doc-view-scroll-down-or-previous-page doc-view-scroll-up-or-next-page))))

(with-eval-after-load 'image-mode
  (keyamp--map image-mode-map '(("C-r" . delete-other-windows)))
  (keyamp--remap image-mode-map
    '((keyamp-insert    . keyamp-escape)
      (backward-char    . image-previous-file) (forward-char . image-next-file)
      (back-char        . image-previous-file) (forw-char    . image-next-file)
      (previous-line    . image-decrease-size) (next-line    . image-increase-size)
      (open-line        . image-previous-file) (newline      . image-next-file)
      (undo             . image-dired)         (del-back     . image-rotate)
      (select-word      . image-next-file)     (select-block . image-previous-file)
      (backward-bracket . dired-jump)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(backward-char . forward-char))
    (keyamp--set keymap '(image-previous-file image-next-file))
    (keyamp--hook keymap '(image-mode-hook) nil nil :repeat))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--set keymap '(image-decrease-size image-increase-size))))

(with-eval-after-load 'image-dired
  (keyamp--remap image-dired-thumbnail-mode-map
    '((keyamp-insert . image-dired-display-this))))

(with-eval-after-load 'profiler
  (keyamp--remap profiler-report-mode-map
    '((keyamp-insert . profiler-report-toggle-entry))))

(with-eval-after-load 'proced
  (keyamp--remap proced-mode-map
    '((keyamp-insert . proced-refine))))

(with-eval-after-load 'esh-mode
  (keyamp--map-backtab eshell-mode-map undo)
  (keyamp--map-tab eshell-mode-map completion-at-point)
  (keyamp--remap eshell-mode-map
    '((cut-line        . eshell-clear-input)
      (next-eww-buffer . eshell-clear)
      (select-block    . eshell-previous-input)
      (open-line       . prev-eshell-buffer)
      (newline         . next-eshell-buffer)
      (toggle-comment  . ignore)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--remap keymap
      '((previous-line . eshell-previous-input) (next-line . eshell-next-input)
        (undo          . delete-window)))
    (keyamp--set keymap
      '(eshell-previous-input eshell-next-input eshell-search-input) :command))

  (with-sparse-keymap
    ;; Insert mode primary for eshell. The keymap ready after eshell start,
    ;; command submit or cancel. DEL to list history, SPC to paste.
    (keyamp--map-leader keymap '(eshell-previous-input . paste-or-prev))
    (keyamp--map-backtab keymap eshell-search-input)
    (keyamp--map-tab keymap change-wd)
    (keyamp--set keymap '(eshell-send-input eshell-interrupt-process) nil :insert)

    (defun keyamp-input-timer-payload-eshell ()
      "Set transient keymap for eshell after input timer timeout."
      (if (eq major-mode 'eshell-mode)
          (keyamp-repeat-deactivate-init keymap)))

    (advice-add 'keyamp-input-timer-payload :after 'keyamp-input-timer-payload-eshell)
    (keyamp--hook keymap '(eshell-mode-hook) nil nil :repeat))

  (add-hook 'eshell-mode-hook 'keyamp-input-timer)
  (advice-add-macro '(eshell-send-input eshell-interrupt-process)
                    :after 'keyamp-input-timer))

(with-eval-after-load 'em-cmpl ; <backtab> conflict w/ default
  (keyamp--map-backtab eshell-cmpl-mode-map eshell-search-input))

(advice-add-macro
 ;; Activate command mode after jump from insert.
 '(alt-buf  delete-other-windows delete-window split-window-below
   prev-buf next-buf             save-close-buf dired-jump)
 :after 'keyamp-command-if-insert)

;;; vterm
(with-eval-after-load 'vterm
  (keyamp--map-backtab vterm-mode-map vterm-send-backtab)
  (keyamp--map-tab vterm-mode-map vterm-send-tab)
  (keyamp--map vterm-mode-map '(("C-r" . delete-other-windows)))

  (keyamp--remap vterm-mode-map
    '((select-block        . vterm-up)
      (prev-eww-buffer     . vterm-clear)
      (paste-or-prev       . vterm-yank)
      (paste-from-r1       . vterm-yank-pop)
      (shrink-whitespaces  . vterm-tmux-copy-mode)
      (cut-text-block      . vterm-shell-vi-cmd)
      (toggle-comment      . vterm-read-send-key)
      (open-line           . prev-vterm-buffer)
      (newline             . next-vterm-buffer)
      (cut-line            . vterm-tmux-next-window)
      (new-empty-buffer    . vterm-tmux-create-window)
      (insert-space-before . vterm-tmux-close-window)
      (del-back            . vterm-send-backspace)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(vterm-send-backspace . nil))
    (keyamp--set keymap '(vterm-send-backspace)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--remap keymap '((previous-line . vterm-up) (next-line . vterm-down)))
    (keyamp--set keymap '(vterm-history-search) nil :insert)
    (keyamp--set keymap '(vterm-up vterm-down vterm-yank-pop) :command))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(backward-char . forward-char))
    (keyamp--remap keymap '((backward-char . vterm-left) (forward-char . vterm-right)))
    (keyamp--set keymap '(vterm-left vterm-right vterm-reset-cursor-point) :command))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(vterm-up . self-insert-command))
    (keyamp--map-backtab keymap vterm-history-search)
    (keyamp--map-tab keymap change-wd)
    (keyamp--hook keymap '(vterm-mode-hook) nil nil :repeat)
    (keyamp--set keymap '(vterm-send-return term-interrupt-subjob) nil :insert)

    (defun keyamp-input-timer-payload-vterm ()
      "Set transient keymap for vterm after input timer timeout."
      (if (eq major-mode 'vterm-mode)
          (keyamp-repeat-deactivate-init keymap)))

    (advice-add 'keyamp-input-timer-payload :after 'keyamp-input-timer-payload-vterm))

  (add-hook 'vterm-mode-hook 'keyamp-input-timer)
  (advice-add-macro '(vterm-send-return term-interrupt-subjob) :after 'keyamp-input-timer)

  (with-sparse-keymap
    (keyamp--map-leader keymap '(vterm-tmux-prev-window . vterm-tmux-next-window))
    (keyamp--remap keymap '((open-line . vterm-tmux-prev-window) (newline . vterm-tmux-next-window)))
    (keyamp--set keymap '(vterm-tmux-prev-window vterm-tmux-next-window)))

  ;;; shell prompt vi cmd mode
  (with-sparse-keymap ; map must be set for bash and zsh
    (keyamp--remap keymap
      '((backward-char     . vterm-shell-vi-cmd-self-insert)
        (forward-char      . vterm-shell-vi-cmd-s)
        (back-word         . vterm-shell-vi-cmd-l)
        (forw-word         . vterm-shell-vi-cmd-w)
        (del-back          . vterm-shell-vi-cmd-e)
        (undo              . vterm-shell-vi-cmd-self-insert)
        (beg-of-line       . vterm-shell-vi-cmd-self-insert)
        (end-of-lyne       . vterm-shell-vi-cmd-self-insert)
        (del-word          . vterm-shell-vi-cmd-self-insert)
        (backward-del-word . vterm-shell-vi-cmd-self-insert)))
    (keyamp--set keymap
      '(vterm-up ; cmd mode after list history
        vterm-down
        vterm-shell-vi-cmd
        vterm-shell-vi-cmd-self-insert
        vterm-shell-vi-cmd-s
        vterm-shell-vi-cmd-l
        vterm-shell-vi-cmd-w
        vterm-shell-vi-cmd-e))
    (add-hook 'keyamp-insert-hook 'vterm-shell-vi-insert))

  (with-sparse-keymap ; word move repeat
    (keyamp--remap keymap
      '((backward-char . vterm-shell-vi-cmd-l)
        (forward-char  . vterm-shell-vi-cmd-w)))
    (keyamp--set keymap '(vterm-shell-vi-cmd-l vterm-shell-vi-cmd-w)))

  (with-sparse-keymap ; delete char repeat
    (keyamp--map-leader keymap '(vterm-shell-vi-cmd-e . nil))
    (keyamp--set keymap '(vterm-shell-vi-cmd-e)))

  ;;; tmux copy mode
  (with-sparse-keymap
    (keyamp--map-leader keymap '(vterm-tmux-copy-mode-dot . vterm-tmux-copy-mode-n))
    (keyamp--remap keymap
      '((previous-line      . vterm-tmux-copy-mode-self-insert)
        (backward-char      . vterm-tmux-copy-mode-self-insert)
        (next-line          . vterm-tmux-copy-mode-self-insert)
        (forward-char       . vterm-tmux-copy-mode-self-insert)
        (beg-of-line        . vterm-tmux-copy-mode-dot)
        (end-of-lyne        . vterm-tmux-copy-mode-n)
        (shrink-whitespaces . vterm-tmux-copy-mode-q)
        (copy-line          . vterm-tmux-copy-mode-self-insert)
        (activate-region    . vterm-tmux-copy-mode-self-insert)
        (isearch-forward    . vterm-tmux-copy-mode-self-insert)))
    (keyamp--set keymap
      '(vterm-tmux-copy-mode vterm-tmux-copy-mode-self-insert
        vterm-tmux-copy-mode-dot vterm-tmux-copy-mode-n)))

  (with-sparse-keymap ; repeat page move
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--remap keymap
      '((previous-line . vterm-tmux-copy-mode-dot)
        (next-line     . vterm-tmux-copy-mode-n)))
    (keyamp--set keymap '(vterm-tmux-copy-mode-dot vterm-tmux-copy-mode-n))))

(with-eval-after-load 'info
  (keyamp--remap Info-mode-map
    '((keyamp-insert . Info-follow-nearest-node)
      (open-line     . Info-backward-node)
      (newline       . Info-forward-node)
      (undo          . Info-up)
      (del-back      . Info-next-reference)
      (previous-line . up-line-rev)
      (next-line     . down-line)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(open-line . newline))
    (keyamp--set keymap '(Info-backward-node Info-forward-node)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(Info-prev-reference . Info-next-reference))
    (keyamp--remap keymap '((undo . Info-prev-reference) (del-back . Info-next-reference)))
    (keyamp--set keymap '(Info-prev-reference Info-next-reference))))

(with-eval-after-load 'help-mode
  (push 'help-mode keyamp-button-move-modes)
  (keyamp--remap help-mode-map
    '((undo          . button-back)  (del-back . button-forw)
      (open-line     . help-go-back) (newline  . help-go-forward)
      (keyamp-insert . keyamp-escape))))

(with-eval-after-load 'timer-list
  (keyamp--remap timer-list-mode-map
    '((insert-space-before . timer-list-cancel) (proced-defer . list-timers)
      (point-to-register   . org-timer-notify))))

(with-eval-after-load 'gnus-topic
  (keyamp--map-tab gnus-topic-mode-map gnus-topic-select-group)
  (keyamp--map gnus-topic-mode-map '(("<mouse-1>" . gnus-topic-select-group)))
  (keyamp--remap gnus-topic-mode-map
    '((keyamp-insert . gnus-topic-select-group)
      (previous-line . up-line-rev)        (next-line     . down-line)
      (beg-of-line   . gnus-topic-prev)    (end-of-lyne   . gnus-topic-next)
      (beg-of-block  . gnus-topic-prev)    (end-of-block  . gnus-topic-next)
      (back-char     . screen-idle-escape) (forw-char     . screen-idle)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--remap keymap '((previous-line . up-line-rev) (next-line . down-line)))
    (keyamp--set keymap '(gnus-topic-select-group)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--remap keymap
      '((previous-line . gnus-topic-prev) (next-line    . gnus-topic-next)
        (beg-of-line   . gnus-beg-of-buf) (end-of-lyne  . gnus-end-of-buf)
        (end-of-block  . gnus-beg-of-buf) (beg-of-block . gnus-end-of-buf)))
    (keyamp--set keymap
      '(gnus-topic-prev gnus-topic-next gnus-beg-of-buf gnus-end-of-buf) nil nil nil 2)))

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
      (cut-line          . prev-eww-buffer)
      (copy-line         . screen-idle)
      (paste-or-prev     . tasks)
      (backward-bracket  . downloads)
      (forward-bracket   . save-close-buf)
      (kmacro-helper     . config)
      (point-to-register . sql))))

(with-eval-after-load 'gnus-art
  (push 'gnus-article-mode keyamp-lines-move-modes)
  (keyamp--remap gnus-mime-button-map '((keyamp-insert . gnus-article-press-button)))
  (keyamp--remap gnus-article-mode-map '((undo . button-back) (del-back . button-forw))))

(with-eval-after-load 'gnus-sum
  (push 'gnus-summary-mode keyamp-lines-move-modes)
  (keyamp--map gnus-summary-mode-map '(("<mouse-1>" . gnus-summary-scroll-up)))
  (keyamp--map-tab gnus-summary-mode-map page-dn-half)
  (keyamp--remap gnus-summary-mode-map
    '((previous-line . up-line-rev)
      (next-line     . down-line)
      (back-char     . screen-idle-escape)
      (forw-char     . screen-idle)
      (keyamp-insert . gnus-summary-scroll-up)
      (open-line     . gnus-summary-prev-group)
      (del-back      . toggle-gnus)
      (newline       . gnus-summary-next-group)
      (paste-or-prev . tasks)
      (paste-from-r1 . gnus-summary-save-parts)
      (save-buffer   . gnus-summary-save-parts)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(up-line . down-line))
    (keyamp--map-tab keymap keyamp-TAB)
    (keyamp--remap keymap
      '((open-line . gnus-summary-prev-group) (newline . gnus-summary-next-group)
        (up-line   . toggle-gnus)))
    (keyamp--set keymap
      '(gnus-summary-prev-group gnus-summary-next-group gnus-delete-window-article
        screen-idle-return))
    (keyamp--hook keymap '(gnus-summary-prepared-hook) nil nil :repeat))

  (with-sparse-keymap
    ;; Press RET to open an article then RET again to close it.
    ;; SPC/DEL to switch to the article then SPC/DEL to continue move by links.
    (keyamp--map-leader keymap '(other-window . other-window))
    (keyamp--map-return keymap gnus-delete-window-article)
    (keyamp--set keymap '(gnus-summary-scroll-up))))

(with-eval-after-load 'gnus-srvr
  (keyamp--remap gnus-server-mode-map
    '((keyamp-insert . gnus-server-read-server) (del-back . gnus-server-exit)))
  (keyamp--remap gnus-browse-mode-map
    '((keyamp-insert . gnus-browse-select-group))))

(with-eval-after-load 'recentf
  (keyamp--remap recentf-dialog-mode-map
    '((keyamp-escape     . recentf-cancel-dialog)
      (eshell-split      . recentf-open-most-recent-file-0)
      (kmacro-record     . recentf-open-most-recent-file-1)
      (kmacro-play       . recentf-open-most-recent-file-2)
      (kmacro-helper     . recentf-open-most-recent-file-3)
      (append-to-r1      . recentf-open-most-recent-file-4)
      (terminal          . recentf-open-most-recent-file-5)
      (pass              . recentf-open-most-recent-file-6)
      (jump-to-register  . recentf-open-most-recent-file-7)
      (point-to-register . recentf-open-most-recent-file-8)
      (proced-defer      . recentf-open-most-recent-file-9)))
  (with-sparse-keymap
    (keyamp--map-leader keymap '(widget-backward . widget-forward))
    (keyamp--set keymap '(recentf-open-files))))

(with-sparse-keymap
  (keyamp--remap keymap
    '((eshell-split      . radio-channel-0) (kmacro-record    . radio-channel-1)
      (kmacro-play       . radio-channel-2) (kmacro-helper    . radio-channel-3)
      (append-to-r1      . radio-channel-4) (terminal         . radio-channel-5)
      (pass              . radio-channel-6) (jump-to-register . radio-channel-7)
      (point-to-register . radio-channel-8) (proced-defer     . radio-channel-9)))
  (keyamp--set keymap
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

  (with-sparse-keymap
    (keyamp--map-leader keymap '(snake-move-left . snake-move-right))
    (keyamp--set keymap
      '(snake-start-game snake-pause-game snake-move-left  snake-move-right
        snake-move-down  snake-move-up))
    (keyamp--hook keymap '(snake-mode-hook))))

(with-eval-after-load 'tetris
  (keyamp--remap tetris-mode-map
    '((keyamp-escape . tetris-pause-game)
      (del-back      . tetris-rotate-prev) (alt-buf       . tetris-rotate-prev)
      (newline       . tetris-rotate-next) (next-buf      . tetris-rotate-next)
      (next-line     . tetris-move-bottom) (backward-char . tetris-move-down)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(tetris-move-left . tetris-move-right))
    (keyamp--set keymap
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
  (keyamp--map-backtab find-output-mode-map find-previous-match)
  (keyamp--map-tab find-output-mode-map find-next-match)
  (keyamp--remap find-output-mode-map '((keyamp-insert . find--jump-to-place)))
  (keyamp--map find-output-mode-map '(("<mouse-1>" . find--jump-to-place)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(backward-char . forward-char))
    (keyamp--remap keymap
      '((previous-line . find-previous-file)  (next-line    . find-next-file)
        (backward-char . find-previous-match) (forward-char . find-next-match)))
    (keyamp--set keymap
      '(find-next-match find-previous-file find-previous-match find-next-file))
    (keyamp--hook keymap '(find-output-mode-hook) nil nil :repeat)))

(with-eval-after-load 'emacs-lisp-mode
  (keyamp--map-backtab emacs-lisp-mode-map undo)
  (keyamp--map-tab emacs-lisp-mode-map emacs-lisp-indent)
  (keyamp--remap emacs-lisp-mode-map
    '((reformat-lines . emacs-lisp-remove-paren-pair))))

(with-eval-after-load 'perl-mode
  (keyamp--map-backtab perl-mode-map undo))

(with-sparse-keymap
  (keyamp--map-leader keymap '(backward-char . forward-char))
  (keyamp--remap keymap
    '((backward-char . flymake-goto-prev-error)
      (forward-char  . flymake-goto-next-error)
      (back-word     . flymake-goto-prev-error)
      (forw-word     . flymake-goto-next-error)))
  (keyamp--set keymap '(flymake-goto-prev-error flymake-goto-next-error)))

(with-eval-after-load 'python-mode
  (keyamp--map-backtab python-mode-map python-de-indent)
  (keyamp--map-tab python-mode-map python-indent-or-complete)
  (keyamp--map-return python-mode-map python-return-and-indent)
  (keyamp--remap python-mode-map
    '((newline . python-return-and-indent) (reformat-lines . python-format-buffer)))
  (with-sparse-keymap
    (keyamp--map-leader keymap '(python-de-indent . python-indent-or-complete))
    (keyamp--set keymap '(python-indent-or-complete python-de-indent)
      nil nil nil keyamp-delay-1)))

(with-eval-after-load 'go-ts-mode
  (keyamp--map-tab go-ts-mode-map company-manual-begin)
  (keyamp--remap go-ts-mode-map
    '((describe-foo-at-point . xref-find-definitions)
      (describe-variable     . xref-find-references)
      (mark-defun            . go-mark-defun)
      (stow                  . flymake-show-project-diagnostics)
      (eval-region-or-sexp   . make-run)
      (eval-defun            . make-test)
      (reformat-lines        . eglot-reconnect))))

(with-sparse-keymap
  (keyamp--map-leader keymap '(xref-go-back . xref-find-definitions))
  (keyamp--set keymap '(xref-go-back xref-find-definitions)))

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
  (with-sparse-keymap
    (keyamp--remap keymap '((point-to-register . toggle-sql-type)))
    (keyamp--set keymap '(sql toggle-sql-type exec-query))))

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
      (undo        . delete-window)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(calendar-scroll-right . calendar-scroll-left))
    (keyamp--map-tab keymap calendar-other-month)
    (keyamp--set keymap '(calendar-scroll-left calendar-scroll-right calendar-goto-today))))

(with-eval-after-load 'simple
  (keyamp--remap messages-buffer-mode-map
    '((keyamp-insert     . keyamp-escape)
      (undo              . del-win)
      (del-back          . alt-buf)
      (open-line         . prev-buf)
      (newline           . next-buf)
      (paste-or-prev     . tasks)
      (previous-line     . up-line-rev)
      (next-line         . down-line)
      (backward-del-word . sun-moon)
      (cut-text-block    . calc)
      (cut-line          . prev-eww-buffer)
      (backward-bracket  . downloads)
      (forward-bracket   . save-close-buf)
      (kmacro-helper     . config)
      (point-to-register . sql)))
  (keyamp--remap special-mode-map
    '((undo              . del-win)
      (del-back          . alt-buf)
      (open-line         . prev-buf)
      (newline           . next-buf))))

(with-eval-after-load 'calc
  (advice-add 'calcDigit-start :after 'keyamp-insert)
  (advice-add 'calcDigit-start :after 'keyamp-input-timer))
  (advice-add-macro
    '(calc-plus calc-minus calc-times calc-divide
      calc-mod  calc-inv   calc-power calc-enter) :after 'keyamp-start-input-timer)

(with-eval-after-load 'calc-ext
  (keyamp--remap calc-mode-map
    '((del-back      . calc-pop)       (undo    . calc-undo)
      (open-line     . calc-roll-down) (newline . calc-algebraic-entry)
      (paste-or-prev . calc-yank)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(undo . del-back))
    (keyamp--remap keymap '((del-back . calc-redo)))
    (keyamp--set keymap '(calc-undo calc-redo))

    (defun keyamp-input-timer-payload-calc ()
      "Set transient keymap for calc after input timer timeout."
      (when (eq major-mode 'calc-mode)
        (keyamp-repeat-deactivate-init keymap)))

    (advice-add 'keyamp-input-timer-payload :after 'keyamp-input-timer-payload-calc)))

(with-eval-after-load 'dslide
  (keyamp--remap dslide-mode-map
    '((del-back  . dslide-deck-stop)     (undo    . dslide-deck-start)
      (open-line . dslide-deck-backward) (newline . dslide-deck-forward))))


;; Command indication mapping

(defconst keyamp-screen-commands-hash
  #s(hash-table test equal data
                (async-shell-command                     t
                 calendar-split                          t
                 clock                                   t
                 config                                  t
                 delete-other-windows                    t
                 delete-window                           t
                 describe-char                           t
                 describe-face                           t
                 describe-foo-at-point                   t
                 describe-function                       t
                 describe-key                            t
                 describe-mode                           t
                 describe-variable                       t
                 exec-query                              t
                 find-next-dir-file                      t
                 find-prev-dir-file                      t
                 gnus-summary-scroll-up                  t
                 list-matching-lines                     t
                 next-buffer                             t
                 next-eww-buffer                         t
                 next-eshell-buffer                      t
                 next-vterm-buffer                       t
                 next-proj-buf                           t
                 next-buf                                t
                 occur-cur-word                          t
                 open-in-external-app                    t
                 org-agenda-tasks                        t
                 player                                  t
                 prev-eww-buffer                         t
                 prev-eshell-buffer                      t
                 prev-vterm-buffer                       t
                 prev-proj-buf                           t
                 prev-buf                                t
                 previous-buffer                         t
                 save-close-buf                          t
                 server                                  t
                 split-window-below                      t
                 split-window-horizontally               t
                 sun-moon                                t
                 sync                                    t
                 tasks                                   t
                 test                                    t
                 view-messages                           t
                 vterm-tmux-prev-window                  t
                 vterm-tmux-next-window                  t)))

(defconst keyamp-screen-read-commands-hash
  #s(hash-table test equal data
                (async-shell-command                     t
                 calendar-split                          t
                 clock                                   t
                 describe-char                           t
                 describe-face                           t
                 describe-function                       t
                 describe-key                            t
                 describe-mode                           t
                 describe-variable                       t
                 exec-query                              t
                 gnus-summary-scroll-up                  t
                 list-matching-lines                     t
                 occur-cur-word                          t
                 open-in-external-app                    t
                 player                                  t
                 sun-moon                                t
                 sync                                    t
                 view-messages                           t
                 describe-foo-at-point                   t)))

(defconst keyamp-modify-commands-hash
  #s(hash-table test equal data
                (apply-macro-to-region-lines             t
                 backward-del-word                       t
                 backward-delete-char-untabify           t
                 clean-whitespace                        t
                 cut-line                                t
                 cycle-hyphen-lowline-space              t
                 del-back                                t
                 delete-backward-char                    t
                 del-forw                                t
                 del-word                                t
                 delete-duplicate-lines                  t
                 delete-forward-char                     t
                 delete-matching-lines                   t
                 delete-non-matching-line                t
                 dired-toggle-mark                       t
                 emoji-insert                            t
                 eval-defun                              t
                 eval-region-or-sexp                     t
                 fill-or-unfil                           t
                 hippie-expand                           t
                 ibuffer-do-delete                       t
                 insert-ascii-double-quote               t
                 insert-ascii-single-quote               t
                 insert-backtick-quote                   t
                 insert-brace                            t
                 insert-char                             t
                 insert-column-a-z                       t
                 insert-date                             t
                 insert-double-angle-quote               t
                 insert-double-curly-quote               t
                 insert-emacs-quote                      t
                 insert-formfeed                         t
                 insert-paren                            t
                 insert-space-before                     t
                 insert-square-bracket                   t
                 isearch-del-char                        t
                 json-pretty                             t
                 kill-region                             t
                 new-empty-buffer                        t
                 newline                                 t
                 open-line                               t
                 org-insert-source-code                  t
                 org-delete-backward-char                t
                 org-open-line                           t
                 org-shiftdown                           t
                 org-shiftup                             t
                 org-return                              t
                 python-indent-or-complete               t
                 python-de-indent                        t
                 quote-lines                             t
                 reformat-lines                          t
                 reformat-to-sentence-lines              t
                 run-current-file                        t
                 save-buffer                             t
                 shrink-whitespaces                      t
                 sort-lines-key-value                    t
                 space-to-newline                        t
                 title-case-region-or-line               t
                 todo                                    t
                 toggle-comment                          t
                 toggle-case                             t
                 toggle-prev-letter-case                 t
                 undo                                    t
                 vterm-send-backspace                    t)))

(defconst keyamp-read-commands-hash
  #s(hash-table test equal data
                (back-word-repeat                        t
                 button-back                             t
                 back-char                               t
                 backward-punct                          t
                 beg-of-block                            t
                 beg-of-buf                              t
                 calc-redo                               t
                 calc-undo                               t
                 calendar-goto-today                     t
                 calendar-scroll-left                    t
                 calendar-scroll-right                   t
                 company-manual-begin                    t
                 company-next-page                       t
                 company-previous-page                   t
                 company-select-forw                     t
                 company-select-back                     t
                 completion-at-point                     t
                 comp-back                               t
                 comp-forw                               t
                 copy-line                               t
                 dired-jump                              t
                 dired-mark                              t
                 dired-unmark                            t
                 doc-view-scroll-down-or-previous-page   t
                 doc-view-scroll-up-or-next-page         t
                 doc-view-shrink                         t
                 doc-view-enlarge                        t
                 down-line                               t
                 down-line-rev                           t
                 emms-pause                              t
                 emms-playlist-mode-play-smart           t
                 emms-random                             t
                 emms-seek-backward                      t
                 emms-seek-backward-or-previous          t
                 emms-seek-forward                       t
                 emms-seek-forward-or-next               t
                 end-of-block                            t
                 end-of-buf                              t
                 enlarge-window                          t
                 enlarge-window-horizontally             t
                 eshell-next-input                       t
                 eshell-previous-input                   t
                 eshell-search-input                     t
                 select-word                             t
                 find-next-file                          t
                 find-next-match                         t
                 find-previous-file                      t
                 find-previous-match                     t
                 flymake-goto-next-error                 t
                 flymake-goto-prev-error                 t
                 forw-char                               t
                 forw-word-repeat                        t
                 button-forw                             t
                 forward-punct                           t
                 gnus-beg-of-buf                         t
                 gnus-delete-window-article              t
                 gnus-end-of-buf                         t
                 gnus-summary-next-group                 t
                 gnus-summary-prev-group                 t
                 gnus-topic-next                         t
                 gnus-topic-prev                         t
                 gnus-topic-select-group                 t
                 page-up-half                            t
                 page-dn-half                            t
                 hist-forw                               t
                 hist-back                               t
                 ibuffer-backward-filter-group           t
                 ibuffer-forward-filter-group            t
                 ibuffer-toggle-filter-group             t
                 ido-next-match                          t
                 ido-prev-match                          t
                 image-next-file                         t
                 image-previous-file                     t
                 image-decrease-size                     t
                 image-increase-size                     t
                 Info-backward-node                      t
                 Info-forward-node                       t
                 Info-prev-reference                     t
                 Info-next-reference                     t
                 isearch-wback                           t
                 isearch-wforw                           t
                 isearch-back                            t
                 isearch-forw                            t
                 isearch-ring-advance                    t
                 isearch-ring-retreat                    t
                 isearch-yank-kill                       t
                 jump-mark                               t
                 keyamp--read-dummy                      t
                 minibuffer-previous-completion          t
                 minibuffer-next-completion              t
                 radio-next                              t
                 radio-prev                              t
                 recenter-top-bottom                     t
                 recentf-open-files                      t
                 screen-idle-return                      t
                 scroll-down-command                     t
                 scroll-up-command                       t
                 select-block                            t
                 select-line                             t
                 select-quote                            t
                 shrink-window                           t
                 shrink-window-horizontally              t
                 text-scale-decrease                     t
                 text-scale-increase                     t
                 text-scale-reset                        t
                 toggle-truncate-lines                   t
                 translate                               t
                 up-line                                 t
                 up-line-rev                             t
                 vterm-down                              t
                 vterm-left                              t
                 vterm-right                             t
                 vterm-send-return                       t
                 vterm-up                                t
                 vterm-tmux-copy-mode                    t
                 vterm-tmux-copy-mode-d                  t
                 vterm-tmux-copy-mode-h                  t
                 vterm-tmux-copy-mode-t                  t
                 vterm-tmux-copy-mode-s                  t
                 vterm-tmux-copy-mode-d                  t
                 vterm-tmux-copy-mode-n                  t
                 vterm-tmux-copy-mode-dot                t
                 widget-backward                         t
                 widget-forward                          t
                 xref-find-definitions                   t
                 xref-go-back                            t)))

(defconst keyamp-read-screen-commands-hash
  #s(hash-table test equal data
                (beg-of-block                            t
                 beg-of-buf                              t
                 doc-view-scroll-down-or-previous-page   t
                 doc-view-scroll-up-or-next-page         t
                 down-line                               t
                 down-line-rev                           t
                 end-of-block                            t
                 end-of-buf                              t
                 gnus-beg-of-buf                         t
                 gnus-end-of-buf                         t
                 gnus-summary-next-group                 t
                 gnus-summary-prev-group                 t
                 gnus-topic-next                         t
                 gnus-topic-prev                         t
                 gnus-topic-select-group                 t
                 page-up-half                            t
                 page-dn-half                            t
                 ibuffer-backward-filter-group           t
                 ibuffer-forward-filter-group            t
                 ibuffer-toggle-filter-group             t
                 isearch-wback                           t
                 isearch-wforw                           t
                 isearch-back                            t
                 isearch-forw                            t
                 scroll-down-command                     t
                 scroll-up-command                       t
                 vterm-tmux-copy-mode                    t
                 vterm-tmux-copy-mode-d                  t
                 vterm-tmux-copy-mode-h                  t
                 vterm-tmux-copy-mode-t                  t
                 vterm-tmux-copy-mode-s                  t
                 vterm-tmux-copy-mode-d                  t
                 vterm-tmux-copy-mode-n                  t
                 vterm-tmux-copy-mode-dot                t
                 up-line                                 t
                 up-line-rev                             t)))

(defconst keyamp-idle-commands
  '(ignore monitor activate-region dummy mouse-drag-region)
  "List of indicate idle commands.")

(defconst keyamp-isearch-not-insert
  '(isearch-forw isearch-wforw isearch-back isearch-wback)
  "List of excluded commands from indicate insert mode in isearch.")

(defconst keyamp-blink-modify-commands
  '(kmacro-record stopwatch python-format-buffer)
  "List of commands to `keyamp-blink-modify' after.")

(defconst keyamp-blink-io-commands
  '(vterm-shell-vi-cmd-self-insert
    vterm-shell-vi-cmd-l
    vterm-shell-vi-cmd-w
    vterm-shell-vi-cmd-e
    vterm-read-send-key)
  "List of commands to `keyamp-blink-io' after.")

(defconst keyamp-insert-commands
  '(self-insert-command org-self-insert-command isearch-printing-char)
  "List of insert commands.")

(defconst keyamp-screen-command-commands
  '(dired-find-file ibuffer-visit-buffer open-last-closed
    bookmark-jump   widget-button-press  alt-buf
    alternate-frame)
  "List of command screen commands.")



(defgroup keyamp nil "Customization options for keyamp."
  :group 'help :prefix "keyamp-")

(defvar keyamp-command-hook nil "Hook for `keyamp-command'.")
(defvar keyamp-insert-hook  nil "Hook for `keyamp-insert'.")

(defconst keyamp-karabiner-cli
  "/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli"
  "Karabiner-Elements CLI executable. Optional for mode sync.")

(defvar keyamp-idle-indicator    "∙" "Idle indicator.")
(defvar keyamp-screen-indicator  "∙" "Screen indicator.")
(defvar keyamp-read-indicator    "∙" "Read indicator.")
(defvar keyamp-command-indicator "∙" "Command indicator.")
(defvar keyamp-io-indicator      "∙" "IO indicator.")
(defvar keyamp-insert-indicator  "∙" "Insert indicator.")
(defvar keyamp-modify-indicator  "∙" "Modify indicator.")

(defvar keyamp-idle-color    "#ab82ff" "Idle color.")
(defvar keyamp-screen-color  "#1e90ff" "Screen color.")
(defvar keyamp-read-color    "#00bfff" "Read color.")
(defvar keyamp-command-color "#7cfc00" "Command color.")
(defvar keyamp-accent-color  "#5fff00" "Command accent color.")
(defvar keyamp-io-color      "#ffd700" "IO color.")
(defvar keyamp-insert-color  "#ff8c00" "Insert color.")
(defvar keyamp-modify-color  "#ff0000" "Modify color.")

(defface mode-line-front-space-face
  `((t :foreground ,keyamp-command-color :bold t))
  "Mode line front space face.")

(defvar keyamp-command-cursor 'box        "Command cursor.")
(defvar keyamp-insert-cursor  '(hbar . 2) "Insert cursor.")
(defvar keyamp-read-cursor    'hollow     "Read cursor.")
(defvar keyamp-screen-cursor  nil         "Screen cursor.")
(defvar keyamp-modify-cursor  '(bar . 2)  "Modify cursor.")

(defvar keyamp-idle-timeout 60 "Idle timeout for keymaps without self timeout.")
(defvar keyamp-defer-load 5 "Defer load second priority features.")


;; Input timer

(defconst keyamp-input-timeout 3 "Input timeout.")
(defvar keyamp-input-timer nil
  "Timer activates repeat read mode if no command follows. Any command or self
insert cancel the timer.")

(defun keyamp-cancel-input-timer ()
  "Cancel `keyamp-input-timer'."
  (remove-hook 'post-command-hook 'keyamp-cancel-input-timer)
  (remove-hook 'post-self-insert-hook 'keyamp-cancel-input-timer)
  (if (timerp keyamp-input-timer)
      (cancel-timer keyamp-input-timer)))

(defun keyamp-input-timer-payload ()
  "Payload for `keyamp-input-timer'."
  (keyamp-cancel-input-timer)
  (keyamp-command)
  (keyamp-indicate-read-defer)
  (keyamp-blink-start keyamp-idle-color keyamp-read-color))

(defun keyamp-start-input-timer (&rest _)
  "Start `keyamp-input-timer'."
  (remove-hook 'post-command-hook 'keyamp-start-input-timer)
  (keyamp-cancel-input-timer)
  (add-hook 'post-command-hook 'keyamp-cancel-input-timer)
  (add-hook 'post-self-insert-hook 'keyamp-cancel-input-timer)
  (setq keyamp-input-timer
        (run-with-timer keyamp-input-timeout nil 'keyamp-input-timer-payload)))

(defun keyamp-input-timer (&rest _)
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
    (add-hook 'keyamp-insert-hook    'keyamp-insert-karabiner)
    (add-hook 'keyamp-command-hook   'keyamp-command-karabiner)
    (add-hook 'isearch-mode-hook     'keyamp-insert-karabiner)
    (add-hook 'isearch-mode-end-hook 'keyamp-command-karabiner)))


;; Modes

(defvar keyamp-insert-p t "Non-nil means insert is on.")
(defvar keyamp--deactivate-command-fun nil)

(defun keyamp-command-init ()
  "Set command mode."
  (keyamp-repeat-deactivate)
  (when keyamp-insert-p
    (setq keyamp-insert-p nil)
    (when (buffer-file-name)
      (point-to-register ?7)
      (push-mark (point) t)))
  (if keyamp--deactivate-command-fun
      (funcall keyamp--deactivate-command-fun))
  (setq keyamp--deactivate-command-fun
        (set-transient-map keyamp-command-map (lambda () t)))
  (keyamp-indicate-command)
  (if keyamp-led
      (keyamp-led-on)))

(defun keyamp-insert-init (&rest _)
  "Enter insert mode."
  (keyamp-cancel-repeat-idle-timer)
  (setq keyamp-insert-p t)
  (funcall keyamp--deactivate-command-fun))

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

(defun keyamp-SPC-SPC (&rest _)
  "Insert SPC SPC to activate command mode."
  (if (and (eq before-last-command-event 32)
           (eq last-command-event 32))
      (if isearch-mode
          (isearch-cancel-clean)
        (delete-char -2)
        (save-buffer-silent-defer)
        (keyamp-command-execute 'keyamp-escape))
    (if (eq last-command-event 32)
        (run-with-timer 0 nil 'before-last-command-event 32)
      (setq before-last-command-event nil))))

(add-hook 'post-self-insert-hook 'keyamp-SPC-SPC)
(advice-add-macro '(isearch-printing-char minibuffer-complete-word)
                  :after 'keyamp-SPC-SPC)

(defun keyamp-SPC-DEL (&rest _)
  "Insert SPC DEL to activate command mode."
  (if (eq before-last-command-event 32)
      (if isearch-mode
          (isearch-cancel-clean)
        (save-buffer-silent-defer)
        (keyamp-command-execute 'keyamp-escape))))

(advice-add-macro '(delete-backward-char isearch-del-char) :after 'keyamp-SPC-DEL)

(defun keyamp-command-if-insert (&rest _)
  "Activate command mode if insert mode."
  (if keyamp-insert-p
      (keyamp-command)))

(defun keyamp-insert-and-SPC ()
  "Activate insert mode and insert SPC."
  (interactive)
  (unless keyamp-insert-p (keyamp-insert))
  (insert " "))

(defun keyamp-self-insert-and-insert ()
  "Self insert and activate insert mode."
  (interactive)
  (keyamp-insert)
  (self-insert-command 1))

(defun keyamp-minibuffer-return ()
  "Exit if file completion. It means use content of minibuffer as it is,
no select completion candidates. Else force complete and exit, that
is, select and use first completion candidate. In case file
completion, for most cases no need to complete, because there is NO
right candidate. Otherwise, in all cases one MUST select a candidate.
Simply hit TAB to minibuffer-complete file name if the name exists."
  (interactive)
  (if (eq (icomplete--category) 'file)
      (exit-minibuffer)
    (icomplete-force-complete-and-exit)))

(defun keyamp-minibuffer-y-or-n ()
  "Return t if asked literal y or n question."
  (if-let ((x (minibuffer-prompt)))
      (string-match "y, n, !\\|yn!q" x)))

(defun keyamp-minibuffer-escape ()
  "If minibuffer input not empty then activate command mode instead
of quit minibuffer. Answer q to literal y or n question."
  (interactive)
  (if (keyamp-minibuffer-y-or-n)
      (progn (keyamp-insert-init)
             (execute-kbd-macro (kbd "q")))
    (if (keyamp-minibuffer-empty)
        (keyamp-minibuffer-quit)
      (keyamp-command))))

(defun keyamp-minibuffer-empty ()
  "Return true if minibuffer prompt empty."
  (and (minibufferp)
       (zerop (- (buffer-size) (length (minibuffer-prompt))))))

(defun keyamp-minibuffer-match (String)
  "Return true if minibuffer prompt match STRING."
  (string-match String (minibuffer-prompt)))

(defun keyamp-defer-command-bookmark (Fun)
  "To defer bookmark command `last-nonmenu-event' must be not list."
  (run-at-time nil nil (lambda () (let ((last-nonmenu-event t))
                                    (keyamp-command-execute Fun)))))

(defun keyamp-copy-minibuffer ()
  "Kill minibuffer content to ring for reuse."
  (kill-new (buffer-substring-no-properties
             (1+ (length (minibuffer-prompt))) (point-max))))

(defun keyamp-minibuffer-backtab ()
  "Quit minibuffer and call some other command. Single motion switch."
  (interactive)
  (keyamp-copy-minibuffer)
  (cond ((keyamp-minibuffer-match "Describe function")
         (keyamp-defer-command 0 'describe-variable))
        ((keyamp-minibuffer-match "Describe variable")
         (keyamp-defer-command 0 'describe-function))
        ((keyamp-minibuffer-match "Jump to bookmark")
         (keyamp-defer-command 0 'switch-to-buffer))
        ((keyamp-minibuffer-match "M-x")
         (keyamp-defer-command 0 'describe-function))
        ((keyamp-minibuffer-match "Query replace")
         (keyamp-defer-command 0 'query-replace-regexp))
        ((keyamp-minibuffer-match "Search")
         (keyamp-defer-command 0 'find-name-dired))
        ((keyamp-minibuffer-match "Find-name")
         (keyamp-defer-command 0 'search-string))
        ((keyamp-minibuffer-match "Rename visited file")
         (keyamp-defer-command 0 'write-file))
        ((keyamp-minibuffer-match "Find file")
         (keyamp-defer-command 0 'rename-visited-file))
        ((keyamp-minibuffer-match "Write file")
         (keyamp-defer-command 0 'find-file))
        ((keyamp-minibuffer-match "Copy")
         (keyamp-defer-command 0 'dired-do-rename))
        ((keyamp-minibuffer-match "Rename")
         (keyamp-defer-command 0 'dired-do-copy))
        ((keyamp-minibuffer-match "Switch to buffer")
         (keyamp-defer-command-bookmark 'bookmark-jump))
        ((keyamp-minibuffer-match "Set bookmark named")
         (keyamp-defer-command-bookmark 'bookmark-delete))
        ((keyamp-minibuffer-match "Old bookmark name")
         (keyamp-defer-command-bookmark 'bookmark-rename))
        ((keyamp-minibuffer-match "Delete bookmark")
         (keyamp-defer-command-bookmark 'bookmark-set)))
  (abort-recursive-edit))

(defun keyamp-minibuffer-tab ()
  "Quit minibuffer and call some other command. Single motion switch."
  (interactive)
  (keyamp-copy-minibuffer)
  (cond ((keyamp-minibuffer-match "Describe function")
         (keyamp-defer-command 0 'describe-variable))
        ((keyamp-minibuffer-match "Describe variable")
         (keyamp-defer-command 0 'describe-function))
        ((keyamp-minibuffer-match "Jump to bookmark")
         (keyamp-defer-command 0 'switch-to-buffer))
        ((keyamp-minibuffer-match "M-x")
         (keyamp-defer-command 0 'describe-variable))
        ((keyamp-minibuffer-match "Query replace")
         (keyamp-defer-command 0 'query-replace-regexp))
        ((keyamp-minibuffer-match "Search")
         (keyamp-defer-command 0 'find-name-dired))
        ((keyamp-minibuffer-match "Find-name")
         (keyamp-defer-command 0 'search-string))
        ((keyamp-minibuffer-match "Rename visited file")
         (keyamp-defer-command 0 'find-file))
        ((keyamp-minibuffer-match "Find file")
         (keyamp-defer-command 0 'write-file))
        ((keyamp-minibuffer-match "Write file")
         (keyamp-defer-command 0 'rename-visited-file))
        ((keyamp-minibuffer-match "Copy")
         (keyamp-defer-command 0 'dired-do-rename))
        ((keyamp-minibuffer-match "Rename")
         (keyamp-defer-command 0 'dired-do-copy))
        ((keyamp-minibuffer-match "Switch to buffer")
         (keyamp-defer-command-bookmark 'bookmark-jump))
        ((keyamp-minibuffer-match "Set bookmark named")
         (keyamp-defer-command-bookmark 'bookmark-rename))
        ((keyamp-minibuffer-match "Old bookmark name")
         (keyamp-defer-command-bookmark 'bookmark-delete))
        ((keyamp-minibuffer-match "Delete bookmark")
         (keyamp-defer-command-bookmark 'bookmark-set)))
  (abort-recursive-edit))

(defun keyamp-input (Key)
  "Activate insert mode and input KEY or answer literal y or n question."
  (keyamp-insert-init)
  (if (keyamp-minibuffer-y-or-n)
      (execute-kbd-macro (kbd (keyamp--convert-kbd-str Key)))
    (insert (keyamp--convert-kbd-str Key))))

(defun keyamp-insert-1 ()         (interactive) (keyamp-input "1"))
(defun keyamp-insert-2 ()         (interactive) (keyamp-input "2"))
(defun keyamp-insert-3 ()         (interactive) (keyamp-input "3"))
(defun keyamp-insert-4 ()         (interactive) (keyamp-input "4"))
(defun keyamp-insert-5 ()         (interactive) (keyamp-input "5"))
(defun keyamp-insert-6 ()         (interactive) (keyamp-input "6"))
(defun keyamp-insert-7 ()         (interactive) (keyamp-input "7"))
(defun keyamp-insert-8 ()         (interactive) (keyamp-input "8"))
(defun keyamp-insert-9 ()         (interactive) (keyamp-input "9"))
(defun keyamp-insert-0 ()         (interactive) (keyamp-input "0"))

(defun keyamp-insert-q ()         (interactive) (keyamp-input "q"))
(defun keyamp-insert-w ()         (interactive) (keyamp-input "w"))
(defun keyamp-insert-e ()         (interactive) (keyamp-input "e"))
(defun keyamp-insert-r ()         (interactive) (keyamp-input "r"))
(defun keyamp-insert-t ()         (interactive) (keyamp-input "t"))
(defun keyamp-insert-y ()         (interactive) (keyamp-input "y"))
(defun keyamp-insert-u ()         (interactive) (keyamp-input "u"))
(defun keyamp-insert-i ()         (interactive) (keyamp-input "i"))
(defun keyamp-insert-o ()         (interactive) (keyamp-input "o"))
(defun keyamp-insert-p ()         (interactive) (keyamp-input "p"))
(defun keyamp-insert-bracket ()   (interactive) (keyamp-input "["))

(defun keyamp-insert-a ()         (interactive) (keyamp-input "a"))
(defun keyamp-insert-s ()         (interactive) (keyamp-input "s"))

(defun keyamp-insert-d ()
  "D answers N to y or n question if asked."
  (interactive)
  (if (string-match "y or n" (minibuffer-prompt))
      (y-or-n-p-insert-n)
    (keyamp-input "d")))

(defun keyamp-insert-f ()         (interactive) (keyamp-input "f"))

(defun keyamp-insert-g ()
  "G switches minibuffer. Input almost never starts with comma.
Engram layout has comma in place of G. Choose similar for your layout."
  (interactive)
  (if (string-equal keyamp-current-layout "engineer-engram")
      (keyamp-minibuffer-tab)
    (keyamp-input "g")))

(defun keyamp-insert-h ()         (interactive) (keyamp-input "h"))
(defun keyamp-insert-j ()         (interactive) (keyamp-input "j"))

(defun keyamp-insert-k ()
  "K answers Y to y or n question if asked."
  (interactive)
  (if (string-match "y or n" (minibuffer-prompt))
      (y-or-n-p-insert-y)
    (keyamp-input "k")))

(defun keyamp-insert-l ()         (interactive) (keyamp-input "l"))
(defun keyamp-insert-semicolon () (interactive) (keyamp-input ";"))
(defun keyamp-insert-quote ()     (interactive) (keyamp-input "'"))

(defun keyamp-insert-z ()         (interactive) (keyamp-input "z"))
(defun keyamp-insert-x ()         (interactive) (keyamp-input "x"))
(defun keyamp-insert-c ()         (interactive) (keyamp-input "c"))

(defun keyamp-insert-v ()
  "V pastes by default, input preferable sometimes."
  (interactive)
  (if (or (keyamp-minibuffer-match "M-x")
          (keyamp-minibuffer-match "Jump to bookmark")
          (keyamp-minibuffer-match "Switch to buffer"))
      (keyamp-input "v")
    (keyamp-command-execute 'paste-or-prev)))

(defun keyamp-insert-b ()         (interactive) (keyamp-input "b"))

(defun keyamp-insert-n ()
  "N starts I-search in minibuffer, backward to fetch last history first.
Engram layout has ? in place of N."
  (interactive)
  (keyamp-command-execute 'isearch-backward))

(defun keyamp-insert-N ()         (interactive) (keyamp-input "N"))
(defun keyamp-insert-m ()         (interactive) (keyamp-input "m"))
(defun keyamp-insert-comma ()     (interactive) (keyamp-input ","))
(defun keyamp-insert-dot ()       (interactive) (keyamp-input "."))
(defun keyamp-insert-slash ()     (interactive) (keyamp-input "/"))

(setq-default cursor-in-non-selected-windows nil)

(defun keyamp-cancel-indicate-read-timer ()
  "Cancel indicate read timer."
  (when (timerp keyamp-indicate-read-timer)
    (cancel-timer keyamp-indicate-read-timer)
    (setq keyamp-indicate-read-timer nil)))

(defun keyamp-cursor-type (Cursor)
  "Set cursor type."
  (modify-all-frames-parameters `((cursor-type . ,Cursor))))

(defun keyamp-indicator (Indicator)
  "Set `mode-line-front-space' to INDICATOR."
  (setq mode-line-front-space Indicator)
  (unless (eq this-command last-command)
    (force-mode-line-update t)))

(defun keyamp-indicator-color (Color)
  "Set `mode-line-front-space-face' face COLOR."
  (set-face-attribute 'mode-line-front-space-face nil :foreground Color))

(defun keyamp-indicate (Indicator Cursor Color)
  "Indicate mode with INDICATOR, CURSOR and COLOR."
  (keyamp-cancel-indicate-read-timer)
  (keyamp-indicator Indicator)
  (keyamp-cursor-type Cursor)
  (keyamp-indicator-color Color))

(defun keyamp-indicate-read ()
  "Indicate read."
  (if (not keyamp-insert-p)
      (keyamp-indicate keyamp-read-indicator keyamp-read-cursor keyamp-read-color))
  (if (and (eq last-command 'keyamp--read-dummy)
           (minibufferp)
           (not (isearch-minibuffer-prompt)))
      (keyamp-blink-start keyamp-insert-color keyamp-read-color)))

(defvar keyamp-indicate-read-timer nil "Indicate read timer.")

(defun keyamp-indicate-read-defer ()
  "Defer indicate repeat read is active. Runs after first read command exactly
after a delay even if there more read commands follow."
  (unless (memq mode-line-front-space
                `(,keyamp-command-indicator ,keyamp-read-indicator))
    (keyamp-indicate-command))
  (when (gethash this-command keyamp-read-screen-commands-hash)
    (keyamp-indicate-read)
    (keyamp-blink-start keyamp-screen-color keyamp-read-color))
  (when (or (and (not (timerp keyamp-indicate-read-timer))
                 (not (eq mode-line-front-space keyamp-read-indicator)))
            (not (eq last-command this-command)))
    (if (eq mode-line-front-space keyamp-read-indicator)
        (keyamp-indicate-read)
      (keyamp-blink-stop)
      (keyamp-cancel-indicate-read-timer)
      (setq keyamp-indicate-read-timer
            (run-with-timer keyamp-blink-shift nil 'keyamp-indicate-read)))))

(defvar keyamp-repeat-p nil "Non-nil means repeat is on.")
(defvar keyamp--deactivate-repeat-fun nil "Repeat mode deactivate function.")
(defvar keyamp--repeat-idle-timer nil "Repeat mode idle timer.")

(defun keyamp-repeat-init (Keymap)
  "Init repeat mode."
  (setq keyamp-repeat-p t)
  (setq keyamp--deactivate-repeat-fun (set-transient-map Keymap)))

(defun keyamp-cancel-repeat-idle-timer ()
  "Cancel `keyamp--repeat-idle-timer'."
  (if (timerp keyamp--repeat-idle-timer)
      (cancel-timer keyamp--repeat-idle-timer)))

(defun keyamp-repeat-deactivate ()
  "Deactivate repeat."
  (if keyamp-repeat-p
      (setq keyamp-repeat-p nil))
  (if keyamp--deactivate-repeat-fun
      (funcall keyamp--deactivate-repeat-fun))
  (keyamp-cancel-repeat-idle-timer))

(defun keyamp-repeat-deactivate-init (Keymap)
  "Deactivate repeat and init repeat KEYMAP."
  (keyamp-repeat-deactivate)
  (keyamp-repeat-init Keymap))

(defun keyamp-indicate-idle ()
  "Indicate idle."
  (keyamp-indicate keyamp-idle-indicator keyamp-command-cursor keyamp-idle-color))

(defun keyamp-indicate-sleep ()
  "Indicate sleep."
  (keyamp-blink-stop)
  (keyamp-indicate-idle))

(defun keyamp-indicate-screen ()
  "Indicate screen."
  (keyamp-indicate keyamp-screen-indicator keyamp-screen-cursor keyamp-screen-color)
  (cond ((gethash this-command keyamp-screen-read-commands-hash)
         (keyamp-blink-start keyamp-read-color keyamp-screen-color))
        ((eq this-command 'save-close-buf)
         (keyamp-blink-start keyamp-modify-color keyamp-screen-color))
        (t (keyamp-blink-start keyamp-idle-color keyamp-screen-color))))

(defun keyamp-indicate-modify ()
  "Indicate modify."
  (keyamp-indicate keyamp-modify-indicator keyamp-modify-cursor keyamp-modify-color)
  (if (eq this-command 'undo) ; other repeatable modify have timeout
      (keyamp-blink-start keyamp-modify-color keyamp-read-color)))

(defun keyamp-indicate-insert ()
  "Indicate insert."
  (keyamp-indicate keyamp-insert-indicator keyamp-insert-cursor keyamp-insert-color)
  (cond ((gethash this-command keyamp-read-commands-hash)
         (keyamp-blink-start keyamp-read-color keyamp-insert-color))
        ((memq this-command keyamp-insert-commands)
         (keyamp-blink-io))
        ((gethash this-command keyamp-modify-commands-hash)
         (keyamp-blink-modify))))

(defun keyamp-indicate-command ()
  "Indicate command."
  (keyamp-blink-stop)
  (keyamp-indicate keyamp-command-indicator keyamp-command-cursor keyamp-command-color)
  (cond ((memq this-command keyamp-screen-command-commands)
         (keyamp-blink-start keyamp-screen-color keyamp-command-color))
        (t (keyamp-blink-start keyamp-accent-color keyamp-command-color))))

(defvar keyamp-user-error nil
  "True if previous command signaled `user-error'. See `command-error-function'.")

(defun keyamp-transient ()
  "Indicate transient. Run with `post-command-hook'."
  (if keyamp-user-error
      (progn (keyamp-command)
             (setq keyamp-user-error nil))
    (if (and (eq this-command 'mac-mwheel-scroll) ; ease scroll
             (eq mode-line-front-space keyamp-command-indicator))
        (keyamp-indicate keyamp-command-indicator keyamp-command-cursor keyamp-command-color)
      (cond ((or keyamp-insert-p
                 (and isearch-mode
                      (not (memq this-command keyamp-isearch-not-insert))))
             (keyamp-indicate-insert))
            ((gethash this-command keyamp-screen-commands-hash)
             (keyamp-indicate-screen))
            ((gethash this-command keyamp-read-commands-hash)
             (keyamp-indicate-read-defer))
            ((eq real-this-command 'repeat)
             (keyamp-blink-start keyamp-read-color keyamp-modify-color))
            ((gethash this-command keyamp-modify-commands-hash)
             (keyamp-indicate-modify))
            ((memq this-command keyamp-idle-commands)
             (keyamp-indicate-idle))
            (t (keyamp-indicate-command)))
      (if (or defining-kbd-macro
              (memq this-command keyamp-blink-modify-commands)
              (eq major-mode 'wdired-mode))
          (keyamp-blink-modify))
      (if (memq this-command keyamp-blink-io-commands)
          (keyamp-blink-io)))))

(defvar keyamp-indicate-modify-timer nil "Indicate modify timer.")

(defun keyamp-indicate-modify-cancel (Indicator Cursor Color)
  "Cancel indicate modify."
  (if (eq keyamp-modify-indicator mode-line-front-space)
      (keyamp-indicate Indicator Cursor Color)))

(defun keyamp-blink-modify ()
  "Blink modify."
  (let ((xInd mode-line-front-space)
        (xCur (frame-parameter nil 'cursor-type))
        (xCol (face-attribute 'mode-line-front-space-face :foreground)))
    (unless (eq xCol keyamp-modify-color)
      (keyamp-indicate keyamp-modify-indicator xCur keyamp-modify-color)
      (if (timerp keyamp-indicate-modify-timer)
          (cancel-timer keyamp-indicate-modify-timer))
      (setq keyamp-indicate-modify-timer
            (run-with-timer keyamp-blink-blink nil
                            'keyamp-indicate-modify-cancel xInd xCur xCol)))))

(defvar keyamp-indicate-io-timer nil "Indicate IO timer.")

(defun keyamp-indicate-io-cancel (Indicator Cursor Color)
  "Cancel indicate IO."
  (if (eq keyamp-io-indicator mode-line-front-space)
      (keyamp-indicate Indicator Cursor Color)))

(defun keyamp-blink-io ()
  "Blink IO."
  (let ((xInd mode-line-front-space)
        (xCur (frame-parameter nil 'cursor-type))
        (xCol (face-attribute 'mode-line-front-space-face :foreground)))
    (unless (eq xCol keyamp-io-color)
      (keyamp-indicate keyamp-io-indicator xCur keyamp-io-color)
      (if (timerp keyamp-indicate-io-timer)
          (cancel-timer keyamp-indicate-io-timer))
      (setq keyamp-indicate-io-timer
            (run-with-timer keyamp-blink-blink nil
                            'keyamp-indicate-io-cancel xInd xCur xCol)))))

(defun keyamp-indicate-io (&rest _)
  "Indicate IO feedback from emacsclient evals or processes calls.
Cancel after `keyamp-blink-blink'. Double blink."
  (keyamp-blink-io)
  (run-with-timer (* 2 keyamp-blink-blink) nil 'keyamp-blink-io))

(defun after-save-hook-indicate-io ()
  "Indicate IO after save if save not with command and not in insert mode."
  (unless (or keyamp-insert-p this-command)
    (keyamp-indicate-io)))

(add-hook 'after-save-hook 'after-save-hook-indicate-io)

(defconst keyamp-prefix-idle '([32] [127] [backspace] [8])
  "Indicate idle prefixes list: [SPC] [DEL] [backspace] [C-h].")

(defconst keyam-prefix-modify '([32 101] [32 97] [32 111] [17])
  "Indicate modify prefixes list: [SPC D] [SPC F] [SPC E] [C-q] [C-u].")

(defun keyamp-indicate-prefix ()
  "Indicate prefix."
  (cond ((member (this-single-command-keys) keyamp-prefix-idle)
         (keyamp-blink-stop)
         (keyamp-indicate-idle))
        ((or (member (this-single-command-keys) keyam-prefix-modify)
             prefix-arg)
         (keyamp-blink-stop)
         (keyamp-indicate-modify))))

(defvar keyamp-prefix-delay 0.1 "Delay before indicate prefix keymap.")

(defun keyamp-prefix ()
  "Run `keyamp-indicate-prefix' with idle timer."
  (run-with-idle-timer keyamp-prefix-delay t 'keyamp-indicate-prefix))

(defvar keyamp-blink-on-timer nil "Blink indicator on timer.")
(defvar keyamp-blink-off-timer nil "Blink indicator off timer.")

(defconst keyamp-blink 3.5 "Blink period.")
(defconst keyamp-blink-blink 0.5 "Blink duration.")
(defconst keyamp-blink-shift 1 "Blink shift.")

(defun keyamp-blink (Color1 Color2)
  "Blink."
  (keyamp-indicator-color Color1)
  (setq keyamp-blink-off-timer
        (run-with-timer keyamp-blink-blink nil 'keyamp-indicator-color Color2)))

(defun keyamp-blink-stop ()
  "Stop blink."
  (remove-hook 'post-command-hook 'keyamp-blink-stop)
  (if (timerp keyamp-blink-off-timer)
      (cancel-timer keyamp-blink-off-timer))
  (if (timerp keyamp-blink-on-timer)
      (cancel-timer keyamp-blink-on-timer)))

(defun keyamp-blink-start (Color1 Color2)
  "Start blink."
  (keyamp-blink-stop)
  (add-hook 'post-command-hook 'keyamp-blink-stop)
  (setq keyamp-blink-on-timer
        (run-with-timer keyamp-blink-shift keyamp-blink 'keyamp-blink Color1 Color2)))

(defun keyamp-led-on ()
  "Caps lock led on."
  (call-process "/bin/bash" nil 0 nil "-c" "setleds +caps"))

(defvar keyamp-led nil "Keyboard led control.")

(defun keyamp-led-init ()
  "Init led control."
  (if (executable-find "setleds")
      (setq keyamp-led t)))

(defvar keyamp-idle-timer nil "Idle timer.")

(defun keyamp-idle-init ()
  "Idle init."
  (if isearch-mode
      (isearch-cancel))
  (keyamp-command))

(defun keyamp-idle-detect ()
  "Idle detect."
  (if (timerp keyamp-idle-timer)
      (cancel-timer keyamp-idle-timer))
  (setq keyamp-idle-timer
        (run-with-idle-timer keyamp-idle-timeout t 'keyamp-idle-init)))

(defvar keyamp-minibuffer-quit-funs nil
  "List of funs called after minibuffer quit.")

(defun keyamp-minibuffer-quit ()
  "Abort recursive edit and call `keyamp-minibuffer-quit-funs'."
  (run-at-time nil nil
               (lambda ()
                 (mapc (lambda (fun) (if (fboundp fun) (funcall fun)))
                       keyamp-minibuffer-quit-funs)))
  (abort-recursive-edit))

(defun keyamp-escape ()
  "Return to command mode, clear selection or quit minibuffer."
  (interactive)
  (cond ((or keyamp-repeat-p keyamp-insert-p) (keyamp-command))
        ((region-active-p)                    (deactivate-mark))
        ((minibufferp)                        (keyamp-minibuffer-quit))
        ((< 0 (recursion-depth))              (exit-recursive-edit))
        (t                                    (keyamp-command))))

(defun keyamp-features ()
  "Second priority features."
  (keyamp-karabiner-init)
  (keyamp-led-init)
  (keyamp-map-input-source 'russian-computer)
  (keyamp-push-quail-keyboard-layout))

(defun keyamp-defer-load ()
  "Defer load `keyamp-features'."
  (run-with-timer keyamp-defer-load nil 'keyamp-features))

;;;###autoload
(define-minor-mode keyamp
  "Keyboard Amplifier."
  :global t
  :keymap keyamp-map
  (when keyamp
    (keyamp-command)
    (keyamp-catch-tty-ESC)
    (keyamp-idle-detect)
    (keyamp-defer-load)
    (keyamp-prefix)
    (add-hook 'post-command-hook     'keyamp-transient)
    (add-hook 'pre-command-hook      'keyamp-repeat-deactivate)
    (add-hook 'minibuffer-exit-hook  'keyamp-command)
    (add-hook 'minibuffer-exit-hook  'keyamp-deactivate-region)
    (add-hook 'isearch-mode-hook     'keyamp-repeat-deactivate)
    (add-hook 'isearch-mode-end-hook 'keyamp-command)
    (add-hook 'debugger-mode-hook    'keyamp-command)
    (add-function :after after-focus-change-function #'keyamp-command-if-insert)))

(provide 'keyamp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical unresolved)
;; End:
;;; keyamp.el ends here
