;;; keyamp.el --- Keyboard Amplifier -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Egor Maltsev <x0o1@ya.ru>
;; Version: 1.2 2025-01-07 Spectrum
;;          __   _____   __
;;         |__| |_____| |__|
;;
;; IDE workflow with ordinary on-screen keyboard

;;; Commentary:

;; Keyamp provides 3 modes: insert, command and repeat. Command mode
;; based on persistent transient keymap. Repeat mode adds transient
;; remaps on top of command mode for command chains easy repeat
;; during screen positioning, cursor move and editing. Mode line front
;; space color indicates active transient keymap. Repeat mode switched
;; automatically either by advice or with timer.

;; Plug Keyboard Amplifier in:
;;
;; (require 'keyamp)
;; (keyamp)

;;; Code:



(require 'eieio)

(require 'keycom)



(defgroup keyamp nil "Customization options for keyamp."
  :group 'help :prefix "keyamp-")


;; Quail

(defconst keyamp-input-methods '(russian-computer hebrew)
  "Input methods, activate when not available otherwise. See
`toggle-input-method', first one is primary.

On activation Quail package also loaded which required for mapping
translation of corresponding non-ASCII command key sequences and
mapping to non-QWERTY layouts. Mappings defined in QWERTY notation
throughout the code of the package.")

(defvar keyamp-input-method-to-ascii nil "Input method to ASCII char.")

(mapc
 (lambda (method)
   (activate-input-method method)
   (let ((var-name (intern (concat "keyamp-standard-to-" (symbol-name method)))))
     (eval `(defvar ,var-name nil ,(concat "Standard layout to input method "
                                           (symbol-name method) ".")))
     (mapc
      (lambda (map)
        (when-let ((to (char-to-string (car map)))
                   (from (quail-get-translation (cadr map) to 1))
                   ((characterp from))
                   (from (char-to-string from)))
          (push (cons from to) keyamp-input-method-to-ascii)
          (push (cons to from) (symbol-value var-name))))
      (cdr (quail-map))))
   (activate-input-method nil))
 keyamp-input-methods)

(push '("engineer-engram" . "\
                              \
  1@2&3/4$5<6>7*8=9+0\\#|%^`~  \
  bByYoOuU'(\")lLdDwWvVzZ{[    \
  cCiIeEaA,;.:hHtTsSnNqQ}]    \
  gGxXjJkK-_?!rRmMfFpP        \
                              ")
      quail-keyboard-layout-alist)

(defun keyamp--map-command-input-method ()
  "Define non-ASCII command mode sequences for use in `keyamp-command-map'.
Non-ASCII chars come from input method e.g. \"prefix k\" translated to \"prefix л\".
Single non-ASCII chars mapped in `keyamp--map' macro."
  (mapc
   (lambda (method)
     (activate-input-method method)
     (mapc
      (lambda (map)
        (if-let ((to (keyamp--convert-kbd-str (char-to-string (car map))))
                 (from (quail-get-translation (cadr map) to 1))
                 (to (string-to-char to))
                 ((characterp from))
                 ((> from 127)))
            (mapc
             (lambda (modifier)
               (define-key local-function-key-map
                           (vector (append modifier (list from)))
                           (vector (append modifier (list to)))))
             '(nil (control)))))
      (cdr (quail-map))))
     keyamp-input-methods)
  (activate-input-method nil))

(defun toggle-input-method ()
  "Toggle input method command. Activate when input method not available in OS."
  (interactive)
  (let ((method (car keyamp-input-methods)))
    (activate-input-method (if current-input-method nil method))
    (message "%s %s" method (if current-input-method "activated" "deactivated"))))

(defun toggle-standard-to-current-layout ()
  "Toggle translation standard keyboard to `keyamp-current-layout' command.
Activate when `keyamp-current-layout' not available in OS. The layout must
present in `quail-keyboard-layout-alist'."
  (interactive)
  (if (get 'toggle-standard-to-current-layout 'state)
      (progn
        (quail-set-keyboard-layout "standard")
        (put 'toggle-standard-to-current-layout 'state nil)
        (message "Deactivated standard keyboard to %s" keyamp-current-layout))
    (if (assoc keyamp-current-layout quail-keyboard-layout-alist)
        (quail-set-keyboard-layout keyamp-current-layout)
      (user-error "Unable to activate standard keyboard to %s" keyamp-current-layout))
    (put 'toggle-standard-to-current-layout 'state t)
    (message "Activated standard keyboard to %s" keyamp-current-layout))
  (let ((define (get 'toggle-standard-to-current-layout 'state)))
    (mapc
     (lambda (pair)
       (keymap-set key-translation-map (car pair) (if define (cdr pair))))
     keyamp--convert-table)))


;; Macros

(defvar keyamp-layouts '(("qwerty" . nil))
  "A alist. Key is layout name, string type.
Value is an alist, each element is of the form (\"e\" . \"d\").
First char is QWERTY, second is corresponding char of the destination layout.
When a char is not in this alist, they are assumed to be the same.")

(defvar keyamp-current-layout "qwerty" "Current layout.")

(defconst keyamp-ascii-chars (number-sequence 33 126)
  "List of ASCII printable characters except space.")

(defun keyamp-layouts-quail-push (Layout)
  "Push keyboard LAYOUT to `keyamp-layouts' calculated from LAYOUT defined
in `quail-keyboard-layout-alist'."
  (unless (assoc Layout quail-keyboard-layout-alist)
    (user-error "Unable to push %s to keyamp-layouts" Layout))
  (when-let (((not (assoc Layout keyamp-layouts)))
             (layout quail-keyboard-layout-type))
    (quail-set-keyboard-layout Layout)
    (push (cons Layout nil) keyamp-layouts)
    (mapc
     (lambda (char)
       (push (cons (char-to-string (quail-keyboard-translate char))
                   (char-to-string char))
             (cdr (assoc Layout keyamp-layouts))))
     keyamp-ascii-chars)
    (quail-set-keyboard-layout layout)))

(keyamp-layouts-quail-push "engineer-engram")

(defvar keyamp--convert-table (cdr (assoc keyamp-current-layout keyamp-layouts))
  "A alist that's the conversion table from QWERTY to current layout.
Value structure is one of the key's value of `keyamp-layouts'.
Value is programmatically set from value of `keyamp-current-layout'.
Do not manually set this variable.")

(defun keyamp--convert-kbd-str (CharStr)
  "Return the corresponding char Charstr according to
`keyamp--convert-table'. Charstr must be a string that is, the argument
to `kbd'. E.g. \"a\" and \"a b c\". Each space separated token is
converted according to `keyamp--convert-table'."
  (mapconcat
   'identity
   (mapcar
    (lambda (char)
      (if-let (x (cdr (assoc char keyamp--convert-table))) x char))
    (split-string CharStr " +"))
   " "))

(defmacro keyamp--map (KeymapName KeyCmdAlist)
  "Map `keymap-set' over a alist KEYCMDALIST, with key layout remap."
  (declare (indent defun))
  `(progn
     ,@(mapcar
        (lambda (pair)
          `(keymap-set ,KeymapName
                       (keyamp--convert-kbd-str ,(car pair))
                       ,(list 'quote (cdr pair))))
        (cadr KeyCmdAlist))
     ,@(mapcan
        (lambda (pair)
          (mapcan
           (lambda (non-ascii)
             (when (> (string-to-char non-ascii) 127)
               `((keymap-set ,KeymapName ,non-ascii ,(list 'quote (cdr pair))))))
           (mapcar #'car
                   (seq-filter
                    (lambda (x)
                      (string-equal (cdr x) (car pair)))
                    keyamp-input-method-to-ascii))))
        (cadr KeyCmdAlist))))

(defun keyamp--map-ascii (KeymapName Cmd)
  "Map `keymap-set' over each ASCII char to CMD.
Map `keymap-set' over each corresponding non-ASCII input method char to CMD."
  (mapcar
   (lambda (char)
     (let ((charStr (char-to-string char)))
       (keymap-set KeymapName charStr Cmd)
       (mapc
        (lambda (non-ascii)
          (when (> (string-to-char non-ascii) 127)
            (keymap-set KeymapName non-ascii Cmd)))
        (mapcar #'car
                (seq-filter
                 (lambda (x)
                   (string-equal (cdr x) charStr))
                 keyamp-input-method-to-ascii)))))
   keyamp-ascii-chars))

(defmacro keyamp--remap (KeymapName CmdCmdAlist)
  "Map `keymap-set' remap over a alist CMDCMDALIST."
  (declare (indent defun))
  `(progn
     ,@(mapcar
        (lambda (pair)
          `(keymap-set ,KeymapName
                       ,(concat "<remap> <" (format "%s" (car pair)) ">")
                       ,(list 'quote (cdr pair))))
        (cadr CmdCmdAlist))))

(defun keyamp-unless-kbd-macro ()
  "Return t if not defining or executing kbd macro."
  (not (or defining-kbd-macro executing-kbd-macro)))

(defmacro keyamp--set (KeymapName CmdList &optional CommandMode InsertMode How Timeout)
  "Map `set-transient-map' using `advice-add' over a list CMDLIST.

Advice default HOW :after might be changed by specific HOW. Activate
COMMANDMODE or INSERTMODE mode optionally. Deactivate repeat mode
after idle for TIMEOUT seconds. Ignore the advice when defining or
executing kbd macro."
  (declare (indent defun))
  `(progn
     ,@(mapcar
        (lambda (cmd)
          `(advice-add ,(list 'quote cmd) (if ,How ,How :after)
                       (lambda (&rest _) "auto repeat"
            (when (and (keyamp-unless-kbd-macro)
                       (or (eq real-this-command 'repeat)
                           (eq this-command 'kill-region) ; exception
                           (eq this-command 'undo)        ; exception
                           (eq this-command ,(list 'quote cmd))))
              (if (and ,CommandMode
                       keyamp-insert-p)
                  (keyamp-command))
              (keyamp-repeat-init ,KeymapName)
              (keyamp-cancel-repeat-idle-timer)
              (if (and ,Timeout
                       (not keyamp-insert-p))
                  (setq keyamp--repeat-idle-timer
                        (run-with-idle-timer ,Timeout nil 'keyamp-escape)))
              (if ,InsertMode
                  (keyamp-insert))))))
        (cadr CmdList))))

(defmacro keyamp--hook (KeymapName HookList &optional CommandMode InsertMode RepeatMode)
  "Map `set-transient-map' using `add-hook' over a list HOOKLIST.
Activate command, insert or repeat mode optionally."
  (declare (indent defun))
  `(progn
     ,@(mapcar
        (lambda (hook)
          `(add-hook ,(list 'quote hook)
                     (lambda () "auto repeat"
                       (when this-command
                         (if (and ,CommandMode
                                  keyamp-insert-p)
                             (keyamp-command))
                         (if (and ,InsertMode
                                  (not keyamp-insert-p))
                             (keyamp-insert))
                         (keyamp-repeat-init ,KeymapName)
                         (if ,RepeatMode
                             (keyamp-command-execute 'keyamp--hook-indicate))))))
        (cadr HookList))))

(defun keyamp--hook-indicate ()
  "Hook indication."
  (interactive)
  t)

(defun keyamp-command-execute (Cmd)
  "Change this command to CMD and execute it. Indicate when not idle."
  (setq this-command Cmd)
  (command-execute Cmd)
  (if (or (null (current-idle-time))
          (< (time-convert (current-idle-time) 'integer) keyamp-idle-timeout))
      (keyamp-transient)))

(defmacro keyamp--map-leader (KeymapName CmdCons)
  "Map leader keys using `keyamp--map'."
  (declare (indent defun))
  `(if (display-graphic-p)
       (keyamp--map ,KeymapName
         '(("SPC" . ,(car (cadr CmdCons)))
           ("<backspace>" . ,(cdr (cadr CmdCons)))))
     (keyamp--map ,KeymapName
       '(("SPC" . ,(car (cadr CmdCons)))
         ("DEL" . ,(cdr (cadr CmdCons)))))))

(defmacro keyamp--map-tab (KeymapName Cmd)
  "Map TAB or <tab> keys to CMD using `keyamp--map'."
  (declare (indent defun))
  `(if (display-graphic-p)
       (keyamp--map ,KeymapName '(("<tab>" . ,Cmd)))
     (keyamp--map ,KeymapName '(("TAB" . ,Cmd)))))

(defmacro keyamp--map-backtab (KeymapName Cmd)
  "Map S-<tab> and <backtab> keys to CMD using `keyamp--map'."
  (declare (indent defun))
  `(if (display-graphic-p)
       (keyamp--map ,KeymapName '(("S-<tab>" . ,Cmd))))
  `(keyamp--map ,KeymapName '(("<backtab>" . ,Cmd))))

(defmacro keyamp--map-return (KeymapName Cmd)
  "Map RET or <return> keys to CMD using `keyamp--map'."
  (declare (indent defun))
  `(if (display-graphic-p)
       (keyamp--map ,KeymapName '(("<return>" . ,Cmd)))
     (keyamp--map ,KeymapName '(("RET" . ,Cmd)))))

(defmacro keyamp--map-escape (KeymapName Cmd)
  "Map <escape> key to CMD using `keyamp--map'."
  (declare (indent defun))
  `(keyamp--map ,KeymapName '(("<escape>" . ,Cmd))))

(defmacro with-sparse-keymap (&rest body)
  "Make sparse keymap for next use in BODY."
  (declare (indent defun))
  `(let ((keymap (make-sparse-keymap)))
     ,@body))

(defmacro advice-add-macro (CmdList How Fun)
  "Map `advice-add' HOW over a list CMDLIST to FUN."
  `(progn
     ,@(mapcar
        (lambda (cmd)
          `(advice-add ,(list 'quote cmd) ,How ,Fun))
        (cadr CmdList))))


;; Double press

(defconst keyamp-double-press-timeout (/ 300 1000.0) "Double key press timeout.")
(defvar keyamp-double-press-timer nil "Double key press timer.")

(defun keyamp-double-press (Cmd)
  "Execute COMMAND after second command call during `keyamp-double-press-timeout'."
  (if (and (timerp keyamp-double-press-timer)
           (eq this-command last-command)
           (keyamp-unless-kbd-macro))
      (keyamp-command-execute Cmd))
  (setq keyamp-double-press-timer
        (run-with-timer keyamp-double-press-timeout nil
                        (lambda () (setq keyamp-double-press-timer nil)))))

(defmacro keyamp--map-double (CmdCmdAlist)
  "Map over alist CMDCMDALIST double press of CAR CMDCONS to CDR CMDCONS."
  (declare (indent defun))
  `(progn
     ,@(mapcar
        (lambda (pair)
          `(advice-add ,(list 'quote (car pair)) :after
                       (lambda (&rest _) "double press"
                         (keyamp-double-press ,(list 'quote (cdr pair))))))
        (cadr CmdCmdAlist))))


;; Triple press (hold down)

(defvar keyamp-defer-command-timer nil "Defer command timer.")
(defconst keyamp-key-repeat-delay (/ (if (display-graphic-p) 30 90) 1000.0)
  "Key repeat delay. Higher value for network access.")

(defun keyamp-defer-command (Defer Cmd)
  "Delay execution of CMD for DEFER seconds."
  (if (keyamp-unless-kbd-macro)
      (setq keyamp-defer-command-timer
            (run-with-timer Defer nil 'keyamp-command-execute Cmd))))

(defun keyamp-cancel-defer-command-timer ()
  "Cancel `keyamp-defer-command-timer'."
  (when (and (timerp keyamp-defer-command-timer)
             (keyamp-unless-kbd-macro))
    (cancel-timer keyamp-defer-command-timer)
    (setq keyamp-defer-command-timer nil)))

(defun keyamp-defer-command-around (fun &rest _)
  "Run `keyamp-defer-command' as around advice."
  (if (or defining-kbd-macro executing-kbd-macro)
      (keyamp-command-execute fun)
    (if (memq last-command triple-press-direction-commands-list)
        (before-last-command))
    (keyamp-defer-command keyamp-key-repeat-delay fun)))


;; Terminal ESC to <escape>

(defconst keyamp-tty-seq-timeout (/ 30 1000.0)
  "Timeout to wait key sequence after ESC sent in tty.")

(defun keyamp-tty-ESC-filter (map)
  "Map last ESC key from this single command keys to <escape>.
Prefix sequence may contain last key ESC."
  (if-let ((tty-seq (this-single-command-keys))
           ((= ?\e (aref tty-seq (1- (length tty-seq)))))
           ((or (or defining-kbd-macro executing-kbd-macro)
                (sit-for keyamp-tty-seq-timeout))))
      [escape] map))

(defun keyamp-lookup-key (map key)
  (catch 'found
    (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

(defun keyamp-catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into <escape>."
  (when-let (((memq (terminal-live-p (frame-terminal)) '(t pc)))
             (esc-binding (keyamp-lookup-key input-decode-map ?\e))
             (esc `(menu-item "" ,esc-binding :filter keyamp-tty-ESC-filter)))
    (keymap-set input-decode-map "ESC" esc)
    (keymap-set key-translation-map "ESC" "<escape>")))


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

(keyamp--map-command-input-method)

(define-prefix-command 'keyamp-lleader-map)
(define-prefix-command 'keyamp-rleader-map)

(keyamp--map-escape keyamp-map keyamp-escape)
(keyamp--map keyamp-map
  '(;; Control sequences for leaders used as key hold down prefixes in Karabiner.
    ("C-^" . keyamp-lleader-map) ("C-_" . keyamp-rleader-map)
    ("C-+" . keyamp-lleader-map) ("C-И" . keyamp-rleader-map)))

(keyamp--map-leader keyamp-command-map '(keyamp-lleader-map . keyamp-rleader-map))
(keyamp--map-return keyamp-command-map keyamp-insert)
(keyamp--map keyamp-command-map
  '(;; left half
    ("`" . make-frame-command)   ("~" . keyamp-insert-and-self-insert)
    ("1" . kmacro-record)        ("!" . keyamp-insert-and-self-insert)
    ("2" . kmacro-play)          ("@" . keyamp-insert-and-self-insert)
    ("3" . kmacro-helper)        ("#" . keyamp-insert-and-self-insert)
    ("4" . append-to-r1)         ("$" . keyamp-insert-and-self-insert)
    ("5" . terminal)             ("%" . keyamp-insert-and-self-insert)

    ("q" . insert-space-before)  ("Q" . keyamp-insert-and-self-insert)
    ("w" . backward-del-word)    ("W" . keyamp-insert-and-self-insert)
    ("e" . undo)                 ("E" . keyamp-insert-and-self-insert)
    ("r" . del-word)             ("R" . keyamp-insert-and-self-insert)
    ("t" . cut-text-block)       ("T" . keyamp-insert-and-self-insert)

    ("a" . shrink-whitespaces)   ("A" . keyamp-insert-and-self-insert)
    ("s" . open-line)            ("S" . keyamp-insert-and-self-insert)
    ("d" . del-back)             ("D" . keyamp-insert-and-self-insert)
    ("f" . newline)              ("F" . keyamp-insert-and-self-insert)
    ("g" . activate-region)      ("G" . keyamp-insert-and-self-insert)

    ("z" . toggle-comment)       ("Z" . keyamp-insert-and-self-insert)
    ("x" . cut-line)             ("X" . keyamp-insert-and-self-insert)
    ("c" . copy-line)            ("C" . keyamp-insert-and-self-insert)
    ("v" . paste-or-prev)        ("V" . keyamp-insert-and-self-insert)
    ("b" . toggle-case)          ("B" . keyamp-insert-and-self-insert)

    ;; right half
    ("6" . pass)                 ("^" . keyamp-insert-and-self-insert)
    ("7" . jump-to-register)     ("&" . keyamp-insert-and-self-insert)
    ("8" . point-to-register)    ("*" . goto-match-br) ; QWERTY * -> = Engram, QWERTY / -> = RU PC Karabiner
    ("9" . proced-defer)         ("(" . keyamp-insert-and-self-insert)
    ("0" . sh-defer)             (")" . keyamp-insert-and-self-insert)
    ("-" . enlarge-window)       ("_" . keyamp-insert-and-self-insert)
    ("=" . goto-match-br)        ("+" . keyamp-insert-and-self-insert)

    ("y"  . occur-cur-word)      ("Y" . keyamp-insert-and-self-insert)
    ("u"  . back-word)           ("U" . keyamp-insert-and-self-insert)
    ("i"  . previous-line)       ("I" . keyamp-insert-and-self-insert)
    ("o"  . forw-word)           ("O" . keyamp-insert-and-self-insert)
    ("p"  . jump-mark)           ("P" . keyamp-insert-and-self-insert)
    ("["  . scratch)             ("{" . keyamp-insert-and-self-insert)
    ("]"  . list-timers)         ("}" . keyamp-insert-and-self-insert)
    ("\\" . bookmark-set)        ("|" . keyamp-insert-and-self-insert)

    ("h" . beg-of-line)          ("H"  . keyamp-insert-and-self-insert)
    ("j" . bchar)                ("J"  . keyamp-insert-and-self-insert)
    ("k" . next-line)            ("K"  . keyamp-insert-and-self-insert)
    ("l" . fchar)                ("L"  . keyamp-insert-and-self-insert)
    (";" . end-of-lyne)          (":"  . keyamp-insert-and-self-insert)
    ("'" . alt-buf)              ("\"" . keyamp-insert-and-self-insert)

    ("n" . isearch-forward)      ("N" . keyamp-insert-and-self-insert)
    ("m" . backward-bracket)     ("M" . keyamp-insert-and-self-insert)
    ("," . other-win)            ("<" . keyamp-insert-and-self-insert)
    ("." . forward-bracket)      (">" . keyamp-insert-and-self-insert)
    ("/" . goto-match-br)        ("?" . keyamp-insert-and-self-insert)

    ("<left>"  . back-char)
    ("<right>" . forw-char)
    ("<up>"    . up-line)
    ("<down>"  . down-line)))

(keyamp--map-leader keyamp-lleader-map '(select-word . select-quote))
(keyamp--map-return keyamp-lleader-map execute-extended-command)
(keyamp--map keyamp-lleader-map '(("C-t" . display-line-numbers-mode)))
(keyamp--map-escape keyamp-lleader-map ignore)
(keyamp--map-tab keyamp-lleader-map read-only-mode)
(keyamp--map-backtab keyamp-lleader-map volume-decrease)
(keyamp--map keyamp-lleader-map
  '(;; left leader left half
    ("`" . revert-buffer)
    ("1" . periodic-chart)
    ("2" . kmacro-play-toggle)
    ("3" . delete-window)
    ("4" . clear-r1)
    ("5" . repeat-command)

    ("q" . reformat-lines)
    ("w" . org-ctrl-c-ctrl-c)
    ("e" . split-window-below)
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
    ("i DEL" . count-matches)              ("i SPC" . count-words)
    ("i <escape>" . ignore)                ("i RET" . show-in-desktop)

    ("o"  . flymake-goto-next-error)
    ("p"  . show-kill-ring)
    ("["  . toggle-frame-maximized)
    ("]"  . gptel-menu)
    ("\\" . yt-dlp)

    ("h"  . prog-new)

                                           ("j i"   . widen)
                                           ("j l"   . narrow-to-region-or-block)
                                           ("j k"   . narrow-to-defun)
                                           ("j j"   . diff-buffers)
    ("j DEL" . whitespace-mode)            ("j SPC" . hl-line-mode)
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
    ("k DEL" . flyspell-buffer)            ("k SPC" . ispell-word)
    ("k <escape>" . ignore)                ("k RET" . list-matching-lines)

    ("l" . screen-idle)
    (";" . recentf-open-files)
    ("'" . alternate-frame)
    ("n" . list-processes)
    ("m" . tt-conn)
    ("," . ai)
    ("." . open-last-closed)
    ("/" . goto-line)))

(if (display-graphic-p)
    (keyamp--map keyamp-lleader-map
      '(("i DEL" . nil)        ("i <backspace>" . count-matches)
        ("i RET" . nil)        ("i <return>"    . show-in-desktop)
        ("j DEL" . nil)        ("j <backspace>" . whitespace-mode)
        ("j RET" . nil)        ("j <return>"    . toggle-word-wrap)
        ("k DEL" . nil)        ("k <backspace>" . flyspell-buffer)
        ("k RET" . nil)        ("k <return>"    . find-file)
        ("<mouse-1>" . ignore)
        ("<mouse-2>" . ignore)
        ("<mouse-3>" . ignore)
        ("<down-mouse-1>" . mouse-drag-region-rectangle))))

(keyamp--map-leader keyamp-rleader-map '(select-line . select-block))
(keyamp--map-return keyamp-rleader-map open-file)
(keyamp--map keyamp-rleader-map '(("C-t" . toggle-truncate-lines)))
(keyamp--map-escape keyamp-rleader-map ignore)
(keyamp--map-backtab keyamp-rleader-map speedbar)
(keyamp--map-tab keyamp-rleader-map next-proj-buf)
(keyamp--map keyamp-rleader-map
  '(;; right leader left half
    ("`" . toggle-input-method)            ("~" . toggle-standard-to-current-layout)
    ("1" . view-lossage)
    ("2" . insert-kbd-macro)
    ("3" . config)
    ("4" . change-bracket-pairs)
    ("5" . json-pretty)

    ("q" . fill-or-unfill)
    ("w" . sun-moon)

    ("e e"   . todo)                       ("e k"   . weather)
    ("e SPC" . clock)                      ("e DEL" . calendar)
    ("e <escape>" . ignore)                ("e RET" . insert-date)

    ("r" . query-replace-regexp)
    ("t" . calc)
    ("a" . mark-whole-buffer)
    ("s" . clean-whitespace)

    ("D"     . repeat)                     ("В"     . repeat)
    ("d e"   . org-shiftup)                ("d i"   . async-shell-command)
    ("d d"   . eval-region-or-sexp)        ("d k"   . run-current-file)
    ("d SPC" . stow)                       ("d DEL" . eval-defun-visual)
    ("d <escape>" . ignore)                ("d RET" . shell-command)

    ("f e"   . insert-emacs-quote)         ("f i"   . insert-ascii-single-quote)
    ("f f"   . copy-char)                  ("f j"   . insert-brace)
    ("f k"   . insert-paren)
    ("f s"   . insert-formfeed)            ("f l"   . insert-square-bracket)
    ("f g"   . insert-double-angle-quote)  ("f h"   . insert-double-curly-quote)
    ("f DEL" . insert-ascii-double-quote)  ("f SPC" . insert-backtick-quote)
    ("f <escape>" . ignore)                ("f RET" . emoji-insert)

    ("g" . tools)
    ("z" . goto-char)
    ("x" . next-eww-buf)
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
    ("=" . ignore)

    ("y"  . toggle-case-fold-search)
    ("u"  . backward-punct)
    ("i"  . beg-of-block-rev)
    ("o"  . forward-punct)
    ("p"  . mark-defun)
    ("["  . backward-sexp)
    ("]"  . forward-sexp)
    ("\\" . dslide-deck-start)

    ("h" . page-up-half)
    ("j" . isearch-wback)
    ("k" . end-of-block)
    ("l" . isearch-wforw)
    (";" . page-dn-half)
    ("'" . describe-mode)

    ("n" . help-command)
    ("m" . dired-jump)
    ("," . delete-other-windows)
    ("." . save-close-buf)
    ("/" . view-messages)
    ("*" . view-messages)))

(if (display-graphic-p)
    (keyamp--map keyamp-rleader-map
      '(("e DEL" . nil)        ("e <backspace>" . calendar)
        ("e RET" . nil)        ("e <return>"    . insert-date)
        ("d DEL" . nil)        ("d <backspace>" . eval-defun-visual)
        ("d RET" . nil)        ("d <return>"    . shell-command)
        ("f DEL" . nil)        ("f <backspace>" . insert-ascii-double-quote)
        ("f RET" . nil)        ("f <return>"    . emoji-insert)
        ("<mouse-1>" . ignore)
        ("<mouse-2>" . ignore)
        ("<mouse-3>" . ignore))))

(keyamp--map-double
  '((keyamp-escape  . alternate-frame) (other-win     . jump-buffer-or-bookmark)
    (beg-of-line    . beg-of-buf)      (end-of-lyne   . end-of-buf)
    (proced-defer   . save-close-buf)  (sh-defer      . delete-other-windows)
    (occur-cur-word . search-string)   (kmacro-record . keyamp-delete)))


;; Remaps

(when (display-graphic-p)
  (keymap-set global-map          "<tab>" 'indent-for-tab-command) ; /lisp/indent.el.gz:816
  (keymap-set key-translation-map "S-SPC"         "<tab>")
  (keymap-set key-translation-map "S-<backspace>" "<backtab>")
  (keymap-set key-translation-map "S-<return>"    "C-t")
  (keymap-set key-translation-map "S-<escape>"    "C-q"))

(keymap-set global-map "C-o"    'touchid)
(keymap-set global-map "<home>" 'beg-of-buf)
(keymap-set global-map "<end>"  'end-of-buf)

(setq help-map (make-sparse-keymap))
(fset 'help-command help-map)

(keyamp--map-leader help-map '(translate . lookup-word-definition))
(keyamp--map-escape help-map ignore)
(keyamp--map-return help-map lookup-web)
(keyamp--map-tab help-map lookup-wikipedia)
(keyamp--map-backtab help-map lookup-etymology)
(keyamp--map help-map
  '(("e" . describe-char)      ("i" . info)
    ("s" . info-lookup-symbol) ("j" . describe-mode)
    ("d" . man)                ("k" . describe-key)
    ("f" . elisp-index-search) ("l" . describe-variable)
    ("q" . describe-syntax)    ("p" . apropos-documentation)
    ("w" . describe-bindings)  ("o" . lookup-all-dictionaries)
    ("r" . ignore)             ("u" . lookup-all-synonyms)
    ("a" . describe-face)      (";" . lookup-wiktionary)
    ("g" . apropos-command)    ("h" . describe-coding-system)
    ("n" . describe-function)))

(dotimes (n 10) ; help command + number to send corresponding fn key
  (keymap-set help-map (format "%d" (% n 10))
              `(lambda ()
                 (interactive)
                 (execute-kbd-macro (kbd ,(format "<f%d>" n))))))

(keyamp--map global-map '(("C-t" . hippie-expand)))
(keyamp--remap global-map '((quoted-insert . quoted-insert-custom)))

;; Pass single key through the net
(keyamp--map global-map '(("<f10>" . exec-query) ("<f12>" . keyamp-escape)))

;; Useful in Termius add-on
(keymap-set key-translation-map "<deletechar>" "DEL")
(keymap-set key-translation-map "<insertchar>" "SPC")

(when (display-graphic-p)
  (keyamp--map global-map
    '(("<prior>"          . page-up-half) ("<next>"    . page-dn-half)
      ("<double-mouse-1>" . select-word)  ("<mouse-3>" . mouse-3)))
  (advice-add 'mouse-set-point   :around #'lookup-around)
  (advice-add 'mouse-set-point   :before #'scroll-one-pixel)
  (advice-add 'mouse-set-point   :after  #'keyamp-command-if-insert)
  (advice-add 'mouse-drag-region :before #'copy-selection))

(advice-add 'keyamp-insert :before #'delete-before)
(advice-add 'keyamp-insert :around #'lookup-around)
(advice-add 'keyamp-insert :around #'translate-around)

(with-sparse-keymap
  ;; Repeat using DEL/SPC or D. The concept widely used to form Repeat mode.
  (keyamp--map-leader keymap '(del-back . del-back))
  (keyamp--remap keymap '((del-back . repeat)))
  (keyamp--set keymap '(repeat)))

(with-sparse-keymap
  ;; S-RET to call `hippie-expand'. Press RET to insert a possible expansion.
  ;; SPC to confirm, DEL to reset.
  (keyamp--map-leader keymap '(insert-space-before . hippie-expand-reset))
  (keyamp--map-return keymap hippie-expand)
  (keyamp--set keymap '(hippie-expand)))

(with-sparse-keymap ; next DEL to exit or RET to start over
  (keyamp--map-leader keymap '(insert-space-before . delete-backward-char))
  (keyamp--map-return keymap hippie-expand)
  (keyamp--set keymap '(hippie-expand-reset)))


;; I-search

(with-sparse-keymap
  ;; After starting up an isearch press DEL to retreat to the previous
  ;; search string. Press SPC to pull string from kill ring into search string.
  (keyamp--map-leader keymap '(isearch-yank-kill . isearch-ring-retreat))
  (keyamp--map-return keymap isearch-direction-switch)
  (keyamp--map keymap '(("C-t" . isearch-forward-regexp) ("n" . save-buffer-isearch-cancel)))
  (keyamp--map-tab keymap isearch-double-back) ; repeat backward
  (keyamp--hook keymap '(isearch-mode-hook) nil nil :repeat))

;; Hit TAB to repeat after typing in search string and set following transient
;; map. Backtab to repeat backward. S-DEL/S-SPC for Backtab/TAB.
(keyamp--map-leader isearch-mode-map '(isearch-printing-char . isearch-del-char))
(keyamp--map-escape isearch-mode-map isearch-cancel)
(keyamp--map-tab isearch-mode-map isearch-forw)
(keyamp--map-backtab isearch-mode-map isearch-back)
(keyamp--map isearch-mode-map '(("C-^" . keyamp-lleader-map) ("C-t" . isearch-complete)))
(keyamp--remap isearch-mode-map '((paste-from-r1 . isearch-yank-r1)))

(with-sparse-keymap
  ;; Find the occurrence of the current search string with J/L or DEL/SPC.
  ;; Press I/K or DEL/SPC to get search strings from the ring.
  ;; S-SPC to find the occurrence of the last search string.
  (keyamp--map-leader keymap '(isearch-forw . isearch-back))
  (keyamp--map-return keymap isearch-exit)
  (keyamp--map keymap
    '(("i" . isearch-ring-retreat) ("j" . isearch-back)
      ("k" . isearch-ring-advance) ("l" . isearch-forw)))
  (keyamp--set keymap
    '(isearch-ring-retreat     isearch-ring-advance
      isearch-back             isearch-forw
      isearch-wforw            isearch-wback
      isearch-yank-kill        isearch-double-back))

 (defun isearch-mode-exit-minibuffer ()
   "Setup isearch transient after choice from the ring and exit minibuffer."
   (when (eq real-this-command 'exit-minibuffer)
     (keyamp-repeat-deactivate-init keymap)
     (setq this-command 'isearch-forw)))

 (add-hook 'isearch-mode-hook 'isearch-mode-exit-minibuffer 96))

(with-sparse-keymap
  (keyamp--remap keymap '((occur-cur-word . isearch-occur)))
  (keyamp--set keymap '(isearch-exit isearch-cancel)))

(with-sparse-keymap
  ;; Press I/K or DEL/SPC to get search strings from the ring
  ;; then S-DEL/S-SPC to find the occurrence of the search string.
  (keyamp--map-leader keymap '(hist-forw . hist-back))
  (keyamp--map-escape keymap isearch-cancel-clean-are)
  (keyamp--map-tab keymap exit-minibuffer)
  (keyamp--map-backtab keymap exit-minibuffer)

 (defun isearch-mode-setup-minibuffer ()
   "Setup isearch transient in minibuffer before choice from the ring."
   (if (isearch-minibuffer-prompt)
       (keyamp-repeat-deactivate-init keymap)))

 (add-hook 'minibuffer-setup-hook 'isearch-mode-setup-minibuffer 96)
 (advice-add-macro '(hist-back hist-forw) :after 'isearch-mode-setup-minibuffer))



(defun keyamp-tab ()
  "Tab key command for transient use."
  (interactive)
  (cond
   ((eq major-mode 'org-agenda-mode)
    (keyamp-command-execute 'todo))
   ((eq major-mode 'ibuffer-mode)
    (keyamp-command-execute 'ibuffer-select-group))
   ((eq major-mode 'gnus-group-mode)
    (keyamp-command-execute 'gnus-topic-select-group))
   ((eq last-command 'up-line)
    (keyamp-command-execute 'split-window-below)) ; below down-line default
   (t (keyamp-command-execute 'page-dn-half))))

(defun keyamp-ret ()
  "Return key command for transient use."
  (interactive)
  (let ((cmd (keymap-lookup overriding-local-map
                            (if (display-graphic-p) "<return>" "RET"))))
    (keyamp-command-execute ; double remap
     (if (or (equal cmd 'keyamp-insert)
             (null cmd))
         'keyamp-escape
       cmd))))

(defun keyamp-delete ()
  "Keyamp delete command."
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (keyamp-command-execute 'dired-do-delete))
   ((eq major-mode 'eshell-mode)
    (keyamp-command-execute 'eshell-interrupt-process))
   ((eq major-mode 'vterm-mode)
    (keyamp-command-execute 'term-interrupt-subjob))
   (t (keyamp-command-execute 'ignore))))


;; Repeat mode - screen commands

(defvar keyamp-delay-1 1 "Delay 1 second.")
(defvar keyamp-delay-2 2 "Delay 2 seconds.")
(defvar keyamp-delay-3 3 "Delay 3 seconds.")

(with-sparse-keymap
  ;; Leader layer to become transient main. Base map for next leaders adjustment
  ;; by transient maps which set by following target commands subsets.
  (keyamp--map-leader keymap '(newline . open-line))
  (keyamp--map-return keymap keyamp-escape)
  (keyamp--map-backtab keymap page-up-half)
  (keyamp--map-tab keymap keyamp-tab)
  (keyamp--remap keymap
    '((make-frame-command  . delete-frame)
      (insert-space-before . ignore)
      (backward-del-word   . ignore)
      (undo                . ignore)
      (del-word            . ignore)
      (cut-text-block      . ignore)
      (goto-match-br       . ignore)
      (shrink-whitespaces  . ignore)
      (del-back            . switch-to-buffer)
      (toggle-comment      . ignore)
      (cut-line            . ignore)
      (copy-line           . ignore)
      (paste-or-prev       . tasks)
      (backward-bracket    . dired-jump)
      (forward-bracket     . save-close-buf)
      (kmacro-helper       . config)
      (up-line             . view-messages)
      (down-line           . screen-idle)
      (back-char           . next-buf)
      (forw-char           . prev-buf)))

  (keyamp--set keymap
    '(prev-buf                   next-buf
      save-close-buf             tools
      prev-proj-buf              next-proj-buf
      prev-eww-buf               next-eww-buf
      prev-eshell-buf            next-eshell-buf
      prev-dired-buf             next-dired-buf
      tasks                      config
      previous-buffer            next-buffer
      find-prev-dir-file         find-next-dir-file
      shrink-window              enlarge-window
      shrink-window-horizontally enlarge-window-horizontally
      volume-decrease            volume-increase
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
      sync                    calendar-split
      isearch-occur           exec-query-async)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . prev-buf) (newline . next-buf)))
  (keyamp--set keymap
    '(prev-buf next-buf delete-other-windows delete-window
      split-window-horizontally)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . prev-proj-buf) (newline . next-proj-buf)))
  (keyamp--set keymap '(prev-proj-buf next-proj-buf)))

(with-sparse-keymap
  (keyamp--map-tab keymap keyamp-tab)
  (keyamp--map-backtab keymap page-up-half)
  (keyamp--remap keymap
    '((open-line . prev-eww-buf) (newline . next-eww-buf)
      (del-back  . eww-reload)   (undo    . justify-buffer)))
  (keyamp--set keymap '(prev-eww-buf next-eww-buf)))

(with-sparse-keymap
  (keyamp--map-tab keymap keyamp-tab)
  (keyamp--map-backtab keymap page-up-half)
  (keyamp--remap keymap '((open-line . prev-eshell-buf) (newline . next-eshell-buf)))
  (keyamp--set keymap '(prev-eshell-buf next-eshell-buf)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . prev-dired-buf) (newline . next-dired-buf)))
  (keyamp--set keymap '(prev-dired-buf next-dired-buf)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . tasks) (newline . next-buf)))
  (keyamp--set keymap '(tasks org-agenda-tasks)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . prev-buf) (newline . alt-buf)))
  (keyamp--set keymap '(tools)))

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
      (forward-bracket . save-close-buf) (keyamp-insert . keyamp-escape)))
  (keyamp--set keymap '(save-close-buf)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . shrink-window) (newline . enlarge-window)))
  (keyamp--set keymap '(shrink-window enlarge-window) nil nil nil keyamp-delay-2))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . volume-decrease) (newline . volume-increase)))
  (keyamp--set keymap '(volume-increase volume-decrease) nil nil nil keyamp-delay-2))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . shrink-window-horizontally) (newline . enlarge-window-horizontally)))
  (keyamp--set keymap '(enlarge-window-horizontally shrink-window-horizontally)
    nil nil nil keyamp-delay-2))

(with-sparse-keymap
  (keyamp--remap keymap '((make-frame-command . delete-frame)))
  (keyamp--set keymap '(scratch)))


;; Repeat mode - read commands

(with-sparse-keymap ; hold down RET to ESC, fast second RET press for double ESC
  (keyamp--map-return keymap keyamp-escape)
  (keyamp--set keymap '(keyamp-escape) nil nil nil keyamp-blink-idle-duration))

(with-sparse-keymap
  ;; Initiate by triple DEL/SPC (hold down).
  ;; I/K or DEL/SPC to move by lines. See `return-before'.
  (keyamp--map-leader keymap '(down-line . up-line))
  (keyamp--map-return keymap keyamp-ret)
  (keyamp--map-tab keymap keyamp-tab)
  (keyamp--map-backtab keymap hscroll-left)
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
  (keyamp--map-leader keymap '(up-line-rev . down-line-rev))
  (keyamp--map-return keymap keyamp-ret)
  (keyamp--remap keymap '((previous-line . up-line) (next-line . down-line)))
  (keyamp--set keymap '(up-line-rev down-line-rev)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(down-line . up-line))
  (keyamp--map-return keymap keyamp-ret)
  (keyamp--remap keymap '((previous-line . beg-of-block) (next-line . end-of-block)))
  (keyamp--set keymap '(beg-of-buf end-of-buf)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(end-of-block . beg-of-block))
  (keyamp--map-return keymap keyamp-ret)
  (keyamp--remap keymap '((previous-line . beg-of-block-rev) (next-line . end-of-block)))
  (keyamp--set keymap '(beg-of-block end-of-block)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(beg-of-block-rev . end-of-block-rev))
  (keyamp--map-return keymap keyamp-ret)
  (keyamp--remap keymap '((previous-line . beg-of-block) (next-line . end-of-block)))
  (keyamp--set keymap '(beg-of-block-rev end-of-block-rev)))

;; In case triple DEL received during `keyamp-key-repeat-delay',
;; `select-block' would be ignored. Must call before following transient maps.
;; Same for triple SPC and `select-word'.
(advice-add-macro '(select-word select-block) :around 'keyamp-defer-command-around)
(advice-add-macro '(up-line down-line) :before 'keyamp-cancel-defer-command-timer)

(with-sparse-keymap
  (keyamp--map-leader keymap '(down-line . keyamp-escape))
  (keyamp--remap keymap '((keyamp-escape . return-before)))
  (keyamp--set keymap '(select-word)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(keyamp-escape . up-line))
  (keyamp--remap keymap
    '((previous-line . beg-of-block)  (next-line     . select-word)
      (keyamp-escape . return-before) (hippie-expand . exec-query)))
  (keyamp--set keymap '(select-block)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(copy-line . copy-line))
  (keyamp--map-escape keymap return-before)
  (keyamp--set keymap '(select-quote)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(copy-line . copy-line))
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

(with-sparse-keymap ; left/right arrows repeat by DEL/SPC
  (keyamp--map-leader keymap '(forw-char . back-char))
  (keyamp--set keymap '(back-char forw-char)))

;; touch screen
(with-sparse-keymap
  (keyamp--map-tab keymap next-buf)
  (keyamp--set keymap '(forw-char)))

(with-sparse-keymap
  (keyamp--map-tab keymap other-win)
  (keyamp--set keymap '(back-char)))

(with-sparse-keymap
  (keyamp--remap keymap '((bchar . back-word) (fchar . forw-word)))
  (keyamp--set keymap '(back-word forw-word)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(fchar . bchar))
  (keyamp--remap keymap '((bchar . back-word-repeat) (fchar . forw-word-repeat)))
  (keyamp--set keymap '(back-word-repeat forw-word-repeat)))

(with-sparse-keymap
  (keyamp--remap keymap '((back-word . backward-punct) (forw-word . forward-punct)))
  (keyamp--set keymap '(backward-punct forward-punct)))

(with-sparse-keymap
  (keyamp--remap keymap '((previous-line . backward-sexp) (next-line . forward-sexp)))
  (keyamp--set keymap '(backward-sexp forward-sexp)))

;; U and O act as leader keys.
(defvar keyamp--deactivate-leader-fun nil "Virtual leader deactivate function.")
(defvar keyamp-virtual-leader-timer nil "Virtual leader deactivate timer.")

(defun keyamp--leader-deactivate ()
  "Deactivate virtual leader."
  (if keyamp--deactivate-leader-fun
      (funcall keyamp--deactivate-leader-fun))
  (setq keyamp-virtual-leader-timer nil))

(defun keyamp-ascii-to-input-method-vector (char)
  "Calculate list of vectors of corresponding input methods chars from qwerty
ascii CHAR."
  (mapcar
   (lambda (map)
     (vector (string-to-char map)))
   (mapcar #'car
           (seq-filter
            (lambda (x)
              (string-equal (cdr x) char))
            keyamp-input-method-to-ascii))))

(defun keyamp-virtual-leader-init (Keymap)
  "Set virtual leader transient KEYMAP."
  (when-let (((keyamp-unless-kbd-macro))
             ((member (this-command-keys)
                      `(,(keyamp--convert-kbd-str "u")
                        ,@(keyamp-ascii-to-input-method-vector "u")
                        ,(keyamp--convert-kbd-str "o")
                        ,@(keyamp-ascii-to-input-method-vector "o")))))
    (setq keyamp--deactivate-leader-fun (set-transient-map Keymap))
    (if (timerp keyamp-virtual-leader-timer)
        (cancel-timer keyamp-virtual-leader-timer))
    (setq keyamp-virtual-leader-timer
          (run-with-timer keyamp-double-press-timeout
                          nil 'keyamp--leader-deactivate))))

(define-prefix-command 'keyamp-lleader-i-map)

(with-sparse-keymap
  (keyamp--map keymap '(("i" . keyamp-lleader-i-map)))
  (keyamp--map keyamp-lleader-i-map '(("i" . backup-and-copy)))
  (keyamp--remap keymap '((back-word . select-word) (forw-word . select-quote)))
  (advice-add 'back-word :after
              (lambda () "virtual leader" (keyamp-virtual-leader-init keymap))))

(with-sparse-keymap
  (keyamp--map-leader keymap '(newline . open-line))
  (keyamp--remap keymap '((open-line . downloads) (newline . tools)))
  (keyamp--remap keymap
    '((back-word     . select-line)   (forw-word        . select-block)
      (previous-line . beg-of-block)  (next-line        . end-of-block)
      (bchar . isearch-wback)         (backward-bracket . dired-jump)))
  (advice-add 'forw-word :after
              (lambda () "virtual leader" (keyamp-virtual-leader-init keymap))))

(defun keyamp-virtual-leader-return-before (&rest _)
  "Return before, that is, compensate word move."
  (if (timerp keyamp-virtual-leader-timer)
      (set-mark-command t)))

(advice-add-macro '(select-word      select-quote
                    select-line      select-block
                    backup-and-copy  isearch-wback
                    prev-buf         tools)
                  :before 'keyamp-virtual-leader-return-before)

;; G acts as leader key.
(with-sparse-keymap
  (keyamp--map-leader keymap '(newline . tools))
  (keyamp--map-escape keymap deactivate-region)
  (keyamp--map-return keymap toggle-ibuffer)
  (keyamp--remap keymap
    '((undo               . todo)
      (shrink-whitespaces . player)
      (open-line          . alt-buf)
      (del-back           . del-forw)
      (newline            . toggle-pin-window)
      (activate-region    . rectangle)
      (toggle-case        . downloads)
      (alt-buf            . jump-6)
      (other-win          . jump-7)
      (isearch-forward    . jump-8)))

  (advice-add 'activate-region :after
              (lambda () "virtual leader G transient"
                (if (and (keyamp-unless-kbd-macro)
                         (eq (mark) (point)))
                    (set-transient-map keymap)))))

(defun keyamp-deactivate-region (&rest _)
  "Deactivate region if mark equal point."
  (if (eq (mark) (point))
      (deactivate-region)))

(advice-add-macro
 '(del-forw              tools
   jump-6
   jump-7                jump-8
   screen-idle           downloads
   toggle-pin-window     toggle-ibuffer
   player                alt-buf)
 :before 'keyamp-deactivate-region)

(with-sparse-keymap
  ;; Repeat half page up/down with I/K or DEL/SPC.
  (keyamp--map-leader keymap '(next-line . previous-line))
  (keyamp--map-return keymap keyamp-ret)
  (keyamp--map-tab keymap scroll-up-command)
  (keyamp--map-backtab keymap scroll-down-command)
  (keyamp--remap keymap
    '((previous-line . page-up-half) (next-line . page-dn-half)
      (down-line     . page-dn-half) (up-line   . page-up-half)))
  (unless (display-graphic-p) ; touch reader
    (keyamp--remap keymap '((down-line . page-up-half) (up-line . page-dn-half))))
  (keyamp--set keymap '(page-up-half page-dn-half)))

(with-sparse-keymap
  ;; Initially TAB makes half page forward, following presses do full page.
  ;; Arrows always do half page and keep TAB transient, see previous keymap.
  (keyamp--map-leader keymap '(next-line . previous-line))
  (keyamp--map-return keymap keyamp-ret)
  (keyamp--map-tab keymap next-line)
  (keyamp--map-backtab keymap previous-line)
  (keyamp--remap keymap
    '((previous-line . scroll-down-command) (next-line . scroll-up-command)
      (down-line     . page-dn-half) (up-line   . page-up-half)))
  (unless (display-graphic-p) ; touch reader
    (keyamp--remap keymap '((down-line . page-up-half) (up-line . page-dn-half))))
  (keyamp--set keymap '(scroll-down-command scroll-up-command)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(fchar . bchar))
  (keyamp--remap keymap '((bchar . hscroll-right) (fchar . hscroll-left)))
  (keyamp--set keymap '(hscroll-left hscroll-right)))

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
  (keyamp--map-leader keymap '(down-line . up-line))
  (keyamp--remap keymap '((undo . button-back) (del-back . button-forw)))
  (keyamp--set keymap '(button-back button-forw)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(next-line . next-line))
  (keyamp--remap keymap '((next-line . recenter-top-bottom)))
  (keyamp--set keymap '(recenter-top-bottom)) nil nil nil keyamp-delay-2)

(with-sparse-keymap
  (keyamp--map-return keymap toggle-truncate-lines)
  (keyamp--set keymap '(toggle-truncate-lines)))


;; Repeat mode - modify commands

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . delete-forward-char)))
  (keyamp--set keymap '(delete-forward-char) nil nil nil keyamp-delay-2))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . insert-space-before)))
  (keyamp--set keymap '(insert-space-before) nil nil nil keyamp-delay-2))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . del-forw)))
  (keyamp--set keymap '(del-forw) nil nil nil keyamp-delay-2))

(with-sparse-keymap
  (keyamp--remap keymap '((split-window-below . undo-redo)))
  (keyamp--set keymap '(undo undo-redo)))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . cut-text-block)))
  (keyamp--set keymap '(cut-text-block)))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . shrink-whitespaces)))
  (keyamp--set keymap '(shrink-whitespaces) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . toggle-comment)))
  (keyamp--set keymap '(toggle-comment) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . cut-line)))
  (keyamp--set keymap '(cut-line) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . copy-line)))
  (keyamp--set keymap '(copy-line)))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . toggle-case)))
  (keyamp--set keymap '(toggle-case) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--remap keymap '((undo . org-shiftup) (del-back . org-shiftdown)))
  (keyamp--set keymap '(org-shiftup org-shiftdown) nil nil nil keyamp-delay-3))

(with-sparse-keymap
  (keyamp--remap keymap '((undo . todo)))
  (keyamp--set keymap '(todo insert-date) nil nil nil keyamp-delay-1))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . cycle-hyphen-lowline-space)))
  (keyamp--set keymap '(cycle-hyphen-lowline-space) nil nil nil keyamp-delay-1))


;; Modes Remaps

(with-eval-after-load 'minibuffer
  (with-sparse-keymap
    ;; On minibuffer startup press DEL to list history backwards or
    ;; SPC to paste. S-SPC to start list completion candidates forward.
    ;; I/K or DEL/SPC to list either history or completion candidates
    ;; accordingly choice made. RET to confirm and exit, ESC to quit.
    ;; To switch from history to candidates listing press ESC then double
    ;; SPC `select-block' and DEL/SPC or I/K again to continue move
    ;; backward/forward. Similarly double DEL to activate history move.
    ;; Fast history or completion candidates direction switch to quit.
    (keyamp--map-leader keymap '(paste-or-prev . select-block))
    (keyamp--map-escape keymap keyamp-minibuffer-escape)
    (keyamp--map-return keymap keyamp-minibuffer-return)
    (keyamp--map keymap '(("C-t" . keyamp-minibuffer-shift))) ; S-<return>
    (keyamp--map-tab keymap comp-forw)
    (keyamp--map-backtab keymap isearch-backward)
    (keyamp--map-ascii keymap 'keyamp-insert-minibuffer)
    (keyamp--map keymap
      '(("<left>" . isearch-backward) ("<right>" . keyamp-minibuffer-shift)
        ("<up>"   . select-word)      ("<down>"  . comp-forw)))

    ;; The hook is last one run during minibuffer setup and set the keymap.
    (keyamp--hook keymap '(minibuffer-setup-hook) :command nil :repeat))

  (with-sparse-keymap ; quit with ESC right away after paste
    (keyamp--map-escape keymap keyamp-minibuffer-quit)
    (advice-add 'paste-or-prev :after
                (lambda () (when (minibufferp) (set-transient-map keymap)))))

  ;; Hit D/DEL for No, K/SPC for Yes to answer non-literal Y or N.
  (keyamp--remap y-or-n-p-map
    '((select-word   . y-or-n-p-insert-n) (del-back  . y-or-n-p-insert-n)
      (paste-or-prev . y-or-n-p-insert-y) (next-line . y-or-n-p-insert-y)
      (select-block  . y-or-n-p-insert-n)))

  (keyamp--remap minibuffer-local-map
    '((previous-line . hist-back) (next-line . hist-forw)
      (select-block  . hist-back)
      (up-line       . hist-back) (down-line . hist-forw)))

  (keyamp--map-tab minibuffer-local-completion-map minibuffer-complete)
  (keyamp--map minibuffer-local-completion-map '(("C-t" . keyamp-minibuffer-shift)))
  (keyamp--remap minibuffer-mode-map
    '((previous-line . hist-back) (next-line . hist-forw)
      (select-block  . hist-back)
      (up-line       . hist-back) (down-line . comp-forw)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(minibuffer-next-completion . minibuffer-previous-completion))
    (keyamp--map-escape keymap delete-completion-win)
    (keyamp--map-return keymap minibuffer-choose-completion)
    (keyamp--map-tab keymap minibuffer-next-completion)
    (keyamp--map-backtab keymap minibuffer-previous-completion)
    (keyamp--set keymap
      '(completion-at-point minibuffer-previous-completion minibuffer-next-completion)))

  (advice-add 'completion-at-point :after
              (lambda (&rest _) "select candidate" (minibuffer-next-completion)))
  (advice-add-macro '(completion-at-point minibuffer-choose-completion delete-completion-win)
                    :after 'keyamp-insert-init)

  (keyamp--map minibuffer-inactive-mode-map
    '(("<mouse-1>" . toggle-ibuffer) ("<double-mouse-1>" . ignore)))
  (keyamp--remap minibuffer-inactive-mode-map '((mouse-3 . radio-next))))

(with-eval-after-load 'icomplete
  (keyamp--map-return icomplete-minibuffer-map keyamp-minibuffer-return)
  (keyamp--remap icomplete-minibuffer-map
    '((previous-line . comp-back) (next-line . comp-forw) (select-word . comp-forw)
      (up-line       . comp-back) (down-line . comp-forw)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
    (keyamp--map-escape keymap abort-recursive-edit)
    (keyamp--map-return keymap keyamp-minibuffer-return)
    (keyamp--remap keymap '((previous-line . comp-back) (next-line . comp-forw)))
    (keyamp--remap keymap '((up-line . comp-back) (down-line . comp-forw)))
    (keyamp--set keymap '(comp-back comp-forw)))

  (with-sparse-keymap
    (keyamp--remap keymap '((previous-line . hist-back) (next-line . comp-forw)))
    (keyamp--remap keymap '((up-line . hist-back) (down-line . comp-forw)))
    (keyamp--hook keymap '(icomplete-minibuffer-setup-hook) nil nil :repeat))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
    (keyamp--map-escape keymap abort-recursive-edit)
    (keyamp--map-return keymap exit-minibuffer)
    (keyamp--remap keymap '((previous-line . hist-back) (next-line . hist-forw)))
    (keyamp--remap keymap '((up-line . hist-back) (down-line . hist-forw)))
    (keyamp--set keymap '(hist-back hist-forw))))

(add-hook 'ido-setup-hook
  (lambda () "ido-completion-map created after ido setup only"
    (keyamp--remap ido-completion-map
      '((keyamp-insert . ido-exit-minibuffer)
        (previous-line . hist-back)      (select-block       . hist-back)
        (next-line     . ido-next-match) (select-word        . ido-next-match)
        (up-line       . hist-back)      (down-line          . ido-next-match)
        (comp-forw     . ido-next-match) (ido-complete-space . self-insert-command)))))

(with-sparse-keymap
  (keyamp--map-leader keymap '(next-line . previous-line))
  (keyamp--remap keymap '((previous-line . ido-prev-match) (next-line . ido-next-match)))
  (keyamp--remap keymap '((up-line . ido-prev-match) (down-line . ido-next-match)))
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
      (open-line           . prev-dired-buf)
      (del-back            . dired-toggle-mark)
      (newline             . next-dired-buf)
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

  (advice-add 'dired-toggle-marks :before #'dired-unmark-all-marks))

(with-eval-after-load 'wdired
  (keyamp--map wdired-mode-map '(("C-q" . wdired-abort-changes) ("C-t" . wdired-finish-edit)))
  (advice-add-macro '(wdired-abort-changes wdired-finish-edit) :after 'keyamp-command))

(with-eval-after-load 'dired-utils
  (keyamp--map-backtab dired-mode-map dired-rleader-map)
  (keyamp--map-backtab (define-prefix-command 'dired-rleader-map) dired-omit-mode)
  (keyamp--map-leader dired-rleader-map '(nil . dired-size))

  (keyamp--map-tab dired-mode-map dired-lleader-map)
  (keyamp--map-tab (define-prefix-command 'dired-lleader-map) dired-omit-mode)
  (keyamp--map-leader dired-lleader-map '(dired-size . nil))
  (keyamp--map dired-lleader-map
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
  (keyamp--map-tab ibuffer-mode-map ibuffer-visit-buffer)
  (keyamp--map-backtab ibuffer-mode-map ibuffer-backward-filter-group)
  (keyamp--remap ibuffer-mode-map
    '((previous-line       . up-line)
      (next-line           . down-line)
      (keyamp-insert       . ibuffer-visit-buffer)
      (end-of-block        . ibuffer-forward-filter-group)
      (beg-of-block        . ibuffer-backward-filter-group)
      (insert-space-before . clock)
      (backward-del-word   . sun-moon)
      (undo                . split-window-below)
      (cut-text-block      . calc)
      (goto-match-br       . view-messages)
      (shrink-whitespaces  . calendar-split)
      (open-line           . prev-buf)
      (del-back            . alt-buf)
      (newline             . next-buf)
      (toggle-comment      . view-messages)
      (cut-line            . prev-eww-buf)
      (paste-or-prev       . tasks)
      (toggle-case         . downloads)
      (forward-bracket     . ibuffer-do-delete)
      (kmacro-helper       . config)
      (del-word            . toggle-gnus)
      (forw-char           . screen-idle-return)
      (back-char           . screen-idle)
      (append-to-r1        . recentf-open-files)))

  (keyamp--map ibuffer-mode-filter-group-map '(("<mouse-1>" . ibuffer-toggle-filter-group)))
  (keyamp--map-tab ibuffer-mode-filter-group-map ibuffer-toggle-filter-group)
  (keyamp--remap ibuffer-mode-filter-group-map
    '((keyamp-insert . ibuffer-toggle-filter-group)
      (forw-char     . screen-idle-return)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
    (keyamp--remap keymap '((previous-line . up-line-rev) (next-line . down-line)))
    (keyamp--set keymap '(ibuffer-toggle-filter-group)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(ibuffer-forward-filter-group . ibuffer-backward-filter-group))
    (keyamp--set keymap '(ibuffer-backward-filter-group ibuffer-forward-filter-group))))

(with-eval-after-load 'ibuffer
  (keyamp--map ibuffer-name-map '(("<mouse-1>" . mouse-set-point)))

  (with-sparse-keymap
    (keyamp--remap keymap '((del-back . ibuffer-do-delete)))
    (keyamp--set keymap '(ibuffer-do-delete) nil nil nil keyamp-delay-3)))

(with-eval-after-load 'company
  (keyamp--map-tab company-active-map company-complete-common)

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
    (keyamp--map-escape keymap company-abort)
    (keyamp--map-return keymap company-complete-selection)
    (keyamp--remap keymap
      '((isearch-forward . company-search-candidates)
        (previous-line   . company-select-back)
        (next-line       . company-select-forw)
        (beg-of-line     . company-previous-page)
        (end-of-lyne     . company-next-page)))

    (keyamp--set keymap
      '(company-select-back company-select-forw     company-previous-page
        company-next-page   company-show-doc-buffer company-search-abort
        company-manual-begin))

    (advice-add 'company-manual-begin :before #'keyamp-command)

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
    (keyamp--map-leader keymap '(keyamp-insert-and-spc . undo))
    (keyamp--set keymap '(company-search-abort company-complete-selection)))

  (advice-add 'company-search-candidates :after #'keyamp-insert-init)
  (keyamp--map-escape company-search-map company-search-abort)
  (keyamp--map-tab company-search-map company-search-repeat-forward)
  (keyamp--map-backtab company-search-map company-search-repeat-backward)

  (with-sparse-keymap
    (keyamp--map-leader keymap '(company-search-repeat-forward . company-search-repeat-backward))
    (keyamp--set keymap '(company-search-repeat-backward company-search-repeat-forward))))

(with-eval-after-load 'transient
  (keyamp--map-escape transient-base-map transient-quit-one)
  (advice-add 'transient-quit-one :after 'keyamp-command-if-insert))

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
    (keyamp--map-leader keymap '(next-match . nil))
    (keyamp--set keymap '(next-match)))

  (keyamp--map-return query-replace-map edit-replacement)
  (keyamp--map-escape query-replace-map exit)
  (keyamp--map query-replace-map '(("d" . skip) ("k" . act))))

(with-eval-after-load 'shr
  (keyamp--remap shr-map '((keyamp-insert . shr-browse-url))))

(with-eval-after-load 'simple
  (keyamp--remap completion-list-mode-map '((keyamp-insert . choose-completion))))

(with-eval-after-load 'wid-edit
  (keyamp--remap widget-link-keymap '((keyamp-insert . widget-button-press)))
  (with-sparse-keymap
    (keyamp--map-leader keymap '(widget-forward . widget-backward))
    (keyamp--set keymap '(widget-backward widget-forward))))

(with-eval-after-load 'org
  (keyamp--map-tab org-mode-map org-cycle)
  (keyamp--map-backtab org-mode-map undo)
  (keyamp--remap org-mode-map
    '((eval-region-or-sexp . insert-date) (insert-date . org-time-stamp))))

(with-eval-after-load 'org-agenda
  (keyamp--map-tab org-agenda-mode-map todo)
  (keyamp--map-backtab org-agenda-mode-map ignore)
  (keyamp--map org-agenda-mode-map
    '(("<double-mouse-1>" . org-agenda-tasks) ("<mouse-3>" . mouse-3)))
  (keyamp--remap org-agenda-mode-map
    '((keyamp-insert     . org-agenda-tasks)
      (del-word          . toggle-gnus)
      (goto-match-br     . view-messages)
      (open-line         . prev-buf)
      (del-back          . calendar-split)
      (newline           . next-buf)
      (previous-line     . up-line-rev)
      (next-line         . down-line)
      (toggle-comment    . org-agenda-redo)
      (cut-line          . prev-eww-buf)
      (paste-or-prev     . tasks)
      (toggle-case       . downloads)
      (backward-bracket  . dired-jump)
      (kmacro-helper     . config)
      (forw-char         . screen-idle-escape)
      (back-char         . screen-idle-return)
      (kmacro-record     . alarm)
      (pass              . stopwatch-lap)
      (jump-to-register  . stopwatch)
      (point-to-register . timer)
      (insert-register   . timer-stop)
      (proced-defer      . timer-display))))

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
(keyamp--map-tab screen-idle-escape-keymap keyamp-tab)
(keyamp--map screen-idle-escape-keymap
  '(("<left>" . screen-idle)    ("<right>" . screen-idle-return)
    ("<up>"   . toggle-ibuffer)))
(keyamp--set screen-idle-escape-keymap '(screen-idle-escape novel))

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
      (bchar              . back-word)
      (fchar              . forw-word)
      (previous-line      . up-line-rev)
      (next-line          . down-line)))
  (keyamp--remap eww-link-keymap '((keyamp-insert . eww-follow-link))))

(with-eval-after-load 'emms
  (with-sparse-keymap
    (keyamp--map-leader keymap '(newline . open-line))
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
    (keyamp--map-leader keymap '(newline . open-line))
    (keyamp--remap keymap '((open-line . emms-seek-backward) (newline . emms-seek-forward)))
    (keyamp--set keymap '(emms-seek-backward emms-seek-forward))))

(with-eval-after-load 'emms-playlist-mode
  (keyamp--remap emms-playlist-mode-map
    '((keyamp-insert     . emms-playlist-mode-play-smart)
      (mouse-set-point   . emms-playlist-mode-play-smart)
      (open-line         . emms-seek-backward-or-previous)
      (newline           . emms-seek-forward-or-next)
      (undo              . split-window-below)
      (backward-del-word . emms-seek-backward)
      (del-word          . emms-seek-forward)
      (del-back          . emms-playlist-mode-center-current)
      (backward-bracket  . dired-jump)
      (forward-bracket   . ignore))))

(with-eval-after-load 'flyspell
  (with-sparse-keymap
    (keyamp--map-leader keymap '(newline . open-line))
    (keyamp--remap keymap
      '((del-back  . ispell-word)
        (open-line . flyspell-goto-prev-error)
        (newline   . flyspell-goto-next-error)))
    (keyamp--set keymap
      '(flyspell-buffer          ispell-word
        flyspell-goto-prev-error flyspell-goto-next-error))))

(with-eval-after-load 'doc-view
  (keyamp--remap doc-view-mode-map
    '((keyamp-insert  . keyamp-escape)
      (select-block   . doc-view-scroll-down-or-previous-page)
      (select-word    . doc-view-scroll-up-or-next-page)
      (previous-line  . doc-view-scroll-down-or-previous-page)
      (next-line      . doc-view-scroll-up-or-next-page)
      (up-line        . doc-view-scroll-down-or-previous-page)
      (down-line      . doc-view-scroll-up-or-next-page)
      (bchar          . doc-view-previous-page)
      (fchar          . doc-view-next-page)
      (enlarge-window . doc-view-enlarge)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(doc-view-enlarge . doc-view-shrink))
    (keyamp--set keymap '(doc-view-shrink doc-view-enlarge) nil nil nil keyamp-delay-2))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
    (keyamp--remap keymap
      '((previous-line . doc-view-scroll-down-or-previous-page)
        (next-line     . doc-view-scroll-up-or-next-page)
        (up-line       . doc-view-scroll-down-or-previous-page)
        (down-line     . doc-view-scroll-up-or-next-page)))
    (keyamp--set keymap
      '(doc-view-scroll-down-or-previous-page doc-view-scroll-up-or-next-page))))

(with-eval-after-load 'image-mode
  (keyamp--remap image-mode-map
    '((keyamp-insert    . keyamp-escape)
      (bchar            . image-previous-file) (fchar        . image-next-file)
      (back-char        . image-previous-file) (forw-char    . image-next-file)
      (previous-line    . image-decrease-size) (next-line    . image-increase-size)
      (open-line        . image-previous-file) (newline      . image-next-file)
      (undo             . image-dired)         (del-back     . image-rotate)
      (select-word      . image-next-file)     (select-block . image-previous-file)
      (backward-bracket . dired-jump)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(fchar . bchar))
    (keyamp--set keymap '(image-previous-file image-next-file)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
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

(with-eval-after-load 'sql
  (keyamp--remap sql-interactive-mode-map '((select-block . comint-previous-input)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
    (keyamp--remap keymap
      '((previous-line . comint-previous-input) (next-line . comint-next-input)))
    (keyamp--set keymap '(comint-previous-input comint-next-input) :command)))

(with-eval-after-load 'esh-mode
  (keyamp--map-tab eshell-mode-map completion-at-point)
  (keyamp--map-backtab eshell-mode-map undo)
  (keyamp--remap eshell-mode-map
    '((cut-line       . eshell-clear)
      (select-block   . eshell-previous-input)
      (open-line      . prev-eshell-buf)
      (newline        . next-eshell-buf)
      (toggle-comment . ignore)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
    (keyamp--remap keymap
      '((previous-line . eshell-previous-input) (next-line . eshell-next-input)))
    (keyamp--set keymap
      '(eshell-previous-input eshell-next-input eshell-search-input) :command))

  (with-sparse-keymap
    ;; Insert mode primary for eshell. The keymap ready after eshell start,
    ;; command submit or cancel. DEL to list history, SPC to paste.
    (keyamp--map-leader keymap '(paste-or-prev . eshell-previous-input))
    (keyamp--map-tab keymap change-wd)
    (keyamp--map-backtab keymap eshell-search-input)
    (keyamp--set keymap '(eshell-send-input eshell-interrupt-process) nil :insert)

    (defun keyamp-input-timer-payload-eshell ()
      "Set transient keymap for eshell after input timer timeout."
      (if (eq major-mode 'eshell-mode)
          (keyamp-repeat-deactivate-init keymap)))

    (advice-add 'keyamp-input-timer-payload :after #'keyamp-input-timer-payload-eshell))

  (advice-add-macro '(eshell-send-input eshell-interrupt-process)
                    :after 'keyamp-input-timer))

(with-eval-after-load 'em-cmpl ; <backtab> conflict w/ default
  (keyamp--map-backtab eshell-cmpl-mode-map eshell-search-input))

;;; vterm
(with-eval-after-load 'vterm
  (keyamp--map-tab vterm-mode-map vterm-send-tab)
  (keyamp--map-backtab vterm-mode-map vterm-send-backtab)

  (keyamp--remap vterm-mode-map
    '((select-block        . vterm-up-vi-cmd)
      (paste-or-prev       . vterm-yank)
      (kill-line           . vterm-tmux-copy) ; activate tmux copy mode
      (page-up-half        . vterm-tmux-copy-hpu)
      (page-dn-half        . vterm-tmux-copy-hpd)
      (cut-text-block      . tt-conn-reconnect)
      (open-line           . vterm-tmux-prev-window)
      (insert-space-before . vterm-shell-vi-cmd) ; sync point position and activate shell vi cmd mode
      (newline             . vterm-tmux-next-window)
      (toggle-comment      . vterm-read-send-key)
      (cut-line            . vterm-clear)
      (new-empty-buffer    . vterm-tmux-new-window)
      (query-replace       . vterm-vi) ; activate vi mode
      (reformat-lines      . vterm-tmux-close-window)
      (back-char           . vterm-left)
      (forw-char           . vterm-right)
      (up-line             . vterm-up)
      (down-line           . vterm-down)
      (toggle-case         . prev-vterm-buf)
      (dired-jump          . tt-conn-tramp)
      (bookmark-set        . tt-sftp)))

  ;; sync point on insert
  (add-hook 'keyamp-insert-hook 'vterm-reset-cursor-point)

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
    (keyamp--remap keymap '((previous-line . vterm-up-vi-cmd) (next-line . vterm-down)))
    (keyamp--set keymap '(vterm-history-search) nil :insert)
    (keyamp--set keymap '(vterm-up-vi-cmd vterm-down vterm-yank-pop) :command))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(vterm-tmux-copy . vterm-up-vi-cmd))
    (keyamp--map-tab keymap change-wd)
    (keyamp--map-backtab keymap vterm-history-search)
    (keyamp--set keymap '(vterm-send-return term-interrupt-subjob) nil :insert)
    (keyamp--set keymap '(vterm-vi-save-quit vterm-vi-quit))

    (defun keyamp-input-timer-payload-vterm ()
      "Set transient keymap for vterm after input timer timeout."
      (when (and (eq major-mode 'vterm-mode)
                 keyamp-insert-p)
        (keyamp-repeat-deactivate-init keymap)))

    (advice-add 'keyamp-input-timer-payload :after #'keyamp-input-timer-payload-vterm))

  (advice-add-macro '(vterm-send-return term-interrupt-subjob) :after 'keyamp-input-timer)

  (with-sparse-keymap
    (keyamp--map-return keymap keyamp-ret)
    (keyamp--remap keymap '((open-line . vterm-tmux-prev-window) (newline . vterm-tmux-next-window)))
    (keyamp--set keymap '(vterm-tmux-prev-window vterm-tmux-next-window)))

  (with-sparse-keymap
    (keyamp--remap keymap
      '((bchar         . vterm-left) (fchar     . vterm-right)
        (previous-line . vterm-up)   (next-line . vterm-down)))
    (keyamp--set keymap '(vterm-left vterm-right vterm-up vterm-down)))

  ;;;;;; shell prompt vi cmd mode
  (with-sparse-keymap
    (keyamp--remap keymap
      '((bchar              . vterm-shell-vi-self-insert)
        (fchar              . vterm-shell-vi-s) ; Engram notation
        (back-word          . vterm-shell-vi-l)
        (forw-word          . vterm-shell-vi-w)
        (del-back           . vterm-shell-vi-e)
        (shrink-whitespaces . vterm-shell-vi-a) ; delete char forward
        (undo               . vterm-shell-vi-self-insert)
        (beg-of-line        . vterm-shell-vi-self-insert)
        (end-of-lyne        . vterm-shell-vi-self-insert)
        (del-word           . vterm-shell-vi-self-insert)
        (backward-del-word  . vterm-shell-vi-self-insert)))
    (keyamp--set keymap
      '(vterm-up-vi-cmd ; vi cmd mode auto enable
        vterm-down
        vterm-shell-vi-cmd    vterm-shell-vi-self-insert
        vterm-shell-vi-s      vterm-shell-vi-l
        vterm-shell-vi-w      vterm-shell-vi-e
        vterm-shell-vi-a))
    ;; vi insert sync with keyamp
    (add-hook 'keyamp-insert-hook 'vterm-shell-vi-insert))

  (with-sparse-keymap ; move word repeat
    (keyamp--remap keymap '((bchar . vterm-shell-vi-l) (fchar . vterm-shell-vi-w)))
    (keyamp--set keymap '(vterm-shell-vi-l vterm-shell-vi-w)))

  (with-sparse-keymap ; delete char repeat
    (keyamp--map-leader keymap '(vterm-shell-vi-a . vterm-shell-vi-e))
    (keyamp--set keymap '(vterm-shell-vi-e vterm-shell-vi-a)))

  ;; Config examples:

  ;;;;;; .inputrc
  ;; $if mode=vi
  ;;    set keymap vi-command
  ;;    "\C-m": vi-insertion-mode
  ;;    "i": previous-history
  ;;    "k": next-history
  ;;    "j": backward-char
  ;;    "u": backward-word
  ;;    "l": forward-char
  ;;    "o": forward-word
  ;;    "d": backward-delete-char
  ;;    "f": delete-char
  ;;    "h": beginning-of-line
  ;;    ";": end-of-line
  ;;    "r": kill-word
  ;;    "w": backward-kill-word
  ;;    "\C-?": previous-history
  ;;    " ": next-history
  ;;    "e": undo
  ;;    "^[": vi-movement-mode
  ;; $endif

  ;;;;;; .zshrc
  ;; set -o vi
  ;; bindkey "^[" vi-cmd-mode

  ;; bindkey -M vicmd "\C-m" vi-insert
  ;; bindkey -M vicmd "\C-j" vi-insert
  ;; bindkey -M vicmd "j" vi-backward-char
  ;; bindkey -M vicmd "k" down-line-or-history
  ;; bindkey -M vicmd "i" up-line-or-history
  ;; bindkey -M vicmd "l" vi-forward-char
  ;; bindkey -M vicmd "u" vi-backward-word
  ;; bindkey -M vicmd "o" vi-forward-word
  ;; bindkey -M vicmd "d" vi-backward-delete-char
  ;; bindkey -M vicmd "f" vi-delete-char
  ;; bindkey -M vicmd "h" vi-beginning-of-line
  ;; bindkey -M vicmd ";" vi-end-of-line
  ;; bindkey -M vicmd "r" kill-word
  ;; bindkey -M vicmd "w" vi-backward-kill-word
  ;; bindkey -M vicmd " " down-line-or-history
  ;; bindkey -M vicmd "^?" up-line-or-history
  ;; bindkey -M vicmd "e" undo

  ;;;;;; tmux copy mode vi
  (with-sparse-keymap
    (keyamp--map-leader keymap '(vterm-tmux-copy-self-insert . vterm-tmux-copy-self-insert))
    (keyamp--map-return keymap vterm-shell-vi-cmd) ; quit and sync prompt position
    (keyamp--remap keymap
      '((previous-line   . vterm-tmux-copy-self-insert)
        (bchar           . vterm-tmux-copy-self-insert)
        (back-word       . vterm-tmux-copy-self-insert)
        (forw-word       . vterm-tmux-copy-self-insert)
        (next-line       . vterm-tmux-copy-self-insert)
        (fchar           . vterm-tmux-copy-self-insert)
        (beg-of-line     . vterm-tmux-copy-self-insert)
        (end-of-lyne     . vterm-tmux-copy-self-insert)
        (del-back        . vterm-shell-vi-cmd)
        (copy-line       . vterm-tmux-copy-self-insert)
        (activate-region . vterm-tmux-copy-self-insert)
        (isearch-forward . vterm-tmux-copy-self-insert)))
    (keyamp--set keymap
      '(vterm-tmux-copy     vterm-tmux-copy-self-insert
        vterm-tmux-copy-hpu vterm-tmux-copy-hpd) :command))

  (with-sparse-keymap
    (keyamp--map-return keymap keyamp-ret)
    (keyamp--remap keymap
      '((previous-line . vterm-tmux-copy-hpu) (next-line . vterm-tmux-copy-hpd)))
    (keyamp--set keymap '(vterm-tmux-copy-hpu vterm-tmux-copy-hpd)))

  ;;;;;; tmux.conf
  ;; bind -T copy-mode-vi c send-keys -X copy-pipe-and-cancel 'tee > /tmp/tmux-copy~$(date "+%Y-%m-%d_%H%M%S")~'
  ;; if-shell 'uname | grep -q Darwin' { bind -T copy-mode-vi c send-keys -X copy-pipe-and-cancel 'pbcopy' }

  ;; bind -T copy-mode-vi Escape send-keys -X cancel
  ;; bind -T copy-mode-vi Enter send-keys -X cancel

  ;; bind -T copy-mode-vi i send-keys -X cursor-up
  ;; bind -T copy-mode-vi j send-keys -X cursor-left
  ;; bind -T copy-mode-vi k send-keys -X cursor-down
  ;; bind -T copy-mode-vi l send-keys -X cursor-right

  ;; bind -T copy-mode-vi u send-keys -X previous-word
  ;; bind -T copy-mode-vi o send-keys -X next-word-end

  ;; bind -T copy-mode-vi C-? send-keys -X halfpage-up
  ;; bind -T copy-mode-vi Space send-keys -X halfpage-up
  ;; bind -T copy-mode-vi BSpace send-keys -X halfpage-down
  ;; bind -T copy-mode-vi h send-keys -X start-of-line
  ;; bind -T copy-mode-vi ; send-keys -X end-of-line

  ;; bind -T copy-mode-vi n command-prompt -T search -p "(search up)" { send-keys -X search-backward "%%" }

  ;; bind -T copy-mode-vi Tab send-keys -X search-reverse
  ;; bind -T copy-mode-vi BTab send-keys -X search-again

  ;;;;;; vi mode - run TUI inside Emacs
  (with-sparse-keymap
    (keyamp--map-leader keymap '(vterm-vi-self-insert . vterm-vi-self-insert))
    (keyamp--map-escape keymap vterm-vi-escape)
    (keyamp--map-return keymap vterm-vi-self-insert)
    (keyamp--map-tab keymap vterm-vi-self-insert)
    (keyamp--map-backtab keymap vterm-vi-self-insert)
    (keyamp--map-ascii keymap 'vterm-vi-self-insert)
    (keyamp--map keymap
      '(("<left>" . vterm-vi-self-insert) ("<right>" . vterm-vi-self-insert)
        ("<up>"   . vterm-vi-self-insert) ("<down>"  . vterm-vi-self-insert)
        ("C-q"    . keyamp-command)))
    (keyamp--set keymap '(vterm-vi vterm-vi-self-insert vterm-vi-escape) :command))

  (defun vterm-vi-auto (&rest _)
    "Auto enable vi mode."
    (when (string-match " vi " vterm-last-command)
      (keyamp-cancel-input-timer)
      (keyamp-command)
      (keyamp-command-execute 'vterm-vi)))

  (advice-add 'vterm-send-return :after #'vterm-vi-auto '((depth . 90))))

(defvar keyamp-ignore-map (make-sparse-keymap)
  "Keymap ignores any key. Maybe trigger action with post command hook.")

(keyamp--map-leader keyamp-ignore-map '(ignore . ignore))
(keyamp--map-escape keyamp-ignore-map ignore)
(keyamp--map-return keyamp-ignore-map ignore)
(keyamp--map-tab keyamp-ignore-map ignore)
(keyamp--map-backtab keyamp-ignore-map ignore)
(keyamp--map-ascii keyamp-ignore-map 'ignore)
(keyamp--map keyamp-ignore-map
  '(("<down-mouse-1>" . ignore) ("<mouse-1>" . ignore) ("<drag-mouse-1>" . ignore)
    ("<left>" . ignore) ("<right>" . ignore)
    ("<up>"   . ignore) ("<down>"  . ignore)))

(with-eval-after-load 'info
  (keyamp--remap Info-mode-map
    '((keyamp-insert      . Info-follow-nearest-node)
      (open-line          . Info-backward-node)
      (newline            . Info-forward-node)
      (undo               . Info-up)
      (del-back           . Info-next-reference)
      (shrink-whitespaces . Info-history-back)
      (previous-line      . up-line-rev)
      (next-line          . down-line)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(newline . open-line))
    (keyamp--set keymap '(Info-backward-node Info-forward-node)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(Info-next-reference . Info-prev-reference))
    (keyamp--remap keymap '((undo . Info-prev-reference) (del-back . Info-next-reference)))
    (keyamp--set keymap '(Info-prev-reference Info-next-reference))))

(with-eval-after-load 'help-mode
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
      (previous-line . up-line-rev)        (next-line    . down-line)
      (beg-of-line   . gnus-topic-prev)    (end-of-lyne  . gnus-topic-next)
      (beg-of-block  . gnus-topic-prev)    (end-of-block . gnus-topic-next)
      (back-char     . screen-idle-escape) (forw-char    . screen-idle)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
    (keyamp--remap keymap '((previous-line . up-line-rev) (next-line . down-line)))
    (keyamp--set keymap '(gnus-topic-select-group)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(next-line . previous-line))
    (keyamp--remap keymap
      '((previous-line . gnus-topic-prev) (next-line    . gnus-topic-next)
        (beg-of-line   . gnus-beg-of-buf) (end-of-lyne  . gnus-end-of-buf)
        (end-of-block  . gnus-beg-of-buf) (beg-of-block . gnus-end-of-buf)))
    (keyamp--set keymap
      '(gnus-topic-prev gnus-topic-next gnus-beg-of-buf gnus-end-of-buf) nil nil nil 2)))

(with-eval-after-load 'gnus-group
  (keyamp--remap gnus-group-mode-map
    '((back-char          . screen-idle-escape)
      (forw-char          . screen-idle)
      (backward-del-word  . sun-moon)
      (undo               . split-window-below)
      (del-word           . gnus-group-enter-server-mode)
      (cut-text-block     . calc)
      (goto-match-br      . view-messages)
      (open-line          . prev-buf)
      (del-back           . gnus-group-get-new-news)
      (newline            . next-buf)
      (cut-line           . prev-eww-buf)
      (copy-line          . screen-idle)
      (paste-or-prev      . tasks)
      (backward-bracket   . downloads)
      (forward-bracket    . save-close-buf)
      (kmacro-helper      . config))))

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
      (paste-from-r1 . gnus-summary-save-parts)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(down-line . up-line))
    (keyamp--map-tab keymap keyamp-tab)
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
    '((keyamp-escape      . recentf-cancel-dialog)
      (sh-defer           . recentf-open-most-recent-file-0)
      (kmacro-record      . recentf-open-most-recent-file-1)
      (kmacro-play        . recentf-open-most-recent-file-2)
      (kmacro-helper      . recentf-open-most-recent-file-3)
      (append-to-r1       . recentf-open-most-recent-file-4)
      (terminal           . recentf-open-most-recent-file-5)
      (pass               . recentf-open-most-recent-file-6)
      (jump-to-register   . recentf-open-most-recent-file-7)
      (point-to-register  . recentf-open-most-recent-file-8)
      (proced-defer       . recentf-open-most-recent-file-9)))
  (with-sparse-keymap
    (keyamp--map-leader keymap '(widget-forward . widget-backward))
    (keyamp--set keymap '(recentf-open-files))))

(with-sparse-keymap
  (keyamp--remap keymap
    '((sh-defer          . radio-channel-0) (kmacro-record    . radio-channel-1)
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
      (next-line     . tetris-move-bottom) (bchar . tetris-move-down)))

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
  (keyamp--map find-output-mode-map '(("<mouse-1>" . find--jump-to-place)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(fchar . bchar))
    (keyamp--remap keymap
      '((previous-line . find-previous-file)  (next-line . find-next-file)
        (bchar         . find-previous-match) (fchar     . find-next-match)))
    (keyamp--set keymap
      '(find-next-match find-previous-file find-previous-match find-next-file))
    (keyamp--hook keymap '(find-output-mode-hook) nil nil :repeat)))

(with-eval-after-load 'emacs-lisp-mode
  (keyamp--map-tab emacs-lisp-mode-map emacs-lisp-indent)
  (keyamp--map-backtab emacs-lisp-mode-map undo)
  (keyamp--remap emacs-lisp-mode-map
    '((reformat-lines . emacs-lisp-remove-paren-pair))))

(with-eval-after-load 'perl-mode
  (keyamp--map-backtab perl-mode-map undo))

(with-sparse-keymap
  (keyamp--map-leader keymap '(fchar . bchar))
  (keyamp--remap keymap
    '((bchar     . flymake-goto-prev-error)
      (fchar     . flymake-goto-next-error)
      (back-word . flymake-goto-prev-error)
      (forw-word . flymake-goto-next-error)))
  (keyamp--set keymap '(flymake-goto-prev-error flymake-goto-next-error)))

(with-eval-after-load 'python
  (keyamp--map-tab python-ts-mode-map python-indent-or-complete)
  (keyamp--map-backtab python-ts-mode-map python-de-indent)
  (keyamp--map-return python-ts-mode-map python-return-and-indent)
  (keyamp--remap python-ts-mode-map
    '((newline           . python-return-and-indent)
      (reformat-lines    . python-format-buffer)
      (describe-variable . xref-find-references)))
  (with-sparse-keymap
    (keyamp--map-leader keymap '(python-indent-or-complete . python-de-indent))
    (keyamp--set keymap '(python-indent-or-complete python-de-indent)
      nil nil nil keyamp-delay-1)))

(with-eval-after-load 'go-ts-mode
  (keyamp--map-tab go-ts-mode-map company-manual-begin)
  (keyamp--remap go-ts-mode-map
    '((mark-defun          . go-mark-defun)
      (stow                . flymake-show-project-diagnostics)
      (eval-region-or-sexp . make-run)
      (eval-defun-visual   . make-test)
      (reformat-lines      . eglot-reconnect)
      (describe-variable   . xref-find-references))))

(with-sparse-keymap
  (keyamp--map-leader keymap '(xref-find-definitions . xref-go-back))
  (keyamp--set keymap '(xref-go-back xref-find-definitions)))

(with-eval-after-load 'xref
  (keyamp--remap xref--xref-buffer-mode-map
    '((keyamp-insert . xref-show-location-at-point))))

(with-eval-after-load 'sh-script
  (keyamp--map-tab bash-ts-mode-map indent-for-tab-command)
  (keyamp--map-tab sh-mode-map indent-for-tab-command))

(with-eval-after-load 'sqlite-mode
  (keyamp--remap sqlite-mode-map
    '((keyamp-insert . sqlite-mode-list-data)    (del-back  . sqlite-mode-delete)
      (newline       . sqlite-mode-list-columns) (open-line . sqlite-mode-list-tables))))

(with-eval-after-load 'sql
  (keyamp--remap sql-mode-map
    '((eval-defun-visual   . exec-query-remote)
      (eval-region-or-sexp . exec-query)
      (number-to-register  . toggle-sql-async-conn)
      (quit                . toggle-sql-async-remote)
      (reformat-lines      . sql-format-buffer)))
  (with-sparse-keymap
    (keyamp--remap keymap
      '((point-to-register . toggle-sql-type)
        (jump-to-register  . toggle-sql-async-conn)
        (pass              . toggle-sql-async-remote)))
    (keyamp--remap keymap '((jump-to-register . toggle-sql-async-conn)))
    (keyamp--set keymap
      '(sql toggle-sql-type exec-query
        toggle-sql-async-conn toggle-sql-async-remote))))

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
    (keyamp--map-leader keymap '(calendar-scroll-left . calendar-scroll-right))
    (keyamp--map-tab keymap calendar-other-month)
    (keyamp--set keymap '(calendar-scroll-left calendar-scroll-right calendar-goto-today))))

(with-eval-after-load 'simple
  (keyamp--remap messages-buffer-mode-map
    '((keyamp-insert     . keyamp-escape)
      (undo              . split-window-below)
      (del-back          . alt-buf)
      (open-line         . prev-buf)
      (newline           . next-buf)
      (paste-or-prev     . tasks)
      (previous-line     . up-line-rev)
      (next-line         . down-line)
      (backward-del-word . sun-moon)
      (cut-text-block    . calc)
      (cut-line          . prev-eww-buf)
      (backward-bracket  . downloads)
      (forward-bracket   . save-close-buf)
      (kmacro-helper     . config)))
  (keyamp--remap special-mode-map
    '((undo              . split-window-below)
      (del-back          . alt-buf)
      (open-line         . prev-buf)
      (newline           . next-buf)
      (eval-defun-visual . exec-query))))

(with-eval-after-load 'calc
  (advice-add 'calcDigit-start :after #'keyamp-insert)
  (advice-add 'calcDigit-start :after #'keyamp-input-timer))
  (advice-add-macro
    '(calc-plus calc-minus calc-times calc-divide
      calc-mod  calc-inv   calc-power calc-enter) :after 'keyamp-start-input-timer)

(with-eval-after-load 'calc-ext
  (keyamp--remap calc-mode-map
    '((del-back      . calc-pop)       (undo    . calc-undo)
      (open-line     . calc-roll-down) (newline . calc-algebraic-entry)
      (paste-or-prev . calc-yank)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(del-back . undo))
    (keyamp--remap keymap '((del-back . calc-redo)))
    (keyamp--set keymap '(calc-undo calc-redo))

    (defun keyamp-input-timer-payload-calc ()
      "Set transient keymap for calc after input timer timeout."
      (when (eq major-mode 'calc-mode)
        (keyamp-repeat-deactivate-init keymap)))

    (advice-add 'keyamp-input-timer-payload :after #'keyamp-input-timer-payload-calc)))

(with-eval-after-load 'dslide
  (keyamp--map-backtab dslide-mode-map dslide-deck-stop)
  (keyamp--map-tab dslide-mode-map dslide-deck-start)
  (keyamp--remap dslide-mode-map
    '((del-back  . dslide-deck-start)    (undo    . dslide-deck-stop)
      (open-line . dslide-deck-backward) (newline . dslide-deck-forward)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(dslide-deck-forward . dslide-deck-backward))
    (keyamp--set keymap
      '(dslide-deck-backward dslide-deck-forward dslide-deck-start))))

(with-eval-after-load 'gptel
  (keyamp--remap gptel-mode-map '((eval-region-or-sexp . gptel-send)))
  (advice-add 'gptel-menu :before 'keyamp-insert))


;; Command indication mapping

(defconst keyamp-screen-commands-hash
  #s(hash-table test equal data
                (async-shell-command                     t
                 calendar-split                          t
                 clock                                   t
                 config                                  t
                 describe-char                           t
                 describe-face                           t
                 describe-foo-at-point                   t
                 describe-function                       t
                 describe-key                            t
                 describe-mode                           t
                 describe-variable                       t
                 exec-query                              t
                 exec-query-async                        t
                 find-next-dir-file                      t
                 find-prev-dir-file                      t
                 gnus-summary-scroll-up                  t
                 list-matching-lines                     t
                 isearch-occur                           t
                 next-buffer                             t
                 next-eww-buf                            t
                 next-eshell-buf                         t
                 next-dired-buf                          t
                 next-proj-buf                           t
                 next-buf                                t
                 occur-cur-word                          t
                 open-in-external-app                    t
                 org-agenda-tasks                        t
                 player                                  t
                 prev-eww-buf                            t
                 prev-eshell-buf                         t
                 prev-dired-buf                          t
                 prev-proj-buf                           t
                 prev-buf                                t
                 previous-buffer                         t
                 save-close-buf                          t
                 split-window-horizontally               t
                 sun-moon                                t
                 sync                                    t
                 tasks                                   t
                 tools                                   t
                 view-messages                           t)))

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
                 eval-defun-visual                       t
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
                 undo                                    t)))

(defconst keyamp-read-commands-hash
  #s(hash-table test equal data
                (back-word-repeat                        t
                 button-back                             t
                 back-char                               t
                 beg-of-block                            t
                 beg-of-block-rev                        t
                 beg-of-buf                              t
                 calc-redo                               t
                 calc-undo                               t
                 calendar-goto-today                     t
                 calendar-scroll-left                    t
                 calendar-scroll-right                   t
                 comint-previous-input                   t
                 comint-next-input                       t
                 company-manual-begin                    t
                 company-next-page                       t
                 company-previous-page                   t
                 company-select-forw                     t
                 company-select-back                     t
                 completion-at-point                     t
                 comp-back                               t
                 comp-forw                               t
                 copy-line                               t
                 dired-mark                              t
                 dired-unmark                            t
                 doc-view-scroll-down-or-previous-page   t
                 doc-view-scroll-up-or-next-page         t
                 doc-view-shrink                         t
                 doc-view-enlarge                        t
                 down-line                               t
                 down-line-rev                           t
                 dslide-deck-backward                    t
                 dslide-deck-forward                     t
                 dslide-deck-start                       t
                 emms-pause                              t
                 emms-playlist-mode-play-smart           t
                 emms-random                             t
                 emms-seek-backward                      t
                 emms-seek-backward-or-previous          t
                 emms-seek-forward                       t
                 emms-seek-forward-or-next               t
                 end-of-block                            t
                 end-of-block-rev                        t
                 end-of-buf                              t
                 enlarge-window                          t
                 enlarge-window-horizontally             t
                 eshell-next-input                       t
                 eshell-previous-input                   t
                 eshell-search-input                     t
                 select-block                            t
                 find-next-file                          t
                 find-next-match                         t
                 find-previous-file                      t
                 find-previous-match                     t
                 flymake-goto-next-error                 t
                 flymake-goto-prev-error                 t
                 forw-char                               t
                 forw-word-repeat                        t
                 backward-sexp                           t
                 forward-sexp                            t
                 button-forw                             t
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
                 hscroll-left                            t
                 hscroll-right                           t
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
                 isearch-double-back                     t
                 isearch-forw                            t
                 isearch-ring-advance                    t
                 isearch-ring-retreat                    t
                 isearch-yank-kill                       t
                 jump-mark                               t
                 keyamp--hook-indicate                   t
                 minibuffer-previous-completion          t
                 minibuffer-next-completion              t
                 radio-next                              t
                 radio-prev                              t
                 recenter-top-bottom                     t
                 recentf-open-files                      t
                 screen-idle-return                      t
                 scroll-down-command                     t
                 scroll-up-command                       t
                 select-word                             t
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
                 volume-increase                         t
                 volume-decrease                         t
                 vterm-down                              t
                 vterm-send-return                       t
                 vterm-vi-save-quit                      t
                 vterm-vi-quit                           t
                 vterm-up-vi-cmd                         t
                 vterm-vi                                t
                 vterm-vi-self-insert                    t
                 vterm-vi-escape                         t
                 vterm-tmux-copy                         t
                 vterm-tmux-copy-hpu                     t
                 vterm-tmux-copy-hpd                     t
                 vterm-tmux-copy-self-insert             t
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
                 isearch-double-back                     t
                 isearch-forw                            t
                 vterm-vi-self-insert                    t
                 vterm-vi-escape                         t
                 vterm-vi                                t
                 vterm-tmux-copy                         t
                 vterm-tmux-copy-self-insert             t
                 scroll-down-command                     t
                 scroll-up-command                       t
                 up-line                                 t
                 up-line-rev                             t)))

(defconst keyamp-isearch-not-insert
  '(isearch-forw isearch-wforw isearch-back isearch-double-back isearch-wback)
  "List of excluded commands from indicate insert mode in isearch.")

(defconst keyamp-blink-command-commands
  '(dslide-deck-backward dslide-deck-forward dslide-deck-start)
  "List of commands to blink command after.")

(defconst keyamp-blink-modify-commands
  '(kmacro-record               stopwatch
    python-format-buffer        save-buffer-isearch-cancel
    toggle-input-method         emacs-lisp-indent)
  "List of commands to blink modify after.")

(defconst keyamp-blink-io-commands
  '(vterm-shell-vi-cmd    vterm-shell-vi-self-insert    vterm-shell-vi-s
    vterm-shell-vi-l      vterm-shell-vi-w              vterm-shell-vi-e
    vterm-tmux-copy       vterm-tmux-copy-self-insert   vterm-read-send-key
    vterm-vi              vterm-vi-self-insert          vterm-vi-escape
    vterm-shell-vi-a      vterm-left                    vterm-right
    vterm-up              vterm-down
    copy-to-r1            append-to-r1)
  "List of commands to blink io after.")

(defconst keyamp-insert-commands
  '(self-insert-command      org-self-insert-command
    isearch-printing-char    keyamp-insert-and-self-insert
    keyamp-insert-minibuffer)
  "List of insert commands.")

(defconst keyamp-screen-command-commands
  '(dired-find-file ibuffer-visit-buffer open-last-closed
    bookmark-jump   widget-button-press  alt-buf
    alternate-frame)
  "List of command screen commands.")



(defvar keyamp-command-hook nil "Hook for `keyamp-command'.")
(defvar keyamp-insert-hook  nil "Hook for `keyamp-insert'.")

(defconst keyamp-karabiner-cli
  "/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli"
  "Karabiner-Elements CLI executable. Optional for mode sync.")

(defconst keyamp-idle-indicator    "•" "Idle indicator.")
(defconst keyamp-screen-indicator  "•" "Screen indicator.")
(defconst keyamp-read-indicator    "•" "Read indicator.")
(defconst keyamp-command-indicator "•" "Command indicator.")
(defconst keyamp-io-indicator      "•" "IO indicator.")
(defconst keyamp-insert-indicator  "•" "Insert indicator.")
(defconst keyamp-modify-indicator  "•" "Modify indicator.")

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

(defconst keyamp-command-cursor 'box        "Command cursor.")
(defconst keyamp-insert-cursor  '(hbar . 2) "Insert cursor.")
(defconst keyamp-read-cursor    'hollow     "Read cursor.")
(defconst keyamp-screen-cursor  nil         "Screen cursor.")
(defconst keyamp-modify-cursor  '(bar . 2)  "Modify cursor.")

(defconst keyamp-idle-timeout 60 "Idle timeout.")


;; Input timer

(defconst keyamp-input-timeout 3 "Input timeout.")
(defvar keyamp-input-timer nil
  "Timer activates repeat read mode if no action follows. Any command or self
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
  (when keyamp-insert-p
    (keyamp-command)
    (keyamp-indicate-read-defer)
    (keyamp-blink-start keyamp-command-color keyamp-read-color)))

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
    (add-hook 'isearch-mode-end-hook 'keyamp-command-karabiner)
    (add-hook 'minibuffer-setup-hook 'keyamp-insert-karabiner)))


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
  (keyamp-indicate-command))

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

(defun keyamp-spc-spc (&rest _)
  "Insert fast SPC SPC to activate command mode and save. Quit minibuffer."
  (if-let (((keyamp-unless-kbd-macro))
           (space ?\s)
           ((eq before-last-command-event space))
           ((eq last-command-event space)))
      (cond
       (isearch-mode
        (isearch-cancel-clean))
       ((minibufferp)
        (keyamp-minibuffer-quit))
       (t
        (delete-char (1- -1))
        (save-buffer-silent-defer)
        (keyamp-command-execute 'keyamp-escape)))
    (if (eq last-command-event space)
        (before-last-command-event space)
      (setq before-last-command-event nil))))

(add-hook 'post-self-insert-hook 'keyamp-spc-spc)
(advice-add-macro '(isearch-printing-char minibuffer-complete-word)
                  :after 'keyamp-spc-spc)

(defun keyamp-spc-del (&rest _)
  "Insert fast SPC DEL to move char forward while in insert mode."
  (if-let (((keyamp-unless-kbd-macro))
           (space ?\s)
           ((eq before-last-command-event space)))
      (keyamp-command-execute 'fchar)))

(advice-add 'delete-backward-char :after #'keyamp-spc-del)

(defun keyamp-command-if-insert (&rest _)
  "Activate command mode if insert mode."
  (if keyamp-insert-p
      (keyamp-command)))

(defun keyamp-insert-and-spc ()
  "Activate insert mode and insert space."
  (interactive)
  (unless keyamp-insert-p
    (keyamp-insert))
  (insert " "))

(defun keyamp-insert-and-self-insert ()
  "Self insert and activate insert mode."
  (interactive)
  (cond
   ((eq major-mode 'vterm-mode)
    (keyamp-insert)
    (vterm--self-insert))
   (buffer-read-only
    (keyamp-command-execute 'ignore))
   (t
    (keyamp-insert)
    (self-insert-command 1))))

(defun minibuffer-complete-around (fun &rest r)
  "Minibuffer complete by TAB applied only for file path completion, otherwise
completion forward."
  (if (eq (icomplete--category) 'file)
      (apply fun r)
    (comp-forw)))

(advice-add 'minibuffer-complete :around 'minibuffer-complete-around)

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

(defun keyamp-minibuffer-y-or-n-literal ()
  "Return t if asked literal y or n question."
  (if-let ((str (minibuffer-prompt)))
      (string-match "y, n, !\\|yn!q" str)))

(defun keyamp-minibuffer-y-or-n ()
  "Return t if asked non-literal y or n question."
  (if-let ((str (minibuffer-prompt)))
      (string-match "y or n" str)))

(defun keyamp-insert-minibuffer ()
  "Answer to y or n question if asked or answer literal y or n question if asked.
Else activate insert mode and self insert."
  (interactive)
  (let ((key (this-command-keys)))
    (if (vectorp key)
        (setq key (char-to-string (aref key 0))))
    (if-let ((key-ascii (cdr (assoc key keyamp-input-method-to-ascii)))
             ((> (string-to-char key) 127)))
        (setq key (keyamp--convert-kbd-str key-ascii)))
    (cond
     ((keyamp-minibuffer-y-or-n)
      (if (string-equal key (keyamp--convert-kbd-str "k")) ; QWERTY K
          (y-or-n-p-insert-y)
        (y-or-n-p-insert-n)))
     ((keyamp-minibuffer-y-or-n-literal)
      (keyamp-insert-init) ; no hook run for single char insert
      (if (get 'toggle-standard-to-current-layout 'state) ; convert back to current layout
          (setq key (car (rassoc key keyamp--convert-table))))
      (execute-kbd-macro (kbd key))) ; key press required
     (t
      (keyamp-insert-and-self-insert)))))

(defun keyamp-minibuffer-escape ()
  "If minibuffer input not empty then activate command mode instead
of quit minibuffer. Answer q to literal y or n question."
  (interactive)
  (if (keyamp-minibuffer-y-or-n-literal)
      (progn
        (keyamp-insert-init)
        (let ((key "q"))
          (if (get 'toggle-standard-to-current-layout 'state)
              (setq key (car (rassoc key keyamp--convert-table))))
          (execute-kbd-macro (kbd key))))
    (if (keyamp-minibuffer-empty)
        (keyamp-minibuffer-quit)
      (keyamp-escape))))

(defun keyamp-minibuffer-empty ()
  "Return true if minibuffer prompt empty."
  (and (minibufferp)
       (zerop (- (buffer-size) (length (minibuffer-prompt))))))

(defun keyamp-minibuffer-match (String)
  "Return true if minibuffer prompt match STRING."
  (string-match String (minibuffer-prompt)))

(defun keyamp-defer-command-bookmark (Fun)
  "To defer bookmark command `last-nonmenu-event' must be not list."
  (run-at-time nil nil (lambda ()
                         (let ((last-nonmenu-event t))
                           (keyamp-command-execute Fun)))))

(defun keyamp-copy-minibuffer ()
  "Kill minibuffer content to ring for reuse."
  (if-let ((str (buffer-substring (1+ (length (minibuffer-prompt))) (point-max)))
           ((cl-plusp (length str))))
      (kill-new str)))

(defun keyamp-minibuffer-shift ()
  "Quit minibuffer and call some minibuffer command. Single motion switch."
  (interactive)
  (when (keyamp-unless-kbd-macro)
    (keyamp-copy-minibuffer)
    (cond
     ((keyamp-minibuffer-match "Describe function")
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
    (abort-recursive-edit)))

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
  (unless keyamp-insert-p
    (keyamp-indicate keyamp-read-indicator keyamp-read-cursor keyamp-read-color))
  (if (and (eq last-command 'keyamp--hook-indicate)
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
            (run-with-timer (* keyamp-blink-duration 2) nil 'keyamp-indicate-read)))))

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
  (cond
   ((gethash this-command keyamp-screen-read-commands-hash)
    (keyamp-blink-start keyamp-read-color keyamp-screen-color))
   ((eq this-command 'save-close-buf)
    (keyamp-blink-start keyamp-modify-color keyamp-screen-color))
   (t (keyamp-blink-start keyamp-command-color keyamp-screen-color))))

(defun keyamp-indicate-command ()
  "Indicate command."
  (keyamp-blink-stop)
  (keyamp-indicate keyamp-command-indicator keyamp-command-cursor keyamp-command-color)
  (cond
   ((memq this-command keyamp-screen-command-commands)
    (keyamp-blink-start keyamp-screen-color keyamp-command-color))
   (t (keyamp-blink-start keyamp-accent-color keyamp-command-color))))

(defun keyamp-indicate-io (&rest _)
  "Indicate io feedback from emacsclient evals or processes calls."
  (keyamp-blink keyamp-blinker-io))

(defun keyamp-indicate-insert ()
  "Indicate insert."
  (keyamp-indicate keyamp-insert-indicator keyamp-insert-cursor keyamp-insert-color)
  (cond
   ((gethash this-command keyamp-read-commands-hash)
    (keyamp-blink-start keyamp-read-color keyamp-insert-color))
   ((memq this-command keyamp-insert-commands)
    (keyamp-blink keyamp-blinker-io))
   ((gethash this-command keyamp-modify-commands-hash)
    (keyamp-blink keyamp-blinker-modify))))

(defun keyamp-indicate-modify ()
  "Indicate modify."
  (keyamp-indicate keyamp-modify-indicator keyamp-modify-cursor keyamp-modify-color)
  (if (eq this-command 'undo) ; other repeatable modify have timeout
      (keyamp-blink-start keyamp-modify-color keyamp-read-color)))

(defvar keyamp-user-error nil
  "True if this command signaled `user-error'. See `command-error-function'.")

(defun keyamp-transient ()
  "Indicate transient. Run with `post-command-hook'."
  (if keyamp-user-error
      (progn
        (keyamp-command)
        (setq keyamp-user-error nil))
    (if (and (eq this-command 'mac-mwheel-scroll)
             (eq mode-line-front-space keyamp-command-indicator))
        (progn) ; ease scroll
      (unless (eq this-command last-command)
        (cond
         ((or keyamp-insert-p
              (and isearch-mode
                   (not (memq this-command keyamp-isearch-not-insert))))
          (keyamp-indicate-insert))
         ((eq this-command 'activate-region)
          (keyamp-indicate keyamp-command-indicator
                           keyamp-modify-cursor keyamp-command-color))
         ((gethash this-command keyamp-screen-commands-hash)
          (keyamp-indicate-screen))
         ((gethash this-command keyamp-read-commands-hash)
          (keyamp-indicate-read-defer))
         ((eq real-this-command 'repeat)
          (keyamp-blink-start keyamp-read-color keyamp-modify-color))
         ((gethash this-command keyamp-modify-commands-hash)
          (keyamp-indicate-modify))
         (t (keyamp-indicate-command))))
      (if (or defining-kbd-macro
              (memq this-command keyamp-blink-modify-commands)
              (eq major-mode 'wdired-mode))
          (keyamp-blink keyamp-blinker-modify))
      (if (eq this-command 'ignore)
          (keyamp-blink keyamp-blinker-idle))
      (when (eq this-command 'keyamp-escape)
        (keyamp-blink keyamp-blinker-idle)
        (modify-all-frames-parameters '((cursor-type . hollow))))
      (if (memq this-command keyamp-blink-io-commands)
          (keyamp-blink keyamp-blinker-io))
      (if (memq this-command keyamp-blink-command-commands)
          (keyamp-blink keyamp-blinker-command)))))

(defvar keyamp-blink-on-timer nil "Blink indicator on timer.")
(defvar keyamp-blink-off-timer nil "Blink indicator off timer.")

(defconst keyamp-blink-duration (/ 1.0 2) "Blink duration.")
(defconst keyamp-blink-period (+ keyamp-blink-duration 3) "Blink period.")

(defun keyamp-blinking (Color1 Color2)
  "Blinking."
  (keyamp-indicator-color Color1)
  (if (timerp keyamp-blink-off-timer)
      (cancel-timer keyamp-blink-off-timer))
  (setq keyamp-blink-off-timer
        (run-with-timer keyamp-blink-duration nil 'keyamp-indicator-color Color2)))

(defun keyamp-blink-stop ()
  "Stop blink."
  (remove-hook 'post-command-hook 'keyamp-blink-stop)
  (if (timerp keyamp-blink-off-timer)
      (cancel-timer keyamp-blink-off-timer))
  ;; (if (timerp keyamp-blink-on-timer)
  ;; (cancel-timer keyamp-blink-on-timer))
  ;; FIXME: not safe :(
  (mapc (lambda (timer)
          (if (eq (timer--function timer) 'keyamp-blinking)
              (cancel-timer timer)))
        timer-list))

(defun keyamp-blink-start (Color1 Color2)
  "Start blink."
  (keyamp-blink-stop)
  (add-hook 'post-command-hook 'keyamp-blink-stop)
  (setq keyamp-blink-on-timer
        (run-with-timer (* keyamp-blink-duration 2)
                        keyamp-blink-period 'keyamp-blinking Color1 Color2)))

(defclass keyamp-blinker ()
  ((indicator
    :initarg :indicator
    :documentation "Indicator.")
   (color
    :initarg :color
    :documentation "Color.")
   (duration
    :initform (eval keyamp-blink-duration)
    :initarg :duration
    :documentation "Duration.")
   (timer
    :initform nil
    :initarg :timer
    :documentation "Timer.")
   (curIndicator
    :initarg :curIndicator
    :documentation "Indicator before blink.")
   (curColor
    :initarg :curColor
    :documentation "Color before blink.")
   (curCursor
    :initarg :curCursor
    :documentation "Cursor before blink."))
  "Blinker.")

(cl-defgeneric keyamp-blink (obj) "Blink.")

(cl-defmethod keyamp-blink ((obj keyamp-blinker))
  "Blink with blinker."
  (oset obj curIndicator mode-line-front-space)
  (oset obj curColor (face-attribute 'mode-line-front-space-face :foreground))
  (oset obj curCursor (frame-parameter nil 'cursor-type))
  (unless (eq (oref obj curColor) (oref obj color))
    (keyamp-indicate (oref obj indicator) (oref obj curCursor) (eval (oref obj color)))
    (if (timerp (oref obj timer))
        (cancel-timer (oref obj timer)))
    (oset obj timer (run-with-timer (oref obj duration) nil 'keyamp-blink-end obj))))

(cl-defgeneric keyamp-blink-end (obj) "End blink.")

(cl-defmethod keyamp-blink-end ((obj keyamp-blinker))
  "End blink with blinker."
  (if (eq (oref obj indicator) mode-line-front-space)
      (keyamp-indicate (oref obj curIndicator) (oref obj curCursor) (oref obj curColor))))

(defconst keyamp-blink-idle-duration (/ keyamp-blink-duration 2) "Blink idle duration.")

(defconst keyamp-blinker-idle
  (keyamp-blinker :indicator keyamp-idle-indicator :color 'keyamp-idle-color
                  :duration keyamp-blink-idle-duration)
  "Blinker idle.")

(defconst keyamp-blinker-command
  (keyamp-blinker :indicator keyamp-command-indicator :color 'keyamp-command-color)
  "Blinker command.")

(defconst keyamp-blinker-io
  (keyamp-blinker :indicator keyamp-io-indicator :color 'keyamp-io-color)
  "Blinker io.")

(defconst keyamp-blinker-insert
  (keyamp-blinker :indicator keyamp-insert-indicator :color 'keyamp-insert-color)
  "Blinker insert.")

(defconst keyamp-blinker-modify
  (keyamp-blinker :indicator keyamp-modify-indicator :color 'keyamp-modify-color)
  "Blinker modify.")

(defconst keyamp-prefix-io
  `([?\s] [?\d] [backspace]
    [?\d ,(string-to-char (keyamp--convert-kbd-str "i"))]
    [?\d ,(string-to-char (keyamp--convert-kbd-str "j"))]
    [?\d ,(string-to-char (keyamp--convert-kbd-str "k"))]
    [backspace ,(string-to-char (keyamp--convert-kbd-str "i"))]
    [backspace ,(string-to-char (keyamp--convert-kbd-str "j"))]
    [backspace ,(string-to-char (keyamp--convert-kbd-str "k"))]
    [?\C-h]
    [,(string-to-char (keyamp--convert-kbd-str "'"))]
    [,(string-to-char (if-let ((quotemap (car (rassoc "'" keyamp-input-method-to-ascii))))
                          quotemap
                        ""))])
  "Indicate prefixes with io.")

(defconst keyamp-prefix-modify
  `([?\s ,(string-to-char (keyamp--convert-kbd-str "e"))]
    [?\s ,(string-to-char (keyamp--convert-kbd-str "d"))]
    [?\s ,(string-to-char (keyamp--convert-kbd-str "f"))])
  "Indicate prefixes with modify.")

(defconst keyamp-blink-flash 0.15 "Blink flash duration.")

(defsubst keyamp-blink-flash (Color)
  (let ((keyamp-blink-duration keyamp-blink-flash)
        (keyamp-blink-period (* 2 keyamp-blink-flash)))
    (keyamp-blink-start keyamp-io-color Color)
    (keyamp-cursor-type keyamp-modify-cursor)))

(defun keyamp-indicate-prefix ()
  "Indicate prefix."
  (cond
   ((member (this-single-command-keys) keyamp-prefix-io)
    (keyamp-blink-flash keyamp-command-color))
   ((equal (this-single-command-keys) [?\C-q])
    (keyamp-blink-flash keyamp-insert-color))
   ((or (member (this-single-command-keys) keyamp-prefix-modify)
        prefix-arg) ; C-u
    (keyamp-blink-flash keyamp-modify-color))))

(defvar keyamp-prefix-delay 0.1 "Delay before indicate prefix keymap.")

(defun keyamp-prefix ()
  "Run `keyamp-indicate-prefix' with idle timer."
  (run-with-idle-timer keyamp-prefix-delay t 'keyamp-indicate-prefix))

(defvar keyamp-idle-timer nil "Idle timer.")

(defun keyamp-idle-init ()
  "Idle init.
Cancel isearch. Deactivate region. Deactivate transient keymaps.
Cleanup echo area. Quit minibuffer. Quit wait key sequence."
  (let ((default-directory user-emacs-directory))
    (if isearch-mode
        (isearch-cancel-clean))
    (if (region-active-p)
        (deactivate-mark))
    (keyamp-command)
    (if-let ((buf (get-buffer " *Echo Area 0*"))
             ((cl-plusp (buffer-size buf))))
        (run-at-time nil nil 'message nil))
    (if (minibufferp)
        (keyamp-minibuffer-quit))
    (keyboard-quit)))

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
  (interactive)
  (run-at-time nil nil
               (lambda ()
                 (mapc
                  (lambda (fun)
                    (funcall fun))
                  keyamp-minibuffer-quit-funs)))
  (abort-recursive-edit))

(defun keyamp-escape ()
  "Return to clear selection, command mode or quit minibuffer."
  (interactive)
  (cond
   ((region-active-p)                    (deactivate-region))
   ((or keyamp-repeat-p keyamp-insert-p) (keyamp-command))
   ((minibufferp)                        (keyamp-minibuffer-quit))
   ((cl-plusp (recursion-depth))         (exit-recursive-edit))
   (t                                    (keyamp-command))))

(define-minor-mode keyamp
  "Keyboard Amplifier."
  :global t
  :keymap keyamp-map
  (when keyamp
    (keyamp-command)
    (keyamp-catch-tty-ESC)
    (keyamp-idle-detect)
    (keyamp-prefix)
    (keyamp-karabiner-init)
    (add-hook 'post-command-hook     'keyamp-transient)
    (add-hook 'pre-command-hook      'keyamp-cancel-repeat-idle-timer)
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
