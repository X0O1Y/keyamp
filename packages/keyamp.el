;;; keyamp.el --- Keyboard Amplifier -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Egor Maltsev <X0O1@YA.RU>
;; Version: 1.5 2026-05-09 Touchscreen
;;
;; IDE workflow with standard virtual keyboard

;;; Commentary:

;; KEYAMP provides 3 modes: insert, command and repeat. Command mode
;; is based on persistent transient keymap. Repeat mode adds transient
;; remaps on top of command mode for easy repetition of command chains
;; during screen positioning, cursor move and editing. Mode line front
;; space color indicates the active transient keymap. Repeat mode is
;; switched automatically by advice or timer.

;; Plug Keyboard Amplifier in:
;;
;; (require 'keyamp)
;; (keyamp)

;;; Code:



(require 'eieio)
(require 'quail)

(require 'keycom)



(defgroup keyamp nil "Customization options for keyamp."
  :group 'help :prefix "keyamp-")

(defvar keyamp-cur-layout "qwerty"
  "Keyamp current layout. Set non-standard layout before keyamp load.")

(defvar keyamp-touchp nil "Touchscreen predicate.")


;; Quail

(when (string-equal keyamp-cur-layout "engineer-engram")
  (quail-define-package
   "russian-computer" "Russian" "RU" nil
   "ЙЦУКЕН Russian computer layout for Engineer Engram compatibility."
   nil t t t t nil nil nil nil nil t)

  (quail-define-rules
   ("1" ?1) ("2" ?2) ("3" ?3) ("4" ?4)  ("5" ?5) ("6" ?6)   ("7" ?7) ("8" ?8)
   ("9" ?9) ("0" ?0) ("-" ?b) ("=" ?*)  ("|" ?#) ("`" ?ё)   ("q" ?й) ("w" ?ц)
   ("e" ?у) ("r" ?к) ("t" ?е) ("y" ?н)  ("u" ?г) ("i" ?ш)   ("o" ?щ) ("p" ?з)
   ("[" ?х) ("]" ?ъ) ("a" ?ф) ("s" ?ы)  ("d" ?в) ("f" ?а)   ("g" ?п) ("h" ?р)
   ("j" ?о) ("k" ?л) ("l" ?д) (";" ?ж)  ("'" ?э) ("\\" ?\)) ("z" ?я) ("x" ?ч)
   ("c" ?с) ("v" ?м) ("b" ?и) ("n" ?т)  ("m" ?ь) ("," ?б)   ("." ?ю) ("/" ?h)
   ("!" ?N) ("@" ?y) ("#" ?№) ("$" ?G)  ("%" ?=) ("^" ?H)   ("&" ?n) ("*" ?&)
   ("(" ?T) (")" ?Y) ("_" ?B) ("+" ?\() ("~" ?Ё) ("Q" ?Й)   ("W" ?Ц) ("E" ?У)
   ("R" ?К) ("T" ?Е) ("Y" ?Н) ("U" ?Г)  ("I" ?Ш) ("O" ?Щ)   ("P" ?З) ("{" ?Х)
   ("}" ?Ъ) ("A" ?Ф) ("S" ?Ы) ("D" ?В)  ("F" ?А) ("G" ?П)   ("H" ?Р) ("J" ?О)
   ("K" ?Л) ("L" ?Д) (":" ?Ж) ("\"" ?Э) ("|" ?|) ("Z" ?Я)   ("X" ?Ч) ("C" ?С)
   ("V" ?М) ("B" ?И) ("N" ?Т) ("M" ?Ь)  ("<" ?Б) (">" ?Ю)   ("?" ?g)))

(defconst keyamp-input-methods '(russian-computer hebrew)
  "Input methods, activate when not available otherwise. See
`toggle-primary-input-method', first one is primary.

On activation Quail package also loaded which required for mapping
translation of corresponding non-ASCII command key sequences and
mapping to non-QWERTY layouts. Mappings defined in QWERTY notation
throughout the code of the package.")

(eval-and-compile
  (defvar keyamp-input-methods-to-std
    '(("ё" . "`") ("й" . "q") ("ц" . "w")  ("у" . "e") ("к" . "r")
      ("е" . "t") ("н" . "y") ("г" . "u")  ("ш" . "i") ("щ" . "o")
      ("з" . "p") ("х" . "[") ("ъ" . "]")  ("ф" . "a") ("ы" . "s")
      ("в" . "d") ("а" . "f") ("п" . "g")  ("р" . "h") ("о" . "j")
      ("л" . "k") ("д" . "l") ("ж" . ";")  ("э" . "'") ("я" . "z")
      ("ч" . "x") ("с" . "c") ("м" . "v")  ("и" . "b") ("т" . "n")
      ("ь" . "m") ("б" . ",") ("ю" . ".")  ("№" . "#") ("Ё" . "~")
      ("Й" . "Q") ("Ц" . "W") ("У" . "E")  ("К" . "R") ("Е" . "T")
      ("Н" . "Y") ("Г" . "U") ("Ш" . "I")  ("Щ" . "O") ("З" . "P")
      ("Х" . "{") ("Ъ" . "}") ("Ф" . "A")  ("Ы" . "S") ("В" . "D")
      ("А" . "F") ("П" . "G") ("Р" . "H")  ("О" . "J") ("Л" . "K")
      ("Д" . "L") ("Ж" . ":") ("Э" . "\"") ("Я" . "Z") ("Ч" . "X")
      ("С" . "C") ("М" . "V") ("И" . "B")  ("Т" . "N") ("Ь" . "M")
      ("Б" . "<") ("Ю" . ">")

      ("ק" . "e") ("ר" . "r") ("א" . "t") ("ט" . "y") ("ו" . "u")
      ("ן" . "i") ("ם" . "o") ("פ" . "p") ("ש" . "a") ("ד" . "s")
      ("ג" . "d") ("כ" . "f") ("ע" . "g") ("י" . "h") ("ח" . "j")
      ("ל" . "k") ("ך" . "l") ("ף" . ";") ("ז" . "z") ("ס" . "x")
      ("ב" . "c") ("ה" . "v") ("נ" . "b") ("מ" . "n") ("צ" . "m")
      ("ת" . ",") ("ץ" . "."))
    "Input methods to standard keyboard (QWERTY layout) ASCII char.
  Primary method pairs come first. Keep data in code for compiled package
  since the list is required for `keyamp--map' macro expansion.
  Eval `keyamp-input-methods-to-std' to recreate the list."))

(defun keyamp-input-methods-to-std ()
  "Recreate `keyamp-input-methods-to-std'."
  (setq keyamp-input-methods-to-std nil)
  (mapc
   (lambda (method)
     (activate-input-method method)
     (mapc
      (lambda (map)
        (when-let ((to (char-to-string (car map)))
                   (from (quail-get-translation (cadr map) to 1))
                   ((characterp from))
                   ((> from (1- (expt 2 7))))
                   (from (char-to-string from)))
          (push (cons from to) keyamp-input-methods-to-std)))
      (cdr (quail-map)))
     (activate-input-method nil))
   (reverse keyamp-input-methods)))

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
        (when-let ((to (keyamp--convert-kbd-str (char-to-string (car map))))
                   (from (quail-get-translation (cadr map) to 1))
                   (to (string-to-char to))
                   ((characterp from))
                   ((> from (1- (expt 2 7)))))
          (mapc
           (lambda (modifier)
             (keymap-set local-function-key-map
                         (key-description (vector (append modifier (list from))))
                         (vector (append modifier (list to)))))
           '(nil (control)))))
      (cdr (quail-map))))
   keyamp-input-methods)
  (activate-input-method nil))

(defun toggle-primary-input-method ()
  "Toggle primary input method command.
Activate when input method not available in OS. Or use together with
`keyamp-cur-layout' on OS level and `toggle-input-method-key'."
  (interactive)
  (when-let ((method (car keyamp-input-methods)))
    (activate-input-method (if current-input-method nil method))
    (when (display-graphic-p) ; Use keyamp-cur-layout on OS level
      (quail-set-keyboard-layout
       (if current-input-method
           keyamp-cur-layout
         "standard")))
    (if current-input-method
        (progn
          (add-hook 'keyamp-insert-hook 'map-toggle-input-method)
          (add-hook 'keyamp-command-hook 'map-toggle-input-method))
      (remove-hook 'keyamp-insert-hook 'map-toggle-input-method)
      (remove-hook 'keyamp-command-hook 'map-toggle-input-method))
    (message (capitalize
              (replace-regexp-in-string
               "-" " "
               (format "%s" (if current-input-method method keyamp-cur-layout)))))))

(defvar toggle-input-method-key "`"
  "The key toggles primary input method in insert mode with standard
keyboard. E.g. use standard keyboard via bluetooth for terminal Emacs with
non-QWERTY ASCII layout and toggle non-ASCII input method with the key.")

(defun map-toggle-input-method ()
  "Toggle map `toggle-primary-input-method' to `toggle-input-method-key' and
corresponding non-ASCII key for primary input method in insert mode."
  (let ((cmd (when keyamp-insert-p 'toggle-primary-input-method))
        (input-method-key
         (car (rassoc toggle-input-method-key keyamp-input-methods-to-std))))
    (keymap-set keyamp-map toggle-input-method-key cmd)
    (keymap-set keyamp-map input-method-key cmd)))

(defconst toggle-std-to-cur-layout-silent t
  "Toggle standard keyboard without message.")

(defun toggle-std-to-cur-layout-keymap (&optional Set)
  "Toggle standard keyboard routine.
Modify `key-translation-map', set when SET otherwise unset."
  (mapc
   (lambda (pair)
     (keymap-set key-translation-map (car pair) (when Set (cdr pair))))
   keyamp--convert-table))

(defun toggle-std-to-cur-layout ()
  "Toggle translation standard keyboard to `keyamp-cur-layout' command.
Activate when `keyamp-cur-layout' not available in OS. The layout must
present in `quail-keyboard-layout-alist'."
  (interactive)
  (unless (string-equal keyamp-cur-layout "qwerty")
    (when (get 'toggle-hand-swap 'state)
      (user-error "Hand swap is active"))
    (if (get 'toggle-std-to-cur-layout 'state)
        (progn
          (quail-set-keyboard-layout "standard")
          (put 'toggle-std-to-cur-layout 'state nil)
          (remove-hook 'keyamp-insert-hook 'map-toggle-input-method)
          (remove-hook 'keyamp-command-hook 'map-toggle-input-method)
          (activate-input-method nil)
          (keyamp-use-indicators "default")
          (when (and (not toggle-std-to-cur-layout-silent)
                     (eq this-command 'toggle-std-to-cur-layout))
            (message "Deactivated standard keyboard")))
      (if (assoc keyamp-cur-layout quail-keyboard-layout-alist)
          (quail-set-keyboard-layout keyamp-cur-layout)
        (user-error "Unable to activate standard keyboard to %s" keyamp-cur-layout))
      (put 'toggle-std-to-cur-layout 'state t)
      (add-hook 'keyamp-insert-hook 'map-toggle-input-method)
      (add-hook 'keyamp-command-hook 'map-toggle-input-method)
      (keyamp-use-indicators "standard")
      (when (and (not toggle-std-to-cur-layout-silent)
                 (eq this-command 'toggle-std-to-cur-layout))
        (message "Activated standard keyboard")))
    (toggle-std-to-cur-layout-keymap (get 'toggle-std-to-cur-layout 'state))
    (keyamp-indicator keyamp-command-indicator)))

(defconst keyamp--hand-swap
  `(("SPC" . ,(if (display-graphic-p) "<backspace>" "DEL"))
    ("DEL" . ,(unless (display-graphic-p) "SPC"))
    ("<backspace>" . ,(when (display-graphic-p) "SPC"))
    ("S-SPC" . ,(when (display-graphic-p) "<backtab>"))
    ("S-<backspace>" . ,(when (display-graphic-p) "<tab>"))
    ("TAB" . ,(when (and (not (display-graphic-p))
                         (not keyamp-touchp)) "<backtab>"))
    ("<backtab>" . ,(unless (display-graphic-p) "TAB"))

    ("C-^" . "C-_") ("C-+" . "C-И")
    ("C-_" . "C-^") ("C-И" . "C-+")

    ("<prior>" .  ,(unless keyamp-touchp "<next>"))
    ("<end>"   .  ,(unless keyamp-touchp "<home>"))
    ("<next>"  .  ,(unless keyamp-touchp "<prior>"))
    ("<home>"  .  ,(unless keyamp-touchp "<end>"))

    ;; Standard (QWERTY)
    ("1" . "0")  ("2" . "9")  ("3" . "8") ("4" . "7")  ("5" . "6")
    ("0" . "1")  ("9" . "2")  ("8" . "3") ("7" . "4")  ("6" . "5")
    ("-" . "=")  ("=" . "-")  ("`" . "`") ("]" . "\\") ("\\" . "]")
    ("'" . "[")  ("[" . "'")
    ("q" . "p")  ("w" . "u")  ("e" . "i") ("r" . "o")  ("t" . "y")
    ("a" . "h")  ("s" . "j")  ("d" . "k") ("f" . "l")  ("g" . ";")
    ("z" . "/")  ("x" . ".")  ("c" . ",") ("v" . "m")  ("b" . "n")
    ("y" . "t")  ("u" . "r")  ("i" . "e") ("o" . "w")  ("p" . "q")
    ("h" . "g")  ("j" . "s")  ("k" . "d") ("l" . "f")  (";" . "a")
    ("n" . "b")  ("m" . "v")  ("," . "c") ("." . "x")  ("/" . "z")

    ("!" . ")")  ("@" . "(")  ("#" . "*") ("$" . "&")  ("%" . "^")
    (")" . "!")  ("(" . "@")  ("*" . "#") ("&" . "$")  ("^" . "%")
    ("_" . "+")  ("+" . "_")  ("~" . "~") ("}" . "|")  ("|" . "}")
    ("\"" . "{") ("{" . "\"")
    ("Q" . "P")  ("W" . "U")  ("E" . "I") ("R" . "O")  ("T" . "Y")
    ("A" . "H")  ("S" . "J")  ("D" . "K") ("F" . "L")  ("G" . ":")
    ("Z" . "?")  ("X" . ">")  ("C" . "<") ("V" . "M")  ("B" . "N")
    ("Y" . "T")  ("U" . "W")  ("I" . "E") ("O" . "R")  ("P" . "Q")
    ("H" . "G")  ("J" . "S")  ("K" . "D") ("L" . "F")  (":" . "A")
    ("N" . "B")  ("M" . "V")  ("<" . "C") (">" . "X")  ("?" . "Z")

    ;; Russian-computer (non-ASCII pairs only)
    ("ё" . "ё")  ("э" . "х")  ("х" . "э")
    ("й" . "з")  ("ц" . "г")  ("у" . "ш") ("к" . "щ")  ("е" . "н")
    ("ф" . "р")  ("ы" . "о")  ("в" . "л") ("а" . "д")  ("п" . "ж")
                 ("ч" . "ю")  ("с" . "б") ("м" . "ь")  ("и" . "т")
    ("н" . "е")  ("г" . "к")  ("ш" . "у") ("щ" . "ц")  ("з" . "й")
    ("р" . "п")  ("о" . "ы")  ("л" . "в") ("д" . "а")  ("ж" . "ф")
    ("т" . "и")  ("ь" . "м")  ("б" . "с") ("ю" . "ч")

    ("Ё" . "Ё")  ("Э" . "Х")  ("Х" . "Э")
    ("Й" . "З")  ("Ц" . "Г")  ("У" . "Ш") ("К" . "Щ")  ("Е" . "Н")
    ("Ф" . "Р")  ("Ы" . "О")  ("В" . "Л") ("А" . "Д")  ("П" . "Ж")
                 ("Ч" . "Ю")  ("С" . "Б") ("М" . "Ь")  ("И" . "Т")
    ("Н" . "Е")  ("Г" . "Ц")  ("Ш" . "У") ("Щ" . "К")  ("З" . "Й")
    ("Р" . "П")  ("О" . "А")  ("Л" . "В") ("Д" . "Ы")  ("Ж" . "Ф")
    ("Т" . "И")  ("Ь" . "М")  ("Б" . "С") ("Ю" . "Ч"))
  "Alist for hand-swapping keys.")

(defconst keyamp--hand-swap-direction
  '(("w" . "r") ("s" . "f") ("m" . "."))
  "Alist for hand-swapping keys pairs which must swap direction back.
E.g. \"s\" swap with \"f\" and vise versa because horizontal must stay same.
Keep only left to right pair in the alist, remap right to left as well.")

(defconst keyamp--hand-swap-suffix "-hand-swap" "Hand swap suffix for naming.")

(defun keyamp--hand-swap-direction-advice (&optional Remove)
  "Advice add for hand swap direction or remove if REMOVE."
  (mapc
   (lambda (pair)
     (mapc
      (lambda (char)
        (when-let ((cmd (lookup-key keyamp-command-map (keyamp--convert-kbd-str char)))
                   (cmd-swap (intern (concat (symbol-name cmd) keyamp--hand-swap-suffix))))
          (if Remove
              (advice-remove cmd cmd-swap)
            (advice-add cmd :override cmd-swap))))
      (list (car pair) (cdr pair))))
   keyamp--hand-swap-direction))

(defun keyamp--hand-swap-direction-defun (Pair)
  "Generate defun for hand swap direction advice."
  (when-let ((cmd (lookup-key keyamp-command-map (keyamp--convert-kbd-str (car Pair))))
             (cmd-swap (lookup-key keyamp-command-map (keyamp--convert-kbd-str (cdr Pair)))))
    (eval
     `(defun ,(intern (concat (symbol-name cmd-swap) keyamp--hand-swap-suffix)) ()
        ,(format "Hand swap direction swap back for `%s'." cmd-swap)
        (interactive)
        (keyamp--hand-swap-direction-advice :remove)
        (unwind-protect
            (keyamp-command-execute ',cmd)
          (keyamp--hand-swap-direction-advice))))))

(defun toggle-hand-swap-keymap (&optional Set)
  "Toggle hand swap routine.
Modify `key-translation-map', set when SET otherwise unset."
  (mapc
   (lambda (pair)
     (keymap-set key-translation-map
                 (keyamp--convert-kbd-str (car pair))
                 (when-let ((Set)
                            (char (cdr pair)))
                   (keyamp--convert-kbd-str char))))
   keyamp--hand-swap))

(defconst keyamp--hand-swap-direction-prefix '(("s" . "f") ("j" . "l"))
  "Alist for hand-swapping keys pairs which must spin before read prefix
command sequence.")

(defun keyamp--hand-swap-direction-prefix-off ()
  "See `keyamp--hand-swap-direction-prefix-on'."
  (remove-hook 'post-command-hook 'keyamp--hand-swap-direction-prefix-off)
  (when (get 'toggle-hand-swap 'state)
    (mapc
     (lambda (pair) ; Restore input source hand swap
       (when (> (string-to-char (car pair)) (1- (expt 2 7)))
         (keymap-set key-translation-map
                     (keyamp--convert-kbd-str (car pair))
                     (when-let ((char (cdr pair)))
                       (keyamp--convert-kbd-str char)))))
     keyamp--hand-swap)
    (mapc
     (lambda (pair)
       (keymap-set key-translation-map
                   (keyamp--convert-kbd-str (cdr (assoc (cdr pair) keyamp--hand-swap)))
                   (keyamp--convert-kbd-str (cdr pair)))
       (keymap-set key-translation-map
                   (keyamp--convert-kbd-str (cdr (assoc (car pair) keyamp--hand-swap)))
                   (keyamp--convert-kbd-str (car pair)))
       (keymap-set key-translation-map
                   (car (rassoc (cdr (assoc (cdr pair) keyamp--hand-swap))
                                keyamp-input-methods-to-std))
                   (car (rassoc (cdr pair) keyamp-input-methods-to-std)))
       (keymap-set key-translation-map
                   (car (rassoc (cdr (assoc (car pair) keyamp--hand-swap))
                                keyamp-input-methods-to-std))
                   (car (rassoc (car pair) keyamp-input-methods-to-std))))
     keyamp--hand-swap-direction-prefix)))

(defun keyamp--hand-swap-direction-prefix-on ()
  "Spin for hand swap direction. Prefix sequences must mirror while direction
mirror then spin. E.g. s->j direction but s->l prefix. Corner case.
Input source key to std with hand swap back to read prefix sequence from input
source. Not possible double remap with `local-function-key-map'."
  (when (or (member (this-single-command-keys) keyamp-prefix-io)
            (member (this-single-command-keys) keyamp-prefix-modify))
    (mapc
     (lambda (pair) ; Input source to std with hand swap back
       (when (> (string-to-char (car pair)) (1- (expt 2 7)))
         (keymap-set key-translation-map
                     (car pair)
                     (keyamp--convert-kbd-str
                      (cdr
                       (assoc
                        (cdr
                         (assoc
                          (car pair) keyamp-input-methods-to-std))
                                  keyamp--hand-swap))))))
     keyamp--hand-swap)
    (mapc
     (lambda (pair)
       (keymap-set key-translation-map
                   (keyamp--convert-kbd-str (cdr (assoc (cdr pair) keyamp--hand-swap)))
                   (keyamp--convert-kbd-str (car pair)))
       (keymap-set key-translation-map
                   (keyamp--convert-kbd-str (cdr (assoc (car pair) keyamp--hand-swap)))
                   (keyamp--convert-kbd-str (cdr pair)))
       (keymap-set key-translation-map
                   (car (rassoc (cdr (assoc (cdr pair) keyamp--hand-swap))
                                keyamp-input-methods-to-std))
                   (keyamp--convert-kbd-str (car pair)))
       (keymap-set key-translation-map
                   (car (rassoc (cdr (assoc (car pair) keyamp--hand-swap))
                                keyamp-input-methods-to-std))
                   (keyamp--convert-kbd-str (cdr pair))))
     keyamp--hand-swap-direction-prefix)
    (add-hook 'post-command-hook 'keyamp--hand-swap-direction-prefix-off)))

(defun hand-swap-activate ()
  "Activate hand swap."
  (when keyamp-karabinerp
    (keyamp-set-var-karabiner keyamp-karabiner-hand-swap "1"))
  (unless (fboundp ; Once
           (intern
            (concat
             (symbol-name
              (lookup-key keyamp-command-map
                          (keyamp--convert-kbd-str
                           (caar keyamp--hand-swap-direction))))
             keyamp--hand-swap-suffix)))
    (mapc
     (lambda (pair)
       (keyamp--hand-swap-direction-defun pair)
       (keyamp--hand-swap-direction-defun (cons (cdr pair) (car pair))))
     keyamp--hand-swap-direction))
  (if (get 'toggle-std-to-cur-layout 'state) ; Support standard keyboard
      (progn
        (toggle-std-to-cur-layout-keymap)
        (mapc
         (lambda (pair)
           (keymap-set key-translation-map
                       (car pair)
                       (keyamp--convert-kbd-str
                        (cdr
                         (assoc
                          (car (rassoc (cdr pair) keyamp--convert-table))
                          keyamp--hand-swap)))))
         keyamp--convert-table)
        (mapc
         (lambda (pair)
           (keymap-set key-translation-map
                       (car pair)
                       (when-let ((char (cdr pair)))
                         (keyamp--convert-kbd-str char))))
         keyamp--hand-swap))
    (toggle-hand-swap-keymap t))
  (keyamp--hand-swap-direction-advice)
  (when (and (advice-member-p 'keyamp-virtual-lleader 'back-word)
             (advice-member-p 'keyamp-virtual-rleader 'forw-word))
    (advice-remove 'back-word 'keyamp-virtual-lleader)
    (advice-remove 'forw-word 'keyamp-virtual-rleader)
    (advice-add 'back-word :after 'keyamp-virtual-rleader)
    (advice-add 'forw-word :after 'keyamp-virtual-lleader))
  (advice-add 'keyamp-indicate-prefix :after 'keyamp--hand-swap-direction-prefix-on)
  (when (fboundp 'set-cursor-face-hand-swap)
    (set-cursor-face-hand-swap)
    (add-hook 'after-make-frame-functions 'set-cursor-face-hand-swap 90))
  (keyamp-use-indicators "hand-swap"))

(defun hand-swap-deactivate ()
  "Deactivate hand swap."
  (when keyamp-karabinerp
    (keyamp-set-var-karabiner keyamp-karabiner-hand-swap "0"))
  (if (get 'toggle-std-to-cur-layout 'state)
      (progn
        (toggle-hand-swap-keymap)
        (toggle-std-to-cur-layout-keymap)
        (toggle-std-to-cur-layout-keymap t))
    (toggle-hand-swap-keymap))
  (keyamp-key-translation) ; Restore
  (keyamp--hand-swap-direction-advice :remove)
  (when (and (advice-member-p 'keyamp-virtual-rleader 'back-word)
             (advice-member-p 'keyamp-virtual-lleader 'forw-word))
    (advice-remove 'back-word 'keyamp-virtual-rleader)
    (advice-remove 'forw-word 'keyamp-virtual-lleader)
    (advice-add 'back-word :after 'keyamp-virtual-lleader)
    (advice-add 'forw-word :after 'keyamp-virtual-rleader))
  (advice-remove 'keyamp-indicate-prefix 'keyamp--hand-swap-direction-prefix-on)
  (when (fboundp 'set-cursor-face)
    (set-cursor-face)
    (remove-hook 'after-make-frame-functions 'set-cursor-face-hand-swap))
  (keyamp-use-indicators
   (if (get 'toggle-std-to-cur-layout 'state)
       "standard"
     "default")))

(defconst toggle-hand-swap-silent t "Toggle hand swap without message.")

(defun toggle-hand-swap ()
  "Toggle hand swap with scripting leaders and Russian support.
Insert mode not affected."
  (interactive)
  (cond
   (keyamp-insert-p
    (user-error "Insert mode is active"))
   (isearch-mode
    (user-error "Isearch is active")))
  (if (get 'toggle-hand-swap 'state)
      (progn
        (put 'toggle-hand-swap 'state nil)
        (hand-swap-deactivate)
        (remove-hook 'keyamp-insert-hook    'hand-swap-deactivate)
        (remove-hook 'keyamp-command-hook   'hand-swap-activate)
        (remove-hook 'kill-emacs-hook       'hand-swap-deactivate)
        (remove-hook 'isearch-mode-hook     'hand-swap-deactivate)
        (remove-hook 'isearch-mode-end-hook 'hand-swap-activate)
        (unless toggle-hand-swap-silent
          (message "Deactivated hand swap")))
    (put 'toggle-hand-swap 'state t)
    (hand-swap-activate)
    (add-hook 'keyamp-insert-hook    'hand-swap-deactivate)
    (add-hook 'keyamp-command-hook   'hand-swap-activate)
    (add-hook 'kill-emacs-hook       'hand-swap-deactivate)
    (add-hook 'isearch-mode-hook     'hand-swap-deactivate)
    (add-hook 'isearch-mode-end-hook 'hand-swap-activate)
    (unless toggle-hand-swap-silent
      (message "Activated hand swap"))))


;; Macros

(defvar keyamp-layouts '(("qwerty" . nil))
  "A alist. Key is layout name, string type.
Value is an alist, each element is of the form (\"e\" . \"d\").
First char is QWERTY, second is corresponding char of the destination layout.
When a char is not in this alist, they are assumed to be the same.")

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

(defvar keyamp--convert-table (cdr (assoc keyamp-cur-layout keyamp-layouts))
  "A alist that's the conversion table from QWERTY to current layout.
Value structure is one of the key's value of `keyamp-layouts'.
Value is programmatically set from value of `keyamp-cur-layout'.
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
      (or (cdr (assoc char keyamp--convert-table)) char))
    (split-string CharStr " +"))
   " "))

(eval-and-compile
  (defun keyamp--std-to-non-ascii-list (Key)
    "Standard keyboard ASCII char to list of input methods keys."
    (mapcar
     #'car
     (seq-filter
      (lambda (pair)
        (string-equal (cdr pair) Key))
      keyamp-input-methods-to-std))))

(defmacro keyamp--map (KeymapName KeyCmdAlist)
  "Map `keymap-set' over a alist KEYCMDALIST. Map input methods keys too."
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
             (when (> (string-to-char non-ascii) (1- (expt 2 7)))
               `((keymap-set ,KeymapName ,non-ascii ',(cdr pair)))))
           (keyamp--std-to-non-ascii-list (car pair))))
        (cadr KeyCmdAlist))))

(defun keyamp--map-std (KeymapName Cmd)
  "Map `keymap-set' over each standard keyboard ASCII char to CMD.
Map `keymap-set' over each corresponding non-ASCII input method char to CMD."
  (mapcar
   (lambda (char)
     (let ((charStr (char-to-string char)))
       (keymap-set KeymapName charStr Cmd)
       (mapc
        (lambda (non-ascii)
          (when (> (string-to-char non-ascii) (1- (expt 2 7)))
            (keymap-set KeymapName non-ascii Cmd)))
        (keyamp--std-to-non-ascii-list charStr))))
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

(defmacro keyamp--set (KeymapName CmdList &optional
                                  CommandMode InsertMode How Timeout On-enter On-exit)
  "Map `set-transient-map' using `advice-add' over a list CMDLIST.

Advice default HOW :after might be changed by specific HOW. Activate
COMMANDMODE or INSERTMODE mode optionally. Deactivate repeat mode
after idle for TIMEOUT seconds. Ignore the advice when defining or
executing kbd macro.
Optional arg ON-EXIT, if non-nil, specifies a function that is
called, with no arguments, after MAP is deactivated."
  (declare (indent defun))
  `(progn
     ,@(mapcar
        (lambda (cmd)
          `(advice-add ,(list 'quote cmd) (if ,How ,How :after)
                       (lambda (&rest _) "auto repeat"
                         (when (and (keyamp-unless-kbd-macro)
                                    (or (eq real-this-command 'repeat)
                                        (eq this-command 'kill-region) ; Exception
                                        (eq this-command ,(list 'quote cmd))))
                           (when (and ,CommandMode
                                      keyamp-insert-p)
                             (keyamp-command))
                           (keyamp-repeat-init ,KeymapName ,On-enter ,On-exit)
                           (keyamp-cancel-repeat-idle-timer)
                           (when (and ,Timeout
                                      (not keyamp-insert-p))
                             (setq keyamp--repeat-idle-timer
                                   (run-with-idle-timer ,Timeout nil 'keyamp-command)))
                           (when ,InsertMode
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
                         (when (and ,CommandMode
                                    keyamp-insert-p)
                           (keyamp-command))
                         (when (and ,InsertMode
                                    (not keyamp-insert-p))
                           (keyamp-insert))
                         (keyamp-repeat-init ,KeymapName)
                         (when ,RepeatMode
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
  (when (or (null (current-idle-time))
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
  `(when (display-graphic-p)
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


;; Prefix

(defvar keyamp-toggle-which-key-timer nil "Timer for `keyamp-toggle-which-key'.")

(defun keyamp-toggle-which-key (Delay Echo &optional Prefix)
  "Toggle which key mode to temporary change `which-key-idle-delay' to DELAY."
  (when which-key-mode
    (which-key-mode -1)
    (setq which-key-idle-delay Delay)
    (setq which-key-show-prefix Prefix)
    (setq echo-keystrokes Echo)
    (which-key-mode)
    (setq keyamp-toggle-which-key-timer nil)))

(defun keyamp-touch-prefix (Key)
  "Simulate pressing KEY so that the next real key goes to its keymap. Make
which key show help right away. Defer restore defaults with idle timer."
  (let ((default-delay which-key-idle-delay)
        (default-prefix which-key-show-prefix)
        (default-echo echo-keystrokes))
    (unless (timerp keyamp-toggle-which-key-timer)
      (keyamp-toggle-which-key 0.1 0))
    (setq unread-command-events
          (cons (cons 'no-record (aref (kbd Key) 0)) unread-command-events))
    (unless (timerp keyamp-toggle-which-key-timer)
      (setq keyamp-toggle-which-key-timer
            (run-with-idle-timer 4 nil 'keyamp-toggle-which-key
                                 default-delay default-echo default-prefix)))))


;; Double press

(defconst keyamp-double-press-timeout (/ 300 1000.0) "Double key press timeout.")
(defvar keyamp-double-press-timer nil "Double key press timer.")

(defun keyamp-double-press (Cmd)
  "Execute COMMAND after second command call during `keyamp-double-press-timeout'."
  (if (and (timerp keyamp-double-press-timer)
           (eq this-command last-command)
           (keyamp-unless-kbd-macro))
      (progn
        (setq keyamp-double-press-timer nil)
        (keyamp-command-execute Cmd))
    (setq keyamp-double-press-timer
          (run-with-timer keyamp-double-press-timeout nil
                          (lambda () (setq keyamp-double-press-timer nil))))))

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

(defun keyamp-defer-command (&optional Cmd Defer)
  "Delay execution of CMD for DEFER seconds."
  (when (and (keyamp-unless-kbd-macro)
             Cmd)
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
    (when (memq last-command triple-press-direction-commands-list)
      (before-last-command))
    (keyamp-defer-command fun keyamp-key-repeat-delay)))


;; Terminal ESC to <escape>

(defconst keyamp-tty-seq-timeout (/ 30 1000.0)
  "Timeout to wait key sequence after ESC sent in tty.")

(defun keyamp-tty-ESC-filter (map)
  "Map last ESC key from this single command keys to <escape>.
Prefix sequence may contain last key ESC."
  (if-let ((tty-seq (this-single-command-keys))
           ((= ?\e (aref tty-seq (1- (length tty-seq)))))
           ((or (or defining-kbd-macro
                    executing-kbd-macro)
                (sit-for keyamp-tty-seq-timeout))))
      [escape]
    map))

(defun keyamp-lookup-key (map key)
  (catch 'found
    (map-keymap (lambda (k b) (when (equal key k) (throw 'found b))) map)))

(defun keyamp-catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into <escape>."
  (when-let (((memq (terminal-live-p (frame-terminal)) '(t pc)))
             (esc-binding (keyamp-lookup-key input-decode-map ?\e))
             (esc `(menu-item "" ,esc-binding :filter keyamp-tty-ESC-filter)))
    (keymap-set input-decode-map "ESC" esc)
    (keymap-set key-translation-map "ESC" "<escape>")))


;; Bracketed paste

(defun keyamp-xterm-translate-bracketed-paste (_prompt)
  "Translate single-character bracketed paste back to a normal key event.

Some Android terminal keyboards, notably Termius with Cyrillic IME input,
send each typed non-ASCII character as an xterm bracketed paste sequence:

  ESC [ 200 ~
  UTF-8 character bytes
  ESC [ 201 ~

Emacs normally translates that sequence in `input-decode-map' into one
`xterm-paste' event, so command-mode key handling sees `<xterm-paste>'
instead of the actual character and keyamp cannot process the key through
its normal input-method/layout translation path.

This translator runs at the same level as Emacs'
`xterm-translate-bracketed-paste'.  It reads the bracketed payload with
`xterm--pasted-text'.  If the payload is exactly one character, return it
as a normal input event vector, so keyamp receives the original character.
If the payload contains more than one character, keep the standard Emacs
paste behavior by returning an `xterm-paste' event."
  (let ((s (xterm--pasted-text)))
    (if (= (length s) 1)
        (vconcat (string-to-list s))
      (vector (list 'xterm-paste s)))))

(unless (display-graphic-p)
  (define-key input-decode-map "\e[200~" #'keyamp-xterm-translate-bracketed-paste))


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
(define-prefix-command 'keyamp-script-leader-map)

(defun keyamp-ignore (&rest _)
  "Ignore ARGUMENTS, do nothing, and return nil.
Huge amount of bindings from `keyamp-script-leader-map' goes here."
  (interactive)
  nil)

;; Leader prefix ignores scripting leader
(keyamp--map-std keyamp-script-leader-map 'keyamp-ignore)

(keyamp--map-escape keyamp-map keyamp-escape)
(keyamp--map keyamp-map
  '(;; Control sequences as leader prefixes for scripting
    ("C-^" . keyamp-lleader-map)           ("C-_" . keyamp-rleader-map)
    ("C-+" . keyamp-lleader-map)           ("C-И" . keyamp-rleader-map) ; Russian-computer
                                           ("C-b" . keyamp-rleader-map) ; Hebrew (experimental)

    ("C-q" . quoted-insert-custom)         ("C-S-q" . quoted-insert)       ; Escape hold down
    ("C-t" . hippie-expand)                ("C-S-t" . hippie-expand-reset) ; Return hold down

    ("<home>"   . ignore)
    ("<end>"    . ignore)
    ("<prior>"  . ignore)
    ("<next>"   . ignore)
    ("<select>" . ignore)))

(keyamp--map-leader keyamp-command-map '(keyamp-lleader-map . keyamp-rleader-map))
(keyamp--map-return keyamp-command-map keyamp-insert)
(keyamp--map keyamp-command-map
  '(("C-q" . toggle-agent)                 ("C-S-q" . terminal)
    ("C-t" . terminal)                     ("C-S-t" . toggle-agent)))
(keyamp--map keyamp-command-map
  '(;; Left half
    ("`" . alternate-frame)                ("~"  . keyamp-insert-and-self-insert)
    ("1" . kmacro-record)                  ("!"  . keyamp-insert-and-self-insert)
    ("2" . kmacro-play)                    ("@"  . keyamp-insert-and-self-insert)
    ("3" . terminal)                       ("#"  . keyamp-insert-and-self-insert)
    ("4" . append-to-r1)                   ("$"  . keyamp-insert-and-self-insert)
    ("5" . config)                         ("%"  . keyamp-insert-and-self-insert)

    ("q" . insert-space-before)            ("Q"  . keyamp-insert-and-self-insert)
    ("w" . backward-del-word)              ("W"  . keyamp-insert-and-self-insert)
    ("e" . undo)                           ("E"  . keyamp-insert-and-self-insert)
    ("r" . del-word)                       ("R"  . keyamp-insert-and-self-insert)
    ("t" . cut-text-block)                 ("T"  . keyamp-insert-and-self-insert)

    ("a" . shrink-whitespaces)             ("A"  . keyamp-insert-and-self-insert)
    ("s" . open-line)                      ("S"  . keyamp-insert-and-self-insert)
    ("d" . del-back)                       ("D"  . keyamp-insert-and-self-insert)
    ("f" . newline)                        ("F"  . keyamp-insert-and-self-insert)
    ("g" . activate-region)                ("G"  . keyamp-insert-and-self-insert)

    ("z" . toggle-comment)                 ("Z"  . keyamp-insert-and-self-insert)
    ("x" . cut-line)                       ("X"  . keyamp-insert-and-self-insert)
    ("c" . copy-line)                      ("C"  . keyamp-insert-and-self-insert)
    ("v" . paste-or-prev)                  ("V"  . keyamp-insert-and-self-insert)
    ("b" . toggle-case)                    ("B"  . keyamp-insert-and-self-insert)

    ;; Right half
    ("6" . search-string)                  ("^"  . keyamp-insert-and-self-insert)
    ("7" . jump-to-register)               ("&"  . keyamp-insert-and-self-insert)
    ("8" . point-to-register)              ("*"  . keyamp-insert-and-self-insert)
    ("9" . proced-defer)                   ("("  . keyamp-insert-and-self-insert)
    ("0" . sh-defer)                       (")"  . keyamp-insert-and-self-insert)
    ("-" . enlarge-window-any)             ("_"  . keyamp-insert-and-self-insert)
    ("=" . text-scale-increase)            ("+"  . keyamp-insert-and-self-insert)

    ("y"  . pass)                          ("Y"  . keyamp-insert-and-self-insert)
    ("u"  . back-word)                     ("U"  . keyamp-insert-and-self-insert)
    ("i"  . previous-line)                 ("I"  . keyamp-insert-and-self-insert)
    ("o"  . forw-word)                     ("O"  . keyamp-insert-and-self-insert)
    ("p"  . goto-match-br)                 ("P"  . keyamp-insert-and-self-insert)
    ("["  . toggle-ibuffer)                ("{"  . keyamp-insert-and-self-insert)
    ("]"  . tree-view)                     ("}"  . keyamp-insert-and-self-insert)
    ("\\" . screen-lock)                   ("|"  . keyamp-insert-and-self-insert)

    ("h" . beg-of-line)                    ("H"  . keyamp-insert-and-self-insert)
    ("j" . bchar)                          ("J"  . keyamp-insert-and-self-insert)
    ("k" . next-line)                      ("K"  . keyamp-insert-and-self-insert)
    ("l" . fchar)                          ("L"  . keyamp-insert-and-self-insert)
    (";" . end-of-lyne)                    (":"  . keyamp-insert-and-self-insert)
    ("'" . tools)                          ("\"" . keyamp-insert-and-self-insert)

    ("n" . isearch-forward)                ("N"  . keyamp-insert-and-self-insert)
    ("m" . backward-bracket)               ("M"  . keyamp-insert-and-self-insert)
    ("," . other-win)                      ("<"  . keyamp-insert-and-self-insert)
    ("." . forward-bracket)                (">"  . keyamp-insert-and-self-insert)
    ("/" . buf-or-bookmark)                ("?"  . keyamp-insert-and-self-insert)

    ("<left>"  . back-char)
    ("<right>" . forw-char)
    ("<up>"    . up-line)
    ("<down>"  . down-line)

    ("<prior>" . ignore)
    ("<next>"  . ignore)
    ("<home>"  . ignore)
    ("<end>"   . ignore) ("<select>" . ignore)

    ("<f1>"  . ignore) ("<f2>"  . ignore) ("<f3>"  . ignore)
    ("<f4>"  . ignore) ("<f5>"  . ignore) ("<f6>"  . ignore)
    ("<f7>"  . ignore) ("<f8>"  . ignore) ("<f9>"  . ignore)
    ("<f10>" . ignore) ("<f11>" . ignore) ("<f12>" . ignore)
    ("<f13>" . ignore) ("<f14>" . ignore) ("<f15>" . ignore)
    ("<f16>" . ignore) ("<f17>" . ignore) ("<f18>" . ignore)
    ("<f19>" . ignore) ("<f20>" . ignore) ("<f21>" . ignore)
    ("<f22>" . ignore) ("<f23>" . ignore) ("<f24>" . ignore)))

(when keyamp-touchp
  (keyamp--map keyamp-command-map
    '(("C-q" . execute-extended-command) ("C-t" . toggle-dired))))

(keyamp--map-leader keyamp-lleader-map '(select-word . select-quote))
(keyamp--map-return keyamp-lleader-map execute-extended-command)
(keyamp--map keyamp-lleader-map
  '(("C-q" . ignore)                       ("C-S-q" . ignore)
    ("C-t" . display-line-numbers-mode)    ("C-S-t" . ignore)))
(keyamp--map-escape keyamp-lleader-map ignore)
(keyamp--map-tab keyamp-lleader-map read-only-mode)
(keyamp--map-backtab keyamp-lleader-map ignore)
(keyamp--map keyamp-lleader-map
  '( ;; Left leader left half
    ("`" . toggle-primary-input-method)
    ("1" . periodic-chart)
    ("2" . kmacro-play-toggle)
    ("3" . revert-buffer)
    ("4" . clear-r1)
    ("5" . repeat-command)

    ("q" . toggle-hand-swap)
    ("w" . org-ctrl-c-ctrl-c)
    ("e" . split-window-below)
    ("r" . query-replace)
    ("t" . copy-text-block)

    ("a" . kill-line)
    ("s" . prev-buf)
    ("d" . del-forw)
    ("f" . next-buf)
    ("g" . new-empty-buffer)

    ("z" . universal-argument)
    ("x" . restart-emacs)
    ("c" . copy-to-r1)
    ("v" . paste-from-r1)
    ("b" . toggle-prev-case)

    ;; Left leader right half
    ("6" . find-name-dired)
    ("7" . number-to-register)
    ("8" . sql)
    ("9" . screenshot)
    ("0" . eww)
    ("-" . reformat-lines)
    ("=" . mark-defun)

    ("y" . pass-find)
    ("u" . flymake-goto-prev-error)

    ("i i"   . copy-file-path)
    ("i DEL" . count-matches)              ("i SPC" . count-words)
    ("i <escape>" . ignore)                ("i RET" . show-in-desktop)

    ("o"  . flymake-goto-next-error)
    ("p"  . goto-line)
    ("["  . toggle-frame-maximized)
    ("]"  . make-frame-command)
    ("\\" . quit)

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

    ("l" . isearch-wforw)
    (";" . recentf-open-files)
    ("'" . list-timers)
    ("n" . list-processes)
    ("m" . vt-conn)
    ("," . ai)
    ("." . open-last-closed)
    ("/" . bookmark-jump-remote)

    ("<left>"  . ignore)
    ("<right>" . ignore)
    ("<up>"    . ignore)
    ("<down>"  . ignore)

    ("<prior>" . ignore)
    ("<next>"  . ignore)
    ("<home>"  . ignore)
    ("<end>"   . ignore) ("<select>" . ignore)

    ("C-^" . keyamp-script-leader-map)     ("C-_" . keyamp-script-leader-map)
    ("C-+" . keyamp-script-leader-map)     ("C-И" . keyamp-script-leader-map)
    ("C-b" . keyamp-script-leader-map)

    ;; Scripting corner cases for Russian
    ("G"  . toggle-comment)      ; Slash
    ("H"  . universal-argument)  ; Slash hold down
    ("Y"  . enlarge-window)      ; Hyphen
    ("T"  . text-scale-increase) ; Equal sign
    ))

(when keyamp-touchp
  (keyamp--map keyamp-lleader-map
    '(("<left>"  . hl-line-mode)
      ("<right>" . display-line-numbers-mode)
      ("<up>"    . read-only-mode)
      ("<down>"  . whitespace-mode)

      ("<prior>" . ignore)
      ("<next>"  . ignore)
      ("<home>"  . ignore)
      ("<end>"   . ignore) ("<select>" . ignore))))

(when (display-graphic-p)
  (keyamp--map keyamp-lleader-map
    '(("i DEL" . nil)                      ("i <backspace>" . count-matches)
      ("i RET" . nil)                      ("i <return>"    . show-in-desktop)
      ("j DEL" . nil)                      ("j <backspace>" . whitespace-mode)
      ("j RET" . nil)                      ("j <return>"    . toggle-word-wrap)
      ("k DEL" . nil)                      ("k <backspace>" . flyspell-buffer)
      ("k RET" . nil)                      ("k <return>"    . find-file)
      ("<mouse-1>" . ignore)
      ("<mouse-2>" . ignore)
      ("<mouse-3>" . ignore)
      ("<down-mouse-1>" . mouse-drag-region-rectangle))))

(keyamp--map-leader keyamp-rleader-map '(select-line . select-block))
(keyamp--map-return keyamp-rleader-map open-file)
(keyamp--map keyamp-rleader-map
  '(("C-q" . ignore)                       ("C-S-q" . ignore)
    ("C-t" . toggle-truncate-lines)        ("C-S-t" . ignore)))
(keyamp--map-escape keyamp-rleader-map ignore)
(keyamp--map-backtab keyamp-rleader-map ignore)
(keyamp--map-tab keyamp-rleader-map open-in-external-app)
(keyamp--map keyamp-rleader-map
  '(;; Right leader left half
    ("`" . toggle-std-to-cur-layout)
    ("1" . view-lossage)
    ("2" . insert-kbd-macro)
    ("3" . repeat)
    ("4" . change-bracket-pairs)
    ("5" . json-pretty)

    ("q" . fill-or-unfill)
    ("w" . sun-moon)

    ("e e"   . todo)                       ("e k"   . weather)
    ("e d"   . org-shiftdown)
    ("e SPC" . clock)                      ("e DEL" . calendar)
    ("e <escape>" . ignore)                ("e RET" . insert-date)

    ("r" . query-replace-regexp)
    ("t" . calc)
    ("a" . mark-whole-buffer)
    ("s" . clean-whitespace)

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

    ("g" . player)
    ("z" . goto-char)
    ("x" . next-eww-buf)
    ("c" . copy-all)
    ("v" . tasks)
    ("b" . title-case-region-or-line)

    ;; Right leader right half
    ("6" . toggle-case-fold-search)
    ("7" . copy-to-register)
    ("8" . insert-register)
    ("9" . org-insert-source-code)
    ("0" . toggle-theme)
    ("-" . reformat-lines)
    ("=" . mark-defun)

    ("y"  . password-store)
    ("u"  . backward-punct)
    ("i"  . beg-of-block-rev)
    ("o"  . forward-punct)
    ("p"  . toggle-hand-swap)
    ("["  . toggle-frame-maximized) ; Same for left leader
    ("]"  . make-frame-command) ; Same for left leader
    ("\\" . empty-bin)

    ("h" . page-up-half)
    ("j" . occur-cur-word)
    ("k" . end-of-block)
    ("l" . bookmark-set)
    (";" . page-dn-half)
    ("'" . scratch)

    ("n" . help-command)
    ("m" . dired-jump)
    ("," . delete-other-windows)
    ("." . save-close-buf)
    ("/" . view-messages)

    ("<left>"  . ignore)
    ("<right>" . ignore)
    ("<up>"    . ignore)
    ("<down>"  . ignore)

    ("<prior>" . ignore)
    ("<next>"  . ignore)
    ("<home>"  . ignore)
    ("<end>"   . ignore)

    ("C-^" . keyamp-script-leader-map)     ("C-_" . keyamp-script-leader-map)
    ("C-+" . keyamp-script-leader-map)     ("C-И" . keyamp-script-leader-map)
    ("C-b" . keyamp-script-leader-map)

    ;; Scripting corner cases for Russian
    (")"  . empty-bin)       ; Backslash hold down
    ("("  . screen-lock)     ; Backslash
    ("N"  . tree-view)       ; Close bracket
    ("G"  . buf-or-bookmark) ; Slash
    ("H"  . view-messages)   ; Slash hold down
    ))

(when keyamp-touchp
  (keyamp--map keyamp-rleader-map
    '(("<left>"  . toggle-truncate-lines)
      ("<right>" . scratch)
      ("<up>"    . toggle-messages)
      ("<down>"  . delete-other-windows)

      ("<prior>" . ignore)
      ("<next>"  . ignore)
      ("<home>"  . ignore)
      ("<end>"   . ignore) ("<select>" . ignore))))

(when (display-graphic-p)
  (keyamp--map keyamp-rleader-map
    '(("e DEL" . nil)                      ("e <backspace>" . calendar)
      ("e RET" . nil)                      ("e <return>"    . insert-date)
      ("d DEL" . nil)                      ("d <backspace>" . eval-defun-visual)
      ("d RET" . nil)                      ("d <return>"    . shell-command)
      ("f DEL" . nil)                      ("f <backspace>" . insert-ascii-double-quote)
      ("f RET" . nil)                      ("f <return>"    . emoji-insert)
      ("<mouse-1>" . ignore)
      ("<mouse-2>" . ignore)
      ("<mouse-3>" . ignore))))

(keyamp--map keyamp-script-leader-map
  '(("G" . bookmark-jump-remote) ; Left leader slash
    ("(" . quit)                 ; Left leader backslash
    ))

(keyamp--map-double
  '((keyamp-escape . toggle-ibuffer)       (other-win   . jump-mark)
    (beg-of-line   . beg-of-buf)           (end-of-lyne . end-of-buf)
    (proced-defer  . save-close-buf)       (sh-defer    . delete-other-windows)))

(when keyamp-touchp
  (keyamp--map-double '((keyamp-escape . toggle-agent))) )

(when keyamp-touchp ; Standard iOS hold down candidates
  (keyamp--map keyamp-command-map
    '(("ŵ" . org-ctrl-c-ctrl-c)              ("é" . ignore)
      ("è" . split-window-below)             ("ê" . ignore)
      ("ě" . ignore)                         ("ẽ" . ignore)
      ("ē" . ignore)                         ("ė" . ignore)
      ("ę" . ignore)                         ("ř" . ignore)
      ("ț" . ignore)                         ("ť" . ignore)
      ("þ" . ignore)                         ("ý" . ignore)
      ("ŷ" . ignore)                         ("ÿ" . ignore)
      ("ú" . ignore)                         ("ü" . ignore)
      ("ũ" . ignore)                         ("ū" . ignore)
      ("ű" . ignore)                         ("ů" . ignore)
      ("ų" . ignore)                         ("ù" . ignore)
      ("û" . ignore)                         ("ǔ" . ignore)
      ("į" . ignore)                         ("ı" . ignore)
      ("ī" . ignore)                         ("ĩ" . ignore)
      ("ǐ" . ignore)                         ("ï" . ignore)
      ("í" . ignore)                         ("ì" . ignore)
      ("î" . ignore)                         ("ò" . ignore)
      ("ó" . ignore)                         ("ô" . ignore)
      ("ö" . ignore)                         ("ǒ" . ignore)
      ("œ" . ignore)                         ("õ" . ignore)
      ("ō" . ignore)                         ("ő" . ignore)
      ("à" . ignore)                         ("á" . ignore)
      ("â" . ignore)                         ("ä" . ignore)
      ("ǎ" . ignore)                         ("æ" . ignore)
      ("ã" . ignore)                         ("å" . ignore)
      ("ā" . ignore)                         ("ă" . ignore)
      ("ą" . ignore)                         ("ß" . ignore)
      ("ş" . ignore)                         ("ș" . ignore)
      ("ś" . ignore)                         ("š" . ignore)
      ("ď" . ignore)                         ("ð" . ignore)
      ("ğ" . ignore)                         ("ġ" . ignore)
      ("ħ" . page-up-half)                   ("ķ" . other-win)
      ("ł" . end-of-lyne)                    ("ļ" . end-of-buf)
      ("ľ" . ignore)                         ("ź" . universal-argument)
      ("ž" . hide-virtual-keyboard)          ("ż" . ignore)
      ("ç" . ignore)                         ("ć" . ignore)
      ("č" . ignore)                         ("ċ" . ignore)
      ("ñ" . ignore)                         ("ń" . ignore)
      ("ņ" . ignore)                         ("ň" . ignore))))


;; Remaps

(defun keyamp-wdired-enter ()
  "Dynamically change mapping on wdired enter."
  (keyamp--map keyamp-command-map
    '(("C-q" . wdired-abort-changes) ("C-t" . wdired-finish-edit))))

(defun keyamp-wdired-exit ()
  "Dynamically change mapping on wdired exit."
  (if keyamp-touchp
      (keyamp--map keyamp-command-map
        '(("C-q" . execute-extended-command) ("C-t" . toggle-dired)))
    (keyamp--map keyamp-command-map
      '(("C-q" . toggle-agent) ("C-t" . terminal)))))

(add-hook 'wdired-mode-hook 'keyamp-wdired-enter)
(advice-add 'wdired-change-to-dired-mode :after 'keyamp-wdired-exit)

(when keyamp-touchp
  (keyamp--map-backtab global-map toggle-ibuffer))

(defun keyamp-map-override-insert ()
  "Toggle map override insert mode only."
  (keyamp--map-backtab keyamp-map undo))

(defun keyamp-map-override-command ()
  "Toggle map override command mode only."
  (keyamp--map-backtab keyamp-map nil))

(add-hook 'keyamp-insert-hook 'keyamp-map-override-insert)
(add-hook 'keyamp-command-hook 'keyamp-map-override-command)

(when (display-graphic-p) ; /lisp/indent.el.gz:816
  (keymap-set global-map "<tab>" 'indent-for-tab-command))

(defun keyamp-key-translation ()
  "Key translations."
  (when (display-graphic-p)
    (keymap-set key-translation-map "S-SPC"         "<tab>")
    (keymap-set key-translation-map "S-<backspace>" "<backtab>")
    (keymap-set key-translation-map "S-<return>"    "<escape>") ; Test
    (keymap-set key-translation-map "C-k"           "C-t") ; Temp qwerty to engram
    (keymap-set key-translation-map "C-b"           "C-q") ; Temp bug
    (keymap-set key-translation-map "S-<escape>"    "<escape>"))
  (when keyamp-touchp
    (keymap-set key-translation-map "TAB"      "<escape>")  ; Double tap
    (keymap-set key-translation-map "<prior>"  "C-q")       ; Two fingers up
    (keymap-set key-translation-map "<next>"   "C-t")       ; Two fingers down
    (keymap-set key-translation-map "<end>"    "TAB")       ; Two fingers right
    (keymap-set key-translation-map "<select>" "TAB")       ; "
    (keymap-set key-translation-map "<home>"   "<backtab>") ; Two fingers left
    ))

;; Run on load
(keyamp-key-translation)

(setq help-map (make-sparse-keymap))
(fset 'help-command help-map)

(keyamp--map-leader help-map '(translate . lookup-word-definition))
(keyamp--map-escape help-map ignore)
(keyamp--map-return help-map lookup-web)
(keyamp--map-tab help-map lookup-wikipedia)
(keyamp--map-backtab help-map nil)
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
(keyamp--map help-map
  '(("C-^" . keyamp-script-leader-map) ("C-_" . keyamp-script-leader-map)
    ("C-+" . keyamp-script-leader-map) ("C-И" . keyamp-script-leader-map)
                                       ("C-b" . keyamp-script-leader-map)))

(dotimes (n 10) ; Help command + number to send corresponding fn key
  (keymap-set help-map (format "%d" (% n 10))
              `(lambda ()
                 (interactive)
                 (execute-kbd-macro (kbd ,(format "<f%d>" n))))))

;; Pass single key through the network
(keyamp--map global-map '(("<f10>" . exec-query) ("<f12>" . keyamp-escape)))

(when (display-graphic-p) ; Mouse
  (keyamp--map global-map '(("<double-mouse-1>" . open-file) ("<mouse-3>" . mouse-3)))
  (advice-add 'mouse-set-point   :around #'lookup-around)
  (advice-add 'mouse-set-point   :before #'scroll-one-pixel)
  (advice-add 'mouse-set-point   :after  #'keyamp-command-if-insert)
  (advice-add 'mouse-set-point   :after  #'vterm-set-point)
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

(with-sparse-keymap ; Next DEL to exit or RET to start over
  (keyamp--map-leader keymap '(insert-space-before . delete-backward-char))
  (keyamp--map-return keymap hippie-expand)
  (keyamp--set keymap '(hippie-expand-reset)))


;; I-search

(with-sparse-keymap
  ;; After starting up an isearch press DEL to retreat to the previous
  ;; search string. Press SPC to pull string from kill ring into search string.
  (keyamp--map-leader keymap '(isearch-yank-kill . isearch-ring-retreat))
  (keyamp--map-escape keymap save-buffer-isearch-cancel)
  (keyamp--map-return keymap isearch-direction-switch)
  (keyamp--map keymap
    '(("C-t"    . isearch-forward-regexp)  ("C-q"     . isearch-backward-regexp)
      ("C-S-t"  . isearch-backward-regexp) ("C-S-q"   . isearch-forward-regexp)
      ("<up>"   . isearch-ring-retreat)    ("<down>"  . isearch-ring-advance)
      ("<left>" . isearch-double-back)     ("<right>" . isearch-forw)))
  (keyamp--map-tab keymap isearch-forw) ; Repeat prev search forward
  (keyamp--map-backtab keymap isearch-double-back) ; Repeat prev search backward
  (keyamp--hook keymap '(isearch-mode-hook) nil nil :repeat))

;; Hit TAB to repeat after typing in search string and set following transient
;; map. Backtab to repeat backward. S-DEL/S-SPC for Backtab/TAB.
(keyamp--map-leader isearch-mode-map '(isearch-printing-char . isearch-del-char))
(keyamp--map-escape isearch-mode-map isearch-cancel)
(keyamp--map-tab isearch-mode-map isearch-forw)
(keyamp--map-backtab isearch-mode-map isearch-back)
(keyamp--map isearch-mode-map
  '(("C-^"    . keyamp-lleader-map)    ("C-t"     . ignore)
    ("<up>"   . isearch-ring-retreat)  ("<down>"  . isearch-ring-advance)
    ("<left>" . isearch-back)          ("<right>" . isearch-forw)))
(keyamp--remap isearch-mode-map '((paste-from-r1 . isearch-yank-r1)))

(with-sparse-keymap
  ;; Find the occurrence of the current search string with J/L or DEL/SPC.
  ;; Press I/K or DEL/SPC to get search strings from the ring.
  ;; S-SPC to find the occurrence of the last search string.
  (keyamp--map-leader keymap '(isearch-forw . isearch-back))
  (keyamp--map-return keymap isearch-exit)
  (keyamp--map-std keymap 'isearch-printing-char)
  (keyamp--map keymap
    '(("C-t" . ignore)
      ("i" . isearch-ring-retreat) ("j" . isearch-back)
      ("k" . isearch-ring-advance) ("l" . isearch-forw)
      ("e" . isearch-ring-retreat) ("s" . isearch-back)
      ("d" . isearch-ring-advance) ("f" . isearch-forw)))
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
    (when (isearch-minibuffer-prompt)
      (keyamp-repeat-deactivate-init keymap)))

  (add-hook 'minibuffer-setup-hook 'isearch-mode-setup-minibuffer 96)
  (advice-add-macro '(hist-back hist-forw) :after 'isearch-mode-setup-minibuffer))


;; Repeat mode - screen commands

(when keyamp-touchp
  (defun keyamp-touch-backtab-cmd ()
    "Prefix command."
    (interactive)
    (let ((key "M-^"))
      (keymap-set keyamp-command-map key 'keyamp-touch-backtab)
      (keyamp-touch-prefix key)))

  (define-prefix-command 'keyamp-touch-backtab)
  (keyamp--map-std keyamp-touch-backtab 'ignore)
  (keyamp--map-escape keyamp-touch-backtab keyamp-escape)
  (keyamp--map-backtab keyamp-touch-backtab backward-del-word)
  (keyamp--map-tab keyamp-touch-backtab del-word)
  (keyamp--map keyamp-touch-backtab
    '(("C-q"    . org-ctrl-c-ctrl-c) ("C-t"     . universal-argument)
      ("<left>" . open-line)         ("<right>" . newline)
      ("<up>"   . undo)              ("<down>"  . del-back)))

  (defun keyamp-touch-tab-cmd ()
    "Prefix command."
    (interactive)
    (let ((key "M-_"))
      (keymap-set keyamp-command-map key 'keyamp-touch-tab)
      (keyamp-touch-prefix key)))

  (define-prefix-command 'keyamp-touch-tab)
  (keyamp--map-std keyamp-touch-tab 'ignore)
  (keyamp--map-escape keyamp-touch-tab keyamp-escape)
  (keyamp--map-backtab keyamp-touch-tab list-timers)
  (keyamp--map-tab keyamp-touch-tab proced-defer)
  (keyamp--map keyamp-touch-tab
    '(("C-q"    . list-processes)     ("C-t"     . list-registers)
      ("<left>" . enlarge-window-any) ("<right>" . keyamp-insert)
      ("<up>"   . save-close-buf)     ("<down>"  . beg-of-block)))

  (with-sparse-keymap
    (keyamp--map-escape keymap toggle-agent)
    (keyamp--map-backtab keymap keyamp-touch-backtab-cmd)
    (keyamp--map-tab keymap keyamp-touch-tab-cmd)
    (keyamp--map keymap
      '(("C-q"    . toggle-messages) ("C-t"     . delete-other-windows)
        ("<left>" . tasks)           ("<right>" . screen-home-toggle)
        ("<up>"   . page-dn-half)    ("<down>"  . page-up-half)))
    (keyamp--set keymap '(keyamp-escape) nil nil nil 1)))

(with-sparse-keymap
  ;; Leader layer to become transient main. Base map for next leaders adjustment
  ;; by transient maps which set by following target commands subsets.
  (keyamp--map-leader keymap '(newline . open-line))
  (keyamp--map-return keymap keyamp-escape)
  (when keyamp-touchp
    (keyamp--remap keymap '((up-line . page-dn-half) (down-line . screen-home))))
  (keyamp--remap keymap
    '((make-frame-command  . delete-frame)
      (insert-space-before . ignore)
      (backward-del-word   . ignore)
      (undo                . ignore)
      (del-word            . ignore)
      (cut-text-block      . ignore)
      (goto-match-br       . ignore)
      (shrink-whitespaces  . ignore)
      (del-back            . save-close-buf)
      (cut-line            . ignore)
      (kill-line           . ignore)
      (copy-line           . ignore)
      (paste-or-prev       . tasks)
      (toggle-case         . tools)
      (backward-bracket    . dired-jump)
      (forward-bracket     . save-close-buf)
      (back-char           . next-buf)
      (forw-char           . prev-buf)))

  (keyamp--set keymap
    '(prev-buf                   next-buf
      save-close-buf
      prev-proj-buf              next-proj-buf
      prev-eww-buf               next-eww-buf
      prev-eshell-buf            next-eshell-buf
      prev-dired-buf             next-dired-buf
      tasks                      config
      previous-buffer            next-buffer
      find-prev-dir-file         find-next-dir-file
      shrink-window              enlarge-window
      shrink-window-horizontally enlarge-window-horizontally
      enlarge-window-any         shrink-window-any
      volume-decrease            volume-increase
      org-agenda-tasks           split-window-horizontally)))

(with-sparse-keymap
  ;; DEL/SPC to switch other window after split as a result of the commands.
  (keyamp--map-leader keymap '(other-window . other-window))
  (keyamp--map-return keymap delete-other-windows)
  (keyamp--set keymap
    '(describe-foo-at-point   describe-variable
      describe-function       describe-key
      describe-mode           describe-char
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
  (keyamp--remap keymap
    '((open-line . prev-eww-buf) (newline . next-eww-buf)
      (del-back  . eww-reload)   (undo    . justify-buffer)))
  (keyamp--set keymap '(prev-eww-buf next-eww-buf)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . prev-eshell-buf) (newline . next-eshell-buf)))
  (keyamp--set keymap '(prev-eshell-buf next-eshell-buf)))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . prev-dired-buf) (newline . next-dired-buf)))
  (keyamp--set keymap '(prev-dired-buf next-dired-buf)))

(with-sparse-keymap
  (keyamp--remap keymap
    '((open-line . tasks) (newline   . next-buf)
      (back-char . tasks) (forw-char . prev-buf)))
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
      (del-back        . save-close-buf)))
  (keyamp--set keymap '(save-close-buf)))

(with-sparse-keymap
  (keyamp--remap keymap
    '((open-line . shrink-window-any) (newline . enlarge-window-any)))
  (keyamp--map keymap '(("<up>" . shrink-window-any) ("<down>" . enlarge-window-any)))
  (keyamp--set keymap
    '(shrink-window enlarge-window enlarge-window-any shrink-window-any)
    nil nil nil 2))

(with-sparse-keymap
  (keyamp--remap keymap '((open-line . volume-decrease) (newline . volume-increase)))
  (keyamp--set keymap '(volume-increase volume-decrease) nil nil nil 2))


;; Repeat mode - read commands

(with-sparse-keymap
  ;; Initiate by triple DEL/SPC (hold down).
  ;; I/K or DEL/SPC to move by lines. See `return-before'.
  (keyamp--map-leader keymap '(down-line . down-line))
  (keyamp--map-return keymap nil)
  (keyamp--map keymap '(("<up>" . up-line-rev)))
  (keyamp--remap keymap '((previous-line . up-line-rev) (next-line . down-line-rev)))
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

(with-sparse-keymap ; Swap leaders up/down
  (keyamp--map-leader keymap '(up-line-rev . up-line-rev))
  (keyamp--remap keymap '((previous-line . up-line) (next-line . down-line-rev)))
  (keyamp--set keymap '(up-line-rev down-line-rev)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(end-of-block . end-of-block-rev))
  (keyamp--remap keymap '((previous-line . beg-of-block-rev) (next-line . end-of-block)))
  (keyamp--map keymap '(("<up>" . beg-of-block-rev) ("<down>" . end-of-block)))
  (keyamp--set keymap '(beg-of-block end-of-block)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(beg-of-block-rev . end-of-block-rev))
  (keyamp--remap keymap '((previous-line . beg-of-block) (next-line . end-of-block)))
  (keyamp--map keymap '(("<up>" . beg-of-block) ("<down>" . end-of-block)))
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
  (keyamp--map-leader keymap '(keyamp-escape . down-line))
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

(with-sparse-keymap ; Left/right arrows repeat by DEL/SPC
  (keyamp--map-leader keymap '(forw-char . back-char))
  (keyamp--set keymap '(back-char forw-char)))

(with-sparse-keymap
  (keyamp--remap keymap '((bchar . back-word) (fchar . forw-word)))
  (keyamp--remap keymap '((next-line . keyamp-escape)))
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

(with-sparse-keymap
  (keyamp--remap keymap '((other-win . jump-mark)))
  (keyamp--set keymap '(jump-mark)))

;; U and O act as leader keys.
(defvar keyamp--deactivate-leader-fun nil "Virtual leader deactivate function.")
(defvar keyamp-virtual-leader-timer nil "Virtual leader deactivate timer.")

(defun keyamp--leader-deactivate ()
  "Deactivate virtual leader."
  (when keyamp--deactivate-leader-fun
    (funcall keyamp--deactivate-leader-fun))
  (setq keyamp-virtual-leader-timer nil))

(defun keyamp-standard-to-input-method-vector (char)
  "Calculate list of vectors of corresponding input methods chars from standard
keyboard ASCII CHAR."
  (mapcar
   (lambda (map)
     (vector (string-to-char map)))
   (keyamp--std-to-non-ascii-list char)))

(defun keyamp-virtual-leader-init (Keymap)
  "Set virtual leader transient KEYMAP."
  (when-let (((keyamp-unless-kbd-macro))
             ((member (this-command-keys)
                      `(,(keyamp--convert-kbd-str "u")
                        ,@(keyamp-standard-to-input-method-vector "u")
                        ,(keyamp--convert-kbd-str "o")
                        ,@(keyamp-standard-to-input-method-vector "o")))))
    (setq keyamp--deactivate-leader-fun (set-transient-map Keymap))
    (when (timerp keyamp-virtual-leader-timer)
      (cancel-timer keyamp-virtual-leader-timer))
    (setq keyamp-virtual-leader-timer
          (run-with-timer keyamp-double-press-timeout
                          nil 'keyamp--leader-deactivate))))

(define-prefix-command 'keyamp-lleader-i-map)

(with-sparse-keymap
  (keyamp--map keymap '(("i" . keyamp-lleader-i-map)))
  (keyamp--map keyamp-lleader-i-map '(("i" . backup-and-copy)))
  (keyamp--remap keymap '((back-word . select-word) (forw-word . select-quote)))
  (defun keyamp-virtual-lleader ()
    "Virtual leader left."
    (keyamp-virtual-leader-init keymap))
  (advice-add 'back-word :after 'keyamp-virtual-lleader))

(with-sparse-keymap
  (keyamp--remap keymap
    '((back-word     . select-line)   (forw-word        . select-block)
      (previous-line . beg-of-block)  (next-line        . end-of-block)
      (bchar         . isearch-wback) (backward-bracket . downloads)))
  (defun keyamp-virtual-rleader ()
    "Virtual leader right."
    (keyamp-virtual-leader-init keymap))
  (advice-add 'forw-word :after 'keyamp-virtual-rleader))

(defun keyamp-virtual-leader-return-before (&rest _)
  "Return before, that is, compensate word move."
  (when (timerp keyamp-virtual-leader-timer)
    (set-mark-command t)))

(advice-add-macro '(select-word      select-quote
                    select-line      select-block
                    backup-and-copy  isearch-wback
                    downloads)
                  :before 'keyamp-virtual-leader-return-before)

(defun keyamp-dired-do-delete ()
  "Hit G DEL DEL to execute `dired-do-delete'."
  (interactive)
  (keyamp-deactivate-region)
  (if (eq major-mode 'dired-mode)
      (set-transient-map keyamp-g-leader-map)
    (keyamp-command-execute 'ignore)))

;; G acts as leader key.
(with-sparse-keymap
  (keyamp--map-leader keymap '(screen-home-toggle . keyamp-dired-do-delete))
  (define-prefix-command 'keyamp-g-leader-map)
  (keyamp--map-leader keyamp-g-leader-map '(nil . dired-do-delete))
  (keyamp--map-escape keymap deactivate-region)
  (keyamp--map-return keymap toggle-ibuffer)
  (keyamp--remap keymap
    '((activate-region . rectangle)
      (other-win       . delete-window)
      (isearch-forward . jump-8)
      (tools           . jump-7)))

  (advice-add 'activate-region :after
              (lambda () "virtual leader G transient"
                (when (and (keyamp-unless-kbd-macro)
                           (eq (mark) (point)))
                  (set-transient-map keymap)))))

(defun keyamp-deactivate-region (&rest _)
  "Deactivate region if mark equal point."
  (when (eq (mark) (point))
    (deactivate-region)))

(advice-add-macro '(jump-8 jump-7 screen-home-toggle delete-window toggle-ibuffer)
                  :before 'keyamp-deactivate-region)

(with-sparse-keymap
  ;; Repeat half page up/down with I/K or DEL/SPC.
  (keyamp--map-leader keymap '(page-dn-half . page-up-half))
  (keyamp--remap keymap
    '((previous-line . page-up-half-rev) (next-line . page-dn-half)
      (down-line     . page-dn-half)     (up-line   . page-up-half)))
  (when keyamp-touchp
    (keyamp--map keymap '(("C-q" . end-of-buf) ("C-t" . beg-of-buf)))
    (keyamp--remap keymap
      '((down-line      . page-up-half)
        (up-line        . page-dn-half)
        (toggle-comment . hide-virtual-keyboard))))
  (keyamp--set keymap '(page-up-half page-dn-half)))

(with-sparse-keymap ; Swap leaders up/down
  (keyamp--map-leader keymap '(page-up-half-rev . page-dn-half-rev))
  (keyamp--remap keymap '((previous-line . page-up-half) (next-line . page-dn-half)))
  (keyamp--set keymap '(page-up-half-rev page-dn-half-rev)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(next-line . previous-line))
  (keyamp--remap keymap
    '((previous-line . scroll-down-command) (next-line . scroll-up-command)
      (down-line     . page-dn-half)        (up-line   . page-up-half)))
  (when keyamp-touchp
    ;; Arrows always do half page and keep transient, see previous keymap.
    (keyamp--map keymap '(("C-q" . beg-of-buf) ("C-t" . end-of-buf)))
    (keyamp--remap keymap '((down-line . page-up-half) (up-line . page-dn-half))))
  (keyamp--set keymap '(scroll-down-command scroll-up-command)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(fchar . bchar))
  (keyamp--remap keymap '((bchar . hscroll-right) (fchar . hscroll-left)))
  (keyamp--set keymap '(hscroll-left hscroll-right)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(text-scale-decrease . text-scale-increase))
  (keyamp--map-return keymap keyamp-escape)
  (keyamp--map-tab keymap text-scale-reset)
  (keyamp--remap keymap '((buf-or-bookmark . text-scale-reset)))
  (keyamp--set keymap '(text-scale-decrease text-scale-increase text-scale-reset)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(down-line . up-line))
  (keyamp--remap keymap '((undo . button-back) (del-back . button-forw)))
  (keyamp--set keymap '(button-back button-forw)))


;; Repeat mode - modify commands

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . delete-forward-char)))
  (keyamp--set keymap '(delete-forward-char) nil nil nil 2))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . insert-space-before)))
  (keyamp--set keymap '(insert-space-before) nil nil nil 2))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . del-forw)))
  (keyamp--set keymap '(del-forw) nil nil nil 2))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . cut-text-block)))
  (keyamp--set keymap '(cut-text-block)))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . shrink-whitespaces)))
  (keyamp--set keymap '(shrink-whitespaces) nil nil nil 1))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . toggle-comment)))
  (keyamp--set keymap '(toggle-comment) nil nil nil 1))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . cut-line)))
  (keyamp--set keymap '(cut-line) nil nil nil 1))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . copy-line)))
  (keyamp--set keymap '(copy-line)))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . toggle-case)))
  (keyamp--set keymap '(toggle-case) nil nil nil 1))

(with-sparse-keymap
  (keyamp--remap keymap '((undo . org-shiftup) (del-back . org-shiftdown)))
  (keyamp--set keymap '(org-shiftup org-shiftdown)))

(with-sparse-keymap
  (keyamp--remap keymap '((undo . todo)))
  (keyamp--set keymap '(todo insert-date) nil nil nil 1))

(with-sparse-keymap
  (keyamp--remap keymap '((del-back . cycle-hyphen-lowline-space)))
  (keyamp--set keymap '(cycle-hyphen-lowline-space) nil nil nil 1))

(with-sparse-keymap
  (keyamp--remap keymap '((split-window-below . split-window-r)))
  (keyamp--set keymap '(split-window-below)))


;; Modes Remaps

(with-eval-after-load 'minibuffer
  (with-sparse-keymap
    ;; On minibuffer startup press DEL to list history backwards or
    ;; SPC to paste. S-SPC to start list completion candidates forward.
    ;; I/K or DEL/SPC to list either history or completion candidates
    ;; accordingly choice made. RET to confirm and exit, ESC to quit.
    ;; To switch from history to candidates listing press ESC then double
    ;; SPC `select-word' and DEL/SPC or I/K again to continue move
    ;; backward/forward. Similarly double DEL to activate history move.
    ;; Fast history or completion candidates direction switch to quit.
    (keyamp--map-leader keymap '(paste-or-prev . hist-back))
    (keyamp--map-escape keymap keyamp-minibuffer-escape)
    (keyamp--map-return keymap keyamp-minibuffer-return)
    (keyamp--map-tab keymap comp-forw)
    (keyamp--map-backtab keymap comp-forw-rev)
    (keyamp--map-std keymap 'keyamp-insert-minibuffer)
    (keyamp--map keymap '(("<up>" . select-word) ("<down>" . comp-forw)))
    (keyamp--map keymap
      '(("C-q" . keyamp-minibuffer-shift-up)   ("C-S-q" . keyamp-minibuffer-shift-down)
        ("C-t" . keyamp-minibuffer-shift-down) ("C-S-t" . keyamp-minibuffer-shift-up)))
    (when keyamp-touchp
      (keyamp--map keymap
        '(("<left>" . hist-back) ("<right>" . keyamp-minibuffer-return))))

    ;; The hook is last one run during minibuffer setup and set the keymap.
    (keyamp--hook keymap '(minibuffer-setup-hook) :command nil :repeat))

  (with-sparse-keymap ; Quit with ESC right away after paste
    (keyamp--map-escape keymap keyamp-minibuffer-quit)
    (advice-add 'paste-or-prev :after
                (lambda () (when (minibufferp)
                             (set-transient-map keymap)))))

  ;; Hit D/DEL for No, K/SPC for Yes to answer non-literal Y or N.
  (keyamp--remap y-or-n-p-map
    '((select-word   . y-or-n-p-insert-y) (del-back  . y-or-n-p-insert-n)
      (paste-or-prev . y-or-n-p-insert-y) (next-line . y-or-n-p-insert-y)
      (select-block  . y-or-n-p-insert-n) (hist-back . y-or-n-p-insert-n)))
  (when keyamp-touchp
    (keyamp--remap y-or-n-p-map
      '((keyamp-minibuffer-return . y-or-n-p-insert-y)))
    (keyamp--map y-or-n-p-map
      '(("<left>" . y-or-n-p-insert-n) ("<right>" . y-or-n-p-insert-y))))

  (keyamp--remap minibuffer-local-map
    '((previous-line . hist-back) (next-line . hist-forw)
      (select-block  . hist-back)
      (up-line       . hist-back) (down-line . hist-forw)))

  (keyamp--map-tab minibuffer-local-completion-map comp-forw)
  (keyamp--map minibuffer-local-completion-map '(("C-t" . minibuffer-complete)))
  (keyamp--remap minibuffer-mode-map
    '((previous-line . hist-back) (next-line . hist-forw)
      (select-block  . hist-back)
      (up-line       . hist-back) (down-line . comp-forw)
      (indent-for-tab-command . comp-forw)))

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
    '(("<mouse-1>" . toggle-messages)      ("<double-mouse-1>" . ignore)
      ("<left-fringe> <mouse-1>" . ignore) ("<right-fringe> <mouse-1>" . ignore)))
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
    (when keyamp-touchp
      (keyamp--map keymap
        '(("<left>" . hist-back) ("<right>" . keyamp-minibuffer-return))))
    (keyamp--set keymap '(comp-back comp-forw)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--map-escape keymap abort-recursive-edit)
    (keyamp--map-return keymap keyamp-minibuffer-return)
    (keyamp--remap keymap '((previous-line . comp-back-rev) (next-line . comp-forw-rev)))
    (keyamp--remap keymap '((up-line . comp-back-rev) (down-line . comp-forw-rev)))
    (keyamp--set keymap '(comp-back-rev comp-forw-rev)))

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
    (when keyamp-touchp
      (keyamp--map keymap
        '(("<left>" . comp-forw) ("<right>" . keyamp-minibuffer-return))))
    (keyamp--set keymap '(hist-back hist-forw))))

(add-hook 'ido-setup-hook
  (lambda () "ido-completion-map created after ido setup only"
    (keyamp--remap ido-completion-map
      '((keyamp-insert . ido-exit-minibuffer)
        (previous-line . hist-back)          (select-block           . hist-back)
        (next-line     . ido-next-match)     (select-word            . ido-next-match)
        (up-line       . hist-back)          (down-line              . ido-next-match)
        (comp-forw     . ido-next-match)     (ido-complete-space     . self-insert-command)
        (comp-forw-rev . ido-next-match-rev) (indent-for-tab-command . ido-next-match)))
    (keyamp--map-tab ido-completion-map ido-next-match)
    (keyamp--map-backtab ido-completion-map ido-next-match-rev)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(next-line . previous-line))
  (keyamp--remap keymap '((previous-line . ido-prev-match) (next-line . ido-next-match)))
  (keyamp--remap keymap '((up-line . ido-prev-match) (down-line . ido-next-match)))
  (keyamp--set keymap '(ido-prev-match ido-next-match)))

(with-sparse-keymap
  (keyamp--map-leader keymap '(previous-line . next-line))
  (keyamp--remap keymap '((previous-line . ido-prev-match-rev) (next-line . ido-next-match-rev)))
  (keyamp--remap keymap '((up-line . ido-next-match-rev) (down-line . ido-next-match-rev)))
  (keyamp--set keymap '(ido-prev-match-rev ido-next-match-rev)))

(with-eval-after-load 'dired
  (when keyamp-touchp
    (keyamp--remap dired-mode-map
      '((back-char . dired-jump) (forw-char . dired-find-file))))
  (keyamp--map dired-mode-map
    '(("<double-mouse-1>" . dired-find-file)
      ("<mouse-1>" . mouse-set-point) ("<mouse-2>" . mouse-set-point)))
  (keyamp--remap dired-mode-map
    '((keyamp-insert        . dired-find-file)
      (backward-bracket     . dired-jump)
      (insert-space-before  . dired-size)
      (del-word             . dired-unmark-all-marks)
      (query-replace        . dired-zip)
      (query-replace-regexp . dired-unzip)
      (backward-del-word    . dired-do-chmod)
      (shrink-whitespaces   . dired-hide-details-mode)
      (kill-line            . vt-conn-tramp-docker)
      (open-line            . prev-dired-buf)
      (del-back             . dired-toggle-mark)
      (newline              . next-dired-buf)
      (toggle-comment       . dired-omit-mode)
      (cut-line             . dired-kill-subdir)
      (cut-text-block       . ignore)
      (dired-jump           . dired-subtree-toggle)
      (copy-text-block      . dired-decrypt)
      (calc                 . ignore)
      (paste-or-prev        . dired-create-directory)
      (toggle-case          . dired-sort)
      (toggle-prev-case     . dired-encrypt)
      (copy-to-r1           . dired-do-copy)
      (paste-from-r1        . dired-do-rename)
      (mark-whole-buffer    . dired-toggle-marks)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(dired-toggle-mark . dired-toggle-mark))
    (keyamp--set keymap '(dired-toggle-mark) nil nil nil 1))

  (advice-add 'dired-toggle-marks :before #'dired-unmark-all-marks))

(with-eval-after-load 'wdired
  (keyamp--map wdired-mode-map
    '(("C-q" . wdired-abort-changes) ("C-t" . wdired-finish-edit)))
  (advice-add-macro '(wdired-abort-changes wdired-finish-edit)
                    :after 'keyamp-command))

(with-eval-after-load 'dired-utils
  (keyamp--map-tab dired-mode-map dired-leader-map)
  (keyamp--map-tab (define-prefix-command 'dired-leader-map) dired-omit-mode)
  (keyamp--map-escape dired-leader-map keyamp-escape)
  (keyamp--map dired-leader-map
    '(("e" . dired-optimize-png)     ("u" . dired-2drawing)
      ("o" . dired-rotate-img-right) ("p" . dired-rotate-img-left)
      ("a" . dired-image-autocrop)   ("s" . dired-open-marked)
      ("d" . dired-show-metadata)    ("h" . dired-rotate-img-180)
      ("l" . dired-2png)             (";" . dired-scale-image)
      ("c" . dired-2jpg))))

(with-eval-after-load 'rect ; Sane rectangle controls
  (keyamp--remap rectangle-mark-mode-map
    '((keyamp-insert       . string-rectangle)
      (insert-space-before . open-rectangle)
      (copy-line           . copy-rectangle-as-kill)
      (del-back            . kill-rectangle)
      (paste-or-prev       . yank-rectangle)
      (copy-to-register    . copy-rectangle-to-register)
      (toggle-comment      . rectangle-number-lines)
      (cut-line            . clear-rectangle)
      (clean-whitespace    . delete-whitespace-rectangle)
      (bchar               . rectangle-backward-char)
      (fchar               . rectangle-forward-char))))

(with-eval-after-load 'ibuf-ext
  (keyamp--map ibuffer-mode-map '(("<double-mouse-1>" . ibuffer-visit-buffer)))
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
      (goto-match-br       . toggle-messages)
      (shrink-whitespaces  . calendar-split)
      (open-line           . prev-buf)
      (del-back            . ibuffer-do-delete)
      (newline             . next-buf)
      (cut-line            . prev-eww-buf)
      (paste-or-prev       . tasks)
      (toggle-case         . tools)
      (forward-bracket     . nil)
      (del-word            . toggle-gnus)
      (append-to-r1        . recentf-open-files)))
  (when keyamp-touchp
    (keyamp--map-backtab ibuffer-mode-map nil)
    (keyamp--map-tab ibuffer-mode-map ibuffer-visit-buffer)
    (keyamp--remap ibuffer-mode-map
      '((forw-char . screen-home-left) (back-char . screen-home))))

  (keyamp--map ibuffer-mode-filter-group-map '(("<mouse-1>" . ibuffer-toggle-filter-group)))
  (keyamp--map-tab ibuffer-mode-filter-group-map ibuffer-toggle-filter-group)
  (keyamp--remap ibuffer-mode-filter-group-map
    '((keyamp-insert . ibuffer-toggle-filter-group)))
  (when keyamp-touchp
    (keyamp--remap ibuffer-mode-filter-group-map
      '((forw-char . screen-home-left))))

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
    (keyamp--set keymap '(ibuffer-do-delete))))

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
      (when company-candidates
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
  (keyamp--map query-replace-map '(("d" . skip) ("k" . act)))
  (when (string-equal keyamp-cur-layout "engineer-engram")
    (keyamp--map query-replace-map '(("Т" . automatic))))
  (keyamp--map-leader query-replace-map '(act . skip)))

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
  (keyamp--map-backtab org-mode-map nil)
  (keyamp--remap org-mode-map
    '((eval-region-or-sexp . insert-date) (insert-date . org-time-stamp)
      (open-line . nil))))

(with-eval-after-load 'org-agenda
  (keyamp--map-tab org-agenda-mode-map todo)
  (keyamp--map-backtab org-agenda-mode-map nil)
  (keyamp--map org-agenda-mode-map
    '(("<double-mouse-1>" . org-agenda-tasks) ("<mouse-3>" . mouse-3)))
  (keyamp--remap org-agenda-mode-map
    '((keyamp-insert      . org-agenda-tasks)
      (del-word           . toggle-gnus)
      (goto-match-br      . org-agenda-redo)
      (shrink-whitespaces . scratch)
      (open-line          . prev-buf)
      (del-back           . calendar-split)
      (newline            . next-buf)
      (previous-line      . up-line)
      (next-line          . down-line)
      (toggle-comment     . ignore)
      (cut-line           . prev-eww-buf)
      (paste-or-prev      . tasks)
      (toggle-case        . tools)
      (backward-bracket   . dired-jump)
      (kmacro-record      . alarm)
      (search-string      . stopwatch-lap)
      (jump-to-register   . stopwatch)
      (point-to-register  . timer)
      (insert-register    . timer-stop)
      (proced-defer       . timer-display)))
  (when keyamp-touchp
    (keyamp--remap org-agenda-mode-map
      '((forw-char      . screen-home-right)
        (back-char      . screen-home-left)
        (toggle-comment . hide-virtual-keyboard)))))

(when keyamp-touchp
  (defvar screen-home-keymap (make-sparse-keymap))

  (keyamp--map screen-home-keymap
    '(("<right>" . screen-home-right) ("<left>" . screen-home-left)
      ("<up>"    . alt-buf)           ("<down>" . screen-lock)))

  (keyamp--set screen-home-keymap '(screen-lock alt-buf))

  (advice-add 'delete-other-windows :after
              (lambda (&rest _) "screen-home-keymap"
                (when (eq major-mode 'org-agenda-mode)
                  (set-transient-map screen-home-keymap))))

  (defvar screen-home-right-keymap (make-sparse-keymap))
  (keyamp--map-tab screen-home-right-keymap nil)
  (keyamp--map screen-home-right-keymap
    '(("<left>" . screen-home) ("<right>" . screen-home-left)
      ("<up>"   . nil)         ("<down>"  . nil)))
  (keyamp--set screen-home-right-keymap '(screen-home-right)))

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
    (keyamp--set keymap '(doc-view-shrink doc-view-enlarge) nil nil nil 2))

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
      (undo             . ignore)              (del-back     . image-rotate)
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
  (keyamp--map-backtab eshell-mode-map nil)
  (keyamp--remap eshell-mode-map
    '((cut-line       . eshell-clear)
      (select-word    . eshell-previous-input)
      (open-line      . prev-eshell-buf)
      (newline        . next-eshell-buf)
      (toggle-comment . ignore)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--remap keymap
      '((previous-line . eshell-previous-input) (next-line . eshell-next-input)))
    (keyamp--set keymap
      '(eshell-previous-input eshell-next-input eshell-search-input) :command))

  (with-sparse-keymap
    ;; Insert mode primary for eshell. The keymap ready after eshell start,
    ;; command submit or cancel. DEL to list history, SPC to paste.
    (keyamp--map-leader keymap '(eshell-previous-input . eshell-next-input))
    (keyamp--map-tab keymap change-wd)
    (keyamp--map-backtab keymap eshell-search-input)
    (keyamp--set keymap '(eshell-send-input eshell-interrupt-process) nil :insert)

    (defun keyamp-input-timer-payload-eshell ()
      "Set transient keymap for eshell after input timer timeout."
      (when (eq major-mode 'eshell-mode)
        (keyamp-repeat-deactivate-init keymap)))

    (advice-add 'keyamp-input-timer-payload :after #'keyamp-input-timer-payload-eshell))

  (advice-add-macro '(eshell-send-input eshell-interrupt-process)
                    :after 'keyamp-input-timer))

(with-eval-after-load 'em-cmpl ; <backtab> conflict w/ default
  (keyamp--map-backtab eshell-cmpl-mode-map nil))

;;; VTerm
(with-eval-after-load 'vterm
  (keyamp--map-tab vterm-mode-map vterm-send-tab)
  (keyamp--map-backtab vterm-mode-map nil)

  (keyamp--map vterm-mode-map '(("C-t" . vterm-send-tab)))
  (keyamp--remap vterm-mode-map
    '(;; Left half
      (insert-space-before . vt-enter-copy-mode)    ; Q
      (periodic-chart      . vt-split-view)         ; SPC 1
      (backward-del-word   . vt-shell-vi-cmd)       ; W Sync point or do modify if in transient
      (undo                . vterm-undo)            ; E
      (del-word            . vt-shell-vi-cmd)       ; R Sync point or do modify if in transient
      (query-replace       . vt-vi)                 ; SPC R Activate vi mode (TUI)
      (cut-text-block      . vt-conn-reconnect)     ; T
      (copy-text-block     . vt-sftp-jump)          ; SPC T
      (shrink-whitespaces  . vt-conn-localhost)     ; A
      (kill-line           . vt-close-window)       ; SPC A
      (open-line           . vt-prev-window)        ; S
      (del-back            . vt-shell-vi-cmd)       ; D Sync point or do modify if in transient
      (newline             . vt-next-window)        ; F
      (new-empty-buffer    . vt-new-window)         ; SPC G
      (toggle-comment      . vterm-read-send-key)   ; Z
      (cut-line            . vterm-clear)           ; X
      (paste-or-prev       . vterm-yank)            ; V
      (paste-from-r1       . paste-from-r1-vt)      ; SPC V
      (toggle-case         . vt-position)           ; B
      (toggle-prev-case    . vt-command-copy)       ; SPC B
      (revert-buffer       . prev-vterm-buf)        ; SPC 3
      (select-word         . vt-shell-vi-cmd-up)    ; SPC SPC
      (org-ctrl-c-ctrl-c   . vterm-send-c-c)        ; SPC W

      ;; Right half
      (page-up-half        . vt-page-up-half)       ; DEL H
      (page-dn-half        . vt-page-dn-half)       ; DEL ;
      (dired-jump          . vt-conn-tramp)         ; DEL M
      (copy-all            . vt-copy)               ; DEL C
      (password-store      . vt-sudo-password-copy) ; DEL Y
      (back-char           . vterm-left)
      (forw-char           . vterm-right)
      (up-line             . vterm-up)
      (down-line           . vterm-down)))

  (keyamp--map-tab vterm-copy-mode-map vterm-send-tab)
  (keyamp--map-backtab vterm-copy-mode-map nil)

  (keyamp--map vterm-copy-mode-map '(("C-t" . vterm-send-tab)))
  (keyamp--remap vterm-copy-mode-map
    '(;; Left half
      (insert-space-before . vt-exit-copy-mode)      ; Q
      (periodic-chart      . vt-split-view)          ; SPC 1
      (backward-del-word   . vt-shell-vi-cmd)        ; W Sync point or do modify if in transient
      (undo                . vterm-undo)             ; E
      (del-word            . vt-shell-vi-cmd)        ; R Sync point or do modify if in transient
      (query-replace       . vt-vi)                  ; SPC R Activate vi mode (TUI)
      (cut-text-block      . vt-conn-reconnect)      ; T
      (copy-text-block     . nil)                    ; SPC T
      (shrink-whitespaces  . vt-conn-localhost)      ; A
      (kill-line           . vt-close-window)        ; SPC A
      (open-line           . vt-prev-window)         ; S
      (del-back            . vt-shell-vi-cmd)        ; D Sync point or do modify if in transient
      (newline             . vt-next-window)         ; F
      (new-empty-buffer    . vt-new-window)          ; SPC G
      (toggle-comment      . vterm-read-send-key)    ; Z
      (cut-line            . vterm-clear)            ; X
      (paste-or-prev       . vterm-yank)             ; V
      (paste-from-r1       . paste-from-r1-vt)       ; SPC V
      (toggle-case         . vt-position)            ; B
      (toggle-prev-case    . vt-command-copy)        ; SPC B
      (revert-buffer       . prev-vterm-buf)         ; SPC 3
      (select-word         . nil)                    ; SPC SPC
      (org-ctrl-c-ctrl-c   . vterm-send-c-c)         ; SPC W

      ;; Right half
      (page-up-half        . vt-page-up-half)        ; DEL H
      (page-dn-half        . vt-page-dn-half)        ; DEL ;
      (dired-jump          . vt-conn-tramp)          ; DEL M
      (back-char           . vterm-left)
      (forw-char           . vterm-right)
      (up-line             . vterm-up)
      (down-line           . vterm-down)))

  ;; Sync point on insert
  (add-hook 'keyamp-insert-hook 'vterm-reset-cursor-point)

  (with-sparse-keymap
    (keyamp--map-leader keymap '(previous-line . next-line))
    (keyamp--remap keymap
      '((previous-line . vt-shell-vi-cmd-up) (next-line . vterm-down)))
    (keyamp--map-backtab keymap vt-shell-history)
    (keyamp--set keymap '(vt-shell-history) nil :insert)
    (keyamp--set keymap '(vt-shell-vi-cmd-up vterm-down vterm-yank-pop) :command))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(vt-shell-vi-cmd-up . vt-tmux-copy))
    (keyamp--map-tab keymap change-wd)
    (keyamp--map-backtab keymap vt-shell-history)
    (keyamp--set keymap '(vterm-send-return term-interrupt-subjob) nil :insert)
    (keyamp--set keymap '(vt-vi-save-quit vt-vi-quit))

    (defun keyamp-input-timer-payload-vterm ()
      "Set transient keymap for vterm after input timer timeout."
      (when (and (eq major-mode 'vterm-mode)
                 keyamp-insert-p)
        (keyamp-repeat-deactivate-init keymap)))

    (advice-add 'keyamp-input-timer-payload
                :after #'keyamp-input-timer-payload-vterm))

  (advice-add-macro '(vterm-send-return term-interrupt-subjob)
                    :after 'keyamp-input-timer)

  (with-sparse-keymap
    (keyamp--remap keymap
      '((bchar         . vterm-left)         (fchar     . vterm-right)
        (previous-line . vt-shell-vi-cmd-up) (next-line . vterm-down)))
    (keyamp--set keymap '(vterm-left vterm-right vterm-up vterm-down)))

  ;;;;;; Shell prompt vi cmd mode
  (with-sparse-keymap
    (keyamp--remap keymap
      '((bchar             . vt-shell-vi-self-insert)
        (fchar             . vt-shell-vi-l)
        (back-word         . vt-shell-vi-u)
        (forw-word         . vt-shell-vi-o)
        (del-back          . vt-shell-vi-d)
        (undo              . vt-shell-vi-self-insert)
        (beg-of-line       . vt-shell-vi-self-insert)
        (end-of-lyne       . vt-shell-vi-self-insert)
        (del-word          . vt-shell-vi-self-insert)
        (backward-del-word . vt-shell-vi-self-insert)))
    (keyamp--set keymap
      '(vt-shell-vi-cmd-up ; Vi cmd mode auto enable
        vterm-down         vt-shell-vi-del
        vt-shell-vi-cmd    vt-shell-vi-self-insert
        vt-shell-vi-l      vt-shell-vi-u
        vt-shell-vi-o      vt-shell-vi-d) nil nil nil nil
      'keyamp-blink-cursor-mode-activate
      'keyamp-blink-cursor-mode-deactivate)
    ;; Vi insert sync with keyamp
    (add-hook 'keyamp-insert-hook 'vt-shell-vi-insert)
    (advice-add 'vterm-set-point :after
                (lambda (&rest _) "transient"
                  (when (eq major-mode 'vterm-mode)
                    (let ((p (point)))
                      (vterm-reset-cursor-point)
                      (if (and (not (string-equal "*vterm clock*" (buffer-name)))
                               (eq (point) p))
                          (keyamp-repeat-init keymap 'keyamp-blink-cursor-mode-activate
                                              'keyamp-blink-cursor-mode-deactivate)
                        (goto-char p)))))))

  (with-sparse-keymap ; Move word repeat
    (keyamp--remap keymap '((bchar . vt-shell-vi-u) (fchar . vt-shell-vi-o)))
    (keyamp--set keymap '(vt-shell-vi-u vt-shell-vi-o)))

  (with-sparse-keymap ; Delete repeat
    (keyamp--map-leader keymap '(nil . vt-shell-vi-d))
    (keyamp--set keymap '(vt-shell-vi-d)))

  (with-sparse-keymap ; Forward delete repeat
    (keyamp--map-leader keymap '(nil . vt-shell-vi-del))
    (keyamp--remap keymap '((del-back . vt-shell-vi-del)))
    (keyamp--set keymap '(vt-shell-vi-del)))

  ;; Config Reference:

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
  ;; bindkey "^O" history-incremental-search-backward
  ;; # Force bind backward delete after vi cmd mode
  ;; bindkey "^?" backward-delete-char

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
    (keyamp--map-leader keymap '(vt-tmux-copy-self-insert . vt-tmux-copy-self-insert))
    (keyamp--map-return keymap vt-shell-vi-cmd) ; Quit and sync prompt position
    (keyamp--remap keymap
      '((previous-line   . vt-tmux-copy-self-insert)
        (bchar           . vt-tmux-copy-self-insert)
        (back-word       . vt-tmux-copy-self-insert)
        (forw-word       . vt-tmux-copy-self-insert)
        (next-line       . vt-tmux-copy-self-insert)
        (fchar           . vt-tmux-copy-self-insert)
        (beg-of-line     . vt-tmux-copy-self-insert)
        (end-of-lyne     . vt-tmux-copy-self-insert)
        (del-back        . vt-shell-vi-cmd)
        (copy-line       . vt-tmux-copy-self-insert)
        (activate-region . vt-tmux-copy-self-insert)
        (isearch-forward . vt-tmux-copy-self-insert)))
    (keyamp--set keymap
      '(vt-tmux-copy    vt-tmux-copy-self-insert
        vt-page-up-half vt-page-dn-half) :command))

  (with-sparse-keymap
    (keyamp--remap keymap
      '((previous-line . vt-page-up-half) (next-line . vt-page-dn-half)))
    (keyamp--map keymap
      '(("<up>" . vt-page-up-half) ("<down>" . vt-page-dn-half)))
    (when keyamp-touchp
      (keyamp--map keymap
        '(("<up>" . vt-page-dn-half) ("<down>" . vt-page-up-half))))
    (keyamp--set keymap '(vt-page-up-half vt-page-dn-half)))

    (when keyamp-touchp
      (with-sparse-keymap
        (keyamp--map keymap
          '(("<left>" . vt-next-window) ("<right>" . vt-prev-window)
            ("<up>"   . terminal)       ("<down>"  . vt-page-up-half)))
        (keyamp--set keymap
          '(terminal        vt-conn-localhost
            vt-next-window  vt-prev-window
            prev-vterm-buf  next-vterm-buf))))

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

  ;; bind -T copy-mode-vi Space send-keys -X halfpage-up
  ;; bind -T copy-mode-vi C-? send-keys -X halfpage-up
  ;; bind -T copy-mode-vi BSpace send-keys -X halfpage-down
  ;; bind -T copy-mode-vi h send-keys -X start-of-line
  ;; bind -T copy-mode-vi \; send-keys -X end-of-line

  ;; bind -T copy-mode-vi n command-prompt -T search -p "(search up)" { send-keys -X search-backward "%%" }

  ;; bind -T copy-mode-vi Tab send-keys -X search-reverse
  ;; bind -T copy-mode-vi BTab send-keys -X search-again

  ;;;;;; vi mode - keymap for TUI
  (with-sparse-keymap
    (keyamp--map-leader keymap '(vt-vi-self-insert . vt-vi-self-insert))
    (keyamp--map-escape keymap vt-vi-escape)
    (keyamp--map-return keymap vt-vi-self-insert)
    (keyamp--map-tab keymap vt-vi-self-insert)
    (keyamp--map-backtab keymap vt-vi-self-insert)
    (keyamp--map-std keymap 'vt-vi-self-insert)
    (keyamp--map keymap
      '(("<left>" . vt-vi-self-insert) ("<right>" . vt-vi-self-insert)
        ("<up>"   . vt-vi-self-insert) ("<down>"  . vt-vi-self-insert)
        ("C-q" . keyamp-command)))
    (keyamp--set keymap '(vt-vi vt-vi-self-insert vt-vi-escape) :command nil nil nil
      'keyamp-blink-cursor-mode-activate 'keyamp-blink-cursor-mode-deactivate))

  (defun vt-vi-auto (&rest _)
    "Activate vi mode automatically."
    (when (string-match " vi " vterm-last-command)
      (keyamp-input-timer-cancel)
      (keyamp-command)
      (keyamp-command-execute 'vt-vi)))

  (advice-add 'vterm-send-return :after #'vt-vi-auto '((depth . 90)))

  ;;;;;; Codex vi
  (with-sparse-keymap
    (keyamp--remap keymap
      '((bchar             . vt-codex-vi-left)
        (fchar             . vt-codex-vi-right)
        (previous-line     . vt-codex-vi-up)
        (next-line         . vt-codex-vi-down)
        (back-word         . vt-codex-vi-back-word)
        (forw-word         . vt-codex-vi-forward-word)
        (del-word          . vt-codex-vi-delete-word)
        (backward-del-word . vt-codex-vi-back-delw)
        (beg-of-line       . vt-codex-vi-bol)
        (end-of-lyne       . vt-codex-vi-eol)
        (paste-or-prev     . vt-codex-vi-yank)
        (paste-from-r1     . vt-codex-vi-paste-from-r1)))

    (keyamp--set keymap
      '(vt-codex-vi-sync-point   vt-codex-vi-paste-from-r1
        vt-codex-vi-left
        vt-codex-vi-right        vt-codex-vi-up
        vt-codex-vi-down         vt-codex-vi-back-word
        vt-codex-vi-forward-word vt-codex-vi-delete-word
        vt-codex-vi-back-delw    vt-codex-vi-bol
        vt-codex-vi-eol          vt-codex-vi-yank) :command))

  (with-sparse-keymap ; Move word repeat
    (keyamp--remap keymap
      '((bchar . vt-codex-vi-back-word) (fchar . vt-codex-vi-forward-word)))
    (keyamp--set keymap '(vt-codex-vi-back-word vt-codex-vi-forward-word)))

  (add-hook 'keyamp-insert-hook 'vt-codex-vi-insert)
  (advice-add 'vt-shell-vi-cmd :after #'vt-codex-vi-cmd)

  ;;;;;; config.toml
  ;; [tui.keymap.vim_normal]
  ;; enter_insert = "f12"
  ;; open_line_below = []
  ;; substitute_char = []
  ;; move_up = "i"
  ;; move_left = "j"
  ;; move_down = "k"
  ;; move_right = "l"
  ;; move_word_backward = "u"
  ;; move_word_forward = "o"
  ;; move_word_end = []
  ;; move_line_start = "h"
  ;; move_line_end = ";"
  ;; delete_char = "f"
  )

(defvar keyamp-ignore-map (make-sparse-keymap)
  "Keymap ignores any key. Maybe trigger action with post command hook.")

(keyamp--map-leader keyamp-ignore-map '(ignore . ignore))
(keyamp--map-escape keyamp-ignore-map ignore)
(keyamp--map-return keyamp-ignore-map ignore)
(keyamp--map-tab keyamp-ignore-map ignore)
(keyamp--map-backtab keyamp-ignore-map ignore)
(keyamp--map-std keyamp-ignore-map 'ignore)
(keyamp--map keyamp-ignore-map
  '(("<down-mouse-1>" . ignore) ("<mouse-1>" . ignore) ("<drag-mouse-1>" . ignore)
    ("<mouse-2>"      . ignore) ("<mouse-3>" . ignore)
    ("<left>"         . ignore) ("<right>"   . ignore)
    ("<up>"           . ignore) ("<down>"    . ignore)
    ("<home>"         . ignore) ("<end>"     . ignore)
    ("<prior>"        . ignore) ("<next>"    . ignore) ("<select>" . ignore)
    ("<f1>"  . ignore) ("<f2>"  . ignore) ("<f3>"  . ignore)
    ("<f4>"  . ignore) ("<f5>"  . ignore) ("<f6>"  . ignore)
    ("<f7>"  . ignore) ("<f8>"  . ignore) ("<f9>"  . ignore)
    ("<f10>" . ignore) ("<f11>" . ignore) ("<f12>" . ignore)
    ("<f13>" . ignore) ("<f14>" . ignore) ("<f15>" . ignore)
    ("<f16>" . ignore) ("<f17>" . ignore) ("<f18>" . ignore)
    ("<f19>" . ignore) ("<f20>" . ignore) ("<f21>" . ignore)
    ("<f22>" . ignore) ("<f23>" . ignore) ("<f24>" . ignore)))

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
      (beg-of-block  . gnus-topic-prev)    (end-of-block . gnus-topic-next)))
  (when keyamp-touchp
    (keyamp--remap gnus-topic-mode-map
      '((back-char . screen-home-right)  (forw-char . screen-home))))

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
    '((backward-del-word  . sun-moon)
      (undo               . split-window-below)
      (del-word           . gnus-group-enter-server-mode)
      (cut-text-block     . calc)
      (open-line          . prev-buf)
      (del-back           . gnus-group-get-new-news)
      (newline            . next-buf)
      (cut-line           . prev-eww-buf)
      (paste-or-prev      . tasks)
      (backward-bracket   . downloads)
      (forward-bracket    . save-close-buf)))
  (when keyamp-touchp
    (keyamp--remap gnus-group-mode-map
      '((back-char . screen-home-right) (forw-char . screen-home)))))

(with-eval-after-load 'gnus-art
  (push 'gnus-article-mode keyamp-lines-move-modes)
  (keyamp--remap gnus-mime-button-map '((keyamp-insert . gnus-article-press-button)))
  (keyamp--remap gnus-article-mode-map '((undo . button-back) (del-back . button-forw))))

(with-eval-after-load 'gnus-sum
  (push 'gnus-summary-mode keyamp-lines-move-modes)
  (keyamp--map gnus-summary-mode-map '(("<mouse-1>" . gnus-summary-scroll-up)))
  (keyamp--map-tab gnus-summary-mode-map nil)
  (keyamp--map-backtab gnus-summary-mode-map nil)
  (keyamp--remap gnus-summary-mode-map
    '((previous-line . up-line-rev)
      (next-line     . down-line)
      (keyamp-insert . gnus-summary-scroll-up)
      (open-line     . gnus-summary-prev-group)
      (del-back      . toggle-gnus)
      (newline       . gnus-summary-next-group)
      (paste-or-prev . tasks)
      (paste-from-r1 . gnus-summary-save-parts)))
  (when keyamp-touchp
    (keyamp--remap gnus-summary-mode-map
      '((back-char . screen-home-right) (forw-char . screen-home)
        (toggle-comment . hide-virtual-keyboard)))

    (with-sparse-keymap
      (keyamp--map-leader keymap '(page-dn-half . down-line))
      (keyamp--remap keymap
        '((open-line . gnus-summary-prev-group) (newline . gnus-summary-next-group)
          (up-line   . page-dn-half)))
      (keyamp--set keymap
        '(gnus-summary-prev-group gnus-summary-next-group gnus-delete-window-article))
      (keyamp--set keymap '(screen-home-left))
      (keyamp--hook keymap '(gnus-summary-prepared-hook) nil nil :repeat)))

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
      (terminal           . recentf-open-most-recent-file-3)
      (append-to-r1       . recentf-open-most-recent-file-4)
      (config             . recentf-open-most-recent-file-5)
      (search-string      . recentf-open-most-recent-file-6)
      (jump-to-register   . recentf-open-most-recent-file-7)
      (point-to-register  . recentf-open-most-recent-file-8)
      (proced-defer       . recentf-open-most-recent-file-9)))
  (with-sparse-keymap
    (keyamp--map-leader keymap '(widget-forward . widget-backward))
    (keyamp--set keymap '(recentf-open-files))))

(with-sparse-keymap
  (keyamp--remap keymap
    '((sh-defer          . radio-channel-0) (kmacro-record    . radio-channel-1)
      (kmacro-play       . radio-channel-2) (terminal         . radio-channel-3)
      (append-to-r1      . radio-channel-4) (config           . radio-channel-5)
      (search-string     . radio-channel-6) (jump-to-register . radio-channel-7)
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
      (del-back      . tetris-rotate-prev)
      (newline       . tetris-rotate-next) (next-buf . tetris-rotate-next)
      (next-line     . tetris-move-bottom) (bchar    . tetris-move-down)))

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

      ("w"   . nil) ; Required
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
  (keyamp--map-backtab find-output-mode-map nil)
  (keyamp--remap find-output-mode-map '((keyamp-insert . find--jump-to-place)))
  (keyamp--map find-output-mode-map '(("<mouse-1>" . find--jump-to-place)))

  (with-sparse-keymap
    (keyamp--map-leader keymap '(fchar . bchar))
    (keyamp--map keymap
      '(("<up>"   . find-previous-file)  ("<down>"  . find-next-file)
        ("<left>" . find-previous-match) ("<right>" . find-next-match)))
    (keyamp--remap keymap
      '((previous-line . find-previous-file)  (next-line . find-next-file)
        (bchar         . find-previous-match) (fchar     . find-next-match)))
    (keyamp--set keymap
      '(find-next-match find-previous-file find-previous-match find-next-file))
    (keyamp--hook keymap '(find-output-mode-hook) nil nil :repeat)))

(with-eval-after-load 'emacs-lisp-mode
  (keyamp--map-tab emacs-lisp-mode-map emacs-lisp-indent)
  (keyamp--map-backtab emacs-lisp-mode-map nil)
  (keyamp--remap emacs-lisp-mode-map
    '((reformat-lines . emacs-lisp-remove-paren-pair)
      (periodic-chart . emacs-lisp-remove-paren-pair))))

(with-eval-after-load 'perl-mode
  (keyamp--map-backtab perl-mode-map nil))

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
      (periodic-chart    . python-format-buffer)
      (describe-variable . xref-find-references)))
  (with-sparse-keymap
    (keyamp--map-leader keymap '(python-indent-or-complete . python-de-indent))
    (keyamp--set keymap '(python-indent-or-complete python-de-indent)
      nil nil nil 1)))

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
      (find-name-dired     . toggle-sql-async-remote)
      (reformat-lines      . sql-format-buffer)
      (empty-bin           . cb-generate-ddl)))
  (with-sparse-keymap
    (keyamp--remap keymap
      '((point-to-register . toggle-sql-type)
        (jump-to-register  . toggle-sql-async-conn)
        (search-string     . toggle-sql-async-remote)))
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

(with-eval-after-load 'neotree
  (keyamp--remap neotree-mode-map
    '((keyamp-insert . neotree-ret)
      (del-back      . neotree-quick-look))))

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
      (forward-bracket   . save-close-buf)))
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
      calc-mod  calc-inv   calc-power calc-enter) :after 'keyamp-input-timer)

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
  (keyamp--map-backtab dslide-mode-map nil)
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
                 toggle-prev-case                        t
                 undo                                    t)))

(defconst keyamp-read-commands-hash
  #s(hash-table test equal data
                (back-word-repeat                        t
                 button-back                             t
                 back-char                               t
                 beg-of-block                            t
                 beg-of-block-rev                        t
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
                 comp-back-rev                           t
                 comp-forw                               t
                 comp-forw-rev                           t
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
                 page-up-half-rev                        t
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
                 keyamp--hook-indicate                   t
                 minibuffer-previous-completion          t
                 minibuffer-next-completion              t
                 radio-next                              t
                 radio-prev                              t
                 recenter-top-bottom                     t
                 recentf-open-files                      t
                 screen-home-left                        t
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
                 vterm-send-return                       t
                 vt-vi-save-quit                         t
                 vt-vi-quit                              t
                 vt-shell-vi-cmd-up                      t
                 vterm-down                              t
                 vt-tmux-copy                            t
                 vt-page-up-half                         t
                 vt-page-dn-half                         t
                 vt-tmux-copy-self-insert                t
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
                 vt-vi-self-insert                       t
                 vt-vi-escape                            t
                 vt-vi                                   t
                 vt-tmux-copy                            t
                 vt-tmux-copy-self-insert                t
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
    emacs-lisp-indent           vterm-send-backspace)
  "List of commands to blink modify after.")

(defconst keyamp-blink-io-commands
  '(vt-shell-vi-cmd     vt-shell-vi-self-insert    vt-shell-vi-l
    vt-shell-vi-u       vt-shell-vi-o              vt-shell-vi-d
    vt-tmux-copy        vt-tmux-copy-self-insert   vterm-read-send-key
    vt-vi               vt-vi-self-insert          vt-vi-escape
    vt-shell-vi-del     vterm-left                 vterm-right
    vterm-up            vterm-down                 vterm--self-insert
    copy-to-r1          append-to-r1)
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

(defconst keyamp-karabinerp (executable-find keyamp-karabiner-cli)
  "Karabiner use predicate.")

(defconst keyamp-indicator-default   "•" "Default keyamp indicator.")
(defconst keyamp-indicator-hand-swap "∘" "Hand swap indicator.")
(defconst keyamp-indicator-standard  "•" "Standard keyboard indicator.")
(defconst keyamp-indicator-input     "›" "Input keyamp indicator.")

(defconst keyamp-transient-states '(idle screen read command io insert modify)
  "Keyamp transient states.")

(defun keyamp-def-indicators ()
  "Define keyamp indicators variables."
  (mapc
   (lambda (type)
     (mapc
      (lambda (state)
        (let ((var (intern (format "keyamp-%s-indicator-%s" state type))))
          (set-default var (symbol-value
                            (intern (format "keyamp-indicator-%s" type))))
          (put var 'variable-documentation
               (format "Indicator %s %s." (symbol-name state) type)))
        (when-let (((string-equal type "default"))
                   (var (intern (format "keyamp-%s-indicator" state))))
          (set-default var (symbol-value
                            (intern (format "keyamp-indicator-%s" type))))
          (put var 'variable-documentation
               (format "Indicator %s." (symbol-name state)))))
      keyamp-transient-states))
   '("default" "hand-swap" "standard")))

(keyamp-def-indicators)

(defun keyamp-use-indicators (Type)
  "Set keyamp indicators to variants ending with TYPE."
  (mapc
   (lambda (state)
     (let ((var (intern (format "keyamp-%s-indicator" state))))
       (set var (symbol-value (intern (format "%s-%s" var Type))))))
   keyamp-transient-states))

(defvar keyamp-idle-color     "#AB82FF" "Idle color.")
(defvar keyamp-idle2-color    "#B38EFF" "Idle accent color.")
(defvar keyamp-screen-color   "#1E90FF" "Screen color.")
(defvar keyamp-read-color     "#00BFFF" "Read color.")
(defvar keyamp-command-color  "#7CFC00" "Command color.")
(defvar keyamp-command2-color "#5FFF00" "Command accent color.")
(defvar keyamp-io-color       "#FFD700" "IO color.")
(defvar keyamp-insert-color   "#FF8C00" "Insert color.")
(defvar keyamp-modify-color   "#FF0000" "Modify color.")

(defface mode-line-front-space-face
  `((t :foreground ,keyamp-command-color :bold nil))
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

(defun keyamp-input-timer-cancel ()
  "Cancel `keyamp-input-timer'."
  (remove-hook 'pre-command-hook 'keyamp-input-timer-cancel)
  (remove-hook 'post-self-insert-hook 'keyamp-input-timer-cancel)
  (when (timerp keyamp-input-timer)
    (cancel-timer keyamp-input-timer)))

(defun keyamp-input-timer (&rest _)
  "Start `keyamp-input-timer'."
  (keyamp-input-timer-cancel)
  (add-hook 'pre-command-hook 'keyamp-input-timer-cancel)
  (add-hook 'post-self-insert-hook 'keyamp-input-timer-cancel)
  (setq keyamp-input-timer
        (run-with-timer keyamp-input-timeout nil 'keyamp-input-timer-payload)))

(defun keyamp-input-timer-payload ()
  "Payload for `keyamp-input-timer'."
  (keyamp-input-timer-cancel)
  (when keyamp-insert-p
    (keyamp-command)
    (keyamp-indicate-read-defer)
    (keyamp-blink-start keyamp-command-color keyamp-read-color)))


;; Karabiner

(defun keyamp-set-var-karabiner (K V)
  "Set karabiner variable K to value V via shell command."
  (call-process keyamp-karabiner-cli nil 0 nil
                "--set-variables" (concat "{\"" K "\":" V "}")))

(defconst keyamp-karabiner-insert-mode "insert mode activated"
  "Karabiner keyamp insert mode variable.")

(defconst keyamp-karabiner-hand-swap "hand swap activated"
  "Karabiner hand swap variable.")

(defun keyamp-insert-karabiner ()
  "Sync insert mode with karabiner."
  (keyamp-set-var-karabiner keyamp-karabiner-insert-mode "1"))

(defun keyamp-command-karabiner ()
  "Sync command mode with karabiner."
  (keyamp-set-var-karabiner keyamp-karabiner-insert-mode "0"))

(defun keyamp-karabiner-init ()
  "Init karabiner."
  (when keyamp-karabinerp
    (add-hook 'keyamp-insert-hook    'keyamp-insert-karabiner)
    (add-hook 'keyamp-command-hook   'keyamp-command-karabiner)
    (add-hook 'isearch-mode-hook     'keyamp-insert-karabiner)
    (add-hook 'isearch-mode-end-hook 'keyamp-command-karabiner)
    (add-hook 'minibuffer-setup-hook 'keyamp-insert-karabiner)
    (add-hook 'kill-emacs-hook       'keyamp-karabiner-deactivate)
    (keyamp-command-karabiner)))

(defun keyamp-karabiner-deactivate ()
  "Deactivate karabiner."
  (when keyamp-karabinerp
    (remove-hook 'keyamp-insert-hook    'keyamp-insert-karabiner)
    (remove-hook 'keyamp-command-hook   'keyamp-command-karabiner)
    (remove-hook 'isearch-mode-hook     'keyamp-insert-karabiner)
    (remove-hook 'isearch-mode-end-hook 'keyamp-command-karabiner)
    (remove-hook 'minibuffer-setup-hook 'keyamp-insert-karabiner)
    (keyamp-insert-karabiner)))


;; Modes

(defvar keyamp-insert-p t "Non-nil means insert is on.")
(defvar keyamp--deactivate-command-fun nil "Deactivate command mode function.")
(defconst keyamp-blink-cursor-mode t
  "Use blink cursor to indicate transient maps like vi or recursive modes like
 isearch. Disable blink cursor with `keyamp-command-init'.")

(defun keyamp-blink-cursor-mode-activate ()
  "Activate function of transient map indication with blink cursor."
  (when keyamp-blink-cursor-mode
    (blink-cursor-mode 1)))

(defun keyamp-blink-cursor-mode-deactivate ()
  "Deactivate function of transient map indication with blink cursor."
  (when keyamp-blink-cursor-mode
    (blink-cursor-mode -1)))

(defun keyamp-command-init ()
  "Set command mode."
  (keyamp-repeat-deactivate)
  (when keyamp-insert-p
    (setq keyamp-insert-p nil)
    (when (buffer-file-name)
      (point-to-register ?7)
      (push-mark (point) t)))
  (when keyamp--deactivate-command-fun
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

(defun keyamp-SPC-SPC (&rest _)
  "Insert fast SPC SPC to activate command mode and save. Quit minibuffer."
  (if-let (((keyamp-unless-kbd-macro))
           (space ?\s)
           ((eq before-last-command-event space))
           ((eq last-command-event space)))
      (cond
       (isearch-mode
        (isearch-cancel-clean))
       ((eq major-mode 'vterm-mode)
        (vterm-send-backspace)
        (vterm-send-backspace)
        (keyamp-command-execute 'keyamp-command))
       ((minibufferp)
        (keyamp-minibuffer-quit))
       (t
        (delete-char (1- -1))
        (when-let ((file (buffer-file-name))
                   ((not (file-remote-p file))))
          (save-buffer-silent-defer))
        (keyamp-command-execute 'keyamp-command)))
    (if (eq last-command-event space)
        (before-last-command-event space)
      (setq before-last-command-event nil))))

(add-hook 'post-self-insert-hook 'keyamp-SPC-SPC)
(advice-add-macro
 '(isearch-printing-char minibuffer-complete-word vterm--self-insert)
 :after 'keyamp-SPC-SPC)

(defun keyamp-SPC-DEL (&rest _)
  "Insert fast SPC DEL to move char forward while in insert mode."
  (when-let (((keyamp-unless-kbd-macro))
             (space ?\s)
             ((eq before-last-command-event space)))
    (keyamp-command-execute 'fchar)))

(advice-add 'delete-backward-char :after #'keyamp-SPC-DEL)

(defun keyamp-command-if-insert (&rest _)
  "Activate command mode if insert mode."
  (when keyamp-insert-p
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
    (when (and (default-value 'delete-selection-mode)
               (use-region-p))
      (kill-region (region-beginning) (region-end)))
    (keyamp-insert)
    (if-let (((get 'toggle-hand-swap 'state))
             (key (this-command-keys)))
        (progn
          (when (vectorp key)
            (setq key (char-to-string (aref key 0))))
          (if (> (string-to-char key) (1- (expt 2 7)))
              (setq key (car (rassoc key keyamp--hand-swap)))
            (setq key (keyamp--convert-kbd-str
                       (car
                        (rassoc
                         (or (car (rassoc key keyamp--convert-table))
                             key)
                         keyamp--hand-swap)))))
          (if key ; This key code comes from OS
              (execute-kbd-macro (kbd key))
            (self-insert-command 1)))
      (self-insert-command 1)))))

(defun keyamp-minibuffer-return ()
  "Exit if file completion. It means use content of minibuffer as it is,
no select completion candidates. Else force complete and exit, that
is, select and use first completion candidate. In case file
completion, for most cases no need to complete, because there is NO
right candidate. Otherwise, in all cases one MUST select a candidate.
Hit C-t to minibuffer-complete file name if the name exists."
  (interactive)
  (if (eq (icomplete--category) 'file)
      (exit-minibuffer)
    (icomplete-force-complete-and-exit)))

(defun keyamp-minibuffer-y-or-n-literal ()
  "Return t if asked literal y or n question."
  (when-let ((str (minibuffer-prompt)))
    (string-match "y, n, !\\|yn!q" str)))

(defun keyamp-minibuffer-y-or-n ()
  "Return t if asked non-literal y or n question."
  (when-let ((str (minibuffer-prompt)))
    (string-match "y or n" str)))

(defun keyamp-insert-minibuffer ()
  "Answer to y or n question if asked or answer literal y or n question if asked.
Else activate insert mode and self insert."
  (interactive)
  (let ((key (this-command-keys)))
    (when (vectorp key)
      (setq key (char-to-string (aref key 0))))
    (when-let ((key-standard (cdr (assoc key keyamp-input-methods-to-std)))
               ((> (string-to-char key) (1- (expt 2 7)))))
      (setq key (keyamp--convert-kbd-str key-standard)))
    (cond
     ((keyamp-minibuffer-y-or-n)
      (if (string-equal key (keyamp--convert-kbd-str "k")) ; QWERTY K
          (y-or-n-p-insert-y)
        (y-or-n-p-insert-n)))
     ((keyamp-minibuffer-y-or-n-literal)
      (keyamp-insert-init) ; No hook run for single char insert
      (when (get 'toggle-std-to-cur-layout 'state) ; Convert back to current layout
        (setq key (car (rassoc key keyamp--convert-table))))
      (execute-kbd-macro (kbd key))) ; Key press required
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
          (when (get 'toggle-std-to-cur-layout 'state)
            (setq key (car (rassoc key keyamp--convert-table))))
          (execute-kbd-macro (kbd key))))
    (if (keyamp-minibuffer-empty)
        (keyamp-minibuffer-quit)
      (keyamp-escape))))

(defun keyamp-minibuffer-empty ()
  "Return true if minibuffer prompt empty."
  (and (minibufferp)
       (zerop (- (buffer-size) (length (minibuffer-prompt))))))

(defun keyamp-minibuffer-shift-up ()
  "Quit minibuffer and call some command. Single motion switch."
  (interactive)
  (keyamp-defer-command
   (cdr (assoc (minibuffer-prompt)
               '(("M-x"           . buf-or-bookmark)
                 ("Buffer"        . describe-function)
                 ("Secret copy"   . pass-otp)
                 ("Secret OTP"    . pass-user)
                 ("Query replace" . query-replace-regexp))
               #'string-match-p)))
  (abort-recursive-edit))

(defun keyamp-minibuffer-shift-down ()
  "Quit minibuffer and call some command. Single motion switch."
  (interactive)
  (keyamp-defer-command
   (cdr (assoc (minibuffer-prompt)
               '(("M-x"         . isearch-forward)
                 ("Buffer"      . execute-extended-command)
                 ("Secret copy" . pass-user)
                 ("Secret user" . pass-otp))
               #'string-match-p)))
  (abort-recursive-edit))

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

(defun keyamp-indicator-color (Color &optional Bold)
  "Set `mode-line-front-space-face` face COLOR."
  (set-face-attribute 'mode-line-front-space-face nil
                      :foreground Color :weight (if Bold 'bold 'normal)))

(defun keyamp-indicate (Indicator Cursor Color &optional Bold)
  "Indicate mode with INDICATOR, CURSOR and COLOR."
  (keyamp-cancel-indicate-read-timer)
  (keyamp-indicator Indicator)
  (keyamp-cursor-type Cursor)
  (keyamp-indicator-color Color Bold))

(defun keyamp-indicate-read ()
  "Indicate read."
  (unless keyamp-insert-p
    (keyamp-indicate keyamp-read-indicator keyamp-read-cursor keyamp-read-color))
  (when (and (eq this-command 'keyamp--hook-indicate)
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

(defun keyamp-repeat-init (Keymap &optional On-enter On-exit)
  "Init repeat mode.
Optional arg ON-ENTER, if non-nil, specifies a function that is
called, with no arguments, before KEYMAP is activated.
Optional arg ON-EXIT, if non-nil, specifies a function that is
called, with no arguments, after KEYMAP is deactivated."
  (setq keyamp-repeat-p t)
  (when (functionp On-enter)
    (funcall On-enter))
  (setq keyamp--deactivate-repeat-fun (set-transient-map Keymap nil On-exit)))

(defun keyamp-cancel-repeat-idle-timer ()
  "Cancel `keyamp--repeat-idle-timer'."
  (when (timerp keyamp--repeat-idle-timer)
    (cancel-timer keyamp--repeat-idle-timer)))

(defun keyamp-repeat-deactivate ()
  "Deactivate repeat."
  (when keyamp-repeat-p
    (setq keyamp-repeat-p nil))
  (when keyamp--deactivate-repeat-fun
    (funcall keyamp--deactivate-repeat-fun))
  (keyamp-cancel-repeat-idle-timer))

(defun keyamp-repeat-deactivate-init (Keymap)
  "Deactivate repeat and init repeat KEYMAP."
  (keyamp-repeat-deactivate)
  (keyamp-repeat-init Keymap))

(defun keyamp-indicate-idle ()
  "Indicate idle."
  (keyamp-blink-start keyamp-idle2-color keyamp-idle-color))

(defun keyamp-indicate-sleep ()
  "Indicate sleep."
  (keyamp-blink-start keyamp-idle-color
                      (if (display-graphic-p)
                          (face-attribute 'default :background)
                        "white")))

(defun keyamp-indicate-screen ()
  "Indicate screen."
  (keyamp-indicate keyamp-screen-indicator keyamp-screen-cursor keyamp-screen-color)
  (cond
   ((gethash this-command keyamp-screen-read-commands-hash)
    (keyamp-blink-start keyamp-read-color keyamp-screen-color))
   ((eq this-command 'save-close-buf)
    (keyamp-blink-start keyamp-modify-color keyamp-screen-color))
   (t
    (keyamp-blink-start keyamp-command-color keyamp-screen-color))))

(defun keyamp-indicate-command ()
  "Indicate command."
  (keyamp-blink-stop)
  (keyamp-indicate keyamp-command-indicator keyamp-command-cursor keyamp-command-color)
  (cond
   ((memq this-command keyamp-screen-command-commands)
    (keyamp-blink-start keyamp-screen-color keyamp-command-color))
   (t
    (keyamp-blink-start keyamp-command2-color keyamp-command-color))))

(defun keyamp-indicate-io (&rest _)
  "Indicate io feedback from emacsclient evals or processes calls."
  (oset keyamp-blinker-io indicator 'keyamp-io-indicator)
  (keyamp-blink keyamp-blinker-io))

(defun keyamp-indicate-input ()
  "Indicate input."
  (keyamp-indicate keyamp-indicator-input keyamp-insert-cursor keyamp-insert-color)
  (cond
   ((gethash this-command keyamp-read-commands-hash)
    (keyamp-blink-start keyamp-read-color keyamp-insert-color))
   ((memq this-command keyamp-insert-commands)
    (oset keyamp-blinker-io indicator 'keyamp-indicator-input)
    (keyamp-blink keyamp-blinker-io))
   ((gethash this-command keyamp-modify-commands-hash)
    (oset keyamp-blinker-modify indicator 'keyamp-indicator-input)
    (keyamp-blink keyamp-blinker-modify))))

(defun keyamp-indicate-insert ()
  "Indicate insert."
  (keyamp-indicate keyamp-insert-indicator keyamp-insert-cursor keyamp-insert-color)
  (cond
   ((gethash this-command keyamp-read-commands-hash)
    (keyamp-blink-start keyamp-read-color keyamp-insert-color))
   ((memq this-command keyamp-insert-commands)
    (oset keyamp-blinker-io indicator 'keyamp-io-indicator)
    (keyamp-blink keyamp-blinker-io))
   ((gethash this-command keyamp-modify-commands-hash)
    (oset keyamp-blinker-modify indicator 'keyamp-modify-indicator)
    (keyamp-blink keyamp-blinker-modify))))

(defun keyamp-indicate-modify ()
  "Indicate modify."
  (keyamp-indicate keyamp-modify-indicator keyamp-modify-cursor keyamp-modify-color)
  (when (eq this-command 'undo) ; Other repeatable modify have timeout
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
        (progn) ; Ease scroll
      (cond
       ((and isearch-mode
             (not (memq this-command keyamp-isearch-not-insert)))
        (keyamp-indicate-input))
       (keyamp-insert-p
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
       (t
        (keyamp-indicate-command)))
      (cond
       ((or defining-kbd-macro
            (memq this-command keyamp-blink-modify-commands))
        (oset keyamp-blinker-modify indicator 'keyamp-modify-indicator)
        (keyamp-blink keyamp-blinker-modify))
       ((eq this-command 'keyamp-escape)
        (keyamp-blink keyamp-blinker-idle))
       ((memq this-command '(ignore keyamp-ignore))
        (oset keyamp-blinker-idle indicator 'keyamp-idle-indicator)
        (keyamp-blink keyamp-blinker-command))
       ((memq this-command keyamp-blink-io-commands)
        (oset keyamp-blinker-io indicator 'keyamp-io-indicator)
        (keyamp-blink keyamp-blinker-io))
       ((memq this-command keyamp-blink-command-commands)
        (oset keyamp-blinker-command indicator 'keyamp-command-indicator)
        (keyamp-blink keyamp-blinker-command))))))

(defvar keyamp-blink-on-timer nil "Blink indicator on timer.")
(defvar keyamp-blink-off-timer nil "Blink indicator off timer.")

(defconst keyamp-blink-duration (/ 1.0 2) "Blink duration.")
(defconst keyamp-blink-period (+ keyamp-blink-duration 3) "Blink period.")

(defun keyamp-blinking (Color1 Color2)
  "Blinking."
  (keyamp-indicator-color Color1 t)
  (when (timerp keyamp-blink-off-timer)
    (cancel-timer keyamp-blink-off-timer))
  (setq keyamp-blink-off-timer
        (run-with-timer keyamp-blink-duration nil 'keyamp-indicator-color Color2)))

(defun keyamp-blink-stop ()
  "Stop blink."
  (remove-hook 'post-command-hook 'keyamp-blink-stop)
  (when (timerp keyamp-blink-off-timer)
    (cancel-timer keyamp-blink-off-timer))
  (when (timerp keyamp-blink-on-timer)
    (cancel-timer keyamp-blink-on-timer)))

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
    :initform (symbol-value 'keyamp-blink-duration)
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
    (keyamp-indicate (symbol-value (oref obj indicator))
                     (oref obj curCursor) (symbol-value (oref obj color)) t)
    (when (timerp (oref obj timer))
      (cancel-timer (oref obj timer)))
    (oset obj timer (run-with-timer (oref obj duration) nil 'keyamp-blink-end obj))))

(cl-defgeneric keyamp-blink-end (obj) "End blink.")

(cl-defmethod keyamp-blink-end ((obj keyamp-blinker))
  "End blink with blinker. Reset values only if no changes."
  (when (and (eq (symbol-value (oref obj indicator)) mode-line-front-space)
             (eq (symbol-value (oref obj color))
                 (face-attribute 'mode-line-front-space-face :foreground))
             (eq (oref obj curCursor) (frame-parameter nil 'cursor-type)))
    (keyamp-indicate (oref obj curIndicator)
                     (oref obj curCursor) (oref obj curColor))))

(defconst keyamp-blink-idle-duration (/ keyamp-blink-duration 2)
  "Blink idle duration.")

(defconst keyamp-blinker-idle
  (keyamp-blinker :indicator 'keyamp-idle-indicator :color 'keyamp-idle-color
                  :duration keyamp-blink-idle-duration)
  "Blinker idle.")

(defconst keyamp-blinker-screen
  (keyamp-blinker :indicator 'keyamp-screen-indicator :color 'keyamp-screen-color
                  :duration keyamp-blink-idle-duration)
  "Blinker screen.")

(defconst keyamp-blinker-command
  (keyamp-blinker :indicator 'keyamp-command-indicator :color 'keyamp-command-color)
  "Blinker command.")

(defconst keyamp-blinker-io
  (keyamp-blinker :indicator 'keyamp-io-indicator :color 'keyamp-io-color)
  "Blinker io.")

(defconst keyamp-blinker-insert
  (keyamp-blinker :indicator 'keyamp-insert-indicator :color 'keyamp-insert-color)
  "Blinker insert.")

(defconst keyamp-blinker-modify
  (keyamp-blinker :indicator 'keyamp-modify-indicator :color 'keyamp-modify-color)
  "Blinker modify.")

(defconst keyamp-prefix-io
  `([?\s] [?\d] [backspace]
    [?\s ,(string-to-char (keyamp--convert-kbd-str "i"))]
    [?\s ,(string-to-char (keyamp--convert-kbd-str "j"))]
    [?\s ,(string-to-char (keyamp--convert-kbd-str "k"))]
    [?\C-h]
    [?\C-_ ,(string-to-char (keyamp--convert-kbd-str "n"))]
    [?\C-И ,(string-to-char (car (rassoc "n" keyamp-input-methods-to-std)))])
  "Indicate prefixes with io.")

(defconst keyamp-prefix-modify
  `([?\d ,(string-to-char (keyamp--convert-kbd-str "e"))]
    [?\d ,(string-to-char (keyamp--convert-kbd-str "d"))]
    [?\d ,(string-to-char (keyamp--convert-kbd-str "f"))]
    [backspace ,(string-to-char (keyamp--convert-kbd-str "e"))]
    [backspace ,(string-to-char (keyamp--convert-kbd-str "d"))]
    [backspace ,(string-to-char (keyamp--convert-kbd-str "f"))])
  "Indicate prefixes with modify.")

(defconst keyamp-blink-flash 0.3 "Blink flash duration.")

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

(defvar keyamp-prefix-delay (/ 50 1000.0) "Delay before indicate prefix keymap.")

(defun keyamp-prefix ()
  "Run `keyamp-indicate-prefix' with idle timer."
  (run-with-idle-timer keyamp-prefix-delay t 'keyamp-indicate-prefix))

(defvar keyamp-idle-timer nil "Idle timer.")

(defun keyamp-prefix-key-waiting-p ()
  "If a prefix key sequence is waiting for more input."
  (when-let ((keys (this-command-keys-vector))
             ((cl-plusp (length keys))))
    (keymapp (key-binding keys))))

(defun keyamp-idle-init ()
  "Idle init.
Cancel isearch. Deactivate region. Deactivate transient keymaps.
Cleanup echo area. Quit minibuffer. Indicate idle. Hide which key popup.
Quit wait key sequence."
  (let ((default-directory (expand-file-name "~/")))
    (when isearch-mode
      (isearch-cancel-clean))
    (when (region-active-p)
      (deactivate-mark))
    (when (get 'toggle-hand-swap 'state)
      (toggle-hand-swap))
    (keyamp-command)
    (when-let ((buf (get-buffer " *Echo Area 0*"))
               ((cl-plusp (buffer-size buf))))
      (run-at-time nil nil 'message nil))
    (when (minibufferp)
      (keyamp-minibuffer-quit))
    (when (fboundp 'minibuffer-line)
      (minibuffer-line))
    (save-some-buffers t) ; Adjust auto-save-visited-predicate
    (keyamp-indicate-idle)
    (when (and (fboundp 'which-key--popup-showing-p)
               (which-key--popup-showing-p))
      (which-key--hide-popup))
    (when (keyamp-prefix-key-waiting-p) ; Defer to let keyamp-idle-init finish without error
      (run-with-timer 1 nil 'keyboard-quit))))

(defun keyamp-idle-detect ()
  "Idle detect."
  (when (timerp keyamp-idle-timer)
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
   ((region-active-p)
    (deactivate-region))
   ((or keyamp-repeat-p
        keyamp-insert-p)
    (keyamp-command))
   ((minibufferp)
    (keyamp-minibuffer-quit))
   (t
    (keyamp-command))))

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
    (advice-add 'debugger-setup-buffer :after (lambda (&rest _) (keyamp-command)))
    (add-function :after after-focus-change-function #'keyamp-command-if-insert)))

(provide 'keyamp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars lexical unresolved)
;; End:
;;; keyamp.el ends here
