;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'elpy)

(advice-add 'python-mode :before 'elpy-enable)
(setq elpy-rpc-virtualenv-path "~/.emacs.d/elpy")

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; disable flymake proc legacy warning messages
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(defun run-python ()
  "Run and show python shell, do not select other window."
  (interactive)
  (python-shell-make-comint
          (python-shell-calculate-command)
          (python-shell-get-process-name nil) t)
  (other-window 1))

(defun run-pdb ()
  "Debug current file with pdb."
  (interactive)
  (pdb (concat "python3 -m pdb " (file-name-nondirectory buffer-file-name))))

(defun python-indent ()
  "Do indent."
  (interactive)
  (let (($curChar (char-before)))
    (if (not (or (eq $curChar 32) (eq $curChar 9) (eq $curChar 10)))
        (progn
          (nil-beginning-of-line-or-block)
          (insert "    "))
      (insert "    "))))


;; bindings

(define-key python-mode-map (kbd "TAB")          'python-indent)
(define-key python-mode-map (kbd "<backtab>")    'python-indent-shift-left)
(define-key python-mode-map (kbd "<f7> j k")     'python-format-buffer)


;; formatter

(defvar python-formatter-software "black"
  "value should be string, either “black” or “yapf”.")

(defvar python-formatter-black-path "black --skip-string-normalization"
  "full path or name of python code formatter “black”")

(defvar python-formatter-yapf-path "yapf"
  "full path or name of python code formatter yapf")

(defun python-format-buffer ()
  "Format the current buffer file by calling `nil-python-formatter-software'.
If buffer is a file, it is saved first."
  (interactive)
  (let (($buffFileName (buffer-file-name)))
    (if $buffFileName
        (progn
          (save-buffer)
          (cond
           ((string-equal python-formatter-software "black")
            (shell-command
             (format "%s %s -q" python-formatter-black-path $buffFileName)))
           ((string-equal python-formatter-software "yapf")
            (shell-command
             (format "%s -i %s" python-formatter-yapf-path $buffFileName)))
           (t
            (user-error "`nil-python-formatter-software' should be “black” or “yapf”")))
          (revert-buffer t t t))
      (python-format-region (point-min) (point-max))))
  )

(defun python-format-region (Begin End)
  "Format the current region file using `nil-python-formatter-software'.
The region must be a valid python code. File is saved first."
  (interactive "r")
  (cond
   ((string-equal python-formatter-software "black")
    (shell-command-on-region
     Begin End
     (format "%s - -q" python-formatter-black-path)
     nil
     t))))
