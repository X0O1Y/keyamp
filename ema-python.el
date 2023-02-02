;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'elpy)
(elpy-enable)
(setq elpy-rpc-virtualenv-path "~/.emacs.d/elpy")

(defun ema-run-python ()
  "Run and show python shell, do not select other window.
Version 2022-11-08"
  (interactive)
  (python-shell-make-comint
          (python-shell-calculate-command)
          (python-shell-get-process-name nil) t)
  (other-window 1))

;; disable flymake proc legacy warning messages
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(defun ema-pdb ()
  "Debug current file with pdb.
Verstion 2022-12-22"
  (interactive)
  (pdb (concat "python3 -m pdb " (file-name-nondirectory buffer-file-name))))


;; bindings

(define-key python-mode-map (kbd "TAB")          'python-indent-shift-right)
(define-key python-mode-map (kbd "<backtab>")    'python-indent-shift-left)
(define-key python-mode-map (kbd "<f7> j k")     'xah-python-format-buffer)


;; format python code

(defvar xah-python-formatter-software
  "black" "value should be string, either “black” or “yapf”.")

(defvar xah-python-formatter-black-path
  "black --skip-string-normalization"
  "full path or name of python code formatter “black”")

(defvar xah-python-formatter-yapf-path "yapf"
  "full path or name of python code formatter yapf")

(defun xah-python-format-buffer ()
  "Format the current buffer file by calling `xah-python-formatter-software'.
If buffer is a file, it is saved first.

URL `http://xahlee.info/emacs/emacs/xah_format_python_code.html'
Version 2022-08-25 2022-08-28"
  (interactive)
  (let (($buffFileName (buffer-file-name)))
    (if $buffFileName
        (progn
          (save-buffer)
          (cond
           ((string-equal xah-python-formatter-software "black")
            (shell-command
             (format "%s %s -q" xah-python-formatter-black-path $buffFileName)))
           ((string-equal xah-python-formatter-software "yapf")
            (shell-command
             (format "%s -i %s" xah-python-formatter-yapf-path $buffFileName)))
           (t
            (user-error "`xah-python-formatter-software' should be “black” or “yapf”")))
          (revert-buffer t t t))
      (xah-python-format-region (point-min) (point-max))))
  ;; (user-error "buffer should be a file. Use `xah-python-format-region' instead.")
  )

(defun xah-python-format-region (Begin End)
  "Format the current region file using `xah-python-formatter-software'.
The region must be a valid python code.
File is saved first.
URL `http://xahlee.info/emacs/emacs/xah_format_python_code.html'
Version 2022-08-25 2022-08-28"
  (interactive "r")
  (cond
   ((string-equal xah-python-formatter-software "black")
    (shell-command-on-region
     Begin End
     (format "%s - -q" xah-python-formatter-black-path)
     nil
     t))))
