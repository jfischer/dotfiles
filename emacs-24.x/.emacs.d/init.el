(setq load-path (cons "~/.emacs.d/lisp" load-path))

(when (featurep 'aquamacs)
 ;; code for aquamacs goes here
 '())

;; my preferred keybindings
(global-set-key (kbd "C-x G") 'goto-line)

;; commands enabled when using a real window system
(if (display-graphic-p)
    (load-library "frame-cmds") ; needed to maximize frame
    (global-set-key (kbd "M-RET") 'toggle-max-frame))

(defun prev-window ()
  "Move the focus to the previous window. This is the opposite of, C-x o, a.k.a. other-window."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x p") 'prev-window)

;; Highlight the character in column 80.
;; See http://www.emacswiki.org/emacs/ColumnMarker
;; To use manually, run C-u 80 M-x column-marker-1
(load-library "column-marker")

; set column marker automatically for python and rst buffers
(setq python-mode-hook
 (lambda ()
  (nlinum-mode)
  (column-marker-1 80)))
(require 'rst)
(setq rst-mode-hook
 (lambda ()
  (column-marker-1 80)))

;; define function to run pyflakes on the current (python) buffer
(defun get-default-pyflakes-path ()
  (let ((pyflakes-where-result
	(replace-regexp-in-string "\n\\'" "" 
				  (shell-command-to-string "which pyflakes"))))
    (if (equal pyflakes-where-result "")
	"NEED-TO-SET-PYFLAKES_PATH")
     pyflakes-where-result))

(defvar pyflakes-path
  (get-default-pyflakes-path)
  "Path to pyflakes utility")
(defun pyflakes ()
  "Run the pyflakes utility on the current buffer"
  (interactive)
  (if (not (equal mode-name "Python"))
      (error
       (concat "pyflakes only runs on buffers in Python mode, this buffer is '"
	       mode-name "'")))
  (compile (concat pyflakes-path " " buffer-file-name)))
(global-set-key (kbd "<f1> p") 'pyflakes)

;; add other package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; programming language modes
;; The version is hard-coded in this - not sure why emacs doesn't
;; do this automatically.
(add-to-list 'load-path "~/.emacs.d/elpa/go-mode-20160715.205")
(require 'go-mode-autoloads)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
