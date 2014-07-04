;; eddie-common.el

;; add this to 
;; (add-to-list 'load-path "~/.emacs.d")
;; (require 'eddie-common)
;; mkdir ~/.saves
;; TODO
;;(add-to-list 'load-path "/usr/local/go/misc/emacs")
;;(require 'go-mode-load)
(provide 'eddie-common)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; left side line number
(global-linum-mode t)

;; save open files and place in file
(custom-set-variables
  '(desktop-enable t nil (desktop))
  '(save-place t nil (saveplace)))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
(setq require-final-newline 'query)

(global-set-key [f5] 'compile)
(global-set-key [f6] 'find-grep-dired)
(global-set-key [f4] 'svn-status)
(global-set-key [f2] 'delete-trailing-whitespace)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(line-number-mode t)    ; makes the line number show up
(column-number-mode t)  ; makes the column number show up
(tool-bar-mode nil)

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t)
	    (setq tab-width 2)))

;;(desktop-save-mode t)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t)

(require 'psvn)
(defun eddie-timestamp ()
  "Spit out the current time"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun eddie-sign ()
  "spit out my name, email and the current time"
  (interactive)
  (insert "-- Eddie McCreary <software@heorot.com>")
  (eddie-timestamp))

(defun save-framegeometry ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/framegeometry."
  (let (
        (framegeometry-left (frame-parameter (selected-frame) 'left))
        (framegeometry-top (frame-parameter (selected-frame) 'top))
        (framegeometry-width (frame-parameter (selected-frame) 'width))
        (framegeometry-height (frame-parameter (selected-frame) 'height))
        (framegeometry-file (expand-file-name "~/.emacs.d/framegeometry"))
        )

    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
      (when (file-writable-p framegeometry-file)
        (write-file framegeometry-file))))
  )
(defun load-framegeometry ()
  "Loads ~/.emacs.d/framegeometry which should load the previous frame's geometry."
  (let ((framegeometry-file (expand-file-name "~/.emacs.d/framegeometry")))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file)))
  )

;; Special work to do ONLY when there is a window system being used
;;(if window-system
;;    (progn
;;      (add-hook 'after-init-hook 'load-framegeometry)
;;      (add-hook 'kill-emacs-hook 'save-framegeometry))
;;  )

(add-to-list 'load-path "~/.emacs.d/themes")

''(require 'color-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'github t)

(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h 10)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(tool-bar-mode -1)
