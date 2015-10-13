(provide 'eddie-common)


;;(require 'xcscope)
;;(setq cscope-do-not-update-database t)
;;(setq cscope-initial-directory "~/bin")
(require 'ggtags)
(add-hook 'c-mode-common-hook
		  (lambda ()
			(c-set-offset 'case-label '+)
			(ggtags-mode 1)))

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
	(if (zerop (call-process "global" nil t nil "-pr"))
		(buffer-substring (point-min) (1- (point-max)))
	  nil)))

(defun gtags-update-single(filename)  
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when ggtags-mode
	(when (gtags-root-dir)
	  (gtags-update-current-file))))

(add-hook 'after-save-hook 'gtags-update-hook)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(show-paren-mode 1)

(require 'go-mode-load)
;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)

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
(setq c-default-style "k&r"
	            c-basic-offset 4)
(line-number-mode t)    ; makes the line number show up
(column-number-mode t)  ; makes the column number show up

(desktop-save-mode t)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t)

(defun eddie-timestamp ()
  "Spit out the current time"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun eddie-sign ()
  "spit out my name, email and the current time"
  (interactive)
  (insert "-- Eddie McCreary <eddie.mccreary@hp.com>")
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

;; eof
;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;;(load-file "~/emacs/cedet/common/cedet.el")

;;(add-to-list 'load-path "~/emacs/ecb")
;;(add-to-list 'load-path "~/emacs/color-theme")
(add-to-list 'load-path "~/.emacs.d/themes")

;;(require 'color-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(require 'color-theme-solarized)
;;(load-theme 'solarized-dark t)
(load-theme 'github t)
;;(require 'ecb-autoloads)
;;(setq ecb-tip-of-the-day nil)

;; Enable EDE (Project Management) features
;;(global-ede-mode 1)

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;;(semantic-load-enable-minimum-features)
;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;;(semantic-load-enable-code-helpers)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 90 :width normal)))))