
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp" t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes" t)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)


(require 'mk-project)
(require 'eddie-common)

(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p o") 'project-multi-occur)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file) ; or project-find-file-ido
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)

(project-def "astromech-core"
      '((basedir          "/home/eddie/projects/astromech/core/")
        (src-patterns     ("*.c"))
        (ignore-patterns  ("*.o"))
        (tags-file        "/home/eddie/projects/.astromech/core/TAGS")
        (file-list-cache  "/home/eddie/projects/.astromech/core/files")
        (open-files-cache "/home/eddie/projects/.astromech/core/open-files")
        (vcs              git)
        (compile-cmd      "idf.py")
        (ack-args         "")
        (startup-hook     astromech-core-startup)
        (shutdown-hook    nil)))

(defun astromech-core-startup ()
)

(project-def "amforth"
      '((basedir          "/home/eddie/projects/amforth/")
        (src-patterns     ("*.c"))
        (ignore-patterns  ("*.o"))
        (tags-file        "/home/eddie/projects/.amforth/TAGS")
        (file-list-cache  "/home/eddie/projects/.amforth/files")
        (open-files-cache "/home/eddie/projects/.amforth/open-files")
        (vcs              git)
        (compile-cmd      "make")
        (ack-args         "")
        (startup-hook     amforth-startup)
        (shutdown-hook    nil)))

(defun amforth-startup ()
)


(load-theme 'nord t)
(set-face-attribute 'font-lock-comment-face nil
                      :foreground "#81A1C1") ; nord9
(set-face-attribute 'vertical-border nil
	                      :foreground "#EBCB8B") ; nord13

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   (quote
    ("3327c4c64f742dda1b58af263fe39fc0716a54a48831e0177fcee837f8ee8dc5" "4515feff287a98863b7b7f762197a78a7c2bfb6ec93879e7284dff184419268c" default)))
 '(package-selected-packages (quote (auto-complete)))
 '(show-paren-mode t)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro for Powerline" :foundry "ADBO" :slant normal :weight normal :height 90 :width normal)))))
(put 'downcase-region 'disabled nil)
