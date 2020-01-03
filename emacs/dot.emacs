(add-to-list 'load-path "~/.emacs.d/site-lisp" t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes" t)

(require 'eddie-common)


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
 '(package-selected-packages (quote (company auto-complete)))
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
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 90 :width normal)))))
(put 'downcase-region 'disabled nil)
