; Useful: https://github.com/EnigmaCurry/emacs/blob/master/init.el

(setq *is-mac* (eq system-type 'darwin))
(setq *is-windows* (eq system-type 'windows-nt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General

; marmalade repo
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; Paths
(add-to-list 'load-path "~/.emacs.d/vendor")

(setq inhibit-startup-message t)
(setq inhibit-scratch-message t)

(add-hook 'find-file-hook 'flymake-find-file-hook)

; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

; Spaces rather than tabs
(setq-default indent-tabs-mode nil)

; Show matching paren
(show-paren-mode 1)

; Save backups to ~/.saves
(setq backup-directory-alist `(("." . "~/.saves")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mac

; Allow hash to be entered  
(when *is-mac*
  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#"))))

(when *is-mac*
  (exec-path-from-shell-initialize))

(add-to-list 'ido-ignore-files "\\.DS_Store")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Windows

; Windows features (e.g. good keyboard shortcuts (copy, paste, etc))
(when *is-windows*
  (cua-mode t)
  (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
  (transient-mark-mode 1) ;; No region when it is not highlighted
  (setq cua-keep-region-after-copy t)) ;; Standard Windows behaviour


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Clojure

; Rainbow parens
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

; Nrepl install suggestions
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
(setq nrepl-hide-special-buffers t)
;(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'clojure-mode-hook 'nrepl-interaction-mode)

; Paredit in clojure-mode
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'clojure-test-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)

; ClojureScript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Javascript
; (Mac only for now)
(when *is-mac*
  (add-to-list 'load-path "~/.emacs.d/vendor/jshint-mode")
  (require 'flymake-jshint)
  (add-hook 'javascript-mode-hook
	    (lambda () (flymake-mode t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General Audio/Visual

; Turn off toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

; Smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

; Line & column numbers
(global-linum-mode 1)
(setq column-number-mode t)

; No bell
(setq visible-bell nil)
(setq ring-bell-function `(lambda ()))

; Slightly smaller font
(set-face-attribute 'default (selected-frame) :height 90)

; No word wrap
(setq-default truncate-lines 1)

; Better trackpad dragging
(setq mouse-wheel-progressive-speed nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Misc funcs
; TODO Move to another file

; http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Org

(when *is-mac*
  (setq org-directory "/Users/graham/Dropbox/emacs/org/"))
(when *is-windows*
  (setq org-directory "C:/Users/grahamm/Dropbox/emacs/org/"))

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

; TODO - Only on work PC
(defun gtd()
   (interactive)
   (find-file (concat org-directory "workgtd.org")))
(defun bugs()
   (interactive)
   (find-file (concat org-directory "bugs.org")))
