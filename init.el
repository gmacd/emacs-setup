; Useful: https://github.com/EnigmaCurry/emacs/blob/master/init.el

(setq *is-mac* (eq system-type 'darwin))
(setq *is-windows* (eq system-type 'windows-nt))


; marmalade repo (from nrepL install docs)
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; Paths
(add-to-list 'load-path "~/.emacs.d/vendor")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

; Windows features (e.g. good keyboard shortcuts (copy, paste, etc))
(when *is-windows*
  (cua-mode t)
  (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
  (transient-mark-mode 1) ;; No region when it is not highlighted
  (setq cua-keep-region-after-copy t)) ;; Standard Windows behaviour

(setq inhibit-startup-message t)
(setq inhibit-scratch-message t)

; Nrepl install suggestions
(add-hook 'nrepl-interaction-mode-hook
	  'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'paredit-mode)

; Paredit in clojure-mode
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

(add-hook 'find-file-hook 'flymake-find-file-hook)

; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(add-to-list 'ido-ignore-files "\\.DS_Store")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Javascript
; (Mac only for now)
(when *is-mac*
  (add-to-list 'load-path "~/.emacs.d/vendor/jshint-mode")
  (require 'flymake-jshint)
  (add-hook 'javascript-mode-hook
	    (lambda () (flymake-mode t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Audio/Visual

; Turn off toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

; Smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

; Rainbow parens
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

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
