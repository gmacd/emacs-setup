; Useful: https://github.com/EnigmaCurry/emacs/blob/master/init.el

(setq *is-mac* (eq system-type 'darwin))
(setq *is-windows* (eq system-type 'windows-nt))
(setq *is-linux* (eq system-type 'gnu/linux))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General

(setq default-tab-width 4)

; http://stackoverflow.com/questions/145175/how-to-invoke-an-interactive-elisp-interpreter-in-emacs
; ???
; (setq debug-on-error t)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(when *is-mac*
  (exec-path-from-shell-initialize))

; Paths
(add-to-list 'load-path "~/.emacs.d/vendor")

;(load-theme 'solarized-[light|dark] t)
;(load-theme 'solarized-light t)
(load-theme 'monokai t)

(setq inhibit-startup-message t)
(setq inhibit-scratch-message t)

; Auto refresh files
(global-auto-revert-mode t)

; Save session on exit
(desktop-save-mode 1)

; Recent files mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

; Spaces rather than tabs
(setq-default indent-tabs-mode nil)

; Auto indent on return
(define-key global-map (kbd "RET") 'newline-and-indent)

; Auto complete
(require 'auto-complete)
;(add-to-list 'load-path "~/.emacs.d")    ; This may not be appeared if you have already added.
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

; ac-nrepl
;(require 'ac-nrepl)
;(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

;(load-file "/Users/graham/Documents/Code/github/nrepl-inspect/nrepl-inspect.el")
;(define-key nrepl-mode-map (kbd "C-c C-i") 'nrepl-inspect)

; Show matching paren
(show-paren-mode 1)

; Save backups to ~/.saves
(setq backup-directory-alist `(("." . "~/.saves")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "M-o") 'projectile-find-other-file)
            (setq c-basic-offset 4)
            (c-set-offset 'substatement-open 0)))

;; Open .h files in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux

;; Disable ctrl-z keybinding
(when *is-linux*
  (global-unset-key (kbd "C-z")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac

; Allow hash to be entered  
(when *is-mac*
  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
  (setq mac-command-modifier 'meta))

(when *is-mac*
  (exec-path-from-shell-initialize))

(add-to-list 'ido-ignore-files "\\.DS_Store")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows

; Windows features (e.g. good keyboard shortcuts (copy, paste, etc))
(when *is-windows*
  (cua-mode t)
  (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
  (transient-mark-mode 1) ;; No region when it is not highlighted
  (setq cua-keep-region-after-copy t)) ;; Standard Windows behaviour

(when *is-mac*
;;  (require 'redo)
  (require 'mac-key-mode)
  (mac-key-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)
            (setq c-basic-offset 4)
            (c-set-offset 'substatement-open 0)))

;; Open .h files in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Audio/Visual

(windmove-default-keybindings 'meta)

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
;(global-linum-mode 1)
(setq column-number-mode t)

; No bell
(setq visible-bell nil)
(setq ring-bell-function `(lambda ()))

; Slightly smaller font
(set-face-attribute 'default (selected-frame) :height 100)

; No word wrap
(setq-default truncate-lines 1)

; Better trackpad dragging
(setq mouse-wheel-progressive-speed nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc funcs
;; TODO Move to another file

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

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; eshell

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)
(add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Org

;(require 'org-journal)

;(when *is-mac*
;  (setq org-directory "/Users/graham/Dropbox/emacs/org/"))
;(when *is-windows*
;  (setq org-directory "C:/Users/grahamm/Dropbox/emacs/org/"))

;(setq org-journal-dir (concat org-directory "journal/"))
;(setq org-default-notes-file (concat org-directory "/notes.org"))
;(define-key global-map "\C-cc" 'org-capture)
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-ca" 'org-agenda)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default)))
 '(js-indent-level 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
