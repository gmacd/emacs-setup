; Useful: https://github.com/EnigmaCurry/emacs/blob/master/init.el

; Paths
(add-to-list 'load-path "~/.emacs.d/vendor")

; Windows features (e.g. good shortcuts
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(setq inhibit-startup-message t)
(setq inhibit-scratch-message t)

; marmalade repo (from nrepL install docs)
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; Nrepl install suggestions
(add-hook 'nrepl-interaction-mode-hook
	  'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'paredit-mode)

; Paredit in clojure-mode
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)


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

; Line numbers
(global-linum-mode 1)

; No bell
(setq visible-bell nil)
(setq ring-bell-function `(lambda ()))

; Slightly smaller font
(set-face-attribute 'default (selected-frame) :height 90)
