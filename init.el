
(defconst MILLION 1000000)
(defconst HUNDRED-MILLION 100000000)

(setq gc-cons-threshold HUNDRED-MILLION)

(tool-bar-mode -1)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(setq ring-bell-function 'ignore)
(setq-default x-stretch-cursor t)
;; Fix empty pasteboard error.
(setq save-interprogram-paste-before-kill nil)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top


;; Full path in frame title
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Auto refresh buffers when edits occur outside emacs
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)



;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Smooth Scroll:
(setq mouse-wheel-scroll-amount '(1 ((shift) .1))) ;; one line at a time

;; Scrol one line when hitting bottom of window
(setq scroll-conservatively 10000)

;; Change Cursor
(setq-default cursor-type 'box)

;; Remove alarm (bell) on scroll
(setq ring-bell-function 'ignore)

(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 4)
(electric-indent-mode -1)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Word Wrap (t is no wrap, nil is wrap)
(setq-default truncate-lines nil)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; from 'better-defaults.el'
;; Allow clipboard from outside emacs
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))


(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")


(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
   White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun with-bash (cmd)
  "Runs cmd in a bash shell."
  (interactive)
  (let ((old-shell (symbol-value 'shell-file-name)))
    (setq shell-file-name "bash")
    (let ((output (replace-regexp-in-string "\n$" ""
                                            (shell-command-to-string cmd))))
      (setq shell-file-name old-shell)
      output)))

(require 'cl-lib)
(defun multirust-which-rustc (file-name)
  (trim-string (shell-command-to-string "multirust which rustc")))


;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "http://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("tromey" . "http://tromey.com/elpa/") t)

;; Don't need to check every time.
;; (when (not package-archive-contents) 
;;   (package-refresh-contents))

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)

;; Add parts of each file's directory to the buffer name if not unique
(use-package uniquify
                                        ;  :ensure t
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package smart-tabs-mode
  :ensure t
  :config
  (setq indent-tabs-mode t)
  ;; smart-tabs-insinuate-alist
  (smart-tabs-insinuate 'c 'java ;'haskell
						))

(use-package company
  :ensure t
  :defer 2
  :diminish ""
  :config
  (setq company-tooltip-align-annotations t)
  (global-company-mode 1)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common))

(use-package magit
  :ensure t
  :defer 2
  :config
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package paredit
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :mode ("\\.clj\\'" . clojure-mode)
  :config
  (use-package cider
    :ensure t
    :pin melpa-stable)
  (use-package clojure-mode-extra-font-locking
    :ensure t
	:pin melpa-stable)
  (use-package clj-refactor
    :pin melpa-stable
    :ensure t)
  (load "setup-clojure.el"))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config 
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package rust-mode
  :ensure t
  :pin melpa
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (add-hook 'rust-mode-hook (lambda () (aggressive-indent-mode 0)))
  (use-package flycheck-rust
    :ensure t
    :pin melpa-stable
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (add-hook 'rust-mode-hook (lambda ()
                                (flycheck-set-checker-executable
                                 'rust (multirust-which-rustc buffer-file-name)))))
  (use-package racer
    :ensure t
    :pin melpa
    :config
    (setq racer-cmd "racer")
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode #'eldoc-mode))

  (use-package rustfmt
    :ensure t
    :pin melpa
    :config
    (add-hook 'rust-mode-hook #'rustfmt-enable-on-save)
    (define-key rust-mode-map (kbd "C-c C-f") #'rustfmt-format-buffer)))

(use-package projectile
  :ensure t)

(use-package org
  :ensure t
  :defer 1
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-directory (expand-file-name "~/org"))
  (when (equal system-type 'windows-nt)
    (add-to-list 'exec-path "c:/Program Files/Aspell/bin")
    (setq ispell-program-name "aspell")
    (setq ispell-list-command "--list")
    (setq flyspell-issue-message-flag nil))
  
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-default-notes-file (concat org-directory "\\notes.org"))
  
  (define-key global-map "\C-cb" 'org-iswitchb)
  (setq org-log-done t))

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package drag-stuff
  :ensure t
  :config
  (setq drag-stuff-modifier 'shift)
  (drag-stuff-global-mode))

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc-customizations.el")

;; For editing lisps
(load "elisp-editing.el")

(load "company-custom.el")

(load "helm.el")



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (tomorrow-night-bright)))
 '(custom-safe-themes
   (quote
    ("e9f642ee0dbd5638e40390b8b8eded9743f1426ad1390e7b2e5d3fa04efa2969" "1ce793cf04c7fbb4648c20f079b687ef10d8ee3014422cf67cf08c92fa6dc77c" "9bc6cf0c6a6c4b06b929e8cd9952478fa0924a4c727dacbc80c3949fc0734fb9" "2b2fff94a0e7e4f46d46b6cb072d43005a84460f6f812c5e63e0ec9e23b36ba0" "030bed79e98026124afd4ef8038ba7fe064314baf18b58759a5c92b91ec872fb" default)))
 '(initial-frame-alist (quote ((vertical-scroll-bars) (fullscreen . maximized))))
 '(org-startup-indented t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#cfcfcf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "outline" :family "DejaVu Sans Mono"))))
 '(company-scrollbar-bg ((t (:background "#191919"))))
 '(company-scrollbar-fg ((t (:background "#0c0c0c"))))
 '(company-tooltip ((t (:inherit default :background "#282828"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(font-lock-string-face ((t (:foreground "DarkOliveGreen3"))))
 '(font-lock-type-face ((t (:foreground "#e4b474"))))
 '(font-lock-variable-name-face ((t (:foreground "tan1"))))
 '(helm-buffer-file ((t (:foreground "gray" :underline t))))
 '(helm-candidate-number ((t (:background "gray25" :foreground "gray"))))
 '(helm-ff-file ((t (:foreground "light gray" :underline t))))
 '(helm-prefarg ((t (:foreground "sea green"))))
 '(helm-selection ((t (:background "SteelBlue4" :distant-foreground "black"))))
 '(helm-source-header ((t (:background "gray14" :foreground "white" :weight normal :height 1.1 :family "DejaVu Sans Mono")))))


;; scroll two lines at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; spellchecking
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)


;; M-x qrr for y/n prompting regex replace
(defalias 'qrr 'query-replace-regexp)

(put 'upcase-region 'disabled nil)


(setq gc-cons-threshold MILLION)


