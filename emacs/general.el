;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-


;;; UI Configuration

(defun nyc-runemacs/full-screen-setup ()
  "Make Emacs full-screen and adjust UI elements accordingly."
  (interactive)
  (if (fboundp 'toggle-frame-fullscreen)
      (toggle-frame-fullscreen)
      (set-frame-parameter nil 'fullscreen 'maximized)))

(defun nyc-setup-ui ()
  "Configure the UI elements for Emacs."
  (setq inhibit-startup-message t)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (menu-bar-mode -1)
  (set-face-attribute 'tool-bar nil
                      :background nil 
                      :foreground "white"
                      :underline)
  (setq visible-bell t)
  ;; Load Doom Theme
  (use-package doom-themes
               :init (load-theme 'doom-manegarm t)))

(defun nyc-setup-doom-modeline ()
  "Set up Doom's modeline."
  (use-package doom-modeline
               :ensure t
               :init (doom-modeline-mode 1))
  (setq doom-modeline-height 25
        doom-modeline-bar-width 4
        doom-modeline-hud nil
        doom-modeline-window-width-limit 85
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-lsp-icon t
        doom-modeline-time-icon t
        doom-modeline-time-live-icon t
        doom-modeline-time-analogue-clock t
        doom-modeline-time-clock-size 0.7
        doom-modeline-buffer-name t
        doom-modeline-highlight-modified-buffer-name t
        doom-modeline-column-zero-based t
        doom-modeline-percent-position '(-3 "%p")
        doom-modeline-position-line-format '("L%l")
        doom-modeline-position-column-format '("C%c")
        doom-modeline-position-column-line-format '("%l:%c")
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding t
        doom-modeline-vcs-icon t
        doom-modeline-vcs-max-length 15
        doom-modeline-vcs-display-function #'doom-modeline-vcs-name
        doom-modeline-check-icon t
        doom-modeline-check-simple-format nil
        doom-modeline-number-limit 99
        doom-modeline-workspace-name t
        doom-modeline-persp-name t
        doom-modeline-lsp t))

(defun nyc-setup-hide-mode-line ()
  "Set up the hide-mode-line package."
  (require 'hide-mode-line))

(defun nyc-setup-linum-mode ()
  "Configure line numbers for Emacs."
  (setq linum-format "%5d │ ")
  (column-number-mode)
  (global-display-line-numbers-mode t))

(defun nyc-setup-indentation ()
  "Set up default indentation settings."
  (setq lisp-indent-function 'common-lisp-indent-function)
  (setq-default default-major-mode 'text-mode
                indent-tabs-mode nil
                fill-column 100))

(defun nyc-setup-company-mode ()
  "Enable global company mode."
  (add-hook 'after-init-hook 'global-company-mode))

(defun nyc-setup-pretty-symbols ()
  "Enable pretty symbols for Emacs."
  (setq prettify-symbols-alist '(("lambda" . 955))))

(defun nyc-setup-savehist ()
  "Enable and configure savehist mode."
  (savehist-mode t)
  (setq savehist-file "~/.emacs.d/savehist"))

(defun nyc-save ()
  "Save all Emacs data/modules."
  (interactive)
  (desktop-save desktop-dirname)
  (savehist-save)
  (bookmark-save)
  (let ((buf (current-buffer)))
    (save-excursion
     (dolist (b (buffer-list))
       (switch-to-buffer b)
       (save-history)))
    (switch-to-buffer buf)))


;;; Entry-point/Top-level

(defun initialize-emacs-setup ()
  "Initialize all essential Emacs configurations."
  (nyc-runemacs/full-screen-setup)
  (nyc-setup-ui)
  (nyc-setup-doom-modeline)
  (nyc-setup-hide-mode-line)
  (nyc-setup-linum-mode)
  (nyc-setup-indentation)
  (nyc-setup-company-mode)
  (nyc-setup-pretty-symbols)
  (nyc-setup-savehist))

(initialize-emacs-setup)
