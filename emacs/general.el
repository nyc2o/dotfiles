;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

;;; UI Configuration

(defvar runemacs/default-font-size 300)

(setq inhibit-startup-message t)

;;; Bartools 

(scroll-bar-mode -1)

;; Disable visible scrollbar

(tool-bar-mode -1)  	    ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(set-face-attribute 'tool-bar nil
                    :background  nil 
                    :foreground "white"
                    :underline)


;; Set up the visible bell

(setq visible-bell t)

;; Disable line numbers for some modes

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;Doom theme

(use-package doom-themes
             :init (load-theme 'doom-manegarm t))

;;; Doom Mode line

(use-package doom-modeline
             :ensure t
             :init (doom-modeline-mode 1))

(set-face-attribute 'mode-line nil
                    :background "#1D1F27"  ; Dark blue background
                    :foreground "white"
                    :box '(:line-width 8 :color "#1D1F27")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#353644"  ; You can keep this or change it too
                    :foreground "white"
                    :box '(:line-width 8 :color "#353644")
                    :overline nil
                    :underline nil)

(display-battery-mode 1)

(setq
 doom-modeline-support-imenu t
 doom-modeline-height 25
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
 doom-modeline-unicode-fallback nil
 doom-modeline-buffer-name t
 doom-modeline-highlight-modified-buffer-name t
 doom-modeline-column-zero-based t
 doom-modeline-percent-position '(-3 "%p")
 doom-modeline-position-line-format '("L%l")
 doom-modeline-position-column-format '("C%c")
 doom-modeline-position-column-line-format '("%l:%c")
 doom-modeline-minor-modes nil
 doom-modeline-enable-word-count nil
 doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)
 doom-modeline-buffer-encoding t
 doom-modeline-indent-info nil
 doom-modeline-total-line-number nil
 doom-modeline-vcs-icon t
 doom-modeline-vcs-max-length 15
 doom-modeline-vcs-display-function #'doom-modeline-vcs-name
 doom-modeline-check-icon t
 doom-modeline-check-simple-format nil
 doom-modeline-number-limit 99
 doom-modeline-workspace-name t
 doom-modeline-persp-name t
 doom-modeline-display-default-persp-name nil
 doom-modeline-persp-icon t
 doom-modeline-lsp t
 doom-modeline-github nil
 doom-modeline-github-interval (* 30 60)
 doom-modeline-modal t
 doom-modeline-modal-icon t
 doom-modeline-modal-modern-icon t
 doom-modeline-irc t
 doom-modeline-irc-stylize 'identity
 doom-modeline-battery t
 doom-modeline-time t
 doom-modeline-buffer-file-name-function #'identity
 doom-modeline-buffer-file-truename-function #'identity
 doom-modeline-env-version t
 doom-modeline-env-load-string "...")

;;; Hide-mode-line
(require 'hide-mode-line)

;;; Timestamps

(defun format-date (format)
  (let ((system-time-locale "en_US.UTF-8"))
    (insert (format-time-string format))))

(defun insert-date ()
  (interactive)
  (format-date "%A, %B %d %Y"))

(defun insert-date-and-time ()
  (interactive)
  (format-date "%Y-%m-%d %H:%M:%S"))

;;; line numbers

(setq linum-format "%5d │ ")
(defun my-linum-mode-hook ()
  (linum-mode t))
(add-hook 'find-file-hook 'my-linum-mode-hook)

;;; Indentation

(setq lisp-indent-function 'common-lisp-indent-function)
(setq-default default-major-mode 'text-mode
              indent-tabs-mode nil
              fill-column 100)

;;; Global-company

(add-hook 'after-init-hook 'global-company-mode)

;;; Add Support for Pretty Symbols Mode

(setq prettify-symbols-alist '(("lambda" . 955)))

;;; SAVE
(savehist-mode t)
(setq savehist-file "~/.emacs.d/savehist")
(defun save-defaults ()
  (desktop-save desktop-dirname)
  (savehist-save)
  (bookmark-save))

(defun save-histories ()
  (let ((buf (current-buffer)))
    (save-excursion
     (dolist (b (buffer-list))
       (switch-to-buffer b)
       (save-history)))
    (switch-to-buffer buf)))

(defun save ()
  (interactive)
  (save-desktop)
  (save-defaults)
  (save-histories))

;;; Rainbow-parens-delimiters

(use-package rainbow-delimiters
             :hook (prog-mode . rainbow-delimiters-mode))
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

;;Enable Rainbow Delimiters in SLIME REPL:

(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)

;;; Smartparens

(use-package smartparens-config
             :ensure smartparens
             :config (progn (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/src/org-agen-cap/agenda.org"))
 '(org-directory "~/src/org-agen-cap")
 '(package-selected-packages
   '(hide-mode-line org-ref ox-reveal org-re-reveal visual-fill-column spinner smartparens sesman rainbow-delimiters queue parseedn ox-pandoc org-modern org-bullets hydra helpful general forge doom-themes doom-modeline-now-playing dashboard counsel-projectile company command-log-mode calfw))
 '(warning-suppress-log-types '((slime warning) (slime warning))))

(use-package counsel
             :bind (("M-x" . counsel-M-x)
                    ("C-x b" . counsel-ibuffer)
                    ("C-x C-f" . counsel-find-file)
                    :map minibuffer-local-map
                    ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
             :custom
             (counsel-describe-function-function #'helpful-callable)
             (counsel-describe-variable-function #'helpful-variable)
             :bind
             ([remap describe-function] . counsel-describe-function)
             ([remap describe-command] . helpful-command)
             ([remap describe-variable] . counsel-describe-variable)
             ([remap describe-key] . helpful-key))
