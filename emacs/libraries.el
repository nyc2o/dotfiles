;;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-


;;; LOAD QL.

(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
(require 'slime-autoloads)
(slime-setup '(slime-fancy))
(custom-set-variables
 '(package-selected-packages
   '(vterm visual-fill-column taxy-magit-section
     spinner smartparens sesman rainbow-delimiters
     persist parseedn page-break-lines ox-spectacle
     ox-reveal org-ref org-re-reveal org-modern org-bullets
     neotree hide-mode-line helpful general forge doom-themes
     doom-modeline-now-playing dashboard counsel-projectile company
     command-log-mode calfw all-the-icons)))
(custom-set-faces)


;;; Package manager configuration

(defun nyc-setup-package-manager ()
  "Set up the package manager with appropriate package sources."
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))


;;; Org-revel

(defun nyc-setup-org-reveal ()
  "Configure org-reveal for presentations."
  (use-package ox-reveal
               :config
               (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")))


;;; Prettify-symbols

(defun nyc-setup-prettify-symbols ()
  "Configure global prettify symbols mode."
  (use-package emacs
               :config
               (global-prettify-symbols-mode 1)))


;;; Vterm

(defun nyc-setup-vterm ()
  "Set up vterm manually with the appropriate load path."
  (use-package vterm
               :load-path "~/common-lisp/emacs-libvterm"))


;;; Neotree

(defun nyc-setup-neotree ()
  "Set up and configure Neotree."
  (use-package neotree
               :bind ([f8] . neotree-toggle)
               :config
               (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))


;;; All-the-icons

(defun nyc-setup-all-the-icons ()
  "Set up All-the-icons for graphical environments."
  (use-package all-the-icons
               :if (display-graphic-p)
               :config
               (unless (member "all-the-icons" (font-family-list))
                 (all-the-icons-install-fonts t))))


;;; Page break lines

(defun nyc-setup-page-break-lines ()
  "Set up and configure page-break-lines mode."
  (use-package page-break-lines
               :config
               (global-page-break-lines-mode)))


;;; Rainbow delimiters

(defun nyc-setup-rainbow-delimiters ()
  "Enable rainbow-delimiters mode for better parenthesis visibility."
  (use-package rainbow-delimiters
               :hook (prog-mode . rainbow-delimiters-mode)
               :config
               ;; Customizing faces after the package is loaded
               (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")
               (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")
               (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")
               (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")
               (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")
               (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")
               (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")
               (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")
               (set-face-foreground 'rainbow-delimiters-depth-9-face "#666"))
  ;;; Apply it on REPL too.
  (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode))


;; Smartparens

(defun nyc-setup-smartparens ()
  "Configure Smartparens for Emacs."
  (use-package smartparens-config
               :ensure smartparens
               :config (progn (show-smartparens-global-mode t)))
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))


;; Counsel configuration

(defun nyc-setup-packages ()
  "Install and configure essential packages."
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
               ([remap describe-key] . helpful-key)))


;;; Entry-point/Top-level

(defun initialize-custom-setup ()
  "Initialize all custom configurations and settings."
  (nyc-setup-package-manager)
  (nyc-setup-org-reveal)
  (nyc-setup-prettify-symbols)
  (nyc-setup-vterm)
  (nyc-setup-neotree)
  (nyc-setup-all-the-icons)
  (nyc-setup-page-break-lines)
  (nyc-setup-rainbow-delimiters)
  (nyc-setup-smartparens)
  (nyc-setup-packages)
  (message "Custom configuration initialized successfully"))

(initialize-custom-setup)
