;;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

;;; Key Binding Configuration—ESC and load-theme, Adjust text-sizes.

;;; Make ESC quit prompts

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; SPC

(use-package general
             :config
             (general-create-definer rune/leader-keys
                                     :keymaps '(normal insert visual emacs)
                                     :prefix "SPC"
                                     :global-prefix "C-SPC")
             
             (rune/leader-keys
              "t"  '(:ignore t :which-key "toggles")
              "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
 "ts" '(hydra-text-scale/body :which-key "scale text"))


;;; Projectile Configuration

(use-package projectile
             :diminish projectile-mode
             :config (projectile-mode)
             :custom ((projectile-completion-system 'ivy))
             :bind-keymap
             ("C-c p" . projectile-command-map)
             :init
             (when (file-directory-p "~/src")
               (setq projectile-project-search-path '("~/src")))
             (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
             :config (counsel-projectile-mode))

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

;;; Open init-file

(bind-key "C-c e" #'open-init-file)
;;; Split three windows

(bind-key "C-c 3" #'split-three-windows)
