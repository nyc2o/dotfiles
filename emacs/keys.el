;;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

;;; Key Binding Configuration—ESC and load-theme, Adjust text-sizes.

(defun nyc-setup-escape-key ()
  "Bind ESC to quit prompts."
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

(defun nyc-setup-spc-key-bindings ()
  "Set up key bindings for SPC (leader key) using `general`."
  (use-package general
               :config
               (general-create-definer rune/leader-keys
                                       :keymaps '(normal insert visual emacs)
                                       :prefix "SPC"
                                       :global-prefix "C-SPC")
               
               (rune/leader-keys
                "t"  '(:ignore t :which-key "toggles")
                "tt" '(counsel-load-theme :which-key "choose theme"))))

(defun nyc-setup-hydra-text-scale ()
  "Define hydra for text scaling."
  (use-package hydra)
  
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t))
  
  (rune/leader-keys
   "ts" '(hydra-text-scale/body :which-key "scale text")))

(defun nyc-setup-projectile ()
  "Configure and set up Projectile for project management."
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
               :config (counsel-projectile-mode)))

(defun custom-key-bindings ()
  "Custom key bindings."
  (bind-key "C-c e" #'nyc-open-init-file)
  (bind-key "C-c 3" #'nyc-split-three-windows))


;;; Entry-point/Top-level

(defun initialize-key-bindings ()
  "Initialize all key bindings and configuration."
  (nyc-setup-escape-key)
  (nyc-setup-spc-key-bindings)
  (nyc-setup-hydra-text-scale)
  (nyc-setup-projectile)
  (custom-key-bindings))

;; Call the entry point to set up everything
(initialize-key-bindings)

