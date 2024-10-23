(load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")

;;; Global-company
(add-hook 'after-init-hook 'global-company-mode)


;;; Custom commands
(defun split-three-windows ()
  "Split a window into thirds."
  (interactive)
  (split-window-right)
  (split-window-right)
  (balance-windows))
(bind-key "C-c 3" #'split-three-windows)

(defun open-init-file ()
  "Open emacs config."
  (interactive)
  (find-file "~/.emacs"))

(bind-key "C-c e" #'open-init-file)

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
(setq doom-modeline-support-imenu t)
(setq doom-modeline-height 25)
(setq doom-modeline-bar-width 4)
(setq doom-modeline-hud nil)
;; The limit of the window width.
;; If `window-width' is smaller than the limit, some information won't be
;; displayed. It can be an integer or a float number. `nil' means no limit."
(setq doom-modeline-window-width-limit 85)
(setq doom-modeline-project-detection 'auto)
(setq doom-modeline-buffer-file-name-style 'auto)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-lsp-icon t)
(setq doom-modeline-time-icon t)
(setq doom-modeline-time-live-icon t)
(setq doom-modeline-time-analogue-clock t)
(setq doom-modeline-time-clock-size 0.7)
(setq doom-modeline-unicode-fallback nil)
(setq doom-modeline-buffer-name t)
(setq doom-modeline-highlight-modified-buffer-name t)
(setq doom-modeline-column-zero-based t)
(setq doom-modeline-percent-position '(-3 "%p"))
(setq doom-modeline-position-line-format '("L%l"))
(setq doom-modeline-position-column-format '("C%c"))
(setq doom-modeline-position-column-line-format '("%l:%c"))
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-enable-word-count nil)
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-indent-info nil)
(setq doom-modeline-total-line-number nil)
(setq doom-modeline-vcs-icon t)
(setq doom-modeline-vcs-max-length 15)
(setq doom-modeline-vcs-display-function #'doom-modeline-vcs-name)
(setq doom-modeline-check-icon t)
(setq doom-modeline-check-simple-format nil)
(setq doom-modeline-number-limit 99)
(setq doom-modeline-workspace-name t)
(setq doom-modeline-persp-name t)
(setq doom-modeline-display-default-persp-name nil)
(setq doom-modeline-persp-icon t)
(setq doom-modeline-lsp t)
(setq doom-modeline-github nil)
(setq doom-modeline-github-interval (* 30 60))
(setq doom-modeline-modal t)
(setq doom-modeline-modal-icon t)
(setq doom-modeline-modal-modern-icon t)
(setq doom-modeline-irc t)
(setq doom-modeline-irc-stylize 'identity)
(setq doom-modeline-battery t)
(setq doom-modeline-time t)
(setq doom-modeline-buffer-file-name-function #'identity)
(setq doom-modeline-buffer-file-truename-function #'identity)
(setq doom-modeline-env-version t)
(setq doom-modeline-env-load-string "...")


;;; Global prettify
(global-prettify-symbols-mode 1)

;;; VTERM
(use-package vterm
  :load-path  "/home/nycto/common-lisp/emacs-libvterm/")

;;; All-the-icons
(add-to-list 'load-path "/home/nycto/common-lisp/all-the-icons.el")
 (when (display-graphic-p)
   (require 'all-the-icons))

;;; NEOTREE
(add-to-list 'load-path "/home/nycto/common-lisp/neotree")
(require 'neotree)
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))


;;; Page-break-lines
(load "/home/nycto/common-lisp/page-break-lines/page-break-lines.el")
(require 'page-break-lines)


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


;; Add Date
(setq display-time-day-and-date t
      display-time-format "%a | %b, %d | %R"
      display-time-interval 60
      display-time-default-load-average nil)
(display-time)


;;; Basic UI Configuration
;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 150)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
;; (tool-bar-mode -1)          ; Disable the toolbar
;; (tooltip-mode -1)           ; Disable tooltips
;; (set-fringe-mode 10)        ; Give some breathing room
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


;;; Package Manager Configuration 
;;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

(use-package command-log-mode)

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
 '(org-agenda-files '("/home/nycto/src/org-agen-cap/agenda.org"))
 '(org-directory "~/src/org-agen-cap")
 '(package-selected-packages
   '(visual-fill-column spinner smartparens sesman rainbow-delimiters queue parseedn ox-pandoc org-modern org-bullets hydra helpful general forge doom-themes doom-modeline-now-playing dashboard counsel-projectile company command-log-mode calfw)))


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
  
  
;;; Key Binding Configuration—ESC and load-theme, Adjust text-sizes.

;;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))
  
  
;;; Set-up org mode.
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
  
  
;;; Org Mode Configuration 
(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Mate" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "☯" "●" "☯" "●" "☯" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;;; Org Capture & Org Capture
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)
;;; Org Tempalte
(setq x nil)
(setq org-capture-templates
      '(("s" "Snippet" entry
         (file+headline "n.org" "Captured Items")
	 "* Note No. %^{}  \n")))

;;; (add-hook 'org-mode-hook #'org-modern-mode
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
    
(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "◀── NOW! ─────────────────────────────────────────────────")

;; Ellipsis styling
(setq org-ellipsis "…")
(add-hook 'org-mode-hook
          (lambda ()
            (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)))
(global-org-modern-mode)


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


;;; Add Support for Pretty Symbols Mode
(setq prettify-symbols-alist '(("lambda" . 955)))

;;; Dashboard
(defun generate-random-banner-title ()
  "Generate Random Dashboard every open of Emacs."
  (let* ((banners '("~/.emacs.d/img/as.gif"
		    "~/.emacs.d/img/as1.gif"
		    "~/.emacs.d/img/as2.gif"
		    "~/.emacs.d/img/as3.gif"
		    "~/.emacs.d/img/as4.gif"
		    "~/.emacs.d/img/as5.gif"
		    "~/.emacs.d/img/as6.gif"
		    "~/.emacs.d/img/as7.gif"))
	(titles '("Live as if you were to die tomorrow!"
		      "To accomplish great things, we must not only act, but also dream; not only plan, but also believe."
		      "Only passions, great passions can elevate the soul to great things."
		      "I did a lot of great things in the past, but I live for today and for the future."
		      "Believe you can and you’re halfway there."
		      "Be happy for this moment. This moment is your life."
		      "Change your thoughts and you change your world."
		      "Life is only meaningful when we are striving for a goal."))
	(random-banner (nth (random (length banners)) banners))
	(random-titles (nth (random (length titles)) titles)))
    (setq dashboard-startup-banner random-banner
	  dashboard-banner-logo-title random-titles)))

(setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-item-names '(("Recent Files:"               . "Recently opened files:")
			       ("Bookmarks:"                  . "Bookmark links:")
                               ("Agenda for the coming week:" . "Agenda:")))
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-icon-type 'all-the-icons) ;; use `all-the-icons' package
  (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-center-content t) ;; set to 't' for centered content
  :config
  (setq dashboard-heading-shorcut-format " [%s]")
  (generate-random-banner-title)
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
			  (bookmarks . 5))))
(add-to-list 'dashboard-items '(agenda) t)
