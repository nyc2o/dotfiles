;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-


;;; Org Mode Configuration

(defun nyc-org-font-setup ()
  "Set up font and appearance for Org mode."
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Mate" :weight 'regular :height (cdr face)))

  (dolist (face '(org-block org-code org-table org-verbatim org-special-keyword org-meta-line org-checkbox))
    (set-face-attribute face nil :inherit '(shadow fixed-pitch)))
  ;; Disable line numbers for Org mode
  (add-hook 'org-mode-hook (lambda () (when line-number-mode (display-line-numbers-mode 0)))))

(defun nyc-org-mode-visual-fill ()
  "Disable visual fill column and allow full width."
  (visual-fill-column-mode -1) ;; Disable visual-fill-column for full width
  (set-window-margins (selected-window) 0 0)) ;; Remove window margins

(defun nyc-set-default-frame-size ()
  "Set the default frame size for Emacs."
  (add-to-list 'default-frame-alist '(width . 200)) 
  (add-to-list 'default-frame-alist '(height . 50))) 

(defun nyc-org-bullets-setup ()
  "Set up org-bullets for Org mode."
  (use-package org-bullets
               :after org
               :hook (org-mode . org-bullets-mode)
               :custom
               (org-bullets-bullet-list '("◉" "☯" "●" "☯" "●" "☯" "●"))))

(defun nyc-org-capture-templates-setup ()
  "Set up Org capture templates."
  (setq org-capture-templates
        '(("s" "Snippet" entry
           (file+headline "n.org" "Captured Items")
           "* Note No. %^{}  \n"))))

(defun nyc-org-mode-customizations ()
  "Set up general Org mode customizations."
  (setq org-ellipsis " ▾"
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string "◀── NOW! ─────────────────────────────────────────────────"
        org-ellipsis "…"))

(defun nyc-org-modern-setup ()
  "Enable Org modern mode for a sleek look."
  (add-hook 'org-mode-hook (lambda () (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)))
  (global-org-modern-mode))

(defun nyc-org-keybindings-setup ()
  "Set up keybindings for Org mode."
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c a") #'org-agenda))


;;; Entry-point/Top-level

(defun nyc/setup-org-mode ()
  "Run all the setup functions for Org mode."
  (nyc-org-font-setup)
  (nyc-org-mode-customizations)
  (nyc-org-bullets-setup)
  (nyc-org-capture-templates-setup)
  (nyc-org-keybindings-setup)
  (nyc-org-modern-setup)
  (nyc-set-default-frame-size)
  (add-hook 'org-mode-hook #'nyc-org-mode-visual-fill))

(nyc/setup-org-mode)
