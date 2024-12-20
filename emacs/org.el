;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

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

;; Ellipsis styling

(setq org-ellipsis "…")
(add-hook 'org-mode-hook
          (lambda ()
            (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)))
(global-org-modern-mode)

