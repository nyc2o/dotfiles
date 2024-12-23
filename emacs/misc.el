;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-


;;; Custom commands

(defun nyc-split-three-windows ()
  "Split a window into thirds."
  (interactive)
  (split-window-right)
  (split-window-right)
  (balance-windows))

(defun nyc-open-init-file ()
  "Open emacs config."
  (interactive)
  (find-file "~/.emacs"))


;;; Dashboard

(defun nyc-generate-random-banner-title ()
  "Pre-process generation of random Dashboard every open of Emacs."
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
"Processing dashboard generation."
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
             (nyc-generate-random-banner-title)
             (dashboard-setup-startup-hook)
             (setq dashboard-items '((recents . 5)
			             (bookmarks . 5))))
(add-to-list 'dashboard-items '(agenda) t)


;;; Top-level / Entry-points

(defun initialize ()
  (nyc-split-three-windows)
  (nyc-open-init-file)
  (nyc-generate-random-banner-title))
