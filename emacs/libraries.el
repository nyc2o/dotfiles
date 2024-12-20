;;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

;;; LOAD QL.

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

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

;;; Reveal.js

(require 'ox-reveal)
(setq org-reveal-root "file:~/src/video-series-scripts/intro/reveal.js")


;;; Global prettify

(global-prettify-symbols-mode 1)

;;; VTERM

(use-package vterm
             :load-path "~/common-lisp/emacs-libvterm")

;;; NEOTREE

(add-to-list 'load-path "~/common-lisp/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;; All-the-icons

(add-to-list 'load-path "~/common-lisp/all-the-icons.el")
(when (display-graphic-p)
  (require 'all-the-icons))

;;; Page-break-lines

(load "~/common-lisp/page-break-lines/page-break-lines.el")
(require 'page-break-lines)
