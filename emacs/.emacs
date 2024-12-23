;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-


;;; Directories

(defconst emacs-lib-dir (expand-file-name "~/etc/emacs/"))  
(defconst emacs-src-dir (expand-file-name "~/common-lisp/")) 


;;; Helper Functions for Paths

(defun emacs-path-lib (subdir)
  "Constructs the full path for a given subdirectory."
  (expand-file-name subdir emacs-lib-dir)) 


;;; Helper Functions for Paths

(defun emacs-path-src (subdir)
  "Constructs the full path for a given subdirectory."
  (expand-file-name subdir emacs-src-dir))


;;; Load Configuration Files

(defun emacs-load-config-files (files)
  (dolist (file files)
    (let ((file-path (emacs-path-lib (symbol-name file))))
      (load file-path))))


;;; .el files

(defvar *emacs-config-files* '(libraries general keys misc org))


;;; Load .el files

(emacs-load-config-files *emacs-config-files*)
