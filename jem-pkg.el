;;; jem-pkg.el --- Some kind of elisp packaging for and by jemarch

;; Copyright (C) 2005, 2006, 2007, 2008  Jose E. Marchesi

;; Author: Jose E. Marchesi <jemarch@gnu.org>
;; Maintainer: Jose E. Marchesi <jemarch@gnu.org>
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A kind of elisp packaging for and by jemarch

;;; Code:

;; Constants

(defconst jem-pkg-version (list 0 0 4)
  "jem-pkg version")

(defconst jem-pkg-version-string
  (mapconcat #'number-to-string jem-pkg-version ".")
  "Printable jem-pkg version")

(defconst jem-pkg-system-name
  system-name
  "Name of the system where JemPkg is running")

(defconst jem-pkg-system-configuration
  system-configuration
  "System configuration of the system where JemPkg is running")

(defconst jem-pkg-system-type
  system-type
  "System configuration of the system where JemPkg is running")


;; Variables

(defvar jem-pkg-save-file user-init-file
  "File where JemPkg save its settings")
  

(defvar jem-pkg-package-db nil
  "JemPkg package database

This association list contains the existing packages on the
current local site.

The entry for each package is as:

  (package-name package-version)

")

(defvar jem-pkg-sites nil
  "List with the names of local sites configurations")

(defvar jem-pkg-listing-mode-map nil
  "Keymap for JemPkg packages list")


;; Customization

(defgroup jem-pkg nil
  "Elisp packaging for and by jemarch"
  :link '(url-link "http://es.gnu.org/~jemarch/jem-pkg/index.html")
  :prefix "jem-pkg-"
  :group 'local)

(defcustom jem-pkg-site-base "~/emacs"
  "Path to the jem-pkg site on the local machine"
  :group 'jem-pkg
  :type '(file :must-match t))


;; Functions

(defun jem-pkg-define-sites (sites)
  "Define the contents of jem-pkg-sites"
  (setq jem-pkg-sites sites))

(defun jem-pkg-site-init ()
  "Initialization of the site"
  (let* ((site-name jem-pkg-system-name))
    ;; Build the package database
    (setq jem-pkg-package-db (jem-pkg-build-package-db))
    ;; Set up the load path to the package database
    (jem-pkg-set-load-path)
    ;; Load the common configuration
    (jem-pkg-load-common-configuration)
    ;; Load the site configuration
    (jem-pkg-load-site-configuration)))

(defun jem-pkg-set-load-path ()
  "Set the load path according to common and the current site"

  (jem-pkg-set-site-load-path "common")
  (jem-pkg-set-site-load-path jem-pkg-system-name))

(defun jem-pkg-set-site-load-path (site-name)
  ""
  (mapcar (lambda (pkg)
	    (let ((pkg-name (if (listp pkg)
				(car pkg)
			      pkg))
		  (pkg-version (if (listp pkg)
				   (cadr pkg)
				 nil)))
	      ;; Only add the directory to the load-path if
	      ;; the package is not virtual
	      (if (and (assoc pkg-name jem-pkg-package-db)
		       (or (not pkg-version)
			   (equal (cadr (assoc pkg-name jem-pkg-package-db))
				  pkg-version)))
		  (add-to-list 'load-path
			       (concat jem-pkg-site-base
				       "/elisp/"
				       pkg-name
				       (let ((pkg-dir-version (cadr (assoc pkg-name jem-pkg-package-db))))
					 (if pkg-dir-version
					   (concat "%" pkg-dir-version))))))))
	  
	  (cadr (assoc site-name jem-pkg-sites))))

(defun jem-pkg-build-package-db-parse-dir (dirname)
  "packagename(%packageversion)?"

  ;; Match the two parts
  (string-match "^\\([^%]+\\)\\(%\\(.*\\)\\)?$" dirname)
  ;; Get the list
  (list (match-string 1 dirname)
	(match-string 3 dirname)))

(defun jem-pkg-build-package-db ()
  "Traverses elisp/ and build jem-pkg-package-db"
  (let* ((elisp-dir (concat jem-pkg-site-base "/elisp" ))
	 (pkg-dir-list (cddr (directory-files elisp-dir nil))))
    (mapcar 'jem-pkg-build-package-db-parse-dir pkg-dir-list)))

(defun jem-pkg-load-common-configuration ()
  "Load the common Emacs environment"
  (let ((conf-filename (concat jem-pkg-site-base "/etc/common/common.el")))
    (if (file-readable-p conf-filename)
	(load-file conf-filename)))
  (mapcar 'jem-pkg-load-common-package (jem-pkg-get-site-configuration "common")))

(defun jem-pkg-get-site-configuration (site-name)
  "Get a configuration from jem-pkg-sites"
  (cadr (assoc site-name jem-pkg-sites)))

(defun jem-pkg-load-site-configuration ()
  "Load the Emacs environment for SITENAME"
  (let* ((site-name jem-pkg-system-name)
	 (site-configuration (jem-pkg-get-site-configuration site-name)))
    (load-file (concat jem-pkg-site-base "/etc/" 
		       site-name  "/" site-name ".el"))
    (mapcar 'jem-pkg-load-site-package site-configuration)))

(defun jem-pkg-load-common-package (package-name)
  ""
  (jem-pkg-load-package "common" package-name))

(defun jem-pkg-load-site-package (package-name)
  ""
  (jem-pkg-load-package jem-pkg-system-name package-name))

(defun jem-pkg-load-package (site-name package-name)
  ""
  (let ((package-version (cadr (assoc package-name jem-pkg-package-db)))
	(filename (concat jem-pkg-site-base
			  "/etc/"
			  site-name
			  "/"
			  package-name)))
    ;; Get the appropiate load file
    (if package-version
	(if (file-readable-p (concat filename "%" package-version ".el"))
	    (setq filename (concat filename "%" package-version))))
    ;; Load the file if it exist
    (if (file-readable-p (concat filename ".el"))
	(load-file (concat filename ".el")))))

(defun jem-pkg-get-site-pkg-db (sitename)
  "Return the subset of the packages database that were
loaded using the SITENAME configuration"
  (let ((pkg-db nil))
    (mapcar 
     (lambda (pname)
       (let ((pkg-entry (assoc pname jem-pkg-package-db)))
	 (if pkg-entry
	     (setq pkg-db (cons 
			   (list (car pkg-entry) (cadr pkg-entry))
			   pkg-db))
	   ;; This is a phantom package, so insert a dummy
	   ;; entry
	   (setq pkg-db (cons
			 (list pname nil)
			 pkg-db)))))
     (jem-pkg-get-site-configuration sitename))

    pkg-db))

(defun jem-pkg-print-site-packages (site-name)
  "Print a section with the packages of SITE-NAME into
the current buffer"
  (let ((pkg-db (jem-pkg-get-site-pkg-db site-name))
	(max-pname-length 0))
    ;; Insert a header
    (insert (propertize (concat site-name
				":")
			'face 'bold))
    (insert "\n\n")
    ;; Calculate the maximum package name length
    (let ((aux-pkg-db pkg-db))
      (while aux-pkg-db
	(progn
	  (if (> (length (caar aux-pkg-db)) max-pname-length)
	      (setq max-pname-length (length (caar aux-pkg-db))))
	  (setq aux-pkg-db (cdr aux-pkg-db)))))
    ;; Print package information
    (mapcar
     (lambda (package-entry)
       (insert " ")
       (let ((package-name (propertize 
			    (car package-entry) 
			    'jempkgpkgname (car package-entry))))
	 (insert package-name)
	 (if (equal site-name "common")
	     (add-text-properties (- (point) (length (car package-entry)))
				  (point)
				  '(jempkgcommon t))))
       (let ((i (- (+ max-pname-length 2) (length (car package-entry)))))
	 (while (not (= i 0))
	   (progn
	     (insert " ")
	     (setq i (- i 1)))))
       (if (cadr package-entry)
	   (let ((package-version (propertize
				   (cadr package-entry)
				   'bold t
				   'jempkgpkgversion (cadr package-entry))))
	     (insert package-version)
	     (if (equal site-name "common")
			(add-text-properties (- (point) (length (cadr package-entry)))
					     (point)
					     '(jempkgcommon t))))
	 (insert (propertize "(unknown)"
			     'jempkgpkgversion "(unknown)")))
       (insert "\n"))
     pkg-db)))

(defun jem-pkg-list-packages ()
  "List all loaded packages on the running emacs"
  (interactive)
  (save-excursion
    (if (get-buffer "*Packages*")
	(kill-buffer (get-buffer "*Packages*")))
    (let ((pkg-buffer (get-buffer-create "*Packages*")))
      ;; Change the current buffer
      (set-buffer pkg-buffer)
      ;; Print some instructions
      (insert "This buffer list all the packages currently loaded into your emacs by
JemPkg.\n\n")
      ;; Print package listings
      (jem-pkg-print-site-packages "common")
      (insert "\n")
      (jem-pkg-print-site-packages jem-pkg-system-name)
      ;; Make the entire buffer read-only
      (add-text-properties (point-min) (point-max)
			   '(read-only true))
      ;; Set the major mode
      (jem-pkg-listing-mode)
      ;; Goto the first package and show it
      ;; into some other window
      (goto-char (next-single-property-change 1 'jempkgpkgname))
      (switch-to-buffer-other-window pkg-buffer))))

(defun jem-pkg-listing-configure-site ()
  "Open a buffer with the configuration of the site that contain
the package at point"
  (interactive)
  (if (get-text-property (point) 'jempkgpkgname)
      (find-file-other-window
       (concat jem-pkg-site-base
	       "/etc/"
	       (if (get-text-property (point) 'jempkgcommon)
		   "common"
		 jem-pkg-system-name)
	       "/"
	       (if (get-text-property (point) 'jempkgcommon)
		   "common"
		 jem-pkg-system-name)
	       ".el"))
    (error "No package at point")))

(defun jem-pkg-listing-configure-generic-package ()
  "Open a buffer with the configuration of the package at point"
  (interactive)
  (if (get-text-property (point) 'jempkgpkgname)
      (find-file-other-window 
       (concat jem-pkg-site-base
	       "/etc/"
	       (if (get-text-property (point) 'jempkgcommon)
		   "common"
		 jem-pkg-system-name)
	       "/"
	       (get-text-property (point) 'jempkgpkgname)
	       ".el"))
    (error "No package at point")))

(defun jem-pkg-listing-configure-package ()
  "Open a buffer with the configuration of the package (taking care about the
version) at point"
  (interactive)
  (save-excursion
    (if (get-text-property (point) 'jempkgpkgname)
	(let ((package-name (get-text-property (point) 'jempkgpkgname))
	      (commonp (get-text-property (point) 'jempkgcommon)))
	  (goto-char (next-single-property-change (point) 'jempkgpkgversion))
	  (if (equal (get-text-property (point) 'jempkgpkgversion)
		     "(unknown)")
	      (error "This package is not versioned"))
	  (find-file-other-window
	   (concat jem-pkg-site-base
		   "/etc/"
		   (if commonp
		       "common"
		     jem-pkg-system-name)
		   "/"
		   package-name
		   "%"
		   (get-text-property (point) 'jempkgpkgversion)
		   ".el")))
      (error "No package at point"))))

(defun jem-pkg-listing-next-package ()
  "Goto the next package (if any)"
  (interactive)
  (save-excursion
    (goto-char (next-single-property-change (point) 'jempkgpkgname))
    (if (not (get-text-property (point) 'jempkgpkgname))
	(if (not (next-single-property-change (point) 'jempkgpkgname))
	    (error "No more packages"))))
  (goto-char (next-single-property-change (point) 'jempkgpkgname))
  (if (not (get-text-property (point) 'jempkgpkgname))
      (goto-char (next-single-property-change (point) 'jempkgpkgname))))

(defun jem-pkg-listing-previous-package ()
  "Goto the next package (if any)"
  (interactive)
  (if (not (previous-single-property-change (point) 'jempkgpkgname))
      (error "No previous package"))
  (goto-char (previous-single-property-change (point) 'jempkgpkgname))
  (if (not (get-text-property (point) 'jempkgpkgname))
      (goto-char (previous-single-property-change (point) 'jempkgpkgname))))

(defun jem-pkg-listing-load-site-configuration ()
  "Load the configuration file of the site of the package at point,
if it exist"
  (interactive)
  (let ((package-name (get-text-property (point) 'jempkgpkgname))
	(commonp (get-text-property (point) 'jempkgcommon)))
    (if (not package-name)
	(error "No package at point"))
    (let ((filename (concat jem-pkg-site-base
			    "/etc/"
			    (if commonp
				"common"
			      jem-pkg-system-name)
			    "/"
			    (if commonp
				"common"
			      jem-pkg-system-name)
			    ".el")))
      (if (file-readable-p filename)
	  (load-file filename)
	(error "This site do not have a configuration file")))))
    

(defun jem-pkg-listing-load-configuration ()
  "Load the configuration files of the package at point.
Note that the common configuration (if any) is load before the specific one (if any)."
  (interactive)
  (let* ((package-name (get-text-property (point) 'jempkgpkgname))
	 (commonp (get-text-property (point) 'jempkgcommon))
	 (generic-filename (concat jem-pkg-site-base
				   "/etc/"
				   (if commonp
				       "common"
				     jem-pkg-system-name)
				   "/"
				   package-name
				   ".el")))
    ;; Load generic configuration, if it exist
    (if (file-readable-p generic-filename)
	(load-file generic-filename))
    ;; Load the versioned configuration, if it exist
    (save-excursion
      (goto-char (next-single-property-change (point) 'jempkgpkgversion))
      (let ((versioned-filename (concat
				 jem-pkg-site-base
				 "/etc/"
				 (if commonp
				     "common"
				   jem-pkg-system-name)
				 "/"
				 package-name
				 "%"
				 (get-text-property (point) 'jempkgpkgversion)
				 ".el")))
	(if (file-readable-p versioned-filename)
	    (load-file versioned-filename))))))

(defun jem-pkg-listing-quit ()
  "Delete the listing window and the *Packages* buffer"
  (interactive)
  (delete-windows-on "*Packages*")
  (kill-buffer "*Packages*"))

(defun jem-pkg-listing-mode ()
  "Major mode for editing JemPkg listings.

Commands:
\\{jem-pkg-listing-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq jem-pkg-listing-mode-map (make-keymap))
  (define-key jem-pkg-listing-mode-map "n" 'jem-pkg-listing-next-package)
  (define-key jem-pkg-listing-mode-map "p" 'jem-pkg-listing-previous-package)
  (define-key jem-pkg-listing-mode-map "c" 'jem-pkg-listing-configure-package)
  (define-key jem-pkg-listing-mode-map "C" 'jem-pkg-listing-configure-generic-package)
  (define-key jem-pkg-listing-mode-map "s" 'jem-pkg-listing-configure-site)
  (define-key jem-pkg-listing-mode-map "l" 'jem-pkg-listing-load-configuration)
  (define-key jem-pkg-listing-mode-map "L" 'jem-pkg-listing-load-site-configuration)
  (define-key jem-pkg-listing-mode-map "q" 'jem-pkg-listing-quit)
  (use-local-map jem-pkg-listing-mode-map)
  (setq mode-name "JemPkg-Listing")
  (setq major-mode 'jem-pkg-listing-mode))


(defun jem-pkg-install-package (filename)
  "Install a package described on FILENAME"
  (interactive "fFile with package description: ")
  (if (not (file-readable-p filename))
      (error (concat "Cannot read " filename)))
  (save-excursion
    (with-temp-buffer
      (insert-file-contents filename)
      (hack-local-variables)
      ;; Verify we have all the required metainfo
      (if (not (local-variable-p 'jem-pkg-package-name))
	  (error "This file seems to not be a jempkg package: package name required"))
      ;; Collect metadata
      (let ((package-name jem-pkg-package-name)
	    (package-version (if (local-variable-p 'jem-pkg-package-version)
				 jem-pkg-package-version
			       nil))
	    (package-depends (if (local-variable-p 'jem-pkg-package-depends)
				 jem-pkg-package-depends
			       nil))
	    (package-manifest (if (local-variable-p 'jem-pkg-package-manifest)
				  jem-pkg-package-manifest
				nil)))
	;; This package may be already installed
	(if (and (assoc package-name jem-pkg-package-db)
		 (equal (cadr (assoc package-name jem-pkg-package-db)) package-version))
	    (if (not (yes-or-no-p "This package already exist. Do you want to install anyway? "))
		(message "Package not installed")
	      ;; Install the package
	      (if package-manifest 
		  (jem-pkg-install-multi-file-package filename
						      package-name
						      package-version
						      package-depends
						      package-manifest)
		(jem-pkg-install-single-file-package filename
						     package-name
						     package-version
						     package-depends))))))))

(defun jem-pkg-install-single-file-package (filename pname pversion pdepends)
  "Install the single package called PNAME."
  (let ((package-directory (concat jem-pkg-site-base
			  "/elisp/"
			  package-name
			  (if package-version
			      (concat "%" package-version)))))
    (message (concat "Installing "
		     package-name
		     " ("
		     package-version
		     ")..."))
    (make-directory package-directory)
    (copy-file filename package-directory t)
    ;; Update the jempkg package database
    (message "Rebuilding the package database...")
    (setq jem-pkg-package-db (jem-pkg-build-package-db))
    (message "Package succesfully installed")))


(defun jem-pkg-save-sites ()
  "Save the `jem-pkg-sites' variable value as a call to
jem-pkg-define-sites in `jem-pkg-save-file'."
  (jem-pkg-save-delete 'jem-pkg-define-sites))
  
;; The following function is a copy of `custom-save-delete' from cus-edit.el
(defun jem-pkg-save-delete (symbol)
  "Delete all calls to SYMBOL from the contents of the current buffer.
Leave point at the old location of the first such call,
or (if there were none) at the end of the buffer.

This function does not save the buffer."
  (goto-char (point-min))
  ;; Skip all whitespace and comments.
  (while (forward-comment 1))
  (or (eobp)
      (save-excursion (forward-sexp (buffer-size)))) ; Test for scan errors.
  (let (first)
    (catch 'found
      (while t ;; We exit this loop only via throw.
	;; Skip all whitespace and comments.
	(while (forward-comment 1))
	(let ((start (point))
	      (sexp (condition-case nil
			(read (current-buffer))
		      (end-of-file (throw 'found nil)))))
	  (when (and (listp sexp)
		     (eq (car sexp) symbol))
	    (delete-region start (point))
	    (unless first
	      (setq first (point)))))))
    (if first
	(goto-char first)
      ;; Move in front of local variables
      (let ((pos (point-max))
	    (case-fold-search t))
	(save-excursion
	  (goto-char (point-max))
	  (search-backward "\n\^L" (max (- (point-max) 3000) (point-min))
			   'move)
	  (when (search-forward "Local Variables:" nil t)
	    (setq pos (line-beginning-position))))
	(goto-char pos)))))

(provide 'jem-pkg)
;;; jem-pkg.el ends here
