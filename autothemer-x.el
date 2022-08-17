;;; autothemer-x.el --- Autothemer experimental... -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jason M23
;;
;; Author: Jason M23 <jasonm23@gmail.com>
;; Maintainer: Jason M23 <jasonm23@gmail.com>
;; Created: August 17, 2022
;; Modified: August 17, 2022
;; Version: 0.0.1
;; Homepage: https://github.com/jasonm23/autothemer
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Autothemer experimental stuff.
;;
;;; Code:

(require 's)
(require 'f)
(require 'dash)
(require 'autothemer)

(defun autothemer--filtered-face-list (regexp)
  "Return a list subset of the face list filtered by REGEXP."
  (--filter (s-matches? regexp (format "%s" it)) (face-list)))

(defvar autothemer--cached-known-theme-names nil
  "Cached names of known autothemer theme names.

These are the file basenames of the theme, not the actual theme names.

Use autothemer--theme-variants to find variants.")

(defun autothemer--theme-variants (theme-library)
  "Return a list variant names for THEME-LIBRARY."
   (--map
    (s-replace ".el" "" (f-filename it))
    (autothemer--theme-variant-files theme-library)))
(autothemer--theme-variants "gruvbox")

(defun autothemer--theme-variant-files (theme-library)
  "Return a list theme variant filenames for THEME-LIBRARY.

Given the THEME-NAME, find other emacs-lisp files in the library
folder (recursive).

Files named `*theme-name*(.*)-theme.el$' will be assumed to be theme variants.

See TVA in README.md

Theme Variance Architecture (TVA):

Example theme package library folder:

- gruvbox.el <- the theme library
- gruvbox-theme.el              < themes
- gruvbox-dark-hard-theme.el    <
- gruvbox-dark-medium-theme.el  <
- gruvbox-dark-soft-theme.el    <
... etc.

`gruvbox.el' has the following macro definition:

    (defmacro gruvbox-deftheme (name description palette &rest body))

Themes `gruvbox-dark-hard-theme.el' etc. use the
`gruvbox-deftheme' macro to declare their palette (keeping the
same color variable names!) The face specs shared in `gruvbox.el'
/ `gruvbox-deftheme' cut out duplication and allow simple
maintenance and effortless creation of multiple palettes / theme
variants.
"
  (f-entries
   (f-dirname
    (locate-library theme-library))
   (lambda (filename)
     (s-matches?
      (format "%s\\(.*?\\)-theme[.]el$" theme-library)
      filename))))

(defun autothemer--known-themes (&optional refresh)
  "Return a list of known installed autothemer themes.

Optional REFRESH will rebuild the cached list of known-themes.

Uses `autothemer--cached-known-theme-names' to cache known-themes."
  (if (or refresh (null autothemer--cached-known-theme-names))
    (let* ((theme-names (mapcar 'symbol-name (custom-available-themes)))
           (autothemer-theme-packages (--filter (let ((filename (locate-library it)))
                                                  (when filename
                                                    (s-matches? "(autothemer-deftheme"
                                                                (f-read-text filename))))
                                        (-flatten (-map
                                                   (lambda (name)
                                                    (if (s-ends-with? "theme" name)
                                                        `(,name)
                                                      `(,name ,(format "%s-theme" name))))
                                                   theme-names))))
           (autothemer-libaries (--reject
                                  (s-contains-p "-theme" it)
                                  autothemer-theme-packages))
           (autothemer-variants (--map
                                 (autothemer--theme-variants it)
                                 autothemer-libaries)))
      (setq autothemer--cached-known-theme-names
            (--reject
             (memq it autothemer-libaries)
             (-flatten (list autothemer-variants autothemer-theme-packages)))))
    autothemer--cached-known-theme-names))

(defun autothemer--select-attribute-values (face attributes)
  "For the given FACE, Set values for each ATTRIBUTE."
  (if (null autothemer--current-theme)
      (load-file (locate-library (completing-read "Select theme: " (autothemer--known-themes))))
    (-map
     (lambda (attribute)
       (let ((value (completing-read
                     (format "%s %s: " face attribute)
                     (--map (autothemer--color-name it)
                            (autothemer--theme-colors
                             autothemer--current-theme)))))
         (if (string= "" value)
             ""
           (format "%s %s" attribute value))))
     attributes)))

(defun autothemer-insert-stubs (regexp &rest attributes)
  "Insert a set of autothemer face stubs, filtered by REGEXP.
Stubs will be filled using ATTRIBUTES..."
  (interactive
   `(,(if (and (boundp 'regexp) (not (null regexp)))
          regexp
        (completing-read "Filter faces (regexp): " (face-list)))
     ,(if (and (boundp 'attributes) (not (null attributes)))
          attributes
        (completing-read-multiple
         "Choose attributes: " '(":foreground" ":background" ":inherit")))))
         
  (let* ((faces (--map
                 (symbol-name it)
                 (autothemer--filtered-face-list regexp)))
         (max-name-width
          (--reduce-from
           (max acc (length it))
           0
           faces))
         (attributes (if (stringp (car attributes))
                         attributes
                       (car attributes)))
         (spec-format (format "(%%-%is (%%s))" max-name-width)))
    (insert
     (s-join "\n"
             (--map
              (format spec-format it
                      (s-join " " (autothemer--select-attribute-values it attributes)))
              faces)))))

(defun autothemer-get-face-specs (regexp &optional theme)
  "Get the specs for faces which match REGEXP.
Get specs from `autothemer--current-theme' or optionally THEME."
  (ignore-errors
   (when theme (load-file (locate-library theme)) (load-theme (intern (s-replace "-theme" "" theme)) t)))
  (let ((faces (--filter
                (s-matches-p regexp (format "%s" it))
                (autothemer--theme-defined-faces
                 autothemer--current-theme))))
     (-map (lambda (face)
             (let ((specs (-flatten
                            (--map (-cons-to-list it)
                              (ignore-errors
                                (autothemer--face-to-alist face))))))
              (list face (--reject
                              (s-contains-p "unspecified" (cdr it))
                          specs))))
      faces)))

(autothemer-get-face-specs "cursor" "darktooth-theme")

(provide 'autothemer-x)
;;; autothemer-x.el ends here
