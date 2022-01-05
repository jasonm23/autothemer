;;; autothemer.el --- Conveniently define themes. -*- lexical-binding: t -*-

;; Copyright 2015-2022 Sebastian Sturm

;; Author: Sebastian Sturm
;; URL: https://github.com/sebastiansturm/autothemer
;; Version: 0.2.3
;; Package-Requires: ((dash "2.10.0") (emacs "24") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Reduces the amount of boilerplate code needed to define custom themes. Also
;; provides the user with an interactive command that automatically generates
;; face customization code using the theme's color palette.

;;; Code:
(require 'cl-lib)
(require 'dash)

(cl-defstruct autothemer--color name value)

(cl-defstruct autothemer--theme colors defined-faces)

(defvar autothemer--current-theme nil
  "Internal variable of type `autothemer--theme' used by autothemer.
Contains the color palette and the list of faces most recently
customized using `autothemer-deftheme'.")

(defun autothemer--reduced-spec-to-facespec (display reduced-specs)
  "Create a face spec for DISPLAY, with specs REDUCED-SPECS.
E.g., (autothemer--reduced-spec-to-facespec '(min-colors 60)
'(button (:underline t :foreground red)))
-> `(button (((min-colors 60) (:underline ,t :foreground
,red))))."
  (let* ((face (elt reduced-specs 0))
         (properties (elt reduced-specs 1))
         (spec (autothemer--demote-heads `(list (,display ,properties)))))
    `(list ',face ,spec)))

(defun autothemer--demote-heads (expr)
  "Demote every list head within EXPR by one element.
E.g., (a (b c d) e (f g)) -> (list a (list b c d) e (list f g))."
  (if (listp expr)
      `(list ,@(mapcar (lambda (it) (if (and (listp it) (not (eq (car it) 'quote)))
                                        (autothemer--demote-heads it) it))
                       expr))
    expr))

;;;###autoload
(defmacro autothemer-deftheme (name description palette reduced-specs &rest body)
  "Define a theme NAME with description DESCRIPTION.
A color PALETTE can be used to define let*-like
bindings within both the REDUCED-SPECS and the BODY."
  (let* ((face-names (-map #'car reduced-specs))
         (color-names (-map #'car (-drop 1 palette)))
         (n-displays (length (car palette)))
         (n-faces (length reduced-specs))
         (face-customizer)
         (full-palette (autothemer--fill-empty-palette-slots palette))
         (face-specs (make-symbol "face-specs"))
         (temp-n (make-symbol "n"))
         (temp-defined-colors (make-symbol "defined-colors"))
         (temp-color-structs (make-symbol "defined-colors-as-structs"))
         (temp-color (make-symbol "color"))
         (temp-colorname (make-symbol "colorname")))
    (setq face-customizer
          `(let ((,face-specs)
                 (,temp-color-structs)
                 (,temp-defined-colors))
             (deftheme ,name ,description)
             ,@(cl-loop for n from 0 to (1- n-displays)
                        collect
                        `(let* ,(autothemer--extract-let-block full-palette n)
                           ,@(when (and body (eq n 0))
                               body)
                           ,(when (> n 0)
                              `(ignore ,@color-names))
                           ,(when (and (eq n 0) (not (bound-and-true-p byte-compile-current-file)))
                              `(progn
                                 (setq ,temp-defined-colors
                                       (list ,@(--map (list 'list `',it it) color-names)))
                                 (setq ,temp-color-structs
                                       (cl-loop for (,temp-colorname ,temp-color)
                                                in ,temp-defined-colors
                                                collect (make-autothemer--color :name ,temp-colorname
                                                                                :value ,temp-color)))
                                 (setq autothemer--current-theme
                                       (make-autothemer--theme
                                        :colors ,temp-color-structs
                                        :defined-faces ',face-names))))
                           (setq ,face-specs
                                 (autothemer--append-column
                                  ,face-specs
                                  (list ,@(--map `(list
                                                   (list
                                                    ',(autothemer--extract-display palette n)
                                                    ,(autothemer--demote-heads (elt it 1))))
                                                 reduced-specs))
                                  ))))
             (apply #'custom-theme-set-faces ',name
                    (cl-loop for ,temp-n from 0 to ,(1- n-faces)
                             collect (list (elt ',face-names ,temp-n)
                                           (elt ,face-specs ,temp-n))))))
    face-customizer))

(defun autothemer--color-distance (color autothemer-color)
  "Return the distance in rgb space between COLOR and AUTOTHEMER-COLOR.
Here, COLOR is an Emacs color specification and AUTOTHEMER-COLOR is of
type `autothemer--color'."
  (let ((rgb-1 (color-values color))
        (rgb-2 (color-values (autothemer--color-value autothemer-color))))
    (-sum (--zip-with (abs (- it other)) rgb-1 rgb-2))))

(defun autothemer--find-closest-color (colors color)
  "Return the element of COLORS that is closest in rgb space to COLOR.
Here, COLOR is an Emacs color specification and COLORS is a list
of `autothemer--color' structs."
  (let ((mindistance 0)
        (closest-color nil))
    (mapc (lambda (candidate)
            (when (color-defined-p (autothemer--color-value candidate))
              (let ((distance (autothemer--color-distance color candidate)))
                (if (or (not closest-color) (< distance mindistance))
                    (setq closest-color candidate
                          mindistance distance)))))
          colors)
    closest-color))

(defun autothemer--unthemed-faces ()
  "Find uncustomized faces.
Iterate through all currently defined faces and return those that
were left uncustomized by the most recent call to
`autothemer-deftheme'."
  (let ((all-faces (face-list))
        (themed-faces (autothemer--theme-defined-faces autothemer--current-theme)))
    (--filter (not (-contains? themed-faces it)) all-faces)))

(defun autothemer--face-to-alist (face)
  "Return the attribute alist for FACE in frame (selected-frame)."
  (face-all-attributes face (selected-frame)))

(defun autothemer--cons-to-tree (the-cons)
  "Turn THE-CONS into a list, unless its cdr is `unspecified'."
  (let ((property-name (car the-cons))
        (property-value (cdr the-cons))
        (result))
    (unless (eq property-value 'unspecified)
      (setq result (list property-name property-value)))
    result))

(defun autothemer--alist-to-reduced-spec (facename alist)
  "Generate a reduced-spec for FACENAME, based on the face attribute ALIST."
  (list facename
        (--reduce-from (append acc it) nil
                       (mapcar 'autothemer--cons-to-tree
                               alist))))

(defun autothemer--approximate-spec (reduced-spec theme)
  "Replace colors in REDUCED-SPEC by their closest approximations in THEME.
Replace every expression in REDUCED-SPEC that passes
`color-defined-p' by the closest approximation found in
`autothemer--current-theme'.  Also quote all face names and
unbound symbols, such as `normal' or `demibold'."
  (let ((colors (autothemer--theme-colors theme))
        (face (car reduced-spec))
        (spec (cdr reduced-spec)))
    `(,face ,@(--tree-map (cond ((and (stringp it) (color-defined-p it))
                                 (autothemer--color-name
                                  (autothemer--find-closest-color colors it)))
                                ((stringp it) it)
                                ((numberp it) it)
                                ((facep it) `(quote ,it))
                                ((not (boundp it)) `(quote ,it))
                                (t it))
                          spec))))

(defun autothemer--pad-with-nil (row min-number-of-elements)
  "Make sure that ROW has at least MIN-NUMBER-OF-ELEMENTS, pad with nil if necessary."
  (append row (-repeat (max 0 (- min-number-of-elements (length row))) nil)))

(defun autothemer--replace-nil-by-precursor (palette-row)
  "Iterate over elements of PALETTE-ROW and replace every occurrence of nil by its most recent non-nil precursor.  The first element of PALETTE-ROW should be non-nil."
  (cl-assert (car palette-row))
  (let* ((color-name (car palette-row))
         (color-definitions (cdr palette-row))
         (last-definition))
    (cons color-name
          (cl-loop for definition in color-definitions
                   do (when definition (setq last-definition definition))
                   collect last-definition))))

(defun autothemer--fill-empty-palette-slots (palette)
  "Make sure that every color definition in PALETTE (elements 1 and above) contain exactly (length (car palette)) elements, corresponding to the displays defined in (car palette)."
  (let ((n-displays (length (car palette))))
    (cons (car palette)
          (cl-loop for row in (cdr palette)
                   collect (autothemer--replace-nil-by-precursor
                            (autothemer--pad-with-nil row (1+ n-displays)))))))

(defun autothemer--extract-display (palette n)
  "Extract from PALETTE display specification #N."
  (elt (car palette) n))

(defun autothemer--extract-let-block (palette n)
  "Extract a variable definition block from PALETTE containing all color definitions corresponding to display type #N."
  (cl-loop for row in (cdr palette)
           collect (list (car row) (elt row (1+ n)))))

;;;###autoload
(defun autothemer-generate-templates ()
  "Autogenerate customizations for all unthemed faces.
Iterate through all currently defined faces, select those that
have been left uncustomized by the most recent call to
`autothemer-deftheme' and generate customizations that best
approximate the faces' current definitions using the color
palette used in the most recent invocation of
`autothemer-deftheme'."
  (interactive)
  (let* ((missing-faces (autothemer--unthemed-faces))
         (templates (--map (autothemer--approximate-spec
                            (autothemer--alist-to-reduced-spec
                             it (autothemer--face-to-alist it))
                            autothemer--current-theme)
                           missing-faces))
         (buffer (get-buffer-create (generate-new-buffer-name "*Autothemer: unthemed faces*"))))
    (with-current-buffer buffer (emacs-lisp-mode) (insert (pp templates)))
    (switch-to-buffer buffer)))

(cl-defsubst autothemer--append-column (list-of-lists new-column)
  "If LIST-OF-LISTS is nil, return NEW-COLUMN.  Otherwise, append to every element of LIST-OF-LISTS the corresponding element of NEW-COLUMN."
  (cl-assert (or (not list-of-lists) (eq (length list-of-lists) (length new-column))))
  (if list-of-lists (inline (-zip-with #'append list-of-lists new-column))
    new-column))

(provide 'autothemer)
;;; autothemer.el ends here
