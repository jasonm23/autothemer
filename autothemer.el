;;; autothemer.el --- Conveniently define themes. -*- lexical-binding: t -*-

;; Copyright 2015 Sebastian Sturm

;; Author: Sebastian Sturm
;; URL: https://github.com/sebastiansturm/autothemer
;; Version: 0.2.2
;; Package-Requires: ((dash "2.10.0") (emacs "24") (cl-lib "0.5"))

;;; Commentary:

;; Reduces the amount of boilerplate code needed to define custom themes. Also
;; provides the user with an interactive command that automatically generates
;; face customization code using the theme's color palette.

;;; Code:
(require 'cl-lib)
(require 'dash)

(defcustom autothemer-color-metric 'Lab
  "Color space within which autothemer determines color distances
 (using the Euclidean distance metric). Possible values are 'rgb, 'xyz and 'Lab."
  :type '(choice (const :tag "Lab" Lab)
                 (const :tag "RGB" rgb)
                 (const :tag "XYZ" xyz))
  :group 'autothemer)
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
                 (,temp-defined-colors))
             (deftheme ,name ,description)
             ,@(cl-loop for n from 0 to (1- n-displays)
                        collect
                        `(let* ,(autothemer--extract-let-block full-palette n)
                           ,@(when (and body (eq n 0))
                               body)
                           ;; FIXME: instead of emitting this code for all n,
                           ;; and including a runtime check for n = 0, the when
                           ;; clause below should only be emitted for n = 0 in
                           ;; the first place
                           (when ,(eq n 0)
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
                                    :defined-faces ',face-names)))
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

;; the following color conversion functions have been taken from www.brucelindbloom.com,
;; hopefully without introducing too many transcription (or interpretation) errors
;; along the way
(defun autothemer--srgb-to-xyz (srgb-color)
  "Convert an sRGB triplet to XYZ. Cf. www.brucelindbloom.com"
  (cl-flet ((V-to-v (V) (if (<= V 0.04045) (/ V 12.92)
                          (expt (/ (+ V 0.055) 1.055) 2.4))))
    (let ((r (V-to-v (/ (nth 0 srgb-color) 65535.0)))
          (g (V-to-v (/ (nth 1 srgb-color) 65535.0)))
          (b (V-to-v (/ (nth 2 srgb-color) 65535.0))))
      (list (+ (* 0.4124564 r) (* 0.3575761 g) (* 0.1804375 b))
            (+ (* 0.2126729 r) (* 0.7151522 g) (* 0.0721750 b))
            (+ (* 0.0193339 r) (* 0.1191920 g) (* 0.9503041 b))))))

(defun autothemer--xyz-to-lab (xyz-color)
  "Convert an XYZ color triplet to Lab space, cf. www.brucelindbloom.com."
  ;; using (0.95047 1 1.08883) as the reference white point
  (let ((eps 0.008856)
        (kappa 903.3)
        (x (/ (nth 0 xyz-color) 0.95047))
        (y (nth 1 xyz-color))
        (z (/ (nth 2 xyz-color) 1.08883)))
    (cl-flet ((f (val) (if (> val eps) (expt val 0.333333)
                         (/ (+ 16 (* kappa val)) 116))))
      (list (- (* 116 (f y)) 16)
            (* 500 (- (f x) (f y)))
            (* 200 (- (f y) (f z)))))))

(defun autothemer--srgb-to-lab (srgb-color)
  "Convert an sRGB color triplet to Lab space, cf. www.brucelindbloom.com."
  (autothemer--xyz-to-lab (autothemer--srgb-to-xyz srgb-color)))

(defun autothemer--color-distance (color autothemer-color color-fn)
  "Return the euclidean distance between COLOR and AUTOTHEMER-COLOR,
where COLOR is an Emacs color specification, AUTOTHEMER-COLOR is of
type `autothemer--color' and COLOR-FN is a function mapping rgb values
(with all components ranging between 0 and 65535) to the three-dimensional
target color space. If COLOR-FN is the identity function,
`autothemer--color-distance' effectively measures the Euclidean distance
in RGB space. Using the conversion functions `autothemer--srgb-to-xyz' and
`autothemer--srgb-to-lab', the conversion can alternatively be performed in
the XYZ or Lab color space."
  (let* ((rgb-1 (color-values color))
         (rgb-2 (color-values (autothemer--color-value autothemer-color)))
         (coords-1 (funcall color-fn rgb-1))
         (coords-2 (funcall color-fn rgb-2)))
    (-sum (--zip-with (abs (- it other)) coords-1 coords-2))))

(defun autothemer--find-closest-color (colors color)
  "Return the element of COLORS that is closest in rgb space to COLOR.
Here, COLOR is an Emacs color specification and COLORS is a list
of `autothemer--color' structs."
  (cl-flet ((color-fn (case autothemer-color-metric
                        ('rgb #'identity)
                        ('xyz #'autothemer--srgb-to-xyz)
                        ('Lab #'autothemer--srgb-to-lab))))
    (let ((mindistance 0) (closest-color nil))
      (mapc (lambda (candidate)
              (when (color-defined-p (autothemer--color-value candidate))
                (let ((distance (autothemer--color-distance color candidate #'color-fn)))
                  (if (or (not closest-color) (< distance mindistance))
                      (setq closest-color candidate
                            mindistance distance)))))
            colors)
      closest-color)))

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
  (if (and (boundp 'autothemer--current-theme)
           autothemer--current-theme)
      (let* ((missing-faces (autothemer--unthemed-faces))
             (templates (--map (autothemer--approximate-spec
                                (autothemer--alist-to-reduced-spec
                                 it (autothemer--face-to-alist it))
                                autothemer--current-theme)
                               missing-faces))
             (buffer (get-buffer-create (generate-new-buffer-name
                                         "*Autothemer: unthemed faces*"))))
        (with-current-buffer buffer (emacs-lisp-mode) (insert (pp templates)))
        (switch-to-buffer buffer))
    (message "Error: for `autothemer-generate-templates' to work, you first need to define
a theme using `autothemer-deftheme'.")))

(defun autothemer--append-column (list-of-lists new-column)
  "If LIST-OF-LISTS is nil, return NEW-COLUMN.  Otherwise, append to every element of LIST-OF-LISTS the corresponding element of NEW-COLUMN."
  (cl-assert (or (not list-of-lists) (eq (length list-of-lists) (length new-column))))
  (if list-of-lists (-zip-with #'append list-of-lists new-column)
    new-column))

(provide 'autothemer)
;;; autothemer.el ends here
