;;; autothemer.el --- Conveniently define themes. -*- lexical-binding: t -*-
;; TODO: Allow the user to define different colors for different terminal types,
;;       maybe also allow for finer-grained distinctions as made, e.g.,
;;       by solarized-emacs?
;;; Commentary:
;;
;; TODO: add description
;;; Code:
(require 'cl)
(require 'dash)

(cl-defstruct autothemer--color name value)

(cl-defstruct autothemer--theme colors defined-faces)

(defvar autothemer--current-theme nil
  "Internal variable of type `autothemer--theme' used by autothemer.
Contains the color palette and the list of faces most recently
customized using `autothemer-defautotheme'.")

(defun autothemer--reduced-spec-to-facespec (reduced-spec)
  "Convert REDUCED-SPEC into a face spec.
E.g., (button (:underline t :foreground red))
-> `(button ((t (:underline ,t :foreground ,red))))."
  (let* ((face (elt reduced-spec 0))
         (properties (elt reduced-spec 1))
         (spec (autothemer--demote-heads `(list (t ,properties)))))
    `(list ',face ,spec)))

(defun autothemer--demote-heads (expr)
  "Demote every list head within EXPR by one element.
E.g., (a (b c d) e (f g)) -> (list a (list b c d) e (list f g))."
  (mapcar (lambda (it) (if (and (listp it) (not (eq (car it) 'quote)))
                           `(list ,@(autothemer--demote-heads it)) it))
          expr))

;;;###autoload
(defmacro autothemer-defautotheme (name description colors reduced-specs &rest body)
  "Define a theme NAME with description DESCRIPTION.
A set of color definitions COLORS can be used as let-like
bindings within both the REDUCED-SPECS and the arbitrary BODY.
As an example, the following snippet defines a theme named
useless-theme that customizes the faces button and error
and prints the color value #550000.

(autothemer-defautotheme useless-theme \"This is a truly useless theme.\"
                         ((red \"#550000\") (orange \"DarkGoldenrod4\"))
                         ((error (:underline t :foreground red :weight 'demibold))
                          (button (:box nil :background orange)))
                         (print red))"
  (let ((temp-defined-colors (make-symbol "defined-colors"))
        (temp-color-structs (make-symbol "color-structs")))
    `(let* (,@colors)
       (deftheme ,name ,description)
       (setq ,temp-defined-colors
             (list ,@(--map (list 'list `',(car it) (car it)) colors)))
       (setq ,temp-color-structs
             (cl-loop for (colorname color) in ,temp-defined-colors
                      collect (make-autothemer--color :name colorname
                                                      :value color)))
       (setq autothemer--current-theme
             (make-autothemer--theme
              :colors ,temp-color-structs
              :defined-faces (list ,@(--map `',(car it) reduced-specs))))
       ,@body
       (custom-theme-set-faces ',name
                               ,@(-map 'autothemer--reduced-spec-to-facespec reduced-specs)))))

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
            (let ((distance (autothemer--color-distance color candidate)))
              (if (or (not closest-color) (< distance mindistance))
                  (setq closest-color candidate
                        mindistance distance))))
          colors)
    closest-color))

(defun autothemer--unthemed-faces ()
  "Find uncustomized faces.
Iterate through all currently defined faces and return those that
were left uncustomized by the most recent call to
`autothemer-defautotheme'."
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

(defun autothemer--transform-reduced-spec (reduced-spec theme)
  "Replace colors in REDUCED-SPEC by their closest approximations in THEME.
Replace every expression in REDUCED-SPEC that passes
`color-defined-p' by the closest approximation found in
`autothemer--current-theme'.  Also quote all face names and
unbound symbols, such as `normal' or `demibold'."
  (let ((colors (autothemer--theme-colors theme))
        (face (car reduced-spec))
        (spec (cdr reduced-spec)))
    `(,face ,@(--tree-map (cond ((color-defined-p it)
                                 (autothemer--color-name
                                  (autothemer--find-closest-color colors it)))
                                ((stringp it) it)
                                ((numberp it) it)
                                ((facep it) `(quote ,it))
                                ((not (boundp it)) `(quote ,it))
                                (t it))
                          spec))))


;;;###autoload
(defun autothemer-generate-templates ()
  "Autogenerate customizations for all unthemed faces.
Iterate through all currently defined faces, select those that
have been left uncustomized by the most recent call to
`autothemer-defautotheme' and generate customizations that best
approximate the faces' current definitions using the color
palette used in the most recent invocation of
`autothemer-defautotheme'."
  (interactive)
  (let* ((missing-faces (autothemer--unthemed-faces))
         (templates (--map (autothemer--transform-reduced-spec
                            (autothemer--alist-to-reduced-spec
                             it (autothemer--face-to-alist it))
                            autothemer--current-theme)
                           missing-faces))
         (buffer (get-buffer-create (generate-new-buffer-name "*Autothemer: unthemed faces*"))))
    (with-current-buffer buffer (emacs-lisp-mode) (insert (pp templates)))
    (switch-to-buffer buffer)))

;; (macroexpand '(autothemer-defautotheme
;;                sometheme "somethemename"
;;                ((red "#880011")
;;                 (orange "DarkGoldenrod4")
;;                 (blue "#001199"))
;;                ((flycheck-error (:underline t :foreground red :weight 'demibold))
;;                 (button (:box nil :background blue))
;;                 ;; etc.
;;                 )
;;                ;; execute code that uses red, orange, blue
;;                ))


(provide 'autothemer)
;;; autothemer.el ends here
