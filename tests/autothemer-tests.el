;; autothemer-tests.el

;;; Code:

(require 'autothemer)

(progn "Test autothemer-deftheme"
  (autothemer-deftheme theme-example
   "Autothemer example..."

    ;; Specify the color classes used by the theme
    ((((class color) (min-colors #xFFFFFF))
      ((class color) (min-colors #xFF)))

     ;; Specify the color palette for each of the classes above.
     (example-red    "#781210" "#FF0000")
     (example-green  "#22881F" "#00D700")
     (example-blue   "#212288" "#0000FF")
     (example-purple "#812FFF" "#Af00FF")
     (example-yellow "#EFFE00" "#FFFF00")
     (example-orange "#E06500" "#FF6600")
     (example-cyan   "#22DDFF" "#00FFFF"))

    ;; specifications for Emacs faces.
    ((button (:underline t :weight 'bold :foreground example-yellow))
     (error  (:foreground example-red)))

    ;; Forms after the face specifications are evaluated.
    ;; (palette vars can be used, read below for details.)
    (custom-theme-set-variables
     'theme-example
     `(ansi-color-names-vector
       [,example-red
        ,example-green
        ,example-blue
        ,example-purple
        ,example-yellow
        ,example-orange
        ,example-cyan])))

  (ert-deftest current-theme ()
    "Test current theme is available."
    (should (not (null
                  autothemer--current-theme))))

  (ert-deftest theme-has-colors ()
    "Check theme has colors."
    (should (eql 7 (length (autothemer--theme-colors
                            autothemer--current-theme)))))

  (ert-deftest theme-has-face-specs ()
    "Check theme has face specs."
    (should (eql 2 (length (autothemer--theme-defined-faces
                            autothemer--current-theme)))))

  (ert-deftest color-value ()
    "Check color value."
    (should (string= "#781210"
                     (autothemer--color-value
                      (car (autothemer--theme-colors
                            autothemer--current-theme))))))

  (ert-deftest color-name ()
    "Check color name."
    (should (string= "example-red"
                     (autothemer--color-name
                      (car (autothemer--theme-colors
                            autothemer--current-theme))))))

  (ert-deftest spec-name ()
    "Check spec name."
    (should (equal 'button
                      (car (autothemer--theme-defined-faces
                                  autothemer--current-theme)))))

  (ert-deftest theme-has-description ()
    "Check theme description."
    (should (string=
             "Autothemer example..."
             (autothemer--theme-description
              autothemer--current-theme))))

  (ert-deftest theme-has-name ()
    "Check theme name."
    (should (string=
             "theme-example"
             (autothemer--theme-name
              autothemer--current-theme))))

  (ert-deftest let-palette ()
    "Check autothemer-let-palette"
    (should (string=
             "#781210"
             (autothemer-let-palette example-red))))

  (ert-deftest autothemer-plist-bind ()
    "Test plist-bind"
    (autothemer--plist-bind (a b) '(:a 1 :b 2)
     (should (eql a 1))
     (should (eql b 2))))

  (ert-deftest autothemer-colorize-alist ()
    "Check autothemer-colorize-alist."
    (should (equal '(("example-red" . "#781210")
                     ("example-green" . "#22881F")
                     ("example-blue" . "#212288")
                     ("example-purple" . "#812FFF")
                     ("example-yellow" . "#EFFE00")
                     ("example-orange" . "#E06500")
                     ("example-cyan" . "#22DDFF"))
                   (autothemer-colorize-alist)))))

;;; Example theme in memory:
'(#s(autothemer--theme
     (#s(autothemer--color example-red "#781210")
        #s(autothemer--color example-green "#22881F")
        #s(autothemer--color example-blue "#212288")
        #s(autothemer--color example-purple "#812FFF")
        #s(autothemer--color example-yellow "#EFFE00")
        #s(autothemer--color example-orange "#E06500")
        #s(autothemer--color example-cyan "#22DDFF"))
     (button error)
     "theme-example" "Autothemer example..."))

;;; autothemer-tests.el ends here
