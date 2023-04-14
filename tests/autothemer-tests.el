;; autothemer-tests.el

;; Version: 0.2.18

;;; Code:

(require 'autothemer)

;;; Test Helpers

(defun name-color-to-struct (C)
  "Cons C with name & value to `autothemer--color` struct."
  (make-autothemer--color :name (car C) :value (cdr C)))

(defun struct-to-cons-name-color (S)
  "S (`autothemer--color` struct) to `(cons name color)'."
  (cons
   (autothemer--color-name S)
   (autothemer--color-value S)))

;;; Tests

(progn "Test autothemer-deftheme"

;;; Example theme
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

;;; autothemer-current-theme is set.

  (ert-deftest current-theme ()
    "Test current theme is available."
    (should (not (null
                  autothemer-current-theme))))

  (ert-deftest theme-has-colors ()
    "Check theme has colors."
    (should (eql 7 (length (autothemer--theme-colors
                            autothemer-current-theme)))))

  (ert-deftest theme-has-face-specs ()
    "Check theme has face specs."
    (should (eql 2 (length (autothemer--theme-defined-faces
                            autothemer-current-theme)))))

  (ert-deftest color-value ()
    "Check color value."
    (should (string= "#781210"
                     (autothemer--color-value
                      (car (autothemer--theme-colors
                            autothemer-current-theme))))))

  (ert-deftest color-name ()
    "Check color name."
    (should (string= "example-red"
                     (autothemer--color-name
                      (car (autothemer--theme-colors
                            autothemer-current-theme))))))

  (ert-deftest spec-name ()
    "Check spec name."
    (should (equal 'button
                      (car (autothemer--theme-defined-faces
                                  autothemer-current-theme)))))

  (ert-deftest theme-has-description ()
    "Check theme description."
    (should (string=
             "Autothemer example..."
             (autothemer--theme-description
              autothemer-current-theme))))

  (ert-deftest theme-has-name ()
    "Check theme name."
    (should (string=
             "theme-example"
             (autothemer--theme-name
              autothemer-current-theme))))

;;; Let palette

  (ert-deftest let-palette ()
    "Check autothemer-let-palette"
    (should (string=
             "#781210"
             (autothemer-let-palette example-red))))

;;; AT Helper functions

  (ert-deftest unindent ()
    "Test unindent."
    (should
      (string=
        (autothemer--unindent "|Hello world
                               |  Foo bar
                               |  Indent
                               |")
        "Hello world\n  Foo bar\n  Indent\n")))

  (ert-deftest autothemer-plist-bind ()
    "Test plist-bind."
    (autothemer--plist-bind (a b) '(:a 1 :b 2)
     (should (eql a 1))
     (should (eql b 2))))

;;; Color conversion

  (ert-deftest autothemer-hex-to-rgb ()
    "Test hex to rgb."
   (should (equal '(0 0 0) (autothemer-hex-to-rgb "#000000")))
   (should (equal '(65535 65535 65535) (autothemer-hex-to-rgb "#FFFFFF")))
   (should (equal '(65535 0 0) (autothemer-hex-to-rgb "#FF0000")))
   (should (equal '(65535 65535 0) (autothemer-hex-to-rgb "#FFFF00")))
   (should (equal '(0 65535 0) (autothemer-hex-to-rgb "#00FF00")))
   (should (equal '(0 65535 65535) (autothemer-hex-to-rgb "#00FFFF")))
   (should (equal '(0 0 65535) (autothemer-hex-to-rgb "#0000FF")))
   (should (equal '(32896 32896 32896) (autothemer-hex-to-rgb "#808080"))))

  (ert-deftest autothemer--color-to-hsv ()
    "Test color to hsv conversion."
    (should (equal (autothemer--color-to-hsv '(0 0 0)) '(0.0 0.0 0.0)))
    (should (equal (autothemer--color-to-hsv '(65535 65535 65535)) '(0.0 0.0 1.0)))
    (should (equal (autothemer--color-to-hsv '(0 0 65535)) '(0.6666666666666666 1.0 1.0)))
    (should (equal (autothemer--color-to-hsv '(12896 0 32896)) '(0.7320038910505837 1.0 0.5019607843137255))))

;;; HSV Color components

  (ert-deftest autothemer-color-hue ()
   "Test get hue of hex-color."
    (should (= (autothemer-color-hue #s(autothemer--color example-color-020 "#2391CB")) 0.5575396825396826))
    (should (= (autothemer-color-hue "#FF0000") 0))
    (should (= (autothemer-color-hue "#FFFF00") 0.16666666666666666))
    (should (= (autothemer-color-hue "#00FF00") 0.33333333333333333))
    (should (= (autothemer-color-hue "#0000FF") 0.66666666666666666)))
  
  (ert-deftest autothemer-color-sat ()
    "Test get sat of hex-color."
    (should (= (autothemer-color-sat #s(autothemer--color example-color-020 "#2391CB")) 0.8275862068965516))
    (should (= (autothemer-color-sat "#0000FF") 1.0))
    (should (= (autothemer-color-sat "#FF00FF") 1.0))
    (should (= (autothemer-color-sat "#778822") 0.75))
    (should (= (autothemer-color-sat "#772288") 0.75))
    (should (= (autothemer-color-sat "#112233") 0.6666666666666667)))
  
  (ert-deftest autothemer-color-brightness ()
    "Test get brightness of hex-color."
    (should (= (autothemer-color-brightness #s(autothemer--color example-color-020 "#2391CB")) 0.796078431372549))
    (should (= (autothemer-color-brightness "#0000FF") 1.0))
    (should (= (autothemer-color-brightness "#00FF00") 1.0))
    (should (= (autothemer-color-brightness "#FF00FF") 1.0))
    (should (= (autothemer-color-brightness "#333333") 0.2))
    (should (= (autothemer-color-brightness "#555555") 0.3333333333333333)))

;;; Color distance

  (ert-deftest autothemer--color-distance ()
    "Test color distance."
    (let ((color-struct (make-autothemer--color :name "Test" :value "#100000")))
     (should (eql (autothemer--color-distance "#100000" color-struct) 0))
     (should (eql (autothemer--color-distance "#100001" color-struct) 257))
     (should (eql (autothemer--color-distance "#000001" color-struct) 4369))
     (should (eql (autothemer--color-distance "#FF0000" color-struct) 61423))))

  (ert-deftest autothemer-cie-de2000 ()
    "Test color distance with CIE DE2000."
    (let ((color-struct (make-autothemer--color :name "Test" :value "#100000")))
     (should (eql (autothemer-cie-de2000 "#100000" color-struct) 0.0))
     (should (eql (autothemer-cie-de2000 "#100001" color-struct) 0.38178419390755014))
     (should (eql (autothemer-cie-de2000 "#000001" color-struct) 5.891618859336162))
     (should (eql (autothemer-cie-de2000 "#FF0000" color-struct) 48.817322029166725))))

  (ert-deftest autothemer-rgb-to-hex ()
    (should (equal (autothemer-rgb-to-hex '(0 0 0)) "#000000"))
    (should (equal (autothemer-rgb-to-hex '(65535 0 0)) "#FF0000"))
    (should (equal (autothemer-rgb-to-hex '(65535 65535 65535)) "#FFFFFF")))

;;; Colorization

  (ert-deftest autothemer--colorize-alist ()
    "Check autothemer-colorize-alist."
    (should (equal '(("example-red" . "#781210")
                     ("example-green" . "#22881F")
                     ("example-blue" . "#212288")
                     ("example-purple" . "#812FFF")
                     ("example-yellow" . "#EFFE00")
                     ("example-orange" . "#E06500")
                     ("example-cyan" . "#22DDFF"))
                   (autothemer--colorize-alist))))

;;; Color/Palette grouping & sorting

  (ert-deftest autothemer-color-hue-grouping ()
    "Test autothemer-color-hue-group."
    (should (equal (autothemer-hue-grouping "#FF0005") 'red))
    (should (equal (autothemer-hue-grouping "#00FF00") 'green))
    (should (equal (autothemer-hue-grouping "#FF00FF") 'magenta))
    (should (equal (autothemer-hue-grouping "#00FFFF") 'cyan))
    (should (equal (autothemer-hue-grouping "#0000FF") 'blue-magenta))
    (should (equal (autothemer-hue-grouping "#FFFF00") 'yellow-green))
    (should (equal (autothemer-hue-grouping "#FFFF00" autothemer-hue-groups) 'yellow-green))
    (should (equal (autothemer-hue-grouping "#FFFF00" autothemer-simple-hue-groups) 'green)))

  (ert-deftest autothemer-brightness-grouping ()
    "Test autothemer-brightness-group."
    (should (equal (autothemer-brightness-grouping "#FF0005") 'brightness-080-100-percent))
    (should (equal (autothemer-brightness-grouping "#007700") 'brightness-040-060-percent))
    (should (equal (autothemer-brightness-grouping "#FF55FF") 'brightness-080-100-percent))
    (should (equal (autothemer-brightness-grouping "#004444") 'brightness-020-040-percent))
    (should (equal (autothemer-brightness-grouping "#020202") 'brightness-000-020-percent))
    (should (equal (autothemer-brightness-grouping
                    "#020202"
                    autothemer-dark-mid-light-brightness-groups)
                   'dark))
    (should (equal (autothemer-brightness-grouping
                    "#777777"
                    autothemer-dark-mid-light-brightness-groups)
                   'mid)))

  (ert-deftest autothemer-saturation-grouping ()
    "Test autothemer-saturation-group."
    (should (equal (autothemer-saturation-grouping "#FF0005") 'saturation-080-100-percent))
    (should (equal (autothemer-saturation-grouping "#007700") 'saturation-080-100-percent))
    (should (equal (autothemer-saturation-grouping "#FF55FF") 'saturation-060-080-percent))
    (should (equal (autothemer-saturation-grouping "#004444") 'saturation-080-100-percent))
    (should (equal (autothemer-saturation-grouping "#020202") 'saturation-000-020-percent))
    (should (equal (autothemer-saturation-grouping
                    "#020202"
                    autothemer-low-mid-high-saturation-groups)
                   'low))
    (should (equal (autothemer-saturation-grouping
                    "#336677"
                    autothemer-low-mid-high-saturation-groups)
                   'mid)))

  (ert-deftest autothemer-group-colors ()
    "Group colors into a plist of color lists, with group names as keys."
    (should (equal
             (autothemer-group-colors
              (list
               (make-autothemer--color :name 'example-color-005 :value "#112063")
               (make-autothemer--color :name 'example-color-006 :value "#88DDCC")
               (make-autothemer--color :name 'example-color-006 :value "#99DDCC")
               (make-autothemer--color :name 'example-color-006 :value "#FFDDCC")
               (make-autothemer--color :name 'example-color-006 :value "#FFEECC")
               (make-autothemer--color :name 'example-color-007 :value "#281993")
               (make-autothemer--color :name 'example-color-010 :value "#240933"))
              (list :group-fn 'autothemer-saturation-grouping
                    :group-args autothemer-low-mid-high-saturation-groups))
             '((low
                #s(autothemer--color example-color-006 "#99DDCC")
                #s(autothemer--color example-color-006 "#FFDDCC")
                #s(autothemer--color example-color-006 "#FFEECC"))
               (mid
                #s(autothemer--color example-color-006 "#88DDCC"))
               (high
                #s(autothemer--color example-color-005 "#112063")
                #s(autothemer--color example-color-007 "#281993")
                #s(autothemer--color example-color-010 "#240933"))))))

  (ert-deftest autothemer-group-and-sort ()
    "Group and sort a palette of `autothemer--color' structs."
    (let ((result (autothemer-group-and-sort
                     (mapcar
                      'name-color-to-struct
                      '((example-color-001 . "#702414")
                        (example-color-002 . "#642C12")
                        (example-color-003 . "#583410")
                        (example-color-004 . "#191204")
                        (example-color-005 . "#181818")
                        (example-color-006 . "#191904")
                        (example-color-007 . "#373D0A")
                        (example-color-008 . "#243108")
                        (example-color-009 . "#162506")
                        (example-color-010 . "#224C0E")
                        (example-color-011 . "#287C16")
                        (example-color-012 . "#0E4C0E")
                        (example-color-013 . "#147024")
                        (example-color-014 . "#0E4C22")
                        (example-color-015 . "#167C49")
                        (example-color-016 . "#20BE87")
                        (example-color-017 . "#28E4C4")
                        (example-color-018 . "#1AA4A4")
                        (example-color-019 . "#178297")
                        (example-color-020 . "#2391CB")
                        (example-color-021 . "#13416F")
                        (example-color-022 . "#13306F")
                        (example-color-023 . "#112063")
                        (example-color-024 . "#0D0D4B")
                        (example-color-025 . "#281993")
                        (example-color-026 . "#170933")
                        (example-color-027 . "#620FA9")
                        (example-color-028 . "#240933")
                        (example-color-029 . "#63136F")
                        (example-color-030 . "#330933")
                        (example-color-031 . "#971782")
                        (example-color-032 . "#D62499")
                        (example-color-033 . "#A41A5F")
                        (example-color-034 . "#D82662")
                        (example-color-035 . "#B11D37")
                        (example-color-036 . "#E52929")))
                   '(:group-fn 'autothemer-hue-grouping
                     :group-args autothemer-simple-hue-groups
                     :sort-fn 'autothemer-darkest-order)))

          (expected '((red
                       #s(autothemer--color example-color-005 "#181818")
                       #s(autothemer--color example-color-002 "#642C12")
                       #s(autothemer--color example-color-001 "#702414")
                       #s(autothemer--color example-color-035 "#B11D37")
                       #s(autothemer--color example-color-036 "#E52929"))
                      (orange
                       #s(autothemer--color example-color-004 "#191204")
                       #s(autothemer--color example-color-003 "#583410"))
                      (green
                       #s(autothemer--color example-color-006 "#191904")
                       #s(autothemer--color example-color-009 "#162506")
                       #s(autothemer--color example-color-008 "#243108")
                       #s(autothemer--color example-color-007 "#373D0A")
                       #s(autothemer--color example-color-010 "#224C0E")
                       #s(autothemer--color example-color-012 "#0E4C0E")
                       #s(autothemer--color example-color-014 "#0E4C22")
                       #s(autothemer--color example-color-013 "#147024")
                       #s(autothemer--color example-color-011 "#287C16"))
                      (cyan
                       #s(autothemer--color example-color-021 "#13416F")
                       #s(autothemer--color example-color-015 "#167C49")
                       #s(autothemer--color example-color-019 "#178297")
                       #s(autothemer--color example-color-018 "#1AA4A4")
                       #s(autothemer--color example-color-016 "#20BE87")
                       #s(autothemer--color example-color-020 "#2391CB")
                       #s(autothemer--color example-color-017 "#28E4C4"))
                      (blue
                       #s(autothemer--color example-color-026 "#170933")
                       #s(autothemer--color example-color-028 "#240933")
                       #s(autothemer--color example-color-024 "#0D0D4B")
                       #s(autothemer--color example-color-023 "#112063")
                       #s(autothemer--color example-color-022 "#13306F")
                       #s(autothemer--color example-color-025 "#281993")
                       #s(autothemer--color example-color-027 "#620FA9"))
                      (magenta
                       #s(autothemer--color example-color-030 "#330933")
                       #s(autothemer--color example-color-029 "#63136F")
                       #s(autothemer--color example-color-031 "#971782")
                       #s(autothemer--color example-color-033 "#A41A5F")
                       #s(autothemer--color example-color-032 "#D62499")
                       #s(autothemer--color example-color-034 "#D82662")))))
         (should (equal result expected))))

  (ert-deftest autothemer-groups-to-palette ()
    "Flatten a grouped palette (keeping order)."
    (let ((result
           (autothemer-groups-to-palette '((high
                                            #s(autothemer--color example-color-005 "#112063")
                                            #s(autothemer--color example-color-007 "#281993")
                                            #s(autothemer--color example-color-010 "#240933"))
                                           (mid
                                            #s(autothemer--color example-color-006 "#88DDCC"))
                                           (low
                                            #s(autothemer--color example-color-006 "#99DDCC")
                                            #s(autothemer--color example-color-006 "#FFDDCC")
                                            #s(autothemer--color example-color-006 "#FFEECC")))))

          (expected '( #s(autothemer--color example-color-005 "#112063")
                       #s(autothemer--color example-color-007 "#281993")
                       #s(autothemer--color example-color-010 "#240933")
                       #s(autothemer--color example-color-006 "#88DDCC")
                       #s(autothemer--color example-color-006 "#99DDCC")
                       #s(autothemer--color example-color-006 "#FFDDCC")
                       #s(autothemer--color example-color-006 "#FFEECC")))))))

(defun autothemer-groups-to-palette (grouped-palette)
  "Flatten a GROUPED-PALETTE from `autothemer-group-and-sort' to a single list."
  (-flatten (--map (cdr it) grouped-palette)))

(autothemer-color-hue #s(autothemer--color example-color-020 "#2391CB"))

;;; autothemer-tests.el ends here
