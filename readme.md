# Autothemer

Autothemer provides a thin layer on top of `deftheme` and
`custom-theme-set-faces` that creates a new custom color theme.

## Usage

Autothemer requires a set of color classes, a color palette and
simplified face specifications to be applied to Emacs.

Take a look at the example below.

```elisp
(autothemer-deftheme example-name "Autothemer example..."

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
    ((button (:underline t :weight 'bold :foreground yellowish))
     (error  (:foreground reddish)))

    ;; Forms after the face specifications are evaluated.
    ;; (palette vars can be used, read below for details.)
    (custom-theme-set-variables 'example-name
        `(ansi-color-names-vector [,example-red
                                   ,example-green
                                   ,example-blue
                                   ,example-purple
                                   ,example-yellow
                                   ,example-orange
                                   ,example-cyan])))
```

## Faces and Color Classes

One of the things that makes writing themes for Emacs difficult is the clumsy syntax of `defface`, the macro used to configre Emacs `face` definitions.

Because the syntax isn't particularly developer friendly, it usually results in themes with limited support for different color displays, usually GUI / 24bit themes are made, and the results in the terminal are often sub par.  On occassion a theme does appear that provides better support for multiple display types, but due to the manual work involved in adding face specs, mode support is limited and development often stalls.

On the plus side the complexity of face specifcations means we can in theory design themes that support any display with any number of colors, we can support dark and light background modes.  It's a shame that it's been so hard to fully exploit the potential.

Autothemer solves most of the problems that a theme developer would face.

By defining a simple set of color class rules we can remove swathes of repetitive face specs.  Looking again at the example above.

```
(((class color) (min-colors #xFFFFFF))
 ((class color) (min-colors #xFF)))
```

Here we've setup a color class for 16.8million (0xFFFFFF) color display i.e. 24bit,  which will be read from first column in the palette.  We've then setup a color class for 256 (0xFF) color displays i.e. Xterm-256color, this will be read from the second column.

We can setup as many columns as we'd like to support, here's a few more examples.

For a two color display:

```
((class color) (monochrome)))
```

For a light background 24bit

```
((class color) (min-colors #xFFFFFF) (background light))
```

For a dark background 24bit

```
((class color) (min-colors #xFFFFFF) (background dark))
```

You can read more about defining faces in the Emacs manual, [display types and class color is covered here.](https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html)

### Palette

The palette definition is specified as a list of lists, each of the nested lists is a color name and then color values that correspond to each of the display/color classes defined above.

You can set color values as nil and the first color to the left will be used.

For example, if we have three display classes defined, 256, 24bit, 16 color:

```
((((class color) (min-colors #xFF))
  ((class color) (min-colors #xFFFFFF))
  ((class color) (min-colors 16)))

  ;; We define my-red in 256 color mode only.
  (my-red "#FF0000" nil nil))
```

Note we only specify 256 color mode's `my-red` value, and leave the
others as nil.  Autothemer will set the others with the value
`#FF0000`.

### Simplified face specs

In a regular theme (created with the `deftheme` macro) we have to
specify faces with the display attributes included for every face.
Autothemer's primary purpose is to reduce this down to a minimum.

As we can see in the example above face specs now look like this:

```
;; specifications for Emacs faces.
((button (:underline t :weight 'bold :foreground yellowish))
 (error  (:foreground reddish)))
```

color names from the palette can be used directly, as we can see here.
The faces are using colors named `yellowish` and `redish`.

One important thing to remember is that we are in a different context
to `deftheme` so symbols like `bold` or faces we want to `:inherit`
from must use the `'` quote-mark. (See the example above `'bold` would
usually not be quoted.) The following face attributes will be
affected.

- `:inherit`
- `:weight`
- `:slant`
- `:style`

(NOTE: there may be others I have missed!)

### Body / Evaluated Forms

After defining the display specs, palette and simplified face specs,
you can include other code to be evaluated.

Be aware that colors named in the palette will need to be `,`
comma-ed.  For example if you wanted to use the color `my-red` in a
form, you would refer to it as `,my-red`, so that it's evaluated
properly.

(This section of the README will be updated as I find any other
gotchas.)

### Auto generating missing specs

You can automatically generate specs for faces that are not in your
theme using the command

`M-x autothemer-generate-templates`

This will create a new buffer with simplified specs for all unthemed
faces.  Colors will be selected from the theme palette based on the
nearest RGB distance to the un-themed color.

We recommend thoroughly reviewing the auto generated themes so that
you produce a high quality theme.  Autothemer doesn't replace good
judgement and taste!

### Re-using the color palette

While autothemer doesn't export the defined color variables for external
use, you can define simple advice on `autothemer-deftheme` to do so:

```emacs-lisp
(define-advice autothemer-deftheme (:before (_ _ palette &rest _) defcolors)
  (mapcar (lambda (e)
            (setf (symbol-value (car e))
                  (cadr e)))
          (cdr palette)))
```
If you place the advice definition before the autothemer-generated theme
is loaded, e.g. `my-red` from the example above will be available as a 
variable that can be used in other parts of your emacs configuration.

### Themes using Autothemer

- [Darktooth](https://github.com/emacsfodder/emacs-theme-darktooth)
- [Creamsody](https://github.com/emacsfodder/emacs-theme-creamsody)
- [Gruvbox](https://github.com/greduan/emacs-theme-gruvbox)
...

### Contributing

We welcome all issues and pull requests, for review by the project author.

### Licence

See LICENCE
