# Autothemer
[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/autothemer.svg)](https://elpa.nongnu.org/nongnu/autothemer.html)

Autothemer provides a thin layer on top of `deftheme` and
`custom-theme-set-faces` that creates a new custom color theme.

## Usage

Autothemer requires a set of color classes, a color palette and
simplified face specifications to be applied to Emacs.

Take a look at the example below.

```lisp
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
    ((button (:underline t :weight 'bold :foreground example-yellow))
     (error  (:foreground example-red)))

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

One of the things that makes writing themes for Emacs difficult is the syntax of `defface`, the macro used to configre Emacs `face` definitions.

Because the syntax isn't particularly developer friendly, it usually results in themes with limited support for different color displays, usually GUI / 24bit themes are made, and the results in the terminal are often sub par.  On occassion a theme does appear that provides better support for multiple display types, but due to the manual work involved in adding face specs, mode support is limited and development often stalls.

On the plus side the complexity of face specifcations means we can in theory design themes that support any display with any number of colors, we can support dark and light background modes.  Until now it's been hard to fully exploit the potential.

Autothemer solves most of the problems that a theme developer would face.

By defining a simple set of color class rules we can remove swathes of repetitive face specs.  Looking again at the example above.

```lisp
(((class color) (min-colors #xFFFFFF))
 ((class color) (min-colors #xFF)))
```

Here we've setup a color class for 16.8million (0xFFFFFF) color display i.e. 24bit,  which will be read from first column in the palette.  We've then setup a color class for 256 (0xFF) color displays i.e. Xterm-256color, this will be read from the second column.

We can setup as many columns as we'd like to support, here's a few more examples.

For a two color display:

```lisp
((class color) (monochrome)))
```

For a light background 24bit

```lisp
((class color) (min-colors #xFFFFFF) (background light))
```

For a dark background 24bit

```lisp
((class color) (min-colors #xFFFFFF) (background dark))
```

You can read more about defining faces in the Emacs manual, [display types and class color is covered here.](https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html)

### Palette

The palette definition is specified as a list of lists, each of the nested lists is a color name and then color values that correspond to each of the display/color classes defined above.

You can set color values as nil and the first color to the left will be used.

For example, if we have three display classes defined, 256, 24bit, 16 color:

```lisp
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

```lisp
;; specifications for Emacs faces.
((button (:underline t :weight 'bold :foreground example-yellow))
 (error  (:foreground example-red)))
```

color names from the palette can be used directly, as we can see here.
The faces are using colors named `example-yellow` and `example-red`.

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

```lisp
(define-advice autothemer-deftheme (:before (_ _ palette &rest _) defcolors)
  (mapcar (lambda (e)
            (setf (symbol-value (car e))
                  (cadr e)))
          (cdr palette)))
```
If you place the advice definition before the autothemer-generated theme
is loaded, e.g. `my-red` from the example above will be available as a 
variable that can be used in other parts of your emacs configuration.

# TVA

Theme Variance Architecture is the pattern used in the Gruvbox theme for creating theme variants.

For tooling compatibility you should use this architecture when creating theme variants with [Autothemer].

### TVA Specification

TVA requires package, themes and variants to be named using a specific convention.

### Convert an autothemer based theme to the TVA style.

For example, let's say we've created a standalone theme called:

```
foo
```

Given the `package.el` and Emacs convention we will have named the Emacs lisp file:

```
foo-theme.el
```

To prepare to add variants, we'll create a macro for the `foo` theme family. It'll look after setting all the face specs in emacs, and allow us to define our variant palette elsewhere.  **As long as the same palette variable names are used, we should be ok.**

[Let's take a look at how this 
was done in Gruvbox](https://github.com/greduan/emacs-theme-gruvbox/blob/3929f29674ac1cb59efaa017e3c534e0d8d72a2d/gruvbox.el#L88)

For our `foo-theme` we'd do the following.

- Create a file called `foo.el` in the package folder.
- Create `deftheme-foo` in `foo.el`.
  ```
  (require 'autothemer)
  
  (defmacro deftheme-foo (name description palette &rest body) 
    ,(autothemer ,name
                 ,description
                 ,palette
                 
                 ((face (specs...)))
                 
                 ,@body)
  ```
- Move the face specs from `foo-theme.el` into the `deftheme-foo` macro definition replacing:
  ```
  ((face (specs...)))
  ```  
  For Gruvbox's development, [this commit captures the face specs move.](https://github.com/greduan/emacs-theme-gruvbox/commit/250df251d0972aecd259144ad1ad3daf33c97cb2). Although we'd already created variants at that point, and had a lot of code duplication. (TVA is the way we've DRYed up this duplication, giving `gruvbox.el` the status of single point where we add new mode support.)
- Modify `foo-theme.el`
  - use `deftheme-foo` instead of `autothemer-deftheme`
  - Replace `(require 'autothemer)` with `(require 'foo)`

### Creating variants.

Once we've completed the conversion to TVA style above. We can create a variant by copying `foo-theme.el` to a new name.

For a light variant of our theme, we'll copy `foo-theme.el` to  `foo-light-theme.el`.

We can now modify the palette (use the same palette names and just modify the color values). We must also update the theme name.

Check the differences in [`gruvbox-theme.el`](https://github.com/greduan/emacs-theme-gruvbox/blob/e9f8e6ee52727f6008c125b71a26c80cfa59c0af/gruvbox-theme.el) and [`gruvbox-dark-hard-theme.el`](https://github.com/greduan/emacs-theme-gruvbox/blob/e9f8e6ee52727f6008c125b71a26c80cfa59c0af/gruvbox-dark-hard-theme.el)

`gruvbox-theme.el`

```lisp
  1 ;;; gruvbox-theme.el --- A retro-groove colour theme for Emacs -*- lexical-binding: t -*-
    
 53 (gruvbox-deftheme)
 54  gruvbox
 55  "A retro-groove colour theme" 
    
146  (custom-theme-set-variables 'gruvbox)
    
165 (provide-theme 'gruvbox)
     
172 ;;; gruvbox-theme.el ends here
```

`gruvbox-dark-hard-theme.el`

```lisp
  1 ;;; gruvbox-dark-hard-theme.el --- A retro-groove colour theme for Emacs -*- lexical-binding: t -*-
     
 55 (gruvbox-deftheme)
 56  gruvbox-dark-hard
 57  "A retro-groove colour theme (dark version, hard contrast)"
     
148  (custom-theme-set-variables 'gruvbox-dark-hard)
     
167 (provide-theme 'gruvbox-dark-hard)
      
173 ;;; gruvbox-dark-hard-theme.el ends here
```

Once this is done you test your theme.

(I use `disable-theme` and `enable-theme` to test/use themes under development.  
Make sure you eval all the theme's elisp files before enabling the theme.)



### Themes using Autothemer

- [Gruvbox](https://github.com/greduan/emacs-theme-gruvbox)
- [Darktooth](https://github.com/emacsfodder/emacs-theme-darktooth)
- [Creamsody](https://github.com/emacsfodder/emacs-theme-creamsody)
- [Sakura](https://github.com/emacsfodder/emacs-theme-sakura)
- [Cyanometric](https://github.com/emacsfodder/emacs-theme-cyanometric)
- [Orangey Bits](https://github.com/emacsfodder/emacs-theme-orangey-bits)
- [Vegetative](https://github.com/emacsfodder/emacs-theme-vegetative)

If you are creating themes with Autothemer, please let us know (you can email the maintainer.)

### Contributing

We welcome all issues and pull requests, for review by the project author.

### Licence

See [LICENCE](LICENCE)

[Autothemer]: https://github.com/jasonm23/autothemer
