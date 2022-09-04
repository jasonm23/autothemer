# Autothemer

Reduce the amount of pain and boilerplate code needed to create custom themes using `autothemer-deftheme`.

Autothemer also includes interactive commands and functions to
assist with theme building, here are a few highlights...

- Generate specs for unthemed faces using the theme color palette.
  - `autothemer-generate-templates`
  - `autothemer-generate-templates-filtered` (filter by regexp)
- Generate a palette SVG image
  - `autothemer-generate-palette-svg`
- Insert a color name or color from the active palette
  - `autothemer-insert-color-name`
  - `autothemer-insert-color`
- Colorize/font-lock palette color names in the buffer
  - `autothemer-colorize`  (requires `rainbow-mode` during development.)

 - - -
# Function reference

### Commands
- [autothemer-colorize](#autothemer-colorize-)
- [autothemer-generate-palette-svg](#autothemer-generate-palette-svg---options)
- [autothemer-generate-templates](#autothemer-generate-templates---regexp)
- [autothemer-generate-templates-filtered](#autothemer-generate-templates-filtered--regexp)
- [autothemer-insert-color](#autothemer-insert-color-)
- [autothemer-insert-color-name](#autothemer-insert-color-name-)
### User Functions
- [autothemer-brightness-group](#autothemer-brightness-group--color--brightness-groups) 
- [autothemer-color-brightness](#autothemer-color-brightness--color) 
- [autothemer-color-hue](#autothemer-color-hue--color) 
- [autothemer-color-sat](#autothemer-color-sat--color) 
- [autothemer-color-to-group](#autothemer-color-to-group--color-fn-groups) 
- [autothemer-darkest-order](#autothemer-darkest-order--a-b) 
- [autothemer-desaturated-order](#autothemer-desaturated-order--a-b) 
- [autothemer-group-and-sort](#autothemer-group-and-sort--palette-options) 
- [autothemer-group-colors](#autothemer-group-colors--palette-options) 
- [autothemer-group-sort](#autothemer-group-sort--groups-sort-fn) 
- [autothemer-groups-to-palette](#autothemer-groups-to-palette--grouped-palette) 
- [autothemer-hex-to-rgb](#autothemer-hex-to-rgb--hex) 
- [autothemer-hue-group](#autothemer-hue-group--color--hue-groups) 
- [autothemer-hue-order](#autothemer-hue-order--a-b) 
- [autothemer-lightest-order](#autothemer-lightest-order--a-b) 
- [autothemer-saturated-order](#autothemer-saturated-order--a-b) 
- [autothemer-saturation-group](#autothemer-saturation-group--color--saturation-groups) 
- [autothemer-sort-palette](#autothemer-sort-palette--theme-colors--sort-fn-group-fn-group-args) 
### Internal Functions
- [autothemer--alist-to-reduced-spec](#autothemer--alist-to-reduced-spec--facename-alist)
- [autothemer--approximate-spec](#autothemer--approximate-spec--reduced-spec-theme)
- [autothemer--color-distance](#autothemer--color-distance--color-autothemer-color)
- [autothemer--color-to-hsv](#autothemer--color-to-hsv--rgb)
- [autothemer--colorize-alist](#autothemer--colorize-alist-)
- [autothemer--cons-to-tree](#autothemer--cons-to-tree--the-cons)
- [autothemer--current-theme-guard](#autothemer--current-theme-guard-)
- [autothemer--demote-heads](#autothemer--demote-heads--expr)
- [autothemer--extract-display](#autothemer--extract-display--palette-n)
- [autothemer--extract-let-block](#autothemer--extract-let-block--palette-n)
- [autothemer--face-to-alist](#autothemer--face-to-alist--face)
- [autothemer--fill-empty-palette-slots](#autothemer--fill-empty-palette-slots--palette)
- [autothemer--find-closest-color](#autothemer--find-closest-color--colors-color)
- [autothemer--get-color](#autothemer--get-color--color-name)
- [autothemer--pad-with-nil](#autothemer--pad-with-nil--row-min-number-of-elements)
- [autothemer--reduced-spec-to-facespec](#autothemer--reduced-spec-to-facespec--display-reduced-specs)
- [autothemer--replace-nil-by-precursor](#autothemer--replace-nil-by-precursor--palette-row)
- [autothemer--select-color](#autothemer--select-color---prompt)
- [autothemer--unindent](#autothemer--unindent--s)
- [autothemer--unthemed-faces](#autothemer--unthemed-faces-)
### <a id="autothemer-colorize-" aria-hidden="true"></a>autothemer-colorize command

In the current buffer, colorize palette color names, from the last evaluated theme, by their color value.

<sup>function signature</sup>
```lisp
(autothemer-colorize)
```

- - -

### <a id="autothemer-generate-palette-svg---options" aria-hidden="true"></a>autothemer-generate-palette-svg command

Create an SVG palette image for a theme.

Optional parameter `options` (a plist). Any required values not
supplied in `options` will use defaults or prompt interactively.

| Option                 | Description                                         |
|------------------------|-----------------------------------------------------|
| `:theme-file`          | theme filename                                      |
| `:theme-name`          | override the title found in :theme-file             |
| `:theme-description`   | override the description found in :theme-file       |
| `:theme-url`           | override the url found in :theme-file               |
| `:font-family`         | font name to use in the generated SVG               |
| `:columns`             | number of columns for each palette row (default: 6) |
| `:bg-color`            | Page background color                               |
| `:text-color`          | Main text color                                     |
| `:text-accent-color`   | Text accent color                                   |
| `:page-template`       | see page-template below                             |
| `:page-top-margin`     | (default: 120)                                      |
| `:page-right-margin`   | (default: 30)                                       |
| `:page-bottom-margin`  | (default: 60)                                       |
| `:page-left-margin`    | (default: 30)                                       |
| `:swatch-template`     | see swatch-template below                           |
| `:swatch-border-color` | the border color of a color swatch                  |
| `:swatch-width`        | px spacing width of a color swatch (default: 100)   |
| `:swatch-height`       | px spacing height of a color swatch (default: 150)  |
| `:swatch-rotate`       | degrees of rotation for swatch (default: 45)        |
| `:h-space`             | horizontal-space between swatches (default: 10)     |
| `:v-space`             | vertical-space between swatches (default: 10)       |
| `:sort-palette`        | arrange palette using a function name               |
| `:group-swatches`      | boolean                                             |
| `:svg-out-file`        | the file/pathname to save SVG output                |

For advanced customization the :page-template and :swatch-template can be
used to provide customize the SVG templates.

Note: Template parameters are filled by `format` so we mark them as follows:

Page Template parameters:

| Parameter | Description       |
|-----------|-------------------|
| `%1$s`    | width             |
| `%2$s`    | height            |
| `%3$s`    | font-family       |
| `%4$s`    | text-color        |
| `%5$s`    | text-accent-color |
| `%6$s`    | bg-color          |
| `%7$s`    | theme-name        |
| `%8$s`    | theme-description |
| `%9$s`    | theme-url         |
| `%10$s`   | color swatches    |

Swatch Template parameters:

| Parameter | Description         |
|-----------|---------------------|
| `%1$s`    | x                   |
| `%2$s`    | y                   |
| `%3$s`    | swatch-border-color |
| `%4$s`    | swatch-color        |
| `%5$s`    | text-accent-color   |
| `%6$s`    | swatch-color-name   |

<sup>function signature</sup>
```lisp
(autothemer-generate-palette-svg (&optional options))
```

- - -

### <a id="autothemer-generate-templates---regexp" aria-hidden="true"></a>autothemer-generate-templates command

Autogenerate customizations for unthemed faces (optionally by `regexp`).

Generate customizations that approximate current face definitions using the
nearest colors in the color palette of `autothemer-current-theme`.

An error is shown when no current theme is available.

<sup>function signature</sup>
```lisp
(autothemer-generate-templates (&optional regexp))
```

- - -

### <a id="autothemer-generate-templates-filtered--regexp" aria-hidden="true"></a>autothemer-generate-templates-filtered command

Autogenerate customizations for unthemed faces matching `regexp`.

Calls `autothemer-generate-templates` after user provides `regexp` interactively.

<sup>function signature</sup>
```lisp
(autothemer-generate-templates-filtered (regexp))
```

- - -

### <a id="autothemer-insert-color-" aria-hidden="true"></a>autothemer-insert-color command

Select and insert a color from the current autotheme palette.

<sup>function signature</sup>
```lisp
(autothemer-insert-color)
```

- - -

### <a id="autothemer-insert-color-name-" aria-hidden="true"></a>autothemer-insert-color-name command

Select and insert a color name from the current autotheme palette.

<sup>function signature</sup>
```lisp
(autothemer-insert-color-name)
```

- - -

### <a id="autothemer-brightness-group--color--brightness-groups" aria-hidden="true"></a>autothemer-brightness-group 

Return the brightness group of `color`.
Functionally identical to `autothemer-hue-groups` for brightness.
Optionally provide a list of `brightness-groups`.
The default is `autothemer-20-percent-brightness-groups`.

<sup>function signature</sup>
```lisp
(autothemer-brightness-group (color &optional brightness-groups))
```

- - -

### <a id="autothemer-color-brightness--color" aria-hidden="true"></a>autothemer-color-brightness 

Return the HSV brightness of `color` (hex color or autothemer--color struct).

<sup>function signature</sup>
```lisp
(autothemer-color-brightness (color))
```

- - -

### <a id="autothemer-color-hue--color" aria-hidden="true"></a>autothemer-color-hue 

Return the HSV hue of `color` (hex color or autothemer--color struct).

<sup>function signature</sup>
```lisp
(autothemer-color-hue (color))
```

- - -

### <a id="autothemer-color-sat--color" aria-hidden="true"></a>autothemer-color-sat 

Return the HSV saturation of `color` (hex color or autothemer--color struct).

<sup>function signature</sup>
```lisp
(autothemer-color-sat (color))
```

- - -

### <a id="autothemer-color-to-group--color-fn-groups" aria-hidden="true"></a>autothemer-color-to-group 

Group `color` using `fn`, in `groups`.

<sup>function signature</sup>
```lisp
(autothemer-color-to-group (color fn groups))
```

- - -

### <a id="autothemer-darkest-order--a-b" aria-hidden="true"></a>autothemer-darkest-order 

Return t if the darkness of `a` > `b`.

<sup>function signature</sup>
```lisp
(autothemer-darkest-order (a b))
```

- - -

### <a id="autothemer-desaturated-order--a-b" aria-hidden="true"></a>autothemer-desaturated-order 

Return t if the saturation of `a` < `b`.

<sup>function signature</sup>
```lisp
(autothemer-desaturated-order (a b))
```

- - -

### <a id="autothemer-group-and-sort--palette-options" aria-hidden="true"></a>autothemer-group-and-sort 

Group and sort `palette` using `options`.

Options is a plist of:

| Option        | Description                                      |
|---------------|--------------------------------------------------|
| `:group-fn`   | mandatory group function                         |
| `:group-args` | optional group args (to use a non-default group) |
| `:sort-fn`    | optional sort function                           |

See color grouping functions and group lists:

Hue grouping:

| Function               | Description               |
|------------------------|---------------------------|
| `autothemer-hue-group` | color hue group for COLOR |

| Hue Groups                     | Description                                  |
|--------------------------------|----------------------------------------------|
| `autothemer-hue-groups`        | group colors into major hue groups (default) |
| `autothemer-simple-hue-groups` | group colors into broad hue groups           |

Brightness grouping:

| Function                      | Description                |
|-------------------------------|----------------------------|
| `autothemer-brightness-group` | brightness group for COLOR |

| Brightness Groups                             | Description                   |
|-----------------------------------------------|-------------------------------|
| `autothemer-dark-mid-light-brightness-groups` | 3 brightness groups           |
| `autothemer-10-percent-brightness-groups`     | 10 brightness groups          |
| `autothemer-20-percent-brightness-groups`     | 5 brightness groups (default) |

Saturation grouping:

| Function                      | Description                |
|-------------------------------|----------------------------|
| `autothemer-saturation-group` | saturation group for COLOR |

| Saturation Groups                           | Description                   |
|---------------------------------------------|-------------------------------|
| `autothemer-low-mid-high-saturation-groups` | 3 saturation groups           |
| `autothemer-10-percent-saturation-groups`   | 10 saturation groups          |
| `autothemer-20-percent-saturation-groups`   | 5 saturation groups (default) |
- - -

Sorting:

The sort/ordering functions take args A and B, which are expected
to be `autothemer--color` structs.

| Sort Functions                 | Description                     |
|--------------------------------|---------------------------------|
| `autothemer-darkest-order`     | darkest to lightest             |
| `autothemer-lightest-order`    | lightest to darkest             |
| `autothemer-hue-order`         | sort by hue                     |
| `autothemer-saturated-order`   | sort by most saturated to least |
| `autothemer-desaturated-order` | sort by least saturated to most |

<sup>function signature</sup>
```lisp
(autothemer-group-and-sort (palette options))
```

- - -

### <a id="autothemer-group-colors--palette-options" aria-hidden="true"></a>autothemer-group-colors 

Group `palette` colors into groups as defined in plist `options`:
`:group-fn` - mandatory group function
`:group-args` - args for `group-fn`

<sup>function signature</sup>
```lisp
(autothemer-group-colors (palette options))
```

- - -

### <a id="autothemer-group-sort--groups-sort-fn" aria-hidden="true"></a>autothemer-group-sort 

Sort `groups` of colors using `sort-fn`.
`groups` are produced by `autothemer-group-colors`.

<sup>function signature</sup>
```lisp
(autothemer-group-sort (groups sort-fn))
```

- - -

### <a id="autothemer-groups-to-palette--grouped-palette" aria-hidden="true"></a>autothemer-groups-to-palette 

Flatten a `grouped-palette` from `autothemer-group-and-sort` to a single list.

<sup>function signature</sup>
```lisp
(autothemer-groups-to-palette (grouped-palette))
```

- - -

### <a id="autothemer-hex-to-rgb--hex" aria-hidden="true"></a>autothemer-hex-to-rgb 

Convert `hex` to `(r g b)`.
`r`, `g`, `b` will be values `0..65535`

<sup>function signature</sup>
```lisp
(autothemer-hex-to-rgb (hex))
```

- - -

### <a id="autothemer-hue-group--color--hue-groups" aria-hidden="true"></a>autothemer-hue-group 

Return the color hue group for `color`.

Optionally provide a list of `hue-groups`.
(default uses `autothemer-hue-groups`.)
Also available is `autothemer-simple-hue-groups`,
both are customizable, or define your own.

This facilitates hue grouping & sorting by a secondary axis.
For example sort a list of colors by some axis (brightness or
saturation). Then group by hue groups, and sort the groups.
The format of each group in the list is:

    (group-name (n1 . n2))

Where `group-name` is a symbol to name the group,
`(n1 . n2)` is a hue range specifier (in degrees)
low `n1` to high `n2`.

A hue range which crosses the apex (i.e. `360°..0°`) is permitted.

<sup>function signature</sup>
```lisp
(autothemer-hue-group (color &optional hue-groups))
```

- - -

### <a id="autothemer-hue-order--a-b" aria-hidden="true"></a>autothemer-hue-order 

Return t if the hue of `a` > `b`.

<sup>function signature</sup>
```lisp
(autothemer-hue-order (a b))
```

- - -

### <a id="autothemer-lightest-order--a-b" aria-hidden="true"></a>autothemer-lightest-order 

Return t if the lightness of `a` > `b`.

<sup>function signature</sup>
```lisp
(autothemer-lightest-order (a b))
```

- - -

### <a id="autothemer-saturated-order--a-b" aria-hidden="true"></a>autothemer-saturated-order 

Return t if the saturation of `a` > `b`.

<sup>function signature</sup>
```lisp
(autothemer-saturated-order (a b))
```

- - -

### <a id="autothemer-saturation-group--color--saturation-groups" aria-hidden="true"></a>autothemer-saturation-group 

Return the saturation group of `color`.
Functionally identical to `autothemer-hue-groups` for saturation.
Optionally provide a list of `saturation-groups`.
The default is `autothemer-20-percent-saturation-groups`.

<sup>function signature</sup>
```lisp
(autothemer-saturation-group (color &optional saturation-groups))
```

- - -

### <a id="autothemer-sort-palette--theme-colors--sort-fn-group-fn-group-args" aria-hidden="true"></a>autothemer-sort-palette 

Produce a list of sorted `theme-colors` using `sort-fn`.
If `sort-fn` is nil, sort by default `autothemer-darkest-order`.
Grouping is supported via `group-fn` & `group-args`.

See `autothemer-group-and-sort` for a full list.

<sup>function signature</sup>
```lisp
(autothemer-sort-palette (theme-colors &optional sort-fn group-fn group-args))
```

- - -

### <a id="autothemer--alist-to-reduced-spec--facename-alist" aria-hidden="true"></a>autothemer--alist-to-reduced-spec internal

Generate a reduced-spec for `facename`, based on the face attribute `alist`.

<sup>function signature</sup>
```lisp
(autothemer--alist-to-reduced-spec (facename alist))
```

- - -

### <a id="autothemer--approximate-spec--reduced-spec-theme" aria-hidden="true"></a>autothemer--approximate-spec internal

Replace colors in `reduced-spec` by their closest approximations in `theme`.
Replace every expression in `reduced-spec` that passes
`color-defined-p` by the closest approximation found in
`autothemer-current-theme`.  Also quote all face names and
unbound symbols, such as `normal` or `demibold`.

<sup>function signature</sup>
```lisp
(autothemer--approximate-spec (reduced-spec theme))
```

- - -

### <a id="autothemer--color-distance--color-autothemer-color" aria-hidden="true"></a>autothemer--color-distance internal

Return the distance in rgb space between `color` and AUTOTHEMER-`color`.
Here, `color` is an Emacs color specification and AUTOTHEMER-`color` is of
type `autothemer--color`.

<sup>function signature</sup>
```lisp
(autothemer--color-distance (color autothemer-color))
```

- - -

### <a id="autothemer--color-to-hsv--rgb" aria-hidden="true"></a>autothemer--color-to-hsv internal

Convert `rgb`, a list of `(r g b)` to list `(h s v)`.
The `r` `g` `b` values can range between `0..65535`.

In `(h s v)` `h`, `s` and `v` are `0.0..1.0`.

<sup>function signature</sup>
```lisp
(autothemer--color-to-hsv (rgb))
```

- - -

### <a id="autothemer--colorize-alist-" aria-hidden="true"></a>autothemer--colorize-alist internal

Generate an alist for use with rainbow-mode.

To colorize use:

    (rainbow-colorize-by-assoc (autothemer--colorize-alist))

Colors are from `autothemer-current-theme`.

<sup>function signature</sup>
```lisp
(autothemer--colorize-alist)
```

- - -

### <a id="autothemer--cons-to-tree--the-cons" aria-hidden="true"></a>autothemer--cons-to-tree internal

Turn `the-cons` into a list, unless its cdr is `unspecified`.

<sup>function signature</sup>
```lisp
(autothemer--cons-to-tree (the-cons))
```

- - -

### <a id="autothemer--current-theme-guard-" aria-hidden="true"></a>autothemer--current-theme-guard internal

Guard functions from executing when there's no current theme.

<sup>function signature</sup>
```lisp
(autothemer--current-theme-guard)
```

- - -

### <a id="autothemer--demote-heads--expr" aria-hidden="true"></a>autothemer--demote-heads internal

Demote every list head within `expr` by one element.
E.g., (a (b c d) e (f g)) -> (list a (list b c d) e (list f g)).

<sup>function signature</sup>
```lisp
(autothemer--demote-heads (expr))
```

- - -

### <a id="autothemer--extract-display--palette-n" aria-hidden="true"></a>autothemer--extract-display internal

Extract from `palette` display specification #`n`.

<sup>function signature</sup>
```lisp
(autothemer--extract-display (palette n))
```

- - -

### <a id="autothemer--extract-let-block--palette-n" aria-hidden="true"></a>autothemer--extract-let-block internal

Extract a variable definition block from `palette` for display type `n`.

<sup>function signature</sup>
```lisp
(autothemer--extract-let-block (palette n))
```

- - -

### <a id="autothemer--face-to-alist--face" aria-hidden="true"></a>autothemer--face-to-alist internal

Return the attribute alist for `face` in frame (selected-frame).

<sup>function signature</sup>
```lisp
(autothemer--face-to-alist (face))
```

- - -

### <a id="autothemer--fill-empty-palette-slots--palette" aria-hidden="true"></a>autothemer--fill-empty-palette-slots internal

Fill empty `palette` slots so each display has all color-definitions.

<sup>function signature</sup>
```lisp
(autothemer--fill-empty-palette-slots (palette))
```

- - -

### <a id="autothemer--find-closest-color--colors-color" aria-hidden="true"></a>autothemer--find-closest-color internal

Return the element of `colors` that is closest in rgb space to `color`.
Here, `color` is an Emacs color specification and `colors` is a list
of `autothemer--color` structs.

<sup>function signature</sup>
```lisp
(autothemer--find-closest-color (colors color))
```

- - -

### <a id="autothemer--get-color--color-name" aria-hidden="true"></a>autothemer--get-color internal

Return color palette object for (string) `color-name`.

Search the `autothemer-current-theme` color palette for `color-name`
and returns a color in the form of `autothemer--color` struct.

See also `autothemer--color-p`,
         `autothemer--color-name`,
         `autothemer--color-value`.

<sup>function signature</sup>
```lisp
(autothemer--get-color (color-name))
```

- - -

### <a id="autothemer--pad-with-nil--row-min-number-of-elements" aria-hidden="true"></a>autothemer--pad-with-nil internal

Make sure that `row` has at least `min-number-of-elements`.
Pad with nil if necessary.

<sup>function signature</sup>
```lisp
(autothemer--pad-with-nil (row min-number-of-elements))
```

- - -

### <a id="autothemer--reduced-spec-to-facespec--display-reduced-specs" aria-hidden="true"></a>autothemer--reduced-spec-to-facespec internal

Create a face spec for `display`, with specs `reduced-specs`.

For example:

     (autothemer--reduced-spec-to-facespec '(min-colors 60) '(button (:underline t :foreground red)))
     ;; => `(button (((min-colors 60) (:underline ,t :foreground ,red)))).

<sup>function signature</sup>
```lisp
(autothemer--reduced-spec-to-facespec (display reduced-specs))
```

- - -

### <a id="autothemer--replace-nil-by-precursor--palette-row" aria-hidden="true"></a>autothemer--replace-nil-by-precursor internal

Replace nil colors in `palette-row` with their precursor.

`palette-row` is of the form `(name color [color ...])`

Where  the first `color` must be non nil.

Any subsequent nil color will be replaced by the previous value.

For example:

     ("red-foo" "#FF0000" nil)

Will become:

     ("red-foo" "#FF0000" "#FF0000")

<sup>function signature</sup>
```lisp
(autothemer--replace-nil-by-precursor (palette-row))
```

- - -

### <a id="autothemer--select-color---prompt" aria-hidden="true"></a>autothemer--select-color internal

Select a color from the current palette, optionally use `prompt`.
Current palette is read from `autothemer-current-theme`.

The selected color will be in the form of a `autothemer--color`

See also `autothemer--color-p`,
         `autothemer--color-name`,
         `autothemer--color-value`.

<sup>function signature</sup>
```lisp
(autothemer--select-color (&optional prompt))
```

- - -

### <a id="autothemer--unindent--s" aria-hidden="true"></a>autothemer--unindent internal

Unindent string `s` marked with | chars.

<sup>function signature</sup>
```lisp
(autothemer--unindent (s))
```

- - -

### <a id="autothemer--unthemed-faces-" aria-hidden="true"></a>autothemer--unthemed-faces internal

Find uncustomized faces.
Iterate through all currently defined faces and return those that
were left uncustomized by the most recent call to
`autothemer-deftheme`.

<sup>function signature</sup>
```lisp
(autothemer--unthemed-faces)
```

- - -
