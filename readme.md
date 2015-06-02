# Readme #
Autothemer provides a thin layer on top of ```deftheme``` and ```custom-theme-set-faces``` that creates a new custom color theme, based on a set of simplified face specifications and a user-supplied color palette. As an example, the following snippet creates a new custom theme that modifies the two faces ```error``` and ```button```:
```elisp
(autothemer-deftheme some-new-theme "This is a rather pointless theme"
    ;; the first line of the color palette lists display types, ordered from
    ;; most to least capable
    ((((class color) (min-colors 32000)) ((class color) (min-colors 90)) t)
    ;; all following lines list color specifications, starting with the color's name,
    ;; then containing the color values corresponding to each display type. Nil values
    ;; are automatically replaced by their most recent non-nil precursor (i.e., the nil
    ;; below is replaced by "#dd6611")
    (reddish "#dd6611" nil "#FF0000")
    ;; missing values at the end are automatically replaced by the last non-nil entry,
    ;; in this case "#FFFF00"
    (yellowish "#dddd22" "#FFFF00")
    ;; the macro internally creates let* blocks, so these color definitions may contain
    ;; both arbitrary functions and references to already defined colors
    (not-quite-yellowish "#dcdc22" yellowish yellowish))

    ;; Here come the face specifications.
    ((button (:underline t :weight 'bold :foreground yellowish))
     (error (:foreground reddish)))

    ;; autothemer-deftheme can also execute an arbitrary function body. Here,
    ;; all colors refer to their best possible display representations
    ;; (i.e., reddish = "#dd6611", yellowish = "#dddd22", etc.)
    (custom-theme-set-variables 'some-new-theme
        `(ansi-color-names-vector [,reddish
                                   ,reddish
                                   ,reddish
                                   ,yellowish
                                   ,yellowish
                                   ,yellowish
                                   ,not-quite-yellowish
                                   ,not-quite-yellowish])))
```

At theme definition time, ```autothemer-deftheme``` furthermore extracts the best possible color representations (i.e., the first values in every row of the color palette, in this case ```"#dd6611"```, ```"#dddd22"``` and ```"#dcdc22"```) and writes the list to a global variable, together with the corresponding color names.

Missing face customizations can now be generated automatically by calling ```autothemer-generate-templates```. This command iterates over all currently defined faces that have not been customized during the last invocation of ```autothemer-deftheme```, obtain their current definition and replace every color value therein by the name of the color palette entry that best approximates it (where "best" is defined as having the lowest euclidean distance in RGB space). For instance, if the face ```some-face``` currently has the properties ```(:underline t :weight 'demibold :foreground "red")```, ```autothemer``` will find that the color ```"red"``` is most closely approximated by ```reddish``` and emit for ```some-face``` the reduced specification ```(some-face (:underline t :weight 'demibold :foreground reddish))```. All face specifications thus obtained are displayed in a temporary buffer and can be copied verbatim into the definition of ```some-new-theme```.

