(TeX-add-style-hook
 "template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("$documentclass$" "$if(fontsize)$$fontsize$" "$endif$$if(lang)$$babel-lang$" "$endif$$if(handout)$handout" "$endif$$if(beamer)$ignorenonframetext" "$endif$$for(classoption)$$classoption$$sep$" "$endfor$")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("$fontfamily$" "$for(fontfamilyoptions)$$fontfamilyoptions$$sep$" "$endfor$") ("fontenc" "$if(fontenc)$$fontenc$$else$T1$endif$") ("inputenc" "utf8") ("babel" "shorthands=off" "$for(babel-otherlangs)$$babel-otherlangs$" "$endfor$main=$babel-lang$") ("biblatex" "$if(biblio-style)$style=$biblio-style$" "$endif$$for(biblatexoptions)$$biblatexoptions$$sep$" "$endfor$") ("ulem" "normalem") ("bidi" "RTLdocument")))
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimOut")
   (add-to-list 'LaTeX-verbatim-environments-local "SaveVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-environments-local "code")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb*")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "$documentclass$"
    "$documentclass$10"
    "$fontfamily$"
    "lmodern"
    "amssymb"
    "amsmath"
    "ifxetex"
    "ifluatex"
    "fixltx2e"
    "fontenc"
    "inputenc"
    "eurosym"
    "mathspec"
    "fontspec"
    "xeCJK"
    "graphicx"
    "color"
    "beamerthemebars"
    "multicol"
    "multirow"
    "hyperref"
    "tikz"
    "upquote"
    "microtype"
    "babel"
    "polyglossia"
    "natbib"
    "biblatex"
    "listings"
    "fancyvrb"
    "longtable"
    "booktabs"
    "caption"
    "grffile"
    "ulem"
    "bidi")
   (TeX-add-symbols
    "endcols"
    '("LR" 1)
    '("RL" 1)
    "euro"
    "tightlist"
    "maxwidth"
    "maxheight"
    "insertsectionnumber"
    "sectionname"
    "begincols")
   (LaTeX-add-environments
    "RTL"
    "LTR")
   (LaTeX-add-bibliographies
    "$for(bibliography)$$bibliography$$sep$"
    "$endfor$")
   (LaTeX-add-color-definecolors
    "beamer@UIUCblue"
    "beamer@UIUCorange"
    "beamer@UIUCgray"
    "beamer@UIUCgray2"))
 :latex)

