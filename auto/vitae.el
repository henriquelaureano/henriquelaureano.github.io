(TeX-add-style-hook
 "vitae"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "top=1.5cm" "bottom=1.5cm" "left=2cm" "right=2cm") ("fontenc" "T1")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "geometry"
    "listings"
    "color"
    "amsmath"
    "amssymb"
    "bm"
    "enumerate"
    "graphicx"
    "float"
    "hyperref"
    "multicol"
    "microtype"
    "gfsdidot"
    "fontenc")
   (TeX-add-symbols
    '("horrule" 1))
   (LaTeX-add-color-definecolors
    "mygreen"
    "mylilas"))
 :latex)

