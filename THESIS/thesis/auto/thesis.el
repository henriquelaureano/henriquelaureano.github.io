(TeX-add-style-hook
 "thesis"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("abntex2" "12pt" "openright" "oneside" "a4paper" "chapter=TITLE" "section=TITLE" "brazil" "english")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("backref" "english" "hyperpageref") ("abntex2cite" "alf" "abnt-and-type=&" "abnt-etal-list=0") ("caption" "small" "justification=justified" "singlelinecheck=false") ("algpseudocode" "noend")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "./modules/intro"
    "./modules/methods"
    "./modules/model"
    "./modules/datasets"
    "./modules/results"
    "./modules/finalc"
    "abntex2"
    "abntex212"
    "lastpage"
    "etoolbox"
    "lmodern"
    "inputenc"
    "indentfirst"
    "color"
    "graphicx"
    "microtype"
    "setspace"
    "pdflscape"
    "wrapfig"
    "pgf"
    "backref"
    "caption"
    "tikz"
    "float"
    "tocloft"
    "abntex2cite"
    "pslatex"
    "mathpazo"
    "inconsolata"
    "verbatim"
    "bm"
    "Capa"
    "gensymb"
    "xparse"
    "algorithm"
    "listings"
    "multirow"
    "algpseudocode"
    "xpatch"
    "tikzit"
    "pifont"
    "amsmath"
    "amsfonts"
    "amssymb"
    "pdfpages")
   (TeX-add-symbols
    '("datatitle" 1)
    '("refanexo" 1)
    "listofprogramname"
    "floatc"
    "fs"
    "programautorefname"
    "cmark"
    "xmark"
    "cc"
    "numberline")
   (LaTeX-add-labels
    "cap:intro"
    "cap:methods"
    "cap:model"
    "cap:datasets"
    "cap:results"
    "cap:finalc"
    "cap:appendixA")
   (LaTeX-add-bibliographies
    "references"))
 :latex)

