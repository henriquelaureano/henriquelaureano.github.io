(TeX-add-style-hook
 "thesis"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("abntex2" "12pt" "openright" "oneside" "a4paper" "chapter=TITLE" "section=TITLE" "brazil" "english")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("backref" "english" "hyperpageref") ("abntex2cite" "alf") ("caption" "small" "justification=justified" "singlelinecheck=false")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "./Modulos/Introducao"
    "./Modulos/Aplicacoes"
    "./Modulos/Metodologia"
    "./Modulos/Modelo"
    "./Modulos/Resultados"
    "./Modulos/ConsFinais"
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
    "Capa"
    "gensymb"
    "xparse"
    "algorithm"
    "algorithmic"
    "listings"
    "multirow"
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
    "cc")
   (LaTeX-add-labels
    "cap:introducao"
    "cap:aplicacoes"
    "cap:fundamentacaoteorica"
    "cap:multivariatemodel"
    "cap:resultados"
    "cap:considefinais")
   (LaTeX-add-bibliographies
    "referencias"))
 :latex)

