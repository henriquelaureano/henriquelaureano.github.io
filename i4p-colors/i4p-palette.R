##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-jun-24 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## https://imagecolorpicker.com/ with i4p.png --------------------------

i4p_1c <- '#006cb8'
i4p_2c <- c('#0f73be', '#dde2aa')
i4p_3c <- c('#0f73be', '#dde2aa', '#749c64')
i4p_4c <- c('#dde2aa', '#0e72bd', '#77b4dc', '#749c64')
i4p_5c <- c('#0e72bd', '#d2e5f0', '#f9db0a', '#77b4dc', '#749c64')
i4p_6c <- c('#0d72bd', '#d2e5f0', '#f9db0a', '#77b4dc', '#669483',
            '#749c64')
i4p_7c <- c('#d2e5f0', '#086ebc', '#f9db0a', '#579dcf', '#77b4dc',
            '#669483', '#749c64')
i4p_8c <- c('#086ebc', '#d2e5f0', '#f9db0a', '#579dcf', '#77b4dc',
            '#669483', '#8aa274', '#749c64')
i4p_9c <- c('#d2e5f0', '#076ebc', '#f9db0a', '#579dcf', '#77b4dc',
            '#3b8dcc', '#669483', '#8aa274', '#749c64')
i4p_10c <- c('#d2e5f0', '#f9db0a', '#066dbc', '#579dcf', '#77b4dc',
             '#3b8dcc', '#348cc4', '#669483', '#8aa274', '#749c64')
i4p_11c <- c('#d2e5f0', '#f9db0a', '#056dbc', '#579dcf', '#77b4dc',
             '#3b8dcc', '#2781c2', '#348cc4', '#669483', '#8aa274',
             '#749c64')
i4p_bluepalette <- c('#006cb8', '#1a7bbf', '#3389c6', '#4d98cd',
                     '#66a7d4', '#80b6dc', '#99c4e3', '#b3d3ea',
                     '#cce2f1', '#cce2f1')

i4p_colors <- c(
    `blue`        ='#006cb8',
    `light blue`  ='#77b4dc',
    `lighter blue`='#d2e5f0',
    `yellow`      ='#f9db0a',
    `light green` ='#dde2aa',
    `middle green`='#669483',
    `dark green`  ='#749c64')

`main, blue, large`

i4p_palettes <- list(
    `main`  =i4p_cols("blue", "green", "yellow"),
    
    `cool`  =i4p_cols("blue", "green"),
    
    `hot`   =i4p_cols("yellow", "orange", "red"),
    
    `mixed` =i4p_cols("blue", "green", "yellow", "orange", "red"),
    
    `grey`  =i4p_cols("light grey", "dark grey")
)

drsimonj_pal <- function(palette="main", reverse=FALSE, ...)
{
    pal <- drsimonj_palettes[[palette]]
    
    if (reverse) pal <- rev(pal)
    
    colorRampPalette(pal, ...)
}

scale_color_i4p <-
    function(palette="main", discrete=TRUE, reverse=FALSE, ...)
{
    pal <- drsimonj_pal(palette = palette, reverse = reverse)
    
    if (discrete) {
        discrete_scale("colour", paste0("drsimonj_", palette), palette = pal, ...)
    } else {
        scale_color_gradientn(colours = pal(256), ...)
    }
}
