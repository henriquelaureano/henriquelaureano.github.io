##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-out-13 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

if(!requireNamespace('pacman', quietly=TRUE)) install.packages('pacman')
pacman::p_load(tibble, tidyr, colormap, gridExtra, patchwork, fmsb,
               gridGraphics)

## The first two lines indicate the 'spider' max and min, respectively
data <- tibble::tibble('Pesticides reduction'=c(
                           0, -0.1, -0.0453, -0.0313, -0.0096, -0.0753
                       ),
                       'Contract\nlength'=c(
                           0.1, -0.25, 0.0025, -0.1611, -0.2129, 0.0557
                       ),
                       'With penalization'=c(
                           2, 1, 1.2608, 1.0528, 1.0112, 1.7771
                       ),
                       'Unbreakable\ncontract'=c(
                           -0.25, -1, -0.6177, -0.3295, -0.5541, -0.9064
                       ))
datatable <-
    tibble::tibble(Profile=paste('Profile', 1:4),
                   Age=c(
                       'Average', 'Average', 'Mode', 'Min'
                   ),
                   Income=c(
                       'Average', 'Mode', 'Max', 'Average'
                   ),
                   'Education\nlevel'=c(
                       'Average', 'Max', 'Model', 'Average'
                   ),
                   'Annual\npesticides\napplication'=c(
                       'Average', 'Average', 'Min', 'Max'
                   ),
                   'Land use'=c(
                       'Agriculture', 'Forest', 'Cattle', 'Agriculture'
                   ),
                   Region=c(
                       'Piriripau', 'Taquara', 'Taquara', 'Piriripau'
                   ),
                   'Decision\nmaker'=c(
                       'C', 'A', 'B', 'C'
                   ),
                   'Property\nsurface'=c(
                       'Big', 'Small', 'Big', 'Small'
                   ))|>
    tidyr::pivot_longer(!Profile,
                        names_to='Features', values_to='Status')|>
    tidyr::spread(Profile, Status)
datatable <-
    tibble::column_to_rownames(datatable, 'Features')

colors_border <- colormap::colormap(colormap=colormaps$viridis,
                                    nshades=4, alpha=1)
colors_in     <- colormap::colormap(colormap=colormaps$viridis,
                                    nshades=4, alpha=0.3)
cc <- gridExtra::ttheme_default(
                     core=list(bg_params=list(
                                   fill=rep(colors_in, each=8))))

pdf(file='figures/spider.pdf', width=10, height=5)
old_par <- par(mar=c(b=0, l=0, t=0, r=0))
patchwork::wrap_elements(full=~fmsb::radarchart(data,
                                                axistype=0, 
                                                pcol=colors_border,
                                                pfcol=colors_in,
                                                plwd=4,
                                                plty=1, 
                                                cglcol='grey',
                                                cglty=1,
                                                axislabcol='grey')) +
    patchwork::wrap_elements(gridExtra::tableGrob(datatable, theme=cc)) +
patchwork::plot_layout(widths=c(1, 0.8))
par(old_par)
dev.off()

pdf(file='figures/spiders.pdf', width=7, height=6.5)
mytitle <- paste('Profile', 1:4)
par(mar=c(b=0, l=0, t=3, r=0), mfrow=c(2, 2))
for (i in seq(4)){
    fmsb::radarchart(data[c(1, 2, i+2), ],
                     axistype=0, 
                     pcol=colors_border[i],
                     pfcol=colors_in[i],
                     plwd=4,
                     plty=1, 
                     cglcol='grey',
                     cglty=1,
                     axislabcol='grey',
                     title=mytitle[i])
}
layout(1)
dev.off()
