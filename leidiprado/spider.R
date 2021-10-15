##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-out-13 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

if(!requireNamespace('pacman', quietly=TRUE)) install.packages('pacman')
pacman::p_load(tibble, tidyr, colormap, gridExtra, patchwork, fmsb,
               gridGraphics, ggplot2, ggthemes)

# Coefficients ---------------------------------------------------------

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
                       'Med/Big', 'Small', 'Med/Big', 'Small'
                   ))
datatable <-
    tidyr::pivot_longer(datatable, !Profile,
                        names_to='Features', values_to='Status')
datatable <-
    tidyr::spread(datatable, Profile, Status)
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
    patchwork::wrap_elements(
                   gridExtra::tableGrob(datatable, theme=cc)
               ) +
    patchwork::plot_layout(widths=c(1, 0.8))
par(old_par)
dev.off()

pdf(file='figures/spiders.pdf', width=10, height=6.05)
mytitle <- paste('Profile', 1:4)
## par(mar=c(b=0, l=0, t=3, r=0), mfrow=c(2, 2))
## for (i in seq(4)){
##     fmsb::radarchart(data[c(1, 2, i+2), ],
##                      axistype=0, 
##                      pcol=colors_border[i],
##                      pfcol=colors_in[i],
##                      plwd=4,
##                      plty=1, 
##                      cglcol='grey',
##                      cglty=1,
##                      axislabcol='grey',
##                      title=mytitle[i])
## }
## layout(1)

layout <- '
ABC
DEE
'
cc2 <- gridExtra::ttheme_default(
                      core=list(bg_params=list(
                                    fill=rep(colors_in, times=4)
                                )))
old_par <- par(mar=c(b=0, l=0, t=2, r=0))
patchwork::wrap_elements(
               full=~fmsb::radarchart(
                               data[c(1, 2, 3), ], axistype=0,
                               pcol=colors_border[1],
                               pfcol=colors_in[1],
                               plwd=4, plty=1, cglty=1,
                               cglcol='grey', axislabcol='grey',
                               title=mytitle[1]
                           )) +
    patchwork::wrap_elements(
                   full=~fmsb::radarchart(
                                   data[c(1, 2, 4), ], axistype=0, 
                                   pcol=colors_border[2],
                                   pfcol=colors_in[2],
                                   plwd=4, plty=1, cglty=1,
                                   cglcol='grey', axislabcol='grey',
                                   title=mytitle[2]
                               )) +
    patchwork::wrap_elements(
                   full=~fmsb::radarchart(
                                   data[c(1, 2, 5), ], axistype=0, 
                                   pcol=colors_border[3],
                                   pfcol=colors_in[3],
                                   plwd=4, plty=1, cglty=1, 
                                   cglcol='grey', axislabcol='grey',
                                   title=mytitle[3]
                               )) +
    patchwork::wrap_elements(
                   full=~fmsb::radarchart(
                                   data[c(1, 2, 6), ], axistype=0, 
                                   pcol=colors_border[4],
                                   pfcol=colors_in[4],
                                   plwd=4, plty=1, cglty=1, 
                                   cglcol='grey', axislabcol='grey',
                                   title=mytitle[4]
                               )) +
    patchwork::wrap_elements(
                   gridExtra::tableGrob(
                                  round(data[-c(1:2), ], 2),
                                  rows=paste('Profile', 1:4),
                                  cols=c(
                                      'Pesticides\nreduction',
                                      'Contract\nlength',
                                      'With\npenalization',
                                      'Unbreakable\ncontract'
                                  ), 
                                  theme=cc2)
               ) +
    patchwork::plot_layout(design=layout)
par(old_par)
dev.off()

# WTA profit -----------------------------------------------------------

## The first two lines indicate the 'spider' max and min, respectively
data_wta <-
    tibble::tibble('Pesticides reduction'=c(
                       175, 0,
                       91.5470, 93.7128, 19.3433, 152.2316
                   ),
                   'Contract\nlength'=c(
                       450, -150,
                       -5.1169, 325.6761, 430.2755, -112.5390
                   ),
                   'With penalization'=c(
                       -2000, -4000,
                       -2548.2406, -2127.7411, -2043.5779, -3592.0640
                   ),
                   'Unbreakable\ncontract'=c(
                       2000, 500,
                       1247.6465, 664.9827, 1118.8968, 1831.3370
                   ))

pdf(file='figures/spider_wta.pdf', width=10, height=5)
old_par <- par(mar=c(b=0, l=0, t=0, r=0))
patchwork::wrap_elements(full=~fmsb::radarchart(data_wta,
                                                axistype=0, 
                                                pcol=colors_border,
                                                pfcol=colors_in,
                                                plwd=4,
                                                plty=1, 
                                                cglcol='grey',
                                                cglty=1,
                                                axislabcol='grey')) +
    patchwork::wrap_elements(
                   gridExtra::tableGrob(datatable, theme=cc)
               ) +
    patchwork::plot_layout(widths=c(1, 0.8))
par(old_par)
dev.off()

pdf(file='figures/spiders_wta.pdf', width=10, height=6.05)
## par(mar=c(b=0, l=0, t=3, r=0), mfrow=c(2, 2))
## for (i in seq(4)){
##     fmsb::radarchart(data_wta[c(1, 2, i+2), ],
##                      axistype=0, 
##                      pcol=colors_border[i],
##                      pfcol=colors_in[i],
##                      plwd=4,
##                      plty=1, 
##                      cglcol='grey',
##                      cglty=1,
##                      axislabcol='grey',
##                      title=mytitle[i])
## }
## layout(1)

old_par <- par(mar=c(b=0, l=0, t=2, r=0))
patchwork::wrap_elements(
               full=~fmsb::radarchart(
                               data_wta[c(1, 2, 3), ], axistype=0,
                               pcol=colors_border[1],
                               pfcol=colors_in[1],
                               plwd=4, plty=1, cglty=1,
                               cglcol='grey', axislabcol='grey',
                               title=mytitle[1]
                           )) +
    patchwork::wrap_elements(
                   full=~fmsb::radarchart(
                                   data_wta[c(1, 2, 4), ], axistype=0, 
                                   pcol=colors_border[2],
                                   pfcol=colors_in[2],
                                   plwd=4, plty=1, cglty=1,
                                   cglcol='grey', axislabcol='grey',
                                   title=mytitle[2]
                               )) +
    patchwork::wrap_elements(
                   full=~fmsb::radarchart(
                                   data_wta[c(1, 2, 5), ], axistype=0, 
                                   pcol=colors_border[3],
                                   pfcol=colors_in[3],
                                   plwd=4, plty=1, cglty=1, 
                                   cglcol='grey', axislabcol='grey',
                                   title=mytitle[3]
                               )) +
    patchwork::wrap_elements(
                   full=~fmsb::radarchart(
                                   data_wta[c(1, 2, 6), ], axistype=0, 
                                   pcol=colors_border[4],
                                   pfcol=colors_in[4],
                                   plwd=4, plty=1, cglty=1, 
                                   cglcol='grey', axislabcol='grey',
                                   title=mytitle[4]
                               )) +
    patchwork::wrap_elements(
                   gridExtra::tableGrob(
                                  round(data_wta[-c(1:2), ], 2),
                                  rows=paste('Profile', 1:4),
                                  cols=c(
                                      'Pesticides\nreduction',
                                      'Contract\nlength',
                                      'With\npenalization',
                                      'Unbreakable\ncontract'
                                  ), 
                                  theme=cc2)
               ) +
    patchwork::plot_layout(design=layout)
par(old_par)
dev.off()

# Scenarios ------------------------------------------------------------

descs <- c(
    '50% pesticides reduction, 05 years contract and Unbreakable',
    '10% pesticides reduction, 15 years contract and withoutpenality',
    '25% pesticides reduction, 05 years contract and unbreakable',
    '10% pesticides reduction, 10 years contract and Unbreakable',
    '25% pesticides reduction, 15 years contract and withoutpenality',
    '50% pesticides reduction, 05 years contract and Withpenality'
)
dat_scenarios <-
    tibble::tibble(
                profile=rep(c('Profile 1',
                              'Profile 2',
                              'Profile 3',
                              'Profile 4'), times=6), 
                scenario=rep(c('Scenario 11\nProg 1',
                               'Scenario 09\nProg 1',
                               'Scenario 17\nProg 1',
                               'Scenario 24\nProg 2',
                               'Scenario 23\nProg 2',
                               'Scenario 09\nProg 2'), each=4),
                description=rep(descs, each=4),
                amount=c(5799.4126, 6979.0008, 4237.4410, 8880.2215,
                         838.7163, 5822.2688, 6647.5656, -165.7692, 
                         3510.7372, 4636.182, 3753.858, 5074.432, 
                         2111.947, 4858.871, 5615.085, 2228.263, 
                         2211.921, 7227.960, 6937.716, 2117.705, 
                         2003.5255, 4186.2770, 1074.9662, 3456.8206))
dat_scenarios <-
    dplyr::mutate(
               dat_scenarios, 
               scenario=haven::as_factor(scenario),
               description=haven::as_factor(description)
           )
pdf(file='figures/scenarios.pdf', width=7.5, height=9)
ggplot(dat_scenarios, aes(x=amount, y=profile, fill=description))+
    geom_bar(stat='identity', alpha=0.75)+
    geom_text(aes(label=round(amount, 2)),
              fontface='bold', size=4, hjust=-0.2)+
    facet_grid(scenario ~ ., scales='free', space='free')+
    scale_x_continuous(
        expand=c(0, 0), breaks=NULL, limits=c(-175, 10500)
    )+
    labs(title='Scenarios')+
    scale_fill_viridis_d()+
    ggthemes::theme_fivethirtyeight(base_size=14)+
    theme(strip.text.y=element_text(face='bold', size=14, angle=0))+
    guides(fill=guide_legend(ncol=1, title=NULL))
dev.off()
