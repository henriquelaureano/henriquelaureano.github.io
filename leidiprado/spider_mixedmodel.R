
##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-out-18 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

if(!requireNamespace('pacman', quietly=TRUE)) install.packages('pacman')
pacman::p_load(tibble, tidyr, colormap, gridExtra, patchwork, fmsb,
               gridGraphics, dplyr, haven, ggplot2, ggthemes)

# Coefficients ---------------------------------------------------------

## The first two lines indicate the 'spider' max and min, respectively
data <- tibble::tibble('Pesticides reduction, 25%'=c(
                           0, -1050,
                           -107.7132, -116.6967, -8.3620, -1047.6662
                       ), 
                       'Pesticides\nreduction,\n50%'=c(
                           0, -150,
                           -25.8545, -31.7276, -5.4246, -136.7149
                       ),
                       'Contract\nlength,\n10 years'=c(
                           0, -1e4,
                           -958.1973, -1013.8386, -54.0911, -9594.6719
                       ),
                       'Contract length, 15 years'=c(
                           0, -1e4,
                           -999.9526, -1062.3825, -28.1475, -9995.3759
                       ),
                       'With\npenalization'=c(
                           0, 1.5,
                           1.3557, -0.0634, 0.0746, 1.3557
                       ),
                       'Unbreakable\ncontract'=c(
                           0, -8.5,
                           -6.3090, -2.7923, -8.2025, -6.3090
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
                       'Average', 'Max', 'Mode', 'Average'
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

pdf(file='figures_mixedmodel//spider.pdf', width=10, height=5)
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

pdf(file='figures_mixedmodel//spiders.pdf', width=8, height=10)
mytitle <- paste('Profile', 1:4)
layout <- '
AB
CD
EE
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
                                      'Pesticides\nreduction\n(25%)', 
                                      'Pesticides\nreduction,\n50%',
                                      'Contract\nlength\n(10 years)',
                                      'Contract\nlength\n(15 years)',
                                      'With\npenalization',
                                      'Unbreakable\ncontract'
                                  ),
                                  theme=cc2)
               ) +
    patchwork::plot_layout(design=layout, heights=c(1, 1, 0.6))
par(old_par)
dev.off()

# WTA profit -----------------------------------------------------------

## The first two lines indicate the 'spider' max and min, respectively
data_wta <-
    tibble::tibble('Pesticides reduction, 25%'=c(
                       2100, 0,
                       214.4449, 232.3303, 16.6477, 2085.7862
                   ), 
                   'Pesticides\nreduction,\n50%'=c(
                       300, 0,
                       51.4728, 63.1656, 10.7997, 272.1807
                   ),
                   'Contract\nlength,\n10 years'=c(
                       2e4, 100,
                       1907.6474, 2018.4231, 107.6892, 19101.7560
                   ),
                   'Contract length, 15 years'=c(
                       2e4, 50,
                       1990.7660, 2115.0563, 56.0391, 19899.3873
                   ),
                   'With\npenalization'=c(
                       0.25, -2.75,
                       -2.6990, 0.1263, -0.1484, -2.6990
                   ),
                   'Unbreakable\ncontract'=c(
                       17, -6,
                       12.5606, 5.5591, 16.3303, 12.5606
                   ))

pdf(file='figures_mixedmodel//spider_wta.pdf', width=10, height=5)
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

pdf(file='figures_mixedmodel//spiders_wta.pdf', width=8, height=10)
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
                                      'Pesticides\nreduction\n(25%)', 
                                      'Pesticides\nreduction,\n50%',
                                      'Contract\nlength\n(10 years)',
                                      'Contract\nlength\n(15 years)',
                                      'With\npenalization',
                                      'Unbreakable\ncontract'
                                  ), 
                                  theme=cc2)
               ) +
    patchwork::plot_layout(design=layout, heights=c(1, 1, 0.6))
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
                amount=c(65, 70, 29, 286, 1992, 2116, 57, 19901, 228,
                         239, 34, 2100, 1922, 2025, 125, 19116, 2207,
                         2349, 74, 21987, 50, 65, 12, 271)
            )
dat_scenarios <-
    dplyr::mutate(
               dat_scenarios, 
               scenario=haven::as_factor(scenario),
               description=haven::as_factor(description)
           )

pdf(file='figures_mixedmodel//scenarios.pdf', width=7.5, height=9.5)
ggplot(dat_scenarios, aes(x=amount, y=profile, fill=description))+
    geom_bar(stat='identity', alpha=0.75)+
    geom_text(aes(label=round(amount, 2)),
              fontface='bold', size=4, hjust=-0.1)+
    facet_grid(scenario ~ ., scales='free', space='free')+
    scale_x_continuous(
        expand=c(0, 0), breaks=NULL, limits=c(0, 24500)
    )+
    labs(title='Scenarios')+
    scale_fill_viridis_d()+
    ggthemes::theme_fivethirtyeight(base_size=14)+
    theme(strip.text.y=element_text(face='bold', size=14, angle=0))+
    guides(fill=guide_legend(ncol=1, title=NULL))
dev.off()
