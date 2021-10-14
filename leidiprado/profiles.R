##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-out-13 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

if(!requireNamespace('pacman', quietly=TRUE)) install.packages('pacman')
pacman::p_load(readxl, dplyr, tidyr,
               tibble, colormap, gridExtra, patchwork, fmsb,
               gridGraphics)

dat <- readxl::read_xlsx('Key_variables.xlsx', range='A1:AJ123')|>
    dplyr::select(!c(reg, prop.surface, age, income, dec.maker))
str(dat)

dat <-
    dat|>
    tidyr::pivot_longer(c(taquara, pipiripau),
                        names_to='region',
                        values_to='region_status')|>
    tidyr::pivot_longer(dplyr::ends_with('.prop.surface'),
                        names_to='prop.surface.class',
                        values_to='prop.surface_status')|>
    tidyr::pivot_longer(c(agriculture, cattle, porchi, forest),
                        names_to='land.use',
                        values_to='land.use_status')|>
    tidyr::pivot_longer(dplyr::starts_with('age.class.'),
                        names_to='age.class',
                        values_to='age.class_status')|>
    tidyr::pivot_longer(dplyr::starts_with('income.'),
                        names_to='income.class',
                        values_to='income.class_status')|>
    tidyr::pivot_longer(dplyr::starts_with('dec.maker.'),
                        names_to='dec.maker',
                        values_to='dec.maker_status')|>
    dplyr::filter(garden == 0)|>
    dplyr::filter(region_status == 1)|>
    dplyr::select(!region_status)|>
    dplyr::filter(prop.surface_status == 1)|>
    dplyr::select(!prop.surface_status)|>
    dplyr::filter(land.use_status == 1)|>
    dplyr::select(!land.use_status)|>
    dplyr::filter(age.class_status == 1)|>
    dplyr::select(!age.class_status)|>
    dplyr::filter(income.class_status == 1)|>
    dplyr::select(!income.class_status)|>
    dplyr::filter(dec.maker_status == 1)|>
    dplyr::select(!dec.maker_status)
str(dat)

dat|>
    dplyr::count(
               leduc, pita, ta, first.activity, agri, garden, hortfrut,
               region, prop.surface.class, land.use, age.class,
               income.class, dec.maker
           )|>
    ## dplyr::select(n)|>
    ## table()
    dplyr::filter(n == 3)|>
    write.csv('most_frequent_profiles.csv')
