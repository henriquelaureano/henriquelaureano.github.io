#NEVES DO PRADO LEIDIMARI 
# Packages for running the Model:
## packages <- c(
##   # Package for data manipulation:
##   "tidyverse",
##   "readxl",
##   "xlsx", # I have add this one for using write.xlsx tables directly 
##   # Package for visualization:
##   "ggcorrplot",
##   # Package for modeling:
##   "multgee"
## ) 

# Installing and uploading the packages:
## installed_packages <- packages %in% rownames(installed.packages())

# Checking non installed packages and uploading them:
## if (any(installed_packages == FALSE)) {
##   install.packages(packages[!installed_packages])
## } else {
##   invisible(lapply(packages, library, character.only = TRUE))
## }

library(tidyverse)
library(readxl)
library(xlsx)
library(ggcorrplot)
library(multgee)

#setwd("~/Dropbox/Leidimari_PRADO_P3")
## setwd("~/Dropbox/Leidimari_PRADO_P3/Enviar_Henrique")
# Uploading the data set:
da <- read_xlsx("datalnp.xlsx", 
                ## sheet = "Sheet1"
                ) %>%
    ## Rename the first colum:
    rename(ind = (`...1`)) %>%  
    ## Factor:
    mutate(ind = factor(ind))

# data:
glimpse(da)

# Rename:
dt <- da %>%
  # Renaming the variables:
  rename(
    # Changing the order:
    BFC_OK_2008 = BFC_2008_OK,
    BFC_OK_2013 = BFC_2013_OK,
    BFC_OK_2018 = BFC_2018_OK,    
    ) %>%
  # Changing for lowercase letters:
  rename_with(., tolower) %>%
  # Removing letter "ha":
  rename_with(., ~ gsub("ha_", "", .x, fixed = TRUE)) %>%
  select(
    # Removing variables non used:
    -c(
      bfc_ok_2008, bfc_ok_2013, bfc_ok_2018, 
      bfc_2008, bfc_2013, bfc_2018 , 
      total_area_covered_by_forest_2008, 
      total_area_covered_by_forest_2013, total_area_covered_by_forest_2018,
      total_deforested_area_2008, total_deforested_area_2013, total_deforested_area_2018,
      wpp_2013,
    )
  )

glimpse(dt)

# Response variable
response <- dt %>%
  # Selecting the response variables:
  select(ind, contains("l1s0") | contains("l1s1") |
           contains("l0s0") | contains("l0s1")) %>%
  gather(data = ., key = "ls", value = "val", -ind) %>%
  mutate(year = str_sub(ls, start = 6)) %>%
  mutate(
    ls = str_sub(ls, end = 4),
    ls_ind = if_else(ls == "l0s0", 1,
                     if_else(ls == "l1s0", 2,
                             if_else(ls == "l0s1", 3, 4)
                     )
    )
  )

glimpse(response)

responsex <-response[order(response$ind,response$year),]

#############################################################################################
########### Testing variables that will be used in the final model ###########
#############################################################################################

# Covariates:
cov <- dt %>%
  select(
    ind,
    reg,
    coordx, coordy,
    percent_slope,
    gender,                            
    type.prop.surface,
    small.prop.surface,
    medium.prop.surface,
    big.prop.surface,
    region,
  )

glimpse(cov)

# Joining the variables:
data <- inner_join(x = response, y = cov, by = "ind")

# perc_eucalyptus:
c1 <- dt %>%
  select(ind, contains("perc_eucalyptus")) %>%
  gather(data = ., key = "var", value = "perc_eucalyptus", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c1, by = c("ind", "year"))

# perc_agri_:
c2 <- dt %>%
  select(ind, contains("perc_agri_")) %>%
  gather(data = ., key = "var", value = "perc_agri_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c2, by = c("ind", "year"))

# perc_pasture_:
c3 <- dt %>%
  select(ind, contains("perc_pasture_")) %>%
  gather(data = ., key = "var", value = "perc_pasture_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c3, by = c("ind", "year"))

# perc_hort_:
c4 <- dt %>%
  select(ind, contains("perc_hort_")) %>%
  gather(data = ., key = "var", value = "perc_hort_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c4, by = c("ind", "year"))

# total_area_:
c5 <- dt %>%
  select(ind, contains("total_area_")) %>%
  gather(data = ., key = "var", value = "total_area_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c5, by = c("ind", "year"))

# first.activity_:
c6 <- dt %>%
  select(ind, contains("first.activity_")) %>%
  gather(data = ., key = "var", value = "first.activity_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c6, by = c("ind", "year"))


# leduc_:
c7 <- dt %>%
  select(ind, contains("leduc_")) %>%
  gather(data = ., key = "var", value = "leduc_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c7, by = c("ind", "year"))

# age_:
c8 <- dt %>%
  select(ind, contains("age_")) %>%
  gather(data = ., key = "var", value = "age_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c8, by = c("ind", "year"))

# average.income_:
c09 <- dt %>%
  select(ind, contains("average.income_")) %>%
  gather(data = ., key = "var", value = "average.income_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c09, by = c("ind", "year"))

# dec.maker_:
c10 <- dt %>%
  select(ind, contains("dec.maker_")) %>%
  gather(data = ., key = "var", value = "dec.maker_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c10, by = c("ind", "year"))

# hlong_:
c11 <- dt %>%
  select(ind, contains("hlong_")) %>%
  gather(data = ., key = "var", value = "hlong_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c11, by = c("ind", "year"))

# agriculture_:
c12 <- dt %>%
  select(ind, contains("agriculture_")) %>%
  gather(data = ., key = "var", value = "agriculture_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c12, by = c("ind", "year"))

# eucalyptus_:
c13 <- dt %>%
  select(ind, contains("perc_eucalyptus_")) %>%
  gather(data = ., key = "var", value = "perc_eucalyptus_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c13, by = c("ind", "year"))

# pasture_:
c14 <- dt %>%
  select(ind, contains("perc_pasture_")) %>%
  gather(data = ., key = "var", value = "perc_pasture_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c14, by = c("ind", "year"))

# horticultural_:
c15 <- dt %>%
  select(ind, contains("horticultural_")) %>%
  gather(data = ., key = "var", value = "horticultural_", -ind) %>%
  mutate(
    year = str_sub(var, start = -4)
  ) %>%
  select(-var)
data <- data %>% inner_join(x = ., y = c15, by = c("ind", "year"))

data <- data %>%
  mutate(
    perc_agrihort_ = perc_agri_ + perc_hort_
  )

data <- data %>%
  mutate(
    medium.big.surface = medium.prop.surface + big.prop.surface
  ) 

# Data:
glimpse(data)

#######################################################################################
################## My data seems to be organised for the multinomial ##################
#######################################################################################
#----------------------------Fitting the model-------------------------------------

final <- data %>%
  # Filtering the individuals by unique value of val:
  #filter(ind == "1") %>%
  filter(val == "1") %>%
  # Removing NA values (not available values):
  drop_na() %>%
  # Building the factors:
  mutate(
    reg = factor(reg),
    year = factor(year),
    gender = factor(gender),
    type.prop.surface = factor(type.prop.surface),
    first.activity_ = factor(first.activity_), 
    leduc_ = factor(leduc_),
    dec.maker_ = factor(dec.maker_)
  )

glimpse(final)

final = final %>%
  group_by (ind)   %>%
  mutate (yearTest = paste0(year,".", seq_along(year)))

# Fitting the model -adapted from leidi and works
final1 <-final[order(final$ind,final$year),]

# Fitting the model:

table(final1$perc_eucalyptus_)

fitmodINCOME <-
    multgee::nomLORgee(
                                        # Model specification:
                 formula =
                     ls_ind ~
                         1 +
                         average.income_ +
                         age_ +
                         leduc_ +
                         gender +
                         hlong_ +
                         perc_pasture_.y +
                         perc_agrihort_ +
                         perc_eucalyptus_ +
                         small.prop.surface +
                         dec.maker_ +
                         percent_slope +
                         region 
                ,
                                        # Cluster identification:
                 id = ind,
                                        # Marginalized local odds ratios structure:
                 LORstr = "time.exch",
                                        # Order of observations within each cluster:
                 repeated = year ,
                                        # Data set:
                 data = final1
             )

summary(fitmodINCOME)
vcov(fitmodINCOME) 
# perc_pasture_.y + perc_eucalyptus_ + perc_agri_

# Summary:
res <- summary(fitmodINCOME)$coefficients %>%
  as.data.frame() %>%
  rename(`Estimate` = Estimate, `Standard Error` = san.se, `P-value` = `Pr(>|san.z|)`) %>%
  mutate(
    Estimate = round(Estimate, 3),
    `Standard Error` = round(`Standard Error`, 3),
    `P-value` = round(`P-value`, 3)
  ) %>%
  select(-`san.z`)
res

# write.table(res, file = "data/res_summary_model.csv")

## VISUALIZATIONS ------------------------------------------------------

library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

## nrc   <- nrow(res)/3
## l     <- 1 * c(1, 1+nrc, 1+nrc*2)
## nume  <- exp(res[l, 'Estimate'])
## denom <- 1 + sum(nume)
## c(nume/denom, 1/denom)

## write.table(tibble::rownames_to_column(res), file='res_summary_model.csv')
## write.table(dplyr::ungroup(final1), file='final1.csv')

plotObj <- function(i1, i2, i3, start, end)
{
    mgrid  <- seq(start, end, length.out=40)
    n1.est <- exp(res[ 1, 1]              + mgrid *  res[i1, 1]              )
    n1.low <- exp(res[ 1, 1] - res[ 1, 2] + mgrid * (res[i1, 1] - res[i1, 2]))
    n1.upp <- exp(res[ 1, 1] + res[ 1, 2] + mgrid * (res[i1, 1] + res[i1, 2]))
    n2.est <- exp(res[21, 1]              + mgrid *  res[i2, 1]              )
    n2.low <- exp(res[21, 1] - res[21, 2] + mgrid * (res[i2, 1] - res[i2, 2]))
    n2.upp <- exp(res[21, 1] + res[21, 2] + mgrid * (res[i2, 1] + res[i2, 2]))
    n3.est <- exp(res[41, 1]              + mgrid *  res[i3, 1]              )
    n3.low <- exp(res[41, 1] - res[41, 2] + mgrid * (res[i3, 1] - res[i3, 2]))
    n3.upp <- exp(res[41, 1] + res[41, 2] + mgrid * (res[i3, 1] + res[i3, 2]))
    denom.est <- 1 + rowSums(cbind(n1.est, n2.est, n3.est))
    denom.low <- 1 + rowSums(cbind(n1.low, n2.low, n3.low))
    denom.upp <- 1 + rowSums(cbind(n1.upp, n2.upp, n3.upp))
    dat.est <-
        tibble::tibble(x=mgrid,
                       L0S0=n1.est/denom.est,
                       L1S0=n2.est/denom.est,
                       L0S1=n3.est/denom.est,
                       L1S1=     1/denom.est)|>
        tidyr::pivot_longer(L0S0:L1S1,
                            names_to='p', values_to='value.est')
    dat.low <-
        tibble::tibble(L0S0=n1.low/denom.low,
                       L1S0=n2.low/denom.low,
                       L0S1=n3.low/denom.low,
                       L1S1=     1/denom.low)|>
        tidyr::pivot_longer(L0S0:L1S1,
                            names_to='p', values_to='value.low')|>
        dplyr::select(value.low)
    dat.upp <-
        tibble::tibble(L0S0=n1.upp/denom.upp,
                       L1S0=n2.upp/denom.upp,
                       L0S1=n3.upp/denom.upp,
                       L1S1=     1/denom.upp)|>
        tidyr::pivot_longer(L0S0:L1S1,
                            names_to='p', values_to='value.upp')|>
        dplyr::select(value.upp)
    out <- dplyr::bind_cols(dat.est, dat.low, dat.upp)
    return(out)
}

gg1 <-
    plotObj(i1=2, i2=22, i3=42,
            start=min(final1$average.income_),
            end=max(final1$average.income_))|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(2500, 25e3, by=2500))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Average income', y='Probability',
         title='Probabilities vs. Average income')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
gg1

gg2 <-
    plotObj(i1=3, i2=23, i3=43,
            start=min(final1$age_),
            end=max(final1$age_))|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         title='Probabilities vs. Age')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
gg2

gg3 <-
    plotObj(i1=11, i2=31, i3=51,
            start=min(final1$hlong_),
            end=max(final1$hlong_))|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(0, 60, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='H long', y='Probability',
         title='Probabilities vs. H long')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
gg3

gg4 <-
    plotObj(i1=12, i2=32, i3=52,
            start=min(final1$perc_pasture_.y),
            end=max(final1$perc_pasture_.y))|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(0, 100, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Pasture percentage', y='Probability',
         title='Probabilities vs. Pasture percentage')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
gg4

gg5 <-
    plotObj(i1=13, i2=33, i3=53,
            start=min(final1$perc_agrihort_),
            end=max(final1$perc_agrihort_))|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(0, 100, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Agrihort percentage', y='Probability',
         title='Probabilities vs. Agrihort percentage')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
gg5

gg6 <-
    plotObj(i1=18, i2=38, i3=58,
            start=min(final1$percent_slope),
            end=max(final1$percent_slope))|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(0, 100, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Slope percentage', y='Probability',
         title='Probabilities vs. Slope percentage')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
gg6

gg7 <-
    plotObj(i1=14, i2=34, i3=54,
            start=min(final1$perc_eucalyptus_),
            end=max(final1$perc_eucalyptus_))|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(0, 100, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Slope percentage', y='Probability',
         title='Probabilities vs. Slope percentage')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
gg7

layout <- '
ABC
DEF
#G#
'
gg1 + theme(legend.position='none') +
gg2 + theme(legend.position='none') +
gg3 + theme(legend.position='none') +
gg4 + theme(legend.position='none') +
gg5 + theme(legend.position='none') +
gg6 + theme(legend.position='none') +
gg7 + patchwork::plot_layout(design=layout)

plotObj_fac <- function(i1, i2, i3, start, end, j1, j2, j3)
{
    mgrid  <- seq(start, end, length.out=40)
    n1.est <- exp(res[ 1, 1]              +
                  mgrid *  res[i1, 1]               +
                  res[j1, 1])
    n1.low <- exp(res[ 1, 1] - res[ 1, 2] +
                  mgrid * (res[i1, 1] - res[i1, 2]) +
                  res[j1, 1] - res[j1, 2])
    n1.upp <- exp(res[ 1, 1] + res[ 1, 2] +
                  mgrid * (res[i1, 1] + res[i1, 2]) +
                  res[j1, 1] + res[j1, 2])
    n2.est <- exp(res[21, 1]              +
                  mgrid *  res[i2, 1]               +
                  res[j2, 1])
    n2.low <- exp(res[21, 1] - res[21, 2] +
                  mgrid * (res[i2, 1] - res[i2, 2]) +
                  res[j2, 1] - res[j2, 2])
    n2.upp <- exp(res[21, 1] + res[21, 2] +
                  mgrid * (res[i2, 1] + res[i2, 2]) +
                  res[j2, 1] + res[j2, 2])
    n3.est <- exp(res[41, 1]              +
                  mgrid *  res[i3, 1]               +
                  res[j3, 1])
    n3.low <- exp(res[41, 1] - res[41, 2] +
                  mgrid * (res[i3, 1] - res[i3, 2]) +
                  res[j3, 1] - res[j3, 2])
    n3.upp <- exp(res[41, 1] + res[41, 2] +
                  mgrid * (res[i3, 1] + res[i3, 2]) +
                  res[j3, 1] + res[j3, 2])
    denom.est <- 1 + rowSums(cbind(n1.est, n2.est, n3.est))
    denom.low <- 1 + rowSums(cbind(n1.low, n2.low, n3.low))
    denom.upp <- 1 + rowSums(cbind(n1.upp, n2.upp, n3.upp))
    dat.est <-
        tibble::tibble(x=mgrid,
                       L0S0=n1.est/denom.est,
                       L1S0=n2.est/denom.est,
                       L0S1=n3.est/denom.est,
                       L1S1=     1/denom.est)|>
        tidyr::pivot_longer(L0S0:L1S1,
                            names_to='p', values_to='value.est')
    dat.low <-
        tibble::tibble(L0S0=n1.low/denom.low,
                       L1S0=n2.low/denom.low,
                       L0S1=n3.low/denom.low,
                       L1S1=     1/denom.low)|>
        tidyr::pivot_longer(L0S0:L1S1,
                            names_to='p', values_to='value.low')|>
        dplyr::select(value.low)
    dat.upp <-
        tibble::tibble(L0S0=n1.upp/denom.upp,
                       L1S0=n2.upp/denom.upp,
                       L0S1=n3.upp/denom.upp,
                       L1S1=     1/denom.upp)|>
        tidyr::pivot_longer(L0S0:L1S1,
                            names_to='p', values_to='value.upp')|>
        dplyr::select(value.upp)
    out <- dplyr::bind_cols(dat.est, dat.low, dat.upp)
    return(out)
}

ggleduc1 <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=4, j2=24, j3=44)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Level of education 0 to 1')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggleduc1

ggleduc2 <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=5, j2=25, j3=45)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Level of education 0 to 2')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggleduc2

ggleduc3 <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=6, j2=26, j3=46)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Level of education 0 to 3')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggleduc3

ggleduc5 <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=7, j2=27, j3=47)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Level of education 0 to 5')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggleduc5

ggleduc6 <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=8, j2=28, j3=48)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Level of education 0 to 6')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggleduc6

ggleduc7 <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=9, j2=29, j3=49)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Level of education 0 to 7')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggleduc7

ggleduc1 + theme(legend.position='none') +
ggleduc2 + theme(legend.position='none') +
ggleduc3 + theme(legend.position='none') +
ggleduc5 + theme(legend.position='none') +
ggleduc6 +
ggleduc7 + theme(legend.position='none')

gggender <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=10, j2=30, j3=50)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Gender 0 to 1')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
gggender

ggsps <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=15, j2=35, j3=55)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Small property surface 0 to 1')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggsps

ggdmb <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=16, j2=36, j3=56)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Decision maker A to B')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggdmb

ggdmc <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=17, j2=37, j3=57)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Decision maker A to C')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggdmc

ggdmb + ggdmc

ggregb <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=19, j2=39, j3=59)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Region A to B')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggregb

ggregc <-
    plotObj_fac(i1=3, i2=23, i3=43,
                start=min(final1$age_),
                end=max(final1$age_),
                j1=20, j2=40, j3=60)|>
    ggplot(aes(x, value.est, color=p))+
    geom_line(size=1.25)+
    geom_ribbon(aes(ymin=value.low, ymax=value.upp),
                alpha=0.2, linetype='dashed')+
    scale_x_continuous(breaks=seq(17, 90, by=5))+
    scale_y_continuous(breaks=seq(0, 1, by=0.1))+
    scale_color_brewer(palette='Spectral')+
    labs(x='Age', y='Probability',
         subtitle='Probabilities vs. Age',
         title='Region A to C')+
    theme_minimal(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, l=0, b=-3, r=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, l=3, b=0, r=0), 'mm')),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(color=guide_legend(title='Class:', keywidth=2))
ggregc

ggregb + ggregc

###############################################################################
#####some testing
############################

 # average.income_ + age_  + leduc_ + gender + hlong_  + perc_pasture_.y
# + perc_agrihort_ +  small.prop.surface  + dec.maker_ + percent_slope

summary(da$L0S0_2008)
sd(final1$L0S0_2008)

summary(final1$ls_ind)
sd(final1$ls_ind)

#verifying data
lm(ls_ind~age_+average.income_ , data = final1)

######## Checking data and calculating probabilities  #############

# Function for calculating the probabilities:
prob <- function(coef,
                 vaverage.income,
                 vage_,
                 vleduc_ , 
                 vgender ,
                 vhlong_ , 
                 vperc_pasture_.y, 
                 vperc_agrihort_ ,
                 vsmall.prop.surface ,  
                 vdec.maker_ ,
                 vpercent_slope , 
                 vregion) {
  
  ### Parameters:
  
  # Logit 1:
  beta10 <- coef[1]
  average.income1 <- coef[2]
  age_1 <- coef[3]
  leduc_1 <- coef[4]
  gender1 <- coef[5]
  hlong_1 <- coef[6]
  perc_pasture_.y1 <- coef[7]
  perc_agrihort_1 <- coef[8]
  small.prop.surface1 <- coef[9]
  dec.maker_1 <- coef[10]
  percent_slope1 <- coef[11]
  region1 <- coef[12]
  
  # Logit 2:
  beta20 <- coef[13]
  average.income2 <- coef[14]
  age_2 <- coef[15]
  leduc_2 <- coef[16]
  gender2 <- coef[17]
  hlong_2 <- coef[18]
  perc_pasture_.y2 <- coef[19]
  perc_agrihort_2 <- coef[20]
  small.prop.surface2 <- coef[21]
  dec.maker_2 <- coef[22]
  percent_slope2 <- coef[23]
  region2 <- coef[24]
  
  # Logit 3:
  beta30 <- coef[25]
  average.income3 <- coef[26]
  age_3 <- coef[27]
  leduc_3 <- coef[28]
  gender3 <- coef[29]
  hlong_3 <- coef[30]
  perc_pasture_.y3 <- coef[31]
  perc_agrihort_3 <- coef[32]
  small.prop.surface3 <- coef[33]
  dec.maker_3 <- coef[34]
  percent_slope3 <- coef[35]
  region3 <- coef[36]
  
  ### Logit:
  
  # Logit 1:
  lp1 <- (
    beta10 +
      average.income1 * vaverage.income +
      age_1 * vage_ +
      leduc_1 * vleduc_ + 
      gender1 * vgender +
      hlong_1 * vhlong_ +
      perc_pasture_.y1 * vperc_pasture_.y + 
      perc_agrihort_1 * vperc_agrihort_ +
      small.prop.surface1 * vsmall.prop.surface +
      dec.maker_1 * vdec.maker_ +
      percent_slope1 * vpercent_slope +
      region1 * vregion
  )
  
  # Logit 2:
  lp2 <- (
    beta20 +
      average.income2 * vaverage.income +
      age_2 * vage_ +
      leduc_2 * vleduc_ + 
      gender2 * vgender +
      hlong_2 * vhlong_ +
      perc_pasture_.y2 * vperc_pasture_.y + 
      perc_agrihort_2 * vperc_agrihort_ +
      small.prop.surface2 * vsmall.prop.surface +
      dec.maker_2 * vdec.maker_ +
      percent_slope2 * vpercent_slope +
      region2 * vregion
  )
  
  # Logit 3:
  lp3 <- (
    beta30 +
      average.income3 * vaverage.income +
      age_3 * vage_ +
      leduc_3 * vleduc_ + 
      gender3 * vgender +
      hlong_3 * vhlong_ +
      perc_pasture_.y3 * vperc_pasture_.y + 
      perc_agrihort_3 * vperc_agrihort_ +
      small.prop.surface3 * vsmall.prop.surface +
      dec.maker_3 * vdec.maker_ +
      percent_slope3 * vpercent_slope +
      region3 * vregion
  )
  
  ### Probability:
  
  # Prob. logit 1:
  p1 <- exp(lp1) / (1 + exp(lp1) + exp(lp2) + exp(lp3))
  
  # Prob. logit 2:
  p2 <- exp(lp2) / (1 + exp(lp1) + exp(lp2) + exp(lp3))
  
  # Prob. logit 3:
  p3 <- exp(lp3) / (1 + exp(lp1) + exp(lp2) + exp(lp3))
  
  # Prob. logit 4:
  p4 <- 1 - (p1 + p2 + p3)
  
  # Out:
  out <- data.frame(
    "p1" = as.numeric(c(p1)),
    "p2" = as.numeric(c(p2)),
    "p3" = as.numeric(c(p3)),
    "p4" = as.numeric(c(p4))
  )
  
  # Out:
  return((out))
}

# Vectorize:

###########################
#some tests on other data
library("multgee")
data("arthritis", package = "multgee")
 intrinsic.pars(y = y, data = arthritis, id = id, repeated = time, rscale = "ordinal")
 fit <- nomLORgee(formula = y ~  factor(trt) +
                        factor(baseline), id = id, repeated = time,
                     data = arthritis, LORstr = "RC")
  summary(fit)
  
  data(housing)
  fitmod <- nomLORgee(y ~ factor(time) * sec, data = housing, id = id,
                      repeated = time)
  summary(fitmod)
