## ----------------------------------------------------------------------------------- ##
##          META-ANALYSIS: ALEJANDRA CARDOSO, PELÉ PEQUENO PRÍNCIPE RESEARCH INSTITUTE ##
##                                                                                     ##
##                                                                   Henrique Laureano ##
##                                           LAST MODIFICATION ON *Sat, Jul 06 - 2024* ##
## ----------------------------------------------------------------------------------- ##

## --------------------------------------------------------------------------------------
## PKGS & CONF

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman") 

pacman::p_load(meta)

## --------------------------------------------------------------------------------------
## HR

hr <- data.frame(Study    = c("Fu, Y.", "Minard-Colin, V.", "Zhen, Z.", "Huang, S."),
                 HR       = c(    0.34,               0.32,       0.33,        0.57),
                 variance = c(    2.22,                  7,       2.48,         3.1))
hr_meta <- 
  meta::metagen(TE = log(hr$HR), seTE = sqrt(hr$variance), studlab = hr$Study, sm = "HR")

meta::forest(hr_meta)

# I² of 0%, tau² of 0, and a p-value of 1.
#
# The studies are very consistent in their findings regarding the hazard ratios. There is
# no evidence of heterogeneity beyond what would be expected due to chance and no
# indication of significant between-study variance in the true effects. Therefore, the
# studies are remarkably similar in their estimates of the hazard ratios.

## --------------------------------------------------------------------------------------
## OR

or <- data.frame(Study            = c("Fujita et al. (2008)",    "Anoop et al. (2012)", 
                                       "Osumi et al. (2016)", "Woessmann et al (2020)"),
                 events_treatment = c( 4, 10, 16, 35),
                 n_treatment      = c( 7, 17, 28, 55),
                 events_control   = c(13,  8,  1, 13),
                 n_control        = c(19, 10,  5, 13))

or_meta <- meta::metabin(event.e = or$events_treatment,
                             n.e = or$n_treatment,
                         event.c = or$events_control,
                             n.c = or$n_control, studlab = or$Study, sm = "OR")
meta::forest(or_meta)

# - I² of 50% suggests moderate heterogeneity, indicating some inconsistency among study
#   results.
#
# - tau² of 1.1678 indicates considerable between-study variance. P-value of 0.11 
#   suggests that there is no statistically significant heterogeneity, but there may 
#   still be practical or clinical heterogeneity that could be explored further.
#
# These results indicate that while there is moderate inconsistency among the studies, 
# the evidence for heterogeneity is not strong enough to be statistically significant at
# the conventional threshold. It would be prudent to investigate potential sources of 
# heterogeneity, such as differences in study populations, interventions, or 
# methodologies, to better understand the observed variation in effect sizes.

## --------------------------------------------------------------------------------------
## REFERENCES

# We performed the meta-analysis and created the forest plot using R version 4.3.0 
# [R Core Team, 2024]. The meta-analysis was conducted with the 'meta' package 
# version 7.0-0 [Schwarzer, Carpenter, and Rücker, 2024].
#
# R Core Team (2024). 
# R: A language and environment for statistical computing. 
# R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
#
# Schwarzer, G., Carpenter, J. R., & Rücker, G. (2024). 
# meta: General Package for Meta-Analysis. 
# R package version 7.0-0. URL https://CRAN.R-project.org/package=meta.

## ------------------------------------------------------------------------------- FIM ##