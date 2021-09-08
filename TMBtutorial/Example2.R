# Tutorial: Template Model Builder (TMB) R package ----------------------------
# Example 2: Beta mixed regression model --------------------------------------
# Water quality index data set ------------------------------------------------
# Authors: Henrique Laureano, Ricardo Petterle and Wagner Bonat LEG/UFPR ------
# Date: 07/09/2021 ------------------------------------------------------------

rm(list=ls())

# Loading extra packages
library(glmmTMB)
library(TMB)
library(tmbstan)
library(ggplot2)
library(tidyverse)
library(cowplot)

# Loading data set ------------------------------------------------------------
data2 <- read.table("IQA_Data_Set.txt", h = T)
head(data2, n = 12)

# Preparing data set 
data2$TRIM <- as.factor(data2$TRIM)
levels(data2$TRIM)
data2$LOCAL <- as.factor(data2$LOCAL)
data2$LOCAL <- factor(data2$LOCAL, levels = levels(data2$LOCAL)[c(2,3,1)])
levels(data2$LOCAL) <- c("Upstream", "Reservoir", "Downstream")
data2$id <- factor(data2$id)
levels(data2$id) <- paste("U", 1:16, sep = "")
data2 <- na.exclude(data2)
colnames(data2) <- c("id", "y", "location", "quarter")
head(data2)

# Descriptive analysis 
p1 <- ggplot(data2, aes(x = y)) +
        geom_histogram(binwidth = 0.05, colour = "black", 
                       fill = "gray80") + 
        ylab("Frequency") + xlab("y") 

p2 <- data2 %>% 
        ggplot(aes(x = quarter, y = y, fill = location)) +
        geom_boxplot() +
        xlab("Quarter")+ 
        ylab("y") +
        facet_wrap(~ location) +
        scale_fill_grey(start = 0.8, end = 0.8) + theme(legend.position = "none")

p3 <- data2 %>% 
        ggplot(aes(x = id, y = y)) +
        geom_boxplot(fill = "gray80") +
        xlab("Power plant")+ 
        ylab("y") 

bottom_row <- plot_grid(p1, p2, labels = c('', '(B)'), align = 'h', rel_widths = c(1, 1.3))
plot_grid(bottom_row, p3, labels = c('(A)', '(C)'), ncol = 1, rel_heights = c(1, 1.2))

# Fitting: glmmTMB package ----------------------------------------------------
fit_glmmTMB <- glmmTMB::glmmTMB(y ~ quarter + location + (1 | id), 
                                family = beta_family, data = data2)
summary(fit_glmmTMB)
# Precision parameter
glmmTMB::sigma(fit_glmmTMB)
# Fitting: TMB package --------------------------------------------------------
# Compilation
TMB::compile("Beta_Mixed.cpp")
dyn.load(TMB::dynlib("Beta_Mixed"))

# Model matrix
X <- model.matrix(~ quarter + location, data = data2)
Z <- model.matrix(~ factor(id)-1, data2)
X <- as(X,"dgTMatrix")
betas <- rnorm(ncol(X))
Z <- as(Z,"dgTMatrix")
u <- rnorm(ncol(Z))

# Response variable
Y <- data2$y

# Data set 
data <- list(Y = Y, X = X, Z = Z)

# Initial values
parameters <- list(beta = betas, logsigma = 1, logphi = 1, u = u*0)

# Log-likelihood function
Beta_TMB <- TMB::MakeADFun(data, parameters, DLL = "Beta_Mixed", 
                           random = "u",
                           hessian = TRUE, silent = TRUE)
# Fitting via nlminb() --------------------------------------------------------
fit_TMB <- nlminb(start = Beta_TMB$par, objective = Beta_TMB$fn, gradient = Beta_TMB$gr)
fit_TMB

# Report
report <- TMB::sdreport(Beta_TMB)

# Summary for fixed effects
summary(report, "fixed", p.value = TRUE)

# Delta method for phi
summary(report, "report", p.value = TRUE)

# Comparing regression coefficients
tab <- rbind("glmmTMB" = fit_glmmTMB$fit$par[1:6], 
             "TMB Beta" = c(fit_TMB$par[1:6]))
colnames(tab) <- c("(Intercept)", "Quarter2", "Quarter3", 
                   "Quarter4", "LocalReservoir", "LocalDownstream")
tab

# LogLik 
tab2 <- rbind("glmmTMB" = logLik(fit_glmmTMB), "TMB Beta" = abs(fit_TMB$objective))
colnames(tab2) <- "LogLik"
tab2

# Profile ---------------------------------------------------------------------
# Data
data <- as.data.frame(TMB::tmbprofile(Beta_TMB, 1, trace = F))

# mle
mle = fit_TMB$par[1]

p_beta0 <- ggplot() +
        geom_line(data = data, aes(x = beta, y = -1*value), size = 1) +
        theme_minimal(base_size = 16) +
        labs(x = expression(beta[0]), y = "Log-lik") +
        geom_vline(xintercept = mle, linetype = "dashed")

# Profile beta12 (Quarter2) ------------------------------------------------------
# Data
data <- as.data.frame(TMB::tmbprofile(Beta_TMB, 2, trace = F))

# mle
mle = fit_TMB$par[2]

p_beta12 <- ggplot() +
        geom_line(data = data, aes(x = beta, y = -1*value), size = 1) +
        theme_minimal(base_size = 16) +
        labs(x = expression(beta[12]), y = "Log-lik") +
        geom_vline(xintercept = mle, linetype = "dashed")

# Profile beta13 (Quarter3) ------------------------------------------------------
# Data
data <- as.data.frame(TMB::tmbprofile(Beta_TMB, 3, trace = F))

# mle
mle = fit_TMB$par[3]

p_beta13 <- ggplot() +
        geom_line(data = data, aes(x = beta, y = -1*value), size = 1) +
        theme_minimal(base_size = 16) +
        labs(x = expression(beta[13]), y = "Log-lik") +
        geom_vline(xintercept = mle, linetype = "dashed")

# Profile beta14 (Quarter4) ------------------------------------------------------
# Data
data <- as.data.frame(TMB::tmbprofile(Beta_TMB, 4, trace = F))

# mle
mle = fit_TMB$par[4]

p_beta14 <- ggplot() +
        geom_line(data = data, aes(x = beta, y = -1*value), size = 1) +
        theme_minimal(base_size = 16) +
        labs(x = expression(beta[14]), y = "Log-lik") +
        geom_vline(xintercept = mle, linetype = "dashed")

# Profile beta22 (LocalReservoir) ---------------------------------------------
# Data
data <- as.data.frame(TMB::tmbprofile(Beta_TMB, 5, trace = F))

# mle
mle = fit_TMB$par[5]

p_beta22 <- ggplot() +
        geom_line(data = data, aes(x = beta, y = -1*value), size = 1) +
        theme_minimal(base_size = 16) +
        labs(x = expression(beta[22]), y = "Log-lik") +
        geom_vline(xintercept = mle, linetype = "dashed")

# Profile beta23 (LocalDownstream) --------------------------------------------
# Data
data <- as.data.frame(TMB::tmbprofile(Beta_TMB, 6, trace = F))

# mle
mle = fit_TMB$par[6]

p_beta23 <- ggplot() +
        geom_line(data = data, aes(x = beta, y = -1*value), size = 1) +
        theme_minimal(base_size = 16) +
        labs(x = expression(beta[23]), y = "Log-lik") +
        geom_vline(xintercept = mle, linetype = "dashed") 

# Profile logsigma ------------------------------------------------------------
# Data
data <- as.data.frame(TMB::tmbprofile(Beta_TMB, 7, trace = F))

# mle
mle = fit_TMB$par[7]

p_logsigma <- ggplot() +
        geom_line(data = data, aes(x = logsigma, y = -1*value), size = 1) +
        theme_minimal(base_size = 16) +
        labs(x = expression(log(sigma)), y = "Log-lik") +
        geom_vline(xintercept = mle, linetype = "dashed")

# Profile logphi --------------------------------------------------------------
# Data
data <- as.data.frame(TMB::tmbprofile(Beta_TMB, 8, trace = F))

# mle
mle = fit_TMB$par[8]

p_logphi <- ggplot() +
        geom_line(data = data, aes(x = logphi, y = -1*value), size = 1) +
        theme_minimal(base_size = 16) +
        labs(x = expression(log(phi)), y = "Log-lik") +
        geom_vline(xintercept = mle, linetype = "dashed")

# Figure
gridExtra::grid.arrange(p_beta0, p_beta12, p_beta13, p_beta14,
                        p_beta22, p_beta23, p_logsigma, p_logphi,
                        ncol = 4, nrow = 2)

# Unit gamma mixed regression model
# Compilation
TMB::compile("Unit_gamma_Mixed.cpp")
dyn.load(TMB::dynlib("Unit_gamma_Mixed"))

# Model matrix
X <- model.matrix(~ quarter + location, data = data2)
Z <- model.matrix(~ factor(id)-1, data2)
X <- as(X,"dgTMatrix")
betas <- rnorm(ncol(X))
Z <- as(Z,"dgTMatrix")
u <- rnorm(ncol(Z))

# Response variable
Y <- data2$y

# Data set 
data <- list(Y = Y, X = X, Z = Z)

# Initial values
parameters <- list(beta = betas, logsigma = 1, logphi = 1, u = u*0)

# Log-likelihood function
UG_TMB <- TMB::MakeADFun(data, parameters, DLL = "Unit_gamma_Mixed", 
                         random = "u",
                         hessian = TRUE, silent = TRUE)
# Fitting via nlminb() --------------------------------------------------------
fit_UG <- nlminb(start = UG_TMB$par, objective = UG_TMB$fn, gradient = UG_TMB$gr)
fit_UG

# Report
report <- TMB::sdreport(UG_TMB)

# Summary for fixed effects
summary(report, "fixed", p.value = TRUE)

# Delta method for phi
summary(report, "report", p.value = TRUE)

# TMB Stan: Beta mixed regression model ---------------------------------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = T)
fit_TMB_Stan <- tmbstan::tmbstan(Beta_TMB, silent = F, laplace = F, chains = 4)
fit_TMB_Stan

# Plots
plot(fit_TMB_Stan, pars = names(Beta_TMB$par))

# Traceplot
trace <- traceplot(fit_TMB_Stan, pars = names(Beta_TMB$par))
trace + scale_color_discrete() + theme(legend.position = "top")

# TMB Stan: UG mixed regression model -----------------------------------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = T)
fit_TMB_Stan_UG <- tmbstan::tmbstan(UG_TMB, silent = F, laplace = F, chains = 4)
fit_TMB_Stan_UG

# Plots
plot(fit_TMB_Stan_UG, pars = names(UG_TMB$par))

# Traceplot
trace <- traceplot(fit_TMB_Stan_UG, pars = names(UG_TMB$par))
trace + scale_color_discrete() + theme(legend.position = "top")
# END -------------------------------------------------------------------------