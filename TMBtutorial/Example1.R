# Tutorial: Template Model Builder (TMB) R package ----------------------------
# Example 1: Univariate beta regression model ---------------------------------
# Stress anxiety data set -----------------------------------------------------
# Authors: Henrique Laureano, Ricardo Petterle and Wagner Bonat LEG/UFPR ------
# Date: 07/09/2021 ------------------------------------------------------------

rm(list=ls())

# Loading extra packages
library(betareg)
library(TMB)
library(tmbstan)
library(ggplot2)
library(tidyverse)

# Loading data set ------------------------------------------------------------
data1 <- get(data("StressAnxiety"))

# Descriptive analysis
p1 <- data1 %>% 
        ggplot(aes(x = anxiety, y = stress)) + 
        geom_point(size = 1.5, alpha = 0.3, color = "gray40") +
        xlab("Anxiety") + 
        ylab("Stress") +
        ylim(-0.01, 1.01)

p1

# Linear predictor
form1 <- stress ~ anxiety

# Fitting: betareg package ----------------------------------------------------
fit_betareg <- betareg::betareg(form1, data = data1)
summary(fit_betareg)

# Fitting: TMB package --------------------------------------------------------
# Compilation
TMB::compile("Beta_Univariate.cpp")
dyn.load(TMB::dynlib("Beta_Univariate"))

# Model matrix
X <- model.matrix(~ anxiety, data = data1)
# Response variable
Y <- data1$stress

# Data set 
data <- list(Y = Y, X = X)

# Regression coefficients
betas <- as.numeric(coef(fit_betareg)[-3])

# Initial values
parameters <- list(beta = betas, logphi = 1)

# Log-likelihood function
Beta_TMB <- TMB::MakeADFun(data, parameters, DLL = "Beta_Univariate", 
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

# Writing the log-likelihood function

# Compilation
TMB::compile("Beta_Univariate2.cpp")
dyn.load(TMB::dynlib("Beta_Univariate2"))

# Initial values
parameters <- list(beta = betas, logphi = 1)

# Log-likelihood function
Beta_TMB2 <- TMB::MakeADFun(data, parameters, DLL = "Beta_Univariate2", 
                            hessian = TRUE, silent = TRUE)
# Fitting via nlminb() --------------------------------------------------------
fit_TMB2 <- nlminb(start = Beta_TMB2$par, objective = Beta_TMB2$fn, gradient = Beta_TMB2$gr)
fit_TMB2

# Report
report2 <- TMB::sdreport(Beta_TMB2)

# Summary for fixed effects
summary(report2, "fixed", p.value = TRUE)

# Comparing coefficients
tab <- rbind("betareg" = coef(fit_betareg), 
             "TMB dbeta" = c(fit_TMB$par[1:2], exp(fit_TMB$par[3])),
             "TMB log-lik" = c(fit_TMB2$par[1:2], exp(fit_TMB2$par[3])))
tab

# LogLik 
tab2 <- rbind("betareg" = logLik(fit_betareg), "TMB dbeta" = abs(fit_TMB$objective), 
              "TMB log-lik" = abs(fit_TMB2$objective))
colnames(tab2) <- "Log-likelihood"
tab2

# Profile
# Preparing
par(mfrow = c(1,3), mar=c(2.8, 2.6, 1.2, 0.5), mgp = c(1.6, 0.6, 0))

# beta 0
beta0 <- tmbprofile(Beta_TMB, 1, trace = F)
plot(beta0)

# beta 1
beta1 <- tmbprofile(Beta_TMB, 2, trace = F)
plot(beta1)

# logphi
logphi <- tmbprofile(Beta_TMB, 3, trace = F)
plot(logphi)

# tmbstan package
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = T)
fit_TMB_Stan <- tmbstan::tmbstan(Beta_TMB, silent = F, laplace = F, chains = 3)
fit_TMB_Stan

# Plots
plot(fit_TMB_Stan, pars = names(Beta_TMB$par))

# Traceplot
trace <- traceplot(fit_TMB_Stan, pars = names(Beta_TMB$par))
trace + scale_color_discrete() + theme(legend.position = "top")
# END -------------------------------------------------------------------------