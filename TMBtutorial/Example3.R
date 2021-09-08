# Tutorial: Template Model Builder (TMB) R package ----------------------------
# Example 3: Multivariate GLMM for continuous bounded outcomes ----------------
# Body fat percentage data set ------------------------------------------------
# Authors: Henrique Laureano, Ricardo Petterle and Wagner Bonat LEG/UFPR ------
# Date: 08/09/2021 ------------------------------------------------------------

rm(list=ls())

# Loading extra packages
library(knitr)
library(TMB)
library(betareg)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggcorrplot)

# Loading data set ------------------------------------------------------------
data3 <- read.table("Data_Set.csv", header = TRUE, sep = ",", na.strings = "")
data3$ARMS <- data3$ARMS/100
data3$LEGS <- data3$LEGS/100
data3$TRUNK <- data3$TRUNK/100
data3$ANDROID <- data3$ANDROID/100
data3$GYNOID <- data3$GYNOID/100
data3$SEX <- as.factor(data3$SEX)
data3$IPAQ <- as.factor(data3$IPAQ)

# Preparing data set 
# SEX 
data3$SEX <- factor(data3$SEX)
levels(data3$SEX) <- c("Female", "Male")

# IPAQ
data3$IPAQ <- factor(data3$IPAQ)
levels(data3$IPAQ) <- c("Sedentary", "Insufficiently active", "Active")
tail(data3, n = 10)

# Descriptive analysis
summary(data3)

# Preparing
par(mfrow = c(4,5), mar=c(2.8, 2.6, 1.2, 0.5), mgp = c(1.6, 0.6, 0))

# cex (theme_cowplot)
fig_cex = 12

# Age
p1 <- data3 %>% 
  ggplot(aes(x = AGE, y = ARMS)) + 
  geom_point(size = 1.1, alpha = 0.8, color = "gray80") +
  xlab("Age (years)") +  
  ylab("Arms") +
  geom_smooth(method = lm, color = "gray70", 
              formula = y ~ splines::bs(x, 3), 
              se = T) +
  theme_cowplot(fig_cex)

p2 <- data3 %>% 
  ggplot(aes(x = AGE, y = LEGS)) + 
  geom_point(size = 1.1, alpha = 0.8, color = "gray80") +
  xlab("Age (years)") +  
  ylab("Legs") +
  geom_smooth(method = lm, color = "gray70", 
              formula = y ~ splines::bs(x, 3), 
              se = T) +
  theme_cowplot(fig_cex)

p3 <- data3 %>% 
  ggplot(aes(x = AGE, y = TRUNK)) + 
  geom_point(size = 1.1, alpha = 0.8, color = "gray80") +
  xlab("Age (years)") +  
  ylab("Trunk") +
  geom_smooth(method = lm, color = "gray70", 
              formula = y ~ splines::bs(x, 3), 
              se = T) +
  theme_cowplot(fig_cex)

p4 <- data3 %>% 
  ggplot(aes(x = AGE, y = ANDROID)) + 
  geom_point(size = 1.1, alpha = 0.8, color = "gray80") +
  xlab("Age (years)") +
  ylab("Android") +
  geom_smooth(method = lm, color = "gray70", 
              formula = y ~ splines::bs(x, 3), 
              se = T) +
  theme_cowplot(fig_cex)

p5 <- data3 %>% 
  ggplot(aes(x = AGE, y = GYNOID)) + 
  geom_point(size = 1.1, alpha = 0.8, color = "gray80") +
  xlab("Age (years)") +  
  ylab("Gynoid") +
  geom_smooth(method = lm, color = "gray70", 
              formula = y ~ splines::bs(x, 3), 
              se = T) +
  theme_cowplot(fig_cex)

# BMI
p6 <- data3 %>% 
  ggplot(aes(x = BMI, y = ARMS)) + 
  geom_point(size = 1.1, alpha = 0.8, color = "gray80") +
  xlab(expression(paste("BMI (kg/m"^2,")"))) + 
  ylab("Arms") +
  geom_smooth(method = lm, color = "gray70", 
              formula = y ~ splines::bs(x, 3), 
              se = T) +
  theme_cowplot(fig_cex)

p7 <- data3 %>% 
  ggplot(aes(x = BMI, y = LEGS)) + 
  geom_point(size = 1.1, alpha = 0.8, color = "gray80") +
  xlab(expression(paste("BMI (kg/m"^2,")"))) + 
  ylab("Legs") +
  geom_smooth(method = lm, color = "gray70", 
              formula = y ~ splines::bs(x, 3), 
              se = T) +
  theme_cowplot(fig_cex)

p8 <- data3 %>% 
  ggplot(aes(x = BMI, y = TRUNK)) + 
  geom_point(size = 1.1, alpha = 0.8, color = "gray80") +
  xlab(expression(paste("BMI (kg/m"^2,")"))) + 
  ylab("Trunk") +
  geom_smooth(method = lm, color = "gray70", 
              formula = y ~ splines::bs(x, 3), 
              se = T) +
  theme_cowplot(fig_cex)

p9 <- data3 %>% 
  ggplot(aes(x = BMI, y = ANDROID)) + 
  geom_point(size = 1.1, alpha = 0.8, color = "gray80") +
  xlab(expression(paste("BMI (kg/m"^2,")"))) + 
  ylab("Android") +
  geom_smooth(method = lm, color = "gray70", 
              formula = y ~ splines::bs(x, 3), 
              se = T) +
  theme_cowplot(fig_cex)

p10 <- data3 %>% 
  ggplot(aes(x = BMI, y = GYNOID)) + 
  geom_point(size = 1.1, alpha = 0.8, color = "gray80") +
  xlab(expression(paste("BMI (kg/m"^2,")"))) + 
  ylab("Gynoid") +
  geom_smooth(method = lm, color = "gray70", 
              formula = y ~ splines::bs(x, 3), 
              se = T) +
  theme_cowplot(fig_cex)

# Gender
p11 <- data3 %>% 
  ggplot(aes(x = SEX, y = ARMS)) +
  geom_boxplot(fill = "gray80") +
  xlab("Gender") + 
  ylab("Arms") +
  theme_cowplot(fig_cex)

p12 <- data3 %>% 
  ggplot(aes(x = SEX, y = LEGS)) +
  geom_boxplot(fill = "gray80") +
  xlab("Gender") + 
  ylab("Legs") +
  theme_cowplot(fig_cex)

p13 <- data3 %>% 
  ggplot(aes(x = SEX, y = TRUNK)) +
  geom_boxplot(fill = "gray80") +
  xlab("Gender") + 
  ylab("Trunk") +
  theme_cowplot(fig_cex)

p14 <- data3 %>% 
  ggplot(aes(x = SEX, y = ANDROID)) +
  geom_boxplot(fill = "gray80") +
  xlab("Gender") + 
  ylab("Android") +
  theme_cowplot(fig_cex)

p15 <- data3 %>% 
  ggplot(aes(x = SEX, y = GYNOID)) +
  geom_boxplot(fill = "gray80") +
  xlab("Gender") + 
  ylab("Gynoid") +
  theme_cowplot(fig_cex)

# IPAQ
data3$IPAQ <- factor(data3$IPAQ)
levels(data3$IPAQ) <- c("S", "IA", "A")

p16 <- data3 %>% 
  ggplot(aes(x = IPAQ, y = ARMS)) +
  geom_boxplot(fill = "gray80") +
  xlab("IPAQ") + 
  ylab("Arms") +
  theme_cowplot(fig_cex)

p17 <- data3 %>% 
  ggplot(aes(x = IPAQ, y = LEGS)) +
  geom_boxplot(fill = "gray80") +
  xlab("IPAQ") + 
  ylab("Legs") +
  theme_cowplot(fig_cex)

p18 <- data3 %>% 
  ggplot(aes(x = IPAQ, y = TRUNK)) +
  geom_boxplot(fill = "gray80") +
  xlab("IPAQ") + 
  ylab("Trunk") +
  theme_cowplot(fig_cex)

p19 <- data3 %>% 
  ggplot(aes(x = IPAQ, y = ANDROID)) +
  geom_boxplot(fill = "gray80") +
  xlab("IPAQ") + 
  ylab("Android") +
  theme_cowplot(fig_cex)

p20 <- data3 %>% 
  ggplot(aes(x = IPAQ, y = GYNOID)) +
  geom_boxplot(fill = "gray80") +
  xlab("IPAQ") + 
  ylab("Gynoid") +
  theme_cowplot(fig_cex)

# Figure
plot_grid(p1, p2, p3, p4, p5, 
          p6, p7, p8, p9, p10,
          p11, p12, p13, p14, p15,
          p16, p17, p18, p19, p20,
          label_size = 13,
          labels = c("(A)", "(B)", "(C)", "(D)", "(E)", 
                     "(F)", "(G)", "(H)", "(I)", "(J)",
                     "(K)", "(L)", "(M)", "(N)", "(O)",
                     "(P)", "(Q)", "(R)", "(S)", "(T)"), 
          hjust = -0.05, vjust = 1,
          ncol = 5, nrow = 4)

# Univariate beta regression model 
# Response 1
fit1 <- betareg::betareg(ARMS ~ AGE + BMI + SEX + IPAQ, 
                         data = data3)
# Response 2
fit2 <- betareg::betareg(LEGS ~ AGE + BMI + SEX + IPAQ, 
                         data = data3)
# Response 3 
fit3 <- betareg::betareg(TRUNK ~ AGE + BMI + SEX + IPAQ, 
                         data = data3)
# Response 4
fit4 <- betareg::betareg(ANDROID ~ AGE + BMI + SEX + IPAQ, 
                         data = data3)
# Response 5
fit5 <- betareg::betareg(GYNOID ~ AGE + BMI + SEX + IPAQ, 
                         data = data3)

# Initial values from BoundedReg
beta1 = as.numeric(coef(fit1)[-7])
beta2 = as.numeric(coef(fit2)[-7])
beta3 = as.numeric(coef(fit3)[-7])
beta4 = as.numeric(coef(fit4)[-7])
beta5 = as.numeric(coef(fit5)[-7])

# Precision
phi1 <- log(as.numeric(coef(fit1)[7]))
phi2 <- log(as.numeric(coef(fit2)[7]))
phi3 <- log(as.numeric(coef(fit3)[7]))
phi4 <- log(as.numeric(coef(fit4)[7]))
phi5 <- log(as.numeric(coef(fit5)[7]))

# Multivariate Beta model full ------------------------------------------------
compile("MGLMM_Beta.cpp")
dyn.load(dynlib("MGLMM_Beta"))

# Data 
X <- model.matrix(~ AGE + BMI + SEX + IPAQ, data3)
data <- list(Y1 = data3$ARMS, Y2 = data3$LEGS, Y3 = data3$TRUNK, 
             Y4 = data3$ANDROID, Y5 = data3$GYNOID,  X = X)

## Initial values
parameters <- list(beta1 = beta1, beta2 = beta2, beta3 = beta3,
                   beta4 = beta4, beta5 = beta5, phi = c(phi1, phi2, phi3, phi4, phi5),
                   U = matrix(0, ncol = 5, nrow = 298),
                   rho = rep(0,10), sigma = rep(0.11, 5))

## Log-likelihood function
MGLMM_Beta <- MakeADFun(data, parameters, DLL = "MGLMM_Beta", 
                        random = "U", 
                        hessian = TRUE, silent = F)

# Fitting via nlminb() --------------------------------------------------------
fit_mglmm <- nlminb(start = MGLMM_Beta$par, objective = MGLMM_Beta$fn, 
                   gradient = MGLMM_Beta$gr, control = list(eval.max = 1000, 
                                                            iter.max = 1000, 
                                                            abs.tol = 1e-04, 
                                                            rel.tol = 1e-04))
fit_mglmm

# Report
report <- sdreport(MGLMM_Beta)

# Summary for the fixed effects
summary(report, "fixed", p.value = T)

# Correlation matrix
Beta_report <- MGLMM_Beta$report()
COR_Beta <- Beta_report$Cor
colnames(COR_Beta) <- rownames(COR_Beta) <- c("Arms", "Legs", "Trunk", 
                                              "Android", "Gynoid")
COR_Beta

# Plot correlation matrix
ggcorrplot::ggcorrplot(COR_Beta, hc.order = F, type = "lower",
                       lab = TRUE, show.legend = F, lab_size = 6, tl.cex = 16,
                       ggtheme = ggplot2::theme_light(),
                       colors = c("gray60", "gray60", "gray60"))
# END -------------------------------------------------------------------------