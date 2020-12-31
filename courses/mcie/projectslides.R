## ---- include=FALSE------------------------------------------------------
library(knitr)

knit_hooks$set(rmdsize = function(before, options, envir) {
    if (before) "\\footnotesize"
    else "\\normalsize"
})

opts_chunk$set(rmdsize = TRUE,
               warning = FALSE,
               cache = TRUE,
               cache_path = "multinom_cache/")


## ------------------------------------------------------------------------
probs <- c(.2, .6, .2) # probabilities for each category k
library(Matrix)
data_generator <- function(n, k = 3, p) {
    data <- t(rmultinom(n, 1, prob = p))
    colnames(data) <- paste0("p", seq(k))
    return(Matrix(data))
}
set.seed(1993)
data <- data_generator(n = 100, p = probs)
data[seq(5), ]


## ------------------------------------------------------------------------
multinom_lkl <- function(par, xs) {
    N <- nrow(xs) ; k <- ncol(xs)
    ## k-1, since the last parameter is taken as the complementary
    for (i in 1:(k - 1)) assign(paste0("p", i), par[i])
    p <- unlist(mget(c(ls(pattern = glob2rx("^p\\d")))))
    ps <- c(p, 1 - sum(p))
    lkl_p <- rep(1, N) %*% xs %*% log(ps)
    ## normalizing constant
    lkl_c1 <- sum(lfactorial(rowSums(xs)))
    lkl_c2 <- sum(lfactorial(xs))
    lkl <- lkl_c1 - lkl_c2 + lkl_p
    ## returning the negative of it
    return(-as.numeric(lkl))
}


## ------------------------------------------------------------------------
initial_values <- c(1/3, 1/3)
library(bbmle)
names(initial_values) <- parnames(multinom_lkl) <- c("p1", "p2")
model_fit <- mle2(multinom_lkl,
                  start = initial_values, vecpar = TRUE,
                  data = list(xs = data))
## estimated probabilities
c(model_fit@coef, p3 = 1 - sum(model_fit@coef))


## ------------------------------------------------------------------------
colSums(data)/nrow(data)


## ------------------------------------------------------------------------
multinom_deviance <- function(p1, p2) {
    par <- c(p1, p2)
    log_ratio <- multinom_lkl(par, xs = data) - model_fit@min
    deviance <- 2 * (log_ratio)
    return(deviance)
}
multinom_deviance <- Vectorize(multinom_deviance, c("p1", "p2"))


## ---- echo=FALSE---------------------------------------------------------
library(ggplot2)
library(fishualize)
multinom_contour <- function(p1min = .1, p1max = .375,
                             p2min = .425, p2max = .75,
                             deviance_fn, taylor = NULL,
                             alpha_deviance = 1, alpha_taylor = 1,
                             contour_levels, zoom = 18) {
    p1_grid <- seq(p1min, p1max, length.out = 50)
    p2_grid <- seq(p2min, p2max, length.out = 50)
    deviance_outer <- outer(p1_grid, p2_grid, deviance_fn)
    rownames(deviance_outer) <- p1_grid
    colnames(deviance_outer) <- p2_grid
    deviance_outer <- reshape2::melt(deviance_outer)
    deviance_contour <-
        ggplot() +
        stat_contour(data = deviance_outer,
                     aes(Var1, Var2, z = value, fill = ..level..),
                     geom = "polygon",
                     breaks = qchisq(contour_levels, df = 2),
                     alpha = alpha_deviance) +
        guides(fill = "none") +
        scale_fill_fish(option = "Trimma_lantana", direction = -1) +
        labs(x = expression(p[1]), y = expression(p[2])) +
        geom_vline(xintercept = model_fit@coef[1], col = "white") +
        geom_hline(yintercept = model_fit@coef[2], col = "white") +
        theme_minimal(base_size = zoom)
    if (alpha_deviance != 1) {
        taylor_outer <- outer(p1_grid, p2_grid, taylor)
        rownames(taylor_outer) <- p1_grid
        colnames(taylor_outer) <- p2_grid
        taylor_outer <- reshape2::melt(taylor_outer)
        deviance_contour <-
            deviance_contour +
            stat_contour(data = taylor_outer,
                         aes(Var1, Var2, z = value),
                         breaks = qchisq(contour_levels, df = 2),
                         col = "black")
    }
    return(deviance_contour)
}
contour_levels <- c(.99, .95, .9, .8, .7, .5, .3, .1, .05, .01)
multinom_contour(deviance_fn = multinom_deviance,
                 contour_levels = contour_levels)


## ---- echo=FALSE, fig.height=5.25----------------------------------------
multinom_taylor <- function(par, xs) {
    n <- nrow(xs)
    par_est <- model_fit@coef
    off_diag <- n/(1 - sum(par_est))
    Ie <- Matrix(c(
        n/par_est[1] + off_diag, rep(off_diag, 2), n/par_est[2] + off_diag
    ), 2)
    par_res <- par - par_est
    quad_approx <- - model_fit@min - .5 * par_res %*% Ie %*% par_res
    ## returning the negative, since we do the same with the lkl
    return(-quad_approx)
}
deviance_taylor <- function(p1, p2) {
    par <- c(p1, p2)
    log_ratio <- as.numeric(multinom_taylor(par, xs = data) -
                            multinom_taylor(model_fit@coef, xs = data))
    deviance <- 2 * (log_ratio)
    return(deviance)
}
deviance_taylor <- Vectorize(deviance_taylor, c("p1", "p2"))
library(gridExtra)
grid.arrange(
    multinom_contour(
        deviance_fn = multinom_deviance, taylor = deviance_taylor,
        alpha_deviance = .9, contour_levels = contour_levels,
        zoom = 16) + labs(tag = "(B)"),
    multinom_contour(
        p1min = .215, p1max = .245, p2min = .57, p2max = .61,
        deviance_fn = multinom_deviance, taylor = deviance_taylor,
        alpha_deviance = .5, contour_levels = c(.95, .05, .01),
        zoom = 16) + labs(tag = "(S)",
                          title = "Regions with highest deviances"),
    ncol = 2)


## ---- echo=FALSE---------------------------------------------------------
model_fit@coef[1] +
    c(qnorm(.025), qnorm(.975)) * sqrt(model_fit@vcov[1, 1])


## ---- echo=FALSE---------------------------------------------------------
model_fit@coef[2] +
    c(qnorm(.025), qnorm(.975)) * sqrt(model_fit@vcov[2, 2])


## ---- echo=FALSE---------------------------------------------------------
1 - sum(model_fit@coef) +
    c(qnorm(.025), qnorm(.975)) *
    c(sqrt(c(-1, -1) %*% model_fit@vcov %*% c(-1, -1)))


## ---- echo=FALSE, fig.height=4.75----------------------------------------
library(rootSolve)
my_profile <- function(par, range_95, range_99, xlab, mle, xpos) {
    data <- as.data.frame(profile(model_fit, which = par))
    p1_95 <- uniroot.all(
        approxfun(x = data$focal, y = abs(data$z) - 2), range_95)
    p1_99 <- uniroot.all(
        approxfun(x = data$focal, y = abs(data$z) - 2.5), range_99)
    databs <- subset(data, abs(z) < 3)
    ggplot() +
        geom_line(data = databs, aes(x = focal, y = abs(z)), size = 1) +
        geom_point(data = databs, aes(x = focal, y = abs(z)), size = 2) +
        theme_minimal(base_size = 16) +
        labs(x = xlab) +
        geom_vline(xintercept = mle, linetype = "dashed") +
        geom_segment(data = data.frame(x = c(p1_95, p1_99),
                                       y = rep(c(2, 2.5), each = 2),
                                       xend = c(p1_95, p1_99),
                                       yend = rep(0, 4)),
                     aes(x = x, y = y, xend = xend, yend = yend),
                     color = "#0080ff", linetype = "dashed") +
        geom_segment(data = data.frame(x = c(p1_95[1], p1_99[1]),
                                       y = c(2, 2.5),
                                       xend = c(p1_95[2], p1_99[2]),
                                       yend = c(2, 2.5)),
                     aes(x = x, y = y, xend = xend, yend = yend),
                     color = "#0080ff", linetype = "dashed") +
        annotate("text", x = xpos, y = 2.2, label = "95%",
                 color = "#0080ff", size = 5) +
        annotate("text", x = xpos, y = 2.7, label = "99%",
                 color = "#0080ff", size = 5)
}
profile_p1 <- my_profile("p1",
                         range_95 = c(.1, .4), range_99 = c(.1, .4),
                         xlab = expression(p[1]),
                         mle = model_fit@coef[1], xpos = .25)
profile_p2 <- my_profile("p2",
                         range_95 = c(.45, .75), range_99 = c(.45, .75),
                         xlab = expression(p[2]),
                         mle = model_fit@coef[2], xpos = .57)
grid.arrange(profile_p1, profile_p2, ncol = 2)


## ------------------------------------------------------------------------
inv_logit <- function(coefs, preds, data) {
    ## building objects
    ind_pred <- seq(preds) ; n_pred <- length(preds) ; k <- n_pred + 1
    betas <- split(coefs, substr(names(coefs), start = 3, stop = 3))
    list_X <- lapply(preds, model.matrix, data = data)
    ## computing the important stuff
    exp_xb <- sapply(seq(list_X),
                     function(i) exp(list_X[[i]] %*% betas[[i]]))
    link_denominator <- 1 + rowSums(exp_xb)
    ps <- sapply(ind_pred, function(i) exp_xb[ , i]/link_denominator)
    ps <- cbind(ps, 1 - rowSums(ps))
    colnames(ps) <- paste0("p", seq(k))
    return(list(y = data[ , seq(k)], ps = ps))
}


## ------------------------------------------------------------------------
n <- 100 ; k <- 3 # defining sample size and number of categories
naive_data <- function(n, k) {
    data <- as.data.frame(matrix(0, nrow = n, ncol = k))
    names(data) <- paste0("y", 1:k)
    return(data)
}
data <- naive_data(n, k)
set.seed(0080)
data$x1 <- rnorm(n, mean = 5, sd = 1) # covariates
data$x2 <- rnorm(n, mean = 1, sd = .5)
linear_pred <- list(y1 ~ x1 + x2, y2 ~ x1 + x2)
initial_values <- c("b01" = .4, "b11" = .1, "b21" = - .3,
                    "b02" = .2, "b12" = .5, "b22" = - .6)
probs <- inv_logit(initial_values, preds = linear_pred, data = data)$ps


## ------------------------------------------------------------------------
summary(probs)
library(mc2d) # vectorized versions of {r, d}multinom
## finally, simulating the multinomial data
set.seed(1101)
data[ , seq(k)] <- rmultinomial(n, 1, prob = probs)
colSums(data[ , seq(k)]) # as expected, close to the ``probs`` means


## ------------------------------------------------------------------------
multi_lkl <- function(initial_values, linear_pred, data) {
    ilogit <- inv_logit(initial_values, preds = linear_pred, data = data)
    lkl_c1 <- with(ilogit, sum(lfactorial(rowSums(y))))
    lkl_c2 <- with(ilogit, sum(lfactorial(y)))
    lkl_p <- with(ilogit, sum(y * log(ps)))
    lkl <- lkl_c1 - lkl_c2 + lkl_p
    ## lkl <- sum(dmultinomial(as.matrix(ilogit$y), size = 1,
    ##                         prob = ilogit$ps, log = TRUE))
    return(-lkl)
}
parnames(multi_lkl) <- names(initial_values) # mle2 exigency
model_fit <- mle2(multi_lkl, start = initial_values,
                  data = list(linear_pred = linear_pred, data = data))
round(model_fit@coef, 6)


## ---- echo=FALSE---------------------------------------------------------
profile_b01 <- my_profile("b01",
                          range_95 = c(-10, 10), range_99 = c(-10, 10),
                          xlab = expression(beta[0[1]]),
                          mle = model_fit@coef[1], xpos = 6) +
    geom_vline(xintercept = initial_values[1],
               linetype = "dashed", col = "red")
## ---------------------------------------------------------------------
profile_b11 <- my_profile("b11",
                          range_95 = c(-10, 10), range_99 = c(-10, 10),
                          xlab = expression(beta[1[1]]),
                          mle = model_fit@coef[2], xpos = .5) +
    geom_vline(xintercept = initial_values[2],
               linetype = "dashed", col = "red")
## ---------------------------------------------------------------------
profile_b21 <- my_profile("b21",
                          range_95 = c(-10, 10), range_99 = c(-10, 10),
                          xlab = expression(beta[2[1]]),
                          mle = model_fit@coef[3], xpos = -3.5) +
    geom_vline(xintercept = initial_values[3],
               linetype = "dashed", col = "red")
## ---------------------------------------------------------------------
profile_b02 <- my_profile("b02",
                          range_95 = c(-10, 10), range_99 = c(-10, 10),
                          xlab = expression(beta[0[2]]),
                          mle = model_fit@coef[4], xpos = 3) +
    geom_vline(xintercept = initial_values[4],
               linetype = "dashed", col = "red")
## ---------------------------------------------------------------------
profile_b12 <- my_profile("b12",
                          range_95 = c(-10, 10), range_99 = c(-10, 10),
                          xlab = expression(beta[1[2]]),
                          mle = model_fit@coef[5], xpos = 1.25) +
    geom_vline(xintercept = initial_values[5],
               linetype = "dashed", col = "red")
## ---------------------------------------------------------------------
profile_b22 <- my_profile("b22",
                          range_95 = c(-10, 10), range_99 = c(-10, 10),
                          xlab = expression(beta[2[2]]),
                          mle = model_fit@coef[6], xpos = -2.5) +
    geom_vline(xintercept = initial_values[6],
               linetype = "dashed", col = "red")
## ---------------------------------------------------------------------
grid.arrange(profile_b01, profile_b11, profile_b21,
             profile_b02, profile_b12, profile_b22, nrow = 2)


## ------------------------------------------------------------------------
model_fit@coef

## ---- echo=FALSE---------------------------------------------------------
data_long <- reshape2::melt(data, measure.vars = seq(k), variable.name = "y")
data_long <- data_long[data_long$value == 1, ]
data_long$y <- relevel(data_long$y, ref = k) # changing the ref. level

## ---- results="hide"-----------------------------------------------------
fit_nnet <- nnet::multinom(y ~ x1 + x2, data_long)

## ------------------------------------------------------------------------
coef(fit_nnet)

## ------------------------------------------------------------------------
fit_mlogit <- mlogit::mlogit(y ~ 0 | x1 + x2,
                             reflevel = 3, shape = "wide", data_long)
matrix(fit_mlogit$coef, nrow = 2, ncol = 3)


## ------------------------------------------------------------------------
results <- lapply(
    seq(k),
    function(i) cbind(inv_logit(
                    model_fit@coef, linear_pred, data)$ps[ , i],
                    probs[ , i]))
results <- do.call(cbind, results)
colnames(results) <- c(sapply(seq(k),
                              function(i) c(paste0("p", i, "_est"),
                                            paste0("p", i, "_true"))))
head(round(results, 6), 8)


## ------------------------------------------------------------------------
wald <- function(par, value, alpha) {
    mle <- as.numeric(model_fit@coef[par])
    test <- (mle - value)/sqrt(model_fit@vcov[par, par])
    critic <- qnorm(1 - alpha/2)
    print(ifelse(test <= critic, "Accept H0", "Reject H0"))
    return(c("Test" = test, "Critical_value" = critic))
}
wald(par = "b11", value = 0, alpha = .05)


## ------------------------------------------------------------------------
lrt <- function(model_h0, model_h1, alpha) {
    test <- 2 * (model_h0@min - model_h1@min)
    critic <- qchisq(1 - alpha, df = 1)
    print(ifelse(test <= critic, "Accept H0", "Reject H0"))
    return(c("Test" = test, "Critical_value" = critic))
}

## ---- echo=FALSE---------------------------------------------------------
multi_lkl2 <- multi_lkl

## ------------------------------------------------------------------------
test_values <- c("b01" = .4,             "b21" = - .3,
                 "b02" = .2, "b12" = .5, "b22" = - .6)

## ---- echo=FALSE---------------------------------------------------------
parnames(multi_lkl2) <- names(test_values)

## ------------------------------------------------------------------------
lrt(model_h0 = mle2(multi_lkl2, start = test_values,
                    data = list(
                        linear_pred = list(y1 ~ x2, y2 ~ x1 + x2),
                        data = data, "b11" = 0)),
    model_h1 = model_fit, alpha = .05)


## ---- echo=FALSE---------------------------------------------------------
model_h0 <- mle2(multi_lkl2, start = test_values,
                 data = list(linear_pred = list(y1 ~ x2, y2 ~ x1 + x2),
                             data = data, "b11" = 0))

coef_h0 <- numeric(length(model_fit@coef))
names(coef_h0) <- names(model_fit@coef)
coef_h0[names(model_h0@coef)] <- model_h0@coef

U <- numDeriv::grad(multi_lkl, x = coef_h0,
                    linear_pred = linear_pred, data = data)
Ie <- numDeriv::hessian(multi_lkl, x = coef_h0,
                        linear_pred = linear_pred, data = data)
names(U) <- colnames(Ie) <- rownames(Ie) <- names(model_fit@coef)

par <- "b11"
parpos <- which(colnames(Ie) == par)
old_order <- colnames(Ie)
new_order <- c(old_order[parpos], old_order[-parpos])
ordered_Ie <- Ie
if (parpos != 1) {
    ordered_Ie[c(1, parpos), ] <- ordered_Ie[c(parpos, 1), ]
    ordered_Ie[ , c(1, parpos)] <- ordered_Ie[ , c(parpos, 1)]
    colnames(ordered_Ie) <- rownames(ordered_Ie) <- new_order
}

## ------------------------------------------------------------------------
score <- function(par, ordered_Ie, alpha) {
    parpos <- which(colnames(ordered_Ie) == par)
    n <- ncol(ordered_Ie)
    I12 <- ordered_Ie[parpos, (parpos + 1):n]
    Vc <- ordered_Ie[parpos, parpos] -
        I12 %*% solve(ordered_Ie[-parpos, -parpos]) %*% I12
    test <- as.numeric(U[par] * 1/Vc * U[par])
    critic <- qchisq(1 - alpha, df = 1)
    print(ifelse(test <= critic, "Accept H0", "Reject H0"))
    return(c("Test" = test, "Critical_value" = critic))
}
score(par = "b11", ordered_Ie, alpha = .05)


## ---- results="hide"-----------------------------------------------------
library(furrr)
plan(multiprocess)
parallel2run <- function(nsimu) {
    coefs <- Matrix(0, nrow = nsimu, ncol = 6)
    confint_profile <- confint_quad <- Matrix(0, nrow = nsimu, ncol = 12)
    data[ , seq(k)] <- rmultinomial(n, 1, prob = probs)
    fit <- try(mle2(multi_lkl, start = initial_values,
                    data = list(linear_pred = linear_pred, data = data)))
    if (class(fit) != "try-error") {
        coefs[nsimu, ] <- as.numeric(fit@coef)
        confint_profile[nsimu, ] <- c(confint(fit))
        confint_quad[nsimu, ] <- c(confint(fit, method = "quad"))
    }
    return(list(coefs = coefs, confint_profile = confint_profile,
                confint_quad = confint_quad))
}
nsimu <- 500 ; simu <- vector("list", nsimu)
simu <- future_map(rep(1, nsimu), parallel2run)


## ---- echo=FALSE, fig.height=6.75----------------------------------------
fish_hist <- fish(6, option = "Trimma_lantana")

coefs <- Matrix(0, nrow = nsimu, ncol = 6)
coefs <- t(sapply(seq(nsimu), function(i) as.numeric(simu[[i]]$coefs)))

coef_hist <- function(ncol, xlab) {
    ggplot() +
        scale_y_continuous(labels = NULL) +
        theme_minimal(base_size = 15) +
        geom_histogram(
            aes(coefs[ , ncol]), bins = 20,
            col = fish_hist[6], fill = fish_hist[ncol], alpha = .5) +
        geom_vline(xintercept = c(mean(coefs[ , ncol], na.rm = TRUE),
                                  initial_values[ncol],
                                  model_fit@coef[ncol]),
                   linetype = "dashed", col = c(1, 2, "#0080ff"), size = 1) +
        labs(x = xlab, y = NULL)
}
p1 <- coef_hist(ncol = 1, xlab = expression(beta[0[1]]))
p2 <- coef_hist(ncol = 2, xlab = expression(beta[1[1]]))
p3 <- coef_hist(ncol = 3, xlab = expression(beta[2[1]]))
p4 <- coef_hist(ncol = 4, xlab = expression(beta[0[2]]))
p5 <- coef_hist(ncol = 5, xlab = expression(beta[1[2]]))
p6 <- coef_hist(ncol = 6, xlab = expression(beta[2[2]]))
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)


## ------------------------------------------------------------------------
coverage <- function(method) {
    coverage <- Matrix(0, nrow = 6, ncol = nsimu)
    for (i in seq(6))
        for (j in seq(nsimu))
            coverage[i, j] <-
                initial_values[i] >= simu[[j]][[method]][i] &
                initial_values[i] <= simu[[j]][[method]][i + 6]
    means <- rowMeans(coverage, na.rm = TRUE)
    names(means) <- names(initial_values)
    return(round(means, 3))}
coverage(method = 2) # deviance interval
coverage(method = 3) # wald interval


## ---- echo=FALSE---------------------------------------------------------
inv_logit <- function(coefs, preds, bigdata) {
    ind_pred <- seq(preds) ; n_pred <- length(preds) ; k <- n_pred + 1
    alpha <- bigdata[ , paste0("alpha", ind_pred)]
    betas <- split(coefs, substr(names(coefs), start = 3, stop = 3))
    list_X <- lapply(preds, model.matrix, data = bigdata)
    exp_xb <-
        sapply(
            seq(list_X),
            function(i) exp(list_X[[i]] %*% betas[[i]] + alpha[ , i]))
    link_denominator <- 1 + rowSums(exp_xb)
    ps <- sapply(ind_pred, function(i) exp_xb[ , i]/link_denominator)
    ps <- cbind(ps, 1 - rowSums(ps))
    colnames(ps) <- paste0("p", seq(k))
    return(list(y = bigdata[ , paste0("y", seq(k))], ps = ps))
}

## ------------------------------------------------------------------------
data_setup <- function(N, n, k, rho, var1, var2) {
    gauss_dim <- k - 1 ; off_diag <- rho * sqrt(var1) * sqrt(var2)
    set.seed(1417)
    alpha <- rmvnorm(N, mean = rep(0, gauss_dim),
                     sigma = matrix(c(var1, rep(off_diag, 2), var2),
                                    gauss_dim, gauss_dim))
    data <- as.data.frame(
        matrix(0, nrow = N * n, ncol = 2 + k + gauss_dim))
    names(data) <- c("i", "j",
                     paste0("y", seq(k)), paste0("alpha", seq(gauss_dim)))
    data$i <- rep(seq(N), each = n) ; data$j <- rep(seq(n), N)
    for (i in seq(gauss_dim)) {
        data[ , paste0("alpha", i)] <- rep(alpha[ , i], each = n)}
    return(data)
}
N = 250 ; n = 10 ; k = 3 ## subjects, repeated measures and classes
bigdata <- data_setup(N, n, k, rho = - .25, var1 = .2, var2 = .4)
linear_pred <- list(y1 ~ 1, y2 ~ 1)
initial_values <- c("b01" = .85, "b02" = 1.25)
probs <- inv_logit(initial_values, linear_pred, bigdata)$ps


## ------------------------------------------------------------------------
set.seed(0040)
bigdata[ , paste0("y", seq(k))] <- rmultinomial(N * n, 1, prob = probs)
head(bigdata, 15)

## ---- eval=FALSE, include=FALSE------------------------------------------
## set.seed(191120)
## data$x1 <- rnorm(N * n, mean = 5, sd = 1) # covariates
## data$x2 <- rnorm(N * n, mean = 1, sd = .5)
## initial_values <- c("b01" = 1,   "b11" = .5, "b21" = -1.5,
##                     "b02" = .75, "b12" = .7, "b22" = -1)


## ------------------------------------------------------------------------
library(tidyverse)
integrating <- function(alpha, beta, det_sigma, inv_sigma, preds, data) {
    ind_pred <- seq(preds) ; n_pred <- length(preds) ; k <- n_pred + 1
    beta <- split(beta, substr(names(beta), start = 3, stop = 3))
    list_X <- lapply(preds, model.matrix, data = data)
    exp_xb <- sapply(
        seq(list_X),
        function(i) exp(list_X[[i]] %*% beta[[i]] + alpha[i]))
    link_denominator <- 1 + rowSums(exp_xb)
    ps <- sapply(ind_pred, function(i) exp_xb[ , i]/link_denominator)
    ps <- cbind(ps, 1 - rowSums(ps)) ; colnames(ps) <- paste0("p", seq(k))
    y <- as.matrix(data[ , paste0("y", seq(k))]) ; n <- nrow(y)
    out <- sum(dmultinomial(y, size = 1, prob = ps, log = TRUE)) -
        (n/2) * log(2 * pi) -
        .5 * det_sigma - .5 * alpha %*% inv_sigma %*% alpha
    return(out) }


## ------------------------------------------------------------------------
laplace <- function(lkl, dim, ...) {
    integral <- -6e+05
    initial <- numeric(dim)
    logQ <- try(optim(initial, lkl, ..., method = "BFGS",
                      control = list(fnscale = -1), hessian = TRUE),
                silent = TRUE)
    if (class(logQ) != "try-error") {
        integral <- logQ$value +
            (dim/2) * log(2*pi) - .5 * determinant(-logQ$hessian)$modulus
    }
    return(integral)
}


## ---- include=FALSE------------------------------------------------------
library(dplyr)

## ------------------------------------------------------------------------
multi_mixed <- function(theta, data, until) {
    out <- -sqrt(.Machine$double.xmax)
    beta <- theta[str_detect(names(theta), pattern = "^b\\d")]
    rho <- 2 * exp(theta["rho"])/(1 + exp(theta["rho"])) - 1
    var1 <- exp(theta["var1"]) ; var2 <- exp(theta["var2"])
    c1 <- 1 - rho^2 ; off_diag <- -rho/(sqrt(var1) * sqrt(var2) * c1)
    inv_sigma <-
        matrix(c(1/(var1 * c1), rep(off_diag, 2), 1/(var2 * c1)), 2, 2)
    det_sigma <- log(var1 * var2 * c1)[[1]]
    out <- future_map_dbl(
        seq(until),
        function(index) {
            laplace(lkl = integrating, dim = 2, beta = beta,
                    det_sigma = det_sigma, inv_sigma = inv_sigma,
                    preds = linear_pred,
                    data = data[data$i == index, ]) })
    return(-sum(out)) }


## ---- include=FALSE------------------------------------------------------
theta <- c(.85, 1.25, - .25, log(.2), log(.4))
names(theta) <- c("b01", "b02", "rho", "var1", "var2")
parnames(multi_mixed) <- names(theta)

## ------------------------------------------------------------------------
plan(multiprocess)
tic() ; mle2_fit <- mle2(
            multi_mixed, start = theta, method = "BFGS",
            data = list(data = bigdata[ , seq(5)], until = N)) ; toc()
summary(mle2_fit)@coef


## ------------------------------------------------------------------------
thetasss <- theta ; mle2_est <- mle2_fit@coef

mle2_est["rho"] <- 2 * exp(mle2_est["rho"])/(1 + exp(mle2_est["rho"])) - 1
thetasss[c("var1", "var2")] <- exp(thetasss[c("var1", "var2")])
mle2_est[c("var1", "var2")] <- exp(mle2_est[c("var1", "var2")])

rbind(thetasss, mle2_est)


## ---- include=FALSE------------------------------------------------------
grad_vec <- c(
    2 * exp(mle2_fit@coef["rho"])/(1 + exp(mle2_fit@coef["rho"]))^2,
    exp(mle2_fit@coef["var1"]),
    exp(mle2_fit@coef["var2"]))

delta_method <- sqrt(grad_vec * diag(mle2_fit@vcov[3:5, 3:5]) * grad_vec)


## ------------------------------------------------------------------------
## standard errors
c(sqrt(diag(mle2_fit@vcov[1:2, 1:2])), delta_method)


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## tic() ; profiles <- profile(mle2_fit) ; toc()
## save(profiles, file = "profiles.RData")


## ---- echo=FALSE---------------------------------------------------------
profile_plot <- function(par, range, xlab, xpos) {
    data <- as.matrix(profiles@profile[[par]])
    data2 <- data[ , c("z", paste0("par.vals.", par))]
    colnames(data2) <- c("z", "focal")
    data3 <- as.data.frame(data2)
    interval_95 <- rootSolve::uniroot.all(
        approxfun(x = data3$focal, y = abs(data3$z) - 2), range)
    interval_99 <- rootSolve::uniroot.all(
        approxfun(x = data3$focal, y = abs(data3$z) - 2.5), range)
    databs <- subset(data3, abs(z) < 3)
    ggplot() +
        geom_line(data = databs, aes(x = focal, y = abs(z)), size = 1) +
        geom_point(data = databs, aes(x = focal, y = abs(z)), size = 2) +
        theme_minimal(base_size = 16) +
        labs(x = xlab) +
        ## geom_vline(xintercept = c(mle2_fit@coef[par], theta[par]),
        ##            linetype = "dashed", col = 1:2, size = .75) +
        geom_vline(xintercept = theta[par],
                   linetype = "dashed", col = 2, size = .75) +
        geom_segment(data = data.frame(
                     x = c(interval_95, interval_99),
                     y = rep(c(2, 2.5), each = 2),
                     xend = c(interval_95, interval_99),
                     yend = rep(0, 4)),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 color = "#0080ff", linetype = "dashed") +
        geom_segment(data = data.frame(
                     x = c(interval_95[1], interval_99[1]),
                     y = c(2, 2.5),
                     xend = c(interval_95[2], interval_99[2]),
                     yend = c(2, 2.5)),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 color = "#0080ff", linetype = "dashed") +
    annotate("text", x = xpos, y = 2.2, label = "95%",
             color = "#0080ff", size = 5) +
    annotate("text", x = xpos, y = 2.7, label = "99%",
             color = "#0080ff", size = 5)
}
## b01 =================================================================
prof_b01 <- profile_plot(par = "b01", range = c(0, 2),
                         xlab = expression(b[0[1]]), xpos = 1)
## b02 =================================================================
prof_b02 <- profile_plot(par = "b02", range = c(0, 2),
                         xlab = expression(b[0[2]]), xpos = 1.35)
## rho =================================================================
prof_rho <- profile_plot(par = "rho", range = c(-10, 10),
                         xlab = expression(2 * logistic(rho) - 1),
                         xpos = - 1)
## var1 ================================================================
prof_var1 <- profile_plot(par = "var1", range = c(-10, 1),
                          xlab = expression(log(sigma[1]^2)), xpos = - 1)
## var2 ================================================================
prof_var2 <- profile_plot(par = "var2", range = c(-5, 1),
                          xlab = expression(log(sigma[2]^2)),
                          xpos = - 1.25)
## plotting ============================================================
grid.arrange(prof_b01, prof_b02, prof_rho, prof_var1, prof_var2,
             nrow = 2, ncol = 3)


## ---- eval=FALSE, include=FALSE------------------------------------------
## jacob <- function(theta, until) {
##     out <- rootSolve::gradient(f = multi_mixed, x = theta, until = until,
##                                data = bigdata[ , seq(5)])
##     return(out)
## }
## Io <- function(theta, until) {
##     out <- pracma::hessian(f = multi_mixed, x0 = theta, until = until,
##                            data = bigdata[ , seq(5)])
##     return(out)
## }
## newton <- function(theta, until, tol = 1e-04, max_iter = 10) {
##     sol <- matrix(NA, ncol = length(theta), nrow = max_iter)
##     colnames(sol) <- names(theta) ; sol[1, ] <- theta
##     for (i in seq(max_iter)) {
##         grad <- as.numeric(jacob(sol[i, ], until = until))
##         io <- Io(sol[i, ], until = until)
##         ## sol[i + 1, ] = sol[i, ] - solve(io, grad)
##         L <- try(chol(-io), silent = TRUE)
##         inv_sigma <- try(chol2inv(L), silent = TRUE)
##         if (class(L) != "try-error") {
##             if (class(inv_sigma) != "try-error") {
##                 sol[i + 1, ] = sol[i, ] + inv_sigma %*% grad
##                 ## print(sol[i + 1, ])
##                 if(sum(abs(sol[i + 1, ] - sol[i, ])) < tol) break
##             } } }
##     return(sol) }
## tic() ; newton(theta = theta, until = N) ; toc()

