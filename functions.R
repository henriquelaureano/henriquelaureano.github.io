## =====================================================================
## Multinomial GLMM functions ==========================================
## author: henrique laureano
## contact: www.leg.ufpr.br/~henrique
## date: 2019-12-16
## =====================================================================

## =====================================================================
## libraries

pack <- c("mvtnorm", "mc2d", "tidyverse", "furrr", "bbmle", "tictoc")
pacman::p_load(pack, character.only = TRUE)

## =====================================================================
## inv_logit: multinomial inverse logit/logistic function,
## the heart of a multinomial regression
## args:
## -- coefs: named vector where the last digit indicates from which
## linear predictor the coeff corresponds;
## -- preds: list of linear predictors, they can be of different sizes;
## -- bigdata: data.frame, it needs the response vectors and covariates.
## return:
## -- list of two slots, a data.frame with the response vectors and a
## matrix with the probs.

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

## =====================================================================
## data_setup: create the necessary structure for the dataset
## args:
## -- N: number of subjects;
## -- n: number of repeated measures by subject, i.e., all subjects will
## have the same number of measures;
## -- k: number of multinomial classes;
## -- rho: correlation parameter of the latent effect (bivariate
## gaussian);
## -- var1 & var2: variance parameters of the latent effect (IN LOG
## SCALE).
## return:
## -- data.frame with "2 + k + gauss_dim" columns.

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

## =====================================================================
## integrating: augmented likelihood
## args:
## -- alpha: random effects vector to be integrated out;
## -- beta: named vector where the last digit indicates from which
## linear predictor the coeff corresponds;
## -- det_sigma: log-determinant of the variance-covariance matrix;
## -- inv_sigma: precision matrix;
## -- preds: list of linear predictors, they can be of different sizes;
## -- data: data.frame, it needs the response vectors and covariates.
## return:
## -- value, augmented likelihood evaluation.

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
    return(out)
}

## =====================================================================
## IN DEVEPOLMENT ======================================================

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
    return(out)
}

## =====================================================================
## =====================================================================

## =====================================================================
## laplace: laplace approximation
## args:
## -- lkl: log-likelihood function;
## -- dim: dimension in which the laplace approx. will be performed.
## return:
## -- value, laplace approx. evaluation.

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

## =====================================================================
## multi_mixed: marginal likelihood
## args:
## -- theta: named vector;
## -- data: data.frame, it needs the response vectors, covariates and a
## subject index;
## -- until: number of subjects.
## return:
## -- negative of the likelihood sum.

multi_mixed <- function(theta, data, until) {
    out <- -sqrt(.Machine$double.xmax)
    beta <- theta[str_detect(names(theta), pattern = "^b\\d")]
    rho <- 2 * exp(theta["rho"])/(1 + exp(theta["rho"])) - 1
    var1 <- exp(theta["var1"]) ; var2 <- exp(theta["var2"])
    c1 <- 1 - rho^2
    off_diag <- -rho/(sqrt(var1) * sqrt(var2) * c1)
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
    return(-sum(out))
}
