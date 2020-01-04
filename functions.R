## =====================================================================
## Multinomial GLMM functions ==========================================
## author: henrique laureano
## contact: www.leg.ufpr.br/~henrique
## date: 2019-12-31
## =====================================================================

## =====================================================================
## libraries

pack <- c("mvtnorm", "mc2d", "tidyverse", "furrr", "bbmle", "tictoc",
          "Matrix", "parallel", "bbmle", "fishualize", "latex2exp")
pacman::p_load(pack, character.only = TRUE)

## =====================================================================
## inv_logit: multinomial inverse logit/logistic function, the heart of
##            a multinomial regression
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
## -- list with two slots,
##    ["data"] data.frame with "2 + k + gauss_dim" columns;
##    ["alpha"] random effects column named matrix.

data_setup <- function(N, n, k, rho, var1, var2) {
    gauss_dim <- k - 1 ; off_diag <- rho * sqrt(var1) * sqrt(var2)
    set.seed(1417)
    alpha <- rmvnorm(N, mean = rep(0, gauss_dim),
                     sigma = matrix(c(var1, rep(off_diag, 2), var2),
                                    gauss_dim, gauss_dim))
    colnames(alpha) <- paste0("alpha", seq(gauss_dim))
    data <- as.data.frame(
        matrix(0, nrow = N * n, ncol = 2 + k + gauss_dim))
    names(data) <- c("i", "j",
                     paste0("y", seq(k)), paste0("alpha", seq(gauss_dim)))
    data$i <- rep(seq(N), each = n) ; data$j <- rep(seq(n), N)
    for (i in seq(gauss_dim)) {
        data[ , paste0("alpha", i)] <- rep(alpha[ , i], each = n)
    }
    out <- vector("list", 2) ; names(out) <- c("data", "alpha")
    out["data"][[1]] <- data
    out["alpha"][[1]] <- alpha
    return(out)
}

## =====================================================================
## integrating: augmented likelihood
## args:
## -- alpha: named vector, random effects;
## -- beta: named vector;
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
## gradHess: gradient and hessian of the augmented likelihood
##           (integrating fn)
## args:
## -- alpha: vector, random effects;
## -- preds: list of linear predictors, they can be of different sizes;
## -- ys: data.frame, it needs the response vectors and covariates;
## -- beta: named vector;
## -- inv_sigma: precision matrix.
## return:
## -- list with two slots,
##    ["change"] gradient "divided" by the hessian;
##    ["hessian"] hessian.

gradHess <- function(alpha, preds, ys, beta, inv_sigma) {
    list_y <- as.list(ys[ , str_detect(names(ys), pattern = "^y\\d")])
    list_X <- lapply(preds, model.matrix, data = ys)
    beta <- split(beta, substr(names(beta), start = 3, stop = 3))
    n_alpha <- length(alpha)
    exp_xb <- lapply(seq(n_alpha),
                     function(i) exp(list_X[[i]] %*% beta[[i]] + alpha[i]))
    denom <- 1 + Reduce("+", exp_xb)
    grad_multi <- sapply(
        seq(n_alpha),
        function(i) sum((list_y[[i]] * (1 + Reduce("+", exp_xb[-i])) -
                         Reduce("+", list_y[-i]) * exp_xb[[i]])/denom))
    grad <- t(grad_multi - alpha %*% inv_sigma)
    denom2 <- denom^2
    hess_multi <- matrix(NA, nrow = length(alpha), ncol = length(alpha))
    diag(hess_multi) <- sapply(
        seq(n_alpha),
        function(i) sum(-Reduce("+", list_y) * exp_xb[[i]] *
                        (1 + Reduce("+", exp_xb[-i]))/denom2))
    offdiag <- sum(Reduce("+", list_y) * exp_xb[[1]] * exp_xb[[-1]]/denom2)
    hess_multi[is.na(hess_multi)] <- offdiag
    hess <- hess_multi - inv_sigma
    change <- solve(hess, grad)
    out <- vector("list", 2) ; names(out) <- c("change", "hessian")
    out["change"][[1]] <- change
    out["hessian"][[1]] <- hess
    return(out)
}

## =====================================================================
## newton_raphson: optimzation routine to be used inside the laplace
##                 approximation
## args:
## -- initial: vector, random effects;
## -- preds: list of linear predictors, they can be of different sizes;
## -- ys: data.frame, it needs the response vectors and covariates;
## -- beta: named vector;
## -- det_sigma: log-determinant of the variance-covariance matrix;
## -- inv_sigma: precison matrix;
## -- max_iter: maximum number of iterations, pre-fixed at 50;
## -- tol: minimum convergence tolerance.
## return:
## -- list with three slots,
##    ["est"] estimated values;
##    ["iter"] number of iterations up to convergence;
##    ["hessian"] hessian with the estimated values.

newton_raphson <- function(initial,
                           preds, ys, beta, det_sigma, inv_sigma,
                           max_iter = 50, tol = 1e-5) {
    sol <- matrix(NA, nrow = max_iter, ncol = length(initial))
    sol[1, ] <- initial
    for (i in 2:max_iter) {
        change <- gradHess(alpha = initial,
                           preds = preds, ys = ys,
                           beta = beta, inv_sigma = inv_sigma)
        sol[i, ] <- initial - change$change
        initial <- sol[i, ]
        tol_iter <- abs(sol[i, ] - sol[i - 1, ])
        if (all(tol_iter < tol) == TRUE) break
    }
    out <- vector("list", 4)
    names(out) <- c("value", "par", "iter", "hessian")
    out["value"][[1]] <-
        integrating(alpha = initial, beta = beta,
                    det_sigma = det_sigma, inv_sigma = inv_sigma,
                    preds = preds, data = ys)
    out["par"][[1]] <- initial
    out["iter"][[1]] <- i - 1
    out["hessian"][[1]] <- change$hessian
    return(out)
}

## =====================================================================
## laplace: laplace approximation
## args:
## -- initial: vector, random effects;
## -- preds: list of linear predictors, they can be of different sizes;
## -- ys: data.frame, it needs the response vectors and covariates;
## -- beta: named vector;
## -- det_sigma: log-determinant of the variance-covariance matrix;
## -- inv_sigma: precison matrix.
## return:
## -- value, laplace approx. evaluation.

laplace <- function(initial, preds, ys, beta, det_sigma, inv_sigma) {
    integral <- -6e+05
    logQ <- try(
        newton_raphson(initial = initial,
                       preds = preds, ys = ys, beta = beta,
                       det_sigma = det_sigma, inv_sigma = inv_sigma),
        silent = TRUE)
    if (class(logQ) != "try-error") {
        integral <-
            logQ$value + length(initial)/2 * log(2*pi) -
            .5 * determinant(-logQ$hessian)$modulus
    }
    return(integral)
}

## =====================================================================
## vcov2: log-determinant of the variance-covariance matrix; and a
##        precision matrix
## args:
## -- theta: named vector, variance-covariance parameters.
## return:
## -- list with two slots,
##    ["inv"] sigma inverse, i.e., precision matrix;
##    ["logdet"] log-determinant of the variance-covariance matrix.

vcov2 <- function(theta) {
    rho <- 2 * exp(theta["rho"])/(1 + exp(theta["rho"])) - 1
    var1 <- exp(theta["var1"]) ; var2 <- exp(theta["var2"])
    c1 <- 1 - rho^2
    off_diag <- -rho/(sqrt(var1) * sqrt(var2) * c1)
    inv_sigma <- matrix(c(1/(var1 * c1), rep(off_diag, 2),
                          1/(var2 * c1)), 2, 2)
    det_sigma <- log(var1 * var2 * c1)[[1]]
    out <- vector("list", 2) ; names(out) <- c("inv", "logdet")
    out["inv"][[1]] <- inv_sigma
    out["logdet"][[1]] <- det_sigma
    return(out)
}

## =====================================================================
## multi_mixed: marginal likelihood
## args:
## -- theta: named vector, whole parameter vector;
## -- preds: list of linear predictors, they can be of different sizes;
## -- data: data.frame, it needs the response vectors, covariates and a
## subject index;
## -- until: number of subjects.
## return:
## -- negative of the likelihood sum.

multi_mixed <- function(theta, preds, data, until) {
    out <- -sqrt(.Machine$double.xmax)
    beta <- theta[str_detect(names(theta), pattern = "^b\\d")]
    myvcov <- vcov2(theta) ; alpha <- numeric(nrow(myvcov$inv))
    out <- future_map_dbl(
        seq(until),
        function(index) {
            laplace(initial = alpha, preds = preds, beta = beta,
                    ys = data[data$i == index, ],
                    det_sigma = myvcov$logdet, inv_sigma = myvcov$inv)
        })
    return(-sum(out))
}
