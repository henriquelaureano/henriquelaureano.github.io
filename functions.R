## =====================================================================
## Multinomial GLMM functions ==========================================
## author: henrique laureano
## contact: www.leg.ufpr.br/~henrique
## date: 2020-4-9
## =====================================================================

## =====================================================================
## libraries

pack <- c("rmarkdown", "tidyverse", "DT", "tictoc", "fishualize",
          "latex2exp", "gridExtra",
          ## -----------------------------------------------------------
          "mvtnorm", "mc2d", "furrr", "bbmle", "Matrix", "parallel",
          "compiler",
          ## -----------------------------------------------------------
          "gdata")
pacman::p_load(pack, character.only = TRUE)

## =====================================================================
## datasetup: creates a competing risks data structure
## args:
## -- J: numeric, number of clusters;
## -- I: numeric, number of subjects into each cluster j.
##    note: each cluster has the same number of subjects; --------------
## -- k: number of multinomial classes;
## -- E: latent effects covariance matrix.
## return:
## -- data.frame with "2 + k + 1 + gauss_dim" columns.

datasetup <- function(J, I, k, E) {
    gauss_dim <- 2 * (k - 1)
    r <- rmvnorm(J, mean = rep(0, gauss_dim), sigma = E)
    colnames(r) <- c(paste0("u", seq(gauss_dim/2)),
                     paste0("e", seq(gauss_dim/2)))
    out <- data.frame(matrix(0,
                             nrow = I * J,
                             ncol = 2 + k + 1 + gauss_dim))
    names(out) <- c("j", "i", paste0("y", seq(k)), "t", colnames(r))
    out$j <- rep(seq(J), each = I)
    out$i <- rep(seq(I), J)
    for (i in seq(gauss_dim)) {
        out[ , 2 + k + 1 + i] <- rep(r[ , i], each = I)
    }
    return(out)
}

## =====================================================================
## Xcoef: compute the matricial product X %*% coef for each linear
##        predictor
## args:
## -- preds: list of linear predictors, it can be of different sizes;
## -- coefs: list of named vectors (betas and gammas) where the last
##           digit indicates from which linear predictor the coef
##           correspond;
## -- data: data.frame, the output of 'datasetup';
## return:
## -- array (R's tensor).

Xcoef <- function(preds, coef, data) {
    X <- sapply(preds, model.matrix, data, simplify = "array")
    out <- sweep(X, 3, unlist(coef), '*')
    return(out)
}

## =====================================================================
## risklevel: multinomial logistic regression model with random effects,
##            it return a data.frame of probabilites
## args:
## -- Xbeta: a array, output of 'Xcoef';
## -- u: data.frame, subset from the output of 'datasetup' containing
##       the u's random effects.
## return:
## -- data.frame with the classes probabilities.

risklevel <- function(Xbeta, u) {
    numerator <- exp(Xbeta + u)
    denominator <- 1 + rowSums(numerator)
    risklevel <- numerator/denominator
    out <- cbind(risklevel, 1 - rowSums(risklevel))
    colnames(out) <- paste0("p", seq(dim(Xbeta)[3] + 1))
    return(out)
}

## =====================================================================
## failuretimes: compute the failure times to each cause of failure,
##               including the censorship
## args:
## -- Xgama: a array, output of 'Xcoef';
## -- e: data.frame, subset from the output of 'datasetup' containing
##       the e's random effects;
## -- delta: numeric, fixed time point at which all individuals still
##           at risk are censored. this value will be used as a initial
##           value kind, not as the true final value.
## return:
## -- data.frame with the failure times to each class, including the
##    censorship.

failuretimes <- function(Xgama, e, delta) {
    size <- dim(Xgama)[1]
    qvarepsilon <- qnorm(runif(size))
    out <- delta * {atan( { qvarepsilon + Xgama + e }/c(3, 2) ) + 1}/2
    out$censor <- runif(size, 0, delta + 30)
    names(out) <- paste0("t", seq(ncol(out)))
    return(out)
}

## =====================================================================
## preccomp: precision matrix and respective log-determinant -
##           transformations are applied to its parameters
## args:
## -- theta: named vector, diagonal elements should start with 'd' and
##           off-diagonal elements should start with 'offd' followed by
##           its corresponding position number.
## return:
## -- list with two slots,
##    $prec: precision matrix of the transformed parameters;
##    $logprecdet: log-determinant of '$prec'.

preccomp <- function(theta) {
    precdiag <- str_subset(names(theta), "^d\\d")
    precdim <- length(precdiag)
    prec <- matrix(NA, nrow = precdim, ncol = precdim)
    diag(prec) <- exp(theta[precdiag])
    for (i in seq(precdim - 1)) {
        for (j in seq(i + 1, precdim)) {
            ijth <- str_subset(names(theta), paste0("offd", i, j))
            precij <- exp(theta[ijth])
            prec[i, j] <- 2 * precij/(1 + precij) - 1
        }
    }
    lowertri <- lower.tri(prec, diag = TRUE)
    prec[lowertri] <- t(prec)[lowertri]
    logprecdet <- log(det(chol(prec))^2)
    return(list(prec = prec, logprecdet = logprecdet))
}

## =====================================================================
## augloglik: old 'integrating': augmented log-likelihood
## args:
## -- alpha: named vector, random effects;
## -- beta: named vector;
## -- det_sigma: log-determinant of the variance-covariance matrix;
## -- inv_sigma: precision matrix;
## -- preds: list of linear predictors, they can be of different sizes;
## -- data: data.frame, it needs the response vectors and covariates.
## return:
## -- value, augmented likelihood evaluation.

augloglik <- function(r, preds, coef, data, u, detprec, prec) {
    y <- as.matrix(data[ , paste0("y", seq(length(preds) + 1))])
    Xbeta <- Xcoef(preds, coef, data)
    ps <- risklevel(Xbeta, u)
    out <- sum(dmultinomial(y, size = 1, prob = ps, log = TRUE)) -
        2 * log(2 * pi) + .5 * detprec - .5 * r %*% prec %*% r
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
## laplaceC: compiled body expression version of laplace
laplaceC <- cmpfun(laplace)

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
            laplaceC(initial = alpha, preds = preds, beta = beta,
                     ys = data[data$i == index, ],
                     det_sigma = myvcov$logdet, inv_sigma = myvcov$inv)
        })
    return(-sum(out))
}
