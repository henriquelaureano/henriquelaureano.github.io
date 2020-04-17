## =====================================================================
## functions for the competing risks data analysis via a Multinomial
## GLMM ================================================================
## author: henrique laureano
## contact: www.leg.ufpr.br/~henrique
## date: 2020-4-17
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
    denominator <- 1 + Reduce("+", numerator)
    risklevel <- numerator/denominator
    out <- cbind(risklevel, 1 - Reduce("+", risklevel))
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
##    $logdetprec: log-determinant of '$prec'.

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
    logdetprec <- log(det(chol(prec))^2)
    return(list(prec = prec, logdetprec = logdetprec))
}

## =====================================================================
## augloglik: augmented log-likelihood
## args:
## -- r: named vector of random effects to be integrated out;
## -- preds: list of linear predictors, it can be of different sizes;
## -- beta: named vector where the last digit indicates from which
##          linear predictor the coef correspond;
## -- gama: named vector where the last digit indicates from which
##          linear predictor the coef correspond;
## -- data: data.frame, the output of 'datasetup';
## -- w: vector of parameters with the same length that 'preds';
## -- prec: precision matrix, output of 'preccomp';
## -- logdetprec: log-determinant of the precision matrix, also output
##                of 'preccomp'.
## return:
## -- value, augmented log-likelihood evaluation.

augloglik <- function(r, preds, beta, gama, data, w, prec, logdetprec) {
    npreds <- length(preds)
    seqk <- seq(npreds)
    Xbeta <- Xcoef(preds, beta, data)
    Xgama <- Xcoef(preds, gama, data)
    rl <- exp(sweep(Xbeta, 3, r[paste0("u", seqk)], '+')) ## risk level
    rld <- 1 + rowSums(rl) ## rl denominator
    ratio <- sweep(rl, 1, rld, '/')
    t <- data$t
    delta <- max(t) + .1
    xge <- sweep(-Xgama, 3, r[paste0("e", seqk)], '-')
    pk <- t(sapply(
        seq(dim(ratio)[1]), ## number of subjects
        function(i) { ## each column will be  a subject
            ratio[i, , ] *
                w * delta/(2 * t[i] * (delta - t[i])) *
                dnorm(w * atanh(2 * t[i]/delta - 1) + xge[i, , ])
        }))
    ps <- cbind(pk, 1 - rowSums(pk))
    y <- as.matrix(data %>% select(str_subset(names(data), "^y\\d")))
    ## -----------------------------------------------------------------
    out <- sum(dmultinomial(y, size = 1, prob = ps, log = TRUE)) -
        2 * log(2 * pi) + .5 * logdetprec - .5 * r %*% prec %*% r
    return(out)
}

## =====================================================================
## gradHess: gradient and hessian of the augmented log-likelihood
## args:
## -- r: vector, random effects;
## -- preds: list of linear predictors, they can be of different sizes;
## -- beta: named vector;
## -- gama: named vector;
## -- dfj: ;
## -- w: ;
## -- prec: precision matrix, output of 'preccomp'.
## return:
## -- list with two slots,
##    ["change"] gradient "divided" by the hessian;
##    ["hessian"] hessian.

gradHess <- function(r, preds, beta, gama, dfj, w, prec) {
    n <- nrow(dfj) ## number of subjects
    seqk <- seq(length(preds)) ## half of the gradient output dimension
    Xbeta <- Xcoef(preds, beta, dfj)
    Xgama <- Xcoef(preds, gama, dfj)
    rl <- exp(sweep(Xbeta, 3, r[paste0("u", seqk)], '+')) ## risk level
    rlsum <- rowSums(rl) ## length? n
    cd <- 1 + rlsum ## cd, common denominator
    y <- as.matrix(dfj %>% select(str_subset(names(dfj), "^y\\d")))
    t <- dfj$t
    delta <- max(t) + .1
    tt <- sweep(-Xgama, 3, r[paste0("e", seqk)], '-') ## trajectory time
    ## =================================================================
    ## d1u part1, row: subjects
    ## ---------- col: random effects 'u' to be integrated out
    d1u_p1 <- sapply(seqk,
                     function(i) {
                         ( y[ , i] * (1 + rl[ , , -i]) -
                           rowSums(y[ , -i]) * rl[ , , i]
                         )/cd })
    ## recheio, row: random effects 'u' to be integrated out
    ## -------- col: subjects
    recheio <- sapply(
        seqk,
        function(i) { w * atanh(2 * t[i]/delta - 1) + tt[i, , ] })
    ## cereja, row: random effects 'u' to be integrated out
    ## ------- col: subjects
    cereja <- sapply(
        seqk,
        function(i) {
            w * delta/(2 * t[i] * (delta - t[i])) * dnorm(recheio[ , i])
        })
    ## cdlong, long common denominator. length? n
    cdlong <- sapply(seqk,
                     function(i) {
                         1 + sum(rl[i, , ] * (1 - cereja[ , i])) })
    ## d1u part2, row: subjects
    ## ---------- col: random effects 'u' to be integrated out
    d1u_p2 <- sapply(
        seqk,
        function(i) {
            y[ , max(seqk) + 1] * (
                rl[ , , i] * cereja[-i, ] * rl[ , , -i] -
                cereja[i, ] * rl[ , , i] * (1 + rl[ , , -i]) ) })
    d1u_p3 <- d1u_p2/cdlong ## it's right
    d1u <- colSums(d1u_p1 + d1u_p3) - (prec %*% r)[seqk]
    ## =================================================================
    d1e_n <- sapply(seqk,
                    function(i) {
                        (rl/cd)[i, , ] *
                            w * delta/(2 * t[i] * (delta - t[i])) *
                            recheio[ , i] * dnorm(recheio[ , i])
                    })
    d1e_d <- sapply(seqk,
                    function(i) {
                        1 - sum((rl/cd)[i, , ] *
                                w * delta/(2 * t[i] * (delta - t[i])) *
                                dnorm(recheio[ , i]))
                    })
    d1e_p1 <- sapply(seqk,
                     function(i) {
                         sum(y[ , i] * recheio[i, ] -
                             y[ , max(seqk) + 1] * d1e_n[i, ]/d1e_d)
                     })
    d1e <- d1e_p1 - (prec %*% r)[seqk + max(seqk)]
    ## =================================================================
    grad <- c(d1u, d1e)
    ## =================================================================
    ## now, the hessian ================================================
    hess_multi <- matrix(NA, nrow = n_alpha, ncol = n_alpha)
    diag(hess_multi) <- sapply(
        seq(n_alpha),
        function(i) sum(-Reduce("+", yj) * risklevel_num[ , , i] *
                        (1 + risklevel_num[ , , -i])/risklevel_denom2))
    offdiag <- sum(
        Reduce("+", yj) *
        risklevel_num[ , , 1] * risklevel_num[ , , -1]/risklevel_denom2)
    hess_multi[is.na(hess_multi)] <- offdiag
    hess <- hess_multi - prec
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
