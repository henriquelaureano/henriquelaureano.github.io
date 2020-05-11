## =====================================================================
## functions for the competing risks data analysis via a Multinomial
## GLMM ================================================================
## author: henrique laureano
## contact: www.leg.ufpr.br/~henrique
## date: 2020-5-9
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
    ## precdiag <- str_subset(names(theta), "^d\\d")
    ## precdim <- length(precdiag)
    ## prec <- matrix(NA, nrow = precdim, ncol = precdim)
    ## diag(prec) <- exp(theta[precdiag])
    ## for (i in seq(precdim - 1)) {
    ##     for (j in seq(i + 1, precdim)) {
    ##         ijth <- str_subset(names(theta), paste0("offd", i, j))
    ##         precij <- exp(theta[ijth])
    ##         prec[i, j] <- 2 * precij/(1 + precij) - 1
    ##     }
    ## }
    Dentries <- str_subset(names(theta), "^d\\d")
    precdim <- length(Dentries)
    invD2 <- diag(1/exp(theta[Dentries]), precdim, precdim)
    tT <- diag(1, nrow = precdim, ncol = precdim)
    for (i in seq(precdim - 1)) {
        for (j in seq(i + 1, precdim)) {
            ijth <- str_subset(names(theta), paste0("offd", i, j))
            tT[i, j] <- exp(theta[ijth])
        }
    }
    T <- t(tT)
    ## modified cholesky decomposition
    Q <- tT %*% invD2 %*% T
    ## lowertri <- lower.tri(prec, diag = TRUE)
    ## prec[lowertri] <- t(prec)[lowertri]
    ## logdetprec <- determinant(Q)$modulus
    ## logdetprec <- log(prod(diag(chol(prec)))^2)
    logdetQ <- log(prod(diag(invD2)))
    return(list(prec = Q, logdetprec = logdetQ))
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
    cd <- 1 + rowSums(rl) ## common denominator
    ratio <- sweep(rl, 1, cd, '/')
    t <- data$t
    delta <- max(t) + .1
    xge <- sweep(-Xgama, 3, r[paste0("e", seqk)], '-')
    pk <- t(sapply(seq(dim(ratio)[1]), function(i) {
        ## each column will be a subject
        ratio[i, , ] * w * delta/(2 * t[i] * (delta - t[i])) *
            dnorm(w * atanh(2 * t[i]/delta - 1) + xge[i, , ])
    }))
    ps <- cbind(pk, 1 - rowSums(pk))
    y <- as.matrix(data %>% select(str_subset(names(data), "^y\\d")))
    out <- sum(dmultinomial(y, size = 1, prob = ps, log = TRUE)) -
        2 * log(2 * pi) + .5 * logdetprec - .5 * r %*% prec %*% r
    return(out)
}

## =====================================================================
## gradHess: gradient and hessian of the augmented log-likelihood
## args:
## -- r: named vector of random effects to be integrated out;
## -- preds: list of linear predictors, it can be of different sizes;
## -- beta: named vector where the last digit indicates from which
##          linear predictor the coef correspond;
## -- gama: named vector where the last digit indicates from which
##          linear predictor the coef correspond;
## -- dfj: data.frame, the output of 'datasetup' but just with one
##         cluster at time;
## -- w: vector of parameters with the same length that 'preds';
## -- prec: precision matrix, output of 'preccomp'.
## return:
## -- list with two slots,
##    ['change'] gradient 'divided' by the hessian;
##    ['hessian'] hessian.

gradHess <- function(r, preds, beta, gama, dfj, w, prec) {
    n <- nrow(dfj) ## number of subjects
    nr <- length(r)
    seqk <- seq(length(preds)) ## half of the gradient output dimension
    Xbeta <- Xcoef(preds, beta, dfj)
    Xgama <- Xcoef(preds, gama, dfj)
    rl <- exp(sweep(Xbeta, 3, r[paste0("u", seqk)], '+')) ## risk level
    rlsum <- rowSums(rl) ## length? n
    cd <- 1 + rlsum ## cd, common denominator
    ratio <- sweep(rl, 1, cd, '/')
    y <- as.matrix(dfj %>% select(str_subset(names(dfj), "^y\\d")))
    ymax <- y[ , nr/2 + 1]
    y <- y[ , -(nr/2 + 1)]
    t <- dfj$t
    delta <- max(t) + .1
    tt <- sweep(-Xgama, 3, r[paste0("e", seqk)], '-') ## trajectory time
    ## recheio, each column will be a subject --------------------------
    recheio <- sapply(seqk, function(i) {
        w * atanh(2 * t[i]/delta - 1) + tt[i, , ] })
    ## cereja phi(recheio), each column will be a subject --------------
    cereja <- sapply(seqk, function(i) {
        w * delta/(2 * t[i] * (delta - t[i])) * dnorm(recheio[ , i])
    })
    cdlong <- sapply(seqk, function(i) { ## long common denominator
        1 + sum(rl[i, , ] * (1 - cereja[ , i])) })
    ## GRADIENT --------------------------------------------------------
    ## computing by random effect u (do for each subject and then sum)
    d1u <- sapply(seqk, function(i) {
        sum( ( y[ , i] * (1 + rl[ , , -i]) -
               y[ , -i] * rl[ , , i] )/cd + ymax * rl[ , , i] *
             ( cereja[-i, ] * rl[ , , -i] -
               cereja[i, ] * (1 + rl[ , , -i]) )/(cd * cdlong)
            )})
    ## each column corresponds to a random effect eta
    de_n <- sapply(seqk, function(i) {
        ratio[ , , i] * recheio[i, ] * cereja[i, ]
    })
    ## length? n
    de_d <- sapply(seqk, function(i) {
        1 - sum(ratio[i, , ] * cereja[ , i]) })
    d1e <- sapply(seqk, function(i) {
        sum( y[ , i] * recheio[i, ] - ymax * de_n[ , i]/de_d )
    })
    grad <- c(d1u, d1e) - prec %*% r
    ## HESSIAN ---------------------------------------------------------
    hess <- matrix(NA, nrow = nr, ncol = nr)
    ## each column is for a random effect u
    diag(hess)[seqk] <- sapply(seqk, function(i) {
        sum( - rowSums(y[ , seqk]) *
             rl[ , , i] * (1 + rl[ , , -i])/cd^2 + ymax * rl[ , , i] *
             ( ( cereja[-i, ] * rl[ , , -i] -
                 cereja[i, ] * (1 + rl[ , , -i]) )/(cd * cdlong) +
               ( cereja[i, ] * (1 + rl[ , , -i]) -
                 cereja[-i, ] * rl[ , , -i]
               ) * ( cdlong + cd * (1 - cereja[i, ]) )/(cd * cdlong)^2
             )
            )})
    diag(hess)[seqk + max(seqk)] <- sapply(seqk, function(i) {
        sum( - y[ , i] - ymax *
             ( ratio[ , , i] * cereja[i, ] * (recheio[i, ]^2 - 1)/de_d +
               (de_n[ , i]/de_d)^2 )
            )})
    du12 <- sum(
        rowSums(y[ , seqk]) * rl[ , , 1] * rl[ , , 2]/cd^2 + ymax *
        ( rl[ , , 1] * rl[ , , 2] *
          (cereja[2, ] - cereja[1, ])/(cd * cdlong) +
          rl[ , , 1] *
          ( cereja[1, ] * (1 + rl[ , , 2]) - cereja[2, ] * rl[ , , 2]
          ) * rl[ , , 2] *
          ( cdlong + cd * (1 - cereja[2, ]) )/(cd * cdlong)^2
        ) )
    de12 <- sum( - ymax * de_n[ , 1] * de_n[ , 2]/de_d^2)
    due <- sapply(seqk, function(i) {
        sum( ymax *
             ( de_n[ , i] *
               ( rl[ , , i] * rl[ , , -i]/cd^2 * cereja[-i, ] -
                 rl[ , , i] * (cd - rl[ , , i])/cd^2 * cereja[i, ]
               )/de_d^2 -
               rl[ , , i] * (cd - rl[ , , i])/cd^2 *
               recheio[i, ] * cereja[i, ]/de_d
             ) ) })
    due2 <- sapply(seqk, function(i) {
        sum( ymax *
             ( de_n[ , i] *
               ( rl[ , , i] * rl[ , , -i]/cd^2 * cereja[-i, ] -
                 rl[ , , -i] * (cd - rl[ , , -i])/cd^2 * cereja[-i, ]
               )/de_d^2 +
               rl[ , , i] * rl[ , , -i]/cd^2 *
               recheio[i, ] * cereja[i, ]/de_d
             ) ) })
    hess[1, 2] <- hess[2, 1] <- du12
    hess[3, 4] <- hess[4, 3] <- de12
    hess[1, 3] <- due[1] ; hess[2, 4] <- due[2]
    hess[1, 4] <- due2[1] ; hess[2, 3] <- due2[2]
    lowertri <- lower.tri(hess, diag = TRUE)
    hess[lowertri] <- t(hess)[lowertri]
    hess <- hess - prec
    change <- solve(hess, grad)
    return(list('change' = change, 'hessian' = hess))
}

## =====================================================================
## newton_raphson: optimzation routine to be used inside the laplace
##                 approximation
## args:
## -- initial: named vector of random effects to be integrated out;
## -- preds: list of linear predictors, it can be of different sizes;
## -- beta: named vector where the last digit indicates from which
##          linear predictor the coef correspond;
## -- gama: named vector where the last digit indicates from which
##          linear predictor the coef correspond;
## -- dfj: data.frame, the output of 'datasetup' but just with one
##         cluster at time;
## -- w: vector of parameters with the same length that 'preds';
## -- prec: precision matrix, output of 'preccomp';
## -- logdetprec: log-determinant of the precision matrix, also output
##                of 'preccomp';
## -- max_iter: maximum number of iterations, pre-fixed at 50;
## -- tol: minimum convergence tolerance.
## return:
## -- list with four slots,
##    ['value'] augmented log-likelihood evaluated at 'par';
##    ['par'] estimated values;
##    ['iter'] number of iterations up to convergence;
##    ['hessian'] hessian at 'par'.

newton_raphson <- function(initial,
                           preds, beta, gama, dfj, w, prec, logdetprec,
                           max_iter = 50, tol = 1e-5) {
    sol <- matrix(NA, nrow = max_iter, ncol = length(initial))
    colnames(sol) <- names(initial)
    sol[1, ] <- initial
    for (i in 2:max_iter) {
        change <- gradHess(r = initial, preds, beta, gama, dfj, w, prec)
        sol[i, ] <- initial - change$change
        initial <- sol[i, ]
        tol_iter <- abs(sol[i, ] - sol[i - 1, ])
        if (all(tol_iter < tol) == TRUE) break
    }
    return(list('value' = augloglik(r = initial, preds, beta, gama,
                                    data = dfj, w, prec, logdetprec),
                'par' = initial,
                'iter' = i - 1,
                'hessian' = change$hessian))
}

## =====================================================================
## laplace: laplace approximation of the augmented log-likelihood
## args:
## -- initial: named vector of random effects to be integrated out;
## -- preds: list of linear predictors, it can be of different sizes;
## -- beta: named vector where the last digit indicates from which
##          linear predictor the coef correspond;
## -- gama: named vector where the last digit indicates from which
##          linear predictor the coef correspond;
## -- dfj: data.frame, the output of 'datasetup' but just with one
##         cluster at time;
## -- w: vector of parameters with the same length that 'preds';
## -- prec: precision matrix, output of 'preccomp';
## -- logdetprec: log-determinant of the precision matrix, also output
##                of 'preccomp'.
## return:
## -- value, laplace approximation evaluation.

laplace <- function(initial,
                    preds, beta, gama, dfj, w, prec, logdetprec) {
    integral <- -6e+05
    logQ <- try(
        newton_raphson(initial,
                       preds, beta, gama, dfj, w, prec, logdetprec),
        silent = TRUE)
    if (class(logQ) != "try-error") {
        integral <-
            logQ$value + length(initial)/2 * log(2*pi) -
            .5 * determinant(-logQ$hessian)$modulus
            ## log(prod(diag(chol(-logQ$hessian))))
    }
    return(integral)
}

## =====================================================================
## laplaceC: 'laplace' compiled body expression version
## args: see 'laplace' args
## return: see 'laplace' return

laplaceC <- cmpfun(laplace)

## =====================================================================
## marginaloglik: marginal log-likelihood
## args:
## -- theta: named vector, whole parameter vector;
## -- preds: list of linear predictors, they can be of different sizes;
## -- data: data.frame, it needs the response vectors, covariates and a
## subject index;
## -- until: number of subjects.
## return:
## -- negative of the likelihood sum.

marginaloglik <- function(theta, preds, data, until) {
    out <- -sqrt(.Machine$double.xmax)
    beta <- theta[str_detect(names(theta), pattern = "^b\\d")]
    gama <- theta[str_detect(names(theta), pattern = "^g\\d")]
    w <- theta[str_detect(names(theta), pattern = "^w\\d")]
    PREC <- try(preccomp(theta), silent = TRUE)
    lpreds <- length(preds)
    r <- numeric(lpreds * 2)
    names(r) <- c(paste0("u", seq(lpreds)), paste0("e", seq(lpreds)))
    out <- future_map_dbl(seq(until), function(index) {
        laplaceC(initial = r,
                 preds = preds, beta = beta, gama = gama,
                 dfj = data %>% filter(j == index),
                 w = w, prec = PREC$prec, logdetprec = PREC$logdetprec)
    })
    ## out <- sapply(seq(until), function(index) {
    ##     laplaceC(initial = r,
    ##              preds = preds, beta = beta, gama = gama,
    ##              dfj = data %>% filter(j == index),
    ##              w = w, prec = PREC$prec, logdetprec = PREC$logdetprec)
    ## })
    return(-sum(out))
}
