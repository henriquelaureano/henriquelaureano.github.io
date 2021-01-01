dcif1 <- function(mrow) {
    t <- mrow[ , 1] ; eta1 <- mrow[ , 2] ; u1 <- mrow[ , 3] ; u2 <- 0
    X <- 1
    beta1 <- -1.9 ; beta2 <- -0.2
    ## beta1 <- 10 ; beta2 <- 1
    risklevel1 <- exp(X * beta1 + u1)
    risklevel <- risklevel1/(1 + risklevel1 + exp(X * beta2 + u2))
    ## delta <- 80
    delta <- 110
    gamma1 <- 2
    dtraj <- dnorm(3 * atanh(2 * t/delta - 1) - X * gamma1 - eta1)
    out <- risklevel * 3 * delta/(2 * t * (delta - t)) * dtraj
    return(out)
}
## ---------------------------------------------------------------------
t <- seq(30, 110, .5)
u1 <- c(-1, -0.5, 0.5, 1)
eta1 <- c(-2, -1, 0, 1, 2)
## ---------------------------------------------------------------------
cifgrid <- expand.grid(t, eta1, u1)
names(cifgrid) <- c("t", "eta1", "u1")
## ---------------------------------------------------------------------
dcifs <- dcif1(cifgrid)
## ---------------------------------------------------------------------
df <- cbind(dcifs, cifgrid)
## ---------------------------------------------------------------------
## pacman::p_load(ggplot2)
## ---------------------------------------------------------------------
ggplot(df, aes(t, dcifs, group = eta1)) +
    geom_line(aes(linetype = factor(eta1))) +
    facet_wrap(~ u1, labeller = label_bquote(u[1] : .(u1))) +
    ## ylim(0, 0.2) +
    labs(x = "Time", y = "Cluster-specific dCIF",
         linetype = expression(eta[1])) +
    theme(legend.position = c(0.07, 0.83),
          legend.text = element_text(size = 11),
          strip.background = element_rect(colour = "black",
                                          fill = "white"),
          strip.text.x = element_text(size = 11),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(
              size = 11,
              margin = unit(c(t = 3, r = 0, b = 0, l = 0), "mm")),
          axis.title.y = element_text(
              size = 11,
              margin = unit(c(t = 0, r = 3, b = 0, l = 0), "mm")))

## ---------------------------------------------------------------------
pacman::p_load(numDeriv)

cif1 <- function(x, eta1, u1) {
    t <- x ; u2 <- 0
    X <- 1
    beta1 <- -1.9 ; beta2 <- -0.2
    risklevel1 <- exp(X * beta1 + u1)
    risklevel <- risklevel1/(1 + risklevel1 + exp(X * beta2 + u2))
    delta <- 80
    gamma1 <- 1
    traj <- pnorm(3 * atanh(2 * t/delta - 1) - X * gamma1 - eta1)
    out <- risklevel * traj
    return(out)
}

cif1(x = cifgrid[ , 1], mrow = cifgrid[ , -1])

grad(cif1, x = 0.5, eta1 = 2, u1 = -1)
