# Functions for bounded regression models -------------------------------------
# Author: Ricardo Rasmussen Petterle/UFPR -------------------------------------
# Date: 17/07/2017 ------------------------------------------------------------

# Compute expectation and variance --------------------------------------------
mean_variance <- function(mu, phi, model = "beta") {
  if(model == "beta") {
    media <- mu
    variancia <- mu*(1-mu)/(1+phi)
  }
  if(model == "simplex") {
    media <- mu
    lambda <- 1/(phi^2)
    mu2prod <- (mu^2)*(1-mu)^2
    term1 <- sqrt(lambda/2)
    term2 <- lambda/(2*mu2prod)
    variancia <- as.numeric(mu*(1-mu) - term1*exp(term2)*gammainc(term2, 0.5)[2])
  }
  return(c("Var" = variancia))
}
mean_variance <- Vectorize(mean_variance, vectorize.args = "mu")

## Summary simulation----------------------------------------------------------
# Coverage rate
coverage_rate <- function(pt, std, true_value){
  ic.min <- pt - qnorm(0.975)*std
  ic.max <- pt + qnorm(0.975)*std
  fora <- sum(ic.min > true_value) + sum(ic.max < true_value)
  coverage <- 1 - fora/length(pt)
}

# Summary of simulation study 
summary_simulation <- function(results, true_values) {
  NN <- dim(results)[1]
  results <- na.exclude(results)
  FAIL <- NN - dim(results)[1]
  pontual <- results[,1:length(true_values)]
  std_error <- results[,c(length(true_values)+1):c(dim(results)[2])]
  pt_average <- colMeans(pontual, na.rm=TRUE)
  bias <- pt_average - true_values
  std_error_average <- colMeans(std_error, na.rm=TRUE)
  empirical_std_error <- apply(pontual, 2, sd)
  rate_emp_sample <- std_error_average/empirical_std_error
  coverage <- c()
  for(i in 1:length(true_values)){
    coverage[i] <- coverage_rate(pontual[,i], std_error[,i], true_values[i])
  }
  output <- data.frame("Expectation" = as.numeric(pt_average), 
                       "Bias" = as.numeric(bias), 
                       "Expected Std Error" = std_error_average,
                       "Ic Min" = as.numeric(bias) - std_error_average,
                       "Ic Max" = as.numeric(bias) + std_error_average,
                       "Empirical Std Error" = empirical_std_error, 
                       "Coverage" = coverage,
                       "FAIL" = FAIL)
  return(output)
}

ac <- list(pad1=0.5, pad2=0.5, tck=0.5)
mycol <- gray.colors(n=5)
ps <- list(box.rectangle=list(col=1, fill=c("gray70")),
           box.umbrella=list(col=1, lty=1),
           dot.symbol=list(col=1),
           dot.line=list(col=1, lty=3),
           plot.symbol=list(col=1, cex=0.7),
           plot.line=list(col=1),
           plot.polygon=list(col="gray80"),
           superpose.line=list(col=mycol),
           superpose.symbol=list(col=mycol),
           superpose.polygon=list(col=mycol),
           strip.background=list(col=c("gray90","gray70")), 
           layout.widths=list(
             left.padding=0.25,
             right.padding=0.5,
             ylab.axis.padding=0),
           layout.heights=list(
             bottom.padding=0.25,
             top.padding=0,
             axis.xlab.padding=0,
             xlab.top=0),
           axis.components=list(bottom=ac, top=ac, left=ac, right=ac)
)
panel.segplotBy <- function(x, y, z, centers, subscripts, groups, f, ...){
  d <- 2*((as.numeric(groups)-1)/(nlevels(groups)-1))-1
  z <- as.numeric(z)+f*d
  panel.segplot(x, y, z, centers=centers,
                subscripts=subscripts, ...)
}
# -----------------------------------------------------------------------------
dUG <- function(x, mu, sigma, log = FALSE){
  if (any(sigma < 0)) stop(paste("sigma must be positive", "\n", "")) 
  alpha <- (mu^(1/sigma))/(1 - mu^(1/sigma))
  fy1 <- alpha^sigma / gamma(sigma) * x^(alpha - 1) * (log(1/x)^(sigma - 1))
  if(log==FALSE) fy <- fy1 else fy <- log(fy1)
  fy
}
# Reparametrization of shape parameter
dUG2 <- function(x, mu, alpha, log = FALSE){
  if (any(mu <= 0) || any(mu >= 1)) 
    stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(alpha < 0)) stop(paste("alpha must be positive", "\n", "")) 
  sigma <- log(mu)/log(alpha/(alpha + 1))
  fy1 <- alpha^sigma / gamma(sigma) * x^(alpha - 1) * (log(1/x)^(sigma - 1))
  if(log==FALSE) fy <- fy1 else fy <- log(fy1)
  fy
}
# -----------------------------------------------------------------------------
pUG <- function (q, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  if (any(q <= 0) || any(q >= 1)) stop(paste("q must be between 0 and 1", "\n", ""))
  if (any(mu <= 0) || any(mu >= 1))   stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma <= 0) )    stop(paste("sigma must be between positive", "\n", ""))    
  lp <- pmax.int(length(q), length(mu), length(sigma))                                                                  
  q <- rep(q, length = lp)
  sigma <- rep(sigma, length = lp)
  mu <- rep(mu, length = lp)
  zero <- rep(0, length = lp)
  pdf <- function(x, mu,sigma)  {
    alpha <- (mu^(1/sigma))/(1 - mu^(1/sigma))
    pdf <- alpha^sigma / gamma(sigma) * x^(alpha - 1) * (log(1/x)^(sigma - 1))
  }
  cdfun <- function(upper, mu, sigma) {
    int <- integrate(pdf, lower=0, upper=upper, mu, sigma, subdivisions = 350)
    int$value 
  }
  Vcdf <- Vectorize(cdfun)
  cdf <- Vcdf(upper=q, mu=mu, sigma=sigma)
  if(lower.tail==TRUE) cdf  <- cdf else  cdf <- 1-cdf 
  if(log.p==FALSE) cdf  <- cdf else  cdf <- log(cdf) 
  cdf    
}
# Reparametrization of shape parameter
pUG2 <- function (q, mu = 0.5, alpha = 1, lower.tail = TRUE, log.p = FALSE) {
  if (any(q <= 0) || any(q >= 1)) stop(paste("q must be between 0 and 1", "\n", ""))
  if (any(mu <= 0) || any(mu >= 1))   stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(alpha <= 0) )    stop(paste("alpha must be between positive", "\n", ""))    
  lp <- pmax.int(length(q), length(mu), length(alpha))                                                                  
  q <- rep(q, length = lp)
  alpha <- rep(alpha, length = lp)
  mu <- rep(mu, length = lp)
  zero <- rep(0, length = lp)
  pdf <- function(x, mu, alpha)  {
    sigma <- log(mu)/log(alpha/(alpha + 1))
    pdf <- alpha^sigma / gamma(sigma) * x^(alpha - 1) * (log(1/x)^(sigma - 1))
  }
  cdfun <- function(upper, mu, alpha) {
    int <- integrate(pdf, lower=0, upper=upper, mu, alpha)
    int$value 
  }
  Vcdf <- Vectorize(cdfun)
  cdf <- Vcdf(upper=q, mu=mu, alpha=alpha)
  if(lower.tail==TRUE) cdf  <- cdf else  cdf <- 1-cdf 
  if(log.p==FALSE) cdf  <- cdf else  cdf <- log(cdf) 
  cdf    
}
# -----------------------------------------------------------------------------
rUG <- function(n, mu, sigma){
  if (any(sigma < 0)) stop(paste("sigma must be positive", "\n", "")) 
  if (any(n <= 0)) stop(paste("n must be a positive integer", "\n", "")) 
  alpha <- (mu^(1/sigma))/(1 - mu^(1/sigma))
  rg <- rgamma(n, shape = sigma, scale = 1/alpha)
  fy <- exp(-rg)
  fy
}
# Reparametrization of shape parameter
rUG2 <- function(n, mu, alpha){
  if (any(mu <= 0) || any(mu >= 1)) 
    stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(alpha < 0)) stop(paste("alpha must be positive", "\n", "")) 
  if (any(n <= 0)) stop(paste("n must be a positive integer", "\n", "")) 
  sigma <- log(mu)/log(alpha/(alpha + 1))
  rg <- rgamma(n, shape = sigma, scale = 1/alpha)
  fy <- exp(-rg)
  fy
}
# -----------------------------------------------------------------------------
qUG <-function (p, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE) { 
  {
    if (any(sigma <= 0)) 
      stop(paste("sigma must be positive", "\n", ""))
    if (any(mu <= 0) || any(mu >= 1)) 
      stop(paste("mu must be between 0 and 1", "\n", ""))
    if (log.p == TRUE) 
      p <- exp(p)
    else p <- p
    if (lower.tail == TRUE) 
      p <- p
    else p <- 1 - p
    if (any(p < 0) | any(p > 1)) 
      stop(paste("p must be between 0 and 1", "\n", ""))
    lp <- max(length(p), length(mu), length(sigma))
    p <- rep(p, length = lp)
    sigma <- rep(sigma, length = lp)
    mu <- rep(mu, length = lp)
    q <- rep(0, lp)
    h1 <- function(x, mu, sigma, p) pUG(x, mu, sigma) - 
      p
    uni <- function(mu, sigma, p) {
      val <- uniroot(h1, c(0.001, 0.999), mu = mu, sigma = sigma, 
                     p = p)
      val$root
    }
    UNI <- Vectorize(uni)
    q <- UNI(mu = mu, sigma = sigma, p = p)
    q
  }
}
# Reparametrization of shape parameter
qUG2 <-function (p, mu = 0.5, alpha = 1, lower.tail = TRUE, log.p = FALSE) { 
  {
    if (any(alpha <= 0)) 
      stop(paste("alpha must be positive", "\n", ""))
    if (any(mu <= 0) || any(mu >= 1)) 
      stop(paste("mu must be between 0 and 1", "\n", ""))
    if (log.p == TRUE) 
      p <- exp(p)
    else p <- p
    if (lower.tail == TRUE) 
      p <- p
    else p <- 1 - p
    if (any(p < 0) | any(p > 1)) 
      stop(paste("p must be between 0 and 1", "\n", ""))
    lp <- max(length(p), length(mu), length(alpha))
    p <- rep(p, length = lp)
    alpha <- rep(alpha, length = lp)
    mu <- rep(mu, length = lp)
    q <- rep(0, lp)
    h1 <- function(x, mu, alpha, p) pUG2(x, mu, alpha) - 
      p
    uni <- function(mu, alpha, p) {
      val <- uniroot(h1, c(0.001, 0.999), mu = mu, alpha = alpha, 
                     p = p)
      val$root
    }
    UNI <- Vectorize(uni)
    q <- UNI(mu = mu, alpha = alpha, p = p)
    q
  }
}
# END -------------------------------------------------------------------------
