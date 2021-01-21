# Generating coupulas

require(copula)

##################### Gaussian copula #####################
Nsim <- 10^3

####### copula parameters
# Independence
C_para <- c(0)

# positive correlation
# C_para <- 0.8
# positive correlation
# C_para <- -0.8


# Gaussian copula
C_Gaussian <- normalCopula(C_para, dim = 2, dispstr = "un")
# Univariate marginals
U_margins <- list(list(0, 1), list(0, 1))

# Mlt r.v. made of copula and margins
Mlt_gaussian_uni <- mvdc(copula = C_Gaussian, margins = rep("unif", 2),
                     paramMargins = U_margins)

# sample 
Mlt_samp <- rMvdc(Nsim, Mlt_gaussian_uni)
# scatter plot
plot(Mlt_samp, main = "Gaussian coupla", xlab = "U1", ylab = "U2")


### normal marginals with mean 0 and variance 1
Norm_margins <- list(list(0, 1), list(0, 1))

# Mlt r.v. made of copula and margins
Mlt_gaussian_norm <- mvdc(copula = C_Gaussian, margins = rep("norm", 2),
                     paramMargins = Norm_margins)

# sample 
Mlt_samp <- rMvdc(Nsim, Mlt_gaussian_norm)
# scatter plot
plot(Mlt_samp, main = "Gaussian Coupla with N(0,1) marginals", 
     xlab = "X1 = N(0,1)", ylab = "X2 = N(0,1)")





