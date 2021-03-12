#' Plotting Bivarate Gaussian copula 

#' Provides a scatterplot of a Gaussian copula against uniform marginals

#' inputs:
#' C_para    = copula parameter
#' NSim      = number of simulations 


Plot_Gaussian_Copula <- function(C_para, Nsim){
  
# Gaussian copula
C_Gaussian <- normalCopula(C_para, dim = 2, dispstr = "un")
# Uniform marginals
U_margins <- list(list(0, 1), list(0, 1))

# Mlt r.v. made of copula and margins
Mlt_gaussian_uni <- mvdc(copula = C_Gaussian, margins = rep("unif", 2),
                         paramMargins = U_margins)

# sample 
Mlt_samp <- rMvdc(Nsim, Mlt_gaussian_uni)
# scatter plot
plot(Mlt_samp, main = "Gaussian coupla", xlab = expression(U[1]), ylab = expression(U[2]))
# Kendall's tau
tau <- tau(C_Gaussian)
# tail indices
tail_dep <- lambda(C_Gaussian)
dependence <- c("tau" = tau, "lambda" = tail_dep)
return(dependence)  
}



#####################################################
#' Plotting Bivarate Gaussian Distribution

#' Provides a scatterplot of a Bivariate Gaussian against standard normal marginals

#' inputs:
#' C_para    = copula parameter
#' NSim      = number of simulations 

Plot_Gaussian <- function(C_para, Nsim){
  
  # Gaussian copula
  C_Gaussian <- normalCopula(C_para, dim = 2, dispstr = "un")
  ### normal marginals with mean 0 and variance 1
  Norm_margins <- list(list(0, 1), list(0, 1))
  
  # Mlt r.v. made of copula and margins
  Mlt_gaussian_norm <- mvdc(copula = C_Gaussian, margins = rep("norm", 2),
                            paramMargins = Norm_margins)
  
  # sample 
  Mlt_samp <- rMvdc(Nsim, Mlt_gaussian_norm)
  # scatter plot
  plot(Mlt_samp, main = "Gaussian Coupla with N(0,1) marginals", 
       xlab = expression(italic(N)(0,1)), ylab = expression(italic(N)(0,1)))
  # Kendall's tau
  tau <- tau(C_Gaussian)
  # tail indices
  tail_dep <- lambda(C_Gaussian)
  dependence <- c("tau" = tau, "lambda" = tail_dep)
return(dependence)  
  
}
