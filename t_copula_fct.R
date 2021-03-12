#' Plotting Bivarate t copula 

#' Provides a scatterplot of a t copula against uniform marginals

#' inputs:
#' C_para    = copula parameter
#' df        = degree of freedom of coupla and marginals
#' NSim      = number of simulations 


Plot_t_Copula <- function(C_para, df, Nsim){
  
  # t copula
  C_t <- tCopula(C_para, dim = 2, df = df, dispstr = "un")
  # Uniform marginals
  U_margins <- list(list(0, 1), list(0, 1))
  
  # Mlt r.v. made of copula and margins
  Mlt_t_uni <- mvdc(copula = C_t, margins = rep("unif", 2),
                    paramMargins = U_margins)
  
  # sample 
  Mlt_samp <- rMvdc(Nsim, Mlt_t_uni)
  # scatter plot
  plot(Mlt_samp, main = "t coupla", xlab = "U1", ylab = "U2")
  
}



#####################################################
#' Plotting Bivarate t Distribution

#' Provides a scatterplot of a t Gaussian against t marginals

#' inputs:
#' C_para    = copula parameter
#' NSim      = number of simulations 
#' df        = degree of freedom 

Plot_t <- function(C_para, df, Nsim){
  
  # t copula
  C_t <- tCopula(C_para, dim = 2, df = df, dispstr = "un")
  
  # t distributed marginals with degree of freedom
  t_margins <- list(df, df)
  
  Mlt_t_t <- mvdc(copula = C_t, margins = rep("t", 2),
                  paramMargins = t_margins)
  # sample 
  Mlt_samp <- rMvdc(Nsim, Mlt_t_t)
  # scatter plot
  plot(Mlt_samp, main = "t coupla", xlab = "t1", ylab = "t2")
  
}
