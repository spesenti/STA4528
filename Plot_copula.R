# Plotting copulas for different parameters

Nsim <- 10^3

####### copula parameters
# Independence
C_para <- c(0)
# positive correlation
#  C_para <- 0.8
# positive correlation
# C_para <- -0.8


##################### Gaussian copula #####################

# plot Gaussian copula
Plot_Gaussian_Copula(C_para, Nsim)

# plot Bivarate Gaussian
Plot_Gaussian(C_para, Nsim)

##################### t copula #####################

# plot t copula
Plot_t_Copula(C_para, df = 2, Nsim)

# plot Bivarate t distribution
Plot_t(C_para,  df = 3, Nsim)
