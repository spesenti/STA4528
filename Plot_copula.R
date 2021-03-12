# Plotting copulas for different parameters

Nsim <- 10^3

##################### Bivariate copulas #####################

##################### Independent copula #####################
indep_cop <- indepCopula(2)
x <- rCopula(Nsim, indep_cop)
plot(x, main = "independence coupla", 
     xlab = expression(U[1]), ylab = expression(U[2]))
# Kendall's tau
tau(indep_cop)
# tail indices
lambda(indep_cop)


##################### Comonotonic copula #####################


##################### Countermonotonic copula #####################



##################### Gaussian copula #####################

# plot Gaussian copula with correlation C_para
Plot_Gaussian_Copula(C_para = 0.1, Nsim)

# plot Bivarate Gaussian
Plot_Gaussian(C_para = -0.1, Nsim)

##################### t copula #####################

# plot t copula
Plot_t_Copula(C_para = 0.2, df = 2, Nsim)

# plot Bivarate t distribution
Plot_t(C_para = 0.1,  df = 3, Nsim)


##################### Archimedean copulas #####################

# first parameter is the parameter of copula
clayton_copula <- claytonCopula(param = 5, dim = 2)
plot(rCopula(Nsim, clayton_copula), main = "Clayton coupla", 
     xlab = expression(U[1]), ylab = expression(U[2]))
# Kendall's tau
tau(clayton_copula)
# tail indices
lambda(clayton_copula)

# param == 1 is the independence copula
gumbel_copula <- gumbelCopula(param = 2, dim = 2)
plot(rCopula(Nsim, gumbel_copula), main = "Gumbel coupla", 
     xlab = expression(U[1]), ylab = expression(U[2]))
# Kendall's tau
tau(gumbel_copula)
# tail indices
lambda(gumbel_copula)


frank_copula <- frankCopula(param = 5, dim = 2)
plot(rCopula(Nsim, frank_copula), main = "Frank coupla", 
     xlab = expression(U[1]), ylab = expression(U[2]))
# Kendall's tau
tau(frank_copula)
# tail indices
lambda(frank_copula)
# ??????????????

alpha <- c(0.2, 0.7)
Marshall_Olkin_copula <- moCopula(alpha)
plot(rCopula(Nsim, Marshall_Olkin_copula), main = "Marshall Olkin coupla", 
     xlab = expression(U[1]), ylab = expression(U[2]))
# Kendall's tau
tau(Marshall_Olkin_copula)
# tail indices
lambda(Marshall_Olkin_copula)


##################### 3-D copulas #####################

persp(frank.cop, dCopula)


