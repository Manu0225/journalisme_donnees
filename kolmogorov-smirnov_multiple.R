## test kolmogorov - smirnov multiple ?
library("kSamples")
set.seed(142)
samp.num <- 100
alpha <- 2.0; theta <- 3.0  # Gamma parameters shape and scale, using Wikipedia notation
gam.mean <- alpha * theta # mean of the Gamma
gam.sd <- sqrt(alpha) * theta # S.D. of the Gamma
norm.data <- rnorm(samp.num, mean=gam.mean, sd=gam.sd)  # Normal with the same mean and SD as the Gamma
gamma.data <- rgamma(samp.num, shape=alpha, scale=theta)
norm.data2 <- rnorm(samp.num, mean=gam.mean, sd=gam.sd)
norm.data3 <- rnorm(samp.num, mean=gam.mean, sd=gam.sd)
ad.same <- ad.test(norm.data,norm.data2,norm.data3) # "not significant, p ~ 0.459"
ad.diff <- ad.test(gamma.data,norm.data2,norm.data3) # "significant, p ~ 0.00066"