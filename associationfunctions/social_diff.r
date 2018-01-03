###
# social differentiation calculation

### simulated data

# S <- 2.699
# mu <- .01
# sds <- S * mu
# u1 <- mu*((1-mu)/(mu*S^2) - 1)
# u2 <- (1-mu)*((1-mu)/(mu*S^2) -1)
# N <- 100
# n <- sample(1:400, N, replace = TRUE)
# p <- rbeta(N, u1, u2)
# x <- rbinom(N, n, p)

# Sest <- sqrt(var(x) - mean(x))/mean(x)
# muest <- mean(x/n)
# sdest <- muest * Sest
 
### observed data

dese <- which(rownames(assoc) %in% rownames(sr_filtered))
assoc_filtered <- assoc[dese, dese]

dese <- which(rownames(nmatrix) %in% rownames(sr_filtered))
nmatrix_filtered <- nmatrix[dese, dese]

assoc_filtered[is.na(sr_filtered)] <- NA
nmatrix_filtered[is.na(sr_filtered)] <- NA

x <- assoc_filtered[upper.tri(assoc_filtered)]
n <- nmatrix_filtered[upper.tri(nmatrix_filtered)]

x <- x[!is.na(x)]
n <- n[!is.na(n)]

N <- length(n)
Sest <- sd(x/n)/mean(x/n)
muest <- mean(x/n)
sdest <- sd(x/n)


### the model 
library(rjags)
jags <- jags.model('../associationfunctions/socialdiffmod',
                   data = list('x' = x,
                               'N' = N,
							   'n' = n,
							   'cenmu' = muest,
							   'cenSD' = sdest),
				    inits = list('mu' = muest,
							     'sd'  = sdest),
                   n.chains = 1,
                   n.adapt = 100)
ng <- 10000

out <- coda.samples(jags,
             c('mu', 'sd'),
             ng)	 
			 
plot(out)
summary(out)

burnin <- .9 * ng
out.burn <- window(out, burnin, ng)
plot(out.burn)
summary(out.burn)


### how good is the fit?
ss <- as.matrix(out.burn)
mu <- apply(ss, 2, quantile, 0.50)[1]
sd <- apply(ss, 2, quantile, 0.50)[2]

a <- mu*((1-mu)/(mu*(sd/mu)^2) - 1)
b <- (1-mu)*((1-mu)/(mu*(sd/mu)^2) -1)

hist(x/n, nclass = 1000, prob = TRUE, xlim = c(0,0.1), ylim = c(0,2000))
lines(density(rbeta(seq(0,1, length.out = 1000), a, b)), col = "blue", lty = 2)