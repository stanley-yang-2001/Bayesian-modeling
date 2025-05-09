# set up
d <- read.csv("launches.csv", header=TRUE)


d1 <- list(success = d$Success,
            timescaled = as.vector(scale(d$SinceFirst, scale=2*sd(d$SinceFirst)))) # nolint

inits1 <- list(list(betaintercept=10, betaslope=10),
               list(betaintercept=10, betaslope=-10),
               list(betaintercept=-10, betaslope=10),
               list(betaintercept=-10, betaslope=-10))
library(rjags)
load.module("dic")

m1 <- jags.model("model1.bug", d1, inits1, n.chains=4, n.adapt=4000)

update(m1, 4000)        # burn=in
x1 <- coda.samples(m1, c("betaintercept", "betaslope"), n.iter=10000) 

# check for convergence
plot(x1 ,smooth=FALSE)    
print(gelman.diag(x1, autoburnin=FALSE))

# getting another sample with all parameters
x1 <- coda.samples(m1, c("betaintercept", "betaslope", "success", "prob"), n.iter=10000) 

# checking effective sample size
result <- effectiveSize(x1[,1:4])
print(result)

# getting the coefficients for statistical purpose
betaintercept <-as.matrix(x1)[, paste0("betaintercept")]
betaslope <-as.matrix(x1)[, paste0("betaslope")]


# gathering the statistics of the posterior distribution for the two coefficients
print(summary(x1)$statistics)

# 95% posterior central interval for intercept
print(quantile(betaintercept, c(0.025, 0.975)))

# 95% posterior central interval for slope
print(quantile(betaslope, c(0.025, 0.975)))

# getting the porbability that the slope is positively related to days after first launch attempt
print(mean(betaslope > 0))

# getting the 95% posterior central interval for the successful launches probability at first launch
firsttime <- which(d$SinceFirst == 0)
prob <-as.matrix(x1)[, paste0("prob[", firsttime, "]")]
print(quantile(prob, c(0.025, 0.975)))

# producing the Plummer's DIC and associated effective number of parameters
print(dic.samples(m1, 100000))
