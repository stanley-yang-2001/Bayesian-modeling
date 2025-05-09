d <- read.table("polls2016.txt", header=TRUE)

d$sigma <- d$ME/2

print(d)
library(rjags)
initial.vals <- list(list(mu=100, tau = 0.01),
                    list(mu=100, tau = 100),
                    list(mu=-100, tau = 0.01),
                    list(mu=-100, tau = 100))

m1 <- jags.model("polls20161.bug", d, initial.vals, n.chains=4, n.adapt=1000)

update(m1, 2500)
x1 <- coda.samples(m1, c("mu", "tau"), n.iter=5000)
#plot(x1, smooth=FALSE)

result1 <-gelman.diag(x1, autoburnin=FALSE)
print(result1)

autocorr.plot(x1[[2]])

sample1 <- effectiveSize(x1)
print(sample1)