
d <- read.table("polls2016.txt", header=TRUE)

d$sigma <- d$ME/2

print(d)
library(rjags)
print(log(100))
initial.vals <- list(list(mu=100, logtau = log(0.01)),
                    list(mu=100, logtau = log(100)),
                    list(mu=-100, logtau = log(0.01)),
                    list(mu=-100, logtau = log(100)))

m2 <- jags.model("polls20162.bug", d, initial.vals, n.chains=4, n.adapt=1000)

update(m2, 2500)
x2 <- coda.samples(m2, c("mu", "tau"), n.iter=5000)
plot(x2, smooth=FALSE)

result2 <-gelman.diag(x2, autoburnin=FALSE)
print(result1)


autocorr.plot(x2[[2]])

sample2 <- effectiveSize(x2)
print(sample2)