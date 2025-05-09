d <- read.csv("mooreslawdata.csv", header=TRUE)
d$log_trans <- log(d$TransistorCount)
d$averageyear <- mean(d$Year)
#plot(d$log_trans, d$year, xlab = "log transitors", ylab = "year")

library(rjags)

initial.vals <- list(list(beta1 = 10000, beta2 = 10000, sigmasqinv=0.0001),
                    list(beta1 = -10000, beta2 = 10000, sigmasqinv=0.01),
                    list(beta1 = 10000, beta2 = -10000, sigmasqinv=0.0001),
                    list(beta1 = -10000, beta2 = -10000, sigmasqinv=0.01))
data_list <- list(Year = d$Year, y = d$log_trans, averageyear = mean(d$Year), N = nrow(d))

m1 <- jags.model("mooremodel.bug", data_list, initial.vals, n.chains=4, n.adapt=1000)
update(m1, 2000)

x1 <- coda.samples(m1, c("beta1", "beta2", "sigmasq"), n.iter=2000)
print(summary(x1))

plot(x1, smooth=FALSE)

result1 <-gelman.diag(x1, autoburnin=FALSE)
print(result1)


autocorr.plot(x1[[2]])

sample1 <- effectiveSize(x1)

print(summary(x1)$statistics)
print(summary(x1)$quantile)