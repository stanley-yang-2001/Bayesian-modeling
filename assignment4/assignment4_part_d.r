d <- read.csv("mooreslawdata.csv", header=TRUE)
d$log_trans <- log(d$TransistorCount)
d$averageyear <- mean(d$Year)

library(rjags)

initial.vals <- list(list(beta1 = 10000, beta2 = 10000, sigmasqinv=0.0001),
                    list(beta1 = -10000, beta2 = 10000, sigmasqinv=0.01),
                    list(beta1 = 10000, beta2 = -10000, sigmasqinv=0.0001),
                    list(beta1 = -10000, beta2 = -10000, sigmasqinv=0.01))
data_list <- list(Year = d$Year, y = d$log_trans, averageyear = mean(d$Year), targetyear=2025, N = nrow(d))

m2 <- jags.model("mooremodel2.bug", data_list, initial.vals, n.chains=4, n.adapt=1000)
update(m2, 2000)

x2 <- coda.samples(m2, c("y.tilde","y.exp", "invent.year"), n.iter=2000)
print(summary(x2))