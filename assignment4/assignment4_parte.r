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

m3 <- jags.model("mooremodel3.bug", data_list, initial.vals, n.chains=4, n.adapt=1000)
update(m3, 2000)

x3 <- coda.samples(m3, c("y", "yrep", "sigmasq", "linreg"), n.iter=2000)

y <-as.matrix(x3)[, paste0("y[", 1:nrow(d), "]")]
yrep <-as.matrix(x3)[, paste0("yrep[", 1:nrow(d), "]")]
linreg <-as.matrix(x3)[, paste0("linreg[", 1:nrow(d), "]")]
sigma <- sqrt(as.matrix(x3)[, "sigmasq"])


#sim.std.error <- (y-d$log_trans)/sigma
#sim.std.error <- (y-d$log_trans)/sigma
sim.std.error <- (d$log_trans-linreg)/sigma
#sim.rep.std.error <- (yrep - d$log_trans)/sigma
sim.rep.std.error <- (yrep - linreg)/sigma

#simulated standard error test quantity
Tysim <- apply(abs(sim.std.error), 1, max)
#simulated replicate standard error test quantity
Tyrepsim <- apply(abs(sim.rep.std.error), 1, max)

plot(Tysim, Tyrepsim, pch=".", cex=2,
     xlim=c(min(Tysim, Tyrepsim), max(Tysim, Tyrepsim)),
     ylim=c(min(Tysim, Tyrepsim), max(Tysim, Tyrepsim)),
     xlab="T(y,mu,theta)", ylab="T(y-rep,mu,theta)")
abline(a=0,b=1)


post.pred.p <- mean(abs(Tyrepsim) >= abs(Tysim))
print(post.pred.p)