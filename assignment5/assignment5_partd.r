d <- read.csv("pigweights.csv", header=TRUE)
data_matrix <- as.matrix(d[,-1])
subjects <- d$id
weeks <- colnames(d)[-1]

week <- 1:9
week_ave <- mean(week)


d2 <- list(weight = d[,-1], week = week)
print(week)
print(dim(d[,-1]))
b <- week - week_ave

# initialize the chain for mubeta1 and 2, sigmabeta 1 and sigmasqinv
inits2 <- list(list(sigmasqyinv = 10, mubeta1 = 1000, mubeta2 = 1000, sigmabeta1 = 1000, sigmabeta2=1000),
                list(sigmasqyinv = 0.001, mubeta1 = -1000, mubeta2 = 1000, sigmabeta1 = 0.0001, sigmabeta2=1000),
                list(sigmasqyinv = 10, mubeta1 = 1000, mubeta2 = -1000, sigmabeta1 = 1000, sigmabeta2=0.0001),
                list(sigmasqyinv = 0.001, mubeta1 = -1000, mubeta2 = -1000, sigmabeta1 = 0.0001, sigmabeta2=0.0001))
library(rjags)
load.module("dic")
m2 <- jags.model("assignment5_model2.bug", d2, inits2, n.chains = 4, n.adapt = 7000)
update(m2, 20000)
x2 <- coda.samples(m2, c("mubeta1","mubeta2","sigmasqy","sigmabeta1","sigmabeta2"),
                   n.iter=3000)
print(gelman.diag(x2, autoburnin=FALSE, multivariate=FALSE))
print("stat")
print(summary(x2)$statistics)
#densplot(x1[,c("rho")])
result <- effectiveSize(x2[,c("mubeta1","mubeta2","sigmasqy","sigmabeta1","sigmabeta2")])
print(result)
mubeta1 <- as.matrix(x2)[, paste0("mubeta1")]
mubeta2 <- as.matrix(x2)[, paste0("mubeta2")]
expect.week <- 1
pop.mean.reg.2 <- mubeta1+mubeta2*(expect.week - week_ave) 
print("population mean quantile")
print(quantile(pop.mean.reg.2, c(0.025, 0.975)))

print(dic.samples(m2, 100000))