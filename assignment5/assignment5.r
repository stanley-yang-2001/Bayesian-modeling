# part a
d <- read.csv("pigweights.csv", header=TRUE)
data_matrix <- as.matrix(d[,-1])
subjects <- d$id
weeks <- colnames(d)[-1]

# Plot each subject's data over weeks
#matplot(t(data_matrix), type = "l", lty = 1, col = 1:nrow(d),
#        xaxt = "n", xlab = "Week", ylab = "Weights", main = "Pig Weights over Weeks")

# Customize x-axis labels
#axis(1, at = 1:length(weeks), labels = weeks)

#part b
week <- 1:9
week_ave <- mean(week)

betahat <- matrix(NA, nrow(d), 2)
for(j in 1:nrow(d))
    betahat[j,] <- lsfit(week-week_ave, t(d[j, -1]))$coef
#plot(betahat)
#print(apply(betahat, 2, mean))
#print(var(betahat))
#print(cor(betahat))

# part c

d1 <- list(weight = d[,-1], week = week,
            mubeta0 = c(0,0),
            Sigmamubetainv=rbind(c(0.000001, 0),
                                c(0, 0.000001)),
              Sigma0 = rbind(c(15,0),
                            c(0,0.5)))
inits1 <- list(list(sigmasqyinv = 10, mubeta = c(1000,1000),
                    Sigmabetainv = rbind(c(100,0), 
                                        c(0, 100)),.RNG.name="base::Wichmann-Hill",.RNG.seed=123),
                list(sigmasqyinv = 0.001, mubeta = c(-1000,1000),
                    Sigmabetainv = rbind(c(100,0), 
                                        c(0, 100)),.RNG.name="base::Wichmann-Hill",.RNG.seed=456),
                list(sigmasqyinv = 10, mubeta = c(1000,-1000),
                    Sigmabetainv = rbind(c(0.001,0), 
                                        c(0, 0.001)),.RNG.name="base::Wichmann-Hill",.RNG.seed=789),
                list(sigmasqyinv = 0.001, mubeta = c(-1000,-1000),
                    Sigmabetainv = rbind(c(0.001,0), 
                                        c(0, 0.001)),.RNG.name="base::Wichmann-Hill",.RNG.seed=108))
library(rjags)
load.module("dic")
m1 <- jags.model("assignment5.bug", d1, inits1, n.chains = 4, n.adapt = 1000)
update(m1, 5000)
x1 <- coda.samples(m1, c("mubeta","Sigmabeta","sigmasqy","rho"),
                   n.iter=2000)
print(gelman.diag(x1, autoburnin=FALSE, multivariate=FALSE))
print("summary and quantile")
print(summary(x1)$statistics)
print(summary(x1[,c("rho")])$quantile)

result <- effectiveSize(x1[,c("mubeta[1]","mubeta[2]","Sigmabeta[1,1]","Sigmabeta[1,2]",
                    "Sigmabeta[2,2]","sigmasqy","rho")])
print("result)")
print(result)

mubeta <-as.matrix(x1)[, paste0("mubeta[", 1:2, "]")]
expect.week <- 1
pop.mean.reg <- mubeta[,1]+mubeta[,2]*(expect.week - week_ave) # week_Ave defined earlier

print("population mean quantile")
print(quantile(pop.mean.reg, c(0.025, 0.975)))
sigmasq.beta.1 <-as.matrix(x1)[, paste0("Sigmabeta[1,1]")]
rho.sigmabeta1.sigmabeta2 <-as.matrix(x1)[, paste0("Sigmabeta[1,2]")]
sigmasq.beta.2 <-as.matrix(x1)[, paste0("Sigmabeta[2,2]")]

pop.var <- sigmasq.beta.1 + 2*(expect.week - week_ave)*rho.sigmabeta1.sigmabeta2 + (expect.week - week_ave)^2*sigmasq.beta.2
print("population variance quantile")
print(quantile(pop.var, c(0.025, 0.975)))
rho <- as.matrix(x1)[, paste0("rho")]

x.min <- week_ave - rho*sqrt(sigmasq.beta.1)/sqrt(sigmasq.beta.2)

# H2: x.min < 1, H1: x.min >= 1
post.H2 <- mean(x.min < 1)
post.H1 <- mean(x.min >= 1)
prior.H2 <- 0.205
prior.H1 <- 1-prior.H2
bayesfactor <- (post.H2/post.H1)/(prior.H2/prior.H1)
print("post H2 x min < 1")
print(post.H2)
print("Bayes factor")
print(bayesfactor)

print(dic.samples(m1, 100000))