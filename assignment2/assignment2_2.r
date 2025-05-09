set.seed(123)

d <- read.table("thenumbers.txt")

# format the data from the file
d_new <- data.frame(
    y = c(d[, 1], d[, 4], d[, 7]),
    psihat = c(d[, 2], d[, 5], d[, 8]),
    sigma = c(d[, 3], d[, 6], d[, 9])
)
print(d_new)
library(rjags)
#m <- jags.model("asgn2template.bug", d_new)



#update(m, 10000) # 10,000 burn-in
#x1 <- coda.samples(m, c("psi0", "sigmasq0"), n.iter = 100000)

#print(summary(x1))

require(lattice)

#print(densityplot(x1[,c("psi0", "sigmasq0")]))



m2 <- jags.model("asgn2template2.bug", c(as.list(d_new), sigma.tilde=0.125))
update(m2, 10000)
x2 <- coda.samples(m2, c("psihat.tilde", "prob.ind"), n.iter = 100000)
print(summary(x2))