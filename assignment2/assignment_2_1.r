
set.seed(123)

# part 1.a
# part i
n <- 1000
alpha <- rexp(n, 0.001)
beta <- rexp(n, 0.001)

#plot(log(alpha), log(beta), xlab = "log(a)", ylab = "log(b)")

# part ii
theta.1.a <-rbeta(n, alpha, beta)
#hist(theta.1.a,  xlab = "theta", main = "Histogram of theta")

#part 1.b
#part i

phi.1 <- runif(n,0,1)    # recall that n = 1000 iter
phi.2 <- rexp(n, 0.001)

# using a for alpha and b for beta
a <- phi.1/(phi.2^2)
b <- (1-phi.1)/(phi.2^2)

# scatter plot of log(a) and log(b)
plot(log(a), log(b), xlab = "log(a)", ylab = "log(b)")

# part ii
theta.1.b <-rbeta(n, alpha, beta)
hist(theta.1.b,  xlab = "theta", main = "Histogram of theta")