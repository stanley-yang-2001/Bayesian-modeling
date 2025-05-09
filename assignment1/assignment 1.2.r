cat(rep("\n", 500))

# processing the text file
file_path <- "Hbjebw6aT0uuBcW56XZpnQ_0fb0ee5611d74af38b56e997374972f1_randomwikipedia.txt"
data <- read.csv(file_path, header = FALSE)

# Remove the first row since there's problem with the header and the table
data <- data[-1, ]

# processing the data, changing string to integers and adding them to a new data table df
df <- data.frame()
for(x in 1:20){
    split_string <-strsplit(data[x], "\\s+")[[1]]
    split_integers <- as.integer(split_string)
    df <- rbind(df, split_integers)
}

# creating column name
colnames(df) <- c("Index", "PageID", "Bytes")


#hist(df$Bytes)

#hist(log(df$Bytes))

#sample mean
ybar <- mean(log(df$Bytes))
#print(ybar)

#sample variance
s.2 <- var(log(df$Bytes))

# assign variables
sigma.2 <- s.2
tau.2.0 = sigma.2
n <- 20
mu.n <- ybar
tau.2.n <- s.2/n
mu0 = mu.n

#c.2

#curve(dnorm(mu, mu.n, sqrt(tau.2.n)), 3, 12, xname="mu", n = 1000) # posterior
#abline(h = 1, col = "darkgreen", lty=2) #flat prior proportional to 1  
#legend("topright", c("prior", "posterior"), lty=2:1)


# log scale
#result <-qnorm(c(0.025,0.975), mu.n, sqrt(tau.2.n))
#print(result)

# original scale
#result.original <- exp(result)
#print(result.original)

post.sigma.2.sim <- (n-1) * s.2 / rchisq(1000, n-1)
post.mu.sim <- rnorm(1000, ybar, sqrt(post.sigma.2.sim / n))
print(quantile(post.mu.sim, c(0.025,0.975)))
print(quantile(post.sigma.2.sim, c(0.025,0.975)))



#e
# sample mean
ybar <- mean(df$Bytes)

#sample variance
s.2 <- var(df$Bytes)

post.sigma.2.sim <- (n-1) * s.2 / rchisq(1000000, n-1)
post.mu.sim <- rnorm(1000000, ybar, sqrt(post.sigma.2.sim / n))
post.pred.sim <- rnorm(1000000, post.mu.sim, sqrt(post.sigma.2.sim))

print(quantile(post.pred.sim, c(0.025,0.975)))
 print(min(df$Bytes))
result <- mean(post.pred.sim < min(df$Bytes))
data.20 <- result^20
print(result)
print(data.20)