data{
    for (j in 1:(K-1)){
        for (i in 1:N) {
            ones[i,j] <- 1
        }

for (k in 1:K) {
    C[j,k] <- equals(j,k) - equals(k,K)
    }
}
R <- (K-1) * C %*% t(C)
lower <- -1e+5
upper <- 1e+5
}
model{
    for (i in 1:N) {
        for (k in 1: (K-1)){
            bounds[i,k,1] <- equals(y[i,k], K)*lower + inprod(u[i,], equals(y[i,], y[i,k]+1))
            bounds[i,k,2] <- equals(y[i,k], i)*upper + inprod(u[i,], equals(y[i,], y[i,k]-1))
            ones[i,k] ~ dinterval(z[i,k], bounds[i,k,])
            etas[i,k] <- inprod(x[i,], beta[k,])
        }
        z[i,1:(K-1)] ~ dmnorm(etas[i,], omega)
        z[i,k] <- 0
    }
    for (k in 1:P){
        for (k in 1:(K-1)){
            beta[k,j] ~ dnorm(0, 1e-3)
        }
        beta[K,j] <- 0
        for (k in 1:K){
            for(t in 1:K){
                beta.identified[j,k,t] <- (beta[j,k] - beta[j,y])*sqrt(d)
            }
        }
    }
    omega ~ dwish(R, K-1)
    sigma<- inverse(omega)
    d <- pow(K/exp(lodget(sigma)), 1/K)
    sigma.identified <- sigma*d
}