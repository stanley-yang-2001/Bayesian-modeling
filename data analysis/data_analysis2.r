d <- read.csv("launches.csv", header=TRUE, stringsAsFactors=TRUE)

levels(d$Vehicle)
d2 <- list(success = d$Success,
            timescaled = as.vector(scale(d$SinceFirst, scale=2*sd(d$SinceFirst))),
            vehicle = unclass(d$Vehicle)) # nolint

inits2 <- list(list(betaintercept=10, betaslope=10, sigmavehicle= 10),
               list(betaintercept=10, betaslope=-10, sigmavehicle=10),
               list(betaintercept=-10, betaslope=10, sigmavehicle=0.001),
               list(betaintercept=-10, betaslope=-10, sigmavehicle=0.001))



library(rjags)
load.module("dic")
m2 <- jags.model("model2.bug", d2, inits2, n.chains=4, n.adapt=4000)
update(m2, 4000)
x2 <- coda.samples(m2, c("betaintercept", "betaslope", "sigmavehicle"), n.iter=63000) # nolint, add thin=xxx here for thinning, n.iter affects the effective size
print(gelman.diag(x2, autoburnin=FALSE))
#plot(x2, smooth=FALSE)

x2 <- coda.samples(m2, c("betaintercept", "betaslope", "sigmavehicle", "epilson"), n.iter=63000) # nolint, add thin=xxx here for thinning, n.iter affects the effective size
print("effective size: ")
result <- effectiveSize(x2)
print(result)

# 4.c
print(summary(x2)$statistics)
print(summary(x2)$quantile)

# 4.d
par(mfrow=c(1,1))
densplot(x2[,38])
# based on the plot, it seems that not all launch vehicles are equally reliable. Some of the vehicle has distribution crossing over to negative, whcih isn't a good sign of positive correlation

# 4.e
print(dic.samples(m2, 100000))
# mean: 270.4, 
# penalty: 20.85, 
# penalized deviance: 291.4

# 4.f
# check over epilson[1:35] to see which has the most max
# indices will gives the column that has the max for each row/observation
# By the structure of the model, the epilson corresponds to the unclass(vehicle[i])
# this function maps the vehicle type to numerical value from 1 to n, where n = the total amount of unique vehicle type
# in alphabetical order. Example: if the first item in the data file is "xxx", and it is listed as 17 in the unclass, then we
# have epilson[vehicle(1)] = epilson[17].

# epilson[vehicle[i]]
epilson <- as.matrix(x2)[, paste0("epilson[",1:35 , "]")]
indices <- apply(epilson, 1, which.max)
#print(indices)
print(dim(epilson))
vehicle.table <- table(indices)     # nolint create table that gather occurrance of each index
print(vehicle.table)
reliable.id <- as.numeric(names(which.max(vehicle.table))) # nolint give the index with most occurance
print(reliable.id)
print(mean(indices == reliable.id))
print("the most reliable vehicle in this model is: ")
print(levels(d$Vehicle)[reliable.id])