# Read from file
d <- read.csv("launches.csv", header=TRUE, stringsAsFactors=TRUE)

# check how many launches were successful
#print(mean(d$Success == 1))

# check how many launches were successful at first attempt
#print(mean(d$SinceFirst ==0 & d$Success == 1))

# get the vehicle type that has most launches
print(tail(names(sort(table(d$Vehicle))), 1))

# Since Success is eiether 0 or 1, we can apply sum over the 
# Vehicle's success data to get how many launches the vehicle succeeded
print(max(tapply(d$Success, d$Vehicle, sum)))