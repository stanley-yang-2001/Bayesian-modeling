# Load necessary library
par(mfrow = c(1, 2))  # Set plotting area to show two plots side by side

# Set up simulation size
n.sim <- 1000 

# Example of FAST mixing (low autocorrelation)
set.seed(42)
fast_mixing <- arima.sim(n = n.sim, model = list(ar = 0.1))  # Weak AR(1) process
acf(fast_mixing, main = "Autocorrelation - Fast Mixing")

# Example of SLOW mixing (high autocorrelation)
set.seed(42)
slow_mixing <- arima.sim(n = n.sim, model = list(ar = 0.95))  # Strong AR(1) process
acf(slow_mixing, main = "Autocorrelation - Slow Mixing")