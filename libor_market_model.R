
# LIBOR Market Model Calibration and Simulation

# Step 1: Create a hypothetical data set
maturities = seq(1, 10, by=1)
set.seed(123)
hypothetical_vols = 0.02 + 0.005 * maturities + rnorm(10, 0, 0.002)
data = data.frame(maturities, hypothetical_vols)
print(data)

# Step 2: Calibration using optim
objective_function <- function(sigma, vols) {
    return(sum((sigma - vols)^2))
}
initial_guess = 0.03
result = optim(initial_guess, objective_function, vols = hypothetical_vols)
calibrated_vol = result$par
print(calibrated_vol)

# Step 3: Model dynamics using Euler-Maruyama method
T = 1
dt = 0.01
n = T/dt
forward_rate = rep(0.03, n)
set.seed(123)
for (i in 2:n) {
    dW = rnorm(1, 0, sqrt(dt))
    forward_rate[i] = forward_rate[i-1] + calibrated_vol * forward_rate[i-1] * dW
}
plot(seq(0, T-dt, by=dt), forward_rate, type='l', xlab='Time', ylab='Forward Rate', main='Simulated Forward Rate using LMM')
