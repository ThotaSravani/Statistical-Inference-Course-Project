# First we will set the seed
set.seed(3)

# we set the required constants to the corresponding values
lambda <- 0.2
num_sim <- 1000
sample_size <- 40
sim <- matrix(rexp(num_sim*sample_size, rate=lambda), num_sim, sample_size)
row_means <- rowMeans(sim)

## ------------------------------------------------------------------------
# plot the histogram of averages
hist(row_means, breaks=50, prob=TRUE,
     main="Distribution Of Averages Of Samples Drawn From
    Exponential Distribution With lambda=0.2",
     xlab="Mean")

# Density of the averages of samples
lines(density(row_means))

# Theoretical center of distribution
abline(v=1/lambda, col="red")

# Theoretical density of the averages of samples
xfit <- seq(min(row_means), max(row_means), length=100)
yfit <- dnorm(xfit, mean=1/lambda, sd=(1/lambda/sqrt(sample_size)))
lines(xfit, yfit, pch=22, col="red", lty=2)

# Add legend
legend('topright', c("Simulation", "Theoretical"), lty=c(1,2), col=c("black", "red"))


## ------------------------------------------------------------------------
qqnorm(row_means); qqline(row_means)


## ------------------------------------------------------------------------
lambda_vals <- seq(4, 6, by=0.01)
coverage <- sapply(lambda_vals, function(lamb) {
  mu_hats <- rowMeans(matrix(rexp(sample_size*num_sim, rate=0.2),
                             num_sim, sample_size))
  ll <- mu_hats - qnorm(0.975) * sqrt(1/lambda**2/sample_size)
  ul <- mu_hats + qnorm(0.975) * sqrt(1/lambda**2/sample_size)
  mean(ll < lamb & ul > lamb)
})

library(ggplot2)
qplot(lambda_vals, coverage) + geom_hline(yintercept=0.95)