# Aim: to generate the number of samples required, given that the samples are random

mean.untreat <- 4 # mean for the untreated
variance <- 2 # when you increase variance, the number of samples is likely to increase
changeToDetect <- 0.1 # how many percent different
effect <- 5*changeToDetect # effect size to detect
numSimul <- 1000 # number of simulations from a random sample
sampleSize <- 50 # sample size for each replicate

power <- rep(0, numSimul) # sequence for power values

for (i in 1:numSimul) {
sample.untreat <- rnorm(sampleSize, mean = mean.untreat, sd=variance^0.5)
sample.treat <- rnorm(sampleSize, mean = mean.untreat+effect, sd=variance^0.5)
res <- t.test(sample.untreat, sample.treat)
pval <- res$p.value
power[i] <- pval <= 0.05}

empPower = sum(power)/numSimul # sample size required to see the effect

print(empPower)