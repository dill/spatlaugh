# an example in self-confidence

# quick plot to show what those plots should look like in different
# situations

library(ggplot2)

# number of reps
n <- 1000


# when things go "right" -- flat distribution
dat <- data.frame(scenario = "ok",
                  quantile = runif(n))


# spike on the left => negative bias
nb <- rexp(2*n, 2)
nb <- nb[nb<1 & nb >0]
nb <- nb[1:1000]
dat <- rbind(dat,
             data.frame(scenario = "-ve bias",
                        quantile = nb))


# spike on the right => positive bias
nb <- 1-rexp(2*n, 2)
nb <- nb[nb<1 & nb >0]
nb <- nb[1:1000]
dat <- rbind(dat,
             data.frame(scenario = "+ve bias",
                        quantile = nb))

# what we'd like to see
nb <- rnorm(2*n, mean=0.5, sd=0.35)
nb <- nb[nb<1 & nb >0]
nb <- nb[1:1000]
dat <- rbind(dat,
             data.frame(scenario = "conservative",
                        quantile = nb))

p <- ggplot(dat) +
      geom_histogram(aes(quantile), binwidth=0.05)+
      scale_x_continuous(limits=c(0,1))+
      facet_wrap(~scenario, nrow=1) +
      theme_minimal()

print(p)

ggsave(p, file="selfconfidence.pdf", width=12, height=4)
