# get the quantile that true N occurs at in the estimated
# distribution of Nhat
get_N_quantile <- function(N, Nhat, cvN){

  meantrans <- function(m, cv) log(m)-0.5*log(cv^2+1)
  setrans <- function(cv) sqrt(log(cv^2+1))

  plnorm(N, meantrans(Nhat, cvN), setrans(cvN))
}

