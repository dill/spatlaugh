##



library(DSsim)
library(devtools)
load_all("~/current/dsm")
library(Distance)
library(handy2)
source("~/Dropbox/varprop/data/dsm.varprop.R")
library(plyr)


source("../check.sim.setup.R")
source("../plot_df.R")
source("../test_dssim.R")
source("../dsmify.R")
source("quick_dht.R")
source("get_N_quantile.R")

true_N <- 200

n_grid_x <- 300
n_grid_y <- 100
density.surface <- expand.grid(x = seq(0, 3, len=n_grid_x),
                               y = seq(0, 1, len=n_grid_y))
# left-right
#density.surface$density <- rev(density.surface$x)
# right-left
#density.surface$density <- density.surface$x
# flat
density.surface$density <- 1

# setup the prediction grid
cell_side_x <- 0.05
cell_side_y <- 0.05/3
pred_dat1 <- expand.grid(x = seq(0, 3, by=cell_side_x),
                         y = seq(0, 1, by=cell_side_y))
pred_dat1$off.set <- cell_side_x*cell_side_y


# rotation matrix
R <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)),2,2)
# rotate predictions
pred_dat1[,c("xr","yr")] <- t(R %*% t(pred_dat1[,c("x","y")]))


# setup detection function
df_good <- list(key        = "hr",
                scale      = 0.03,
                shape      = 3,
                truncation = 0.05)

# build simulation setup
ss <- test_dssim("../shapes/zzl", density.surface, n_grid_x=n_grid_x,
                 n_grid_y=n_grid_y, n_pop=true_N, df=df_good,
                 region="../shapes/region2/data")
check.sim.setup(ss)

big_res <- c()
nsim <- 500

for(ii in 1:nsim){

  # generate a survey
  survey_res <- create.survey.results(ss, dht.tables=TRUE)
  # put the data in dsm format
  dsm_data <- dsmify(survey_res)
  dist.data <- dsm_data$dist
  obs <- dsm_data$obs
  segs <- dsm_data$segs
  # rotated segs
  segs[,c("xr","yr")] <- t(R %*% t(segs[,c("x","y")]))

  # fit a detection function
  hr.model <- ds(dist.data, key="hr", adjustment=NULL)

  # model list object
  ll <- list()

  # thin plate
  ll[["m_xy_tp"]] <- dsm(count~s(x, y, bs="tp"), hr.model, segs, obs,
                         method="REML", family=tw(a=1.2))
  # thin plate te
  ll[["m_xy_te"]] <- dsm(count~te(x, y, bs="tp"), hr.model, segs, obs,
                         method="REML", family=tw(a=1.2))
  # thin plate te
  ll[["m_xyr_te"]] <- dsm(count~te(xr, yr, bs="tp"), hr.model, segs, obs,
                          method="REML", family=tw(a=1.2))
  # thin plate rotation
  ll[["m_xyr_tp"]] <- dsm(count~s(xr, yr, bs="tp"), hr.model, segs, obs,
                          method="REML", family=tw(a=1.2))
  # thin plate w/ shrinkage
  ll[["m_xy_ts"]] <- dsm(count~s(x, y, bs="ts"), hr.model, segs, obs,
                         method="REML", family=tw(a=1.2))
  # Duchon
  ll[["m_xy_ds"]] <- dsm(count~s(x, y, bs="ds", m=c(1, 0.5)), hr.model, segs, obs,
                         method="REML", family=tw(a=1.2))


  # process -- get N and CVs for the spatial models
  all_res <- ldply(ll, function(x){
    xx <- dsm.varprop(x, pred_dat1)
    N <- sum(xx$pred)
    cv <- sqrt(xx$var[1,1])/N
    return(c(N, cv))
  })

  # get N and CVs for the HT model
  HT <- quick_dht(hr.model, survey_res)

  # bind them together
  all_res <- rbind.data.frame(all_res, c("HT", unname(HT)))

  # get the quantiles
  qs <- apply(all_res[,-1], 1,
              function(x) get_N_quantile(N=true_N, Nhat=x[1], cv=x[2]))

  # build some storage for this set of results
  res <- data.frame(names    = all_res[,1],
                    iter     = ii,
                    quantile = qs)

  # bind to the rest
  big_res <- rbind(big_res, res)
}


library(ggplot2)
ggplot(big_res) + geom_histogram(aes(quantile)) + facet_wrap(~names)


