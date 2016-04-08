# drive the sims



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
nsim <- 500

n_grid_x <- 300
n_grid_y <- 100
density.surface <- expand.grid(x = seq(0, 3, len=n_grid_x),
                               y = seq(0, 1, len=n_grid_y))
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
for(iii in 2){
  if(iii==1){
    # left-right
    density.surface$density <- rev(density.surface$x)
  }else if(iii==2){
    # right-left
    density.surface$density <- density.surface$x
  }else if(iii==3){
    # flat
    density.surface$density <- 1
  }
  # build simulation setup
  ss <- test_dssim("../shapes/zzl", density.surface, n_grid_x=n_grid_x,
                   n_grid_y=n_grid_y, n_pop=true_N, df=df_good,
                   region="../shapes/region2/data")
  source("test.R")
  if(iii==1){
    # left-right
  save(big_res, file="lr.RData")
  }else if(iii==2){
    # right-left
  save(big_res, file="rl.RData")
  }else if(iii==3){
    # flat
  save(big_res, file="f.RData")
  }
}



