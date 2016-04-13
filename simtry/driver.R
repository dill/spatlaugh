# drive the sims



library(DSsim)
library(devtools)
load_all("~/current/dsm")
library(Distance)
library(handy2)
source("~/current/varprop/data/dsm.varprop.R")
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
df_good_002 <- list(key        = "hr",
                    scale      = 0.02,
                    shape      = 1.5,
                    truncation = 0.02)
df_good_001 <- list(key        = "hr",
                    scale      = 0.01,
                    shape      = 1.1,
                    truncation = 0.01)

scenarios <- list(list(density  = rev(density.surface$x),
                       shape    = "../shapes/zzl",
                       df       = df_good,
                       filename = "lr.RData"),
                  list(density  = density.surface$x,
                       shape    = "../shapes/zzl",
                       df       = df_good,
                       filename = "rl.RData"),
                  list(density  = 1,
                       shape    = "../shapes/zzl",
                       df       = df_good,
                       filename = "f.RData"),
                  list(density  = rev(density.surface$x),
                       shape    = "../shapes/zzl",
                       df       = df_good_002,
                       filename = "rl_002.RData"),
                  list(density  = rev(density.surface$x),
                       shape    = "../shapes/zzl",
                       df       = df_good_001,
                       filename = "rl_001.RData"))
#                  list(density  = 
#                       filename = ),
#                  list(density  = 
#                       filename = ))
#                  list(density  = 
#                       filename = ),


for(iii in seq_along(scenarios)){

  this_set <- scenarios[[iii]]

  # set the density
  density.surface$density <- this_set$density

  # build simulation setup
  ss <- test_dssim(this_set$shape, density.surface, n_grid_x=n_grid_x,
                   n_grid_y=n_grid_y, n_pop=true_N, df=this_set$df,
                   region="../shapes/region2/data")
#check.sim.setup(ss)
  source("test.R")

  # write out the results
  save(big_res, file=this_set$filename)
}



