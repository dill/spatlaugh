# drive the sims



library(DSsim)
#library(devtools)
#load_all("~/current/dsm")
library(dsm)
library(Distance)
#library(handy2)
library(plyr)
library(designtester)


true_N <- 500
nsim <- 2#250

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
w_trunc <- 0.06
df <- list()
df[["good"]] <- list(key        = "hr",
                     scale      = 0.025,
                     shape      = 3,
                     truncation = w_trunc)
df[["bad"]] <- list(key        = "hr",
                    scale      = 0.005,
                    shape      = 1,
                    truncation = w_trunc)


stratification <- 1.5

# set the density
density.surface$density <- 5*(density.surface$x/3)

# build simulation setup -- good
ss_good <- build_sim("../shapes/manyzigzags",
                     dsurf=density.surface,
                     n_grid_x=n_grid_x, n_grid_y=n_grid_y,
                     n_pop=true_N, df=df[["good"]],
                     region="../shapes/region2/data", n_sim=1)
ss_bad <- build_sim("../shapes/manyzigzags",
                     dsurf=density.surface,
                     n_grid_x=n_grid_x, n_grid_y=n_grid_y,
                     n_pop=true_N, df=df[["bad"]],
                     region="../shapes/region2/data", n_sim=1)

cov_dat <- build_sim_covar(list(ss_good, ss_bad), logit_scale=0.1,
                           logit_location=1.5)

segs <- cov_dat$segs
obs <- cov_dat$obs
dist <- cov_dat$dist

library(Distance)
library(mrds)
dd <- ds(dist, truncation=w_trunc, formula=~weather, key="hr")

mm <- dsm(count~s(x,y), obs=obs, ddf=dd, segment.data=segs, family=tw(), method="REML")

vis.gam(mm,plot.type="contour", view=c("x","y"),asp=1)
