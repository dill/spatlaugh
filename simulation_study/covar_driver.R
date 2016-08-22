# drive the sims



library(DSsim)
#library(devtools)
#load_all("~/current/dsm")
library(dsm)
library(Distance)
#library(handy2)
library(plyr)
library(ltdesigntester)


true_N <- 500

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
ss_good <- build_sim("../shapes/zzl",
                     dsurf=density.surface,
                     n_grid_x=n_grid_x, n_grid_y=n_grid_y,
                     n_pop=true_N, df=df[["good"]],
                     region="../shapes/region2/data", n_sim=1)
ss_bad <- build_sim("../shapes/zzl",
                     dsurf=density.surface,
                     n_grid_x=n_grid_x, n_grid_y=n_grid_y,
                     n_pop=true_N, df=df[["bad"]],
                     region="../shapes/region2/data", n_sim=1)


## define some transects (rather than segments)
## !!! this is not general yet
aa <- create.survey.results(ss_good, TRUE)
bb <- aa@transects@sampler.info
se <- bb$start.Y==max(bb$start.Y) | bb$end.Y==max(bb$end.Y) |
      bb$start.Y==min(bb$start.Y) | bb$end.Y==min(bb$end.Y)

# get number of segments per transect
se_mat <- matrix(which(se), ncol=2,byrow=TRUE)
tr_n <- apply(se_mat, 1, diff)
# make the labels
tr_id <- rep(1:nrow(se_mat), tr_n+1)


logit_opts <- list(scale=0.1, location=1.5)

# run the simulation!
cov_dat <- do_sim(5, list(ss_good, ss_bad), pred_dat1,
                  stratification, logit_opts, tr_id)


