# more complicated example with a detection level covariate

library(DSsim)
library(ltdesigntester)
library(shapefiles)
library(dsm)
library(Distance)
library(plyr)

# true abundance
true_N <- 5000
# number of simulations to do
nsim <- 1#200 # set to 1 while testing

# setup the true density surface grid
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


# setup densities, these populate density.surface
my_density <- 1
#my_density <- 5*(density.surface$x/3)
#my_density <- rev(densities[["lr"]])

# stratification scheme
stratification <- c(1, 2)

# we can supply an arbitrary aggregation of the segments into
# transects, here we just group in a "logical" way
transects <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
               2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4,
               4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5,
               6, 6, 6, 6, 6, 6, 6, 6, 6)



# set the density
density.surface$density <- my_density


# build simulation setup -- good
ss_good <- build_sim("manyzigzags",
                     dsurf=density.surface,
                     n_grid_x=n_grid_x, n_grid_y=n_grid_y,
                     n_pop=true_N, df=df[["good"]],
                     region="region2/data")
# build simulation setup -- bad
ss_bad <- build_sim("manyzigzags",
                    dsurf=density.surface,
                    n_grid_x=n_grid_x, n_grid_y=n_grid_y,
                    n_pop=true_N, df=df[["bad"]],
                    region="region2/data")

# the change in the covariates is dictated by the logistic function here
# this is setup to change halfway across the study area
logit_opts <- list(scale=0.1, location=1.5)

# run the simulation!
# supply the simulations setups as a list
# you can check each using check_sim_setup() as before
big_res <- do_sim(nsim, list(ss_good, ss_bad), pred_dat1,
                  stratification, logit_opts,
                  transect_id=transects)


# write out the results
save(big_res, file="results-covar.RData")


