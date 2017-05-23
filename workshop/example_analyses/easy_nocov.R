library(DSsim)
library(ltdesigntester)
library(dsm)
library(Distance)
library(plyr)
library(shapefiles)

# set true abundance
true_N <- 2000

# number of sims to do
nsim <- 1#200 # just do 1 for example, in practice 100s are appropriate

# density surface setup
# resolution of the density surface
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
# need to setup rotated columns in the prediction matrix too
# rotation matrix
R <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)),2,2)
# rotate predictions
pred_dat1[,c("xr","yr")] <- t(R %*% t(pred_dat1[,c("x","y")]))


# setup detection function
# hazard-rate ("hn" would give half-normal)
# shape/scale are on the log scale
df <- list(key        = "hr",
           scale      = 0.025,
           shape      = 3,
           truncation = 0.05)


# setup densities
# values that go into density.surface
densities <- list()
densities[["f"]] <- 1
densities[["lr"]] <- 5*(density.surface$x/3)
densities[["rl"]] <- rev(densities[["lr"]])

# stratification schemes
stratification <- c(1, 2)

# we can supply an arbitrary aggregation of the segments into
# transects, here we just group in a "logical" way
transects <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
               2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4,
               4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5,
               6, 6, 6, 6, 6, 6, 6, 6, 6)


# set the density
density.surface$density <- densities[["f"]]

# build simulation setup
ss <- build_sim("manyzigzags",
                dsurf=density.surface,
                n_grid_x=n_grid_x, n_grid_y=n_grid_y,
                n_pop=true_N, df=df,
                region="region2/data")


# plot the setup to check everything went okay
check_sim_setup(ss)

# run the simulation nsim times (this can take a LONG time!)
big_res <- do_sim(nsim, ss, pred_dat1, stratification,
                  transect_id=transects)

# write out the results
save(big_res, file="results.RData")


