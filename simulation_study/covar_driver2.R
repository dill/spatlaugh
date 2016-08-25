# drive the sims


library(DSsim)
library(dsm)
library(Distance)
library(plyr)
library(ltdesigntester)


true_N <- 500
nsim <- 200

n_grid_y <- 300
n_grid_x <- 250
density.surface <- expand.grid(x = seq(-2.5, 0, len=n_grid_x),
                               y = seq(0, 3, len=n_grid_y))
# setup the prediction grid
cell_side_y <- 0.05/2.5
cell_side_x <- 0.05/3
pred_dat1 <- expand.grid(y = seq(0, 3, by=cell_side_y),
                         x = seq(-2.5, 0, by=cell_side_x))
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


# setup densities
densities <- list()
densities[["f"]] <- 1
densities[["lr"]] <- 5*(density.surface$x/3)
densities[["rl"]] <- rev(densities[["lr"]])

# stratification schemes
stratification <- list()
stratification[["tb"]] <- -1.25

## build the simulation scenarios
scenarios <- expand.grid(density = c("lr","rl","f"),
                         design  = c("tb"),
#                         df      = c("good","bad"),#"lr","rl"),
                         stringsAsFactors=FALSE)


# transect definitions
transects <- list(tb = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
                         2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4,
                         4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5,
                         6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7,
                         7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8))



for(iii in 1:nrow(scenarios)){

  # get this set of settings
  this_set <- scenarios[iii,,drop=FALSE]

  # print scenario name
  scenario_name <- paste(apply(this_set, 2, as.character), collapse="-")

  cat("Scenario:", scenario_name, "\n")

  # set the density
  density.surface$density <- densities[[this_set$density]]

  # build simulation setup -- good
  ss_good <- build_sim(paste0("../shapes/", this_set$design),
                       dsurf=density.surface,
                       n_grid_x=n_grid_x, n_grid_y=n_grid_y,
                       n_pop=true_N, df=df[["good"]],
                       region="../shapes/region_big/data", n_sim=1)
  ss_bad <- build_sim(paste0("../shapes/", this_set$design),
                       dsurf=density.surface,
                       n_grid_x=n_grid_x, n_grid_y=n_grid_y,
                       n_pop=true_N, df=df[["bad"]],
                       region="../shapes/region_big/data", n_sim=1)

  logit_opts <- list(scale=0.2, location=-1.75)

  # run the simulation!
  big_res <- do_sim(nsim, list(ss_good, ss_bad), pred_dat1,
                    stratification[[this_set$design]], logit_opts,
                    transect_id=transects[[this_set$design]])


  # write out the results
  save(big_res, file=paste0(scenario_name, "-covar.RData"))
}


