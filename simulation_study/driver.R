# drive the sims



library(DSsim)
library(devtools)
load_all("~/current/dsm")
library(Distance)
library(handy2)
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
df <- list()
df[["good"]] <- list(key        = "hr",
                     scale      = 0.03,
                     shape      = 3,
                     truncation = 0.05)
df[["bad"]] <- list(key        = "hr",
                    scale      = 0.01,
                    shape      = 1.1,
                    truncation = 0.02)


# setup densities
densities <- list()
densities[["f"]] <- 1
densities[["lr"]] <- density.surface$x
densities[["rl"]] <- rev(density.surface$x)

# stratification schemes
stratification <- list()
stratification[["zzl"]] <- 1.5
stratification[["manyzigzags"]] <- c(1, 2)
stratification[["iwc"]] <- c(0.5, 2)

## build the simulation scenarios
scenarios <- expand.grid(density = c("lr","rl","f"),
                         design  = c("zzl","manyzigzags","iwc"),
                         df      = c("good","bad"),#"lr","rl"),
                         stringsAsFactors=FALSE)


for(iii in 1:nrow(scenarios)){


  this_set <- scenarios[iii,,drop=FALSE]

  # print scenario name
  scenario_name <- paste(apply(this_set, 2, as.character), collapse="-")

  cat("Scenario:", scenario_name, "\n")

  # set the density
  density.surface$density <- densities[[this_set$density]]

  # build simulation setup
  ss <- test_dssim(paste0("../shapes/", this_set$design),
                   density.surface,
                   n_grid_x=n_grid_x, n_grid_y=n_grid_y,
                   n_pop=true_N, df=df[[this_set$df]],
                   region="../shapes/region2/data")
#check.sim.setup(ss)
  source("test.R")

  # write out the results
  # getting the filename here is ludicrous
  save(big_res, file=paste0(scenario_name, ".RData"))
}



