library(DSsim)

source("check.sim.setup.R")
source("plot_df.R")
source("test_dssim.R")


# build a density surface
n_grid <- 100
density.surface <- expand.grid(x = seq(0, 1, len=n_grid),
                               y = seq(0, 1, len=n_grid))
# flat
#density.surface$density <- 1
# top to bottom
#density.surface$density <- density.surface$y
# left-right
density.surface$density <- density.surface$x
# diagonal
#density.surface$density <- density.surface$y + density.surface$x


# make it a density?
density.surface$density <- density.surface$density/sum(density.surface$density)



# zig
#ss <- test_dssim("shapes/zig", density.surface, n_grid)

# lots on the left
ss <- test_dssim("shapes/leftie", density.surface, n_grid)

check.sim.setup(ss)



