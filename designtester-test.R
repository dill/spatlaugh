library(DSsim)
library(designtester)

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
df <- list(key        = "hr",
           scale      = 0.025,
           shape      = 3,
           truncation = 0.05)

ss <- build_sim("shapes/leftie", region_path="shapes/region2/data", dsurf=density.surface, n_grid=n_grid, df=df)

check_sim_setup(ss)



