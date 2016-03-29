# detectability vs gradients

# basic idea is can a spatial model tell the difference between
# a left-right gradient and the case where detectability changes
# between the left and right sides of the survey area (in MVB terms
# "in fog in the dark")

library(DSsim)

source("../check.sim.setup.R")
source("../plot_df.R")
source("../test_dssim.R")
source("../dsmify.R")


n_grid <- 100
density.surface <- expand.grid(x = seq(0, 1, len=n_grid),
                               y = seq(0, 1, len=n_grid))
# left-right
#density.surface$density <- 100*density.surface$x
density.surface$density[density.surface$x <= 0.5] <- 0.05
density.surface$density[density.surface$x > 0.5] <- 0.95

# setup the detection function for "good" conditions
df <- list(key        = "hr",
           scale      = 0.025,
           shape      = 3,
           truncation = 0.05)

set.seed(3141)
ss <- test_dssim("../shapes/manyzigzags", density.surface, n_grid, 200, df,
                 region="../shapes/region/data")

# check that looks good
#check.sim.setup(ss)

## part 0 extract some data

set.seed(3141)
# generate a population
pop <- generate.population(ss)
# generate the transects
transects <- generate.transects(ss)
# simulate the survey process of detection
eg.survey <- create.survey.results(ss, dht.tables=TRUE)


# get the data into shape
dsm_data <- dsmify(eg.survey)
dist.data <- dsm_data$dist
segs <- dsm_data$segs
obs <- dsm_data$obs


# make an indicator thats x>0.5 but we'll pretend that's about
# say, weather
dist.data$weather <- as.numeric(dist.data$x > 0.5)
# fix up the "bad weather" distances to be on the line
# i.e., we can't see anything
dist.data$distance[dist.data$weather==0] <- dist.data$distance[dist.data$weather==0]/20


library(dsm)
library(Distance)

## part 1 -- there _is_ a gradient
hr.model <- ds(dist.data, key="hr")
mod_grad <- dsm(Nhat~s(x,y), hr.model, segs, obs)


## part 2 -- it's all about detectability


hr.model_w <- ds(dist.data, key="hr", formula=~as.factor(weather))
mod_weather <- dsm(Nhat~s(x,y), hr.model_w, segs, obs)




## predictions

pred_dat1 <- expand.grid(x = seq(0, 1, by=0.1),
                        y = seq(0, 1, by=0.1))


pred_grad <- predict(mod_grad, pred_dat1, off.set=0.1^2)
pred_weather <- predict(mod_weather, pred_dat1, off.set=0.1^2)


pred_dat <- rbind(pred_dat1, pred_dat1)
pred_dat$N <- c(pred_grad, pred_weather)
pred_dat$type <- c(rep("grad", nrow(pred_grad)),
                   rep("weather", nrow(pred_weather)))

library(ggplot2)
library(viridis)

p <- ggplot(pred_dat, aes(x=x, y=y)) +
      geom_tile(aes(fill=N, width=0.1, height=0.1)) +
      scale_fill_viridis() +
      facet_wrap(~type)
print(p)


# uncertainty

cv_grad <- dsm.var.gam(mod_grad, pred_dat1, off.set=0.1^2)
cv_weather <- dsm.var.gam(mod_weather, pred_dat1, off.set=0.1^2)


pred_dat$var <- c(dsm.var.gam(mod_grad, split(pred_dat1, 1:nrow(pred_dat1)),
                             off.set=0.1^2)$pred.var,
                  dsm.var.gam(mod_weather, split(pred_dat1, 1:nrow(pred_dat1)),
                              off.set=0.1^2)$pred.var)
pred_dat$cv <- sqrt(pred_dat$var)/pred_dat$N
pred_dat$one <- sqrt(pred_dat$var) + 0*pred_dat$N
pred_dat$mone <- 0*pred_dat$N - sqrt(pred_dat$var)

dev.new()
p <- ggplot(pred_dat, aes(x=x, y=y)) +
      geom_tile(aes(fill=one, width=0.1, height=0.1)) +
      scale_fill_viridis(limits=c(-20,20)) +
      facet_wrap(~type)
#print(p)

p + geom_path(data=segs) + geom_point(data=obs)


dev.new()
p <- ggplot(pred_dat, aes(x=x, y=y)) +
      geom_tile(aes(fill=mone, width=0.1, height=0.1)) +
      #scale_fill_viridis(limits=c(0,40)) +
      scale_fill_viridis(limits=c(-20,20)) +
      facet_wrap(~type)
#print(p)

p + geom_path(data=segs) + geom_point(data=obs)


