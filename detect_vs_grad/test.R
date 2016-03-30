# detectability vs gradients

# basic idea is can a spatial model tell the difference between
# a left-right gradient and the case where detectability changes
# between the left and right sides of the survey area (in MVB terms
# "in fog in the dark")

library(DSsim)
library(dsm)
library(Distance)


source("../check.sim.setup.R")
source("../plot_df.R")
source("../test_dssim.R")
source("../dsmify.R")


n_grid <- 100
density.surface <- expand.grid(x = seq(0, 1, len=n_grid),
                               y = seq(0, 1, len=n_grid))
# left-right
#density.surface$density <- 100*density.surface$x
density.surface$density[density.surface$x <= 0.5] <- 0.3
density.surface$density[density.surface$x > 0.5] <- 0.7

# setup the detection function for "good" conditions
df_good <- list(key        = "hr",
                scale      = 0.025,
                shape      = 3,
                truncation = 0.05)
df_bad <- list(key        = "hr",
               scale      = 0.015,
               shape      = 3,
               truncation = 0.05)

ss_good <- test_dssim("../shapes/manyzigzags", density.surface, n_grid, 200,
                      df_good, region="../shapes/region/data")
ss_bad <- test_dssim("../shapes/manyzigzags", density.surface, n_grid, 200,
                      df_bad, region="../shapes/region/data")

# check that looks good
#set.seed(3141)
#check.sim.setup(ss_good)
#dev.new()
#set.seed(3142)
#check.sim.setup(ss_bad)

# okay now generate some data

set.seed(3141)
survey_good <- create.survey.results(ss_good, dht.tables=TRUE)
set.seed(3141)
survey_bad <- create.survey.results(ss_bad, dht.tables=TRUE)

# mash together two surveys
mash <- function(survey1, survey2){


  # get the data into shape
  dat1 <- dsmify(survey1)
  dat2 <- dsmify(survey2)

  segs <- dat1$segs

  labs <- 1:nrow(dat1$segs)
#  pp <- plogis(labs, scale=10, location=5)
  pp <- plogis(labs, scale=2, location=20)

  picker <- as.logical(rbinom(length(labs), 1, pp))
  ones <- (1:nrow(segs))[picker]
  twos <- (1:nrow(segs))[!picker]

  obs <- rbind(dat1$obs[dat1$obs$Sample.Label %in% ones,],
               dat2$obs[dat2$obs$Sample.Label %in% twos,])

  dist <- rbind(dat1$dist[dat1$dist$Sample.Label %in% ones,],
                dat2$dist[dat2$dist$Sample.Label %in% twos,])
  # add in the covariate
  dist$weather <- c(rep(0,sum(dat1$dist$Sample.Label %in% ones)),
                    rep(1,sum(dat2$dist$Sample.Label %in% twos)))
  # remove duplicate observations, perfering data from "survey1"
  dist <- dist[!duplicated(dist$object),]

  return(list(dist=dist, obs=obs, segs=segs))
}

dsm_data <- mash(survey_good, survey_bad)

dist.data <- dsm_data$dist
obs <- dsm_data$obs
segs <- dsm_data$segs


##! dev.new()
##! plot(segs[,c("x", "y")], type="l")
##! text(dist.data[,c("x","y")], lab=dist.data$weather)

## part 1 -- there _is_ a gradient
hr.model <- ds(dist.data, key="hr")
mod_grad <- dsm(Nhat~s(x,y), hr.model, segs, obs, family=tw())


## part 2 -- it's all about detectability


hr.model_w <- ds(dist.data, key="hr", formula=~as.factor(weather))
mod_weather <- dsm(Nhat~s(x,y), hr.model_w, segs, obs, family=tw())




## predictions

cell_side <- 0.1
pred_dat1 <- expand.grid(x = seq(0, 1, by=cell_side),
                         y = seq(0, 1, by=cell_side))


pred_grad <- predict(mod_grad, pred_dat1, off.set=cell_side^2)
pred_weather <- predict(mod_weather, pred_dat1, off.set=cell_side^2)


pred_dat <- rbind(pred_dat1, pred_dat1)
pred_dat$N <- c(pred_grad, pred_weather)
pred_dat$type <- c(rep("grad", nrow(pred_grad)),
                   rep("weather", nrow(pred_weather)))

library(ggplot2)
library(viridis)
library(emoGG)

p <- ggplot(pred_dat, aes(x=x, y=y)) +
      geom_tile(aes(fill=N, width=cell_side, height=cell_side)) +
      scale_fill_viridis() +
      facet_wrap(~type)
print(p)


# uncertainty

cv_grad <- dsm.var.gam(mod_grad, pred_dat1, off.set=cell_side^2)
cv_weather <- dsm.var.gam(mod_weather, pred_dat1, off.set=cell_side^2)


pred_dat$var <- c(dsm.var.gam(mod_grad, split(pred_dat1, 1:nrow(pred_dat1)),
                             off.set=cell_side^2)$pred.var,
                  dsm.var.gam(mod_weather, split(pred_dat1, 1:nrow(pred_dat1)),
                              off.set=cell_side^2)$pred.var)


combine_cv <- function(vargam,predgam, ddf){

  ddf.summary <- summary(ddf)
  cvp.sq <- (ddf.summary$average.p.se/ddf.summary$average.p)^2

  cvgam.sq <- (sqrt(vargam)/predgam)^2
  cv <- sqrt(cvp.sq + cvgam.sq)
  return(cv)
}

pred_dat$cv <- c(combine_cv(pred_dat$var[1:nrow(pred_dat1)],
                            pred_dat$N[1:nrow(pred_dat1)],
                            hr.model$ddf),
                 combine_cv(pred_dat$var[(nrow(pred_dat1)+1):nrow(pred_dat)],
                            pred_dat$N[(nrow(pred_dat1)+1):nrow(pred_dat)],
                            hr.model_w$ddf))


#pred_dat$one <- sqrt(pred_dat$var) + 0*pred_dat$N
#pred_dat$mone <- 0*pred_dat$N - sqrt(pred_dat$var)

dev.new()
p <- ggplot(pred_dat, aes(x=x, y=y)) +
      geom_tile(aes(fill=cv, width=cell_side, height=cell_side)) +
      scale_fill_viridis() +
#      scale_fill_viridis(limits=c(-20,20)) +
      facet_wrap(~type) +
      geom_path(data=segs) +
      #geom_point(data=obs)
      geom_text(data=dist.data, aes(label=weather))
      #geom_emoji(data=dist.data[dist.data$weather==0,], emoji="2600") +
      #geom_emoji(data=dist.data[dist.data$weather==1,], emoji="26a1")
print(p)


dev.new()
pred_dat$se <- pred_dat$cv*pred_dat$N
p <- ggplot(pred_dat, aes(x=x, y=y)) +
      geom_tile(aes(fill=se, width=cell_side, height=cell_side)) +
      scale_fill_viridis() +
#      scale_fill_viridis(limits=c(-20,20)) +
      facet_wrap(~type) +
      geom_path(data=segs) +
      #geom_point(data=obs)
#      geom_text(data=dist.data, aes(label=weather))
      geom_emoji(data=dist.data[dist.data$weather==0,], emoji="2600") +
      geom_emoji(data=dist.data[dist.data$weather==1,], emoji="26a1")
print(p)

#pred_dat$ucl <- pred_dat$N + 2*pred_dat$se
#p <- ggplot(pred_dat, aes(x=x, y=y)) +
#      geom_tile(aes(fill=ucl, width=cell_side, height=cell_side)) +
#      scale_fill_viridis() +
##      scale_fill_viridis(limits=c(-20,20)) +
#      facet_wrap(~type) +
#      geom_path(data=segs) +
#      #geom_point(data=obs)
#      geom_text(data=dist.data, aes(label=weather))
#      #geom_emoji(data=dist.data[dist.data$weather==0,], emoji="2600") +
#      #geom_emoji(data=dist.data[dist.data$weather==1,], emoji="26a1")
#print(p)
#
#dev.new()
#pred_dat$lcl <- pred_dat$N - 2*pred_dat$se
#p <- ggplot(pred_dat, aes(x=x, y=y)) +
#      geom_tile(aes(fill=lcl, width=cell_side, height=cell_side)) +
#      scale_fill_viridis() +
##      scale_fill_viridis(limits=c(-20,20)) +
#      facet_wrap(~type) +
#      geom_path(data=segs) +
#      #geom_point(data=obs)
#      geom_text(data=dist.data, aes(label=weather))
#      #geom_emoji(data=dist.data[dist.data$weather==0,], emoji="2600") +
#      #geom_emoji(data=dist.data[dist.data$weather==1,], emoji="26a1")
#print(p)


#dev.new()
#p <- ggplot(pred_dat, aes(x=x, y=y)) +
#      geom_tile(aes(fill=mone, width=0.1, height=0.1)) +
#      #scale_fill_viridis(limits=c(0,40)) +
#      scale_fill_viridis(limits=c(-20,20)) +
#      facet_wrap(~type) +
#      geom_path(data=segs) + geom_point(data=obs)
#print(p)


