# detectability vs gradients

# basic idea is can a spatial model tell the difference between
# a left-right gradient and the case where detectability changes
# between the left and right sides of the survey area (in MVB terms
# "in fog in the dark")

library(DSsim)
library(devtools)
load_all("~/current/dsm")
library(Distance)
library(handy2)
source("~/Dropbox/varprop/data/dsm.varprop.R")


source("../check.sim.setup.R")
source("../plot_df.R")
source("../test_dssim.R")
source("../dsmify.R")

make_data <- function(seed){
  n_grid <- 100
  density.surface <- expand.grid(x = seq(0, 1, len=n_grid),
                                 y = seq(0, 1, len=n_grid))
  # left-right
  density.surface$density <- 1#00*density.surface$x
#  density.surface$density[density.surface$x <= 0.5] <- 0.3
#  density.surface$density[density.surface$x > 0.5] <- 0.7
  
  # setup the detection function for "good" conditions
  df_good <- list(key        = "hr",
                  scale      = 0.03,
                  shape      = 3,
                  truncation = 0.05)
  df_bad <- list(key        = "hr",
                 scale      = 0.005,
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
  #set.seed(seed)
  #check.sim.setup(ss_bad)
  
  # okay now generate some data
  
  set.seed(seed)
  survey_good <- create.survey.results(ss_good, dht.tables=TRUE)
  set.seed(seed)
  survey_bad <- create.survey.results(ss_bad, dht.tables=TRUE)
  
  # mash together two surveys
  mash <- function(survey1, survey2){
  
  
    # get the data into shape
    dat1 <- dsmify(survey1)
    dat2 <- dsmify(survey2)
  
    segs <- dat1$segs
  
    labs <- 1:nrow(dat1$segs)
  #  pp <- plogis(labs, scale=10, location=5)
    pp <- plogis(labs, scale=3, location=30)
  
    picker <- as.logical(rbinom(length(labs), 1, pp))
    ones <- (1:nrow(segs))[picker]
    twos <- (1:nrow(segs))[!picker]
  
    # get the data for each bit
    obs <- rbind(dat1$obs[dat1$obs$Sample.Label %in% ones,],
                 dat2$obs[dat2$obs$Sample.Label %in% twos,])
  
    dist <- rbind(dat1$dist[dat1$dist$Sample.Label %in% ones,],
                  dat2$dist[dat2$dist$Sample.Label %in% twos,])
    # add in the covariate
    dist$weather <- c(rep(0,sum(dat1$dist$Sample.Label %in% ones)),
                      rep(1,sum(dat2$dist$Sample.Label %in% twos)))
    # remove duplicate observations, perfering data from "survey1"
    dist <- dist[!duplicated(dist$object),]
  
    # add in the weather covariate to the segments
    segs$weather <- 0
    segs$weather[segs$Sample.Label %in% twos] <- 1
  
  
    return(list(dist=dist, obs=obs, segs=segs))
  }
  
  dsm_data <- mash(survey_good, survey_bad)
## predictions

cell_side <- 0.05
pred_dat1 <- expand.grid(x = seq(0, 1, by=cell_side),
                         y = seq(0, 1, by=cell_side))
pred_dat1$off.set <- cell_side^2

  dsm_data$pred_dat1 <- pred_dat1

  return(dsm_data)
}

