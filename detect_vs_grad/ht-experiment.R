# experiments with Horvitz-Thompson estimation vs GAMs/GLMs/etc

library(devtools)
load_all("~/current/dsm")
library(Distance)

# true N is 200

#load("ht-experiment.RData")
source("make_data.R")

big_res <- c()

for(ii in 1:100){

md <- make_data(NULL)

segs <- md$segs
obs <- md$obs
dist.data <- md$dist
pred_dat1 <- md$pred_dat1

# fit detection function with no covariates
hr.model <- ds(dist.data, key="hr", adjustment=NULL)
# with weather as a covariate
hr.model_w <- ds(dist.data, key="hr", formula=~as.factor(weather))


#summary(hr.model)


# A_R
total_area <- sum(pred_dat1$off.set)
# A_c = 2*w*L
covered_area <- 2*max(dist.data$distance)*sum(segs$Effort)

# what's the H-T estimate?
HT <- (total_area/covered_area) * sum(1/predict(hr.model$ddf)$fitted)
HT_w <- (total_area/covered_area) * sum(1/predict(hr.model_w$ddf)$fitted)

# what about stratifying by weather?
weather0_effort <- sum(segs$Effort[segs$weather==0])
HT_w_strat0 <- total_area/(2*max(dist.data$distance)*weather0_effort)*
                                 sum(1/predict(hr.model_w$ddf)$fitted[dist.data$weather==0])
weather1_effort <- sum(segs$Effort[segs$weather==1])
HT_w_strat1 <- total_area/(2*max(dist.data$distance)*weather1_effort)*
                                 sum(1/predict(hr.model_w$ddf)$fitted[dist.data$weather==1])
HT_w_strat <- mean(c(HT_w_strat0, HT_w_strat1))



# fit intercept only model w/ covar
mod_1w <- dsm(count~1, hr.model_w, segs, obs, method="REML",  family=tw(a=1.2))
# fit intercept only model w/o covar
mod_1 <- dsm(count~1, hr.model, segs, obs, method="REML",  family=tw(a=1.2))

# model estimate

m <- sum(predict(mod_1, pred_dat1, off.set=cell_side^2))

m_w <- sum(predict(mod_1w, pred_dat1, off.set=cell_side^2))



# this is the covariate model
# this is invariant to distribution choice?
mod_1w_st <- dsm(count~as.factor(weather)-1, hr.model_w, segs, obs, method="REML",  family=tw(a=1.2))
pred_dat1$weather <- 1
m_w1 <- sum(predict(mod_1w_st, pred_dat1, off.set=cell_side^2))
pred_dat1$weather <- 0
m_w0 <- sum(predict(mod_1w_st, pred_dat1, off.set=cell_side^2))

# is this taking effort into account twice?
m_st <- sum(c((weather0_effort/sum(weather0_effort+weather1_effort))*m_w0,
              (weather1_effort/sum(weather0_effort+weather1_effort))*m_w1))


# what is the equivalent to the stratified model?

m_m <- mean(total_area*exp(coef(mod_1w_st)))


res <- c(HT, HT_w, HT_w_strat, m, m_w, m_st, m_m)

big_res <- rbind(big_res, res)
#cat(ii,"\n")
}

names(big_res) <- c("HT", "HT_w", "HT_w_strat", "m", "m_w", "m_st", "m_m")

#sum((total_area/(2*max(dist.data$distance)*c(weather0_effort,weather1_effort)))*exp(coef(mod_1w_st)))
#
#mod_1w_st2 <- dsm(count~as.factor(weather), hr.model_w, segs, obs, method="REML",  family=tw(a=1.2))
#pred_dat1$weather <- 1
#m_w21 <- sum(predict(mod_1w_st2, pred_dat1, off.set=cell_side^2))
#pred_dat1$weather <- 0
#m_w20 <- sum(predict(mod_1w_st2, pred_dat1, off.set=cell_side^2))
#m_st <- sum(c((weather0_effort/sum(weather0_effort+weather1_effort))*m_w20,
#              (weather1_effort/sum(weather0_effort+weather1_effort))*m_w21))

