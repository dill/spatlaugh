#check.sim.setup(ss)

big_res <- c()

pb <- txtProgressBar(min=1, max=nsim, style=3)

for(ii in 1:nsim){

  # generate a survey
  survey_res <- create.survey.results(ss, dht.tables=TRUE)
  # put the data in dsm format
  dsm_data <- dsmify(survey_res)
  dist.data <- dsm_data$dist
  obs <- dsm_data$obs
  segs <- dsm_data$segs
  # rotated segs
  segs[,c("xr","yr")] <- t(R %*% t(segs[,c("x","y")]))

  # fit a detection function
  hr.model <- suppressMessages(try(ds(dist.data, key="hr", adjustment=NULL)))

  if(any(class(hr.model) == "try-error") || abs(hr.model$ddf$par[1])<1e-6) next

  # model list object
  ll <- list()

  # function that times out so our fits don't go on for ever
  timer <- function(x){
    x <- substitute(x)
    setTimeLimit(elapsed=300) # 5 mins?
    on.exit(setTimeLimit(elapsed=Inf))
    model <- try(eval(x))
    if(any(class(model)=="try-error")){
      return(NULL)
    }else{
      return(model)
    }
  }

  # thin plate
  ll[["m_xy_tp"]] <- timer(dsm(count~s(x, y, bs="tp"), hr.model, segs, obs,
                         method="REML", family=tw(a=1.2)))
  # thin plate te
  ll[["m_xy_te"]] <- timer(dsm(count~te(x, y, bs="tp"), hr.model, segs, obs,
                         method="REML", family=tw(a=1.2)))
  # thin plate te
  ll[["m_xyr_te"]] <- timer(dsm(count~te(xr, yr, bs="tp"), hr.model, segs, obs,
                          method="REML", family=tw(a=1.2)))
  # thin plate rotation
  ll[["m_xyr_tp"]] <- timer(dsm(count~s(xr, yr, bs="tp"), hr.model, segs, obs,
                          method="REML", family=tw(a=1.2)))
  # thin plate w/ shrinkage
  ll[["m_xy_ts"]] <- timer(dsm(count~s(x, y, bs="ts"), hr.model, segs, obs,
                         method="REML", family=tw(a=1.2)))
  # Duchon
  ll[["m_xy_ds"]] <- timer(dsm(count~s(x, y, bs="ds", m=c(1, 0.5)), hr.model, segs, obs,
                         method="REML", family=tw(a=1.2)))


  # process -- get N and CVs for the spatial models
  all_res <- ldply(ll, function(x){
    if(x$converged){
      xx <- dsm_varprop(x, pred_dat1)
      N <- sum(xx$pred)
      cv <- sqrt(xx$var[1,1])/N
      return(c(N, cv))
    }else{
      return(c(NA, NA))
    }
  })

  # get N and CVs for the HT model
  HT <- quick_dht(hr.model, survey_res)
  HT_strat <- quick_dht_strat(hr.model, survey_res,
                              stratification[[this_set$design]])

  # bind them together
  all_res <- rbind.data.frame(all_res,
                              c("HT", unname(HT)),
                              c("HT_strat", unname(HT_strat)))

  all_res$V1 <- as.numeric(all_res$V1)
  all_res$V2 <- as.numeric(all_res$V2)

  # get the quantiles
  qs <- apply(all_res[,-1], 1,
              function(x) get_N_quantile(N=true_N, Nhat=x[1], cv=x[2]))

  # build some storage for this set of results
  res <- data.frame(names    = all_res[,1],
                    iter     = ii,
                    quantile = qs)

  res <- cbind(res, all_res[,2:3])

  ## save smoothing pars
  # bind on the smoothing parameters
  sps <- laply(ll, function(x) {
                 xx <- c(sp1=NA, sp2=NA)
                 xx[1:length(x$sp)] <- x$sp
                 xx})
  # no smoothing parameter for HT
  sps <- rbind(sps, c(NA,NA), c(NA,NA))

  ## save # samples
  res$n <- nrow(dist.data)

  res <- cbind(res, sps)

  # bind to the rest
  big_res <- rbind(big_res, res)

  setTxtProgressBar(pb, ii)

}


#library(ggplot2)
#ggplot(big_res) + geom_histogram(aes(quantile)) + facet_wrap(~names)


