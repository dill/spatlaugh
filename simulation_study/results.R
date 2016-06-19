library(ggplot2)
library(plyr)

# one for each distribution/detectability
f_names <- list.files(pattern="*\\.RData")

# lookup for model names to short descriptions
model_name_lookup <- list("HT" = "Horvitz-Thompson",
                          "HT_strat" = "Horvitz-Thompson (stratified)",
                          "m_xy_ds"  = "Duchon spline bivariate",
                          "m_xy_te"  = "Thin plate tensor product",
                          "m_xy_tp"  = "Thin plate bivariate",
                          "m_xy_ts"  = "Thin plate with shrinkage bivariate",
                          "m_xyr_tp"  = "Thin plate bivariate rotated",
                          "m_xyr_te"  = "Thin plate tensor product rotated"
                          )

# helper functions
reasonable_range <- function(x){
  # 1 and 5 are whiskers
  boxplot.stats(x)$stats[c(1,5)]
}
clipped_obs <- function(x, rr, name){
  x <- x[[name]]
  length(x[x<=rr[1] | x>=rr[2]])
}

# now loop over the files
for(i in seq_along(f_names)){
  
  load(f_names[i])
  
  # re-arrange column names
  big_res$N <- big_res$V1
  big_res$CV <- big_res$V2
  big_res$V1 <- big_res$V2 <- NULL
  big_res$se <- big_res$CV*big_res$N
  big_res$sen <- big_res$se/sqrt(big_res$n)
  
  # does 1/smoothing parameter make more sense (no?)
  big_res$sp1 <- 1/big_res$sp1
  big_res$sp2 <- 1/big_res$sp2
  
  # print the name of this scenario as the header
  f_names[i] <- sub(".RData", "", f_names[i])
  cat("\n\n## ", f_names[i],"\n\n")
  
  ## N
  #rr <- reasonable_range(big_res$N)
  # build a data.frame to count clipped observation
  #clipped_d <- ddply(big_res, .(names), clipped_obs, name="N", rr=rr)
  
  #p <- ggplot(big_res)+
  #  geom_histogram(aes(N))+
  #  facet_wrap(~names, scales="free_x",nrow=2)+
  #  ggtitle(paste(f_names[i], "N"))+
  #  scale_x_continuous(limits = rr) +
  #  geom_text(aes(label=V1, x=rr[1]+(rr[2]-rr[1]), y=50 ), data=clipped_d) +
  #  geom_vline(aes(xintercept=200))
  #print(p) 
  
dev.new()
  # bias boxplot
  big_res$bias <- big_res$N-200
  p <- ggplot(big_res)+
    geom_boxplot(aes(names, bias)) +
    #facet_wrap(~names, scales="free_x",nrow=2)+
    ggtitle("Bias") +
    labs(y="Bias") +
    coord_cartesian(ylim=c(-200,300)) +
    theme_minimal() +
    geom_hline(aes(yintercept=0), colour="red")
  print(p) 
  
dev.new()
  # Mark's fancy quantile diagnostic, described above
  p <- ggplot(big_res)+
    geom_histogram(aes(quantile))+
    facet_wrap(~names, nrow=2)+
    ggtitle("Quantile diagnostic") +
    theme_minimal()
  print(p) 
  
  #rr <- reasonable_range(big_res$sen)
  ## build a data.frame to count clipped observation
  #clipped_d <- ddply(big_res, .(names), clipped_obs, name="sen", rr=rr)
  #p <- ggplot(big_res)+
  #  geom_histogram(aes(sen))+
  #  facet_wrap(~names, scales="free_x",nrow=2)+
  #  scale_x_continuous(limits = rr) +
  #  geom_text(aes(label=V1, x=rr[1]+(rr[2]-rr[1]), y=50 ), data=clipped_d) +
  #  ggtitle(paste(f_names[i], "se/sqrt(n)"))
  #print(p) 
  #
  ## smoothing parameters
  #sp_dat <- big_res[!(big_res$names %in% c("HT","HT_strat")),]
  #sp_dat$names <- as.character(sp_dat$names)
  #
  ## mash the sp2s into the sp1 data
  #sp_dat2 <- big_res[big_res$names %in% c("m_xy_te","m_xyr_te"),]
  #sp_dat2$names <- as.character(sp_dat2$names)
  #sp_dat2$names[sp_dat2$names=="m_xy_te"] <- "m_xy_te sp2"
  #sp_dat2$names[sp_dat2$names=="m_xyr_te"] <- "m_xyr_te sp2"
  #sp_dat <- rbind(sp_dat,sp_dat2)
  #sp_dat$names <- as.factor(sp_dat$names)
  #
  #sp_dat$names <- droplevels(sp_dat$names)
  #rr <- reasonable_range(sp_dat$sp1)
  ## build a data.frame to count clipped observation
  #clipped_d <- ddply(sp_dat, .(names), clipped_obs, name="sp1", rr=rr)
  #p <- ggplot(sp_dat)+
  #  geom_histogram(aes(sp1))+
  #  facet_wrap(~names, scales="free_x",nrow=2)+
  #  ggtitle(paste(f_names[i], "1/sp1"))
  #print(p) 
  
}


## Number of detections per simulation

#ndat <- c()
#for(i in seq_along(f_names)){
#  load(f_names[i])
#  ndat <- rbind(ndat,
#                cbind(scenario=sub(".RData", "", f_names[i]),
#                      big_res[big_res$name=="HT",])
#                )
#}
#p <- ggplot(ndat)+
#  geom_histogram(aes(n), binwidth=2)+
#  ggtitle("detections") +
#  facet_wrap(~scenario, nrow=1) +
#  theme_minimal()
#print(p) 



