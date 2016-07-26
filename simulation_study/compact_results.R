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

huge_res <- c()

# now loop over the files
for(i in seq_along(f_names)){

  load(f_names[i])

  # re-arrange column names
  big_res$N <- big_res$V1
  big_res$CV <- big_res$V2
  big_res$V1 <- big_res$V2 <- NULL
  big_res$se <- big_res$CV*big_res$N
  big_res$sen <- big_res$se/sqrt(big_res$n)

  # print the name of this scenario as the header
  f_names[i] <- sub(".RData", "", f_names[i])
  big_res$scenario <- f_names[i]

  ## WARNING: magic numbers below!!
  # bias boxplot
  big_res$bias <- big_res$N-500

  # separate out the scenario definition
  big_res$density <- sub("([a-z]+)-[a-z]+-[a-z]+", "\\1", big_res$scenario[1])
  big_res$design <- sub("[a-z]+-([a-z]+)-[a-z]+", "\\1", big_res$scenario[1])
  big_res$df <- sub("[a-z]+-[a-z]+-([a-z]+)", "\\1", big_res$scenario[1])

  huge_res <- rbind(huge_res, big_res)
}



## WARNING: magic numbers below!!
p <- ggplot(huge_res)+
  geom_boxplot(aes(names, bias), outlier.size = 0.75) +
  #facet_wrap(~names, scales="free_x",nrow=2)+
  #ggtitle(paste0(f_names[i], "Bias")) +
  labs(y="Bias", x="Model") +
  coord_cartesian(ylim=c(-400,400)) +
  theme_minimal() +
  #facet_wrap(~density+design+df, ncol=3,
  facet_wrap(density~design+df, ncol=6,
             labeller=function(labels, multi_line=FALSE){
                        label_value(labels, multi_line)
                      }) +
  geom_hline(aes(yintercept=0), colour="red") +
  theme(axis.text.x = element_text(angle = 90, size=6))
print(p)


##dev.new()
#  # Mark's fancy quantile diagnostic, described above
#  p <- ggplot(big_res)+
#    geom_histogram(aes(quantile))+
#    facet_wrap(~names, nrow=2)+
#    ggtitle(paste0(f_names[i], "Quantile diagnostic")) +
#    geom_vline(xintercept=0.5) +
#    theme_minimal()
#  print(p)



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



