# get Horvitz-Thompson estimate of N and CV(N)
quick_dht <- function(df, survey_res){
  aa <- dht(df$ddf, survey_res@region.table@region.table,
            survey_res@sample.table@sample.table, survey_res@obs.table@obs.table)
  return(as.vector(aa$individuals$N[,c("Estimate","cv")][1,]))
}

# with stratification about x=1.5
quick_dht_strat <- function(df, survey_res){

  # setup the region
  region <- survey_res@region.table@region.table
  region <- rbind(region,region)
  region$Region.Label <- c(1,2)
  region$Area <- region$Area/2

  samplet <- survey_res@sample.table@sample.table
  samplet$Region.Label <- 2
  samplet$Region.Label[samplet$Sample.Label %in% 1:54] <- 1

  obst <- survey_res@obs.table@obs.table
  obst$Region.Label <- NULL
  obst <- merge(obst, samplet[,c("Region.Label","Sample.Label")],
                by="Sample.Label")
  aa <- dht(df$ddf, region,
            samplet, obst)
  return(apply(aa$individuals$N[,c("Estimate","cv")][3,],2, sum))
}

