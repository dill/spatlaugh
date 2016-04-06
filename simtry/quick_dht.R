# get Horvitz-Thompson estimate of N and CV(N)
quick_dht <- function(df, survey_res){
  aa <- dht(df$ddf, survey_res@region.table@region.table,
            survey_res@sample.table@sample.table, survey_res@obs.table@obs.table)
  return(aa$individuals$N[,c("Estimate","cv")])
}

