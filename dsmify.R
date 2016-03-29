# get the data into a format that dsm can use
dsmify <- function(survey){

  # distance data
  dist.data <- get.distance.data(survey)

  # assemble the sent data
  segs <- survey@transects@sampler.info
  segs$x <- apply(segs, 1, function(x) mean(as.numeric(x[c(2,4)])))
  segs$y <- apply(segs, 1, function(x) mean(as.numeric(x[c(3,5)])))
  segs$Sample.Label <- segs$ID
  segs$Effort <- segs$length
  # assemble observation table
  obs <- survey@obs.table@obs.table
  obs <- merge(obs, dist.data, by="object")
  obs$size <- 1

  return(list(obs=obs, segs=segs, dist=dist.data))
}
