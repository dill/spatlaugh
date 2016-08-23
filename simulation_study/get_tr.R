# take segments and form the transect IDs
get_tr <- function(ss){
  aa <- create.survey.results(ss, TRUE)
  bb <- aa@transects@sampler.info
  seg <- bb$start.Y==max(bb$start.Y) | bb$end.Y==max(bb$end.Y) |
        bb$start.Y==min(bb$start.Y) | bb$end.Y==min(bb$end.Y)
  # get number of segments per transect
  seg_mat <- matrix(which(seg), ncol=2, byrow=TRUE)
  tr_n <- apply(seg_mat, 1, diff)
  # make the labels
  tr_id <- rep(1:nrow(seg_mat), tr_n+1)

  return(tr_id)
}

