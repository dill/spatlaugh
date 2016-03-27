# write out a transect as a shapefile

# this is a completely Byzantine process

library(sp)
library(rgdal)

# transect should have the following columns:
#  x,y   x,y coordinates of **endpoints**
#  leg   survey leg associated
# file   filename (well actually foldername) to write to
write_transects <- function(transect, file){

  legs <- list()

  # make each leg a different Line object
  for(this_leg in unique(transect$leg)){

    # get the transect bits for this leg
    tr <- transect[transect$leg==this_leg,]

    llist <- list()

    for(i in 1:(nrow(tr)-1)){
      ll <- Line(tr[,c("x","y")][i:(i+1),])
      llist <- c(llist, list(ll))
    }

    legs <- c(legs, llist)
  }

  ll <- Lines(unlist(legs, recursive=FALSE), ID=1)

  ll <- SpatialLines(list(ll))
  ll <- SpatialLinesDataFrame(ll, data=data.frame(a=1))

  writeOGR(ll, file, "data", "ESRI Shapefile" )
}
