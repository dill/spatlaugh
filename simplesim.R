### simple simulation...
library(magrittr)
library(sp)
library(rgdal)
source("write_transects.R")

# make a zigzag
n_segs <- 10
zz <- data.frame(x   = c(seq(0, 0.5, len=n_segs),
                         seq(0.5, 1, len=n_segs)[-1]),
                 y   = c(seq(0, 1, len=n_segs),
                         seq(1, 0, len=n_segs)[-1]),
                 leg = c(rep("1", n_segs-1),
                         rep("2", n_segs)))

#plot(zz)
write_transects(zz, "shapes/zig")


# lots of effort in the west
l <- data.frame(x = c(rep(0.1, 5),
                      rep(0.25, 5),
                      rep(0.4, 5)),
                y = rep(seq(0.2, 0.8, len=5),3),
                leg = c(rep("1", 5),
                        rep("2", 5),
                        rep("3", 5)))

#plot(l)
write_transects(l, "shapes/leftie")


### simple region shapefile
region <- data.frame(x=c(0,0,1,1,0),
                     y=c(0,1,1,0,0))

region <- region %>%
            Polygon %>% list %>%
            Polygons(ID="1") %>% list %>%
            SpatialPolygons %>%
            SpatialPolygonsDataFrame(data=data.frame(z=1))
writeOGR(region, "shapes/region", "data", "ESRI Shapefile" )




