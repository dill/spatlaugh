# Create a survey
#
# design_path   - the path to the design shapefile
# dsurf         - density surface
# n_grid        - number of cells in each direction in dsurf (assumed square)
# n_pop         - true population size
# region_path   - path to region shapefile

test_dssim <- function(design_path, dsurf, n_grid, n_pop=500,
                       region_path="shapes/region/data"){
  # get the study region
  region.shapefile <- read.shapefile(region_path)
  region <- make.region(region.name = "Survey Region", units = "m",
                        shapefile = region.shapefile)

  # populate the region with the density
  pop.density <- make.density(region=region, density.surface=list(dsurf),
                              x.space = 1/n_grid, y.space = 1/n_grid)

  pop.description <- make.population.description(region.obj = region,
                                                 density.obj = pop.density,
                                                 N = n_pop, fixed.N = TRUE)

  # build the detection function
  detect <- make.detectability(key.function = "hr",
                               scale.param = 0.025, shape.param=3,
                               truncation = 0.05)

  this_design <- make.design(transect.type = "Line",
                             design.details = c("user specified"),
                             region = region, plus.sampling = FALSE,
                             path = design_path)

  ddf.analyses <- make.ddf.analysis.list(
                                    dsmodel=list(~cds(key="hn", formula=~1)),
                                                 ~cds(key="hr", formula=~1)),
  method = "ds", criteria = "AIC")

  my_simulation <- make.simulation(reps=10, single.transect.set=TRUE,
                                   region.obj=region, design.obj=this_design,
                                   population.description.obj=pop.description,
                                   detectability.obj=detect,
                                   ddf.analyses.list=ddf.analyses)



  return(my_simulation)
}

