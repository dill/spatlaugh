test_dssim <- function(design_path, dsurf, n_grid, region="shapes/region/data"){
  # get the "region"
  region.shapefile <- read.shapefile(region)
  region <- make.region(region.name = "Survey Region", units = "m",
                        shapefile = region.shapefile)

  # populate the region with the density
  pop.density <- make.density(region=region, density.surface=list(dsurf),
                              x.space = 1/n_grid, y.space = 1/n_grid)

  pop.description <- make.population.description(region.obj = region,
                                                 density.obj = pop.density,
                                                 N = 500, fixed.N = TRUE)

  # build the detection function
  detect <- make.detectability(key.function = "hr",
                               scale.param = 0.025, shape.param=3,
                               truncation = 0.05)

  this_design <- make.design(transect.type = "Line",
                             design.details = c("user specified"),
                             region = region, plus.sampling = FALSE,
                             path = design_path)

  ddf.analyses <- make.ddf.analysis.list(
                                    dsmodel=list(~cds(key="hn", formula=~1)), #half-normal model
  #~cds(key = "hr", formula = ~1)), #hazard rate model
  method = "ds", criteria = "AIC")

  my_simulation <- make.simulation(reps=10, single.transect.set=TRUE,
                                   region.obj=region, design.obj=this_design,
                                   population.description.obj=pop.description,
                                   detectability.obj=detect,
                                   ddf.analyses.list=ddf.analyses)



  return(my_simulation)
}

