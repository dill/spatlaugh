library(DSsim)

# based on code from Laura Marshall
check.sim.setup <- function(simsetup) {
  # small function to plot the simulation setup input: simsetup - object
  # created by make.simulation() output: 4-panel plot of study region with
  # popn, study region with transects study region with detected objects,
  # histogram of detection distances
  pop <- generate.population(simsetup) #generate a population
  transects <- generate.transects(simsetup) #generate the transects
  eg.survey <- create.survey.results(simsetup) #simulate the survey process of detection
  dist.data <- get.distance.data(eg.survey) #look at distance data

  par(mfrow = c(2, 2))

  # theoretical section
  plot(simsetup@population.description@density,
       plot.units = "m", style="blocks",
       density.col=heat.colors(50))
  plot_df(simsetup@detectability)

  # what actually happened
  plot(eg.survey)
  hist(dist.data$distance, xlab = "Distance (m)", main = "Distance Data")
  par(mfrow = c(1, 1))
}

# plot a detection function from an object of class "Detectability"
plot_df <- function(df){
  if(df@key.function=="hn"){
    g <- function(x, sigma, b){
      exp(-x^2/(2*sigma^2))
    }
  }else{
    g <- function(x, sigma, b){
      1-exp(-(x/sigma)^(-b))
    }
  }
  xx <- seq(0, df@truncation, len=500)
  plot(xx, g(xx, df@scale.param, df@shape.param), type="l",
       xlab="Distance", ylab="Probability of detection",
       main="Detection function")
}

test_dssim <- function(design_path, dsurf, n_grid){
  # get the "region"
  region.shapefile <- read.shapefile("shapes/region/data")
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


# build a density surface
n_grid <- 100
density.surface <- expand.grid(x = seq(0, 1, len=n_grid),
                               y = seq(0, 1, len=n_grid))
# flat
density.surface$density <- 1
# top to bottom
#density.surface$density <- density.surface$y
# left-right
#density.surface$density <- density.surface$x
# diagonal
#density.surface$density <- density.surface$y + density.surface$x


# make it a density?
density.surface$density <- density.surface$density/sum(density.surface$density)



# zig
#ss <- test_dssim("shapes/zig", density.surface, n_grid)

# lots on the left
ss <- test_dssim("shapes/leftie", density.surface, n_grid)

check.sim.setup(ss)



