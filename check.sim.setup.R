# based on code from Laura Marshall
check.sim.setup <- function(simsetup) {
  # small function to plot the simulation setup input: simsetup - object
  # created by make.simulation() output: 4-panel plot of study region with
  # popn, study region with transects study region with detected objects,
  # histogram of detection distances
  #pop <- generate.population(simsetup) #generate a population
  #transects <- generate.transects(simsetup) #generate the transects
  eg.survey <- create.survey.results(simsetup) #simulate the survey process of detection
  dist.data <- get.distance.data(eg.survey) #look at distance data

  par(mfrow = c(2, 2))

  # theoretical section
  # this is uber slow:
  #plot(simsetup@population.description@density,
  #     plot.units = "m", style="blocks",
  #     density.col=heat.colors(50))
  den_obj <- simsetup@population.description@density@density.surface[[1]]
  xdim <- attr(den_obj, "out.attrs")$dim[1]
  ydim <- attr(den_obj, "out.attrs")$dim[2]
  image(z = matrix(den_obj$density, xdim, ydim),
        x = seq(min(den_obj$x), max(den_obj$x), len=xdim),
        y = seq(min(den_obj$y), max(den_obj$y), len=ydim),
        xlab="X-coords (m)", ylab="Y-coords (m)",
        col=heat.colors(50))
  plot_df(simsetup@detectability)

  # what actually happened
  plot(eg.survey)
  hist(dist.data$distance, xlab = "Distance (m)", main = "Distance Data")
  par(mfrow = c(1, 1))
}
