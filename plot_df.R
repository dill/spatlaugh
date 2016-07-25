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
       ylim=c(0, 1),
       main="Detection function")
}
