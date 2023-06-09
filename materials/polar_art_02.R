library(ggplot2)
library(tibble)

#This function will asign random colors to the variables that change anytime the seed value is changed

polar_art <- function(seed, n) {
  
  # set the state of the random number generator
  set.seed(seed)
  
  # data frame containing random values for 
  # aesthetics we might want to use in the art
  dat <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2),
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = runif(n), 
    size = runif(n)
  )
  
  # Generate a new set of random colors for each execution
  colors <- sample(colors(), n)
  
  # plot segments in various colors, using 
  # polar coordinates and the assigned random color
  ggplot(dat, aes(
    x = x0,
    y = y0,
    xend = x1,
    yend = y1,
    colour = as.factor(colors),  # Convert colors to a factor for discrete scale
    size = size
  )) +
    geom_segment(show.legend = FALSE) +
    coord_polar() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_colour_manual(values = colors) +  # Use the assigned random colors
    scale_size(range = c(0, 10)) + 
    theme_void()

}

polar_art(seed=5,n=100)
polar_art(seed=100,n=100)

