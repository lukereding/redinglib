% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_funcs.R
\name{move_ticks_inside_y}
\alias{move_ticks_inside_y}
\title{move_ticks_inside_y}
\usage{
move_ticks_inside_y()
}
\description{
Move tick marks inside the y-axis.
}
\examples{
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p + move_ticks_inside_y()
}
