% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color_scales.R
\name{scale_color_world}
\alias{scale_color_world}
\title{World color scale}
\usage{
scale_color_world(...)
}
\arguments{
\item{...}{Additional arguments passed to ggplot2::discrete_scale.}
}
\description{
'World' color scale for ggplot graphs.
}
\examples{
p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) + geom_point()
p + scale_color_world()
}
