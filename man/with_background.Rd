% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_funcs.R
\name{with_background}
\alias{with_background}
\title{Change the background color of a ggplot.}
\usage{
with_background(fill = "black", color = "NA")
}
\arguments{
\item{fill}{The color you want to change it to. Default is black.}

\item{color}{The color you want to change the background to. Default is NA.}
}
\description{
Change the background color of a ggplot.
}
\examples{
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p + with_background("pink")
}
