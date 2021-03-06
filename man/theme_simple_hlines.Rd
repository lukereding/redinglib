% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_funcs.R
\name{theme_simple_hlines}
\alias{theme_simple_hlines}
\title{A simple theme for showing continuous x categorical data.}
\usage{
theme_simple_hlines(font_size = 14, font_family = "", line_size = 0.4)
}
\arguments{
\item{font_size}{Base size of the font. Default is 14.}

\item{font_family}{Font family.}

\item{line_size}{Size of axis ticks. Defauts to 0.4.}
}
\description{
My preferred plotting theme for categorical x numeric data. Major gridlines, larger axis labels, and no ticks on the x-axis.
}
\examples{
p <- ggplot(mtcars, aes(factor(cyl), wt)) + geom_boxplot()
p + theme_simple_hlines()
}
