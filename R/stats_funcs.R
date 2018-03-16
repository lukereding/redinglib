library(tidyverse)


#' Convert Cohen's d to r.
#'
#' Uses formula found here (https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf) for converting Cohen's d to r.
#' @param d Cohen's d.
#' @param n1 Sample size of the first group.
#' @param n2 Sample size of the second group.
#' @export
#' @examples
#' d_to_r(0.5, 12, 15)

d_to_r <- function(d = NULL, n1 = NULL, n2 = NULL) {

  # based on https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf

  # check arguments
  args <- as.list(match.call())
  missing_args <- setdiff(c("d", "n1", "n2"), names(args))
  if (length(missing_args) > 1) {
    stop(paste0("missing arguments "), paste0(missing_args, collapse = ", "))
  }

  # mapply(check_args, args[2:length(args)], rep("numeric", length(args) - 1))

  a <- ((n1 + n2) ^ 2) / (n1 * n2)

  d / sqrt((d ^ 2) + a)
}

#' Convert log odds ratio to Cohen's D.
#'
#' Uses formula found here (https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf) to convert a log-odds ration to Cohen's d.
#' @param log_odds Logg-odds ratio.
#' @export

logodds_to_d <- function(log_odds = NULL) {

  # based on https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf

  # check arguments
  args <- as.list(match.call())
  missing_args <- setdiff(c("log_odds"), names(args))

  if (length(missing_args) > 1) {
    stop(paste0("missing arguments "), paste0(missing_args, collapse = ", "))
  }

  # mapply(check_args, args[2:length(args)], rep("numeric", length(args) - 1))

  log_odds * sqrt(3) / pi
}

#' Convert correlation coeffeient r to Cohen's d.
#'
#' Uses formula found here (https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf) to convert the correlation coefficient r to Cohen's d.
#' @param r Correlation coefficient.
#' @export
#' @examples
#' r_to_d(0.4)

r_to_d <- function(r = NULL) {

  # based on https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf

  # check arguments
  args <- as.list(match.call())
  missing_args <- setdiff(c("r"), names(args))

  if (length(missing_args) > 1) {
    stop(paste0("missing arguments "), paste0(missing_args, collapse = ", "))
  }

  if (r > 1L | r < -1L) {
    stop("the correlation coefficent must be in the range [-1, 1]")
  }

  # mapply(check_args, args[2:length(args)], rep("numeric", length(args) - 1))

  (2 * r) / sqrt(1 - (r ^ 2))
}
