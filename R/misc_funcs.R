#' check_args
#'
#' Checks whether `x`, a vector, is of a specific type. Useful for figuring out whether something is a factor / numeric for plotting.
#' @param x a vector
#' @param type the type you want to ensure the vector is
#' @keywords cats
#' @export
#' @examples
#' check_args(c(1,3,4),"numeric")

check_args <- function(x, type) {
  if (!is.null(x) && class(x) == type) {
    invisible()
  } else {
    stop(paste0("incorrect type or null argument: ", x))
  }
}
