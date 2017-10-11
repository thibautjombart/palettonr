#' Plot method for paletton objects
#'
#' This is a plot method for paletton objects obtained through
#' \code{\link{import_paletton}}.
#'
#' @export
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param x a \code{paletton} object
#' @param y the margins of the plot
#'
plot.paletton <- function(x, y = c(6, 1, 1, 1), ...) {
  n_items <- length(unlist(x))

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  par(mar = y)
  barplot(rep(1, n_items), col = unlist(x), yaxt = "n",
          names.arg = unlist(x), las = 3, ...)

}
