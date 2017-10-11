#' Print method for paletton objects
#'
#' This is a print method for paletton objects obtained through \code{\link{import_paletton}}.
#'
#' @export
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
print.paletton <- function(x) {
  cat("\n/// paletton object //")
  cat("\n", length(x), "colors ")
  if(length(x) > 0) cat(",", length(x[[1]]), "shades")
  cat("\n")
  print(unclass(x))
}
