#' Derive palettes from paletton objects
#'
#' \code{get_palettes} extract color palettes from paletton objects obtained
#' through \code{\link{import_paletton}}. The function \code{get_palette}
#' extracts a single palette from a specific color. The function
#' \code{get_colors} creates a random collection of diverse colors, given an
#' initial random seed.
#'
#' @export
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @rdname get_palettes
#'
#' @param x a \code{paletton} object
#'
get_palettes <- function(x) {
  out <- lapply(x, grDevices::colorRampPalette)
  names(out) <- gsub("color", "palette", names(out))
  out
}




#' @export
#' @rdname get_palettes
#' @param id an integer indicating the color to use

get_palette <- function(x, id = 1) {
  check_id(x, id)
  out <- grDevices::colorRampPalette(x[[id]])
  out
}





#' @export
#' @rdname get_palettes
#'
#' @param n an integer indicating the number of colors to return
#'
#' @param seed the initial random seed to use; if NULL, then colors are not
#'   randomised.

get_colors <- function(x, n = length(x), seed = NULL) {
  n_col <- length(x)
  palettes <- get_palettes(x)

  if ((n %% n_col) == 0) {
    n_col_per_pal <- n / n_col
  } else {
    n_col_per_pal <- 1 + n / n_col
  }

  out <- unlist(lapply(palettes, function(e) e(n_col_per_pal)))
  if (!is.null(seed)) {
    set.seed(seed)
    out <- sample(out)
  }

  unname(out)
}
