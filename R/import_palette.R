#' Import color palettes from paletton
#'
#' Imports color palettes generated on \url{paletton.com}; go to this website,
#' choose your palette, click on 'Tables/export, then on 'color list..  as
#' text'. Select all the page, copy its content, and then call
#' \code{import_paletton} from R.
#'
#' @export
#' @seealso \url{paletton.com}
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}

import_paletton <- function(){

  ## We grab the input from the keyboard, process the text, and then split the
  ## output by colors. Shades also need to be re-ordered, the default one being
  ## put first, but logically it should be 3rd.

  input <- suppressWarnings(readLines("clipboard"))
  txt <- input[-(1:5)]
  txt <- gsub(" ", "", txt)
  txt <- txt[txt != ""]
  txt <- txt[-length(txt)]


  start_at <- grep("color", txt)
  n_col <- length(start_at)
  stop_at <- c(start_at[-1] - 1, length(txt))

  out <- vector(n_col, mode = "list")
  for(i in seq_len(n_col)) {
    out[[i]] <- txt[(start_at[i] + 1) : stop_at[i]]
  }


  out <- lapply(out, extract_color)
  new_order <- c(2, 3, 1, 4, 5)
  out <- lapply(out, function(e) e[new_order])
  names(out) <- paste("color", seq_len(n_col), sep = "_")
  class(out) <- c("paletton", "list")
  out
}





## Turn:
## "shade0=#7B9F35=rgb(123,159,53)=rgba(123,159,53,1)=rgb0(0.482,0.624,0.208)"
## into:
## #7B9F35

extract_color <- function(x) {
  vapply(x, function(e) unlist(strsplit(e, "="))[2],
         character(1), USE.NAMES = FALSE)
}
