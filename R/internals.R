
check_id <- function(x, id) {
  if (id < 1) stop("id is negative")
  if (id > length(x)) stop("id exceeds the number of colors -", length(x))
}
