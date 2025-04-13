valid_url <- function(url_in, t = 1.5) {
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = t), silent = T)[1])
  suppressWarnings(try(close.connection(con), silent = T))
  is.null(check)
}
