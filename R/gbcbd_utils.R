#' Tests for an internet conection
#'
#' @noRd
gbcbd_test_internet <- function() {
  # check for internet
  test.internet <- curl::has_internet()

  if (!test.internet) {
    stop('No internet connection found...')
  }
}

#' Returns default cache folder
#'
#' Internal use. USe the same default cache folder for all other functions.
#'
#' @noRd
gbcbd_get_default_cache_folder <- function() {
  name.cache.dir <- file.path(tempdir(), 'gbcbd_cache')

  return(name.cache.dir)
}

#' Function for switching functions depending on using cache or not
#'
#' Internal use. Simply switches a function given a choice for using memoise.
#'
#' @noRd
gbcbd_get_JSON_fct <- function(use.memoise = TRUE,
                               cache.path = gbcbd_get_default_cache_folder()) {

  if (use.memoise) {
    fct_JSON <- memoise::memoise(f = jsonlite::fromJSON,
                                 cache = memoise::cache_filesystem(cache.path))
  } else {
    fct_JSON <- jsonlite::fromJSON
  }

  return(fct_JSON)

}

#' Custom function for printing messages
#'
#' @noRd
gbcbd_message <- function(str.in, be.quiet) {

  if (be.quiet) {
    message('', appendLF = FALSE)
  } else {
    message(str.in, appendLF = FALSE)
  }

}
