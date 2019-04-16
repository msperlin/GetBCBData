#' Tests for an internet conection
#'
#' A helper function that testa and make sure the user has a live internet conection.
#'
#' @return Logical TRUE/FALSE
#' @export
#'
#' @examples
#'
#' inet.flag <- gbcbd_test_internet()
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
#' @return A path to directory
#' @export
#'
#' @examples
#' my.cache.folder <- gbcbd_get_default_cache_folder()
gbcbd_get_default_cache_folder <- function() {
  name.cache.dir <- 'gbcbd_cache'

  return(name.cache.dir)
}

#' Function for switching functions depending on using cache or not
#'
#' Internal use. Simply switches a function given a choice for using memoise.
#'
#' @inheritParams gbcbd_get_series
#' @return A JSON function that will use cache or not
#' @export
#'
#' @examples
#'
#' fct_get_JSON <- gbcbd_get_JSON_fct()
gbcbd_get_JSON_fct <- function(use.memoise = TRUE,
                               cache.path = gbcbd_get_default_cache_folder()) {

  if (use.memoise) {
    fct_JSON <- memoise::memoise(f = jsonlite::fromJSON,
                                 cache = cache.path)
  } else {
    fct_JSON <- jsonlite::fromJSON
  }

  return(fct_JSON)

}

#' Custom function for printing messages
#'
#' @param str.in Message string
#' @param be.quiet Logical. Should print it or not?
#'
#' @return Nothing
#' @export
#'
#' @examples
#' gbcbd_message('Test', FALSE)
gbcbd_message <- function(str.in, be.quiet) {

  if (be.quiet) {
    message('', appendLF = FALSE)
  } else {
    message(str.in, appendLF = FALSE)
  }

}
