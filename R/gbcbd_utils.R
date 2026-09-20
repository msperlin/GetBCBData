#' Tests for an internet conection
#'
#' @noRd
gbcbd_test_internet <- function() {
  # check for internet
  test_internet <- curl::has_internet()

  if (!test_internet) {
    stop('No internet connection found...')
  }
}

#' Returns default cache folder
#'
#' Internal use. USe the same default cache folder for all other functions.
#'
#' @noRd
gbcbd_get_default_cache_folder <- function() {
  name_cache_dir <- file.path(tempdir(), 'gbcbd_cache')

  return(name_cache_dir)
}

# hidden environment for memoization
.gbcbd_env <- new.env(parent = emptyenv())

#' Function for switching functions depending on using cache or not
#'
#' Internal use. Simply switches a function given a choice for using memoise.
#'
#' @noRd
gbcbd_get_JSON_fct <- function(use_memoise = TRUE,
                               cache_path = gbcbd_get_default_cache_folder()) {

  fct_to_use <- function(...) {
    Sys.sleep(1.5)
    return(jsonlite::fromJSON(...))
  }

  if (!use_memoise) {
    return(fct_to_use)
  }

  # check if memoized function exists in env
  if (exists("fct_JSON_memoized", envir = .gbcbd_env)) {
    # check if cache path is the same
    if (identical(get("cache_path", envir = .gbcbd_env), cache_path)) {
      return(get("fct_JSON_memoized", envir = .gbcbd_env))
    }
  }

  # if not, create it
  fct_JSON <- memoise::memoise(f = fct_to_use,
                               cache = memoise::cache_filesystem(cache_path))

  # save it to env
  assign("fct_JSON_memoized", fct_JSON, envir = .gbcbd_env)
  assign("cache_path", cache_path, envir = .gbcbd_env)

  return(fct_JSON)

}
