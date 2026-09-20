#' Imports time series data from BCB-SGS System (Banco Central do Brasil, sistema de series temporais)
#'
#' Using BCB's oficial API at <https://www.bcb.gov.br/>, this function will download data for a specific set of ids and dates.
#' The main advantage is the use of caching and parallel computing for fast operations. You can search for available series at <https://www.bcb.gov.br/?sgs>
#'
#' @param id Id of time series. The name of the vector sets the name of the series in the output (e.g i_d <- c('SELIC' = 11)).
#' You can search for ids in the official BCB-SGS webpage <https://www.bcb.gov.br/?sgs>
#' @param first_date First date of time series
#' @param last_date Last date of time series
#' @param format_data The format of the datasets - long (default, series incremented by rows) or wide (series incremented by columns)
#' @param be_quiet Logical. Should functions output messages to screen? - FALSE (default) or TRUE
#' @param use_memoise Logical. Sets the use of caching system - TRUE (default) or FALSE
#' @param cache_path Path to save cache files - 'gbcbd_cache' (default)
#' @param do_parallel Logical for parallel data importation - FALSE (default)
#' @param first.date Deprecated. Use \code{first_date} instead.
#' @param last.date Deprecated. Use \code{last_date} instead.
#' @param format.data Deprecated. Use \code{format_data} instead.
#' @param be.quiet Deprecated. Use \code{be_quiet} instead.
#' @param use.memoise Deprecated. Use \code{use_memoise} instead.
#' @param cache.path Deprecated. Use \code{cache_path} instead.
#' @param do.parallel Deprecated. Use \code{do_parallel} instead.
#'
#' @return A dataframe with requested datasets
#' @export
#'
#' @examples
#'
#' \dontrun{
#' my_id <- c('Selic Rate' = 11)
#' df <- gbcbd_get_series(my_id, cache_path = tempdir())
#' }
gbcbd_get_series <- function(id,
                             first_date = Sys.Date() - 5*365,
                             last_date = Sys.Date(),
                             format_data = 'long',
                             be_quiet = FALSE,
                             use_memoise = TRUE,
                             cache_path = gbcbd_get_default_cache_folder(),
                             do_parallel = FALSE,
                             first.date = NULL,
                             last.date = NULL,
                             format.data = NULL,
                             be.quiet = NULL,
                             use.memoise = NULL,
                             cache.path = NULL,
                             do.parallel = NULL) {

  # accept deprecated dot-case arguments, with a warning
  first_date <- gbcbd_resolve_arg(first.date, 'first.date', 'first_date',
                                  !missing(first_date), first_date)
  last_date <- gbcbd_resolve_arg(last.date, 'last.date', 'last_date',
                                 !missing(last_date), last_date)
  format_data <- gbcbd_resolve_arg(format.data, 'format.data', 'format_data',
                                   !missing(format_data), format_data)
  be_quiet <- gbcbd_resolve_arg(be.quiet, 'be.quiet', 'be_quiet',
                                !missing(be_quiet), be_quiet)
  use_memoise <- gbcbd_resolve_arg(use.memoise, 'use.memoise', 'use_memoise',
                                   !missing(use_memoise), use_memoise)
  cache_path <- gbcbd_resolve_arg(cache.path, 'cache.path', 'cache_path',
                                  !missing(cache_path), cache_path)
  do_parallel <- gbcbd_resolve_arg(do.parallel, 'do.parallel', 'do_parallel',
                                   !missing(do_parallel), do_parallel)

  # verify arguments
  args <- gbcbd_verify_args(id, first_date, last_date, format_data,
                            be_quiet, use_memoise, do_parallel)
  id <- args$id
  first_date <- args$first_date
  last_date <- args$last_date

  # check for internet connection
  gbcbd_test_internet()

  #set args
  my_args <- list(id = id,
                  series_name = names(id),
                  first_date = first_date,
                  last_date = last_date,
                  format_data = format_data,
                  be_quiet = be_quiet,
                  use_memoise = use_memoise,
                  cache_path = cache_path)

  if (!do_parallel) {

    my_l <- purrr::pmap(.l = my_args ,
                        gbcbd_get_single_series)
  } else {

    gbcbd_setup_parallel(be_quiet)

    my_l <- furrr::future_pmap(.l = my_args,
                               gbcbd_get_single_series,
                               .progress = TRUE)
  }

  # check and change desired format output
  df_out <- gbcbd_format_output(my_l, format_data)

  n_failed <- sum(vapply(my_l,
                         function(x) isTRUE(attr(x, 'fetch_failed')),
                         logical(1)))

  if (!be_quiet) {
    if (n_failed > 0) {
      cli::cli_alert_danger("Failed to fetch {n_failed}/{length(my_l)} series. Check the ids and date ranges.")
    } else {
      cli::cli_alert_success("Finished fetching data. Total rows: {nrow(df_out)}")
    }
  }

  return(df_out)
}


#' Resolves a (possibly deprecated) argument
#'
#' Allows the old dot-case argument names to keep working while the new
#' snake_case names are preferred. Errors if both are supplied at once.
#'
#' @noRd
gbcbd_resolve_arg <- function(old_value, old_name, new_name,
                              new_supplied, new_value) {
  if (is.null(old_value)) return(new_value)

  if (new_supplied) {
    stop(paste0("Use either '", new_name, "' or the deprecated '", old_name,
                "', not both."), call. = FALSE)
  }

  cli::cli_warn("Argument '{old_name}' is deprecated, use '{new_name}' instead.")

  return(old_value)
}

#' Gets a single series from BCB-SGS (internal use)
#'
#' This function should not be called directly. Its a helper for gbcbd_get_series
#'
#'@noRd
gbcbd_get_single_series <- function(id,
                                    series_name = paste0('SGS ', id),
                                    first_date = Sys.Date()-360,
                                    last_date = Sys.Date(),
                                    format_data = 'long',
                                    be_quiet = FALSE,
                                    use_memoise = TRUE,
                                    cache_path = gbcbd_get_default_cache_folder()) {

  # the BCB API blocks single requests spanning more than ~10 years
  max_years <- 10
  diff_years <- as.numeric(last_date - first_date)/365

  if (diff_years >= max_years) {

    my_interval <- '3 years'

    # unique() avoids a zero-length period when last_date is already
    # produced by seq() (i.e. when the span is a multiple of the interval)
    vec_dates <- sort(unique(c(seq(first_date, last_date,
                                   by = my_interval),
                               last_date)))

    if (!be_quiet) {
      cli::cli_alert_info("using sequential data fetching for {length(vec_dates)-1} time periods")
    }

    df_all <- dplyr::tibble()
    fetch_failed <- FALSE
    for (i_dates in seq(1:(length(vec_dates)-1))) {

      first_date_now <- vec_dates[i_dates]
      last_date_now <- vec_dates[i_dates+1]

      df_now <- query_api(id, series_name, first_date_now, last_date_now,
                      format_data,  be_quiet, use_memoise, cache_path)

      if (isTRUE(attr(df_now, 'fetch_failed'))) fetch_failed <- TRUE

      df_all <- dplyr::bind_rows(
        df_all,
        df_now
      )
    }

    # make sure it is unique (no overlaps)
    df_all <- unique(df_all)

    # unique() drops custom attributes, so restore the failure flag
    if (fetch_failed) attr(df_all, 'fetch_failed') <- TRUE


  } else {
    if (!be_quiet) {
      cli::cli_alert_info("using single call for small query")
    }

    df_all <- query_api(id, series_name, first_date, last_date,
                    format_data,  be_quiet, use_memoise, cache_path)
  }

  if (!be_quiet) {
    n_rows <- nrow(df_all)
    n_cols <- ncol(df_all)
    cli::cli_alert_success("got data with {n_rows} rows and {n_cols} columns")
  }

  return(df_all)
}

#' Verifies arguments for gbcbd_get_series
#' @noRd
gbcbd_verify_args <- function(id, first_date, last_date, format_data,
                              be_quiet, use_memoise, do_parallel) {
  # check if arguments make sense
  if (!(is.numeric(id) || is.character(id)) || length(id) == 0) {
    stop('Argument id should be a non-empty vector of ids (e.g. c("Selic" = 11)).')
  }

  first_date <- tryCatch(as.Date(first_date), error = function(e) as.Date(NA))
  if (length(first_date) != 1 || anyNA(first_date)) {
    stop('Argument first_date is not a valid date!')
  }

  last_date <- tryCatch(as.Date(last_date), error = function(e) as.Date(NA))
  if (length(last_date) != 1 || anyNA(last_date)) {
    stop('Argument last_date is not a valid date!')
  }

  if (last_date < first_date) {
    stop('It seems that last_date < first_date. Check your inputs.')
  }

  if (is.null(names(id))) {
    names(id) <- paste0('id = ', id)
  }

  possible_values <- c('long', 'wide')
  if (!(format_data %in% possible_values)) {
    stop('Input format_data should be "long" or "wide".')
  }

  check_logical <- function(x, arg_name) {
    if (!is.logical(x) || length(x) != 1 || is.na(x)) {
      stop(paste0('Argument ', arg_name, ' should be either TRUE or FALSE.'))
    }
  }

  check_logical(be_quiet, 'be_quiet')
  check_logical(use_memoise, 'use_memoise')
  check_logical(do_parallel, 'do_parallel')

  return(list(id = id, first_date = first_date, last_date = last_date))
}

#' Sets up parallel processing
#' @noRd
gbcbd_setup_parallel <- function(be_quiet) {
  # find number of used cores
  used_workers <- future::nbrOfWorkers()
  available_cores <- parallelly::availableCores()

  if (!be_quiet) {
    cli::cli_alert_info('Running parallel GetBCBData with {used_workers} cores ({available_cores} available)')
  }

  # test if plan() was called
  is_sequential <- inherits(future::plan(), "sequential")

  if (is_sequential) {
    stop(paste0('When using do_parallel = TRUE, you need to call future::plan() to configure your parallel settings. \n',
                'A suggestion, write the following lines:\n\n',
                'future::plan(future::multisession, workers = floor(parallelly::availableCores()/2))',
                '\n\n',
                'The last line should be placed just before calling GetBCBData.\n',
                'Notice it will use half of your available cores so that your OS has some room to breathe.'))
  }
}

#' Formats output of gbcbd_get_series
#' @noRd
gbcbd_format_output <- function(my_l, format_data) {
  if (format_data == 'long') {
    df_out <- dplyr::bind_rows(my_l)
  } else {
    df_out <- purrr::reduce(my_l,
                            dplyr::full_join, by = "ref_date")

    # order by date
    idx <- order(df_out$ref_date)
    df_out <- df_out[idx, ]
  }
  return(df_out)
}
