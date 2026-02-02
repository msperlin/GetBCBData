#' Imports time series data from BCB-SGS System (Banco Central do Brasil, sistema de series temporais)
#'
#' Using BCB's oficial API at <https://www.bcb.gov.br/>, this function will download data for a specific set of ids and dates.
#' The main advantage is the use of caching and parallel computing for fast operations. You can search for available series at <https://www.bcb.gov.br/?sgs>
#'
#' @param id Id of time series. The name of the vector sets the name of the series in the output (e.g i.d <- c('SELIC' = 11)).
#' You can search for ids in the official BCB-SGS webpage <https://www.bcb.gov.br/?sgs>
#' @param first.date First date of time series
#' @param last.date Last date of time series
#' @param format.data The format of the datasets - long (default, series incremented by rows) or wide (series incremented by columns)
#' @param be.quiet Logical. Should functions output messages to screen? - FALSE (default) or TRUE
#' @param use.memoise Logical. Sets the use of caching system - TRUE (default) or FALSE
#' @param cache.path Path to save cache files - 'rbcb2_cache' (default)
#' @param do.parallel Logical for parallel data importation - FALSE (default)
#'
#' @return A dataframe with requested datasets
#' @export
#'
#' @examples
#'
#' \dontrun{
#' my.id <- c('Selic Rate' = 11)
#' df <- gbcbd_get_series(my.id, cache.path = tempdir())
#' }
gbcbd_get_series <- function(id,
                             first.date = Sys.Date() - 5*365,
                             last.date = Sys.Date(),
                             format.data = 'long',
                             be.quiet = FALSE,
                             use.memoise = TRUE,
                             cache.path = gbcbd_get_default_cache_folder(),
                             do.parallel = FALSE) {

  # verify arguments
  args <- gbcbd_verify_args(id, first.date, last.date, format.data)
  id <- args$id
  first.date <- args$first.date
  last.date <- args$last.date

  #set args
  my.args <- list(id = id,
                  series.name = names(id),
                  first.date = first.date,
                  last.date = last.date,
                  format.data = format.data,
                  be.quiet = be.quiet,
                  use.memoise = use.memoise,
                  cache.path = cache.path)

  if (!do.parallel) {

    my.l <- purrr::pmap(.l = my.args ,
                        gbcbd_get_single_series)
  } else {

    gbcbd_setup_parallel(be.quiet)

    my.l <- furrr::future_pmap(.l = my.args,
                               gbcbd_get_single_series,
                               .progress = TRUE)
  }

  # check and change desired format output
  df.out <- gbcbd_format_output(my.l, format.data)

  if (!be.quiet) {
    cli::cli_alert_success("Finished fetching data. Total rows: {nrow(df.out)}")
  }

  return(df.out)
}


#' Gets a single series from BCB-SGS (internal use)
#'
#' This function should not be called directly. Its a helper for gbcbd_get_series
#'
#'@noRd
gbcbd_get_single_series <- function(id,
                                    series.name = paste0('SGS ', id),
                                    first.date = Sys.Date()-360,
                                    last.date = Sys.Date(),
                                    format.data = 'long',
                                    be.quiet = FALSE,
                                    use.memoise = TRUE,
                                    cache.path = gbcbd_get_default_cache_folder()) {

  # 20250422 - new system for diff_years > 10
  diff_years <- as.numeric(last.date - first.date)/365
  my_interval <- '3 years'

  if (diff_years > 8) {


    vec_dates <- c(seq(first.date, last.date,
                       by =  my_interval),
                   last.date)

    cli::cli_alert_info("using sequential data fetching for {length(vec_dates)-1} time periods")

    df_all <- dplyr::tibble()
    for (i_dates in seq(1:(length(vec_dates)-1))) {

      first_date_now <- vec_dates[i_dates]
      last_date_now <- vec_dates[i_dates+1]

      df_now <- query_api(id, series.name, first_date_now, last_date_now,
                      format.data,  be.quiet, use.memoise, cache.path)

      df_all <- dplyr::bind_rows(
        df_all,
        df_now
      )
    }

    # make sure it is unique (no overlaps)
    df_all <- unique(df_all)


  } else {
    cli::cli_alert_info("using single call for small query")

    df_all <- query_api(id, series.name, first.date, last.date,
                    format.data,  be.quiet, use.memoise, cache.path)
  }

  n_rows <- nrow(df_all)
  n_cols <- ncol(df_all)
  cli::cli_alert_success("got data with {n_rows} rows and {n_cols} columns")

  return(df_all)
}

#' Verifies arguments for gbcbd_get_series
#' @noRd
gbcbd_verify_args <- function(id, first.date, last.date, format.data) {
  # check if arguments make sense
  first.date <- as.Date(first.date)
  if (!inherits(first.date, 'Date')) {
    stop('Argument first.date is not a valid date!')
  }

  last.date <- as.Date(last.date)
  if (!inherits(last.date, 'Date')) {
    stop('Argument last.date is not a valid date!')
  }

  if (last.date < first.date) {
    stop('It seems that last.date < first.date. Check your inputs.')
  }

  if (is.null(names(id))) {
    names(id) <- paste0('id = ', id)
  }

  possible.values <- c('long', 'wide')
  if (!(format.data %in% possible.values)) {
    stop(paste0('Input format.data should be "long" or "wide".'))
  }

  return(list(id = id, first.date = first.date, last.date = last.date))
}

#' Sets up parallel processing
#' @noRd
gbcbd_setup_parallel <- function(be.quiet) {
  # find number of used cores
  used.workers <- future::nbrOfWorkers()
  available.cores <- parallelly::availableCores()

  if (!be.quiet) {
    cli::cli_alert_info('Running parallel GetBCBData with {used.workers} cores ({available.cores} available)')
  }

  # test if plan() was called
  is_sequential <- inherits(future::plan(), "sequential")

  if (is_sequential) {
    stop(paste0('When using do.parallel = TRUE, you need to call future::plan() to configure your parallel settings. \n',
                'A suggestion, write the following lines:\n\n',
                'future::plan(future::multisession, workers = floor(parallelly::availableCores()/2))',
                '\n\n',
                'The last line should be placed just before calling GetBCBData.\n',
                'Notice it will use half of your available cores so that your OS has some room to breathe.'))
  }
}

#' Formats output of gbcbd_get_series
#' @noRd
gbcbd_format_output <- function(my.l, format.data) {
  if (format.data == 'long') {
    df.out <- dplyr::bind_rows(my.l)
  } else {
    df.out <- purrr::reduce(my.l,
                            dplyr::full_join, by = "ref.date")

    # order by date
    idx <- order(df.out$ref.date)
    df.out <- df.out[idx, ]
  }
  return(df.out)
}
