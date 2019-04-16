#' Imports time series data from BCB-SGS System (Banco Central do Brasil, sistema de series temporais)
#'
#' Using BCB's oficial API at <https://www.bcb.gov.br/>, this function will download data for a specific set of ids and dates.
#' The main advantage is the use of caching and parallel computing for fast operations. You can search for available series at <http://www.bcb.gov.br/?sgs>
#'
#' @param id Id of time series. The name of the vector sets the name of the series in the output (e.g i.d <- c('SELIC' = 11)).
#' You can search for ids in the official BCB-SGS webpage <http://www.bcb.gov.br/?sgs>
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
#' my.id <- c('Taxa de juros - Selic' = 11)
#' df <- gbcbd_get_series(my.id)
gbcbd_get_series <- function(id,
                             first.date = Sys.Date() - 10*365,
                             last.date = Sys.Date(),
                             format.data = 'long',
                             be.quiet = FALSE,
                             use.memoise = TRUE,
                             cache.path = gbcbd_get_default_cache_folder(),
                             do.parallel = FALSE) {

  # check if arguments make sense
  first.date <- as.Date(first.date)
  if (class(first.date) != 'Date') {
    stop('Argument first.date is not a valid date!')
  }

  last.date <- as.Date(last.date)
  if (class(last.date) != 'Date') {
    stop('Argument last.date is not a valid date!')
  }

  if (last.date < first.date) {
    stop('It seems that last.date < first.date. Check your inputs.')
  }

  if (is.null(names(id))) {
    names(id) <- c(paste0('id = ', id))
  }

  possible.values <- c('long', 'wide')
  if (!(format.data %in% possible.values)) {
    stop(paste0('Input format.data should be "long" or "wide".'))
  }

  # check if ids exist
  # REMOVED: Api at https://dadosabertos.bcb.gov.br/ does not contain all data from SGS
  #
  # df.series <- gbcbd_get_available_series(use.memoise = use.memoise,
  #                                         cache.path = cache.path)
  # flag <- !(id %in% df.series$id.num)
  #
  # if (any(flag)) {
  #   failed.ids <- id[flag]
  #
  #   stop(paste0("Can't find the following ids within the BCB-SGS system:\n\n",
  #                paste0(paste0(failed.ids, ' (', names(failed.ids), ')'), collapse = ', ')),
  #        '\n\n')
  # }

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

    # find number of used cores
    formals.parallel <- formals(future::plan())
    used.workers <- formals.parallel$workers

    available.cores <- future::availableCores()

    gbcbd_message(paste0('\nRunning parallel GetBCBData with ', used.workers, ' cores (',
               available.cores, ' available)',
               '\n\n'),
               be.quiet = be.quiet)

    # test if plan() was called
    msg <- utils::capture.output(future::plan())

    flag <- stringr::str_detect(msg[1], 'sequential')

    if (flag) {
      stop(paste0('When using do.parallel = TRUE, you need to call future::plan() to configure your parallel settings. \n',
                  'A suggestion, write the following lines:\n\n',
                  'future::plan(future::multisession, workers = floor(future::availableCores()/2))',
                  '\n\n',
                  'The last line should be placed just before calling GetBCBData.\n',
                  'Notice it will use half of your available cores so that your OS has some room to breathe.'))
    }

    my.l <- furrr::future_pmap(.l = my.args,
                               gbcbd_get_single_series,
                               .progress = TRUE)
  }

  # check and change desired format output
  if (format.data == 'long') {

    df.out <- dplyr::bind_rows(my.l)

  } else {

    df.out <- purrr::reduce(my.l,
                            dplyr::full_join, by = "ref.date")

    # order by date
    idx <- order(df.out$ref.date)
    df.out <- df.out[idx, ]

  }

  gbcbd_message('\n', be.quiet)

  return(df.out)
}


#' Gets a single series from BCB-SGS (internal use)
#'
#' This function should not be called directly. Its a helper for gbcbd_get_series
#'
#' @inheritParams gbcbd_get_series
#' @param id Id of series from BCB-SGS
#' @param series.name Name of time series
#'
#' @return A dataframe for a single series
#' @export
#'
#' @examples
#' df <- gbcbd_get_single_series(id = 1)
gbcbd_get_single_series <- function(id,
                                    series.name = paste0('SGS ', id),
                                    first.date = Sys.Date()-360,
                                    last.date = Sys.Date(),
                                    format.data = 'long',
                                    be.quiet = FALSE,
                                    use.memoise = TRUE,
                                    cache.path = gbcbd_get_default_cache_folder()) {

  my.url <- sprintf('https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json',
                    id)

  my.url <- sprintf(paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.',
                           '%s','/dados?formato=json&',
                           'dataInicial=%s&',
                           'dataFinal=%s'),
                    id,
                    format(first.date, '%d/%m/%Y'),
                    format(last.date, '%d/%m/%Y'))


  gbcbd_message(paste0('\nFetching ',
                       series.name, ' [', id, '] ',
                       'from BCB-SGS'),
                be.quiet)

  if (use.memoise) {
    gbcbd_message(' with cache ', be.quiet)
  }
  else {
    gbcbd_message(' from Online API ', be.quiet)
  }

  cache.db = memoise::cache_filesystem(cache.path)
  fct_JSON <- gbcbd_get_JSON_fct(use.memoise,
                                 cache.db)

  df <- NULL
  try({
    utils::capture.output(
    df <- fct_JSON(my.url)
    )
  })

  if (is.null(df)) {
    df <- dplyr::tibble(ref.date = NA,
                         value = NA,
                         id.num = id,
                         series.name = series.name)

    warning(paste0('\n\t Error in fetching data\n'))
    warning(paste0('\n\tId ', id, ' does not seem available at BCB-SGS. Check it again at : ',
               '<http://www.bcb.gov.br/?sgs>'))
    return(df)

  } else {
    gbcbd_message(paste0('\n\t Found ', nrow(df), ' observations'),
                  be.quiet)
  }


  df$data <- as.Date(df$data, '%d/%m/%Y')
  df$valor <- as.numeric(df$valor, '%d/%m/%Y')
  df$id.num <- id
  df$series.name <- series.name

  # fix for "no visible global fct definition" msg from CRAN CHECK
  #data <- valor <- NULL
  df <- dplyr::rename(df,
                      'ref.date' = 'data',
                      'value' = 'valor')

  # change format
  if (format.data == 'wide') {

    df$id.num <- NULL
    df$series.name <- NULL

    names(df) <- c('ref.date', series.name)
  }

  return(df)
}


