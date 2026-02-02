
get_url <- function(id, first.date, last.date) {

  # 20250307: new url (with https)
  my.url <- sprintf(paste0('https://api.bcb.gov.br/dados/serie/bcdata.sgs.',
                           '%s','/dados?formato=json&',
                           'dataInicial=%s&',
                           'dataFinal=%s'),
                    id,
                    format(first.date, '%d/%m/%Y'),
                    format(last.date, '%d/%m/%Y'))

  return(my.url)
}

query_api <- function(id, series.name, first.date, last.date, format.data,
                      be.quiet,
                      use.memoise, cache.path,
                      max_retries = 3) {

  # 20250307: new url (with https)
  my.url <- get_url(id,
                    first.date,
                    last.date)

  if (!be.quiet) {
    cli::cli_alert_info("Fetching {series.name} [id={id}] | {first.date} -> {last.date}")
  }

  cache.db = cache.path
  fct_JSON <- gbcbd_get_JSON_fct(use.memoise,
                                 cache.db)

  df <- NULL
  attempt <- 1
  while (attempt <= max_retries) {
    try({
      utils::capture.output({
        df <- fct_JSON(my.url)
      })
    }, silent = TRUE)

    if (!is.null(df)) break

    if (attempt < max_retries) {
      wait_time <- 2 ^ attempt
      if (!be.quiet) {
        cli::cli_alert_warning("Fetch failed for id={id}. Retrying in {wait_time}s (attempt {attempt}/{max_retries})...")
      }
      Sys.sleep(wait_time)
    }
    attempt <- attempt + 1
  }

  if (is.null(df)) {
    df <- dplyr::tibble(ref.date = as.Date(NA),
                        value = as.numeric(NA),
                        id.num = id,
                        series.name = series.name)

    if (!be.quiet) {
      cli::cli_alert_danger("Failed to fetch data for id={id} after {max_retries} attempts.")
      cli::cli_alert_info("Check if the ID is correct at <http://www.bcb.gov.br/?sgs> or if the date range is valid (max 10 years for daily data).")
    }

    return(df)
  }

  df$data <- as.Date(df$data, '%d/%m/%Y')
  df$valor <- as.numeric(df$valor)
  df$id.num <- id
  df$series.name <- series.name

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
