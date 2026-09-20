
get_url <- function(id, first_date, last_date) {

  # 20250307: new url (with https)
  my_url <- sprintf(paste0('https://api.bcb.gov.br/dados/serie/bcdata.sgs.',
                           '%s','/dados?formato=json&',
                           'dataInicial=%s&',
                           'dataFinal=%s'),
                    id,
                    format(first_date, '%d/%m/%Y'),
                    format(last_date, '%d/%m/%Y'))

  return(my_url)
}

query_api <- function(id, series_name, first_date, last_date, format_data,
                      be_quiet,
                      use_memoise, cache_path,
                      max_retries = 3) {

  # 20250307: new url (with https)
  my_url <- get_url(id,
                    first_date,
                    last_date)

  if (!be_quiet) {
    cli::cli_alert_info("Fetching {series_name} [id={id}] | {first_date} -> {last_date}")
  }

  cache_db = cache_path
  fct_JSON <- gbcbd_get_JSON_fct(use_memoise,
                                 cache_db)

  df <- NULL
  attempt <- 1
  while (attempt <= max_retries) {
    df <- tryCatch(suppressWarnings(fct_JSON(my_url)),
                   error = function(e) NULL)

    if (!is.null(df)) break

    if (attempt < max_retries) {
      wait_time <- 2 ^ attempt
      if (!be_quiet) {
        cli::cli_alert_warning("Fetch failed for id={id}. Retrying in {wait_time}s (attempt {attempt}/{max_retries})...")
      }
      Sys.sleep(wait_time)
    }
    attempt <- attempt + 1
  }

  if (is.null(df)) {
    # keep the same column structure as a successful call for the requested format
    if (format_data == 'wide') {
      df <- dplyr::tibble(ref_date = as.Date(NA))
      df[[series_name]] <- as.numeric(NA)
    } else {
      df <- dplyr::tibble(ref_date = as.Date(NA),
                          value = as.numeric(NA),
                          id_num = id,
                          series_name = series_name)
    }

    # flag the failure so callers can report it
    attr(df, 'fetch_failed') <- TRUE

    if (!be_quiet) {
      cli::cli_alert_danger("Failed to fetch data for id={id} after {max_retries} attempts.")
      cli::cli_alert_info("Check if the ID is correct at <http://www.bcb.gov.br/?sgs> or if the date range is valid (max 10 years for daily data).")
    }

    return(df)
  }

  df$data <- as.Date(df$data, '%d/%m/%Y')
  df$valor <- as.numeric(df$valor)
  df$id_num <- id
  df$series_name <- series_name

  df <- dplyr::rename(df,
                      'ref_date' = 'data',
                      'value' = 'valor')

  # change format
  if (format_data == 'wide') {

    df$id_num <- NULL
    df$series_name <- NULL

    names(df) <- c('ref_date', series_name)
  }

  return(df)
}
