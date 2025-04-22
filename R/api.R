
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
                      use.memoise, cache.path) {

  # 20250307: new url (with https)
  my.url <- get_url(id,
                    first.date,
                    last.date)

  gbcbd_message(paste0('\nFetching ',
                       series.name, ' [id=', id, '] ',
                       '| ', first.date, ' -> ', last.date,
                       ''),
                be.quiet)

  #cache.db = memoise::cache_filesystem(cache.path)
  cache.db = cache.path
  fct_JSON <- gbcbd_get_JSON_fct(use.memoise,
                                 cache.db)

  df <- NULL
  try({
    utils::capture.output({
      df <- fct_JSON(my.url)
    })
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
    #gbcbd_message(paste0('\n\t Found ', nrow(df), ' observations'),
     #             be.quiet)
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

