#' Searches BCB-SGS series by text
#'
#' Searches the BCB-SGS catalog (sistema gerenciador de series temporais) for
#' series whose name or metadata match a given text. The search is performed
#' against the official BCB-SGS search service at
#' <https://www3.bcb.gov.br/sgspub/>. The returned \code{id} column can be
#' passed directly to \code{\link{gbcbd_get_series}}.
#'
#' @param search_text A single string with the text to search for (e.g. 'selic').
#' @param max_results Maximum number of series to return. Use \code{Inf} for no limit.
#' @param be_quiet Logical. Should functions output messages to screen? - FALSE (default) or TRUE
#' @param use_memoise Logical. Sets the use of caching system - TRUE (default) or FALSE
#' @param cache_path Path to save cache files - 'gbcbd_cache' (default)
#'
#' @return A tibble with the columns \code{id}, \code{series_name},
#'   \code{unit}, \code{frequency}, \code{first_date}, \code{last_date} and
#'   \code{source}. Returns zero rows when nothing matches.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df_search <- gbcbd_search_series('selic')
#' print(df_search)
#'
#' # fetch the first matching series
#' my_id <- df_search$id[1]
#' names(my_id) <- df_search$series_name[1]
#' df_series <- gbcbd_get_series(my_id)
#' }
gbcbd_search_series <- function(search_text,
                                max_results = 100,
                                be_quiet = FALSE,
                                use_memoise = TRUE,
                                cache_path = gbcbd_get_default_cache_folder()) {

  # verify arguments
  if (!is.character(search_text) || length(search_text) != 1 || is.na(search_text)) {
    stop('Argument search_text should be a single string.')
  }

  search_text <- trimws(search_text)
  if (!nzchar(search_text)) {
    stop('Argument search_text should be a non-empty string.')
  }

  if (!is.numeric(max_results) || length(max_results) != 1 ||
      is.na(max_results) || max_results <= 0) {
    stop('Argument max_results should be a positive number (use Inf for no limit).')
  }

  check_logical <- function(x, arg_name) {
    if (!is.logical(x) || length(x) != 1 || is.na(x)) {
      stop(paste0('Argument ', arg_name, ' should be either TRUE or FALSE.'))
    }
  }

  check_logical(be_quiet, 'be_quiet')
  check_logical(use_memoise, 'use_memoise')

  # check for internet connection
  gbcbd_test_internet()

  if (!be_quiet) {
    cli::cli_alert_info("Searching BCB-SGS series for '{search_text}'")
  }

  fct_search <- gbcbd_search_get_fct(use_memoise, cache_path)
  df_out <- fct_search(search_text, max_results)

  if (!be_quiet) {
    cli::cli_alert_success("Found {nrow(df_out)} series matching '{search_text}'")
  }

  return(df_out)
}

#' Returns the search function, possibly memoised
#'
#' Internal use. Switches between the plain and the memoised version of the
#' search depending on the user choice and the cache path.
#'
#' @noRd
gbcbd_search_get_fct <- function(use_memoise = TRUE,
                                 cache_path = gbcbd_get_default_cache_folder()) {

  if (!use_memoise) {
    return(gbcbd_search_fetch_all)
  }

  # check if memoized function exists in env for the same cache path
  if (exists('fct_search_memoized', envir = .gbcbd_env)) {
    if (identical(get('search_cache_path', envir = .gbcbd_env), cache_path)) {
      return(get('fct_search_memoized', envir = .gbcbd_env))
    }
  }

  fct_search <- memoise::memoise(f = gbcbd_search_fetch_all,
                                 cache = memoise::cache_filesystem(cache_path))

  assign('fct_search_memoized', fct_search, envir = .gbcbd_env)
  assign('search_cache_path', cache_path, envir = .gbcbd_env)

  return(fct_search)
}

#' Fetches and parses all pages of a search (internal use)
#'
#' @noRd
gbcbd_search_fetch_all <- function(search_text, max_results) {

  handle <- gbcbd_search_new_session()

  page_data <- gbcbd_search_fetch_page(handle,
                                       search_text,
                                       page = 1,
                                       method = 'localizarSeriesPorTexto')

  df_out <- page_data$df

  total <- page_data$total
  if (is.na(total)) total <- nrow(df_out)

  page_size <- nrow(df_out)

  if (page_size > 0 && total > page_size) {

    n_pages <- ceiling(total / page_size)
    for (i_page in seq(2, n_pages)) {

      if (is.finite(max_results) && nrow(df_out) >= max_results) break

      page_now <- gbcbd_search_fetch_page(handle,
                                          search_text,
                                          page = i_page,
                                          method = 'getPagina')

      df_out <- dplyr::bind_rows(df_out, page_now$df)
    }
  }

  # remove any duplicated id (page overlaps)
  df_out <- df_out[!duplicated(df_out$id), ]

  if (is.finite(max_results) && nrow(df_out) > max_results) {
    df_out <- df_out[seq_len(max_results), ]
  }

  return(df_out)
}

#' Starts a session with the BCB-SGS search service
#'
#' The legacy search service requires a valid session cookie, so we make an
#' initial request that sets it. Cookies are kept in memory by the curl handle.
#'
#' @noRd
gbcbd_search_new_session <- function() {

  handle <- curl::new_handle()
  curl::handle_setopt(handle, cookiefile = '', cookiejar = '')

  curl::curl_fetch_memory('https://www3.bcb.gov.br/sgspub/', handle = handle)

  return(handle)
}

#' Fetches a single result page from the BCB-SGS search service
#'
#' @noRd
gbcbd_search_fetch_page <- function(handle, search_text, page = 1,
                                    method = 'localizarSeriesPorTexto') {

  my_url <- paste0('https://www3.bcb.gov.br/sgspub/localizarseries/',
                   'localizarSeries.do?method=', method)

  body <- paste0('texto=', curl::curl_escape(search_text),
                 '&hdTipoPesquisa=6',
                 '&hdTipoOrdenacao=0',
                 '&hdNumPagina=', page,
                 '&bilQtde=1',
                 '&periodicidade=0',
                 '&fonte=341')

  curl::handle_setopt(handle, postfields = charToRaw(body))

  res <- curl::curl_fetch_memory(my_url, handle = handle)

  if (res$status_code != 200) {
    stop(paste0('BCB-SGS search failed with HTTP status ', res$status_code))
  }

  html <- gbcbd_search_decode(res$content)

  return(gbcbd_search_parse(html))
}

#' Decodes the raw HTML returned by the BCB-SGS search service
#'
#' The service replies in ISO-8859-1 and may include spurious NUL bytes.
#'
#' @noRd
gbcbd_search_decode <- function(raw_content) {

  raw_content <- raw_content[raw_content != as.raw(0)]
  html <- iconv(rawToChar(raw_content), from = 'ISO-8859-1', to = 'UTF-8')

  return(html)
}

#' Parses the HTML of a BCB-SGS search result page
#'
#' @noRd
gbcbd_search_parse <- function(html) {

  doc <- xml2::read_html(html)

  total_txt <- xml2::xml_text(
    xml2::xml_find_first(doc, '//span[contains(., "Amount of series found")]/b')
  )
  total <- suppressWarnings(as.integer(total_txt))

  rows <- xml2::xml_find_all(doc, '//table[@id="tabelaSeries"]//tr[td]')

  if (length(rows) == 0) {
    return(list(df = gbcbd_search_empty_df(), total = total))
  }

  get_cell <- function(row, idx) {
    xml2::xml_find_first(row, paste0('./td[', idx, ']'))
  }
  cell_text <- function(row, idx) {
    trimws(xml2::xml_text(get_cell(row, idx)))
  }
  cell_attr <- function(row, idx, attr_name) {
    node <- xml2::xml_find_first(get_cell(row, idx), './/span')
    if (inherits(node, 'xml_missing')) return(NA_character_)
    return(xml2::xml_attr(node, attr_name))
  }

  ids <- vapply(rows, cell_text, character(1), idx = 2)
  series_names <- vapply(rows, cell_text, character(1), idx = 3)
  units <- vapply(rows, cell_text, character(1), idx = 4)
  frequencies <- vapply(rows, cell_attr, character(1), idx = 5,
                        attr_name = 'title')
  first_dates <- vapply(rows, cell_text, character(1), idx = 6)
  last_dates <- vapply(rows, cell_text, character(1), idx = 7)
  sources <- vapply(rows, cell_text, character(1), idx = 8)

  df_out <- dplyr::tibble(
    id = as.integer(ids),
    series_name = series_names,
    unit = units,
    frequency = frequencies,
    first_date = as.Date(first_dates, '%d/%m/%Y'),
    last_date = as.Date(last_dates, '%d/%m/%Y'),
    source = sources
  )

  return(list(df = df_out, total = total))
}

#' Returns an empty search result with the right structure
#'
#' @noRd
gbcbd_search_empty_df <- function() {
  dplyr::tibble(
    id = integer(),
    series_name = character(),
    unit = character(),
    frequency = character(),
    first_date = as.Date(character()),
    last_date = as.Date(character()),
    source = character()
  )
}
