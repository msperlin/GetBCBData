## Version 0.9.4  (2026-09-20)

- added new function gbcbd_search_series() to search the BCB-SGS catalog by text
- added 'xml2' dependency to parse the BCB-SGS search results
- the search supports caching (use_memoise/cache_path) and pagination (max_results)

## Version 0.9.3  (2026-09-20)

- renamed function arguments and internal variables to snake_case
- old dot-case argument names (first.date, format.data, etc.) still work as deprecated aliases, with a warning
- renamed output columns to snake_case: ref_date, id_num and series_name (breaking change)
- updated documentation, README and vignette

## Version 0.9.2  (2026-09-20)

- fixed wide format output when a requested series fails (columns are now consistent with successful calls)
- fixed duplicated/zero-length period when the queried date span is a multiple of the sequential interval
- made the sequential-fetching threshold consistent with the documented 10-year API limit
- cleaned up query_api retry loop and suppressed HTTP warnings before retrying
- be.quiet is now respected in the per-series helper; fetch failures are reported instead of a misleading success message
- strengthened argument validation (id, dates and logical arguments)
- removed unused 'utils' dependency
- test suite cleanup (removed duplicated tests, added regression tests)

## Version 0.9.1  (2026-02-02)

- now using parallelly::availableCores() (fixes #12)
- now using  future::nbrOfWorkers()
- other code improvements
 
## Version 0.9.0  (2025-04-22)

- added new sequential method for querying with diff_year < 10 years (this fixes #11)


## Version 0.8.0  (2025-03-07)

- Fixed bcb url to https
- fixed  example of main function gbcbd_get_series()
- added time difference control for any request with diff year > 10 (the api blocks such requests..)

## Version 0.7.0  (2022-06-07)

- Fixed cran notes by removing some of unnecessary packages.

## Version 0.6  (2021-03-15)

- Fixed issue with memoise package

## Version 0.5  (2019-04-15)

- First release
