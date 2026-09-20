skip_search_tests <- function() {
  skip_if_offline()
  skip_on_cran() # too heavy for cran
}

expected_cols <- function() {
  c('id', 'series_name', 'unit', 'frequency', 'first_date', 'last_date', 'source')
}

test_that("Search returns expected columns and known series", {

  skip_search_tests()

  df_search <- gbcbd_search_series('selic',
                                   be_quiet = TRUE,
                                   use_memoise = FALSE)

  expect_true(nrow(df_search) > 0)
  expect_equal(names(df_search), expected_cols())
  expect_true(432 %in% df_search$id)
  expect_true(is.integer(df_search$id))
  expect_true(inherits(df_search$first_date, 'Date'))
})

test_that("Search respects max_results", {

  skip_search_tests()

  df_search <- gbcbd_search_series('ipca',
                                   max_results = 5,
                                   be_quiet = TRUE,
                                   use_memoise = FALSE)

  expect_lte(nrow(df_search), 5)
})

test_that("Search paginates to return all matches", {

  skip_search_tests()

  df_search <- gbcbd_search_series('ipca',
                                   max_results = Inf,
                                   be_quiet = TRUE,
                                   use_memoise = FALSE)

  # more than one page of results
  expect_gt(nrow(df_search), 50)
  expect_equal(anyDuplicated(df_search$id), 0L)
})

test_that("Search with no match returns an empty result", {

  skip_search_tests()

  df_search <- gbcbd_search_series('xyzzy-nomatch-term',
                                   be_quiet = TRUE,
                                   use_memoise = FALSE)

  expect_equal(nrow(df_search), 0)
  expect_equal(names(df_search), expected_cols())
})

test_that("Search validates its arguments", {

  expect_error(gbcbd_search_series(''), 'non-empty')
  expect_error(gbcbd_search_series('selic', max_results = -1), 'positive')
  expect_error(gbcbd_search_series('selic', be_quiet = 'yes'), 'TRUE or FALSE')
})
