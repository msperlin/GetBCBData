test_df <- function(df_in) {

  expect_true(nrow(df_in) > 0)
  expect_true(ncol(df_in) > 1)

  return(invisible(TRUE))
}

my_skip_tests <- function() {
  skip_if_offline()
  skip_on_cran() # too heavy for cran
}

test_that("sequential strategy (without memoise)", {

  my_skip_tests()

  my_id <- c('Selic' = 432)
  df_bcb <- gbcbd_get_series(my_id,
                             first_date = Sys.Date() - 15*365,
                             use_memoise = FALSE)

  test_df(df_bcb)
})

test_that("sequential strategy (with memoise)", {

  my_skip_tests()

  my_id <- c('Selic' = 432)
  df_bcb <- gbcbd_get_series(my_id,
                             first_date = Sys.Date() - 15*365,
                             use_memoise = TRUE)

  test_df(df_bcb)
})

test_that("Vanilla call (no cache)", {

  my_skip_tests()

  my_id <- c('Selic' = 432)
  df_bcb <- gbcbd_get_series(my_id,
                             use_memoise = FALSE)

  test_df(df_bcb)
})

test_that("Vanilla call (with cache)", {

  my_skip_tests()

  my_id <- c('Selic' = 432)
  df_bcb <- gbcbd_get_series(my_id,
                             use_memoise = TRUE)

  test_df(df_bcb)
})


test_that("Multiple Series (with cache)", {

  my_skip_tests()

  my_id <- c('Selic' = 432, "NOTSURE" = 1 )
  df_bcb <- gbcbd_get_series(my_id,
                             use_memoise = TRUE)

  test_df(df_bcb)
})

test_that("Wide format", {

  my_skip_tests()

  my_id <- c('Selic' = 432,
             'other' = 11,
             'another' = 1839)
  df_bcb <- gbcbd_get_series(my_id,
                             format_data = 'wide')

  test_df(df_bcb)
})

test_that("Wide format keeps structure when a series fails", {

  my_skip_tests()

  my_id <- c('Selic' = 432, 'BAD' = 999999)
  df_bcb <- gbcbd_get_series(my_id,
                             first_date = Sys.Date() - 30,
                             last_date = Sys.Date(),
                             format_data = 'wide',
                             use_memoise = FALSE)

  expect_equal(names(df_bcb), c('ref_date', 'Selic', 'BAD'))
  test_df(df_bcb)
})

test_that("Sequential span that is a multiple of the interval", {

  my_skip_tests()

  # 12 years -> seq(by = '3 years') ends exactly on last_date
  my_id <- c('Selic' = 432)
  expect_silent(
    df_bcb <- gbcbd_get_series(my_id,
                               first_date = '2010-01-01',
                               last_date = '2022-01-01',
                               use_memoise = FALSE,
                               be_quiet = TRUE)
  )

  test_df(df_bcb)
})

test_that("deprecated dot-case arguments still work", {

  my_skip_tests()

  expect_warning(
    df_bcb <- gbcbd_get_series(c('Selic' = 432),
                               first.date = Sys.Date() - 30,
                               format_data = 'long',
                               be_quiet = TRUE),
    'deprecated'
  )

  test_df(df_bcb)
})

test_that("supplying both new and deprecated argument names errors", {

  expect_error(
    gbcbd_get_series(c('Selic' = 432),
                     first_date = Sys.Date(),
                     first.date = Sys.Date(),
                     be_quiet = TRUE),
    'not both'
  )
})
