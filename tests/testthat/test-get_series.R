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

  my.id <- c('Selic' = 432)
  df.bcb <- gbcbd_get_series(my.id,
                             first.date = Sys.Date() - 15*365,
                             use.memoise = FALSE)

  test_df(df.bcb)
})

test_that("sequential strategy (with memoise)", {

  my_skip_tests()

  my.id <- c('Selic' = 432)
  df.bcb <- gbcbd_get_series(my.id,
                             first.date = Sys.Date() - 15*365,
                             use.memoise = TRUE)

  test_df(df.bcb)
})

test_that("Vanilla call (no cache)", {

  my_skip_tests()

  my.id <- c('Selic' = 432)
  df.bcb <- gbcbd_get_series(my.id,
                             use.memoise = FALSE)

  test_df(df.bcb)
})

test_that("Vanilla call (with cache)", {

  my_skip_tests()

  my.id <- c('Selic' = 432)
  df.bcb <- gbcbd_get_series(my.id,
                             use.memoise = TRUE)

  test_df(df.bcb)
})


test_that("Multiple Series (with cache)", {

  my_skip_tests()

  my.id <- c('Selic' = 432, "NOTSURE" = 1 )
  df.bcb <- gbcbd_get_series(my.id,
                             use.memoise = TRUE)

  test_df(df.bcb)
})

test_that("Wide format", {

  my_skip_tests()

  my.id <- c('Selic' = 432,
             'other' = 11,
             'another' = 1839)
  df.bcb <- gbcbd_get_series(my.id,
                             format.data = 'wide')

  test_df(df.bcb)
})

test_that("Wide format keeps structure when a series fails", {

  my_skip_tests()

  my.id <- c('Selic' = 432, 'BAD' = 999999)
  df.bcb <- gbcbd_get_series(my.id,
                             first.date = Sys.Date() - 30,
                             last.date = Sys.Date(),
                             format.data = 'wide',
                             use.memoise = FALSE)

  expect_equal(names(df.bcb), c('ref.date', 'Selic', 'BAD'))
  test_df(df.bcb)
})

test_that("Sequential span that is a multiple of the interval", {

  my_skip_tests()

  # 12 years -> seq(by = '3 years') ends exactly on last.date
  my.id <- c('Selic' = 432)
  expect_silent(
    df.bcb <- gbcbd_get_series(my.id,
                               first.date = '2010-01-01',
                               last.date = '2022-01-01',
                               use.memoise = FALSE,
                               be.quiet = TRUE)
  )

  test_df(df.bcb)
})
