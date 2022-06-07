test_df <- function(df_in) {

  expect_true(nrow(df_in) > 0)
  expect_true(ncol(df_in) > 1)

  return(invisible(TRUE))
}

my_skip_tests <- function() {
  skip_if_offline()
  skip_on_cran() # too heavy for cran
}

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

test_that("Long download", {

  my_skip_tests()

  my.id <- c('Selic' = 432)
  df.bcb <- gbcbd_get_series(my.id, first.date = '1980-01-01',
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

test_that("Single series (wide format)", {

  my_skip_tests()

  my.id <- c('Selic' = 432)
  df.bcb <- gbcbd_get_series(my.id,
                             format.data = 'wide')

  test_df(df.bcb)
})

test_that("Single series (long format)", {

  my_skip_tests()

  my.id <- c('Selic' = 432,
             'other' = 11,
             'another' = 1839)

  df.bcb <- gbcbd_get_series(my.id,
                             format.data = 'long')

  test_df(df.bcb)
})
