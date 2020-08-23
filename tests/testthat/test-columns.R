context("columns")


# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

setup(suppressMessages({

  create_repository(
    name        = str_c("test-columns-", suffix),
    description = "This is a repository to test columns"
  )

  create_project(
    name = str_c("Test columns ", suffix),
    body = "A project to test columns functions",
    repo = str_c("ChadGoymer/test-columns-", suffix)
  )

}))

teardown(suppressMessages({

  delete_repository(str_c("ChadGoymer/test-columns-", suffix))

}))


# TEST: create_column ----------------------------------------------------------

test_that("create_columns creates a column and returns its properties", {

  column <- create_column(
    name    = str_c("Test column ", suffix),
    project = str_c("Test columns ", suffix),
    repo    = str_c("ChadGoymer/test-columns-", suffix)
  )

  expect_is(column, "list")
  expect_identical(attr(column, "status"), 201L)
  expect_identical(
    map_chr(column, ~ class(.)[[1]]),
    c(
      id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(column$name, str_c("Test column ", suffix))

})


# TEST: update_column ----------------------------------------------------------

test_that("update_column updates a column and returns a list of properties", {

  column <- update_column(
    column  = str_c("Test column ", suffix),
    name    = str_c("Updated test column ", suffix),
    project = str_c("Test columns ", suffix),
    repo    = str_c("ChadGoymer/test-columns-", suffix)
  )

  expect_is(column, "list")
  expect_identical(attr(column, "status"), 200L)
  expect_identical(
    map_chr(column, ~ class(.)[[1]]),
    c(
      id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(column$name, str_c("Updated test column ", suffix))

})


# TEST: move_column ------------------------------------------------------------

test_that("move_column changes the position of a column", {

  column2 <- create_column(
    name    = str_c("Test column 2 ", suffix),
    project = str_c("Test columns ", suffix),
    repo    = str_c("ChadGoymer/test-columns-", suffix)
  )

  first_column <- move_column(
    column   = str_c("Test column 2 ", suffix),
    position = "first",
    project  = str_c("Test columns ", suffix),
    repo     = str_c("ChadGoymer/test-columns-", suffix)
  )

  expect_is(first_column, "list")
  expect_identical(attr(first_column, "status"), 201L)
  expect_identical(
    map_chr(first_column, ~ class(.)[[1]]),
    c(
      id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  last_column <- move_column(
    column   = str_c("Test column 2 ", suffix),
    position = "last",
    project  = str_c("Test columns ", suffix),
    repo     = str_c("ChadGoymer/test-columns-", suffix)
  )

  expect_is(last_column, "list")
  expect_identical(attr(last_column, "status"), 201L)
  expect_identical(
    map_chr(last_column, ~ class(.)[[1]]),
    c(
      id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  after_column <- move_column(
    column  = str_c("Updated test column ", suffix),
    after   = str_c("Test column 2 ", suffix),
    project = str_c("Test columns ", suffix),
    repo    = str_c("ChadGoymer/test-columns-", suffix)
  )

  expect_is(after_column, "list")
  expect_identical(attr(after_column, "status"), 201L)
  expect_identical(
    map_chr(after_column, ~ class(.)[[1]]),
    c(
      id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

})

test_that("move_column throws an error in invalid arguments are supplied", {

  expect_error(
    move_column(
      column  = str_c("Test column 2 ", suffix),
      project = str_c("Test columns ", suffix),
      repo    = str_c("ChadGoymer/test-columns-", suffix)
    ),
    "Either 'position' or 'after' must be supplied"
  )

})


# TEST: view_columns -----------------------------------------------------------

test_that("view_columns returns a tibble summarising the columns", {

  columns <- view_columns(
    project = str_c("Test columns ", suffix),
    repo    = str_c("ChadGoymer/test-columns-", suffix),
    n_max   = 10
  )

  expect_is(columns, "tbl")
  expect_identical(attr(columns, "status"), 200L)
  expect_identical(
    map_chr(columns, ~ class(.)[[1]]),
    c(
      id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_true(str_c("Updated test column ", suffix) %in% columns$name)

})


# TEST: view_column ------------------------------------------------------------

test_that("view_column returns a list of column properties", {

  column <- view_column(
    column  = str_c("Updated test column ", suffix),
    project = str_c("Test columns ", suffix),
    repo    = str_c("ChadGoymer/test-columns-", suffix)
  )

  expect_is(column, "list")
  expect_identical(attr(column, "status"), 200L)
  expect_identical(
    map_chr(column, ~ class(.)[[1]]),
    c(
      id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(column$name, str_c("Updated test column ", suffix))

  column_by_id <- view_column(column = column$id)

  expect_is(column_by_id, "list")
  expect_identical(attr(column_by_id, "status"), 200L)
  expect_identical(
    map_chr(column_by_id, ~ class(.)[[1]]),
    c(
      id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(column_by_id$id, column$id)

})

test_that("view_column can accept a column number", {

  columns <- view_columns(
    project = str_c("Test columns ", suffix),
    repo    = str_c("ChadGoymer/test-columns-", suffix)
  )

  first_column <- view_column(
    column  = columns$id[[1]],
    project = str_c("Test columns ", suffix),
    repo    = str_c("ChadGoymer/test-columns-", suffix)
  )

  expect_is(first_column, "list")
  expect_identical(attr(first_column, "status"), 200L)
  expect_identical(
    map_chr(first_column, ~ class(.)[[1]]),
    c(
      id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(first_column$id, columns$id[[1]])

})

test_that("view_column throws an error if invalid arguments are supplied", {

  expect_error(
    view_column(
      column  = TRUE,
      project = str_c("Test columns ", suffix),
      repo    = str_c("ChadGoymer/test-columns-", suffix)
    ),
    "'column' must be either an integer or a string"
  )

})


# TEST: delete_column ----------------------------------------------------------

test_that("delete_column deletes the columns and returns TRUE", {

  column <- delete_column(
    column  = str_c("Updated test column ", suffix),
    project = str_c("Test columns ", suffix),
    repo    = str_c("ChadGoymer/test-columns-", suffix)
  )

  expect_is(column, "logical")
  expect_identical(attr(column, "status"), 204L)
  expect_identical(as.logical(column), TRUE)

})
