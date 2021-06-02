# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  repo <- create_repository(
    name        = str_c("test-labels-", suffix),
    description = "This is a repository to test labels"
  )

  create_issue(
    title     = str_c("test labels ", suffix),
    repo      = repo$full_name,
    body      = "This is an issue to test add_labels() and remove_labels()"
  )

})

teardown(suppressMessages({

  try(delete_repository(repo$full_name), silent = TRUE)

}))


# TEST: create_label -----------------------------------------------------------

test_that("create_label creates a label and returns a list of the properties", {

  simple_label <- create_label(
    name  = "simple label",
    repo  = repo$full_name,
    color = "blue"
  )

  expect_is(simple_label, "list")
  expect_identical(attr(simple_label, "status"), 201L)
  expect_identical(
    map_chr(simple_label, ~ class(.)[[1]]),
    c(
      name        = "character",
      color       = "character",
      description = "character"
    )
  )

  expect_identical(simple_label$name, "simple label")
  expect_identical(simple_label$color, "0000FF")

  detailed_label <- create_label(
    name        = "detailed-label",
    repo        = repo$full_name,
    color       = "green",
    description = "This is a detailed label"
  )

  expect_is(detailed_label, "list")
  expect_identical(attr(detailed_label, "status"), 201L)
  expect_identical(
    map_chr(detailed_label, ~ class(.)[[1]]),
    c(
      name        = "character",
      color       = "character",
      description = "character"
    )
  )

  expect_identical(detailed_label$name, "detailed-label")
  expect_identical(detailed_label$color, "00FF00")
  expect_identical(detailed_label$description, "This is a detailed label")

})


# TEST: update_label -----------------------------------------------------------

test_that("update_label changes a label and returns a list of the properties", {

  updated_label <- update_label(
    label       = "simple label",
    repo        = repo$full_name,
    name        = "updated label",
    color       = "pink",
    description = "This is an updated label"
  )

  expect_is(updated_label, "list")
  expect_identical(attr(updated_label, "status"), 200L)
  expect_identical(
    map_chr(updated_label, ~ class(.)[[1]]),
    c(
      name        = "character",
      color       = "character",
      description = "character"
    )
  )

  expect_identical(updated_label$name, "updated label")
  expect_identical(updated_label$color, "FFC0CB")
  expect_identical(updated_label$description, "This is an updated label")

})


# TEST: add_labels -------------------------------------------------------------

test_that("add_labels adds labels to an issue and returns the properties", {

  added_labels <- add_labels(
    labels = c("updated label", "detailed-label"),
    issue  = str_c("test labels ", suffix),
    repo   = repo$full_name
  )

  expect_is(added_labels, "tbl")
  expect_identical(attr(added_labels, "status"), 200L)
  expect_identical(
    map_chr(added_labels, ~ class(.)[[1]]),
    c(
      name        = "character",
      color       = "character",
      description = "character"
    )
  )

  expect_true(all(c("updated label", "detailed-label") %in% added_labels$name))

})


# TEST: view_labels ------------------------------------------------------------

test_that("view_labels returns a tibble of label properties", {

  repo_labels <- view_labels(
    repo  = repo$full_name,
    n_max = 10
  )

  expect_is(repo_labels, "tbl")
  expect_identical(attr(repo_labels, "status"), 200L)
  expect_identical(
    map_chr(repo_labels, ~ class(.)[[1]]),
    c(
      name        = "character",
      color       = "character",
      description = "character"
    )
  )

  expect_true(all(c("updated label", "detailed-label") %in% repo_labels$name))

  issue_labels <- view_labels(
    issue = str_c("test labels ", suffix),
    repo  = repo$full_name,
    n_max = 10
  )

  expect_is(issue_labels, "tbl")
  expect_identical(attr(issue_labels, "status"), 200L)
  expect_identical(
    map_chr(issue_labels, ~ class(.)[[1]]),
    c(
      name        = "character",
      color       = "character",
      description = "character"
    )
  )

  expect_true(all(c("updated label", "detailed-label") %in% issue_labels$name))

})


# TEST: view_label -------------------------------------------------------------

test_that("view_label returns a list of repository properties", {

  detailed_label <- view_label(
    label = "detailed-label",
    repo  = repo$full_name
  )

  expect_is(detailed_label, "list")
  expect_identical(attr(detailed_label, "status"), 200L)
  expect_identical(
    map_chr(detailed_label, ~ class(.)[[1]]),
    c(
      name        = "character",
      color       = "character",
      description = "character"
    )
  )

  expect_identical(detailed_label$name, "detailed-label")

})


# TEST: remove_labels ----------------------------------------------------------

test_that("add_labels adds labels to an issue and returns the properties", {

  removed_labels <- remove_labels(
    labels = c("updated label", "detailed-label"),
    issue  = str_c("test labels ", suffix),
    repo   = repo$full_name
  )

  expect_is(removed_labels, "tbl")
  expect_identical(attr(removed_labels, "status"), 200L)
  expect_identical(
    map_chr(removed_labels, ~ class(.)[[1]]),
    c(
      name        = "character",
      color       = "character",
      description = "character"
    )
  )

  expect_identical(nrow(removed_labels), 0L)

})


# TEST: delete_label -----------------------------------------------------------

test_that("delete_label removes a label and returns TRUE", {

  updated_label <- delete_label(
    label = "updated label",
    repo  = repo$full_name
  )

  expect_is(updated_label, "logical")
  expect_identical(attr(updated_label, "status"), 204L)
  expect_identical(as.logical(updated_label), TRUE)

  detailed_label <- delete_label(
    label = "detailed-label",
    repo  = repo$full_name
  )

  expect_is(detailed_label, "logical")
  expect_identical(attr(detailed_label, "status"), 204L)
  expect_identical(as.logical(detailed_label), TRUE)

})
