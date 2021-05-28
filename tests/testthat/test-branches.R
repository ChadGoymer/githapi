# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  repo <- create_repository(
    name        = str_c("test-branches-", suffix),
    description = "This is a repository to test branches",
    auto_init   = TRUE
  )

  Sys.sleep(1)

  create_file(
    content = "This is a commit to test branches",
    path    = str_c("test-branches-", suffix, ".txt"),
    branch  = str_c("test-branches-1-", suffix),
    message = "Commit to test branches",
    repo    = repo$full_name,
    parent  = repo$default_branch
  )

})

teardown(suppressMessages({

  try(delete_repository(repo$full_name), silent = TRUE)

}))


# TEST: create_branch ----------------------------------------------------------

test_that("create_branch creates a branch and returns a list of properties", {

  main_sha <- view_sha(ref = repo$default_branch, repo = repo$full_name)

  new_branch <- create_branch(
    name = str_c("test-branches-2-", suffix),
    ref  = repo$default_branch,
    repo = repo$full_name
  )

  expect_is(new_branch, "list")
  expect_identical(attr(new_branch, "status"), 201L)
  expect_identical(
    map_chr(new_branch, ~ class(.)[[1]]),
    c(
      name      = "character",
      sha       = "character",
      protected = "logical",
      html_url  = "character"
    )
  )

  expect_identical(new_branch$name, str_c("test-branches-2-", suffix))
  expect_identical(new_branch$sha, as.character(main_sha))

})


# TEST: update_branch ----------------------------------------------------------

test_that("update_branch updates a branch and returns a list of properties", {

  update_sha <- view_sha(
    ref = str_c("test-branches-1-", suffix),
    repo = repo$full_name
  )

  updated_branch <- update_branch(
    branch = str_c("test-branches-2-", suffix),
    ref    = str_c("test-branches-1-", suffix),
    repo   = repo$full_name
  )

  expect_is(updated_branch, "list")
  expect_identical(attr(updated_branch, "status"), 200L)
  expect_identical(
    map_chr(updated_branch, ~ class(.)[[1]]),
    c(
      name      = "character",
      sha       = "character",
      protected = "logical",
      html_url  = "character"
    )
  )

  expect_identical(updated_branch$name, str_c("test-branches-2-", suffix))
  expect_identical(updated_branch$sha, as.character(update_sha))

})


# TEST: view_branches ----------------------------------------------------------

test_that("view_branches returns a tibble of branch properties", {

  all_branches <- view_branches(
    repo  = repo$full_name,
    n_max = 10
  )

  expect_is(all_branches, "tbl")
  expect_identical(attr(all_branches, "status"), 200L)
  expect_identical(
    map_chr(all_branches, ~ class(.)[[1]]),
    c(
      name      = "character",
      sha       = "character",
      protected = "logical",
      html_url  = "character"
    )
  )

  expect_true(
    all(str_c("test-branches-", 1:2, "-", suffix) %in% all_branches$name)
  )

})


# TEST: view_branch ------------------------------------------------------------

test_that("view_branch returns a list of branch properties", {

  branch <- view_branch(
    branch = str_c("test-branches-1-", suffix),
    repo   = repo$full_name
  )

  expect_is(branch, "list")
  expect_identical(attr(branch, "status"), 200L)
  expect_identical(
    map_chr(branch, ~ class(.)[[1]]),
    c(
      name      = "character",
      sha       = "character",
      protected = "logical",
      html_url  = "character"
    )
  )

  expect_identical(branch$name, str_c("test-branches-1-", suffix))

})


# TEST: delete_branch ----------------------------------------------------------

test_that("delete_branch deletes a branch", {

  deleted_branch <- delete_branch(
    branch = str_c("test-branches-1-", suffix),
    repo   = repo$full_name
  )

  expect_is(deleted_branch, "logical")
  expect_identical(attr(deleted_branch, "status"), 204L)
  expect_identical(as.logical(deleted_branch), TRUE)

})
