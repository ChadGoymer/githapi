# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  repo <- create_repository(
    name        = str_c("test-tags-", suffix),
    description = "This is a repository to test tags",
    auto_init   = TRUE
  )

  Sys.sleep(1)

  create_file(
    content = "This is a commit to test tags",
    path    = str_c("test-tags-", suffix, ".txt"),
    branch  = str_c("test-tags-1-", suffix),
    message = "Commit to test tags",
    repo    = repo$full_name,
    parent  = repo$default_branch
  )

})

teardown(suppressMessages({

  try(delete_repository(repo$full_name), silent = TRUE)

}))


# TEST: create_tag -------------------------------------------------------------

test_that("create_tag creates a tag and returns a list of the properties", {

  branch_sha <- view_sha(
    ref  = str_c("test-tags-1-", suffix),
    repo = repo$full_name
  )

  branch_tag <- create_tag(
    name = str_c("test-tags-1-", suffix),
    ref  = branch_sha,
    repo = repo$full_name
  )

  expect_is(branch_tag, "list")
  expect_identical(attr(branch_tag, "status"), 201L)
  expect_identical(
    map_chr(branch_tag, ~ class(.)[[1]]),
    c(
      name     = "character",
      sha      = "character",
      html_url = "character",
      zip_url  = "character",
      tar_url  = "character"
    )
  )

  expect_identical(branch_tag$name, str_c("test-tags-1-", suffix))
  expect_identical(branch_tag$sha, as.character(branch_sha))


  main_sha <- view_sha(
    ref  = repo$default_branch,
    repo = repo$full_name
  )

  main_tag <- create_tag(
    name = str_c("test-tags-2-", suffix),
    ref  = repo$default_branch,
    repo = repo$full_name
  )

  expect_is(main_tag, "list")
  expect_identical(attr(main_tag, "status"), 201L)
  expect_identical(
    map_chr(main_tag, ~ class(.)[[1]]),
    c(
      name     = "character",
      sha      = "character",
      html_url = "character",
      zip_url  = "character",
      tar_url  = "character"
    )
  )

  expect_identical(main_tag$name, str_c("test-tags-2-", suffix))
  expect_identical(main_tag$sha, as.character(main_sha))

})


# TEST: update_tag -------------------------------------------------------------

test_that("update_tag updates a tag and returns a list of the properties", {

  update_sha <- view_sha(
    ref  = str_c("test-tags-1-", suffix),
    repo = repo$full_name
  )

  updated_tag <- update_tag(
    tag  = str_c("test-tags-2-", suffix),
    ref  = str_c("test-tags-1-", suffix),
    repo = repo$full_name
  )

  expect_is(updated_tag, "list")
  expect_identical(attr(updated_tag, "status"), 200L)
  expect_identical(
    map_chr(updated_tag, ~ class(.)[[1]]),
    c(
      name     = "character",
      sha      = "character",
      html_url = "character",
      zip_url  = "character",
      tar_url  = "character"
    )
  )

  expect_identical(updated_tag$name, str_c("test-tags-2-", suffix))
  expect_identical(updated_tag$sha, as.character(update_sha))

})


# TEST: view_tags --------------------------------------------------------------

test_that("view_tags returns a tibble of tag properties", {

  all_tags <- view_tags(repo$full_name, n_max = 10)

  expect_is(all_tags, "tbl")
  expect_identical(attr(all_tags, "status"), 200L)
  expect_identical(
    map_chr(all_tags, ~ class(.)[[1]]),
    c(
      name     = "character",
      sha      = "character",
      html_url = "character",
      zip_url  = "character",
      tar_url  = "character"
    )
  )

  expect_true(all(str_c("test-tags-", 1:2, "-", suffix) %in% all_tags$name))

})


# TEST: view_tag ---------------------------------------------------------------

test_that("view_tag returns a list of tag properties", {

  tag <- view_tag(
    tag  = str_c("test-tags-1-", suffix),
    repo = repo$full_name
  )

  expect_is(tag, "list")
  expect_identical(attr(tag, "status"), 200L)
  expect_identical(
    map_chr(tag, ~ class(.)[[1]]),
    c(
      name     = "character",
      sha      = "character",
      html_url = "character",
      zip_url  = "character",
      tar_url  = "character"
    )
  )

  expect_identical(tag$name, str_c("test-tags-1-", suffix))

})


# TEST: delete_tag -------------------------------------------------------------

test_that("delete_tag deletes a tag", {

  deleted_tag <- delete_tag(
    tag  = str_c("test-tags-1-", suffix),
    repo = repo$full_name
  )

  expect_is(deleted_tag, "logical")
  expect_identical(attr(deleted_tag, "status"), 204L)
  expect_identical(as.logical(deleted_tag), TRUE)

})
