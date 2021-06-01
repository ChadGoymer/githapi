# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  user <- view_user()

  repo <- create_repository(
    name        = str_c("test-pulls-", suffix),
    description = "This is a repository to test pull requests",
    auto_init   = TRUE
  )

  Sys.sleep(1)

  create_file(
    content = "This is a commit to test pull requests",
    path    = str_c("test-pulls-", suffix, ".txt"),
    branch  = str_c("test-pulls-1-", suffix),
    message = "Commit to test pull requests",
    repo    = repo$full_name,
    parent  = repo$default_branch
  )

  create_file(
    content = "This is a repository to test pull requests",
    path    = str_c("test-pulls-", suffix, ".txt"),
    branch  = str_c("test-pulls-2-", suffix),
    message = "Commit to test pull requests",
    repo    = repo$full_name,
    parent  = repo$default_branch
  )

  create_milestone(
    title       = str_c("test-pulls-", suffix),
    repo        = repo$full_name,
    description = "This is a milestone to test pull requests"
  )

  create_label(
    name        = str_c("test-pulls-", suffix),
    repo        = repo$full_name,
    description = "This is a label to test pull requests"
  )

})

teardown(suppressMessages({

  try(delete_repository(repo$full_name), silent = TRUE)

}))


# TEST: create_pull_request ----------------------------------------------------

test_that("create_pull_request creates a pull request and returns properties", {

  pull_request <- create_pull_request(
    title = str_c("test pull request ", suffix),
    repo  = repo$full_name,
    head  = str_c("test-pulls-1-", suffix),
    base  = repo$default_branch,
    body  = "This is a pull request to test create_pull_request()"
  )

  expect_is(pull_request, "list")
  expect_identical(attr(pull_request, "status"), 201L)
  expect_identical(
    map_chr(pull_request, ~ class(.)[[1]]),
    c(
      number     = "integer",
      title      = "character",
      body       = "character",
      head_sha   = "character",
      head_ref   = "character",
      head_repo  = "character",
      base_sha   = "character",
      base_ref   = "character",
      merge_sha  = "character",
      assignees  = "character",
      reviewers  = "character",
      labels     = "character",
      milestone  = "character",
      state      = "character",
      repository = "character",
      diff_url   = "character",
      creator    = "character",
      mergeable  = "logical",
      rebaseable = "logical",
      merged     = "logical",
      merged_by  = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      merged_at  = "POSIXct",
      closed_at  = "POSIXct"
    )
  )

  expect_identical(pull_request$title, str_c("test pull request ", suffix))
  expect_identical(
    pull_request$repository,
    repo$full_name
  )
  expect_identical(pull_request$head_ref, str_c("test-pulls-1-", suffix))
  expect_identical(pull_request$base_ref, repo$default_branch)
  expect_identical(
    pull_request$body,
    "This is a pull request to test create_pull_request()"
  )
  expect_identical(pull_request$state, "open")

  assigned_pull_request <- create_pull_request(
    title     = str_c("test assigned pull request ", suffix),
    repo      = repo$full_name,
    head      = str_c("test-pulls-2-", suffix),
    base      = repo$default_branch,
    body      = "This is a pull request to test create_pull_request()",
    assignees = user$login,
    labels    = str_c("test-pulls-", suffix),
    milestone = str_c("test-pulls-", suffix)
  )

  expect_is(assigned_pull_request, "list")
  expect_identical(attr(assigned_pull_request, "status"), 201L)
  expect_identical(
    map_chr(assigned_pull_request, ~ class(.)[[1]]),
    c(
      number     = "integer",
      title      = "character",
      body       = "character",
      head_sha   = "character",
      head_ref   = "character",
      head_repo  = "character",
      base_sha   = "character",
      base_ref   = "character",
      merge_sha  = "character",
      assignees  = "character",
      reviewers  = "character",
      labels     = "character",
      milestone  = "character",
      state      = "character",
      repository = "character",
      diff_url   = "character",
      creator    = "character",
      mergeable  = "logical",
      rebaseable = "logical",
      merged     = "logical",
      merged_by  = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      merged_at  = "POSIXct",
      closed_at  = "POSIXct"
    )
  )

  expect_identical(
    assigned_pull_request$title,
    str_c("test assigned pull request ", suffix)
  )
  expect_identical(
    assigned_pull_request$repository,
    repo$full_name
  )
  expect_identical(
    assigned_pull_request$head_ref,
    str_c("test-pulls-2-", suffix)
  )
  expect_identical(assigned_pull_request$base_ref, repo$default_branch)
  expect_identical(
    assigned_pull_request$body,
    "This is a pull request to test create_pull_request()"
  )
  expect_identical(assigned_pull_request$state, "open")
  expect_identical(assigned_pull_request$assignees, user$login)
  expect_identical(assigned_pull_request$labels, str_c("test-pulls-", suffix))
  expect_identical(
    assigned_pull_request$milestone,
    str_c("test-pulls-", suffix)
  )

})


# TEST: update_pull_request ----------------------------------------------------

test_that("update_pull_request updates a pull request and returns properties", {

  pull_request <- update_pull_request(
    pull_request = str_c("test pull request ", suffix),
    repo         = repo$full_name,
    title        = str_c("test updated pull request ", suffix),
    body         = "This has been updated to test create_pull_request()",
    state        = "closed",
    base         = repo$default_branch
  )

  expect_is(pull_request, "list")
  expect_identical(attr(pull_request, "status"), 200L)
  expect_identical(
    map_chr(pull_request, ~ class(.)[[1]]),
    c(
      number     = "integer",
      title      = "character",
      body       = "character",
      head_sha   = "character",
      head_ref   = "character",
      head_repo  = "character",
      base_sha   = "character",
      base_ref   = "character",
      merge_sha  = "character",
      assignees  = "character",
      reviewers  = "character",
      labels     = "character",
      milestone  = "character",
      state      = "character",
      repository = "character",
      diff_url   = "character",
      creator    = "character",
      mergeable  = "logical",
      rebaseable = "logical",
      merged     = "logical",
      merged_by  = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      merged_at  = "POSIXct",
      closed_at  = "POSIXct"
    )
  )

  expect_identical(
    pull_request$title,
    str_c("test updated pull request ", suffix)
  )
  expect_identical(
    pull_request$repository,
    repo$full_name
  )
  expect_identical(
    pull_request$body,
    "This has been updated to test create_pull_request()"
  )
  expect_identical(pull_request$state, "closed")

  assigned_pull_request <- update_pull_request(
    pull_request = 2,
    repo         = repo$full_name,
    assignees = user$login,
    labels    = str_c("test-pulls-", suffix),
    milestone = str_c("test-pulls-", suffix)
  )

  expect_is(assigned_pull_request, "list")
  expect_identical(attr(assigned_pull_request, "status"), 200L)
  expect_identical(
    map_chr(assigned_pull_request, ~ class(.)[[1]]),
    c(
      number     = "integer",
      title      = "character",
      body       = "character",
      head_sha   = "character",
      head_ref   = "character",
      head_repo  = "character",
      base_sha   = "character",
      base_ref   = "character",
      merge_sha  = "character",
      assignees  = "character",
      reviewers  = "character",
      labels     = "character",
      milestone  = "character",
      state      = "character",
      repository = "character",
      diff_url   = "character",
      creator    = "character",
      mergeable  = "logical",
      rebaseable = "logical",
      merged     = "logical",
      merged_by  = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      merged_at  = "POSIXct",
      closed_at  = "POSIXct"
    )
  )

  expect_identical(
    assigned_pull_request$title,
    str_c("test assigned pull request ", suffix)
  )
  expect_identical(
    assigned_pull_request$repository,
    repo$full_name
  )
  expect_identical(
    assigned_pull_request$head_ref,
    str_c("test-pulls-2-", suffix)
  )
  expect_identical(assigned_pull_request$base_ref, repo$default_branch)
  expect_identical(
    assigned_pull_request$body,
    "This is a pull request to test create_pull_request()"
  )
  expect_identical(assigned_pull_request$state, "open")
  expect_identical(assigned_pull_request$assignees, user$login)
  expect_identical(assigned_pull_request$labels, str_c("test-pulls-", suffix))
  expect_identical(
    assigned_pull_request$milestone,
    str_c("test-pulls-", suffix)
  )

})

test_that("update_pull_request throws an error with invalid arguments", {

  expect_error(
    update_pull_request(TRUE, repo = repo$full_name),
    "'pull_request' must be either an integer or a string"
  )

})


# TEST: view_pull_requests -----------------------------------------------------

test_that("view_pull_requests returns a tibble of issue properties", {

  open_pull_requests <- view_pull_requests(
    repo  = repo$full_name,
    base  = repo$default_branch,
    n_max = 10
  )

  expect_is(open_pull_requests, "tbl")
  expect_identical(attr(open_pull_requests, "status"), 200L)
  expect_identical(
    map_chr(open_pull_requests, ~ class(.)[[1]]),
    c(
      number     = "integer",
      title      = "character",
      body       = "character",
      head_sha   = "character",
      head_ref   = "character",
      head_repo  = "character",
      base_sha   = "character",
      base_ref   = "character",
      merge_sha  = "character",
      assignees  = "list",
      reviewers  = "list",
      labels     = "list",
      milestone  = "character",
      state      = "character",
      repository = "character",
      diff_url   = "character",
      creator    = "character",
      mergeable  = "logical",
      rebaseable = "logical",
      merged     = "logical",
      merged_by  = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      merged_at  = "POSIXct",
      closed_at  = "POSIXct"
    )
  )

  expect_true(
    str_c("test assigned pull request ", suffix) %in% open_pull_requests$title
  )

  closed_pull_requests <- view_pull_requests(
    repo  = repo$full_name,
    head  = str_c("test-pulls-1-", suffix),
    state = "closed",
    n_max = 10
  )

  expect_is(closed_pull_requests, "tbl")
  expect_identical(attr(closed_pull_requests, "status"), 200L)
  expect_identical(
    map_chr(closed_pull_requests, ~ class(.)[[1]]),
    c(
      number     = "integer",
      title      = "character",
      body       = "character",
      head_sha   = "character",
      head_ref   = "character",
      head_repo  = "character",
      base_sha   = "character",
      base_ref   = "character",
      merge_sha  = "character",
      assignees  = "list",
      reviewers  = "list",
      labels     = "list",
      milestone  = "character",
      state      = "character",
      repository = "character",
      diff_url   = "character",
      creator    = "character",
      mergeable  = "logical",
      rebaseable = "logical",
      merged     = "logical",
      merged_by  = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      merged_at  = "POSIXct",
      closed_at  = "POSIXct"
    )
  )

  expect_true(
    str_c("test updated pull request ", suffix) %in% closed_pull_requests$title
  )

})


# TEST: view_pull_request ------------------------------------------------------

test_that("view_pull_request returns a list of pull request properties", {

  pull_request <- view_pull_request(
    pull_request = str_c("test assigned pull request ", suffix),
    repo         = repo$full_name
  )

  expect_is(pull_request, "list")
  expect_identical(attr(pull_request, "status"), 200L)
  expect_identical(
    map_chr(pull_request, ~ class(.)[[1]]),
    c(
      number     = "integer",
      title      = "character",
      body       = "character",
      head_sha   = "character",
      head_ref   = "character",
      head_repo  = "character",
      base_sha   = "character",
      base_ref   = "character",
      merge_sha  = "character",
      assignees  = "character",
      reviewers  = "character",
      labels     = "character",
      milestone  = "character",
      state      = "character",
      repository = "character",
      diff_url   = "character",
      creator    = "character",
      mergeable  = "logical",
      rebaseable = "logical",
      merged     = "logical",
      merged_by  = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      merged_at  = "POSIXct",
      closed_at  = "POSIXct",
      commits    = "github",
      files      = "github",
      reviews    = "github"
    )
  )

  expect_identical(
    pull_request$title,
    str_c("test assigned pull request ", suffix)
  )
  expect_identical(
    pull_request$repository,
    repo$full_name
  )
  expect_identical(
    pull_request$body,
    "This is a pull request to test create_pull_request()"
  )
  expect_identical(pull_request$state, "open")

  expect_identical(
    map_chr(pull_request$commits, ~ class(.)[[1]]),
    c(
      message         = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      parents         = "list",
      html_url        = "character"
    )
  )

  expect_identical(pull_request$commits$message, "Commit to test pull requests")
  expect_identical(pull_request$commits$author_name, user$name)
  expect_identical(pull_request$commits$committer_name, user$name)

  expect_identical(
    map_chr(pull_request$files, ~ class(.)[[1]]),
    c(
      sha       = "character",
      filename  = "character",
      status    = "character",
      additions = "integer",
      deletions = "integer",
      changes   = "integer",
      patch     = "character",
      html_url  = "character"
    )
  )

  expect_identical(
    pull_request$files$filename,
    str_c("test-pulls-", suffix, ".txt")
  )
  expect_identical(pull_request$files$status, "added")
  expect_identical(pull_request$files$additions, 1L)
  expect_identical(pull_request$files$deletions, 0L)
  expect_identical(pull_request$files$changes, 1L)

  expect_identical(
    map_chr(pull_request$reviews, ~ class(.)[[1]]),
    c(body         = "character",
      state        = "character",
      user         = "character",
      html_url     = "character",
      submitted_at = "POSIXct"))

})

test_that("view_pull_request throws an error with invalid arguments", {

  expect_error(
    view_pull_request(TRUE, repo = repo$full_name),
    "'pull_request' must be either an integer or a string"
  )

})


# TEST: browse_pull_request ----------------------------------------------------

test_that("browse_pull_request opens the pull request's page in the browser", {

  skip_if(!interactive(), "browse_pull_request must be tested manually")

  pull_url <- browse_pull_request(
    pull_request = 1,
    repo         = repo$full_name
  )

  base_url <- getOption("github.oauth") %>%
    str_remove("login/oauth") %>%
    str_c(user$login)

  expect_is(pull_url, "character")
  expect_identical(attr(pull_url, "status"), 200L)
  expect_identical(
    as.character(pull_url),
    str_c(base_url, "/test-pulls-", suffix, "/pull/1")
  )

})
