# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  user <- view_user()

  repo <- create_repository(
    name        = str_c("test-releases-", suffix),
    description = "This is a repository to test releases",
    auto_init   = TRUE
  )

  Sys.sleep(1)

  create_file(
    content = "This is a commit to test releases",
    path    = str_c("test-releases-", suffix, ".txt"),
    branch  = str_c("test-releases-", suffix),
    message = "Commit to test releases",
    repo    = repo$full_name,
    parent  = repo$default_branch
  )

})

teardown(suppressMessages({

  try(delete_repository(repo$full_name), silent = TRUE)

}))


# TEST: create_release ---------------------------------------------------------

test_that("create_release creates a release and returns the properties", {

  main_release <- create_release(
    tag  = str_c("main-release-", suffix),
    repo = repo$full_name,
    name = str_c("main release ", suffix),
    body = "This is a release created by create_release()"
  )

  expect_is(main_release, "list")
  expect_identical(attr(main_release, "status"), 201L)
  expect_identical(
    map_chr(main_release, ~ class(.)[[1]]),
    c(
      id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"
    )
  )

  expect_identical(main_release$tag, str_c("main-release-", suffix))
  expect_identical(main_release$name, str_c("main release ", suffix))
  expect_identical(
    main_release$body,
    "This is a release created by create_release()"
  )
  expect_identical(main_release$commit, repo$default_branch)
  expect_false(main_release$draft)
  expect_false(main_release$prerelease)
  expect_identical(main_release$author_login, user$login)


  branch_release <- create_release(
    tag  = str_c("branch-release-", suffix),
    repo = repo$full_name,
    name = str_c("Branch release ", suffix),
    body = "This is a release created by create_release()",
    ref  = str_c("test-releases-", suffix)
  )

  expect_is(branch_release, "list")
  expect_identical(attr(branch_release, "status"), 201L)
  expect_identical(
    map_chr(branch_release, ~ class(.)[[1]]),
    c(
      id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"
    )
  )

  expect_identical(branch_release$tag, str_c("branch-release-", suffix))
  expect_identical(branch_release$name, str_c("Branch release ", suffix))
  expect_identical(
    branch_release$body,
    "This is a release created by create_release()"
  )
  expect_identical(branch_release$commit, str_c("test-releases-", suffix))
  expect_false(branch_release$draft)
  expect_false(branch_release$prerelease)
  expect_identical(branch_release$author_login, user$login)


  draft_release <- create_release(
    tag        = str_c("draft-release-", suffix),
    repo       = repo$full_name,
    name       = str_c("Draft release ", suffix),
    body       = "This is a release created by create_release()",
    draft      = TRUE,
    prerelease = TRUE
  )

  expect_is(draft_release, "list")
  expect_identical(attr(draft_release, "status"), 201L)
  expect_identical(
    map_chr(draft_release, ~ class(.)[[1]]),
    c(
      id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"
    )
  )

  expect_identical(draft_release$tag, str_c("draft-release-", suffix))
  expect_identical(draft_release$name, str_c("Draft release ", suffix))
  expect_identical(
    draft_release$body,
    "This is a release created by create_release()"
  )
  expect_identical(draft_release$commit, repo$default_branch)
  expect_true(draft_release$draft)
  expect_true(draft_release$prerelease)
  expect_identical(draft_release$author_login, user$login)

})


# TEST: update_release ---------------------------------------------------------

draft_release_id <- suppressMessages({
  view_releases(repo$full_name) %>%
    filter(.data$name == str_c("Draft release ", suffix)) %>%
    pull(id)
})

test_that("update_release updates a release and returns the properties", {

  main_release <- update_release(
    release = str_c("main-release-", suffix),
    repo    = repo$full_name,
    tag     = str_c("updated-main-release-", suffix),
    name    = str_c("Updated main release ", suffix),
    body    = "This release has been updated by update_release()"
  )

  expect_is(main_release, "list")
  expect_identical(attr(main_release, "status"), 200L)
  expect_identical(
    map_chr(main_release, ~ class(.)[[1]]),
    c(
      id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"
    )
  )

  expect_identical(main_release$tag, str_c("updated-main-release-", suffix))
  expect_identical(main_release$name, str_c("Updated main release ", suffix))
  expect_identical(
    main_release$body,
    "This release has been updated by update_release()"
  )
  expect_identical(main_release$commit, repo$default_branch)
  expect_false(main_release$draft)
  expect_false(main_release$prerelease)
  expect_identical(main_release$author_login, user$login)


  branch_release <- update_release(
    release = str_c("branch-release-", suffix),
    repo    = repo$full_name,
    tag     = str_c("updated-branch-release-", suffix),
    name    = str_c("Updated branch release ", suffix),
    body    = "This release has been updated by update_release()",
    ref     = str_c("test-releases-", suffix)
  )

  expect_is(branch_release, "list")
  expect_identical(attr(branch_release, "status"), 200L)
  expect_identical(
    map_chr(branch_release, ~ class(.)[[1]]),
    c(
      id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"
    )
  )

  expect_identical(branch_release$tag, str_c("updated-branch-release-", suffix))
  expect_identical(
    branch_release$name,
    str_c("Updated branch release ", suffix)
  )
  expect_identical(
    branch_release$body,
    "This release has been updated by update_release()"
  )
  expect_identical(branch_release$commit, str_c("test-releases-", suffix))
  expect_false(branch_release$draft)
  expect_false(branch_release$prerelease)
  expect_identical(branch_release$author_login, user$login)


  draft_release <- update_release(
    release    = draft_release_id,
    tag        = str_c("updated-draft-release-", suffix),
    repo       = repo$full_name,
    name       = str_c("Updated draft release ", suffix),
    body       = "This release has been updated by update_release()",
    draft      = FALSE,
    prerelease = FALSE
  )

  expect_is(draft_release, "list")
  expect_identical(attr(draft_release, "status"), 200L)
  expect_identical(
    map_chr(draft_release, ~ class(.)[[1]]),
    c(
      id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"
    )
  )

  expect_identical(draft_release$tag, str_c("updated-draft-release-", suffix))
  expect_identical(draft_release$name, str_c("Updated draft release ", suffix))
  expect_identical(
    draft_release$body,
    "This release has been updated by update_release()"
  )
  expect_identical(draft_release$commit, repo$default_branch)
  expect_false(draft_release$draft)
  expect_false(draft_release$prerelease)
  expect_identical(draft_release$author_login, user$login)

})


# TEST: view_releases ----------------------------------------------------------

test_that("view_releases returns a tibble of release properties", {

  all_releases <- view_releases(
    repo  = repo$full_name,
    n_max = 10
  )

  expect_is(all_releases, "tbl")
  expect_identical(attr(all_releases, "status"), 200L)
  expect_identical(
    map_chr(all_releases, ~ class(.)[[1]]),
    c(
      id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "list",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"
    )
  )

  expect_true(str_c("updated-main-release-", suffix) %in% all_releases$tag)
  expect_true(str_c("updated-branch-release-", suffix) %in% all_releases$tag)
  expect_true(str_c("updated-draft-release-", suffix) %in% all_releases$tag)

})


# TEST: view_release -----------------------------------------------------------

test_that("view_release returns a list of release properties", {

  main_release <- view_release(
    release = str_c("updated-main-release-", suffix),
    repo    = repo$full_name
  )

  expect_is(main_release, "list")
  expect_identical(attr(main_release, "status"), 200L)
  expect_identical(
    map_chr(main_release, ~ class(.)[[1]]),
    c(
      id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"
    )
  )

  expect_identical(main_release$tag, str_c("updated-main-release-", suffix))
  expect_identical(main_release$name, str_c("Updated main release ", suffix))
  expect_identical(
    main_release$body,
    "This release has been updated by update_release()"
  )
  expect_identical(main_release$commit, repo$default_branch)
  expect_false(main_release$draft)
  expect_false(main_release$prerelease)
  expect_identical(main_release$author_login, user$login)


  draft_release <- view_release(
    release = draft_release_id,
    repo    = repo$full_name
  )

  expect_is(draft_release, "list")
  expect_identical(attr(draft_release, "status"), 200L)
  expect_identical(
    map_chr(draft_release, ~ class(.)[[1]]),
    c(
      id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"
    )
  )

  expect_identical(draft_release$tag, str_c("updated-draft-release-", suffix))
  expect_identical(draft_release$name, str_c("Updated draft release ", suffix))
  expect_identical(
    draft_release$body,
    "This release has been updated by update_release()"
  )
  expect_identical(draft_release$commit, repo$default_branch)
  expect_false(draft_release$draft)
  expect_false(draft_release$prerelease)
  expect_identical(draft_release$author_login, user$login)

})

test_that("view_release throws as error if invalid arguments are supplied", {

  expect_error(
    view_release(
      release = list(1),
      repo    = repo$full_name
    ),
    "'release' must be either an integer or a valid git reference"
  )

})


# TEST: browse_release ---------------------------------------------------------

test_that("browse_release opens the release's page in the browser", {

  skip_if(!interactive(), "browse_release must be tested manually")

  main_release <- browse_release(
    release = str_c("updated-main-release-", suffix),
    repo    = repo$full_name
  )

  base_url <- getOption("github.oauth") %>%
    str_remove("login/oauth") %>%
    str_c(user$login)

  expect_is(main_release, "character")
  expect_identical(attr(main_release, "status"), 200L)
  expect_identical(
    dirname(main_release),
    str_c(base_url, "/test-releases-", suffix, "/releases/tag")
  )


  draft_release <- browse_release(
    release = draft_release_id,
    repo    = repo$full_name
  )

  expect_is(draft_release, "character")
  expect_identical(attr(draft_release, "status"), 200L)
  expect_identical(
    dirname(draft_release),
    str_c(base_url, "/test-releases-", suffix, "/releases/tag")
  )

})

test_that("browse_release throws as error if invalid arguments are supplied", {

  expect_error(
    browse_release(
      release = list(1),
      repo    = repo$full_name
    ),
    "'release' must be either an integer or a valid git reference"
  )

})


# TEST: delete_release ---------------------------------------------------------

test_that("delete_release deletes a release", {

  deleted_release <- delete_release(
    release = str_c("updated-main-release-", suffix),
    repo    = repo$full_name
  )

  expect_is(deleted_release, "logical")
  expect_identical(attr(deleted_release, "status"), 204L)
  expect_identical(as.logical(deleted_release), TRUE)

})
