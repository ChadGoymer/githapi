# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  user <- view_user()

  repo <- create_repository(
    name        = str_c("test-comments-", suffix),
    description = "This is a repository to test comments",
    auto_init   = TRUE
  )

  Sys.sleep(1)

  create_issue(
    title = str_c("issue to test comments ", suffix),
    repo  = repo$full_name,
    body  = "This is an issue to test comments"
  )

  create_file(
    content = "A file to test comments",
    path    = "test-comments.txt",
    branch  = str_c("test-comments-", suffix),
    message = "A file to test comments",
    repo    = repo$full_name,
    parent  = repo$default_branch
  )

  create_pull_request(
    title = str_c("pull to test comments ", suffix),
    repo  = repo$full_name,
    head  = str_c("test-comments-", suffix),
    base  = repo$default_branch,
    body  = "This is an pull request to test comments"
  )

})

suppressMessages({

  gist <- list("print(\"Hello World!\")") %>%
    set_names(str_c("helloworld-", suffix, ".R")) %>%
    create_gist(description = "gist for testing comments") %>%
    pluck("id")

  main_sha <- view_sha(
    ref  = repo$default_branch,
    repo = repo$full_name
  )

  branch_sha <- view_sha(
    ref  = str_c("test-comments-", suffix),
    repo = repo$full_name
  )

})

teardown(suppressMessages({

  try(
    delete_repository(repo$full_name),
    silent = TRUE
  )

  try(delete_gist(gist), silent = TRUE)

}))


# TEST: create_comment ---------------------------------------------------------

test_that("create_comment creates a comment and returns the properties", {

  gist_comment <- create_comment(
    body = "This is a comment created by create_comment()",
    gist = gist
  )

  expect_is(gist_comment, "list")
  expect_identical(attr(gist_comment, "status"), 201L)
  expect_identical(
    map_chr(gist_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    gist_comment$body,
    "This is a comment created by create_comment()"
  )
  expect_identical(gist_comment$user, user$login)


  issue_comment <- create_comment(
    body  = "This is a comment created by create_comment()",
    issue = str_c("issue to test comments ", suffix),
    repo  = repo$full_name
  )

  expect_is(issue_comment, "list")
  expect_identical(attr(issue_comment, "status"), 201L)
  expect_identical(
    map_chr(issue_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    issue_comment$body,
    "This is a comment created by create_comment()"
  )
  expect_identical(issue_comment$user, user$login)


  pull_comment <- create_comment(
    body         = "This is a comment created by create_comment()",
    pull_request = str_c("pull to test comments ", suffix),
    commit       = str_c("test-comments-", suffix),
    repo         = repo$full_name,
    path         = "test-comments.txt",
    position     = 1
  )

  expect_is(pull_comment, "list")
  expect_identical(attr(pull_comment, "status"), 201L)
  expect_identical(
    map_chr(pull_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      commit     = "character",
      path       = "character",
      position   = "integer",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    pull_comment$body,
    "This is a comment created by create_comment()"
  )
  expect_identical(pull_comment$commit, as.character(branch_sha))
  expect_identical(pull_comment$path, "test-comments.txt")
  expect_identical(pull_comment$position, 1L)
  expect_identical(pull_comment$user, user$login)


  commit_comment <- create_comment(
    body     = "This is a comment created by create_comment()",
    commit   = main_sha,
    repo     = repo$full_name,
    path     = "README.md",
    position = 1
  )

  expect_is(commit_comment, "list")
  expect_identical(attr(commit_comment, "status"), 201L)
  expect_identical(
    map_chr(commit_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      commit     = "character",
      path       = "character",
      position   = "integer",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    commit_comment$body,
    "This is a comment created by create_comment()"
  )
  expect_identical(commit_comment$commit, as.character(main_sha))
  expect_identical(commit_comment$path, "README.md")
  expect_identical(commit_comment$position, 1L)
  expect_identical(commit_comment$user, user$login)

})

test_that("create_comment throws as error if invalid arguments are supplied", {

  expect_error(
    create_comment(
      body = "This is a comment created by create_comment()",
      repo = repo$full_name
    ),
    "An 'issue', 'pull_request', 'commit' or 'gist' must be specified"
  )

})


# TEST: update_comment ---------------------------------------------------------

suppressMessages({

  gist_comments <- view_comments(gist = gist, n_max = 10)
  gist_comment_id <- gist_comments %>%
    filter(body == "This is a comment created by create_comment()") %>%
    pull(id) %>%
    first()

  issue_comments <- view_comments(
    issue = str_c("issue to test comments ", suffix),
    repo  = repo$full_name,
    n_max = 10
  )
  issue_comment_id <- issue_comments %>%
    filter(body == "This is a comment created by create_comment()") %>%
    pull(id) %>%
    first()

  pull_comments <- view_comments(
    pull_request = str_c("pull to test comments ", suffix),
    repo         = repo$full_name,
    n_max        = 10
  )
  pull_comment_id <- pull_comments %>%
    filter(body == "This is a comment created by create_comment()") %>%
    pull(id) %>%
    first()

  commit_comments <- view_comments(
    commit = repo$default_branch,
    repo   = repo$full_name,
    n_max  = 10
  )
  commit_comment_id <- commit_comments %>%
    filter(body == "This is a comment created by create_comment()") %>%
    pull(id) %>%
    first()

})

test_that("update_comment updates a comment and returns the properties", {

  gist_comment <- update_comment(
    comment = gist_comment_id,
    body    = "This comment has been updated by update_comment()",
    gist    = gist
  )

  expect_is(gist_comment, "list")
  expect_identical(attr(gist_comment, "status"), 200L)
  expect_identical(
    map_chr(gist_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    gist_comment$body,
    "This comment has been updated by update_comment()"
  )
  expect_identical(gist_comment$user, user$login)


  issue_comment <- update_comment(
    comment = issue_comment_id,
    body    = "This comment has been updated by update_comment()",
    type    = "issue",
    repo    = repo$full_name
  )

  expect_is(issue_comment, "list")
  expect_identical(attr(issue_comment, "status"), 200L)
  expect_identical(
    map_chr(issue_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    issue_comment$body,
    "This comment has been updated by update_comment()"
  )
  expect_identical(issue_comment$user, user$login)


  pull_comment <- update_comment(
    comment = pull_comment_id,
    body    = "This comment has been updated by update_comment()",
    type    = "pull_request",
    repo    = repo$full_name
  )

  expect_is(pull_comment, "list")
  expect_identical(attr(pull_comment, "status"), 200L)
  expect_identical(
    map_chr(pull_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      commit     = "character",
      path       = "character",
      position   = "integer",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    pull_comment$body,
    "This comment has been updated by update_comment()"
  )
  expect_identical(pull_comment$commit, as.character(branch_sha))
  expect_identical(pull_comment$path, "test-comments.txt")
  expect_identical(pull_comment$position, 1L)
  expect_identical(pull_comment$user, user$login)


  commit_comment <- update_comment(
    comment = commit_comment_id,
    body    = "This comment has been updated by update_comment()",
    type    = "commit",
    repo    = repo$full_name
  )

  expect_is(commit_comment, "list")
  expect_identical(attr(commit_comment, "status"), 200L)
  expect_identical(
    map_chr(commit_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      commit     = "character",
      path       = "character",
      position   = "integer",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    commit_comment$body,
    "This comment has been updated by update_comment()"
  )
  expect_identical(commit_comment$commit, as.character(main_sha))
  expect_identical(commit_comment$path, "README.md")
  expect_identical(commit_comment$position, 1L)
  expect_identical(commit_comment$user, user$login)

})


# TEST: view_comments ----------------------------------------------------------

test_that("view_comments returns a tibble of comment properties", {

  expect_is(gist_comments, "tbl")
  expect_identical(attr(gist_comments, "status"), 200L)
  expect_identical(
    map_chr(gist_comments, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_true(
    "This is a comment created by create_comment()" %in% gist_comments$body
  )


  expect_is(issue_comments, "tbl")
  expect_identical(attr(issue_comments, "status"), 200L)
  expect_identical(
    map_chr(issue_comments, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_true(
    "This is a comment created by create_comment()" %in% issue_comments$body
  )


  expect_is(pull_comments, "tbl")
  expect_identical(attr(pull_comments, "status"), 200L)
  expect_identical(
    map_chr(pull_comments, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      commit     = "character",
      path       = "character",
      position   = "integer",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_true(
    "This is a comment created by create_comment()" %in% pull_comments$body
  )


  expect_is(commit_comments, "tbl")
  expect_identical(attr(commit_comments, "status"), 200L)
  expect_identical(
    map_chr(commit_comments, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      commit     = "character",
      path       = "character",
      position   = "integer",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_true(
    "This is a comment created by create_comment()" %in% commit_comments$body
  )

})

test_that("view_comments throws as error if invalid arguments are supplied", {

  expect_error(
    view_comments(repo = repo$full_name),
    "An 'issue', 'pull_request', 'commit' or 'gist' must be specified"
  )

})


# TEST: view_comment -----------------------------------------------------------

test_that("view_comment returns a list of the properties", {

  gist_comment <- view_comment(
    comment = gist_comment_id,
    gist    = gist
  )

  expect_is(gist_comment, "list")
  expect_identical(attr(gist_comment, "status"), 200L)
  expect_identical(
    map_chr(gist_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    gist_comment$body,
    "This comment has been updated by update_comment()"
  )
  expect_identical(gist_comment$user, user$login)


  issue_comment <- view_comment(
    comment = issue_comment_id,
    type    = "issue",
    repo    = repo$full_name
  )

  expect_is(issue_comment, "list")
  expect_identical(attr(issue_comment, "status"), 200L)
  expect_identical(
    map_chr(issue_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    issue_comment$body,
    "This comment has been updated by update_comment()"
  )
  expect_identical(issue_comment$user, user$login)


  pull_comment <- view_comment(
    comment = pull_comment_id,
    type    = "pull_request",
    repo    = repo$full_name
  )

  expect_is(pull_comment, "list")
  expect_identical(attr(pull_comment, "status"), 200L)
  expect_identical(
    map_chr(pull_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      commit     = "character",
      path       = "character",
      position   = "integer",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    pull_comment$body,
    "This comment has been updated by update_comment()"
  )
  expect_identical(pull_comment$commit, as.character(branch_sha))
  expect_identical(pull_comment$path, "test-comments.txt")
  expect_identical(pull_comment$position, 1L)
  expect_identical(pull_comment$user, user$login)


  commit_comment <- view_comment(
    comment = commit_comment_id,
    type    = "commit",
    repo    = repo$full_name
  )

  expect_is(commit_comment, "list")
  expect_identical(attr(commit_comment, "status"), 200L)
  expect_identical(
    map_chr(commit_comment, ~ class(.)[[1]]),
    c(
      id         = "integer",
      body       = "character",
      commit     = "character",
      path       = "character",
      position   = "integer",
      user       = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(
    commit_comment$body,
    "This comment has been updated by update_comment()"
  )
  expect_identical(commit_comment$commit, as.character(main_sha))
  expect_identical(commit_comment$path, "README.md")
  expect_identical(commit_comment$position, 1L)
  expect_identical(commit_comment$user, user$login)

})


# TEST: browse_comment ---------------------------------------------------------

test_that("browse_comment opens the comment's page in the browser", {

  skip_if(!interactive(), "browse_comment must be tested manually")

  issue_comment <- browse_comment(
    comment = issue_comment_id,
    type    = "issue",
    repo    = repo$full_name
  )

  expect_is(issue_comment, "character")
  expect_identical(attr(issue_comment, "status"), 200L)

  base_url <- getOption("github.oauth") %>%
    str_remove("login/oauth") %>%
    str_c(user$login)

  expect_identical(
    dirname(issue_comment),
    str_c(base_url, "/test-comments-", suffix, "/issues")
  )

  pull_comment <- browse_comment(
    comment = pull_comment_id,
    type    = "pull_request",
    repo    = repo$full_name
  )

  expect_is(pull_comment, "character")
  expect_identical(attr(pull_comment, "status"), 200L)
  expect_identical(
    dirname(pull_comment),
    str_c(base_url, "/test-comments-", suffix, "/pull")
  )

  commit_comment <- browse_comment(
    comment = commit_comment_id,
    type    = "commit",
    repo    = repo$full_name
  )

  expect_is(commit_comment, "character")
  expect_identical(attr(commit_comment, "status"), 200L)
  expect_identical(
    dirname(commit_comment),
    str_c(base_url, "/test-comments-", suffix, "/commit")
  )

})


# TEST: delete_comment ---------------------------------------------------------

test_that("delete_comment deletes a comment", {

  gist_comment <- delete_comment(
    comment = gist_comment_id,
    gist    = gist
  )

  expect_is(gist_comment, "logical")
  expect_identical(attr(gist_comment, "status"), 204L)
  expect_identical(as.logical(gist_comment), TRUE)


  issue_comment <- delete_comment(
    comment = issue_comment_id,
    type    = "issue",
    repo    = repo$full_name
  )

  expect_is(issue_comment, "logical")
  expect_identical(attr(issue_comment, "status"), 204L)
  expect_identical(as.logical(issue_comment), TRUE)


  pull_comment <- delete_comment(
    comment = pull_comment_id,
    type    = "pull_request",
    repo    = repo$full_name
  )

  expect_is(pull_comment, "logical")
  expect_identical(attr(pull_comment, "status"), 204L)
  expect_identical(as.logical(pull_comment), TRUE)


  commit_comment <- delete_comment(
    comment = commit_comment_id,
    type    = "commit",
    repo    = repo$full_name
  )

  expect_is(commit_comment, "logical")
  expect_identical(attr(commit_comment, "status"), 204L)
  expect_identical(as.logical(commit_comment), TRUE)

})
