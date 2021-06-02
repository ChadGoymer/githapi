# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  user <- view_user()

  repo <- create_repository(
    name        = str_c("test-commits-", suffix),
    description = "This is a repository to test commits",
    auto_init   = TRUE
  )

  Sys.sleep(2)

})

teardown(suppressMessages({

  try(
    delete_repository(repo$full_name),
    silent = TRUE
  )

}))


# TEST: upload_commit ----------------------------------------------------------

test_that("upload_commit uploads files in a directory and creates a commit", {

  temp_path <- file.path(tempdir(), "test-upload-commit")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  flat_path <- file.path(temp_path, "flat")
  dir.create(flat_path)

  walk(str_c("file", 1:2, ".txt"), function(f) {
    map_chr(
      1:10,
      ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")
    ) %>%
      writeLines(file.path(flat_path, f))
  })


  flat_commit <- upload_commit(
    path    = flat_path,
    branch  = repo$default_branch,
    message = "Commit to test upload_commit()",
    repo    = repo$full_name
  )

  expect_is(flat_commit, "list")
  expect_identical(attr(flat_commit, "status"), 200L)
  expect_identical(
    map_chr(flat_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(flat_commit$message, "Commit to test upload_commit()")
  expect_identical(flat_commit$author_name, user$name)
  expect_identical(flat_commit$author_email, user$email)
  expect_identical(flat_commit$committer_name, user$name)
  expect_identical(flat_commit$committer_email, user$email)


  recursive_path <- file.path(temp_path, "recursive")
  recursive_file_paths <- file.path(recursive_path, c(
    "dir-1/file-1.txt",
    "dir-1/dir-1-1/file-1.txt",
    "dir-1/dir-1-1/dir-1-1-1/file-2.txt",
    "dir-1/dir-1-2/file-3.txt",
    "dir-2/file-4.txt"
  ))
  file.path(recursive_path, c(
    "dir-1/dir-1-1/dir-1-1-1", "dir-1/dir-1-2", "dir-2"
  )) %>%
    walk(dir.create, recursive = TRUE)

  walk(recursive_file_paths, function(f) {
    map_chr(
      1:10,
      ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")
    ) %>%
      writeLines(f)
  })

  recursive_commit <- upload_commit(
    path    = recursive_path,
    branch  = repo$default_branch,
    message = "Commit to test upload_commit()",
    repo    = repo$full_name
  )

  expect_is(recursive_commit, "list")
  expect_identical(attr(recursive_commit, "status"), 200L)
  expect_identical(
    map_chr(recursive_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(recursive_commit$message, "Commit to test upload_commit()")


  empty_path <- file.path(temp_path, "empty")
  empty_file_paths <- file.path(empty_path, c(
    "dir-1/file-1.txt"
  ))
  file.path(empty_path, c(
    "dir-1/dir-1-1"
  )) %>%
    walk(dir.create, recursive = TRUE)

  walk(empty_file_paths, function(f) {
    map_chr(
      1:10,
      ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")
    ) %>%
      writeLines(f)
  })

  empty_commit <- upload_commit(
    path    = empty_path,
    branch  = repo$default_branch,
    message = "Commit to test upload_commit()",
    repo    = repo$full_name
  )

  expect_is(empty_commit, "list")
  expect_identical(attr(empty_commit, "status"), 200L)
  expect_identical(
    map_chr(empty_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(empty_commit$message, "Commit to test upload_commit()")


  author_commit <- upload_commit(
    path      = flat_path,
    branch    = repo$default_branch,
    message   = "Commit to test upload_commit()",
    repo      = repo$full_name,
    author    = list(name = "Bob",  email = "bob@acme.com"),
    committer = list(name = "Jane", email = "jane@acme.com")
  )

  expect_is(author_commit, "list")
  expect_identical(attr(author_commit, "status"), 200L)
  expect_identical(
    map_chr(author_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(author_commit$message, "Commit to test upload_commit()")
  expect_identical(author_commit$author_name, "Bob")
  expect_identical(author_commit$author_email, "bob@acme.com")
  expect_identical(author_commit$committer_name, "Jane")
  expect_identical(author_commit$committer_email, "jane@acme.com")


  orphan_commit <- upload_commit(
    path    = flat_path,
    branch  = str_c("test-commits-1-", suffix),
    message = "Commit to test upload_commit()",
    repo    = repo$full_name
  )

  expect_is(orphan_commit, "list")
  expect_identical(attr(orphan_commit, "status"), 200L)
  expect_identical(
    map_chr(orphan_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(orphan_commit$message, "Commit to test upload_commit()")
  expect_identical(orphan_commit$parents, character())


  valid_parent_commit <- upload_commit(
    path    = flat_path,
    branch  = repo$default_branch,
    message = "Commit to test upload_commit()",
    repo    = repo$full_name,
    parents = author_commit$sha
  )

  expect_is(valid_parent_commit, "list")
  expect_identical(attr(valid_parent_commit, "status"), 200L)
  expect_identical(
    map_chr(valid_parent_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(
    valid_parent_commit$message,
    "Commit to test upload_commit()"
  )
  expect_identical(valid_parent_commit$parents, author_commit$sha)


  expect_error(
    upload_commit(
      path    = flat_path,
      branch  = repo$default_branch,
      message = "Commit to test upload_commit()",
      repo    = repo$full_name,
      parents = flat_commit$sha
    ),
    "Update is not a fast forward"
  )


  force_parent_commit <- upload_commit(
    path    = flat_path,
    branch  = repo$default_branch,
    message = "Commit to test upload_commit()",
    repo    = repo$full_name,
    parents = flat_commit$sha,
    force   = TRUE
  )

  expect_is(force_parent_commit, "list")
  expect_identical(attr(force_parent_commit, "status"), 200L)
  expect_identical(
    map_chr(force_parent_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(
    force_parent_commit$message,
    "Commit to test upload_commit()"
  )
  expect_identical(force_parent_commit$parents, flat_commit$sha)


  new_branch_commit <- upload_commit(
    path    = flat_path,
    branch  = str_c("test-commits-2-", suffix),
    message = "Commit to test upload_commit()",
    repo    = repo$full_name,
    parents = flat_commit$sha
  )

  expect_is(new_branch_commit, "list")
  expect_identical(attr(new_branch_commit, "status"), 200L)
  expect_identical(
    map_chr(new_branch_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(new_branch_commit$message, "Commit to test upload_commit()")
  expect_identical(new_branch_commit$parents, flat_commit$sha)


  merge_main_commit <- upload_commit(
    path    = flat_path,
    branch  = repo$default_branch,
    message = "Commit to test upload_commit()",
    repo    = repo$full_name,
    parents = c(repo$default_branch, new_branch_commit$sha)
  )

  expect_is(merge_main_commit, "list")
  expect_identical(attr(merge_main_commit, "status"), 200L)
  expect_identical(
    map_chr(merge_main_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(
    merge_main_commit$message,
    "Commit to test upload_commit()"
  )
  expect_identical(
    merge_main_commit$parents,
    c(force_parent_commit$sha, new_branch_commit$sha)
  )


  merge_branch_commit <- upload_commit(
    path    = flat_path,
    branch  = str_c("test-commits-3-", suffix),
    message = "Commit to test upload_commit()",
    repo    = repo$full_name,
    parents = c(repo$default_branch, new_branch_commit$sha)
  )

  expect_is(merge_branch_commit, "list")
  expect_identical(attr(merge_branch_commit, "status"), 200L)
  expect_identical(
    map_chr(merge_branch_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(
    merge_branch_commit$message,
    "Commit to test upload_commit()"
  )
  expect_identical(
    merge_branch_commit$parents,
    c(merge_main_commit$sha, new_branch_commit$sha)
  )

})


# TEST: download_commit --------------------------------------------------------

test_that("download_commit downloads a commit to the specified path", {

  temp_path <- file.path(tempdir(), "test-download-commit") %>%
    normalizePath(winslash = "/", mustWork = FALSE)
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  on.exit(unlink(temp_path))

  path <- download_commit(
    ref  = repo$default_branch,
    repo = repo$full_name,
    path = temp_path
  )

  expect_is(path, "character")
  expect_identical(attr(path, "status"), 200L)

  files <- list.files(temp_path)

  expect_true(length(files) > 0)

})


# TEST: view_commits -----------------------------------------------------------

test_that("view_commits returns a tibble of commit properties", {

  main_commits <- view_commits(
    ref   = repo$default_branch,
    repo  = repo$full_name,
    n_max = 10
  )

  expect_is(main_commits, "tbl")
  expect_identical(attr(main_commits, "status"), 200L)
  expect_identical(
    map_chr(main_commits, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "list",
      html_url        = "character"
    )
  )

  expect_identical(last(main_commits$message), "Initial commit")

  readme_commits <- view_commits(
    ref   = repo$default_branch,
    repo  = repo$full_name,
    path  = "README.md",
    n_max = 10
  )

  expect_is(readme_commits, "tbl")
  expect_identical(attr(readme_commits, "status"), 200L)
  expect_identical(
    map_chr(readme_commits, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "list",
      html_url        = "character"
    )
  )

  expect_identical(last(readme_commits$message), "Initial commit")

  author_commits <- view_commits(
    ref    = repo$default_branch,
    repo   = repo$full_name,
    author = user$login,
    n_max  = 10
  )

  expect_is(author_commits, "tbl")
  expect_identical(attr(author_commits, "status"), 200L)
  expect_identical(
    map_chr(author_commits, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "list",
      html_url        = "character"
    )
  )

  expect_identical(last(author_commits$message), "Initial commit")

  time_commits <- view_commits(
    ref   = repo$default_branch,
    repo  = repo$full_name,
    since = format(Sys.time() - 60 * 60 * 24, "%Y-%m-%d %H:%M:%S"),
    until = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    n_max = 10
  )

  expect_is(time_commits, "tbl")
  expect_identical(attr(time_commits, "status"), 200L)
  expect_identical(
    map_chr(time_commits, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "list",
      html_url        = "character"
    )
  )

  expect_identical(last(time_commits$message), "Initial commit")

})


# TEST: view_commit ------------------------------------------------------------

test_that("view_commit returns a list of commit properties", {

  main_commit <- view_commit(repo$default_branch, repo$full_name)

  expect_is(main_commit, "list")
  expect_identical(attr(main_commit, "status"), 200L)
  expect_identical(
    map_chr(main_commit, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "character",
      html_url        = "character"
    )
  )

  expect_identical(main_commit$author_login, user$login)

})


# TEST: browse_commits ---------------------------------------------------------

base_url <- getOption("github.oauth") %>%
  str_remove("login/oauth") %>%
  str_c(user$login)

test_that("browse_commits opens the commit's history page in the browser", {

  skip_if(!interactive(), "browse_commits must be tested manually")

  commits <- browse_commits(repo$default_branch, repo$full_name)

  expect_is(commits, "character")
  expect_identical(attr(commits, "status"), 200L)

  expect_identical(
    dirname(commits),
    str_c(base_url, "/test-commits-", suffix, "/commits")
  )

})


# TEST: browse_commit ----------------------------------------------------------

test_that("browse_commit opens the commit's history page in the browser", {

  skip_if(!interactive(), "browse_commit must be tested manually")

  commit <- browse_commit(repo$default_branch, repo$full_name)

  expect_is(commit, "character")
  expect_identical(attr(commit, "status"), 200L)
  expect_identical(
    dirname(commit),
    str_c(base_url, "/test-commits-", suffix, "/commit")
  )

})


# TEST: view_sha ---------------------------------------------------------------

test_that("view_sha returns the commit SHA given the reference", {

  main_sha <- view_sha(repo$default_branch, repo = repo$full_name)

  expect_is(main_sha, "character")
  expect_identical(attr(main_sha, "status"), 200L)
  expect_true(is_sha(main_sha))

  tag <- create_tag(
    name = "test-commits",
    ref  = repo$default_branch,
    repo = repo$full_name
  )

  tag_sha <- view_sha(
    ref  = "test-commits",
    repo = repo$full_name
  )

  expect_is(tag_sha, "character")
  expect_identical(attr(tag_sha, "status"), 200L)
  expect_true(is_sha(tag_sha))
  expect_identical(as.character(main_sha), as.character(tag_sha))

})


# TEST: compare_commits --------------------------------------------------------

test_that("compare_commits returns all the commits made between to commits", {

  main_commits <- view_commits(
    ref   = repo$default_branch,
    repo  = repo$full_name,
    n_max = 10
  )

  comparison <- compare_commits(
    base = main_commits$sha[[3]],
    head = repo$default_branch,
    repo = repo$full_name
  )

  expect_is(comparison, "list")
  expect_identical(attr(comparison, "status"), 200L)
  expect_identical(
    map_chr(comparison, ~ class(.)[[1]]),
    c(
      status        = "character",
      ahead_by      = "integer",
      behind_by     = "integer",
      total_commits = "integer",
      html_url      = "character",
      commits       = "github"
    )
  )

  expect_identical(comparison$status, "ahead")
  expect_identical(comparison$ahead_by, 2L)
  expect_identical(comparison$behind_by, 0L)
  expect_identical(comparison$total_commits, 2L)

  expect_is(comparison$commits, "tbl")
  expect_identical(
    map_chr(comparison$commits, ~ class(.)[[1]]),
    c(
      sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      author_date     = "POSIXct",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      committer_date  = "POSIXct",
      tree_sha        = "character",
      parents         = "list",
      html_url        = "character"
    )
  )

  expect_identical(comparison$commits$sha, main_commits$sha[2:1])

})
