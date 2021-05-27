context("files")


# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

setup(suppressMessages({

  create_repository(
    name        = str_c("test-files-", suffix),
    description = "This is a repository to test files",
    auto_init   = TRUE
  )

  Sys.sleep(1)

}))

teardown(suppressMessages({

  delete_repository(str_c("ChadGoymer/test-files-", suffix))

}))


# TEST: upload_blob ------------------------------------------------------------

test_that("upload_blob uploads a file to github", {

  temp_path <- file.path(tempdir(), "test-upload-blob")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  map_chr(
    1:10,
    ~ sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")
  ) %>%
    writeLines(file.path(temp_path, "file-blob.txt"))

  blob <- upload_blob(
    path = file.path(temp_path, "file-blob.txt"),
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(blob, "list")
  expect_identical(attr(blob, "status"), 201L)
  expect_identical(
    map_chr(blob, ~ class(.)[[1]]),
    c(
      sha = "character",
      url = "character"
    )
  )

  expect_true(is_sha(blob$sha))

})


# TEST: upload_tree ------------------------------------------------------------

test_that("upload_tree uploads a directory structure to github", {

  temp_path <- file.path(tempdir(), "test-upload-tree")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  flat_path <- file.path(temp_path, "flat")
  dir.create(flat_path)

  walk(str_c("file", 1:2, ".txt"), function(f) {
    map_chr(
      1:10,
      ~ sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")
    ) %>%
      writeLines(file.path(flat_path, f))
  })


  flat_tree <- upload_tree(
    path = flat_path,
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(flat_tree, "list")
  expect_identical(
    map_chr(flat_tree, ~ class(.)[[1]]),
    c(
      commit_sha = "NULL",
      tree_sha   = "character"
    )
  )

  expect_null(flat_tree$commit_sha)
  expect_true(is_sha(flat_tree$tree_sha))


  recursive_path <- file.path(temp_path, "recursive")
  recursive_file_paths <- file.path(recursive_path, c(
    "dir-1/file-1.txt",
    "dir-1/dir-1-1/file-1.txt",
    "dir-1/dir-1-1/dir-1-1-1/file-2.txt",
    "dir-1/dir-1-2/file-3.txt",
    "dir-2/file-4.txt"
  ))
  file.path(recursive_path, c(
    "dir-1/dir-1-1/dir-1-1-1",
    "dir-1/dir-1-2", "dir-2"
  )) %>%
    walk(dir.create, recursive = TRUE)

  walk(recursive_file_paths, function(f) {
    map_chr(
      1:10,
      ~ sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")
    ) %>%
      writeLines(f)
  })


  recursive_tree <- upload_tree(
    path = recursive_path,
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(recursive_tree, "list")
  expect_identical(
    map_chr(recursive_tree, ~ class(.)[[1]]),
    c(
      commit_sha = "NULL",
      tree_sha   = "character"
    )
  )

  expect_null(recursive_tree$commit_sha)
  expect_true(is_sha(recursive_tree$tree_sha))


  base_tree <- upload_tree(
    path = flat_path,
    repo = str_c("ChadGoymer/test-files-", suffix),
    base_commit = "main"
  )

  expect_is(base_tree, "list")
  expect_identical(
    map_chr(base_tree, ~ class(.)[[1]]),
    c(
      commit_sha = "character",
      tree_sha   = "character"
    )
  )

  expect_true(is_sha(base_tree$commit_sha))
  expect_true(is_sha(base_tree$tree_sha))


  placeholder_path <- file.path(temp_path, "placeholders")
  dir.create(placeholder_path)

  walk(str_c("file", 1:2, ".txt"), function(f) {
    map_chr(
      1:10,
      ~ sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")
    ) %>%
      writeLines(file.path(placeholder_path, f))
  })

  blob_shas <- list.files(placeholder_path, full.names = TRUE) %>%
    map_chr(
      ~ upload_blob(
        path = .,
        repo = str_c("ChadGoymer/test-files-", suffix)
      ) %>%
        pluck("sha")
    )

  walk2(blob_shas, list.files(placeholder_path, full.names = TRUE), writeLines)

  placeholder_tree <- upload_tree(
    path        = placeholder_path,
    repo        = str_c("ChadGoymer/test-files-", suffix),
    placeholder = TRUE
  )

  expect_is(placeholder_tree, "list")
  expect_identical(
    map_chr(placeholder_tree, ~ class(.)[[1]]),
    c(
      commit_sha = "NULL",
      tree_sha   = "character"
    )
  )

  expect_null(placeholder_tree$commit_sha)
  expect_true(is_sha(placeholder_tree$tree_sha))


  ignore_tree <- upload_tree(
    path   = flat_path,
    repo   = str_c("ChadGoymer/test-files-", suffix),
    ignore = "file1.txt"
  )

  expect_is(ignore_tree, "list")
  expect_identical(
    map_chr(ignore_tree, ~ class(.)[[1]]),
    c(
      commit_sha = "NULL",
      tree_sha   = "character"
    )
  )

  expect_null(ignore_tree$commit_sha)
  expect_true(is_sha(ignore_tree$tree_sha))


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
      ~ sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")
    ) %>%
      writeLines(f)
  })


  empty_tree <- upload_tree(
    path = empty_path,
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(empty_tree, "list")
  expect_identical(
    map_chr(empty_tree, ~ class(.)[[1]]),
    c(
      commit_sha = "NULL",
      tree_sha   = "character"
    )
  )

  expect_null(empty_tree$commit_sha)
  expect_true(is_sha(empty_tree$tree_sha))

})


# TEST: upload_files -----------------------------------------------------------

test_that("upload_files uploads files and creates a new commit", {

  temp_path <- file.path(tempdir(), "test-upload-files")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  flat_path <- file.path(temp_path, "flat")
  dir.create(flat_path)

  walk(str_c("file", 1:2, ".txt"), function(f) {
    map_chr(
      1:10,
      ~ sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")
    ) %>%
      writeLines(file.path(flat_path, f))
  })


  flat_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = "main",
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", suffix)
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

  expect_identical(flat_commit$message, "Commit to test upload_files()")
  expect_identical(flat_commit$author_name, "Chad Goymer")
  expect_identical(flat_commit$author_email, "chad.goymer@gmail.com")
  expect_identical(flat_commit$committer_name, "Chad Goymer")
  expect_identical(flat_commit$committer_email, "chad.goymer@gmail.com")

  recursive_path <- file.path(temp_path, "recursive")
  recursive_file_paths <- file.path(recursive_path, c(
    "dir-1/file-1.txt",
    "dir-1/dir-1-1/file-1.txt",
    "dir-1/dir-1-1/dir-1-1-1/file-2.txt",
    "dir-1/dir-1-2/file-3.txt",
    "dir-2/file-4.txt"
  ))
  file.path(recursive_path, c(
    "dir-1/dir-1-1/dir-1-1-1",
    "dir-1/dir-1-2", "dir-2"
  )) %>%
    walk(dir.create, recursive = TRUE)

  walk(recursive_file_paths, function(f) {
    map_chr(
      1:10,
      ~ sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")
    ) %>%
      writeLines(f)
  })


  recursive_commit <- upload_files(
    from_path = recursive_file_paths[c(1, 3, 5)],
    to_path   = c(
      "dir-1/file-1.txt",
      "dir-1/dir-1-1/file-2.txt",
      "dir-2/file-3.txt"
    ),
    branch    = "main",
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", suffix)
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

  expect_identical(recursive_commit$message, "Commit to test upload_files()")


  author_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = "main",
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", suffix),
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

  expect_identical(author_commit$message, "Commit to test upload_files()")
  expect_identical(author_commit$author_name, "Bob")
  expect_identical(author_commit$author_email, "bob@acme.com")
  expect_identical(author_commit$committer_name, "Jane")
  expect_identical(author_commit$committer_email, "jane@acme.com")


  orphan_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = str_c("test-files-1-", suffix),
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", suffix)
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

  expect_identical(orphan_commit$message, "Commit to test upload_files()")
  expect_identical(orphan_commit$parents, character())


  valid_parent_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = "main",
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", suffix),
    parent    = author_commit$sha
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

  expect_identical(valid_parent_commit$message, "Commit to test upload_files()")
  expect_identical(valid_parent_commit$parents, author_commit$sha)


  expect_error(
    upload_files(
      from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
      to_path   = c("file-1.txt", "file-2.txt"),
      branch    = "main",
      message   = "Commit to test upload_files()",
      repo      = str_c("ChadGoymer/test-files-", suffix),
      parent    = flat_commit$sha
    ),
    "Update is not a fast forward"
  )


  force_parent_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = "main",
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", suffix),
    parent    = recursive_commit$sha,
    force     = TRUE
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

  expect_identical(force_parent_commit$message, "Commit to test upload_files()")
  expect_identical(force_parent_commit$parents, recursive_commit$sha)


  new_branch_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = str_c("test-files-2-", suffix),
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", suffix),
    parent    = flat_commit$sha
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

  expect_identical(new_branch_commit$message, "Commit to test upload_files()")
  expect_identical(new_branch_commit$parents, flat_commit$sha)

})


# TEST: download_file ----------------------------------------------------------

test_that("download_file downloads a file and returns its path", {

  temp_path <- file.path(tempdir(), "test-download-file")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  file_path <- download_file(
    from_path = "README.md",
    to_path   = file.path(temp_path, "README.md"),
    ref       = "main",
    repo      = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(file_path, "character")
  expect_identical(attr(file_path, "status"), 200L)
  expect_identical(
    as.character(file_path),
    normalizePath(file.path(temp_path, "README.md"), winslash = "/")
  )
  expect_true(file.exists(file_path))

})


# TEST: create_file ------------------------------------------------------------

test_that("create_file creates a new commit with the file added", {

  main_commit <- create_file(
    content = "# This is a new file\\n\\nCreated by `create_file()`",
    path    = "new-file-1.md",
    branch  = "main",
    message = "Created a new file with create_file()",
    repo    = str_c("ChadGoymer/test-files-", suffix)
  )

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

  expect_identical(main_commit$message, "Created a new file with create_file()")
  expect_identical(main_commit$author_name, "Chad Goymer")
  expect_identical(main_commit$author_email, "chad.goymer@gmail.com")
  expect_identical(main_commit$committer_name, "Chad Goymer")
  expect_identical(main_commit$committer_email, "chad.goymer@gmail.com")


  subfolder_commit <- create_file(
    content = "# This is a new file\\n\\nCreated by `create_file()`",
    path    = "subfolder/new-file-1.md",
    branch  = "main",
    message = "Created a new file in a subfolder with create_file()",
    repo    = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(subfolder_commit, "list")
  expect_identical(attr(subfolder_commit, "status"), 200L)
  expect_identical(
    map_chr(subfolder_commit, ~ class(.)[[1]]),
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
    subfolder_commit$message,
    "Created a new file in a subfolder with create_file()"
  )
  expect_identical(subfolder_commit$author_name, "Chad Goymer")
  expect_identical(subfolder_commit$author_email, "chad.goymer@gmail.com")
  expect_identical(subfolder_commit$committer_name, "Chad Goymer")
  expect_identical(subfolder_commit$committer_email, "chad.goymer@gmail.com")


  branch_commit <- create_file(
    content = "# This is a new file\\n\\nCreated by `create_file()`",
    path    = "new-file-2.md",
    branch  = str_c("create-file-", suffix),
    message = "Created a new file with create_file()",
    repo    = str_c("ChadGoymer/test-files-", suffix),
    parent  = "main"
  )

  expect_is(branch_commit, "list")
  expect_identical(attr(branch_commit, "status"), 200L)
  expect_identical(
    map_chr(branch_commit, ~ class(.)[[1]]),
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
    branch_commit$message,
    "Created a new file with create_file()"
  )
  expect_identical(branch_commit$author_name, "Chad Goymer")
  expect_identical(branch_commit$author_email, "chad.goymer@gmail.com")
  expect_identical(branch_commit$committer_name, "Chad Goymer")
  expect_identical(branch_commit$committer_email, "chad.goymer@gmail.com")


  author_commit <- create_file(
    content   = "# This is a new file\\n\\nCreated by `create_file()`",
    path      = "new-file-3.md",
    branch    = "main",
    message   = "Created a new file with create_file()",
    repo      = str_c("ChadGoymer/test-files-", suffix),
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

  expect_identical(
    author_commit$message,
    "Created a new file with create_file()"
  )
  expect_identical(author_commit$author_name, "Bob")
  expect_identical(author_commit$author_email, "bob@acme.com")
  expect_identical(author_commit$committer_name, "Jane")
  expect_identical(author_commit$committer_email, "jane@acme.com")

})


test_that("Trying to create a file that already exists throws an error", {

  expect_error(
    create_file(
      content = "# This is a new file\\n\\nCreated by `create_file()`",
      path    = "new-file-1.md",
      branch  = "main",
      message = "Created a new file with create_file()",
      repo    = str_c("ChadGoymer/test-files-", suffix)
    ),
    str_c(
      "A file with path 'new-file-1.md' already exists. ",
      "To update it use update_file()"
    )
  )

})


# TEST: update_file ------------------------------------------------------------

test_that("update_file creates a new commit with the file updated", {

  main_commit <- update_file(
    content = "# This is an updated file\\n\\nUpdated by `update_file()`",
    path    = "new-file-1.md",
    branch  = "main",
    message = "Updated a file with update_file()",
    repo    = str_c("ChadGoymer/test-files-", suffix)
  )

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

  expect_identical(main_commit$message, "Updated a file with update_file()")
  expect_identical(main_commit$author_name, "Chad Goymer")
  expect_identical(main_commit$author_email, "chad.goymer@gmail.com")
  expect_identical(main_commit$committer_name, "Chad Goymer")
  expect_identical(main_commit$committer_email, "chad.goymer@gmail.com")


  branch_commit <- update_file(
    content = "# This is an updated file\\n\\nUpdated by `update_file()`",
    path    = "new-file-2.md",
    branch  = str_c("update-file-", suffix),
    message = "Updated a file with update_file()",
    repo    = str_c("ChadGoymer/test-files-", suffix),
    parent  = str_c("create-file-", suffix)
  )

  expect_is(branch_commit, "list")
  expect_identical(attr(branch_commit, "status"), 200L)
  expect_identical(
    map_chr(branch_commit, ~ class(.)[[1]]),
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

  expect_identical(branch_commit$message, "Updated a file with update_file()")
  expect_identical(branch_commit$author_name, "Chad Goymer")
  expect_identical(branch_commit$author_email, "chad.goymer@gmail.com")
  expect_identical(branch_commit$committer_name, "Chad Goymer")
  expect_identical(branch_commit$committer_email, "chad.goymer@gmail.com")


  author_commit <- update_file(
    content = "# This is an updated file\\n\\nUpdated by `update_file()`",
    path      = "new-file-3.md",
    branch    = "main",
    message   = "Updated a file with update_file()",
    repo      = str_c("ChadGoymer/test-files-", suffix),
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

  expect_identical(author_commit$message, "Updated a file with update_file()")
  expect_identical(author_commit$author_name, "Bob")
  expect_identical(author_commit$author_email, "bob@acme.com")
  expect_identical(author_commit$committer_name, "Jane")
  expect_identical(author_commit$committer_email, "jane@acme.com")

})


test_that("Trying to update a file that does not exist throws an error", {

  expect_error(
    update_file(
      content = "# This is an updated file\\n\\nUpdated by `update_file()`",
      path    = "new-file-99.md",
      branch  = "main",
      message = "Updated a file with update_file()",
      repo    = str_c("ChadGoymer/test-files-", suffix)
    ),
    str_c(
      "A file with path 'new-file-99.md' does not exist. ",
      "To create it use create_file()"
    )
  )

})


# TEST: view_files -------------------------------------------------------------

test_that("view_files returns a tibble of file properties", {

  main_files <- view_files("main", str_c("ChadGoymer/test-files-", suffix))

  expect_is(main_files, "tbl")
  expect_identical(attr(main_files, "status"), 200L)
  expect_identical(
    map_chr(main_files, ~ class(.)[[1]]),
    c(
      path     = "character",
      sha      = "character",
      size     = "numeric",
      html_url = "character"
    )
  )

  expect_true("README.md" %in% main_files$path)

  non_recursive_files <- view_files(
    ref       = "main",
    repo      = str_c("ChadGoymer/test-files-", suffix),
    recursive = FALSE
  )

  expect_is(non_recursive_files, "tbl")
  expect_identical(attr(non_recursive_files, "status"), 200L)
  expect_identical(
    map_chr(non_recursive_files, ~ class(.)[[1]]),
    c(
      path     = "character",
      sha      = "character",
      size     = "numeric",
      html_url = "character"
    )
  )

  expect_true("README.md" %in% non_recursive_files$path)
  expect_true(nrow(non_recursive_files) < nrow(main_files))

})


# TEST: view_file --------------------------------------------------------------

test_that("view_file returns a list of file properties", {

  readme_file <- view_file(
    path = "README.md",
    ref  = "main",
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(readme_file, "list")
  expect_identical(attr(readme_file, "status"), 200L)
  expect_identical(
    map_chr(readme_file, ~ class(.)[[1]]),
    c(
      path     = "character",
      sha      = "character",
      size     = "numeric",
      html_url = "character"
    )
  )

  expect_identical(readme_file$path, "README.md")

})


# TEST: browse_files -----------------------------------------------------------

test_that("browse_files opens the file's history page in the browser", {

  skip_if(!interactive(), "browse_files must be tested manually")

  files_url <- browse_files("main", str_c("ChadGoymer/test-files-", suffix))

  expect_is(files_url, "character")
  expect_identical(attr(files_url, "status"), 200L)
  expect_identical(
    dirname(files_url),
    str_c("https://github.com/ChadGoymer/test-files-", suffix, "/tree")
  )

})


# TEST: browse_file ------------------------------------------------------------

test_that("browse_file opens the file's history page in the browser", {

  skip_if(!interactive(), "browse_file must be tested manually")

  file_url <- browse_file(
    path = "README.md",
    ref  = "main",
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(file_url, "character")
  expect_identical(attr(file_url, "status"), 200L)
  expect_identical(
    as.character(file_url),
    str_c(
      "https://github.com/ChadGoymer/test-files-",
      suffix, "/blob/main/README.md"
    )
  )

})


# TEST: delete_file ------------------------------------------------------------

test_that("delete_file creates a new commit with the file deleted", {

  main_commit <- delete_file(
    path    = "new-file-1.md",
    branch  = "main",
    message = "Deleted a file with delete_file()",
    repo    = str_c("ChadGoymer/test-files-", suffix)
  )

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

  expect_identical(main_commit$message, "Deleted a file with delete_file()")
  expect_identical(main_commit$author_name, "Chad Goymer")
  expect_identical(main_commit$author_email, "chad.goymer@gmail.com")
  expect_identical(main_commit$committer_name, "Chad Goymer")
  expect_identical(main_commit$committer_email, "chad.goymer@gmail.com")


  branch_commit <- delete_file(
    path    = "new-file-2.md",
    branch  = str_c("delete-file-", suffix),
    message = "Deleted a file with delete_file()",
    repo    = str_c("ChadGoymer/test-files-", suffix),
    parent  = str_c("update-file-", suffix)
  )

  expect_is(branch_commit, "list")
  expect_identical(attr(branch_commit, "status"), 200L)
  expect_identical(
    map_chr(branch_commit, ~ class(.)[[1]]),
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

  expect_identical(branch_commit$message, "Deleted a file with delete_file()")
  expect_identical(branch_commit$author_name, "Chad Goymer")
  expect_identical(branch_commit$author_email, "chad.goymer@gmail.com")
  expect_identical(branch_commit$committer_name, "Chad Goymer")
  expect_identical(branch_commit$committer_email, "chad.goymer@gmail.com")


  author_commit <- delete_file(
    path      = "new-file-3.md",
    branch    = "main",
    message   = "Deleted a file with delete_file()",
    repo      = str_c("ChadGoymer/test-files-", suffix),
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

  expect_identical(author_commit$message, "Deleted a file with delete_file()")
  expect_identical(author_commit$author_name, "Bob")
  expect_identical(author_commit$author_email, "bob@acme.com")
  expect_identical(author_commit$committer_name, "Jane")
  expect_identical(author_commit$committer_email, "jane@acme.com")

})


# TEST: write_github_file & read_github_file -----------------------------------

test_that("write_github_file creates a commit and read_github_file reads it", {

  file_commit <- write_github_file(
    content = "# This is a new file\\n\\n Created by `write_github_file()`",
    path    = "new-file.md",
    branch  = "main",
    message = "Created a new file with write_github_file()",
    repo    = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(file_commit, "list")
  expect_identical(attr(file_commit, "status"), 200L)
  expect_identical(
    map_chr(file_commit, ~ class(.)[[1]]),
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
    file_commit$message,
    "Created a new file with write_github_file()"
  )

  file_contents <- read_github_file(
    path = "new-file.md",
    ref  = "main",
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(file_contents, "character")
  expect_identical(attr(file_contents, "status"), 200L)
  expect_identical(
    as.character(file_contents),
    "# This is a new file\\n\\n Created by `write_github_file()`"
  )

})


# TEST: write_github_lines & read_github_lines ---------------------------------

test_that("write_github_lines creates commit and read_github_lines reads it", {

  lines_commit <- write_github_lines(
    content = c(
      "# This is a new file", "", "Created by `write_github_lines()`"
    ),
    path    = "new-lines.md",
    branch  = "main",
    message = "Created a new file with write_github_lines()",
    repo    = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(lines_commit, "list")
  expect_identical(attr(lines_commit, "status"), 200L)
  expect_identical(
    map_chr(lines_commit, ~ class(.)[[1]]),
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
    lines_commit$message,
    "Created a new file with write_github_lines()"
  )

  lines_contents <- read_github_lines(
    path = "new-lines.md",
    ref  = "main",
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(lines_contents, "character")
  expect_identical(attr(lines_contents, "status"), 200L)
  expect_identical(
    as.character(lines_contents),
    c("# This is a new file", "", "Created by `write_github_lines()`")
  )

})


# TEST: write_github_csv & read_github_csv -------------------------------------

test_that("write_github_csv creates a commit and read_github_csv reads it", {

  csv_commit <- write_github_csv(
    content = tibble(letters = LETTERS, numbers = 1:26),
    path    = "new-csv.csv",
    branch  = "main",
    message = "Created a new file with write_github_csv()",
    repo    = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(csv_commit, "list")
  expect_identical(attr(csv_commit, "status"), 200L)
  expect_identical(
    map_chr(csv_commit, ~ class(.)[[1]]),
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
    csv_commit$message,
    "Created a new file with write_github_csv()"
  )

  csv_contents <- read_github_csv(
    path = "new-csv.csv",
    ref  = "main",
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(csv_contents, "tbl")
  expect_identical(attr(csv_contents, "status"), 200L)
  expect_equivalent(
    as_tibble(csv_contents),
    tibble(letters = LETTERS, numbers = 1:26 * 1.0)
  )

})


# TEST: github_source ----------------------------------------------------------

test_that("github_source sources a file in GitHub", {

  write_github_file(
    content = "test_source <- function() return(\"Testing github_source\")",
    path    = "test-source.R",
    branch  = "main",
    message = "Created a new R script to test github_source()",
    repo    = str_c("ChadGoymer/test-files-", suffix)
  )

  result <- github_source(
    path = "test-source.R",
    ref  = "main",
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_true(exists("test_source"))
  expect_is(test_source, "function")
  expect_identical(test_source(), "Testing github_source")

})


# TEST: compare_files ----------------------------------------------------------

test_that("compare_files returns the file changes made between to commits", {

  main_commits <- view_commits(
    ref   = "main",
    repo  = str_c("ChadGoymer/test-files-", suffix),
    n_max = 10
  )

  file_changes <- compare_files(
    base = main_commits$sha[[3]],
    head = "main",
    repo = str_c("ChadGoymer/test-files-", suffix)
  )

  expect_is(file_changes, "tbl")
  expect_identical(attr(file_changes, "status"), 200L)
  expect_identical(
    map_chr(file_changes, ~ class(.)[[1]]),
    c(
      path      = "character",
      sha       = "character",
      status    = "character",
      additions = "integer",
      deletions = "integer",
      changes   = "integer",
      patch     = "character",
      html_url  = "character"
    )
  )

})
