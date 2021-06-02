# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  user <- view_user()

})

teardown(suppressMessages({

  created_gists <- view_gists(
    since = as.character(Sys.time() - 60 * 10),
    n_max = 10
  ) %>%
    filter(map_lgl(.data$files, ~ any(str_detect(., suffix))))

  walk(created_gists$id, delete_gist)

}))


# TEST: create_gist ------------------------------------------------------------

test_that("create_gist creates a gist and returns its properties", {

  basic_files <- list("print(\"Hello World!\")") %>%
    set_names(str_c("helloworld-", suffix, ".R"))
  basic_gist <- create_gist(files = basic_files)

  expect_is(basic_gist, "list")
  expect_identical(attr(basic_gist, "status"), 201L)
  expect_identical(
    map_chr(basic_gist, ~ class(.)[[1]]),
    c(
      id          = "character",
      description = "character",
      files       = "github",
      owner       = "character",
      public      = "logical",
      html_url    = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

  expect_identical(
    map_chr(basic_gist$files, ~ class(.)[[1]]),
    c(
      filename  = "character",
      type      = "character",
      content   = "character",
      size      = "integer",
      truncated = "logical"
    )
  )

  expect_identical(basic_gist$description, NA_character_)
  expect_identical(basic_gist$owner, user$login)
  expect_false(basic_gist$public)
  expect_identical(
    basic_gist$files$filename,
    str_c("helloworld-", suffix, ".R")
  )
  expect_identical(
    basic_gist$files$content,
    "print(\"Hello World!\")"
  )


  multiple_files <- list(
    "print(\"Hello World!\")",
    "helloworld <- function() \"Hello World!\""
  ) %>%
    set_names(str_c(c("helloworld-", "helloworld-fn-"), suffix, ".R"))

  multiple_gist <- create_gist(
    files       = multiple_files,
    description = "A gist with multiple files"
  )

  expect_is(multiple_gist, "list")
  expect_identical(attr(multiple_gist, "status"), 201L)
  expect_identical(
    map_chr(multiple_gist, ~ class(.)[[1]]),
    c(
      id          = "character",
      description = "character",
      files       = "github",
      owner       = "character",
      public      = "logical",
      html_url    = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

  expect_identical(
    map_chr(multiple_gist$files, ~ class(.)[[1]]),
    c(
      filename  = "character",
      type      = "character",
      content   = "character",
      size      = "integer",
      truncated = "logical"
    )
  )

  expect_identical(multiple_gist$description, "A gist with multiple files")
  expect_identical(multiple_gist$owner, user$login)
  expect_false(multiple_gist$public)
  expect_identical(
    sort(multiple_gist$files$filename),
    sort(str_c(c("helloworld-", "helloworld-fn-"), suffix, ".R"))
  )
  expect_identical(
    sort(multiple_gist$files$content),
    c("helloworld <- function() \"Hello World!\"", "print(\"Hello World!\")")
  )


  public_gist <- create_gist(
    files       = basic_files,
    description = "A public gist",
    public      = TRUE
  )

  expect_is(public_gist, "list")
  expect_identical(attr(public_gist, "status"), 201L)
  expect_identical(
    map_chr(public_gist, ~ class(.)[[1]]),
    c(
      id          = "character",
      description = "character",
      files       = "github",
      owner       = "character",
      public      = "logical",
      html_url    = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

  expect_identical(
    map_chr(public_gist$files, ~ class(.)[[1]]),
    c(
      filename  = "character",
      type      = "character",
      content   = "character",
      size      = "integer",
      truncated = "logical"
    )
  )

  expect_identical(public_gist$description, "A public gist")
  expect_identical(public_gist$owner, user$login)
  expect_true(public_gist$public)
  expect_identical(
    public_gist$files$filename,
    str_c("helloworld-", suffix, ".R")
  )
  expect_identical(
    public_gist$files$content,
    "print(\"Hello World!\")"
  )

})


# TEST: update_gist ------------------------------------------------------------

suppressMessages({

  created_gists <- view_gists(
    since = as.character(Sys.time() - 60 * 10),
    n_max = 10
  ) %>%
    filter(map_lgl(.data$files, ~ any(str_detect(., suffix))))

})

test_that("update_gist updates a gist and returns its properties", {

  updated_desc <- update_gist(
    gist        = created_gists$id[[1]],
    description = "An updated description"
  )

  expect_is(updated_desc, "list")
  expect_identical(attr(updated_desc, "status"), 200L)
  expect_identical(
    map_chr(updated_desc, ~ class(.)[[1]]),
    c(
      id          = "character",
      description = "character",
      files       = "github",
      owner       = "character",
      public      = "logical",
      html_url    = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

  expect_identical(
    map_chr(updated_desc$files, ~ class(.)[[1]]),
    c(
      filename  = "character",
      type      = "character",
      content   = "character",
      size      = "integer",
      truncated = "logical"
    )
  )

  expect_identical(updated_desc$description, "An updated description")
  expect_identical(updated_desc$owner, user$login)
  expect_true(updated_desc$public)
  expect_identical(
    updated_desc$files$filename,
    str_c("helloworld-", suffix, ".R")
  )
  expect_identical(
    updated_desc$files$content,
    "print(\"Hello World!\")"
  )


  updated_files <- list(c(
    "cat(\"Hello World!\")",
    filename = "hello-world.R"
  )) %>%
    set_names(str_c("helloworld-", suffix, ".R"))

  updated_files <- update_gist(
    gist  = created_gists$id[[1]],
    files = updated_files
  )

  expect_is(updated_files, "list")
  expect_identical(attr(updated_files, "status"), 200L)
  expect_identical(
    map_chr(updated_files, ~ class(.)[[1]]),
    c(
      id          = "character",
      description = "character",
      files       = "github",
      owner       = "character",
      public      = "logical",
      html_url    = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

  expect_identical(
    map_chr(updated_files$files, ~ class(.)[[1]]),
    c(
      filename  = "character",
      type      = "character",
      content   = "character",
      size      = "integer",
      truncated = "logical"
    )
  )

  expect_identical(updated_files$description, "An updated description")
  expect_identical(updated_files$owner, user$login)
  expect_true(updated_files$public)
  expect_identical(updated_files$files$filename, "hello-world.R")
  expect_identical(updated_files$files$content, "cat(\"Hello World!\")")

})


# TEST: view_gists -------------------------------------------------------------

test_that("view_gists returns a tibble of gist properties", {

  expect_is(created_gists, "tbl")
  expect_identical(attr(created_gists, "status"), 200L)
  expect_identical(
    map_chr(created_gists, ~ class(.)[[1]]),
    c(
      id          = "character",
      description = "character",
      files       = "list",
      owner       = "character",
      public      = "logical",
      html_url    = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

  expect_true("A public gist" %in% created_gists$description)
  expect_true("A gist with multiple files" %in% created_gists$description)


  user_gists <- view_gists(user$login, n_max = 10)

  expect_is(user_gists, "tbl")
  expect_identical(attr(user_gists, "status"), 200L)
  expect_identical(
    map_chr(user_gists, ~ class(.)[[1]]),
    c(
      id          = "character",
      description = "character",
      files       = "list",
      owner       = "character",
      public      = "logical",
      html_url    = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )


  public_gists <- view_gists("public", n_max = 10)

  expect_is(public_gists, "tbl")
  expect_identical(attr(public_gists, "status"), 200L)
  expect_identical(
    map_chr(public_gists, ~ class(.)[[1]]),
    c(
      id          = "character",
      description = "character",
      files       = "list",
      owner       = "character",
      public      = "logical",
      html_url    = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

})


# TEST: view_gist --------------------------------------------------------------

test_that("view_gist returns a list of gist properties", {

  gist <- view_gist(created_gists$id[[1]])

  expect_is(gist, "list")
  expect_identical(attr(gist, "status"), 200L)
  expect_identical(
    map_chr(gist, ~ class(.)[[1]]),
    c(
      id          = "character",
      description = "character",
      files       = "github",
      owner       = "character",
      public      = "logical",
      html_url    = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

  expect_identical(
    map_chr(gist$files, ~ class(.)[[1]]),
    c(
      filename  = "character",
      type      = "character",
      content   = "character",
      size      = "integer",
      truncated = "logical"
    )
  )

  expect_identical(gist$description, "An updated description")
  expect_identical(gist$owner, user$login)
  expect_true(gist$public)
  expect_identical(gist$files$filename, "hello-world.R")
  expect_identical(gist$files$content, "cat(\"Hello World!\")")

})


# TEST: browse_gist ------------------------------------------------------------

test_that("browse_gist opens the gist's page in the browser", {

  skip_if(!interactive(), "browse_gist must be tested manually")

  gist_url <- browse_gist(created_gists$id[[1]])

  enterprise_url <- getOption("github.oauth") %>%
    str_replace("login/oauth", "gist")

  expect_is(gist_url, "character")
  expect_identical(attr(gist_url, "status"), 200L)
  expect_true(
    identical(dirname(gist_url), "https://gist.github.com") ||
      identical(dirname(gist_url), enterprise_url)
  )

})


# TEST: download_gist ----------------------------------------------------------

test_that("download_gist downloads a gist and returns its path", {

  temp_path <- file.path(tempdir(), "test-download-gist")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  gist_path <- download_gist(gist = created_gists$id[[2]], path = temp_path)

  expect_is(gist_path, "character")
  expect_identical(attr(gist_path, "status"), 200L)
  expect_identical(
    as.character(gist_path),
    normalizePath(temp_path, winslash = "/")
  )
  expect_true(
    file.exists(file.path(gist_path, str_c("helloworld-", suffix, ".R")))
  )

})


# TEST: source_gist ------------------------------------------------------------

test_that("source_gist sources a file in GitHub", {

  expect_output(source_gist(created_gists$id[[2]]), "Hello World!")
  expect_true(exists("helloworld"))
  expect_is(helloworld, "function")
  expect_identical(helloworld(), "Hello World!")

})


# TEST: delete_gist ------------------------------------------------------------

test_that("delete_gist deletes a gist", {

  deleted_gist <- delete_gist(created_gists$id[[1]])

  expect_is(deleted_gist, "logical")
  expect_identical(attr(deleted_gist, "status"), 204L)
  expect_identical(as.logical(deleted_gist), TRUE)

})
