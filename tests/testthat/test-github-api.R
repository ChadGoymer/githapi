# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  repo <- create_repository(
    name        = str_c("test-github-api-", suffix),
    description = "This is a repository to test github api",
    auto_init   = TRUE
  )

  Sys.sleep(1)

  create_branch(
    name = "test-api",
    ref  = repo$default_branch,
    repo = repo$full_name
  )

})

teardown(suppressMessages({

  try(delete_repository(repo$full_name), silent = TRUE)

}))


# TEST: gh_token ---------------------------------------------------------------

test_that("gh_token returns a valid GitHub personal access token", {

  existing_msgr_level <- getOption("msgr.level")
  on.exit(options(msgr.level = existing_msgr_level), add = TRUE)
  options(msgr.level = 10)

  token1 <- sample(c(0:9, letters[1:6]), size = 40, replace = TRUE) %>%
    str_c(collapse = "")

  expect_message(
    token1_result <- gh_token(token = token1),
    "Using supplied token"
  )

  expect_identical(token1_result, token1)

  token2 <- sample(c(0:9, letters[1:6]), size = 40, replace = TRUE) %>%
    str_c(collapse = "")

  existing_token <- getOption("github.token")
  on.exit(options(github.token = existing_token), add = TRUE)
  options(github.token = token2)

  expect_message(
    token2_result <- gh_token(),
    "Using supplied token"
  )

  expect_identical(token2_result, token2)

})

test_that("gh_token returns a valid GitHub OAuth token", {

  skip_if_not(interactive(), "OAuth authentication must be run manually")

  test_cache <- tempfile("githapi-oauth-")

  existing_msgr_level    <- getOption("msgr.level")
  existing_githapi_cache <- getOption("githapi.cache")
  options(msgr.level = 10, githapi.cache = test_cache)

  on.exit({
    options(
      msgr.level = existing_msgr_level,
      githapi.cache = existing_githapi_cache
    )
    unlink(test_cache)
  })

  expect_error(
    gh_token(token = NULL, secret = "suhfdieudhisauhf", refresh = TRUE),
    "incorrect client credentials"
  )

  expect_message(
    new_token <- gh_token(token = NULL, refresh = TRUE),
    "Retrieving new token"
  )

  expect_is(new_token, "Token")

  expect_message(
    cached_token <- gh_token(token = NULL),
    "Retrieving cached token"
  )

  expect_is(cached_token, "Token")

})

test_that("gh_token throws an error if an invalid token is specified", {

  expect_error(
    gh_token(token = "Bob"),
    "'token' must be a SHA or a Token object"
  )

})


# TEST: gh_url -----------------------------------------------------------------

test_that("gh_url returns a valid URL for the GitHub API", {

  expect_identical(gh_url(), str_c(getOption("github.api"), "/"))

  expect_identical(
    gh_url("repos"),
    file.path(getOption("github.api"), "repos")
  )

  expect_identical(
    gh_url("repos", "ChadGoymer/githapi", "git/refs", "heads/main"),
    file.path(
      getOption("github.api"),
      "repos/ChadGoymer/githapi/git/refs/heads/main"
    )
  )

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/refs", "heads/main")),
    file.path(
      getOption("github.api"),
      "repos/ChadGoymer/githapi/git/refs/heads/main"
    )
  )

  expect_identical(
    gh_url(
      "repos", "ChadGoymer/githapi", "git/trees", "234752384",
      recursive = 1
    ),
    file.path(
      getOption("github.api"),
      "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1"
    )
  )

  expect_identical(
    gh_url(
      c("repos", "ChadGoymer/githapi", "git/trees", "234752384"),
      list(recursive = 1)
    ),
    file.path(
      getOption("github.api"),
      "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1"
    )
  )

  expect_identical(
    gh_url(
      "repos", "ChadGoymer/githapi", "git/trees", "234752384",
      recursive = 1, type = "bob"
    ),
    file.path(
      getOption("github.api"),
      "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1&type=bob"
    )
  )

  expect_identical(
    gh_url(
      c("repos", "ChadGoymer/githapi", "git/trees", "234752384"),
      list(recursive = 1, type = "bob")
    ),
    file.path(
      getOption("github.api"),
      "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1&type=bob"
    )
  )

  expect_identical(
    gh_url(
      c("repos", "ChadGoymer/githapi", "git/trees", "234752384"),
      list()
    ),
    file.path(
      getOption("github.api"),
      "repos/ChadGoymer/githapi/git/trees/234752384"
    )
  )

  expect_identical(
    gh_url(
      "repos", "ChadGoymer/githapi", "labels", "simple label",
      type = "some type"
    ),
    file.path(
      getOption("github.api"),
      "repos/ChadGoymer/githapi/labels/simple%20label?type=some%20type"
    )
  )

})


# TEST: gh_request -------------------------------------------------------------

test_that("gh_request can GET, POST, PATCH and DELETE a tag", {

  main_sha <- view_sha(ref = repo$default_branch, repo = repo$full_name)
  test_sha <- view_sha(ref = "test-api", repo = repo$full_name)

  test_tag <- str_c(
    "refs/tags/test-gh-request-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  )

  created_tag <- gh_request(
    url     = str_c(
      getOption("github.api"), "/repos/", repo$full_name, "/git/refs"
    ),
    type    = "POST",
    payload = list(ref = test_tag, sha = main_sha)
  )

  expect_is(created_tag, "list")
  expect_identical(created_tag$ref, test_tag)
  expect_identical(created_tag$object$sha, as.character(main_sha))

  expect_identical(
    attr(created_tag, "url"),
    str_c(
      getOption("github.api"), "/repos/", repo$full_name, "/git/refs"
    )
  )
  expect_identical(attr(created_tag, "request"), "POST")
  expect_identical(attr(created_tag, "status"), 201L)
  expect_true(length(attr(created_tag, "header")) > 1)

  viewed_tag <- gh_request(
    url  = str_c(
      getOption("github.api"), "/repos/", repo$full_name, "/git/", test_tag
    ),
    type = "GET"
  )

  expect_is(viewed_tag, "list")
  expect_identical(viewed_tag$ref, test_tag)
  expect_identical(viewed_tag$object$sha, as.character(main_sha))

  expect_identical(
    attr(viewed_tag, "url"),
    str_c(
      getOption("github.api"), "/repos/", repo$full_name, "/git/", test_tag
    )
  )
  expect_identical(attr(viewed_tag, "request"), "GET")
  expect_identical(attr(viewed_tag, "status"), 200L)
  expect_true(length(attr(viewed_tag, "header")) > 1)

  updated_tag <- gh_request(
    url     = str_c(
      getOption("github.api"), "/repos/", repo$full_name, "/git/", test_tag
    ),
    type    = "PATCH",
    payload = list(sha = test_sha)
  )

  expect_is(updated_tag, "list")
  expect_identical(updated_tag$ref, test_tag)
  expect_identical(updated_tag$object$sha, as.character(test_sha))

  expect_identical(
    attr(updated_tag, "url"),
    str_c(
      getOption("github.api"), "/repos/", repo$full_name, "/git/", test_tag
    )
  )
  expect_identical(attr(updated_tag, "request"), "PATCH")
  expect_identical(attr(updated_tag, "status"), 200L)
  expect_true(length(attr(updated_tag, "header")) > 1)

  deleted_tag <- gh_request(
    url  = str_c(
      getOption("github.api"), "/repos/", repo$full_name, "/git/", test_tag
    ),
    type = "DELETE"
  )

  expect_is(deleted_tag, "list")

  expect_identical(
    attr(deleted_tag, "url"),
    str_c(
      getOption("github.api"), "/repos/", repo$full_name, "/git/", test_tag
    )
  )
  expect_identical(attr(deleted_tag, "request"), "DELETE")
  expect_identical(attr(deleted_tag, "status"), 204L)
  expect_true(length(attr(deleted_tag, "header")) > 1)

  expect_error(
    gh_request(
      url  = str_c(
        getOption("github.api"), "/repos/", repo$full_name, "/git/", test_tag
      ),
      type = "GET"
    )
  )

})

test_that("gh_request can make a request using an OAuth token", {

  skip_if_not(interactive(), "OAuth authentication must be run manually")

  existing_msgr_level    <- getOption("msgr.level")
  existing_githapi_cache <- getOption("githapi.cache")
  options(msgr.level = 10, githapi.cache = "~/.githapi.oauth")
  on.exit({
    options(
      msgr.level = existing_msgr_level,
      githapi.cache = existing_githapi_cache
    )
  })

  main <- str_c(
    getOption("github.api"), "/repos/", repo$full_name,
    "/git/refs/heads/", repo$default_branch
  ) %>%
    gh_request("GET", token = NULL)

  expect_is(main, "list")
  expect_identical(main$ref, str_c("refs/heads/", repo$default_branch))

  expect_identical(
    attr(main, "url"),
    str_c(
      getOption("github.api"), "/repos/", repo$full_name,
      "/git/refs/heads/", repo$default_branch
    )
  )
  expect_identical(attr(main, "request"), "GET")
  expect_identical(attr(main, "status"), 200L)
  expect_true(length(attr(main, "header")) > 1)

})


# TEST: gh_page ----------------------------------------------------------------

test_that("gh_page returns a list of specified length", {

  users_20 <- gh_page(
    url   = str_c(getOption("github.api"), "/users"),
    n_max = 20
  )

  expect_is(users_20, "list")
  expect_identical(length(users_20), 20L)

  expect_identical(
    attr(users_20, "url"),
    str_c(getOption("github.api"), "/users?per_page=20")
  )
  expect_identical(attr(users_20, "request"), "GET")
  expect_identical(attr(users_20, "status"), 200L)
  expect_true(length(attr(users_20, "header")) > 1)

  users_150 <- gh_page(
    url   = str_c(getOption("github.api"), "/users"),
    n_max = 150
  )

  expect_is(users_150, "list")
  expect_identical(length(users_150), 150L)

  expect_identical(
    attr(users_150, "url"),
    str_c(
      getOption("github.api"),
      c("/users?per_page=100", "/users?per_page=50&since=135")
    )
  )
  expect_identical(attr(users_150, "request"), "GET")
  expect_identical(attr(users_150, "status"), c(200L, 200L))
  expect_true(length(attr(users_150, "header")) > 1)

})

test_that("gh_page still works when the endpoint returns a singular response", {

  main <- str_c(
    getOption("github.api"), "/repos/", repo$full_name,
    "/git/refs/heads/", repo$default_branch
  ) %>%
    gh_page()

  expect_is(main, "list")
  expect_identical(main$ref, "refs/heads/main")

  expect_identical(
    attr(main, "url"),
    str_c(
      getOption("github.api"), "/repos/", repo$full_name,
      "/git/refs/heads/", repo$default_branch, "?per_page=100"
    )
  )
  expect_identical(attr(main, "request"), "GET")
  expect_identical(attr(main, "status"), 200L)
  expect_true(length(attr(main, "header")) > 1)

})


# TEST: gh_find ----------------------------------------------------------------

test_that("gh_find locate an entity with the specified property value", {

  users_150 <- gh_page(
    url   = str_c(getOption("github.api"), "/users"),
    n_max = 150
  )

  user_25 <- gh_find(
    url      = str_c(getOption("github.api"), "/users"),
    property = "login",
    value    = users_150[[25]]$login
  )

  expect_is(user_25, "list")
  expect_identical(user_25$login, users_150[[25]]$login)

  expect_identical(
    attr(user_25, "url"),
    str_c(getOption("github.api"), "/users?per_page=100")
  )
  expect_identical(attr(user_25, "request"), "GET")
  expect_identical(attr(user_25, "status"), 200L)
  expect_true(length(attr(user_25, "header")) > 1)

  user_125 <- gh_find(
    url      = str_c(getOption("github.api"), "/users"),
    property = "login",
    value    = users_150[[125]]$login
  )

  expect_is(user_125, "list")
  expect_identical(user_125$login, users_150[[125]]$login)

  expect_identical(
    attr(user_125, "url"),
    str_c(getOption("github.api"), "/users?per_page=100&since=135")
  )
  expect_identical(attr(user_125, "request"), "GET")
  expect_identical(attr(user_125, "status"), 200L)
  expect_true(length(attr(user_125, "header")) > 1)

})

test_that("gh_find throws an error if it cannot find the property value", {

  expect_error(
    gh_find(
      url      = str_c(
        getOption("github.api"), "/repos/", repo$full_name, "/git/refs/heads/"
      ),
      property = "ref",
      value    = "refs/heads/bob"
    ),
    "Could not find an entity with 'ref' equal to 'refs/heads/bob'"
  )

})


# TEST: gh_download ------------------------------------------------------------

test_that("gh_download downloads a file to a location and returns the path", {

  temp_path <- file.path(tempdir(), "test-gh-download")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  file_path <- str_c(
    getOption("github.api"), "/repos/", repo$full_name,
    "/contents/README.md"
  ) %>%
    gh_download(
      path   = file.path(temp_path, "README.md"),
      accept = "application/vnd.github.v3.raw"
    )

  expect_is(file_path, "character")
  expect_identical(attr(file_path, "status"), 200L)
  expect_identical(
    as.character(file_path),
    normalizePath(file.path(temp_path, "README.md"), winslash = "/")
  )
  expect_true(file.exists(file_path))

  expect_error(
    str_c(
      getOption("github.api"), "/repos/", repo$full_name,
      "/contents/Bob.txt"
    ) %>%
      gh_download(path = file.path(temp_path, "Bob.txt")),
    "Not Found"
  )

})

test_that("gh_download can make a request using an OAuth token", {

  skip_if_not(interactive(), "OAuth authentication must be run manually")

  existing_msgr_level    <- getOption("msgr.level")
  existing_githapi_cache <- getOption("githapi.cache")
  options(msgr.level = 10, githapi.cache = "~/.githapi.oauth")
  on.exit({
    options(
      msgr.level    = existing_msgr_level,
      githapi.cache = existing_githapi_cache
    )
  })

  temp_path <- file.path(tempdir(), "test-gh-download")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  file_path <- str_c(
    getOption("github.api"), "/repos/", repo$full_name,
    "/contents/README.md"
  ) %>%
    gh_download(
      path  = file.path(temp_path, "README.md"),
      accept = "application/vnd.github.v3.raw",
      token  = NULL
    )

  expect_is(file_path, "character")
  expect_identical(attr(file_path, "status"), 200L)
  expect_identical(
    as.character(file_path),
    normalizePath(file.path(temp_path, "README.md"), winslash = "/")
  )
  expect_true(file.exists(file_path))

})


# TEST: print.github -----------------------------------------------------------

test_that("print.github correctly prints lists", {

  test_github_list <- structure(
    list(Name = "Bob"),
    class   = c("github", "list"),
    url     = "https://somwhere/Bob",
    request = "GET",
    status  = 200L,
    header  = list(http_blah = "stuff")
  )

  list_output <- utils::capture.output(test_github_list)

  expect_identical(
    list_output,
    c(
      "\033[34m# GET \033[4mhttps://somwhere/Bob\033[24m",
      "\033[39mList of 1",
      "  Name: chr \"Bob\""
    )
  )

})

test_that("print.github correctly prints vectors", {

  test_github_string <- structure(
    "Bob",
    class   = c("github", "character"),
    url     = "https://somwhere/Bob",
    request = "GET",
    status  = 200L,
    header  = list(http_blah = "stuff")
  )

  string_output <- utils::capture.output(test_github_string)

  expect_identical(
    string_output,
    c(
      "\033[34m# GET \033[4mhttps://somwhere/Bob\033[24m",
      "\033[39m[1] \"Bob\""
    )
  )

})

test_that("print.github correctly prints data.frames", {

  df <- tibble(Name = "Bob", role = "Developer")
  test_github_df <- structure(
    df,
    class   = c("github", class(df)),
    url     = "https://somwhere/Bob",
    request = "GET",
    status  = 200L,
    header  = list(http_blah = "stuff")
  )

  df_output <- utils::capture.output(test_github_df)

  expect_identical(
    df_output,
    c(
      "\033[34m# GET \033[4mhttps://somwhere/Bob\033[24m",
      "\033[39m# A tibble: 1 x 2",
      "  Name  role     ",
      "* <chr> <chr>    ",
      "1 Bob   Developer"
    )
  )

})

test_that("print.github correctly prints multiple URLs", {

  test_github_urls <- structure(
    "Bob",
    class   = c("github", "character"),
    url     = c(
      "https://somwhere/Bob1",
      "https://somwhere/Bob2",
      "https://somwhere/Bob3"
    ),
    request = "GET",
    status  = 200L,
    header  = list(http_blah = "stuff")
  )

  urls_output <- utils::capture.output(test_github_urls)

  expect_identical(
    urls_output,
    c(
      "\033[34m# GET \033[4mhttps://somwhere/Bob1\033[24m",
      "\033[39m[1] \"Bob\""
    )
  )

})
