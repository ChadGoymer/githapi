# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  repo <- create_repository(
    name        = str_c("test-collaborators-", suffix),
    description = "This is a repository to test collaborators"
  )

  user_1 <- view_user()
  user_2 <- tryCatch(
    view_user("ChadGoymer2"),
    error = function(e) list(login = character())
  )

  org <- view_organizations(user = NULL, n_max = 10) %>%
    arrange(.data$login) %>%
    slice(1)

  if (nrow(org) == 1) {
    project <- create_project(
      name = str_c("Test collaborators ", suffix),
      body = "A project to test collaborator functions",
      org  = org$login
    )
  }

})

teardown(suppressMessages({

  try(delete_repository(repo$full_name), silent = TRUE)
  try(delete_project(project = project$name, org = org$login), silent = TRUE)

}))


# TEST: update_collaborator ----------------------------------------------------

test_that("update_collaborator adds a collaborator to a repo or project", {

  skip_if(
    length(user_2$login) != 1,
    "Test user does not exist"
  )

  repo_result <- update_collaborator(
    user = user_2$login,
    repo = repo$full_name
  )

  expect_is(repo_result, "logical")
  expect_identical(attr(repo_result, "status"), 201L)
  expect_identical(as.logical(repo_result), TRUE)

  updated_repo_result <- update_collaborator(
    user       = user_2$login,
    repo       = repo$full_name,
    permission = "admin"
  )

  expect_is(updated_repo_result, "logical")
  expect_identical(attr(updated_repo_result, "status"), 201L)
  expect_identical(as.logical(updated_repo_result), TRUE)

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  project_result <- update_collaborator(
    user    = user_2$login,
    project = project$name,
    org     = org$login
  )

  expect_is(project_result, "logical")
  expect_identical(attr(project_result, "status"), 204L)
  expect_identical(as.logical(project_result), TRUE)

  updated_project_result <- update_collaborator(
    user       = user_2$login,
    project    = project$name,
    org        = org$login,
    permission = "admin"
  )

  expect_is(updated_project_result, "logical")
  expect_identical(attr(updated_project_result, "status"), 204L)
  expect_identical(as.logical(updated_project_result), TRUE)

})

test_that("update_collaborator throws an error with invalid arguments", {

  expect_error(
    update_collaborator(user = "Irrelevant"),
    "A 'repo' or 'project' must be specified when updating a collaborator"
  )

})


# TEST: view_collaborators -----------------------------------------------------

test_that("view_collaborators returns a tibble summarising the collaborators", {

  repo_collaborators <- view_collaborators(
    repo  = repo$full_name,
    n_max = 10
  )

  expect_is(repo_collaborators, "tbl")
  expect_identical(attr(repo_collaborators, "status"), 200L)
  expect_identical(
    map_chr(repo_collaborators, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )

  expect_true(user_1$login %in% repo_collaborators$login)

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  project_collaborators <- view_collaborators(
    project = project$name,
    org     = org$login,
    n_max   = 10
  )

  expect_is(project_collaborators, "tbl")
  expect_identical(attr(project_collaborators, "status"), 200L)
  expect_identical(
    map_chr(project_collaborators, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )

  expect_true(user_1$login %in% project_collaborators$login)

  org_collaborators <- view_collaborators(org = org$login, n_max = 10)

  expect_is(org_collaborators, "tbl")
  expect_identical(attr(org_collaborators, "status"), 200L)
  expect_identical(
    map_chr(org_collaborators, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )

  direct_collaborators <- view_collaborators(
    org         = org$login,
    affiliation = "direct",
    n_max       = 10
  )

  expect_is(direct_collaborators, "tbl")
  expect_identical(attr(direct_collaborators, "status"), 200L)
  expect_identical(
    map_chr(direct_collaborators, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )

})

test_that("view_collaborators throws an error with invalid arguments", {

  expect_error(
    view_collaborators(),
    "A 'repo', 'project' or 'org' must be specified when viewing collaborators"
  )

})


# TEST: view_collaborator ------------------------------------------------------

test_that("view_collaborator returns a list of a collaborator's properties", {

  repo_collaborator <- view_collaborator(
    user = user_1$login,
    repo = repo$full_name
  )

  expect_is(repo_collaborator, "list")
  expect_identical(attr(repo_collaborator, "status"), 200L)
  expect_identical(
    map_chr(repo_collaborator, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character",
      permission = "character"
    )
  )

  expect_identical(repo_collaborator$login, user_1$login)
  expect_identical(repo_collaborator$permission, "admin")

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  project_collaborator <- view_collaborator(
    user    = user_1$login,
    project = project$name,
    org     = org$login
  )

  expect_is(project_collaborator, "list")
  expect_identical(attr(project_collaborator, "status"), 200L)
  expect_identical(
    map_chr(project_collaborator, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character",
      permission = "character"
    )
  )

  expect_identical(project_collaborator$login, user_1$login)
  expect_identical(project_collaborator$permission, "admin")

})

test_that("view_collaborator throws an error with invalid arguments", {

  expect_error(
    view_collaborator(user = "Irrelevant"),
    "A 'repo' or 'project' must be specified when viewing a collaborator"
  )

})


# TEST: delete_collaborator ----------------------------------------------------

test_that("delete_collaborator removes a collaborator", {

  skip_if(
    length(user_2$login) != 1,
    "Test user does not exist"
  )

  repo_collaborator <- delete_collaborator(
    user = user_2$login,
    repo = repo$full_name
  )

  expect_is(repo_collaborator, "logical")
  expect_identical(attr(repo_collaborator, "status"), 204L)
  expect_identical(as.logical(repo_collaborator), TRUE)

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  project_collaborator <- delete_collaborator(
    user    = user_2$login,
    project = project$name,
    org     = org$login
  )

  expect_is(project_collaborator, "logical")
  expect_identical(attr(project_collaborator, "status"), 204L)
  expect_identical(as.logical(project_collaborator), TRUE)

  org_collaborator <- delete_collaborator(user_2$login, org = org$login)

  expect_is(org_collaborator, "logical")
  expect_identical(attr(org_collaborator, "status"), 204L)
  expect_identical(as.logical(org_collaborator), TRUE)

})

test_that("delete_collaborator throws an error with invalid arguments", {

  expect_error(
    delete_collaborator(user = "Irrelevant"),
    "A 'repo', 'project' or 'org' must be specified when deleting collaborators"
  )

})
