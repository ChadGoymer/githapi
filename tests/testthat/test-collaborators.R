context("collaborators")


# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

setup(suppressMessages({

  create_repository(
    name        = str_c("test-collaborators-", suffix),
    description = "This is a repository to test collaborators"
  )

  create_project(
    name = str_c("Test collaborators ", suffix),
    body = "A project to test collaborator functions",
    org  = "HairyCoos"
  )

}))

teardown(suppressMessages({

  delete_repository(str_c("ChadGoymer/test-collaborators-", suffix))

  delete_project(
    project = str_c("Test collaborators ", suffix),
    org     = "HairyCoos"
  )

}))


# TEST: update_collaborator ----------------------------------------------------

test_that("update_collaborator adds a collaborator to a repo or project", {

  repo_result <- update_collaborator(
    user = "ChadGoymer2",
    repo = str_c("ChadGoymer/test-collaborators-", suffix)
  )

  expect_is(repo_result, "logical")
  expect_identical(attr(repo_result, "status"), 201L)
  expect_identical(as.logical(repo_result), TRUE)

  updated_repo_result <- update_collaborator(
    user       = "ChadGoymer2",
    repo       = str_c("ChadGoymer/test-collaborators-", suffix),
    permission = "admin"
  )

  expect_is(updated_repo_result, "logical")
  expect_identical(attr(updated_repo_result, "status"), 201L)
  expect_identical(as.logical(updated_repo_result), TRUE)

  project_result <- update_collaborator(
    user    = "ChadGoymer2",
    project = str_c("Test collaborators ", suffix),
    org     = "HairyCoos"
  )

  expect_is(project_result, "logical")
  expect_identical(attr(project_result, "status"), 204L)
  expect_identical(as.logical(project_result), TRUE)

  updated_project_result <- update_collaborator(
    user       = "ChadGoymer2",
    project    = str_c("Test collaborators ", suffix),
    org        = "HairyCoos",
    permission = "admin"
  )

  expect_is(updated_project_result, "logical")
  expect_identical(attr(updated_project_result, "status"), 204L)
  expect_identical(as.logical(updated_project_result), TRUE)

})

test_that("update_collaborator throws an error with invalid arguments", {

  expect_error(
    update_collaborator(user = "ChadGoymer2"),
    "A 'repo' or 'project' must be specified when updating a collaborator"
  )

})


# TEST: view_collaborators -----------------------------------------------------

test_that("view_collaborators returns a tibble summarising the collaborators", {

  repo_collaborators <- view_collaborators(
    repo  = str_c("ChadGoymer/test-collaborators-", suffix),
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

  expect_true("ChadGoymer" %in% repo_collaborators$login)

  project_collaborators <- view_collaborators(
    project = str_c("Test collaborators ", suffix),
    org     = "HairyCoos",
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

  expect_true("ChadGoymer" %in% project_collaborators$login)

  org_collaborators <- view_collaborators(org = "HairyCoos", n_max = 10)

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
    org         = "HairyCoos",
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
    user = "ChadGoymer2",
    repo = str_c("ChadGoymer/test-collaborators-", suffix)
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

  expect_identical(repo_collaborator$login, "ChadGoymer2")
  expect_identical(repo_collaborator$permission, "read")

  project_collaborator <- view_collaborator(
    user    = "ChadGoymer2",
    project = str_c("Test collaborators ", suffix),
    org     = "HairyCoos"
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

  expect_identical(project_collaborator$login, "ChadGoymer2")
  expect_identical(project_collaborator$permission, "admin")

})

test_that("view_collaborator throws an error with invalid arguments", {

  expect_error(
    view_collaborator(user = "ChadGoymer2"),
    "A 'repo' or 'project' must be specified when viewing a collaborator"
  )

})


# TEST: delete_collaborator ----------------------------------------------------

test_that("delete_collaborator removes a collaborator", {

  repo_collaborator <- delete_collaborator(
    user = "ChadGoymer2",
    repo = str_c("ChadGoymer/test-collaborators-", suffix)
  )

  expect_is(repo_collaborator, "logical")
  expect_identical(attr(repo_collaborator, "status"), 204L)
  expect_identical(as.logical(repo_collaborator), TRUE)

  project_collaborator <- delete_collaborator(
    user    = "ChadGoymer2",
    project = str_c("Test collaborators ", suffix),
    org     = "HairyCoos"
  )

  expect_is(project_collaborator, "logical")
  expect_identical(attr(project_collaborator, "status"), 204L)
  expect_identical(as.logical(project_collaborator), TRUE)

  org_collaborator <- delete_collaborator("ChadGoymer2", org = "HairyCoos")

  expect_is(org_collaborator, "logical")
  expect_identical(attr(org_collaborator, "status"), 204L)
  expect_identical(as.logical(org_collaborator), TRUE)

})

test_that("delete_collaborator throws an error with invalid arguments", {

  expect_error(
    delete_collaborator(user = "ChadGoymer2"),
    "A 'repo', 'project' or 'org' must be specified when deleting collaborators"
  )

})
