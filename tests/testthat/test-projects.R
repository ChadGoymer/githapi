# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  repo <- create_repository(
    name        = str_c("test-projects-", suffix),
    description = "This is a repository to test projects"
  )

  user <- view_user()

  org <- view_organizations(user = NULL, n_max = 10) %>%
    arrange(.data$login) %>%
    slice(1)

  if (nrow(org) == 1) {
    create_team(
      name        = str_c("Test projects ", suffix),
      description = "This was created to test team projects",
      org         = org$login
    )
  }

})

teardown(suppressMessages({

  try(
    delete_team(str_c("Test projects ", suffix), org = org$login),
    silent = TRUE
  )

  try(delete_repository(repo$full_name), silent = TRUE)

}))


# TEST: create_project ---------------------------------------------------------

test_that("create_projects creates a project and returns its properties", {

  repo_project <- create_project(
    name = str_c("Repo project ", suffix),
    body = "This is a repo project",
    repo = repo$full_name
  )

  expect_is(repo_project, "list")
  expect_identical(attr(repo_project, "status"), 201L)
  expect_identical(
    map_chr(repo_project, ~ class(.)[[1]]),
    c(
      id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(repo_project$name, str_c("Repo project ", suffix))

  user_project <- create_project(
    name = str_c("User project ", suffix),
    body = "This is a user project"
  )

  expect_is(user_project, "list")
  expect_identical(attr(user_project, "status"), 201L)
  expect_identical(
    map_chr(user_project, ~ class(.)[[1]]),
    c(
      id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(user_project$name, str_c("User project ", suffix))

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  org_project <- create_project(
    name = str_c("Organization project ", suffix),
    body = "This is an organization project",
    org  = org$login
  )

  expect_is(org_project, "list")
  expect_identical(attr(org_project, "status"), 201L)
  expect_identical(
    map_chr(org_project, ~ class(.)[[1]]),
    c(
      id             = "integer",
      number         = "integer",
      name           = "character",
      body           = "character",
      state          = "character",
      private        = "logical",
      org_permission = "character",
      creator        = "character",
      html_url       = "character",
      created_at     = "POSIXct",
      updated_at     = "POSIXct"
    )
  )

  expect_identical(org_project$name, str_c("Organization project ", suffix))

})

test_that("create_project throws an error if invalid arguments are supplied", {

  expect_error(
    create_project(name = 1, body = "This is an invalid project"),
    "'name' must be a string"
  )

  expect_error(
    create_project(name = "invalid project", body = 1),
    "'body' must be a string"
  )

  expect_error(
    create_project(
      name = "invalid project",
      body = "This is an invalid project",
      repo = 1
    ),
    "'repo' must be a string in the format 'owner/repo'"
  )

  expect_error(
    create_project(
      name = "invalid project",
      body = "This is an invalid project",
      org  = 1
    ),
    "'org' must be a string"
  )

})


# TEST: update_project ---------------------------------------------------------

test_that("update_project updates a project and returns the new properties", {

  repo_project <- update_project(
    project = str_c("Repo project ", suffix),
    name    = str_c("Updated repo project ", suffix),
    body    = "This is an updated repo project",
    repo    = repo$full_name
  )

  expect_is(repo_project, "list")
  expect_identical(attr(repo_project, "status"), 200L)
  expect_identical(
    map_chr(repo_project, ~ class(.)[[1]]),
    c(
      id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(repo_project$name, str_c("Updated repo project ", suffix))

  user_project <- update_project(
    project = str_c("User project ", suffix),
    state   = "closed",
    user    = user$login
  )

  expect_is(user_project, "list")
  expect_identical(attr(user_project, "status"), 200L)
  expect_identical(
    map_chr(user_project, ~ class(.)[[1]]),
    c(
      id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(user_project$state, "closed")

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  org_project <- update_project(
    project    = str_c("Organization project ", suffix),
    permission = "read",
    private    = FALSE,
    org        = org$login
  )

  expect_is(org_project, "list")
  expect_identical(attr(org_project, "status"), 200L)
  expect_identical(
    map_chr(org_project, ~ class(.)[[1]]),
    c(
      id             = "integer",
      number         = "integer",
      name           = "character",
      body           = "character",
      state          = "character",
      private        = "logical",
      org_permission = "character",
      creator        = "character",
      html_url       = "character",
      created_at     = "POSIXct",
      updated_at     = "POSIXct"
    )
  )

  expect_identical(org_project$org_permission, "read")
  expect_identical(org_project$private, FALSE)

  team_project <- update_project(
    project = str_c("Organization project ", suffix),
    team    = str_c("Test projects ", suffix),
    org     = org$login
  )

  expect_is(team_project, "list")
  expect_identical(attr(team_project, "status"), 204L)
  expect_identical(
    map_chr(team_project, ~ class(.)[[1]]),
    c(
      id              = "integer",
      number          = "integer",
      name            = "character",
      body            = "character",
      state           = "character",
      private         = "logical",
      org_permission  = "character",
      team_permission = "character",
      creator         = "character",
      html_url        = "character",
      created_at      = "POSIXct",
      updated_at      = "POSIXct"
    )
  )

  expect_identical(team_project$team_permission, "write")

  upd_team_project <- update_project(
    project    = str_c("Organization project ", suffix),
    team       = str_c("Test projects ", suffix),
    org        = org$login,
    permission = "read"
  )

  expect_is(upd_team_project, "list")
  expect_identical(attr(upd_team_project, "status"), 204L)
  expect_identical(
    map_chr(upd_team_project, ~ class(.)[[1]]),
    c(
      id              = "integer",
      number          = "integer",
      name            = "character",
      body            = "character",
      state           = "character",
      private         = "logical",
      org_permission  = "character",
      team_permission = "character",
      creator         = "character",
      html_url        = "character",
      created_at      = "POSIXct",
      updated_at      = "POSIXct"
    )
  )

  expect_identical(upd_team_project$team_permission, "read")

})


# TEST: view_projects ----------------------------------------------------------

test_that("view_projects returns a tibble summarising the projects", {

  repo_projects <- view_projects(
    repo  = repo$full_name,
    n_max = 10
  )

  expect_is(repo_projects, "tbl")
  expect_identical(attr(repo_projects, "status"), 200L)
  expect_identical(
    map_chr(repo_projects, ~ class(.)[[1]]),
    c(
      id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_true(str_c("Updated repo project ", suffix) %in% repo_projects$name)

  user_projects <- view_projects(
    user  = user$login,
    state = "closed",
    n_max = 10
  )

  expect_is(user_projects, "tbl")
  expect_identical(attr(user_projects, "status"), 200L)
  expect_identical(
    map_chr(user_projects, ~ class(.)[[1]]),
    c(
      id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_true(str_c("User project ", suffix) %in% user_projects$name)

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  org_projects <- view_projects(org = org$login, n_max = 10)

  expect_is(org_projects, "tbl")
  expect_identical(attr(org_projects, "status"), 200L)
  expect_identical(
    map_chr(org_projects, ~ class(.)[[1]]),
    c(
      id             = "integer",
      number         = "integer",
      name           = "character",
      body           = "character",
      state          = "character",
      private        = "logical",
      org_permission = "character",
      creator        = "character",
      html_url       = "character",
      created_at     = "POSIXct",
      updated_at     = "POSIXct"
    )
  )

  expect_true(str_c("Organization project ", suffix) %in% org_projects$name)

  team_projects <- view_projects(
    team  = str_c("Test projects ", suffix),
    org   = org$login,
    n_max = 10
  )

  expect_is(team_projects, "tbl")
  expect_identical(attr(team_projects, "status"), 200L)
  expect_identical(
    map_chr(team_projects, ~ class(.)[[1]]),
    c(
      id              = "integer",
      number          = "integer",
      name            = "character",
      body            = "character",
      state           = "character",
      private         = "logical",
      org_permission  = "character",
      team_permission = "character",
      creator         = "character",
      html_url        = "character",
      created_at      = "POSIXct",
      updated_at      = "POSIXct"
    )
  )

  expect_true(str_c("Organization project ", suffix) %in% team_projects$name)

})

test_that("view_projects throws an error if invalid arguments are supplied", {

  expect_error(
    view_projects(),
    "Must specify either 'repo', 'user' or 'org'!"
  )

})


# TEST: view_project -----------------------------------------------------------

test_that("view_project returns a list of project properties", {

  repo_project <- view_project(
    project = str_c("Updated repo project ", suffix),
    repo    = repo$full_name
  )

  expect_is(repo_project, "list")
  expect_identical(attr(repo_project, "status"), 200L)
  expect_identical(
    map_chr(repo_project, ~ class(.)[[1]]),
    c(
      id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(repo_project$name, str_c("Updated repo project ", suffix))

  user_project <- view_project(
    project = str_c("User project ", suffix),
    user    = user$login
  )

  expect_is(user_project, "list")
  expect_identical(attr(user_project, "status"), 200L)
  expect_identical(
    map_chr(user_project, ~ class(.)[[1]]),
    c(
      id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(user_project$state, "closed")

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  org_project <- view_project(
    project = str_c("Organization project ", suffix),
    org     = org$login
  )

  expect_is(org_project, "list")
  expect_identical(attr(org_project, "status"), 200L)
  expect_identical(
    map_chr(org_project, ~ class(.)[[1]]),
    c(
      id             = "integer",
      number         = "integer",
      name           = "character",
      body           = "character",
      state          = "character",
      private        = "logical",
      org_permission = "character",
      creator        = "character",
      html_url       = "character",
      created_at     = "POSIXct",
      updated_at     = "POSIXct"
    )
  )

  expect_identical(org_project$org_permission, "read")
  expect_identical(org_project$private, FALSE)

  team_project <- view_project(
    project = str_c("Organization project ", suffix),
    team    = str_c("Test projects ", suffix),
    org     = org$login
  )

  expect_is(team_project, "list")
  expect_identical(attr(team_project, "status"), 200L)
  expect_identical(
    map_chr(team_project, ~ class(.)[[1]]),
    c(
      id              = "integer",
      number          = "integer",
      name            = "character",
      body            = "character",
      state           = "character",
      private         = "logical",
      org_permission  = "character",
      team_permission = "character",
      creator         = "character",
      html_url        = "character",
      created_at      = "POSIXct",
      updated_at      = "POSIXct"
    )
  )

  expect_identical(team_project$team_permission, "read")

})

test_that("view_project can accept a project number", {

  projects <- view_projects(repo$full_name)

  first_project <- view_project(
    project = projects$number[[1]],
    repo    = repo$full_name
  )

  expect_is(first_project, "list")
  expect_identical(attr(first_project, "status"), 200L)
  expect_identical(
    map_chr(first_project, ~ class(.)[[1]]),
    c(
      id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(first_project$number, projects$number[[1]])

})

test_that("view_project throws an error if invalid arguments are supplied", {

  expect_error(
    view_project(TRUE, repo$full_name),
    "'project' must be either an integer or a string"
  )

  expect_error(
    view_project(str_c("Repo project ", suffix)),
    "Must specify either 'repo', 'user' or 'org'!"
  )

})


# TEST: browse_project ---------------------------------------------------------

test_that("browse_project opens the project in the browser", {

  skip_if(!interactive(), "browse_project must be tested manually")

  repo_project <- browse_project(
    project = str_c("Updated repo project ", suffix),
    repo    = repo$full_name
  )

  base_url <- getOption("github.oauth") %>%
    str_remove("login/oauth")

  expect_is(repo_project, "character")
  expect_identical(attr(repo_project, "status"), 200L)
  expect_identical(
    dirname(repo_project),
    str_c(base_url, user$login, "/test-projects-", suffix, "/projects")
  )

  user_project <- browse_project(
    project = str_c("User project ", suffix),
    user    = user$login
  )

  expect_is(user_project, "character")
  expect_identical(attr(user_project, "status"), 200L)
  expect_identical(
    dirname(user_project),
    str_c(base_url, "users/", user$login, "/projects")
  )

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  org_project <- browse_project(
    project = str_c("Organization project ", suffix),
    org     = org$login
  )

  expect_is(org_project, "character")
  expect_identical(attr(org_project, "status"), 200L)
  expect_identical(
    dirname(org_project),
    str_c(base_url, "orgs/", org$login, "/projects")
  )

  team_project <- browse_project(
    project = str_c("Organization project ", suffix),
    team    = str_c("Test projects ", suffix),
    org     = org$login
  )

  expect_is(team_project, "character")
  expect_identical(attr(team_project, "status"), 200L)
  expect_identical(
    dirname(team_project),
    str_c(base_url, "orgs/", org$login, "/projects")
  )

})


# TEST: delete_project ---------------------------------------------------------

test_that("delete_project deletes the projects and returns TRUE", {

  repo_project <- delete_project(
    project = str_c("Updated repo project ", suffix),
    repo    = repo$full_name
  )

  expect_is(repo_project, "logical")
  expect_identical(attr(repo_project, "status"), 204L)
  expect_identical(as.logical(repo_project), TRUE)

  user_project <- delete_project(
    project = str_c("User project ", suffix),
    user    = user$login
  )

  expect_is(user_project, "logical")
  expect_identical(attr(user_project, "status"), 204L)
  expect_identical(as.logical(user_project), TRUE)

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  team_project <- delete_project(
    project = str_c("Organization project ", suffix),
    team    = str_c("Test projects ", suffix),
    org     = org$login
  )

  expect_is(team_project, "logical")
  expect_identical(attr(team_project, "status"), 204L)
  expect_identical(as.logical(team_project), TRUE)

  org_project <- delete_project(
    project = str_c("Organization project ", suffix),
    org     = org$login
  )

  expect_is(org_project, "logical")
  expect_identical(attr(org_project, "status"), 204L)
  expect_identical(as.logical(org_project), TRUE)

})
