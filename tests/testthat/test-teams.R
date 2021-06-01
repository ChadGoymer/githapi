# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  user <- view_user()

  org <- view_organizations(user = NULL, n_max = 10) %>%
    arrange(.data$login) %>%
    slice(1)

})


# TEST: create_team ------------------------------------------------------------

test_that("create_team creates a team and returns its properties", {

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  first_team <- create_team(
    name        = str_c("Test team ", suffix),
    org         = org$login,
    description = "This is a test team",
    repo_names  = str_c(org$login, "/test-repo")
  )

  expect_is(first_team, "list")
  expect_identical(attr(first_team, "status"), 201L)
  expect_identical(
    map_chr(first_team, ~ class(.)[[1]]),
    c(
      id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      html_url      = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"
    )
  )

  expect_identical(first_team$name, str_c("Test team ", suffix))
  expect_identical(first_team$organization, org$login)
  expect_identical(first_team$description, "This is a test team")

  maintainers_team <- create_team(
    name        = str_c("Test team 2 ", suffix),
    org         = org$login,
    maintainers = user$login
  )

  expect_is(maintainers_team, "list")
  expect_identical(attr(maintainers_team, "status"), 201L)
  expect_identical(
    map_chr(maintainers_team, ~ class(.)[[1]]),
    c(
      id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      html_url      = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"
    )
  )

  expect_identical(maintainers_team$name, str_c("Test team 2 ", suffix))
  expect_identical(maintainers_team$organization, org$login)
  expect_identical(maintainers_team$members_count, 1L)

  closed_team <- create_team(
    name    = str_c("Test team 3 ", suffix),
    org     = org$login,
    privacy = "closed"
  )

  expect_is(closed_team, "list")
  expect_identical(attr(closed_team, "status"), 201L)
  expect_identical(
    map_chr(closed_team, ~ class(.)[[1]]),
    c(
      id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      html_url      = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"
    )
  )

  expect_identical(closed_team$name, str_c("Test team 3 ", suffix))
  expect_identical(closed_team$organization, org$login)
  expect_identical(closed_team$privacy, "closed")

  parent_team <- create_team(
    name        = str_c("Test team 4 ", suffix),
    org         = org$login,
    parent_team = str_c("Test team 3 ", suffix)
  )

  expect_is(parent_team, "list")
  expect_identical(attr(parent_team, "status"), 201L)
  expect_identical(
    map_chr(parent_team, ~ class(.)[[1]]),
    c(
      id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      html_url      = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"
    )
  )

  expect_identical(parent_team$name, str_c("Test team 4 ", suffix))
  expect_identical(parent_team$organization, org$login)
  expect_identical(parent_team$parent, str_c("Test team 3 ", suffix))

})


# TEST: update_team ------------------------------------------------------------

test_that("update_team changes the team's properties", {

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  original_team <- view_team(str_c("Test team ", suffix), org$login)

  updated_team <- update_team(
    team        = str_c("Test team ", suffix),
    name        = str_c("First test team ", suffix),
    org         = org$login,
    description = "This is a test team",
    privacy     = "closed",
    parent_team = str_c("Test team 3 ", suffix)
  )

  expect_is(updated_team, "list")
  expect_identical(attr(updated_team, "status"), 200L)
  expect_identical(
    map_chr(updated_team, ~ class(.)[[1]]),
    c(
      id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      html_url      = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"
    )
  )

  expect_identical(updated_team$name, str_c("First test team ", suffix))
  expect_identical(updated_team$description, "This is a test team")
  expect_identical(updated_team$privacy, "closed")
  expect_identical(updated_team$parent, str_c("Test team 3 ", suffix))

})


# TEST: view_teams -------------------------------------------------------------

test_that("view_teams returns a tibble summarising the teams", {

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  org_teams <- view_teams(org$login, n_max = 10)

  expect_is(org_teams, "tbl")
  expect_identical(attr(org_teams, "status"), 200L)
  expect_identical(
    map_chr(org_teams, ~ class(.)[[1]]),
    c(
      id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character"
    )
  )

  expect_true(str_c("First test team ", suffix) %in% org_teams$name)

  team_teams <- view_teams(
    org         = org$login,
    parent_team = str_c("Test team 3 ", suffix),
    n_max       = 10
  )

  expect_is(team_teams, "tbl")
  expect_identical(attr(team_teams, "status"), 200L)
  expect_identical(
    map_chr(team_teams, ~ class(.)[[1]]),
    c(
      id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character"
    )
  )

  expect_true(str_c("First test team ", suffix) %in% team_teams$name)

  user_teams <- view_teams(n_max = 10)

  expect_is(user_teams, "tbl")
  expect_identical(attr(user_teams, "status"), 200L)
  expect_identical(
    map_chr(user_teams, ~ class(.)[[1]]),
    c(
      id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character"
    )
  )

  expect_true(str_c("First test team ", suffix) %in% user_teams$name)

})


# TEST: view_team --------------------------------------------------------------

test_that("view_team returns a list of team properties", {

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  team <- view_team(str_c("First test team ", suffix), org$login)

  expect_is(team, "list")
  expect_identical(attr(team, "status"), 200L)
  expect_identical(
    map_chr(team, ~ class(.)[[1]]),
    c(
      id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      html_url      = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"
    )
  )

  expect_identical(team$name, str_c("First test team ", suffix))

  team_by_id <- view_team(team$id)

  expect_is(team_by_id, "list")
  expect_identical(attr(team_by_id, "status"), 200L)
  expect_identical(
    map_chr(team_by_id, ~ class(.)[[1]]),
    c(
      id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      html_url      = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"
    )
  )

  expect_identical(team_by_id$name, str_c("First test team ", suffix))

})


# TEST: browse_team ------------------------------------------------------------

test_that("browse_team opens the team's page in the browser", {

  skip_if(!interactive(), "browse_team must be tested manually")

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  team <- browse_team(str_c("First test team ", suffix), org$login)

  base_url <- getOption("github.oauth") %>%
    str_remove("login/oauth") %>%
    str_c("orgs/", org$login)

  expect_is(team, "character")
  expect_identical(attr(team, "status"), 200L)
  expect_identical(
    as.character(team),
    str_c(base_url, "/teams/first-test-team-", suffix)
  )


  team <- view_team(str_c("First test team ", suffix), org$login)
  team_by_id <- browse_team(team$id)

  expect_is(team_by_id, "character")
  expect_identical(attr(team_by_id, "status"), 200L)
  expect_identical(
    as.character(team_by_id),
    str_c(base_url, "/teams/first-test-team-", suffix)
  )

  expect_error(browse_team(FALSE), "'team' must be an integer or string")

})


# TEST: delete_team ------------------------------------------------------------

test_that("delete_team removes a team from an organization", {

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  first_team <- delete_team(str_c("First test team ", suffix), org$login)

  expect_is(first_team, "logical")
  expect_identical(attr(first_team, "status"), 204L)
  expect_identical(as.logical(first_team), TRUE)

  secret_team <- delete_team(str_c("Test team 2 ", suffix), org$login)

  expect_is(secret_team, "logical")
  expect_identical(attr(secret_team, "status"), 204L)
  expect_identical(as.logical(secret_team), TRUE)

  parent_team <- delete_team(str_c("Test team 3 ", suffix), org$login)

  expect_is(parent_team, "logical")
  expect_identical(attr(parent_team, "status"), 204L)
  expect_identical(as.logical(parent_team), TRUE)

})
