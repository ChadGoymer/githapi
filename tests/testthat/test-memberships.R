# SETUP ------------------------------------------------------------------------

suppressMessages({

  user_1 <- view_user()
  user_2 <- tryCatch(
    view_user("ChadGoymer2"),
    error = function(e) list(login = character())
  )

  org <- view_organizations(user = NULL, n_max = 10) %>%
    arrange(.data$login) %>%
    slice(1)

  team <- view_teams(org = org$login) %>%
    arrange(.data$name) %>%
    slice(1)

})

# TEST: update_membership ------------------------------------------------------

test_that("update_membership returns a list of membership properties", {

  skip_if(
    length(user_2$login) != 1,
    "Test user does not exist"
  )
  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  org_membership <- update_membership(user_2$login, org$login)

  expect_is(org_membership, "list")
  expect_identical(attr(org_membership, "status"), 200L)
  expect_identical(
    map_chr(org_membership, ~ class(.)[[1]]),
    c(
      user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"
    )
  )

  expect_identical(org_membership$user, user_2$login)
  expect_identical(org_membership$organization, org$login)
  expect_identical(org_membership$role, "member")
  expect_identical(org_membership$state, "pending")

  org_role_membership <- update_membership(
    user = user_2$login,
    org  = org$login,
    role = "admin"
  )

  expect_is(org_role_membership, "list")
  expect_identical(attr(org_role_membership, "status"), 200L)
  expect_identical(
    map_chr(org_role_membership, ~ class(.)[[1]]),
    c(
      user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"
    )
  )

  expect_identical(org_role_membership$user, user_2$login)
  expect_identical(org_role_membership$organization, org$login)
  expect_identical(org_role_membership$role, "admin")
  expect_identical(org_role_membership$state, "pending")

  skip_if(
    length(team$name) != 1,
    "Organization does not have any teams defined"
  )

  team_membership <- update_membership(user_2$login, org$login, team$name)

  expect_is(team_membership, "list")
  expect_identical(attr(team_membership, "status"), 200L)
  expect_identical(
    map_chr(team_membership, ~ class(.)[[1]]),
    c(
      user         = "character",
      organization = "character",
      team         = "character",
      role         = "character",
      state        = "character"
    )
  )

  expect_identical(team_membership$user, user_2$login)
  expect_identical(team_membership$organization, org$login)
  expect_identical(team_membership$team, team$name)
  expect_identical(team_membership$role, "member")
  expect_identical(team_membership$state, "pending")

  team_role_membership <- update_membership(
    user = user_2$login,
    org  = org$login,
    team = team$name,
    role = "maintainer"
  )

  expect_is(team_role_membership, "list")
  expect_identical(attr(team_role_membership, "status"), 200L)
  expect_identical(
    map_chr(team_role_membership, ~ class(.)[[1]]),
    c(
      user         = "character",
      organization = "character",
      team         = "character",
      role         = "character",
      state        = "character"
    )
  )

  expect_identical(team_role_membership$user, user_2$login)
  expect_identical(team_role_membership$organization, org$login)
  expect_identical(team_role_membership$team, team$name)
  expect_identical(team_role_membership$role, "maintainer")
  expect_identical(team_role_membership$state, "pending")

})


# TEST: view_memberships -------------------------------------------------------

test_that("view_memberships returns a tibble summarising the memberships", {

  memberships <- view_memberships(n_max = 10)

  expect_is(memberships, "tbl")
  expect_identical(attr(memberships, "status"), 200L)
  expect_identical(
    map_chr(memberships, ~ class(.)[[1]]),
    c(
      user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"
    )
  )

  expect_true(org$login %in% memberships$organization)

  hairy_coos <- filter(memberships, .data$organization == org$login)
  expect_identical(hairy_coos$user, user_1$login)
  expect_identical(hairy_coos$state, "active")
  expect_identical(hairy_coos$role, "admin")

  active_memberships <- view_memberships(state = "active", n_max = 10)

  expect_is(active_memberships, "tbl")
  expect_identical(attr(active_memberships, "status"), 200L)
  expect_identical(
    map_chr(active_memberships, ~ class(.)[[1]]),
    c(
      user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"
    )
  )

  expect_true(org$login %in% active_memberships$organization)

  active_hairy_coos <- active_memberships %>%
    filter(.data$organization == org$login)

  expect_identical(active_hairy_coos$user, user_1$login)
  expect_identical(active_hairy_coos$state, "active")
  expect_identical(active_hairy_coos$role, "admin")

  pending_memberships <- view_memberships(state = "pending", n_max = 10)

  expect_is(pending_memberships, "tbl")
  expect_identical(attr(pending_memberships, "status"), 200L)
  expect_identical(
    map_chr(pending_memberships, ~ class(.)[[1]]),
    c(
      user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"
    )
  )

})


# TEST: view_membership --------------------------------------------------------

test_that("view_membership returns a list of membership properties", {

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  user_membership <- view_membership(user_1$login, org$login)

  expect_is(user_membership, "list")
  expect_identical(attr(user_membership, "status"), 200L)
  expect_identical(
    map_chr(user_membership, ~ class(.)[[1]]),
    c(
      user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"
    )
  )

  expect_identical(user_membership$user, user_1$login)
  expect_identical(user_membership$organization, org$login)
  expect_identical(user_membership$role, "admin")
  expect_identical(user_membership$state, "active")

  skip_if(
    length(team$name) != 1,
    "Organization does not have any teams defined"
  )

  team_membership <- view_membership(user_1$login, org$login, team$name)

  expect_is(team_membership, "list")
  expect_identical(attr(team_membership, "status"), 200L)
  expect_identical(
    map_chr(team_membership, ~ class(.)[[1]]),
    c(
      user         = "character",
      organization = "character",
      team         = "character",
      role         = "character",
      state        = "character"
    )
  )

  expect_identical(team_membership$user, user_1$login)
  expect_identical(team_membership$organization, org$login)
  expect_identical(team_membership$team, team$name)
  expect_identical(team_membership$role, "maintainer")
  expect_identical(team_membership$state, "active")

})


# TEST: delete_membership ------------------------------------------------------

test_that("delete_membership removes users from an organization or team", {

  skip_if(
    length(user_2$login) != 1,
    "Test user does not exist"
  )
  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )
  skip_if(
    length(team$name) != 1,
    "Organization does not have any teams defined"
  )

  team_membership <- delete_membership(user_2$login, org$login, team$name)

  expect_is(team_membership, "logical")
  expect_identical(attr(team_membership, "status"), 204L)
  expect_identical(as.logical(team_membership), TRUE)

  org_membership <- delete_membership(user_2$login, org$login)

  expect_is(org_membership, "logical")
  expect_identical(attr(org_membership, "status"), 204L)
  expect_identical(as.logical(org_membership), TRUE)

})
