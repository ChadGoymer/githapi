# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

suppressMessages({

  user <- view_user()

  org <- view_organizations(user = NULL, n_max = 10) %>%
    arrange(.data$login) %>%
    slice(1)

  if (nrow(org) == 1) {
    create_team(
      name        = str_c("test-users-", suffix),
      org         = org$login,
      description = "This is a team to test users"
    )
  }

})

teardown(suppressMessages({

  try(
    delete_team(str_c("test-users-", suffix), org = org$login),
    silent = TRUE
  )

}))


# TEST: update_user ------------------------------------------------------------

test_that("update_user changes the user's properties", {

  original_user <- view_user(user$login)

  on.exit({
    update_user(
      name     = original_user$name,
      email    = original_user$email,
      blog     = original_user$blog,
      company  = original_user$company,
      location = original_user$location,
      hireable = FALSE,
      bio      = original_user$bio
    )
  })

  updated_user <- update_user(
    name     = "Bob",
    email    = original_user$email,
    blog     = "https://acme.com/blog",
    company  = "ACME",
    location = "Nowhere",
    hireable = TRUE,
    bio      = "Blah Blah"
  )

  expect_is(updated_user, "list")
  expect_identical(attr(updated_user, "status"), 200L)
  expect_identical(
    map_chr(updated_user, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      name       = "character",
      email      = "character",
      blog       = "character",
      company    = "character",
      location   = "character",
      hireable   = "logical",
      bio        = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )

  expect_identical(updated_user$login, user$login)
  expect_identical(updated_user$name, "Bob")
  expect_identical(updated_user$location, "Nowhere")
  expect_identical(updated_user$hireable, TRUE)

})


# TEST: view_users -------------------------------------------------------------

test_that("view_users returns a tibble summarising the users", {

  all_users <- view_users(n_max = 10)

  expect_is(all_users, "tbl")
  expect_identical(attr(all_users, "status"), 200L)
  expect_identical(
    map_chr(all_users, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )

  skip_if(
    length(org$login) != 1,
    "Authenticated user is not a member of an organization"
  )

  org_users <- view_users(org = org$login)

  expect_is(org_users, "tbl")
  expect_identical(attr(org_users, "status"), 200L)
  expect_identical(
    map_chr(org_users, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )

  expect_true(user$login %in% org_users$login)

  team_users <- view_users(
    org = org$login,
    team = str_c("test-users-", suffix)
  )

  expect_is(team_users, "tbl")
  expect_identical(attr(team_users, "status"), 200L)
  expect_identical(
    map_chr(team_users, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )

  expect_true(user$login %in% team_users$login)

  admin_users <- view_users(org = org$login, role = "admin")

  expect_is(admin_users, "tbl")
  expect_identical(attr(admin_users, "status"), 200L)
  expect_identical(
    map_chr(admin_users, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )

  expect_true(user$login %in% admin_users$login)
  expect_true(nrow(admin_users) < nrow(org_users))

})


# TEST: view_user --------------------------------------------------------------

test_that("view_user returns a list of user properties", {

  expect_is(user, "list")
  expect_identical(attr(user, "status"), 200L)
  expect_identical(
    map_chr(user, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      name       = "character",
      email      = "character",
      blog       = "character",
      company    = "character",
      location   = "character",
      hireable   = "logical",
      bio        = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )


  named_user <- view_user(user$login)

  expect_is(named_user, "list")
  expect_identical(attr(named_user, "status"), 200L)
  expect_identical(
    map_chr(named_user, ~ class(.)[[1]]),
    c(
      id         = "integer",
      login      = "character",
      name       = "character",
      email      = "character",
      blog       = "character",
      company    = "character",
      location   = "character",
      hireable   = "logical",
      bio        = "character",
      site_admin = "logical",
      html_url   = "character"
    )
  )

  expect_identical(named_user$login, user$login)

})


# TEST: browse_user ------------------------------------------------------------

test_that("browse_user opens the user's page in the browser", {

  skip_if(!interactive(), "browse_user must be tested manually")

  auth_user <- browse_user()

  base_url <- getOption("github.oauth") %>%
    str_remove("login/oauth") %>%
    str_c(user$login)

  expect_is(auth_user, "character")
  expect_identical(attr(auth_user, "status"), 200L)
  expect_identical(as.character(auth_user), base_url)

  named_user <- browse_user(user$login)

  expect_is(named_user, "character")
  expect_identical(attr(named_user, "status"), 200L)
  expect_identical(as.character(named_user), base_url)

})
