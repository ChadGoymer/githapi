#  FUNCTION: update_user -------------------------------------------------------
#
#' Update your user properties in GitHub
#'
#' This function updates your user properties in GitHub. You cannot update
#' someone else's profile.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "users#update-the-authenticated-user",
#'   ">"
#' ))
#' ```
#'
#' @param name (string, optional) The new name of the user.
#' @param email (string, optional) The publicly visible email address of the
#'   user.
#' @param blog (string, optional) The new blog URL of the user.
#' @param company (string, optional) The new company of the user.
#' @param location (string, optional) The new location of the user.
#' @param hireable (boolean, optional) The new hiring availability of the user.
#' @param bio (string, optional) The new short biography of the user.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_user()` returns a list of the new user properties.
#'
#' **User Properties:**
#'
#' - **id**: The ID of the user.
#' - **login**: The login name of the user.
#' - **name**: The name of the user.
#' - **email**: The public email address of the user.
#' - **blog**: The blog address of the user.
#' - **company**: The company the user works for.
#' - **location**: The location of the user.
#' - **hireable**: Whether the user currently hireable.
#' - **bio**: The biography of the user.
#' - **type**: The type of account.
#' - **site_admin**: Whether the user is an administrator.
#' - **html_url**: The GitHub page for the user.
#'
#' @examples
#' \dontrun{
#'
#'   # Update your name
#'   update_user(name = "Bob Smith")
#'
#'   # Update your company
#'   update_user(company = "Acme")
#'
#'   # Update your hireable status
#'   update_user(hireable = TRUE)
#'
#' }
#'
#' @export
#'
update_user <- function(
  name,
  email,
  blog,
  company,
  location,
  hireable,
  bio,
  ...
) {
  payload <- list()

  if (!is_missing_or_null(name)) {
    assert(
      is_scalar_character(name),
      "'name' must be a string:\n  ", name
    )
    payload$name <- name
  }

  if (!is_missing_or_null(email)) {
    assert(
      is_scalar_character(email),
      "'email' must be a string:\n  ", email
    )
    payload$email <- email
  }

  if (!is_missing_or_null(blog)) {
    assert(
      is_scalar_character(blog),
      "'blog' must be a string:\n  ", blog
    )
    payload$blog <- blog
  }

  if (!is_missing_or_null(company)) {
    assert(
      is_scalar_character(company),
      "'company' must be a string:\n  ", company
    )
    payload$company <- company
  }

  if (!is_missing_or_null(location)) {
    assert(
      is_scalar_character(location),
      "'location' must be a string:\n  ", location
    )
    payload$location <- location
  }

  if (!is_missing_or_null(hireable)) {
    assert(
      is_scalar_logical(hireable),
      "'hireable' must be a boolean:\n  ", hireable
    )
    payload$hireable <- hireable
  }

  if (!is_missing_or_null(bio)) {
    assert(
      is_scalar_character(bio),
      "'bio' must be a string:\n  ", bio
    )
    payload$bio <- bio
  }

  info("Updating user")
  user_lst <- gh_url("user") %>%
    gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  user_gh <- select_properties(user_lst, properties$user)

  info("Done", level = 7)
  user_gh
}


#  FUNCTION: view_users --------------------------------------------------------
#
#' View users in GitHub
#'
#' `view_users()` summarises users in a table with the properties as columns and
#' a row for each user. `view_user()` returns a list of all properties for a
#' single user. `browse_user()` opens the web page for the user in the default
#' browser.
#'
#' You can summarise all the users associated with either an organization or a
#' team within an organization. If none of those are supplied the first `n_max`
#' users of GitHub are returned.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "orgs#list-organization-members",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#list-team-members",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "users#list-users",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "users#get-a-user",
#'   ">"
#' ))
#' ```
#'
#' @param user (string) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param team (string, optional) The name of the team.
#' @param role (string, optional) Filter the result by role. Can specify either
#'   `"admin"`, `"member"` or `"all"`. Default: `"all"`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_users()` returns a tibble of user properties. `view_user()`
#'   returns a list of properties for a single user.  `browse_user()` opens the
#'   default browser on the user's page and returns the URL.
#'
#' **user Properties:**
#'
#' - **id**: The ID of the user.
#' - **login**: The login name of the user.
#' - **name**: The name of the user (only available in `view_user()`).
#' - **email**: The public email address of the user (only available in
#'   `view_user()`).
#' - **blog**: The blog address of the user (only available in `view_user()`).
#' - **company**: The company the user works for (only available in
#'   `view_user()`).
#' - **location**: The location of the user (only available in `view_user()`).
#' - **hireable**: Whether the user currently hireable (only available in
#'   `view_user()`).
#' - **bio**: The biography of the user (only available in `view_user()`).
#' - **site_admin**: Whether the user is an administrator.
#' - **html_url**: The GitHub page for the user.
#'
#' @examples
#' \dontrun{
#'
#'   # View users collaborating on a repository
#'   view_users(repo = "ChadGoymer/githapi")
#'
#'   # View users in an organization
#'   view_users(org = "HairyCoos")
#'
#'   # View users in a team within an organization
#'   view_users(org = "HairyCoos", team = "HeadCoos")
#'
#'   # View the admins of an organization
#'   view_users(org = "HairyCoos", role = "admin")
#'
#'   # View a single user
#'   view_user("ChadGoymer")
#'
#'   # Browse a user's GitHub page
#'   browse_user("ChadGoymer")
#'
#' }
#'
#' @export
#'
view_users <- function(
  org,
  team,
  role  = "all",
  n_max = 1000,
  ...
) {
  if (!is_missing_or_null(org)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )
    assert(
      is_scalar_character(role) && role %in% values$user$role,
      "'role' must be one of '",
      str_c(values$card$role, collapse = "', '"), "':\n  ", role
    )

    if (!is_missing_or_null(team)) {
      assert(
        is_scalar_character(team),
        "'team' must be a string:\n  ", team
      )

      team <- gh_url("orgs", org, "teams") %>%
        gh_find(property = "name", value = team, ...)

      info("Viewing users in team '", team$name, "'")
      url <- gh_url("teams", team$id, "members", role = role)
    }
    else {
      info("Viewing users in organization '", org, "'")
      url <- gh_url("orgs", org, "members", role = role)
    }
  }
  else {
    info("Viewing all users")
    url <- gh_url("users")
  }

  users_lst <- gh_page(url = url, n_max  = n_max, ...)

  info("Transforming results", level = 4)
  users_gh <- bind_properties(users_lst, properties$users)

  info("Done", level = 7)
  users_gh
}


#  FUNCTION: view_user ---------------------------------------------------------
#
#' @rdname view_users
#' @export
#'
view_user <- function(
  user,
  ...
) {
  if (is_missing_or_null(user)) {
    info("Viewing authenticated user")
    url <- gh_url("user")
  }
  else {
    assert(
      is_scalar_character(user),
      "'user' must be a string:\n  ", user
    )
    info("Viewing user '", user, "'")
    url <- gh_url("users", user)
  }

  user_lst <- gh_request("GET", url = url, ...)

  info("Transforming results", level = 4)
  user_gh <- select_properties(user_lst, properties$user)

  info("Done", level = 7)
  user_gh
}


#  FUNCTION: browse_user -------------------------------------------------------
#
#' @rdname view_users
#' @export
#'
browse_user <- function(
  user,
  ...
) {
  user <- view_user(user, ...)

  info("Browsing user '", user$name, "'")
  httr::BROWSE(user$html_url)

  info("Done", level = 7)
  structure(
    user$html_url,
    class   = c("github", "character"),
    url     = attr(user, "url"),
    request = attr(user, "request"),
    status  = attr(user, "status"),
    header  = attr(user, "header")
  )
}
