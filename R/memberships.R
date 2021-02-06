#  FUNCTION: update_membership -------------------------------------------------
#
#' Update membership of an organization or team
#'
#' This function can be used to invite a user into an organization or team, or
#' update their role within the organization or team.
#'
#' Note: you can only invite or update a user if the authenticate user is an
#' organization "owner" or a team "maintainer".
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "orgs#set-organization-membership-for-a-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#add-or-update-team-membership-for-a-user",
#'   ">"
#' ))
#' ```
#'
#' @param user (string) The login of the user.
#' @param org (string) The login of the organization.
#' @param team (integer or string, optional) The ID or name of the team.
#' @param role (string, optional) The role to give the user. For an organization
#'   this is either `"member"` or `"admin"`, for a team it is either `"member"`
#'   or `"maintainer"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_memberships()` returns a list of membership properties
#'
#' **Membership Properties:**
#'
#' - **user**: The user login.
#' - **organization**: The organization login.
#' - **team**: The team name (team membership only).
#' - **role**: The role of the user - for organizations it is either `"member"`
#'   or `"admin"`, for teams it is either `"member"` or `"maintainer"`.
#' - **state**: The state of the membership - either `"active"` or `"pending"`.
#'
#' @examples
#' \dontrun{
#'
#'   # Invite a new user into an organization
#'   update_membership("ChadGoymer2", "HairyCoos")
#'
#'   # Update a user's role in an organization
#'   update_membership("ChadGoymer2", "HairyCoos", role = "admin")
#'
#'   # Invite a new user into a team
#'   update_membership("ChadGoymer2", "HairyCoos", "HeadCoos")
#'
#'   # Update a user's role in a team
#'   update_membership(
#'     user = "ChadGoymer2",
#'     org  = "HairyCoos",
#'     team = "HeadCoos",
#'     role = "maintainer"
#'   )
#'
#' }
#'
#' @export
#'
update_membership <- function(
  user,
  org,
  team,
  role,
  ...
) {
  assert(
    is_scalar_character(user),
    "'user' must be a string:\n  ", user
  )
  assert(
    is_scalar_character(org),
    "'org' must be a string:\n  ", org
  )

  payload <- NULL

  if (is_missing_or_null(team)) {
    if (!is_missing_or_null(role)) {
      assert(
        is_scalar_character(role) && role %in% values$membership$org_role,
        "organization 'role' must be either '",
        str_c(values$membership$org_role, collapse = "', '"), "':\n  ", role
      )
      payload <- list(role = role)
    }

    info("Updating membership for '", user, "' in organization '", org, "'")
    membership_lst <- gh_url("orgs", org, "memberships", user) %>%
      gh_request("PUT", payload = payload, ...)

    info("Transforming results", level = 4)
    membership_gh <- select_properties(membership_lst, properties$memberships)
  }
  else {
    if (!is_missing_or_null(role)) {
      assert(
        is_scalar_character(role) && role %in% values$membership$team_role,
        "organization 'role' must be either '",
        str_c(values$membership$team_role, collapse = "', '"), "':\n  ", role
      )
      payload <- list(role = role)
    }

    if (is_scalar_character(team)) {
      team_id <- gh_url("orgs", org, "teams") %>%
        gh_find(property = "name", value = team, ...) %>%
        pluck("id")
    }
    assert(
      is_scalar_integerish(team_id),
      "'team' must be an integer or string:\n  ", team
    )

    info("Updating membership for '", user, "' in team '", team, "'")
    membership_lst <- gh_url("teams", team_id, "memberships", user) %>%
      gh_request("PUT", payload = payload, ...)

    info("Transforming results", level = 4)
    membership_gh <- select_properties(
      entity     = membership_lst,
      properties = properties$memberships
    ) %>%
      modify_list(user = user, organization = org) %>%
      modify_list(team = team, .after = "organization")
  }

  info("Done", level = 7)
  membership_gh
}


#  FUNCTION: view_memberships --------------------------------------------------
#
#' View membership of organizations or teams
#'
#' `view_memberships()` summarises the authenticate user's membership in
#' organizations in a table with the properties as columns and a row for each
#' organization. `view_membership()` returns a list of all membership properties
#' for a user in a single organization or team.
#'
#' Note: you can only view another user's membership in an organization or team
#' if the authenticate user is also a member.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "orgs#list-organization-memberships-for-the-authenticated-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "orgs#get-organization-membership-for-a-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#get-team-membership-for-a-user",
#'   ">"
#' ))
#' ```
#'
#' @param state (string, optional) Filter results depending on the `state` of
#'   the membership. Can be either `"active"` or `"pending"`. If not supplied,
#'   all memberships are returned for the authenticated user.
#' @param user (string) The login of the user.
#' @param org (string) The login of the organization.
#' @param team (integer or string, optional) The ID or name of the team.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_memberships()` returns a tibble of membership properties.
#'   `view_membership()` returns a list of membership properties for a single
#'   organization or team.
#'
#' **Membership Properties:**
#'
#' - **user**: The user login.
#' - **organization**: The organization login.
#' - **team**: The team name (team membership only).
#' - **role**: The role of the user - for organizations it is either `"member"`
#'   or `"admin"`, for teams it is either `"member"` or `"maintainer"`.
#' - **state**: The state of the membership - either `"active"` or `"pending"`.
#'
#' @examples
#' \dontrun{
#'
#'   # View membership of all organizations for the authenticated user
#'   view_memberships()
#'
#'   # View only active memberships for the authenticated user
#'   view_memberships(state = "active")
#'
#'   # View the membership of a user in an organization
#'   view_membership("ChadGoymer", "HairyCoos")
#'
#'   # View the membership of a user in a team
#'   view_membership(user = "ChadGoymer", org = "HairyCoos", team = "HeadCoos")
#'
#' }
#'
#' @export
#'
view_memberships <- function(
  state,
  n_max = 1000,
  ...
) {
  if (!is_missing_or_null(state)) {
    assert(
      is_scalar_character(state) || state %in% values$membership$state,
      "'state' must be one of '",
      str_c(values$membership$state, collapse = "', '"), "':\n  ", state
    )
  }
  else {
    state <- NULL
  }

  info("Viewing memberships for authenticated user")
  memberships_lst <- gh_url("user/memberships/orgs", state = state) %>%
    gh_page(n_max = n_max, ...)

  info("Transforming results", level = 4)
  memberships_gh <- bind_properties(memberships_lst, properties$memberships)

  info("Done", level = 7)
  memberships_gh
}


#  FUNCTION: view_membership ---------------------------------------------------
#
#' @rdname view_memberships
#' @export
#'
view_membership <- function(
  user,
  org,
  team,
  ...
) {
  assert(
    is_scalar_character(user),
    "'user' must be a string:\n  ", user
  )
  assert(
    is_scalar_character(org),
    "'org' must be a string:\n  ", org
  )

  if (is_missing_or_null(team)) {
    info("Viewing membership for '", user, "' in organization '", org, "'")
    membership_lst <- gh_url("orgs", org, "memberships", user) %>%
      gh_request("GET", ...)

    info("Transforming results", level = 4)
    membership_gh <- select_properties(membership_lst, properties$memberships)
  }
  else {
    if (is_scalar_character(team)) {
      team_id <- gh_url("orgs", org, "teams") %>%
        gh_find(property = "name", value = team, ...) %>%
        pluck("id")
    }
    assert(
      is_scalar_integerish(team_id),
      "'team' must be an integer or string:\n  ", team
    )

    info("Viewing membership for '", user, "' in team '", team, "'")
    membership_lst <- gh_url("teams", team_id, "memberships", user) %>%
      gh_request("GET", ...)

    info("Transforming results", level = 4)
    membership_gh <- select_properties(
      entity     = membership_lst,
      properties = properties$memberships
    ) %>%
      modify_list(user = user, organization = org) %>%
      modify_list(team = team, .after = "organization")
  }

  info("Done", level = 7)
  membership_gh
}


#  FUNCTION: delete_membership -------------------------------------------------
#
#' Remove a member from an organization or team.
#'
#' This function removes a user from an organization or team. Removing someone
#' from a team does not remove them from the organization, whereas removing them
#' from an organization also removes them from any teams within the
#' organization.
#'
#' Note: you can only remove a user if the authenticate user is an organization
#' "owner" or a team "maintainer".
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "orgs#remove-organization-membership-for-a-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#remove-team-membership-for-a-user",
#'   ">"
#' ))
#' ```
#'
#' @param user (string) The login of the user.
#' @param org (string) The login of the organization.
#' @param team (integer or string, optional) The ID or name of the team.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_membership()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   # Remove a user from an organization
#'   delete_membership("ChadGoymer", "HairyCoos")
#'
#'   # Remove a user from a team
#'   delete_membership("ChadGoymer", "HairyCoos", "HeadCoos")
#'
#' }
#'
#' @export
#'
delete_membership <- function(
  user,
  org,
  team,
  ...
) {
  assert(
    is_scalar_character(user),
    "'user' must be a string:\n  ", user
  )
  assert(
    is_scalar_character(org),
    "'org' must be a string:\n  ", org
  )

  if (is_missing_or_null(team)) {
    info("Deleting membership for '", user, "' in organization '", org, "'")
    response <- gh_url("orgs", org, "memberships", user) %>%
      gh_request("DELETE", ...)
  }
  else {
    if (is_scalar_character(team)) {
      team_id <- gh_url("orgs", org, "teams") %>%
        gh_find(property = "name", value = team, ...) %>%
        pluck("id")
    }
    assert(
      is_scalar_integerish(team_id),
      "'team' must be an integer or string:\n  ", team
    )

    info("Deleting membership for '", user, "' in team '", team, "'")
    response <- gh_url("teams", team_id, "memberships", user) %>%
      gh_request("DELETE", ...)
  }

  info("Done", level = 7)
  structure(
    TRUE,
    class   = c("github", "logical"),
    url     = attr(response, "url"),
    request = attr(response, "request"),
    status  = attr(response, "status"),
    header  = attr(response, "header")
  )
}
