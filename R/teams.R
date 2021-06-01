#  FUNCTION: create_team -------------------------------------------------------
#
#' Create a team in a GitHub organization
#'
#' This function creates a new team in the specified organization in GitHub. It
#' can also be used to specify the maintainers, the repositories to add the team
#' to and a parent team.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#create-a-team",
#'   ">"
#' ))
#' ```
#'
#' @param name (string) The name of the team.
#' @param org (string) The login of the organization.
#' @param description (string, optional) The description of the team.
#' @param maintainers (character, optional) The logins of organization members
#'   to add as maintainers of the team. If you do not specify any maintainers,
#'   then you will automatically become a team maintainer when you create a new
#'   team.
#' @param repo_names (character, optional) The full name (e.g.
#'   "organization-name/repository-name") of repositories to add the team to.
#' @param privacy (string, optional) The level of privacy this team should have.
#'   The options are:
#'
#'   For a non-nested team:
#'   - `"secret"`: only visible to organization owners and members of this team.
#'   - `"closed"`: visible to all members of this organization.
#'
#'   Default: `"secret"`.
#'
#'   For a parent or child team:
#'   - `"closed"`: visible to all members of this organization.
#'
#'   Default for child team: `"closed"`
#' @param parent_team (integer or string, optional) The ID or name of a team to
#'   set as the parent team.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_team()` returns a list of the team's properties.
#'
#' **Team Properties:**
#'
#' - **id**: The ID of the team.
#' - **name**: The name of the team.
#' - **organization**: The organization the team is associated with.
#' - **slug**: The team slug name.
#' - **description**: The description of the team.
#' - **privacy**: The privacy setting of the team - either "closed" or "secret".
#' - **permission**: The default repository permissions of the team.
#' - **parent**: The parent team.
#' - **members_count**: The number of members.
#' - **repos_count**: The number of repositories the team has access to.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#' - **html_url**: The URL of the team page in GitHub
#'
#' @examples
#' \dontrun{
#'
#'   # Create a team in an organization
#'   create_team("TestTeam", "HairyCoos")
#'
#'   # Create a team and specify maintainers
#'   create_team(
#'     name        = "TestTeam2",
#'     org         = "HairyCoos",
#'     maintainers = c("ChadGoymer", "chris-walker1")
#'   )
#'
#'   # Create a team and set the privacy
#'   create_team("TestTeam3", "HairyCoos", privacy = "closed")
#'
#'   # Create a team and specify a parent team
#'   create_team("TestTeam4", "HairyCoos", parent_team = "TestTeam3")
#'
#' }
#'
#' @export
#'
create_team <- function(
  name,
  org,
  description,
  maintainers,
  repo_names,
  privacy,
  parent_team,
  ...
) {
  assert(
    is_scalar_character(name),
    "'name' must be a string:\n  ", name
  )
  assert(
    is_scalar_character(org),
    "'org' must be a string:\n  ", org
  )

  payload <- list(name = name)

  if (!is_missing_or_null(description)) {
    assert(
      is_scalar_character(description),
      "'description' must be a string:\n  ", description
    )
    payload$description <- description
  }

  if (!is_missing_or_null(maintainers)) {
    assert(
      is_character(maintainers),
      "'maintainers' must be a character vector:\n  ", maintainers
    )
    payload$maintainers <- as.list(maintainers)
  }

  if (!is_missing_or_null(repo_names)) {
    assert(
      is_character(repo_names),
      "'repo_names' must be a character vector:\n  ", repo_names
    )
    payload$repo_names <- as.list(repo_names)
  }

  if (!is_missing_or_null(privacy)) {
    assert(
      is_scalar_character(privacy) && privacy %in% values$team$privacy,
      "'privacy' must be one of '",
      str_c(values$team$privacy, collapse = "', '"), "':\n  ", privacy
    )
    payload$privacy <- privacy
  }

  if (!is_missing_or_null(parent_team)) {
    parent_team_id <- parent_team
    if (is_scalar_character(parent_team)) {
      parent_team_id <- gh_url("orgs", org, "teams") %>%
        gh_find(property = "name", value = parent_team, ...) %>%
        pluck("id")
    }
    assert(
      is_scalar_integerish(parent_team_id),
      "'parent_team' must be an integer or string:\n  ", parent_team
    )
    payload$parent_team_id <- parent_team_id
  }

  team_lst <- gh_url("orgs", org, "teams") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  team_gh <- select_properties(team_lst, properties$team)

  info("Done", level = 7)
  team_gh
}


#  FUNCTION: update_team -------------------------------------------------------
#
#' Update a team's properties in GitHub
#'
#' This function updates a team's properties in GitHub. You can only update a
#' team if you have the appropriate permissions.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#update-a-team",
#'   ">"
#' ))
#' ```
#'
#' @param team (integer or string) The Id or current name of the team.
#' @param name (string, optional) The new name of the team.
#' @param description (string, optional) The description of the team.
#' @param privacy (string, optional) The level of privacy this team should have.
#'   Editing teams without specifying this parameter leaves privacy intact. The
#'   options are:
#'
#'   For a non-nested team:
#'   - `"secret"`: only visible to organization owners and members of this team.
#'   - `"closed"`: visible to all members of this organization.
#'
#'   For a parent or child team:
#'   - `"closed"`: visible to all members of this organization.
#' @param parent_team (integer or string, optional) The ID or name of a team to
#'   set as the parent team.
#' @param org (string, optional) The organization the team is associated with.
#'   Not required if your are specifying `team` and/or `parent_team` set by ID.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_team()` returns a list of the team's properties.
#'
#' **Team Properties:**
#'
#' - **id**: The ID of the team.
#' - **name**: The name of the team.
#' - **organization**: The organization the team is associated with.
#' - **slug**: The team slug name.
#' - **description**: The description of the team.
#' - **privacy**: The privacy setting of the team - either "closed" or "secret".
#' - **permission**: The default repository permissions of the team.
#' - **parent**: The parent team.
#' - **members_count**: The number of members.
#' - **repos_count**: The number of repositories the team has access to.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#' - **html_url**: The URL of the team page in GitHub
#'
#' @examples
#' \dontrun{
#'
#'   # Update a team
#'   update_team(
#'     team        = "TestTeam",
#'     description = "This is a test team",
#'     privacy     = "closed",
#'     parent_team = "TestTeam3",
#'     org         = "HairyCoos"
#'   )
#'
#' }
#'
#' @export
#'
update_team <- function(
  team,
  name,
  description,
  privacy,
  parent_team,
  org,
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

  if (!is_missing_or_null(description)) {
    assert(
      is_scalar_character(description),
      "'description' must be a string:\n  ", description
    )
    payload$description <- description
  }

  if (!is_missing_or_null(privacy)) {
    assert(
      is_scalar_character(privacy) && privacy %in% values$team$privacy,
      "'privacy' must be one of '",
      str_c(values$team$privacy, collapse = "', '"), "':\n  ", privacy
    )
    payload$privacy <- privacy
  }

  if (!is_missing_or_null(parent_team)) {
    parent_team_id <- parent_team
    if (is_scalar_character(parent_team)) {
      assert(
        is_scalar_character(org),
        "'org' must be a string:\n  ", org
      )
      parent_team_id <- gh_url("orgs", org, "teams") %>%
        gh_find(property = "name", value = parent_team, ...) %>%
        pluck("id")
    }
    assert(
      is_scalar_integerish(parent_team_id),
      "'parent_team' must be an integer or string:\n  ", parent_team
    )
    payload$parent_team_id <- parent_team_id
  }

  team_id <- team
  if (is_scalar_character(team)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )
    team_id <- gh_url("orgs", org, "teams") %>%
      gh_find(property = "name", value = team, ...) %>%
      pluck("id")
  }
  assert(
    is_scalar_integerish(team_id),
    "'team' must be an integer or string:\n  ", team
  )

  team_lst <- gh_url("teams", team_id) %>%
    gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  team_gh <- select_properties(team_lst, properties$team)

  info("Done", level = 7)
  team_gh
}


#  FUNCTION: view_teams --------------------------------------------------------
#
#' View teams in an organization in GitHub
#'
#' `view_teams()` summarises teams in a table with the properties as columns and
#' a row for each team. `view_team()` returns a list of all properties for a
#' single team. `browse_team()` opens the web page for the team in the default
#' browser.
#'
#' You can summarise all the teams within an organisation, that are children to
#' a parent team or, if neither are supplied, the teams the authenticated user
#' is a member of.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#list-teams-for-the-authenticated-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#list-teams",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#list-child-teams",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#get-a-team-by-name",
#'   ">"
#' ))
#' ```
#'
#' @param team (integer or string) The ID or name of the team.
#' @param org (string, optional) The login of the organization.
#' @param parent_team (integer or string, optional) The ID or name of a team. If
#'   supplied, all the child teams will be returned.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_teams()` returns a tibble of team properties. `view_team()`
#'   returns a list of properties for a single team. `browse_team()` opens the
#'   default browser on the team's page and returns the URL.
#'
#' **Team Properties:**
#'
#' - **id**: The ID of the team.
#' - **name**: The name of the team.
#' - **slug**: The team slug name.
#' - **description**: The description of the team.
#' - **privacy**: The privacy setting of the team - either "closed" or "secret".
#' - **permission**: The default repository permissions of the team.
#' - **parent**: The parent team.
#' - **html_url**: The URL of the team page in GitHub
#'
#' The following are only returned using `view_team()`:
#'
#' - **organization**: The organization the team is associated with.
#' - **members_count**: The number of members.
#' - **repos_count**: The number of repositories the team has access to.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # View teams authenticated user is a member of
#'   view_teams()
#'
#'   # View teams in an organization
#'   view_teams("HairyCoos")
#'
#'   # View a single team
#'   view_team("HeadCoos", "HairyCoos")
#'
#'   # Browse a team's GitHub page
#'   browse_team("HeadCoos", "HairyCoos")
#'
#' }
#'
#' @export
#'
view_teams <- function(
  org,
  parent_team,
  n_max = 1000,
  ...
) {
  if (!is_missing_or_null(parent_team)) {
    parent_team_id <- parent_team
    if (is_scalar_character(parent_team)) {
      assert(
        is_scalar_character(org),
        "'org' must be a string:\n  ", org
      )
      parent_team_id <- gh_url("orgs", org, "teams") %>%
        gh_find(property = "name", value = parent_team, ...) %>%
        pluck("id")
    }

    assert(
      is_scalar_integerish(parent_team_id),
      "'parent_team' must be an integer or string:\n  ", parent_team
    )
    info("Viewing child teams of the team '", parent_team, "'")
    url <- gh_url("teams", parent_team_id, "teams")
  }
  else if (!is_missing_or_null(org)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )
    info("Viewing teams in organization '", org, "'")
    url <- gh_url("orgs", org, "teams")
  }
  else {
    info("Viewing teams of authenticated user")
    url <- gh_url("user/teams")
  }

  teams_lst <- gh_page(url = url, n_max  = n_max, ...)

  info("Transforming results", level = 4)
  teams_gh <- bind_properties(teams_lst, properties$teams)

  info("Done", level = 7)
  teams_gh
}


#  FUNCTION: view_team ---------------------------------------------------------
#
#' @rdname view_teams
#' @export
#'
view_team <- function(
  team,
  org,
  ...
) {
  team_id <- team
  if (is_scalar_character(team)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )
    team_id <- gh_url("orgs", org, "teams") %>%
      gh_find(property = "name", value = team, ...) %>%
      pluck("id")
  }
  assert(
    is_scalar_integerish(team_id),
    "'team' must be an integer or string:\n  ", team
  )

  info("Viewing team '", team, "'")
  team_lst <- gh_url("teams", team_id) %>%
    gh_request("GET", ...)

  info("Transforming results", level = 4)
  team_gh <- select_properties(team_lst, properties$team)

  info("Done", level = 7)
  team_gh
}


#  FUNCTION: browse_team -------------------------------------------------------
#
#' @rdname view_teams
#' @export
#'
browse_team <- function(
  team,
  org,
  ...
) {
  if (is_scalar_character(team)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )
    team <- gh_url("orgs", org, "teams") %>%
      gh_find(property = "name", value = team, ...)
  }
  else if (is_scalar_integerish(team)) {
    team <- gh_url("teams", team) %>%
      gh_request("GET", ...)
  }
  else {
    error("'team' must be an integer or string:\n  ", team)
  }

  info("Browsing team '", team$id, "'")
  httr::BROWSE(team$html_url)

  info("Done", level = 7)
  structure(
    team$html_url,
    class   = c("github", "character"),
    url     = attr(team, "url"),
    request = attr(team, "request"),
    status  = attr(team, "status"),
    header  = attr(team, "header")
  )
}


#  FUNCTION: delete_team -------------------------------------------------------
#
#' Delete a team in GitHub
#'
#' This function deletes a team from an organization in GitHub, as long as you
#' have appropriate permissions. Care should be taken as it will not be
#' recoverable.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#delete-a-team",
#'   ">"
#' ))
#' ```
#'
#' @param team (integer or string) The ID or name of the team.
#' @param org (string) The login of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_team()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   # Delete a team
#'   delete_team("HeadCoos", "HairyCoos")
#'
#' }
#'
#' @export
#'
delete_team <- function(
  team,
  org,
  ...
) {
  team_id <- team
  if (is_scalar_character(team)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )
    team_id <- gh_url("orgs", org, "teams") %>%
      gh_find(property = "name", value = team, ...) %>%
      pluck("id")
  }
  assert(
    is_scalar_integerish(team_id),
    "'team' must be an integer or string:\n  ", team
  )

  info("Deleting team '", team, "'")
  response <- gh_url("teams", team_id) %>%
    gh_request("DELETE", ...)

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
