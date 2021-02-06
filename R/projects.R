#  FUNCTION: create_project ----------------------------------------------------
#
#' Create a GitHub project
#'
#' This function creates a new project in GitHub. The project will be empty so
#' you will need to add columns and cards separately.
#'
#' You can create a project associated with either a repository or organization,
#' by supplying them as an input, as long as you have appropriate permissions.
#' If no repository or organization is specified the project is created for the
#' authenticated user.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "projects#create-a-repository-project",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "projects#create-an-organization-project",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "projects#create-a-user-project",
#'   ">"
#' ))
#' ```
#'
#' @param name (string) The name of the project.
#' @param body (string) The description of the project.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_project()` returns a list of the project properties.
#'
#' **Project Properties:**
#'
#' - **id**: The ID of the project.
#' - **number**: The number of the project for the repository, user or
#'   organization.
#' - **name**: The name given to the project.
#' - **body**: The description given to the project.
#' - **state**: Whether the project is "open" or "closed".
#' - **private**: Whether the project is private (organisation project only).
#' - **org_permission**: The default permission for the project - either "read",
#'   "write" or "admin" (organisation project only).
#' - **creator**: The user who created the project.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#' - **html_url**: The URL to view the project.
#'
#' @examples
#' \dontrun{
#'
#'   # Create a project for a repository
#'   create_project(
#'     name = "Repo project",
#'     body = "This is a repository's project",
#'     repo = "ChadGoymer/githapi"
#'   )
#'
#'   # Create a project for the current user
#'   create_project(
#'     name = "User project",
#'     body = "This is a user's project"
#'   )
#'
#'   # Create a project for an organization
#'   create_project(
#'     name = "Organization project",
#'     body = "This is an organization's project",
#'     org  = "HairyCoos"
#'   )
#'
#' }
#'
#' @export
#'
create_project <- function(
  name,
  body,
  repo,
  org,
  ...
) {
  assert(
    is_scalar_character(name),
    "'name' must be a string:\n  ", name
  )
  assert(
    is_scalar_character(body),
    "'body' must be a string:\n  ", body
  )

  if (!is_missing_or_null(repo)) {
    assert(
      is_repo(repo),
      "'repo' must be a string in the format 'owner/repo':\n  ", repo
    )
    info("Creating project '", name, "' for repository '", repo, "'")
    url <- gh_url("repos", repo, "projects")
  }
  else if (!is_missing_or_null(org)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )
    info("Creating project '", name, "' for organization '", org, "'")
    url <- gh_url("orgs", org, "projects")
  }
  else {
    info("Creating project '", name, "' for current user")
    url <- gh_url("user/projects")
  }

  project_lst <- gh_request(
    url     = url,
    type    = "POST",
    payload = list(name = name, body = body),
    accept  = "application/vnd.github.inertia-preview+json",
    ...
  )

  info("Transforming results", level = 4)
  project_gh <- select_properties(project_lst, properties$project)

  if (!is_missing_or_null(org)) {
    project_gh <- project_gh %>%
      modify_list(
        private        = project_lst$private,
        org_permission = project_lst$organization_permission,
        .after         = "state"
      )
  }

  info("Done", level = 7)
  project_gh
}


#  FUNCTION: update_project ----------------------------------------------------
#
#' Update a GitHub project
#'
#' This function updates a project in GitHub. It can be used to change the name
#' and body, but can also be used to close the project, change permissions or
#' add a team.
#'
#' You can update a project associated with either a repository, user, team or
#' organization, by supplying them as an input, as long as you have appropriate
#' permissions. Supplying a team that does not already have access to the
#' project adds them. If they have access, then the team's permissions can be
#' changed with the `permission` argument.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "projects#update-a-project",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#add-or-update-team-project-permissions",
#'   ">"
#' ))
#' ```
#'
#' @param project (integer or string) Either the project number or name.
#' @param name (string, optional) The new name for the project.
#' @param body (string, optional) The new description of the project.
#' @param state (string, optional) The new state of the project, either `"open"`
#'   or `"closed"`.
#' @param permission (string, optional) The new team or organisation permissions
#'   for the project, either `"read"`, `"write"`, `"admin"` or `"none"`. Note:
#'   applies to team and organization projects only.
#' @param private (boolean, optional) Whether the project should be private.
#'   Note: applies to team and organization projects only.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param team (string or integer, optional) The team ID or name.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_project()` returns a list of the project properties.
#'
#' **Project Properties:**
#'
#' - **id**: The ID of the project.
#' - **number**: The number of the project for the repository, user or
#'   organization.
#' - **name**: The name given to the project.
#' - **body**: The description given to the project.
#' - **state**: Whether the project is "open" or "closed".
#' - **private**: Whether the project is private (organization and team projects
#'   only).
#' - **org_permissions**: The default permission for organization members
#'   (organization and team projects only).
#' - **team_permissions**: The default permission for team members (team
#'   projects only).
#' - **creator**: The user who created the project.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#' - **html_url**: The URL to view the project.
#'
#' @examples
#' \dontrun{
#'
#'   # Update the name of a project for a repository
#'   update_project(
#'     project = "Repo project",
#'     name    = "Updated repo project",
#'     body    = "This is an updated repository's project",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Update the state of a project for a user
#'   update_project(
#'     name  = "User project",
#'     state = "closed",
#'     user  = "ChadGoymer"
#'   )
#'
#'   # Update the permissions of a project for an organization
#'   update_project(
#'     name       = "Org project",
#'     permission = "read",
#'     private    = TRUE,
#'     org        = "HairyCoos"
#'   )
#'
#'   # Add a team to the project
#'   update_project(
#'     project = "Org project",
#'     team    = "HeadCoos",
#'     org     = "HairyCoos"
#'   )
#'
#'   # Update the team's permissions on the project
#'   update_project(
#'     project    = "Org project",
#'     permission = "write",
#'     team       = "HeadCoos",
#'     org        = "HairyCoos"
#'   )
#'
#' }
#'
#' @export
#'
update_project <- function(
  project,
  name,
  body,
  state,
  permission,
  private,
  repo,
  user,
  team,
  org,
  ...
) {
  project <- view_project(
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...
  )

  payload <- NULL

  if (!is_missing_or_null(team)) {
    if (!is_missing_or_null(permission)) {
      assert(
        is_scalar_character(permission) &&
          permission %in% values$project$permission,
        "'permission' must be one of '",
        str_c(values$project$permission, collapse = "', '"), "':\n  ",
        permission
      )
      payload$permission <- permission
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

    info("Adding project '", project$name, "' to team '", team, "'")
    result <- gh_url("teams", team_id, "projects", project$id) %>%
      gh_request(
        type    = "PUT",
        payload = payload,
        accept  = "application/vnd.github.inertia-preview+json",
        ...
      )

    project_gh <- view_project(
      project = project$name,
      team    = team,
      org     = org,
      ...
    )

    info("Done", level = 7)
    structure(
      project_gh,
      url     = attr(result, "url"),
      request = attr(result, "request"),
      status  = attr(result, "status"),
      header  = attr(result, "header")
    )
  }
  else {
    if (!is_missing_or_null(permission)) {
      assert(
        is_scalar_character(permission) &&
          permission %in% values$project$permission,
        "'permission' must be one of '",
        str_c(values$project$permission, collapse = "', '"), "':\n  ",
        permission
      )
      payload$organization_permission <- permission
    }

    if (!is_missing_or_null(name)) {
      assert(
        is_scalar_character(name),
        "'name' must be a string:\n  ", name
      )
      payload$name <- name
    }

    if (!is_missing_or_null(body)) {
      assert(
        is_scalar_character(body),
        "'body' must be a string:\n  ", body
      )
      payload$body <- body
    }

    if (!is_missing_or_null(state)) {
      assert(
        is_scalar_character(state) &&
          state %in% str_subset(values$project$state, "all", negate = TRUE),
        "'state' must be one of '",
        str_c(values$project$state, collapse = "', '"), "':\n  ", state
      )
      payload$state <- state
    }

    if (!is_missing_or_null(private)) {
      assert(
        is_scalar_logical(private),
        "'private' must be a boolean:\n  ", private
      )
      payload$private <- private
    }

    info("Updating project '", project$name, "'")
    project_lst <- gh_url("projects", project$id) %>%
      gh_request(
        type    = "PATCH",
        payload = payload,
        accept  = "application/vnd.github.inertia-preview+json",
        ...
      )

    info("Transforming results", level = 4)
    project_gh <- select_properties(project_lst, properties$project)

    if (!is_missing_or_null(org)) {
      project_gh <- project_gh %>%
        modify_list(
          private        = project_lst$private,
          org_permission = project_lst$organization_permission,
          .after         = "state"
        )
    }

    info("Done", level = 7)
    project_gh
  }
}


#  FUNCTION: view_projects -----------------------------------------------------
#
#' View GitHub projects
#'
#' `view_projects()` summarises projects in a table with the properties as
#' columns and a row for each project. `view_project()` returns a list of all
#' properties for a single project. `browse_project()` opens the web page for
#' the project in the default browser.
#'
#' You can summarise all the projects associated with either a repository, user,
#' team or organization, by supplying them as an input.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "projects#list-repository-projects",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "projects#list-user-projects",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "projects#list-organization-projects",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#check-team-permissions-for-a-project",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "projects#get-a-project",
#'   ">"
#' ))
#' ```
#'
#' @param project (integer or string) The number or name of the project.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param team (string or integer, optional) The team ID or name.
#' @param org (string, optional) The name of the organization.
#' @param state (string, optional) Indicates the state of the projects to
#'   return. Can be either "open", "closed", or "all". Default: `"open"`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_projects()` returns a tibble of project properties.
#'   `view_project()` returns a list of properties for a single project.
#'   `browse_project()` opens the default browser on the project's page and
#'   returns the URL.
#'
#' **Project Properties:**
#'
#' - **id**: The ID of the project.
#' - **number**: The number of the project for the repository, user or
#'   organization.
#' - **name**: The name given to the project.
#' - **body**: The description given to the project.
#' - **state**: Whether the project is "open" or "closed".
#' - **private**: Whether the project is private (organization and team projects
#'   only).
#' - **org_permissions**: The default permission for organization members
#'   (organization and team projects only).
#' - **team_permissions**: The default permission for team members (team
#'   projects only).
#' - **creator**: The user who created the project.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#' - **html_url**: The URL to view the project.
#'
#' @examples
#' \dontrun{
#'
#'   # View a repository's projects
#'   view_projects("ChadGoymer/githapi")
#'
#'   # View a user's projects
#'   view_projects(user = "ChadGoymer")
#'
#'   # View an organization's projects
#'   view_projects(org = "HairyCoos")
#'
#'   # View a team's projects
#'   view_projects(team = "HeadCoos", org = "HairyCoos")
#'
#'   # View closed projects
#'   view_projects("ChadGoymer/githapi", state = "closed")
#'
#'   # View all projects
#'   view_projects("ChadGoymer/githapi", state = "all")
#'
#'   # View a specific repository project
#'   view_project("Prioritisation", repo = "ChadGoymer/githapi")
#'
#'   # View a specific user project
#'   view_project("Test project", user = "ChadGoymer")
#'
#'   # View a specific organization project
#'   view_project("Prioritisation", org = "HairyCoos")
#'
#'   # View a specific team project
#'   view_project("Prioritisation", team = "HeadCoos", org = "HairyCoos")
#'
#'   # Browse a specific repository project
#'   browse_project("Prioritisation", "ChadGoymer/githapi")
#'
#'   # Browse a specific user project
#'   browse_project("Test project", user = "ChadGoymer")
#'
#'   # Browse a specific organization project
#'   browse_project("Prioritisation", org = "HairyCoos")
#'
#'   # Browse a specific team project
#'   browse_project("Prioritisation", team = "HeadCoos", org = "HairyCoos")
#'
#' }
#'
#' @export
#'
view_projects <- function(
  repo,
  user,
  team,
  org,
  state = "open",
  n_max = 1000,
  ...
) {
  assert(
    is_scalar_character(state) && state %in% values$project$state,
    "'state' must be one of '", str_c(values$project$state, collapse = "', '"),
    "':\n  ", state
  )

  if (!is_missing_or_null(repo)) {
    assert(
      is_repo(repo),
      "'repo' must be a string in the format 'owner/repo':\n  ", repo
    )
    info("Viewing projects for repository '", repo, "'")
    url <- gh_url("repos", repo, "projects", state = state)
  }
  else if (!is_missing_or_null(user)) {
    assert(
      is_scalar_character(user),
      "'user' must be a string:\n  ", user
    )
    info("Viewing projects for user '", user, "'")
    url <- gh_url("users", user, "projects", state = state)
  }
  else if (!is_missing_or_null(team)) {
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

    info("Viewing projects for team '", team, "'")
    url <- gh_url("teams", team_id, "projects", state = state)
  }
  else if (!is_missing_or_null(org)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )
    info("Viewing projects for organization '", org, "'")
    url <- gh_url("orgs", org, "projects", state = state)
  }
  else {
    error("Must specify either 'repo', 'user' or 'org'!")
  }

  projects_lst <- gh_page(
    url    = url,
    accept = "application/vnd.github.inertia-preview+json",
    n_max  = n_max,
    ...
  )

  info("Transforming results", level = 4)
  projects_gh <- bind_properties(projects_lst, properties$project)

  if (!is_missing_or_null(team)) {
    proj_order <- values$project$permission
    team_permission <- map_chr(projects_lst, function(p) {
      last(
        proj_order[
          proj_order %in% names(p$permissions[as.logical(p$permissions)])
        ]
      )
    })
    projects_gh <- add_column(
      projects_gh,
      team_permission = team_permission,
      .after          = "state"
    )
  }

  if (!is_missing_or_null(org)) {
    org_permission <- map_chr(projects_lst, pluck, "organization_permission")
    private <- map_lgl(projects_lst, pluck, "private")
    projects_gh <- projects_gh %>%
      add_column(org_permission = org_permission, .after = "state") %>%
      add_column(private = private, .after = "state")
  }

  info("Done", level = 7)
  projects_gh
}


#  FUNCTION: view_project ------------------------------------------------------
#
#' @rdname view_projects
#' @export
#'
view_project <- function(
  project,
  repo,
  user,
  team,
  org,
  ...
) {
  if (is_scalar_integerish(project)) {
    property <- "number"
  }
  else if (is_scalar_character(project)) {
    property <- "name"
  }
  else {
    error("'project' must be either an integer or a string:\n  ", project)
  }

  if (!is_missing_or_null(repo)) {
    assert(
      is_repo(repo),
      "'repo' must be a string in the format 'owner/repo':\n  ", repo
    )
    info("Viewing project '", project, "' for repository '", repo, "'")
    url <- gh_url("repos", repo, "projects", state = "all")
  }
  else if (!is_missing_or_null(user)) {
    assert(
      is_scalar_character(user),
      "'user' must be a string:\n  ", user
    )
    info("Viewing project '", project, "' for user '", user, "'")
    url <- gh_url("users", user, "projects", state = "all")
  }
  else if (!is_missing_or_null(team)) {
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

    info("Viewing project '", project, "' for team '", team, "'")
    url <- gh_url("teams", team_id, "projects", state = "all")
  }
  else if (!is_missing_or_null(org)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )
    info("Viewing project '", project, "' for organization '", org, "'")
    url <- gh_url("orgs", org, "projects", state = "all")
  }
  else {
    error("Must specify either 'repo', 'user' or 'org'!")
  }

  project_lst <- gh_find(
    url       = url,
    property  = property,
    value     = project,
    accept    = "application/vnd.github.inertia-preview+json",
    ...
  )

  info("Transforming results", level = 4)
  project_gh <- select_properties(project_lst, properties$project)

  if (!is_missing_or_null(team)) {
    proj_order <- values$project$permission
    permission <- last(
      proj_order[
        proj_order %in%
          names(project_lst$permissions[as.logical(project_lst$permissions)])
      ]
    )

    project_gh <- project_gh %>%
      modify_list(team_permission = permission, .after = "state")
  }

  if (!is_missing_or_null(org)) {
    project_gh <- project_gh %>%
      modify_list(
        private        = project_lst$private,
        org_permission = project_lst$organization_permission,
        .after         = "state"
      )
  }

  info("Done", level = 7)
  project_gh
}


#  FUNCTION: browse_project ----------------------------------------------------
#
#' @rdname view_projects
#' @export
#'
browse_project <- function(
  project,
  repo,
  user,
  team,
  org,
  ...
) {
  project <- view_project(
    project = project,
    repo    = repo,
    user    = user,
    team    = team,
    org     = org,
    ...
  )

  info("Browsing project '", project$name, "'")
  httr::BROWSE(project$html_url)

  info("Done", level = 7)
  structure(
    project$html_url,
    class   = c("github", "character"),
    url     = attr(project, "url"),
    request = attr(project, "request"),
    status  = attr(project, "status"),
    header  = attr(project, "header")
  )
}


#  FUNCTION: delete_project ----------------------------------------------------
#
#' Delete a GitHub project
#'
#' This function deletes a project in GitHub. Care should be taken as it will
#' not be recoverable. If you just want to close the project use
#' [update_project()].
#'
#' You can delete a project associated with either a repository, user, team or
#' organization, by supplying them as an input, as long as you have appropriate
#' permissions. Deleting a team project just removes the team's access. If you
#' want to delete it completely you have to delete the organization's project.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "projects#delete-a-project",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#remove-a-project-from-a-team",
#'   ">"
#' ))
#' ```
#'
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param team (string or integer, optional) The team ID or name.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_project()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   # Delete a project for a repository
#'   delete_project(
#'     project = "Repo project",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Delete a project for a user
#'   delete_project(
#'     project = "User project",
#'     user    = "ChadGoymer"
#'   )
#'
#'   # Remove a team's access to the organization's project
#'   delete_project(
#'     project = "User project",
#'     team    = "HeadCoos",
#'     org     = "HairyCoos"
#'   )
#'
#'   # Delete a project for an organization
#'   delete_project(
#'     project = "User project",
#'     org     = "HairyCoos"
#'   )
#'
#' }
#'
#' @export
#'
delete_project <- function(
  project,
  repo,
  user,
  team,
  org,
  ...
) {
  project <- view_project(
    project = project,
    repo    = repo,
    user    = user,
    team    = team,
    org     = org,
    ...
  )

  if (is_missing_or_null(team)) {
    info("Deleting project '", project$name, "'")
    url <- gh_url("projects", project$id)
  }
  else {
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

    info("Removing project '", project$name, "' from team '", team, "'")
    url <- gh_url("teams", team_id, "projects", project$id)
  }

  result <- gh_request(
    url    = url,
    type   = "DELETE",
    accept = "application/vnd.github.inertia-preview+json",
    ...
  )

  info("Done", level = 7)
  structure(
    TRUE,
    class   = c("github", "logical"),
    url     = attr(result, "url"),
    request = attr(result, "request"),
    status  = attr(result, "status"),
    header  = attr(result, "header")
  )
}
