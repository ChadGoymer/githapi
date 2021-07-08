#  FUNCTION: create_repository -------------------------------------------------
#
#' Create a repository for a user or organization
#'
#' This function creates a new repository for the authenticated user or an
#' organization in GitHub. It can also be used to specify whether the project is
#' private or has issues, projects or a wiki and can define the allowed
#' behaviour when merging pull requests.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#create-a-repository-for-the-authenticated-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#create-an-organization-repository",
#'   ">"
#' ))
#' ```
#'
#' @param name (string) The name of the repository.
#' @param org (string, optional) The name of the organization.
#' @param description (string, optional) A short description of the repository.
#' @param homepage (string, optional) A URL with more information about the
#'   repository.
#' @param private (boolean, optional) Whether the repository is private or
#'   public. Default: `FALSE`.
#' @param has_issues (boolean, optional) Whether to enable issues for the
#'   repository. Default: `TRUE`.
#' @param has_projects (boolean, optional) Whether to enable projects for the
#'   repository. Default: `TRUE`.
#' @param has_wiki (boolean, optional) Whether to enable the wiki for the
#'   repository. Default: `TRUE`.
#' @param auto_init (boolean, optional) Whether to create an initial commit with
#'   empty README. Default: `FALSE`.
#' @param allow_squash_merge (boolean, optional) Whether to allow squash-merging
#'   pull requests. Default: `TRUE`.
#' @param allow_merge_commit (boolean, optional) Whether to allow merging pull
#'   requests with a merge commit. Default: `TRUE`.
#' @param allow_rebase_merge (boolean, optional) Whether to allow rebase-merging
#'   pull requests. Default: `TRUE`.
#' @param delete_branch_on_merge (boolean, optional) Whether to allow
#'   automatically deleting branches when pull requests are merged. Default:
#'   `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_repository()` returns a list of the repository's properties.
#'
#' **Repository Properties:**
#'
#' - **id**: The ID of the repository.
#' - **name**: The name of the repository.
#' - **full_name**: The full name of the repository, in the format:
#'   `owner/repo`.
#' - **description**: The description of the repository.
#' - **owner**: The owner of the repository.
#' - **html_url**: The address of the repository's web page in GitHub.
#' - **homepage**: The homepage for the repository.
#' - **language**: The dominant programming language in the repository.
#' - **size**: The overall size of the repository in bytes.
#' - **default_branch**: The name of the default branch.
#' - **permission**: The permission the authenticated user has.
#' - **private**: Whether the repository is private.
#' - **has_issues**: Whether the repository has issues.
#' - **has_projects**: Whether the repository has projects.
#' - **has_wiki**: Whether the repository has a wiki.
#' - **has_pages**: Whether the repository has GitHub Pages.
#' - **has_downloads**: Whether the repository has downloads.
#' - **allow_squash_merge**: Whether the repository allows squash-merging pull
#'   requests.
#' - **allow_merge_commit**: Whether the repository allows merging pull requests
#'   with a merge commit.
#' - **allow_rebase_merge**: Whether the repository allows rebase-merging pull
#'   requests.
#' - **fork**: Whether the repository is a fork of another.
#' - **archived**: Whether the repository has been archived.
#' - **disabled**: Whether the repository has been disabled.
#' - **watchers_count**: The number of watchers.
#' - **stargazers_count**: The number of stars.
#' - **forks_count**: The number of forks.
#' - **pushed_at**: When the repository was last pushed to.
#' - **created_at**: When the repository was created.
#' - **updated_at**: When the repository was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   create_repository(
#'     name        = "user-repository",
#'     description = "This is a user repository",
#'     homepage    = "https://user-repository.com"
#'   )
#'
#'   create_repository(
#'     name        = "org-repository",
#'     org         = "HairyCoos",
#'     description = "This is a organization repository",
#'     homepage    = "https://org-repository.com"
#'   )
#'
#' }
#'
#' @export
#'
create_repository <- function(
  name,
  org,
  description,
  homepage,
  private                = FALSE,
  has_issues             = TRUE,
  has_projects           = TRUE,
  has_wiki               = TRUE,
  auto_init              = FALSE,
  allow_squash_merge     = TRUE,
  allow_merge_commit     = TRUE,
  allow_rebase_merge     = TRUE,
  delete_branch_on_merge = FALSE,
  env                    = NULL,
  ...
) {
  assert(
    is_scalar_character(name),
    "'name' must be a string:\n  ", name
  )
  assert(
    is_scalar_logical(private),
    "'private' must be a boolean:\n  ", private
  )
  assert(
    is_scalar_logical(has_issues),
    "'has_issues' must be a boolean:\n  ", has_issues
  )
  assert(
    is_scalar_logical(has_projects),
    "'has_projects' must be a boolean:\n  ", has_projects
  )
  assert(
    is_scalar_logical(has_wiki),
    "'has_wiki' must be a boolean:\n  ", has_wiki
  )
  assert(
    is_scalar_logical(auto_init),
    "'auto_init' must be a boolean:\n  ", auto_init
  )
  assert(
    is_scalar_logical(allow_squash_merge),
    "'allow_squash_merge' must be a boolean:\n  ", allow_squash_merge
  )
  assert(
    is_scalar_logical(allow_merge_commit),
    "'allow_merge_commit' must be a boolean:\n  ", allow_merge_commit
  )
  assert(
    is_scalar_logical(allow_rebase_merge),
    "'allow_rebase_merge' must be a boolean:\n  ", allow_rebase_merge
  )
  assert(
    is_scalar_logical(delete_branch_on_merge),
    "'delete_branch_on_merge' must be a boolean:\n  ", delete_branch_on_merge
  )

  payload <- list(
    name                   = name,
    private                = private,
    has_issues             = has_issues,
    has_projects           = has_projects,
    has_wiki               = has_wiki,
    auto_init              = auto_init,
    allow_squash_merge     = allow_squash_merge,
    allow_merge_commit     = allow_merge_commit,
    allow_rebase_merge     = allow_rebase_merge,
    delete_branch_on_merge = delete_branch_on_merge
  )

  if (!is_missing_or_null(description)) {
    assert(
      is_scalar_character(description),
      "'description' must be a string:\n  ", description
    )
    payload$description <- description
  }

  if (!is_missing_or_null(homepage)) {
    assert(
      is_scalar_character(homepage),
      "'description' must be a string:\n  ", homepage
    )
    payload$homepage <- homepage
  }

  if (!is_missing_or_null(org)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )
    info("Creating repository '", name, "' for organization '", org, "'")
    url <- gh_url("orgs", org, "repos", env = env)
  }
  else {
    info("Creating repository '", name, "' for authenticated user")
    url <- gh_url("user/repos", env = env)
  }

  repo_lst <- gh_request("POST", url = url, payload = payload, env = env, ...)

  perm_order <- values$repository$team_permission
  permission <- last(
    perm_order[
      perm_order %in%
        names(repo_lst$permissions[as.logical(repo_lst$permissions)])
    ]
  )

  info("Transforming results", level = 4)
  repo_gh <- select_properties(repo_lst, properties$repository) %>%
    modify_list(permission = permission, .after = "default_branch")

  info("Done", level = 7)
  repo_gh
}


#  FUNCTION: update_repository -------------------------------------------------
#
#' Update a user or organization repository
#'
#' This function updates a repository for the authenticated user or organization
#' in GitHub. It can be used to change whether the project is private or has
#' issues, projects or a wiki, it can redefine the allowed behaviour when
#' merging pull requests or add or update team permissions.
#'
#' The team's permission can be set to:
#' - `"pull"`: Team members can pull from this repository.
#' - `"push"`: Team members can pull from and push to this repository.
#' - `"admin"`: Team members can pull from, push to and administer this
#'   repository.
#' - `"maintain"`: Team members can manage the repository without access to
#'   sensitive or destructive actions. Recommended for project managers. Only
#'   applies to repositories owned by organizations.
#' - `"triage"`: Team members can proactively manage issues and pull requests
#'   without write access. Recommended for contributors who triage a repository.
#'   Only applies to repositories owned by organizations.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#update-a-repository",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#add-or-update-team-repository-permissions",
#'   ">"
#' ))
#' ```
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param name (string, optional) The name of the repository.
#' @param description (string, optional) A short description of the repository.
#' @param homepage (string, optional) A URL with more information about the
#'   repository.
#' @param private (boolean, optional) Whether the repository is private or
#'   public.
#' @param has_issues (boolean, optional) Whether to enable issues for the
#'   repository.
#' @param has_projects (boolean, optional) Whether to enable projects for the
#'   repository.
#' @param has_wiki (boolean, optional) Whether to enable the wiki for the
#'   repository.
#' @param default_branch (string, optional) The name of the default branch.
#' @param allow_squash_merge (boolean, optional) Whether to allow squash-merging
#'   pull requests.
#' @param allow_merge_commit (boolean, optional) Whether to allow merging pull
#'   requests with a merge commit.
#' @param allow_rebase_merge (boolean, optional) Whether to allow rebase-merging
#'   pull requests.
#' @param delete_branch_on_merge (boolean, optional) Whether to allow
#'   automatically deleting branches when pull requests are merged.
#' @param archived (boolean, optional) Whether to archive the repository.
#' @param team (string) The team name.
#' @param permission (string, optional) The permission to set for the team.
#'   Either: `"pull"``, `"push"`, `"admin"`, `"maintain"` or `"triage"`.
#'   Default: `"pull"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_repository()` returns a list of the repository properties.
#'
#' **Repository Properties:**
#'
#' - **id**: The ID of the repository.
#' - **name**: The name of the repository.
#' - **full_name**: The full name of the repository, in the format:
#'   `owner/repo`.
#' - **description**: The description of the repository.
#' - **owner**: The owner of the repository.
#' - **html_url**: The address of the repository's web page in GitHub.
#' - **homepage**: The homepage for the repository.
#' - **language**: The dominant programming language in the repository.
#' - **size**: The overall size of the repository in bytes.
#' - **default_branch**: The name of the default branch.
#' - **permission**: The permission the authenticated user has.
#' - **private**: Whether the repository is private.
#' - **has_issues**: Whether the repository has issues.
#' - **has_projects**: Whether the repository has projects.
#' - **has_wiki**: Whether the repository has a wiki.
#' - **has_pages**: Whether the repository has GitHub Pages.
#' - **has_downloads**: Whether the repository has downloads.
#' - **allow_squash_merge**: Whether the repository allows squash-merging pull
#'   requests.
#' - **allow_merge_commit**: Whether the repository allows merging pull requests
#'   with a merge commit.
#' - **allow_rebase_merge**: Whether the repository allows rebase-merging pull
#'   requests.
#' - **fork**: Whether the repository is a fork of another.
#' - **archived**: Whether the repository has been archived.
#' - **disabled**: Whether the repository has been disabled.
#' - **watchers_count**: The number of watchers.
#' - **stargazers_count**: The number of stars.
#' - **forks_count**: The number of forks.
#' - **pushed_at**: When the repository was last pushed to.
#' - **created_at**: When the repository was created.
#' - **updated_at**: When the repository was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # Update a repository
#'   update_repository(
#'     repo           = "ChadGoymer/user-repository",
#'     name           = "updated-user-repository",
#'     description    = "This is an updated user repository",
#'     homepage       = "https://updated-user-repository.com",
#'     has_issues     = FALSE,
#'     has_projects   = FALSE,
#'     has_wiki       = FALSE,
#'     default_branch = "main"
#'   )
#'
#'   # Update an organization's repository
#'   update_repository(
#'     repo                   = "HairyCoos/org-repository",
#'     name                   = "updated-org-repository",
#'     description            = "This is an updated organization repository",
#'     homepage               = "https://updated-org-repository.com",
#'     private                = FALSE,
#'     allow_squash_merge     = FALSE,
#'     allow_merge_commit     = FALSE,
#'     allow_rebase_merge     = TRUE,
#'     delete_branch_on_merge = TRUE
#'   )
#'
#'   # Archive a repository
#'   update_repository("HairyCoos/org-repository", archived = TRUE)
#'
#'   # Add read access for the specified team
#'   update_repository(
#'     repo = "HairyCoos/test-repository",
#'     team = "test-team"
#'   )
#'
#'   # Update team's permission to "maintain"
#'   update_repository(
#'     repo       = "HairyCoos/test-repository",
#'     team       = "test-team",
#'     permission = "maintain"
#'   )
#'
#' }
#'
#' @export
#'
update_repository <- function(
  repo,
  name,
  description,
  homepage,
  private,
  has_issues,
  has_projects,
  has_wiki,
  default_branch,
  allow_squash_merge,
  allow_merge_commit,
  allow_rebase_merge,
  delete_branch_on_merge,
  archived,
  team,
  permission = "pull",
  env        = NULL,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  payload <- list()

  if (!is_missing_or_null(team)) {
    assert(
      is_scalar_character(team),
      "'team' must be a string:\n  ", team
    )
    org <- dirname(repo)

    team_slug <- gh_url("orgs", org, "teams", env = env) %>%
      gh_find(property = "name", value = team, env = env, ...) %>%
      pluck("slug")

    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )

    assert(
      is_scalar_character(permission) &&
        permission %in% values$repository$team_permission,
      "'permission' for repositories must be either '",
      str_c(values$repository$team_permission, collapse = "', '"), "':\n  ",
      permission
    )
    payload$permission <- permission

    info("Updating permissions for team '", team, "' on repo '", repo, "'")
    gh_url("orgs", org, "teams", team_slug, "repos", repo, env = env) %>%
      gh_request("PUT", payload = payload, env = env, ...)

    gh_json <- "application/vnd.github.v3.repository+json"
    repo_lst <- gh_url("orgs", org, "teams", team_slug, "repos", repo, env = env) %>%
      gh_request("GET", accept = gh_json, env = env, ...)
  }
  else {
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

    if (!is_missing_or_null(homepage)) {
      assert(
        is_scalar_character(homepage),
        "'description' must be a string:\n  ", homepage
      )
      payload$homepage <- homepage
    }

    if (!is_missing_or_null(private)) {
      assert(
        is_scalar_logical(private),
        "'private' must be a boolean:\n  ", private
      )
      payload$private <- private
    }

    if (!is_missing_or_null(has_issues)) {
      assert(
        is_scalar_logical(has_issues),
        "'has_issues' must be a boolean:\n  ", has_issues
      )
      payload$has_issues <- has_issues
    }

    if (!is_missing_or_null(has_projects)) {
      assert(
        is_scalar_logical(has_projects),
        "'has_projects' must be a boolean:\n  ", has_projects
      )
      payload$has_projects <- has_projects
    }

    if (!is_missing_or_null(has_wiki)) {
      assert(
        is_scalar_logical(has_wiki),
        "'has_wiki' must be a boolean:\n  ", has_wiki
      )
      payload$has_wiki <- has_wiki
    }

    if (!is_missing_or_null(default_branch)) {
      assert(
        is_scalar_character(default_branch),
        "'default_branch' must be a string:\n  ", default_branch
      )
      payload$default_branch <- default_branch
    }

    if (!is_missing_or_null(allow_squash_merge)) {
      assert(
        is_scalar_logical(allow_squash_merge),
        "'allow_squash_merge' must be a boolean:\n  ", allow_squash_merge
      )
      payload$allow_squash_merge <- allow_squash_merge
    }

    if (!is_missing_or_null(allow_merge_commit)) {
      assert(
        is_scalar_logical(allow_merge_commit),
        "'allow_merge_commit' must be a boolean:\n  ", allow_merge_commit
      )
      payload$allow_merge_commit <- allow_merge_commit
    }

    if (!is_missing_or_null(allow_rebase_merge)) {
      assert(
        is_scalar_logical(allow_rebase_merge),
        "'allow_rebase_merge' must be a boolean:\n  ", allow_rebase_merge
      )
      payload$allow_rebase_merge <- allow_rebase_merge
    }

    if (!is_missing_or_null(delete_branch_on_merge)) {
      assert(
        is_scalar_logical(delete_branch_on_merge),
        "'delete_branch_on_merge' must be a boolean:\n  ",
        delete_branch_on_merge
      )
      payload$delete_branch_on_merge <- delete_branch_on_merge
    }

    if (!is_missing_or_null(archived)) {
      assert(
        is_scalar_logical(archived),
        "'archived' must be a boolean:\n  ", archived
      )
      payload$archived <- archived
    }

    info("Updating repository '", repo, "'")
    repo_lst <- gh_url("repos", repo, env = env) %>%
      gh_request("PATCH", payload = payload, env = env, ...)
  }

  perm_order <- values$repository$team_permission
  permission <- last(
    perm_order[
      perm_order %in%
        names(repo_lst$permissions[as.logical(repo_lst$permissions)])
    ]
  )

  info("Transforming results", level = 4)
  repo_gh <- select_properties(repo_lst, properties$repository) %>%
    modify_list(permission = permission, .after = "default_branch")

  info("Done", level = 7)
  repo_gh
}


#  FUNCTION: view_repositories -------------------------------------------------
#
#' View repositories for a user, team or organization
#'
#' `view_repositories()` summarises the repositories for a user, team or
#' organization in a table with the properties as columns and a row for each
#' repository. `view_repository()` returns a list of a single repository's
#' properties. `browse_repository()` opens the web page for the repository in
#' the default browser.
#'
#' You can summarise all the repositories associated with either a user, team or
#' organization, by supplying them as an input. If neither a user or
#' organization is specified a summary of the authenticated user's repositories
#' is returned.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#list-repositories-for-the-authenticated-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#list-repositories-for-a-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#list-organization-repositories",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#list-team-repositories",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#get-a-repository",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "teams#check-team-permissions-for-a-repository",
#'   ">"
#' ))
#' ```
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param team (string, optional) The team name.
#' @param org (string, optional) The name of the organization.
#' @param sort (string, optional) The property to order the returned
#'   repositories by. Can be either `"created"`, `"updated"`, `"pushed"` or
#'   `"full_name"`. Default: `"created"`.
#' @param direction (string, optional) The direction of the sort. Can be either
#'   `"asc"` or `"desc"`. Default: `"desc"`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_repositories()` returns a tibble of repository properties.
#'   `view_repository()` returns a list of properties for a single repository.
#'   `browse_repository()` opens the default browser on the repository's page
#'   and returns the URL.
#'
#' **Repository Properties:**
#'
#' - **id**: The ID of the repository.
#' - **name**: The name of the repository.
#' - **full_name**: The full name of the repository, in the format:
#'   `owner/repo`.
#' - **description**: The description of the repository.
#' - **owner**: The owner of the repository.
#' - **html_url**: The address of the repository's web page in GitHub.
#' - **homepage**: The homepage for the repository.
#' - **language**: The dominant programming language in the repository.
#' - **size**: The overall size of the repository in bytes.
#' - **default_branch**: The name of the default branch.
#' - **permission**: The permission the authenticated user or team has.
#' - **private**: Whether the repository is private.
#' - **has_issues**: Whether the repository has issues.
#' - **has_projects**: Whether the repository has projects.
#' - **has_wiki**: Whether the repository has a wiki.
#' - **has_pages**: Whether the repository has GitHub Pages.
#' - **has_downloads**: Whether the repository has downloads.
#' - **allow_squash_merge**: Whether the repository allows squash-merging pull
#'   requests.
#' - **allow_merge_commit**: Whether the repository allows merging pull requests
#'   with a merge commit.
#' - **allow_rebase_merge**: Whether the repository allows rebase-merging pull
#'   requests.
#' - **fork**: Whether the repository is a fork of another.
#' - **archived**: Whether the repository has been archived.
#' - **disabled**: Whether the repository has been disabled.
#' - **watchers_count**: The number of watchers.
#' - **stargazers_count**: The number of stars.
#' - **forks_count**: The number of forks.
#' - **pushed_at**: When the repository was last pushed to.
#' - **created_at**: When the repository was created.
#' - **updated_at**: When the repository was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # View a user's repositories
#'   view_repositories(user = "ChadGoymer")
#'
#'   # View an organization's repositories
#'   view_repositories(org = "HairyCoos")
#'
#'   # View a team's repositories
#'   view_repositories(team = "Test Team", org = "HairyCoos")
#'
#'   # Reorder a user's repositories
#'   view_repositories(
#'     user      = "ChadGoymer",
#'     sort      = "full_name",
#'     direction = "asc"
#'   )
#'
#'   # View a specific user repository
#'   view_repository("Test repo", user = "ChadGoymer")
#'
#'   # View a specific organization repository
#'   view_repository("Test repo", org = "HairyCoos")
#'
#'   # View a specific team repository
#'   view_repository("Test repo", team = "Test Team", org = "HairyCoos")
#'
#'   # Browse a specific user repository
#'   browse_repository("Test repo", user = "ChadGoymer")
#'
#'   # Browse a specific organization repository
#'   browse_repository("Test repo", org = "HairyCoos")
#'
#' }
#'
#' @export
#'
view_repositories <- function(
  user,
  team,
  org,
  sort      = "created",
  direction = "desc",
  n_max     = 1000,
  env       = NULL,
  ...
) {
  assert(
    is_scalar_character(sort) && sort %in% values$repository$sort,
    "'sort' must be either '",
    str_c(values$repository$sort, collapse = "', '"), "':\n  ", sort
  )
  assert(
    is_scalar_character(direction) &&
      direction %in% values$repository$direction,
    "'direction' must be either '",
    str_c(values$repository$direction, collapse = "', '"), "':\n  ", direction
  )

  if (!is_missing_or_null(user)) {
    assert(
      is_scalar_character(user),
      "'user' must be a string:\n  ", user
    )
    info("Viewing repositories for user '", user, "'")
    url <- gh_url(
      "users", user, "repos",
      type      = "all",
      sort      = sort,
      direction = direction,
      env       = env
    )
  }
  else if (!is_missing_or_null(org)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )

    if (is_missing_or_null(team)) {
      info("Viewing repositories for organization '", org, "'")
      url <- gh_url(
        "orgs", org, "repos",
        type      = "all",
        sort      = sort,
        direction = direction,
        env       = env
      )
    }
    else {
      assert(
        is_scalar_character(team),
        "'team' must be a string:\n  ", team
      )
      team_slug <- gh_url("orgs", org, "teams", env = env) %>%
        gh_find(property = "name", value = team, env = env, ...) %>%
        pluck("slug")

      info("Viewing repositories for team '", team, "' in org '", org, "'")
      url <- gh_url(
        "orgs", org, "teams", team_slug, "repos",
        type      = "all",
        sort      = sort,
        direction = direction,
        env       = env
      )
    }
  }
  else {
    info("Viewing repositories for authenticated user")
    url <- gh_url(
      "user/repos",
      type      = "all",
      sort      = sort,
      direction = direction,
      env       = env
    )
  }

  repositories_lst <- gh_page(url = url, n_max = n_max, env = env, ...)

  info("Transforming results", level = 4)
  repositories_gh <- bind_properties(repositories_lst, properties$repository)

  perm_order <- values$repository$team_permission
  permission <- map_chr(repositories_lst, function(r) {
    last(
      perm_order[
        perm_order %in% names(r$permissions[as.logical(r$permissions)])
      ]
    )
  })
  repositories_gh <- repositories_gh %>%
    add_column(permission = permission, .after = "default_branch")

  info("Done", level = 7)
  repositories_gh
}


#  FUNCTION: view_repository ---------------------------------------------------
#
#' @rdname view_repositories
#' @export
#'
view_repository <- function(
  repo,
  team,
  org,
  env = NULL,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  if (is_missing_or_null(team)) {
    info("Viewing repository '", repo, "'")
    repo_lst <- gh_url("repos", repo, env = env) %>%
      gh_request("GET", env = env, ...)
  }
  else {
    assert(
      is_scalar_character(team),
      "'team' must be a string:\n  ", team
    )
    team_slug <- gh_url("orgs", org, "teams", env = env) %>%
      gh_find(property = "name", value = team, env = env, ...) %>%
      pluck("slug")

    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )

    info("Viewing repository '", repo, "' for team '", team, "'")
    gh_json <- "application/vnd.github.v3.repository+json"
    repo_lst <- gh_url("orgs", org, "teams", team_slug, "repos", repo, env = env) %>%
      gh_request("GET", accept = gh_json, env = env, ...)
  }

  perm_order <- values$repository$team_permission
  permission <- last(
    perm_order[
      perm_order %in%
        names(repo_lst$permissions[as.logical(repo_lst$permissions)])
    ]
  )

  info("Transforming results", level = 4)
  repo_gh <- select_properties(repo_lst, properties$repository) %>%
    modify_list(permission = permission, .after = "default_branch")

  info("Done", level = 7)
  repo_gh
}


#  FUNCTION: browse_repository -------------------------------------------------
#
#' @rdname view_repositories
#' @export
#'
browse_repository <- function(
  repo,
  env = NULL,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Browsing repository '", repo, "'")
  repo <- gh_url("repos", repo, env = env) %>%
    gh_request("GET", env = env, ...)
  httr::BROWSE(repo$html_url)

  info("Done", level = 7)
  structure(
    repo$html_url,
    class   = c("github", "character"),
    url     = attr(repo, "url"),
    request = attr(repo, "request"),
    status  = attr(repo, "status"),
    header  = attr(repo, "header")
  )
}


#  FUNCTION: delete_repository -------------------------------------------------
#
#' Delete a user or organization repository
#'
#' This function deletes a repository from GitHub, as long as you have
#' appropriate permissions. Care should be taken as it will not be recoverable.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#delete-a-repository",
#'   ">"
#' ))
#' ```
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_repository()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   # Delete a user's repository
#'   delete_repository("ChadGoymer/user-repository")
#'
#'   # Delete an organization's repository
#'   delete_repository("HairyCoos/org-repository")
#'
#' }
#'
#' @export
#'
delete_repository <- function(
  repo,
  env = NULL,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Deleting repository '", repo, "'")
  response <- gh_url("repos", repo, env = env) %>%
    gh_request("DELETE", env = env, ...)

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
