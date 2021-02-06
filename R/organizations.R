#  FUNCTION: update_organization -----------------------------------------------
#
#' Update an organization's properties in GitHub
#'
#' This function updates an organization's properties in GitHub. You can only
#' update an organization if you have the appropriate permissions.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "orgs#update-an-organization",
#'   ">"
#' ))
#' ```
#'
#' @param org (string) The login of the organization.
#' @param name (string, optional) The shorthand name of the company.
#' @param description (string, optional) The description of the company.
#' @param email (string, optional) The publicly visible email address.
#' @param location (string, optional) The location.
#' @param company (string, optional) The company name.
#' @param billing_email (string, optional) Billing email address. This address
#'   is not publicized.
#' @param has_organization_projects (boolean, optional) Toggles whether an
#'   organization can use organization projects.
#' @param has_repository_projects (boolean, optional) Toggles whether
#'   repositories that belong to the organization can use repository projects.
#' @param default_repository_permission (string, optional) Default permission
#'   level members have for organization repositories:
#'   - `"read"`: can pull, but not push to or administer this repository.
#'   - `"write"`: can pull and push, but not administer this repository.
#'   - `"admin"`: can pull, push, and administer this repository.
#'   - `"none"`: no permissions granted by default.
#'
#'   Default: `"read"`.
#' @param members_can_create_repositories (boolean, optional) Toggles the
#'   ability of non-admin organization members to create repositories. Can be
#'   one of:
#'   - `TRUE`: all organization members can create repositories.
#'   - `FALSE`: only organization owners can create repositories.
#'
#'   Default: `TRUE`. Note: A parameter can override this parameter. See
#'   `members_allowed_repository_creation_type` for details.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_organization()` returns a list of the organization's
#'   properties.
#'
#' **Organization Properties:**
#'
#' - **id**: The ID of the organization.
#' - **login**: The login name of the organization.
#' - **name**: The name of the organization.
#' - **description**: The description of the organization.
#' - **company**: The name of the associated company.
#' - **blog**: The address for a blog.
#' - **location**: The geographical location.
#' - **email**: The email address for the organization.
#' - **is_verified**: Whether the organization has been verified.
#' - **has_organization_projects**: Whether the organization can have projects.
#' - **has_repository_projects**: Whether the organization's repositories can
#'   have projects.
#' - **public_repos**: The number of public repositories.
#' - **public_gists**: The number of public gists.
#' - **html_url**: The address for the organization's GitHub web page.
#' - **created_at**: When the organization was created.
#' - **total_private_repos**: The number of private repositories.
#' - **owned_private_repos**:  The number of owned private repositories.
#' - **private_gists**:  The number of private gists.
#' - **disk_usage**:  The total disk usage for the organization.
#' - **collaborators**: The number of collaborators.
#' - **billing_email**: The email address for billing.
#' - **plan_name**: The name of the GitHub plan.
#' - **plan_space**: The total space allocated for the plan.
#' - **plan_private_repos**: The number of private repositories for the plan.
#' - **default_repository_settings**: The default access for new repositories.
#' - **two_factor_requirement_enabled**: Whether members require two-factor
#'   authentication.
#' - **members_can_create_repositories**: Whether members can create
#'   repositories.
#'
#' @examples
#' \dontrun{
#'
#'   # Update some of your organization properties
#'   update_organization(
#'     org                             = "HairyCoos",
#'     description                     = "We are the Hairy Coos!",
#'     location                        = "The Highlands",
#'     default_repository_permission   = "write",
#'     members_can_create_repositories = FALSE
#'   )
#'
#' }
#'
#' @export
#'
update_organization <- function(
  org,
  name,
  description,
  email,
  location,
  company,
  billing_email,
  has_organization_projects,
  has_repository_projects,
  default_repository_permission,
  members_can_create_repositories,
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

  if (!is_missing_or_null(email)) {
    assert(
      is_scalar_character(email),
      "'email' must be a string:\n  ", email
    )
    payload$email <- email
  }

  if (!is_missing_or_null(location)) {
    assert(
      is_scalar_character(location),
      "'location' must be a string:\n  ", location
    )
    payload$location <- location
  }

  if (!is_missing_or_null(company)) {
    assert(
      is_scalar_character(company),
      "'company' must be a string:\n  ", company
    )
    payload$company <- company
  }

  if (!is_missing_or_null(billing_email)) {
    assert(
      is_scalar_character(billing_email),
      "'billing_email' must be a string:\n  ", billing_email
    )
    payload$billing_email <- billing_email
  }

  if (!is_missing_or_null(has_organization_projects)) {
    assert(
      is_scalar_logical(has_organization_projects),
      "'has_organization_projects' must be a boolean:\n  ",
      has_organization_projects
    )
    payload$has_organization_projects <- has_organization_projects
  }

  if (!is_missing_or_null(has_repository_projects)) {
    assert(
      is_scalar_logical(has_repository_projects),
      "'has_repository_projects' must be a boolean:\n  ",
      has_repository_projects
    )
    payload$has_repository_projects <- has_repository_projects
  }

  if (!is_missing_or_null(default_repository_permission)) {
    assert(
      is_scalar_character(default_repository_permission) &&
        default_repository_permission %in%
        values$organization$default_repository_permission,
      "'default_repository_permission' must be either '",
      str_c(
        values$organization$default_repository_permission,
        collapse = "', '"
      ),
      "':\n  ", default_repository_permission
    )
    payload$default_repository_permission <- default_repository_permission
  }

  if (!is_missing_or_null(members_can_create_repositories)) {
    assert(
      is_scalar_logical(members_can_create_repositories),
      "'members_can_create_repositories' must be a string:\n  ",
      members_can_create_repositories
    )
    payload$members_can_create_repositories <- members_can_create_repositories
  }

  info("Updating organization")
  org_lst <- gh_url("orgs", org) %>%
    gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  org_gh <- select_properties(org_lst, properties$organization)

  info("Done", level = 7)
  org_gh
}


#  FUNCTION: view_organizations ------------------------------------------------
#
#' View organizations in GitHub
#'
#' `view_organizations()` summarises organizations in a table with the
#' properties as columns and a row for each organization. `view_organization()`
#' returns a list of all properties for a single organization.
#' `browse_organization()` opens the web page for the organization in the
#' default browser.
#'
#' You can summarise all the organizations a user is a member of by specifying
#' the user login, or the authenticated user if it is set to `NULL`. If a user
#' is not supplied the first `n_max` organizations of GitHub are returned.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "orgs#list-organizations-for-a-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "orgs#list-organizations-for-the-authenticated-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "orgs#list-organizations",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "orgs#get-an-organization",
#'   ">"
#' ))
#' ```
#'
#' @param org (string) The login of the organization.
#' @param user (string, optional) The login of the user. If `NULL` the
#'   authenticated user is used.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_organizations()` returns a tibble of organization properties.
#'   `view_organization()` returns a list of properties for a single
#'   organization. `browse_organization()` opens the default browser on the
#'   organization's page and returns the URL.
#'
#' **Organization Properties:**
#'
#' - **id**: The ID of the organization.
#' - **login**: The login name of the organization.
#' - **description**: The description of the organization.
#'
#' The following are only returned using `view_organization()`:
#'
#' - **name**: The name of the organization.
#' - **company**: The name of the associated company.
#' - **blog**: The address for a blog.
#' - **location**: The geographical location.
#' - **email**: The email address for the organization.
#' - **is_verified**: Whether the organization has been verified.
#' - **has_organization_projects**: Whether the organization can have projects.
#' - **has_repository_projects**: Whether the organization's repositories can
#'   have projects.
#' - **public_repos**: The number of public repositories.
#' - **public_gists**: The number of public gists.
#' - **html_url**: The address for the organization's GitHub web page.
#' - **created_at**: When the organization was created.
#' - **total_private_repos**: The number of private repositories.
#' - **owned_private_repos**:  The number of owned private repositories.
#' - **private_gists**:  The number of private gists.
#' - **disk_usage**:  The total disk usage for the organization.
#' - **collaborators**: The number of collaborators.
#' - **billing_email**: The email address for billing.
#' - **plan_name**: The name of the GitHub plan.
#' - **plan_space**: The total space allocated for the plan.
#' - **plan_private_repos**: The number of private repositories for the plan.
#' - **default_repository_permission**: The default access for new repositories.
#' - **two_factor_requirement_enabled**: Whether members require two-factor
#'   authentication.
#' - **members_can_create_repositories**: Whether members can create
#'   repositories.
#'
#' @examples
#' \dontrun{
#'
#'   # View organizations a user is a member of
#'   view_organizations(user = "ChadGoymer")
#'
#'   # View organizations the authenticated user is a member of
#'   view_organizations(user = NULL)
#'
#'   # View all organizations
#'   view_organizations()
#'
#'   # View a single organization in more detail.
#'   view_organization("HairyCoos")
#'
#'   # Browse a organization's GitHub page
#'   browse_organization("HairyCoos")
#'
#' }
#'
#' @export
#'
view_organizations <- function(
  user,
  n_max = 1000,
  ...
) {
  if (!is_missing_or_null(user)) {
    if (is_null(user)) {
      info("Viewing organizations for authenticated user")
      url <- gh_url("user/orgs")
    }
    else {
      assert(
        is_scalar_character(user),
        "'user' must be a string:\n  ", user
      )

      info("Viewing organizations for user '", user, "'")
      url <- gh_url("users", user, "orgs")
    }
  }
  else {
    info("Viewing all organizations")
    url <- gh_url("organizations")
  }

  organizations_lst <- gh_page(url = url, n_max  = n_max, ...)

  info("Transforming results", level = 4)
  organizations_gh <- bind_properties(
    collection = organizations_lst,
    properties = properties$organizations
  )

  info("Done", level = 7)
  organizations_gh
}


#  FUNCTION: view_organization -------------------------------------------------
#
#' @rdname view_organizations
#' @export
#'
view_organization <- function(
  org,
  ...
) {
  assert(
    is_scalar_character(org),
    "'org' must be a string:\n  ", org
  )

  info("Viewing organization '", org, "'")
  org_lst <- gh_url("orgs", org) %>%
    gh_request("GET", ...)

  info("Transforming results", level = 4)
  org_gh <- select_properties(org_lst, properties$organization)

  info("Done", level = 7)
  org_gh
}


#  FUNCTION: browse_organization -----------------------------------------------
#
#' @rdname view_organizations
#' @export
#'
browse_organization <- function(
  org,
  ...
) {
  org <- view_organization(org, ...)

  info("Browsing organization '", org$login, "'")
  httr::BROWSE(org$html_url)

  info("Done", level = 7)
  structure(
    org$html_url,
    class   = c("github", "character"),
    url     = attr(org, "url"),
    request = attr(org, "request"),
    status  = attr(org, "status"),
    header  = attr(org, "header")
  )
}
