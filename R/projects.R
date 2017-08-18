#  FUNCTION: gh_project -----------------------------------------------------------------------
#' Get a project
#'
#' url{https://developer.github.com/v3/projects/#get-a-project}
#'
#' @param project (integer) The ID of the project in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A list describing the project (see GitHub's API documentation for details).
#' @export
gh_project <- function(
  project,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(project))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_url("projects", project, api = api) %>%
    gh_json(token = token, accept = "application/vnd.github.inertia-preview+json", ...)
}

#  FUNCTION: gh_projects ----------------------------------------------------------------------
#' List organisation or repository projects
#'
#' url{https://developer.github.com/v3/projects/#list-repository-projects}
#' url{https://developer.github.com/v3/projects/#list-organization-projects}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param org (string) The name of the organization.
#' @param state (string, optional) Indicates the state of the projects to return. Can be either open,
#'   closed, or all. Default: open
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the projects (see GitHub's API documentation for details).
#' @export
gh_projects <- function(
  repo,
  org,
  state = NULL,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.null(state) || is.string(state))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (!missing(repo) && !missing(org))
    stop("Must specify either repo or org, not both!")

  if (!missing(repo)) {
    assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
    url <- gh_url("repos", repo, "projects", state = state, api = api)
  } else if (!missing(org)) {
    assert_that(is.string(org))
    url <- gh_url("orgs", org, "projects", state = state, api = api)
  } else {
    stop("Must specify either repo or org!")
  }

  # NOTE: Projects is currently in beta, so requires preview accept header
  url %>%
    gh_tibble(token = token, accept = "application/vnd.github.inertia-preview+json", ...) %>%
    select(id, number, name, body, state, creator_login, created_at, updated_at, url) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

#  FUNCTION: gh_columns -----------------------------------------------------------------------
#' List project columns
#'
#' url{https://developer.github.com/v3/projects/columns/#list-project-columns}
#'
#' @param project (integer) The ID of the project in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the columns (see GitHub's API documentation for details).
#' @export
gh_columns <- function(
  project,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(project))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_url("projects", project, "columns", api = api) %>%
    gh_tibble(token = token, accept = "application/vnd.github.inertia-preview+json", ...) %>%
    select(id, name, created_at, updated_at, url) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

#  FUNCTION: gh_cards -------------------------------------------------------------------------
#' List project cards
#'
#' url{https://developer.github.com/v3/projects/cards/#list-project-cards}
#'
#' @param column (integer) The ID of the column in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the cards (see GitHub's API documentation for details).
#' @export
gh_cards <- function(
  column,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(column))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_url("projects/columns", column, "cards", api = api) %>%
    gh_tibble(token = token, accept = "application/vnd.github.inertia-preview+json", ...) %>%
    select(id, creator_login, created_at, updated_at, url) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}