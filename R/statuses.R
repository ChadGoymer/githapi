#  FUNCTION: create_status -----------------------------------------------------
#
#' Create a status for a commit
#'
#' This function creates a new status for a commit in the specified repository
#' in GitHub. The status can also have a `description` and a target URL,
#' allowing a user to link to more information. The `context` allows you to
#' differentiate the status from others. The status can then be updated, from
#' `pending` to `success` say, by using the same `context`.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#create-a-commit-status",
#'   ">"
#' ))
#' ```
#'
#' @param state (string) The state of the status. Can be one of `"error"`,
#'   `"failure"`, `"pending"`, or `"success"`.
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param description (string, optional) A short description of the status.
#' @param target_url (string, optional) The target URL to associate with this
#'   status. This URL will be linked from the GitHub UI to allow users to easily
#'   see the source of the status.
#' @param context (string, optional) A string label to differentiate this status
#'   from the status of other systems. Default: `"default"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_status()` returns a list of the status's properties.
#'
#' **Status Properties:**
#'
#' - **id**: The id of the status.
#' - **state**: The state of the status.
#' - **description**: The description of the status.
#' - **target_url**: The URL linked to the status.
#' - **context**: The context of the status.
#' - **creator**: The creator of the status.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # Create a status on the main branch
#'   create_status(
#'     state       = "pending",
#'     ref         = "main",
#'     repo        = "ChadGoymer/test-githapi",
#'     description = "This is a pending status",
#'     target_url  = "https://goymer.me/githapi",
#'     context     = "test/githapi"
#'   )
#'
#'   # Update the status on the main branch
#'   create_status(
#'     state       = "success",
#'     ref         = "main",
#'     repo        = "ChadGoymer/test-githapi",
#'     description = "This is a success status",
#'     target_url  = "https://goymer.me/githapi",
#'     context     = "test/githapi"
#'   )
#'
#' }
#'
#' @export
#'
create_status <- function(
  state,
  ref,
  repo,
  description,
  target_url,
  context = "default",
  ...
) {
  assert(
    is_scalar_character(state) && state %in% values$status$state,
    "'state' must be one of '",
    str_c(values$status$state, collapse = "', '"), "':\n  ", state
  )
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )
  assert(
    is_scalar_character(context),
    "'context' must be a string:\n  ", context
  )

  payload <- list(state = state, context = context)

  if (!is_missing_or_null(description)) {
    assert(
      is_scalar_character(description),
      "'description' must be a string:\n  ", description
    )
    payload$description <- description
  }

  if (!is_missing_or_null(target_url)) {
    assert(
      is_scalar_character(target_url),
      "'target_url' must be a string:\n  ", target_url
    )
    payload$target_url <- target_url
  }

  if (!is_sha(ref)) {
    ref <- view_sha(ref = ref, repo = repo, ...)
  }

  info("Creating status '", state, "' for ref '", ref, "' in repo '", repo, "'")
  status_lst <- gh_url("repos", repo, "statuses", ref) %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  status_gh <- select_properties(status_lst, properties$status)

  info("Done", level = 7)
  status_gh
}


#  FUNCTION: view_statuses -----------------------------------------------------
#
#' View statuses of a commit within a repository
#'
#' `view_statuses()` summarises statuses in a table with the properties as
#' columns and a row for each status for the specified commit in the repository.
#' `view_status()` returns the combined state of all statuses for the commit.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#list-commit-statuses-for-a-reference",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#get-the-combined-status-for-a-specific-reference",
#'   ">"
#' ))
#' ```
#'
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_statuses()` returns a tibble of status properties.
#'   `view_status()` returns a string for combined status.
#'
#' **Status Properties:**
#'
#' - **id**: The id of the status.
#' - **state**: The state of the status.
#' - **description**: The description of the status.
#' - **target_url**: The URL linked to the status.
#' - **context**: The context of the status.
#' - **creator**: The creator of the status.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # View all statuses for a commit in a repository
#'   view_statuses("main", "ChadGoymer/githapi")
#'
#'   # View the combined status
#'   view_status("main", "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
view_statuses <- function(
  ref,
  repo,
  n_max = 1000,
  ...
) {
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Viewing statuses for repository '", repo, "'")
  statuses_lst <- gh_url("repos", repo, "commits", ref, "statuses") %>%
    gh_page(n_max = n_max, ...)

  info("Transforming results", level = 4)
  statuses_gh <- bind_properties(statuses_lst, properties$status)

  info("Done", level = 7)
  statuses_gh
}


#  FUNCTION: view_status -------------------------------------------------------
#
#' @rdname view_statuses
#' @export
#'
view_status <- function(
  ref,
  repo,
  ...
) {
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Viewing statuses for repository '", repo, "'")
  status <- gh_url("repos", repo, "commits", ref, "status") %>%
    gh_request("GET", ...)

  info("Done", level = 7)
  structure(
    status$state,
    class   = c("github", "character"),
    url     = attr(status, "url"),
    request = attr(status, "request"),
    status  = attr(status, "status"),
    header  = attr(status, "header")
  )
}
