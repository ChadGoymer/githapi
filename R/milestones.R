#  FUNCTION: create_milestone --------------------------------------------------
#
#' Create a milestone in a repository
#'
#' This function creates a new milestone for the specified repository in GitHub.
#' It can also be used to set a due date.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#create-a-milestone",
#'   ">"
#' ))
#' ```
#'
#' @param title (string) The title of the milestone.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param description (string, optional) A description of the milestone.
#' @param due_on (string, optional) The milestone due date. This is in the
#'   format: `YYYY-MM-DD`.
#' @param state (string, optional) The state of the milestone. Either `"open"`
#'   or `"closed"`. Default: `"open"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_milestone()` returns a list of the milestone's properties.
#'
#' **Milestone Properties:**
#'
#' - **number**: The number assigned to the milestone within the repository.
#' - **title**: The title of the milestone.
#' - **description**: The description of the milestone.
#' - **state**: The state of the milestone - either "open" or "closed".
#' - **open_issues**: The number of open issues within the milestone.
#' - **closed_issues**: The number of closed issues within the milestone.
#' - **html_url**: The URL of the milestone's web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the milestone was created.
#' - **updated_at**: When the milestone was last updated.
#' - **due_on**: When the milestone is due.
#' - **closed_at**: When the milestone was closed.
#'
#' @examples
#' \dontrun{
#'
#'   create_milestone(
#'     title       = "test milestone",
#'     repo        = "ChadGoymer/githapi",
#'     description = "This is a test milestone",
#'     due_on      = "2030-01-01 00:00:00"
#'   )
#'
#' }
#'
#' @export
#'
create_milestone <- function(
  title,
  repo,
  description,
  due_on,
  state = "open",
  ...
) {
  assert(
    is_scalar_character(title),
    "'title' must be a string:\n  ", title
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )
  assert(
    is_scalar_character(state) && state %in% values$milestone$state,
    "'state' for milestones must be either '",
    str_c(values$milestone$state, collapse = "', '"), "':\n  ", state
  )

  payload <- list(title = title, state = state)

  if (!is_missing_or_null(description)) {
    assert(
      is_scalar_character(description),
      "'description' must be a string:\n  ", description
    )
    payload$description <- description
  }

  if (!is_missing_or_null(due_on)) {
    assert(
      is_scalar_character(due_on),
      "'due_on' must be a string:\n  ", due_on
    )
    due_on <- (as.POSIXct(due_on, format = "%Y-%m-%d") + 12 * 60 * 60) %>%
      format("%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    assert(
      !is.na(due_on),
      "'due_on' must be specified in the format 'YYYY-MM-DD':\n  ", due_on
    )
    payload$due_on <- due_on
  }

  info("Creating milestone '", title, "' for repository '", repo, "'")
  milestone_lst <- gh_url("repos", repo, "milestones") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  milestone_gh <- select_properties(milestone_lst, properties$milestone)

  info("Done", level = 7)
  milestone_gh
}


#  FUNCTION: update_milestone --------------------------------------------------
#
#' Update a milestone in a repository
#'
#' This function updates a milestone for the specified repository in GitHub. It
#' can be used to change title, description or due date, but can also be used to
#' close the milestone.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#update-a-milestone",
#'   ">"
#' ))
#' ```
#'
#' @param milestone (string or character) The number or title of the milestone.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param title (string, optional) The title of the milestone.
#' @param description (string, optional) A description of the milestone.
#' @param due_on (string, optional) The milestone due date. This is in the
#'   format: `YYYY-MM-DD HH:MM:SS`.
#' @param state (string, optional) The state of the milestone. Either `"open"`
#'   or `"closed"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_milestone()` returns a list of the milestone properties.
#'
#' **Milestone Properties:**
#'
#' - **number**: The number assigned to the milestone within the repository.
#' - **title**: The title of the milestone.
#' - **description**: The description of the milestone.
#' - **state**: The state of the milestone - either "open" or "closed".
#' - **open_issues**: The number of open issues within the milestone.
#' - **closed_issues**: The number of closed issues within the milestone.
#' - **html_url**: The URL of the milestone's web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the milestone was created.
#' - **updated_at**: When the milestone was last updated.
#' - **due_on**: When the milestone is due.
#' - **closed_at**: When the milestone was closed.
#'
#' @examples
#' \dontrun{
#'
#'   # Update the properties of a milestone
#'   update_milestone(
#'     milestone   = "test milestone",
#'     repo        = "ChadGoymer/githapi",
#'     title       = "updated test milestone",
#'     description = "This is an updated test milestone",
#'     due_on      = "2020-12-01"
#'   )
#'
#'   # Close a milestone
#'   update_milestone(
#'     milestone = "updated test milestone",
#'     repo      = "ChadGoymer/githapi",
#'     state     = "closed"
#'   )
#'
#' }
#'
#' @export
#'
update_milestone <- function(
  milestone,
  repo,
  title,
  description,
  due_on,
  state,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  payload <- list()

  if (!is_missing_or_null(title)) {
    assert(
      is_scalar_character(title),
      "'title' must be a string:\n  ", title
    )
    payload$title <- title
  }

  if (!is_missing_or_null(description)) {
    assert(
      is_scalar_character(description),
      "'description' must be a string:\n  ", description
    )
    payload$description <- description
  }

  if (!is_missing_or_null(due_on)) {
    assert(
      is_scalar_character(due_on),
      "'due_on' must be a string:\n  ", due_on
    )
    due_on <- (as.POSIXct(due_on, format = "%Y-%m-%d") + 12 * 60 * 60) %>%
      format("%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    assert(
      !is.na(due_on),
      "'due_on' must be specified in the format 'YYYY-MM-DD':\n  ", due_on
    )
    payload$due_on <- due_on
  }

  if (!is_missing_or_null(state)) {
    assert(
      is_scalar_character(state) && state %in% values$milestone$state,
      "'state' for milestones must be either '",
      str_c(values$milestone$state, collapse = "', '"), "':\n  ", state
    )
    payload$state <- state
  }

  milestone <- view_milestone(milestone, repo = repo, ...)

  info("Updating milestone '", milestone$title, "' in repository '", repo, "'")
  milestone_lst <- gh_url("repos", repo, "milestones", milestone$number) %>%
    gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  milestone_gh <- select_properties(milestone_lst, properties$milestone)

  info("Done", level = 7)
  milestone_gh
}


#  FUNCTION: view_milestones ---------------------------------------------------
#
#' View milestones within a repository
#'
#' `view_milestones()` summarises milestones in a table with the properties as
#' columns and a row for each milestone in the repository. `view_milestone()`
#' returns a list of all properties for a single milestone. `browse_milestone()`
#' opens the web page for the milestone in the default browser.
#'
#' You can summarise all the milestones of a repository in a specified `state`
#' and change the order they are returned using `sort` and `direction`.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#list-milestones",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#get-a-milestone",
#'   ">"
#' ))
#' ```
#'
#' @param milestone (string or character) The number or title of the milestone.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param state (string, optional) The state of the milestones to filter the
#'   result. Can be either `"open"`, `"closed"` or `"all"`. Default: `"open"`.
#' @param sort (string, optional) The property to order the returned milestones
#'   by. Can be either `"due_on"` or `"completeness"`. Default: `"due_on"`.
#' @param direction (string, optional) The direction of the sort. Can be either
#'   `"asc"` or `"desc"`. Default: `"asc"`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_milestones()` returns a tibble of milestone properties.
#'   `view_milestone()` returns a list of properties for a single milestone.
#'   `browse_milestone()` opens the default browser on the milestone's page and
#'   returns the URL.
#'
#' **Milestone Properties:**
#'
#' - **number**: The number assigned to the milestone within the repository.
#' - **title**: The title of the milestone.
#' - **description**: The description of the milestone.
#' - **state**: The state of the milestone - either "open" or "closed".
#' - **open_issues**: The number of open issues within the milestone.
#' - **closed_issues**: The number of closed issues within the milestone.
#' - **html_url**: The URL of the milestone's web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the milestone was created.
#' - **updated_at**: When the milestone was last updated.
#' - **due_on**: When the milestone is due.
#' - **closed_at**: When the milestone was closed.
#'
#' @examples
#' \dontrun{
#'
#'   # View open milestones in a repository
#'   view_milestones("ChadGoymer/githapi")
#'
#'   # View closed milestones in a repository
#'   view_milestones("ChadGoymer/githapi", state = "closed")
#'
#'   # View a single milestone
#'   view_milestone(
#'     milestone = "test milestone",
#'     repo      = "ChadGoymer/githapi"
#'   )
#'
#'   # Open a milestone's page in a browser
#'   browse_milestone(
#'     milestone = "test milestone",
#'     repo      = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
view_milestones <- function(
  repo,
  state     = "open",
  sort      = "due_on",
  direction = "asc",
  n_max     = 1000,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  assert(
    is_scalar_character(state) && state %in% values$milestone$state,
    "'state' must be either '",
    str_c(values$milestone$state, collapse = "', '"), "':\n  ", state
  )
  assert(
    is_scalar_character(sort) && sort %in% values$milestone$sort,
    "'sort' must be either '",
    str_c(values$milestone$sort, collapse = "', '"), "':\n  ", sort
  )
  assert(
    is_scalar_character(direction) && direction %in% values$milestone$direction,
    "'direction' must be either '",
    str_c(values$milestone$direction, collapse = "', '"), "':\n  ", direction
  )

  info("Viewing milestones for repository '", repo, "'")
  milestones_lst <- gh_url(
    "repos", repo, "milestones",
    state     = state,
    sort      = sort,
    direction = direction
  ) %>%
    gh_page(n_max = n_max, ...)

  info("Transforming results", level = 4)
  milestones_gh <- bind_properties(milestones_lst, properties$milestone)

  info("Done", level = 7)
  milestones_gh
}


#  FUNCTION: view_milestone ----------------------------------------------------
#
#' @rdname view_milestones
#' @export
#'
view_milestone <- function(
  milestone,
  repo,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  if (is_scalar_integerish(milestone)) {
    info("Viewing milestone '", milestone, "' for repository '", repo, "'")
    milestone_lst <- gh_url("repos", repo, "milestones", milestone) %>%
      gh_request("GET", ...)
  }
  else if (is_scalar_character(milestone)) {
    info("Viewing milestone '", milestone, "' for repository '", repo, "'")
    milestone_lst <- gh_url("repos", repo, "milestones") %>%
      gh_find(property  = "title", value = milestone, ...)
  }
  else {
    error("'milestone' must be either an integer or a string:\n  ", milestone)
  }

  info("Transforming results", level = 4)
  milestone_gh <- select_properties(milestone_lst, properties$milestone)

  info("Done", level = 7)
  milestone_gh
}


#  FUNCTION: browse_milestone --------------------------------------------------
#
#' @rdname view_milestones
#' @export
#'
browse_milestone <- function(
  milestone,
  repo,
  ...
) {
  milestone <- view_milestone(milestone = milestone, repo = repo, ...)

  info("Browsing milestone '", milestone$title, "' in repository '", repo, "'")
  httr::BROWSE(milestone$html_url)

  info("Done", level = 7)
  structure(
    milestone$html_url,
    class   = c("github", "character"),
    url     = attr(milestone, "url"),
    request = attr(milestone, "request"),
    status  = attr(milestone, "status"),
    header  = attr(milestone, "header")
  )
}


#  FUNCTION: delete_milestone --------------------------------------------------
#
#' Delete a milestone from a repository
#'
#' This function deletes a milestone from a repository, as long as you have
#' appropriate permissions. Care should be taken as it will not be recoverable.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#delete-a-milestone",
#'   ">"
#' ))
#' ```
#'
#' @param milestone (string or character) The number or title of the milestone.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_milestone()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   # Delete a milestone
#'   delete_milestone(
#'     milestone = "test milestone",
#'     repo      = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
delete_milestone <- function(
  milestone,
  repo,
  ...
) {
  milestone <- view_milestone(milestone = milestone, repo = repo, ...)

  info("Deleting milestone '", milestone$title, "' in repository '", repo, "'")
  response <- gh_url("repos", repo, "milestones", milestone$number) %>%
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
