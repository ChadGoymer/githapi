#  FUNCTION: create_issue ------------------------------------------------------
#
#' Create an issue in a repository
#'
#' This function creates a new issue for the specified repository in GitHub. It
#' can also be used to assign the issue to a user and add labels or a milestone.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#create-an-issue",
#'   ">"
#' ))
#' ```
#'
#' @param title (string) The title of the issue.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param body (string, optional) The contents of the issue.
#' @param assignees (character, optional) Logins for Users to assign to this
#'   issue. NOTE: Only users with push access can set assignees for new issues.
#' @param labels (character, optional) Labels to associate with this issue.
#'   NOTE: Only users with push access can set labels for new issues.
#' @param milestone (character or integer, optional) The title or number of the
#'   milestone to associate this issue with. NOTE: Only users with push access
#'   can set the milestone for new issues.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_issue()` returns a list of the issue's properties.
#'
#' **Issue Properties:**
#'
#' - **number**: The number assigned to the issue.
#' - **title**: The title of the issue.
#' - **body**: The body contents of the issue.
#' - **assignees**: The users assigned to the issue.
#' - **labels**: The labels attached to the issue.
#' - **milestone**: The milestone assigned to the issue.
#' - **state**: The state of the issue - either `"open"` or `"closed"`.
#' - **repository**: The repository the issue is in.
#' - **pull_request**: Whether the issue is a pull request.
#' - **html_url**: The URL of the issue's web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the issue was created.
#' - **updated_at**: When the issue was last updated.
#' - **closed_at**: When the issue was closed.
#'
#' @examples
#' \dontrun{
#'
#'   create_issue(
#'     title     = "user issue",
#'     repo      = "ChadGoymer/test-issues",
#'     body      = "This is an issue to test create_issue()",
#'     assignees = "ChadGoymer",
#'     labels    = "feature",
#'     milestone = "release-1.0"
#'   )
#'
#'   create_issue(
#'     title     = "organization issue",
#'     repo      = "HairyCoos/test-issues",
#'     body      = "This is an issue to test create_issue()",
#'     assignees = "ChadGoymer"
#'   )
#'
#' }
#'
#' @export
#'
create_issue <- function(
  title,
  repo,
  body,
  assignees,
  labels,
  milestone,
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

  payload <- list(title = title)

  if (!is_missing_or_null(body)) {
    assert(
      is_scalar_character(body),
      "'body' must be a string:\n  ", body
    )
    payload$body <- body
  }

  if (!is_missing_or_null(assignees)) {
    assert(
      is_character(assignees),
      "'assignees' must be a character vector:\n  ", assignees
    )
    payload$assignees <- as.list(assignees)
  }

  if (!is_missing_or_null(labels)) {
    assert(
      is_character(labels),
      "'labels' must be a character vector:\n  ", labels
    )
    payload$labels <- as.list(labels)
  }

  if (!is_missing_or_null(milestone)) {
    if (is_scalar_character(milestone)) {
      milestone <- view_milestone(milestone, repo = repo, ...)$number
    }

    assert(
      is_scalar_integerish(milestone),
      "'milestone' must be a string or integer:\n  ", milestone
    )
    payload$milestone <- milestone
  }

  info("Creating issue '", title, "' for repository '", repo, "'")
  issue_lst <- gh_url("repos", repo, "issues") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  issue_gh <- select_properties(issue_lst, properties$issue) %>%
    modify_list(
      assignees = map_chr(issue_lst$assignees, "login"),
      labels    = map_chr(issue_lst$labels, "name"),
      .before   = "milestone"
    ) %>%
    modify_list(
      pull_request = !is_null(issue_lst$pull_request),
      .before      = "creator"
    ) %>%
    modify_list(repository = repo)

  info("Done", level = 7)
  issue_gh
}


#  FUNCTION: update_issue ------------------------------------------------------
#
#' Update an issue in a repository
#'
#' This function updates an issue for the specified repository in GitHub. It can
#' be used to change the title, body, or assignees, it can also be used to
#' replace labels and milestones, or to close the issue.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#update-an-issue",
#'   ">"
#' ))
#' ```
#'
#' @param issue (string or character) The number or title of the issue.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param title (string, optional) The title of the issue.
#' @param body (string, optional) The contents of the issue.
#' @param assignees (character, optional) Logins for Users to assign to this
#'   issue. NOTE: Only users with push access can set assignees for new issues.
#'   Setting to `NULL` clears all assignees.
#' @param labels (character, optional) Labels to associate with this issue.
#'   NOTE: Only users with push access can set labels for new issues. Setting to
#'   `NULL` clears all labels.
#' @param milestone (character or integer, optional) The title or number of the
#'   milestone to associate this issue with. NOTE: Only users with push access
#'   can set the milestone for new issues. Setting to `NULL` clears the current
#'   milestone.
#' @param state (string, optional) The state of the issue. Either `"open"` or
#'   `"closed"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_issue()` returns a list of the issue's properties.
#'
#' **Issue Properties:**
#'
#' - **number**: The number assigned to the issue.
#' - **title**: The title of the issue.
#' - **body**: The body contents of the issue.
#' - **assignees**: The users assigned to the issue.
#' - **labels**: The labels attached to the issue.
#' - **milestone**: The milestone assigned to the issue.
#' - **state**: The state of the issue - either `"open"` or `"closed"`.
#' - **repository**: The repository the issue is in.
#' - **pull_request**: Whether the issue is a pull request.
#' - **html_url**: The URL of the issue's web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the issue was created.
#' - **updated_at**: When the issue was last updated.
#' - **closed_at**: When the issue was closed.
#'
#' @examples
#' \dontrun{
#'
#'   # Update the properties of a issue
#'   update_issue(
#'     issue   = "test issue",
#'     repo        = "ChadGoymer/githapi",
#'     title       = "updated test issue",
#'     description = "This is an updated test issue",
#'     due_on      = "2020-12-01"
#'   )
#'
#'   # Close a issue
#'   update_issue(
#'     issue = "updated test issue",
#'     repo      = "ChadGoymer/githapi",
#'     state     = "closed"
#'   )
#'
#' }
#'
#' @export
#'
update_issue <- function(
  issue,
  repo,
  title,
  body,
  assignees,
  labels,
  milestone,
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

  if (!is_missing_or_null(body)) {
    assert(
      is_scalar_character(body),
      "'body' must be a string:\n  ", body
    )
    payload$body <- body
  }

  if (!missing(assignees)) {
    assert(
      is_null(assignees) || is_character(assignees),
      "'assignees' must be NULL or a character vector:\n  ", assignees
    )
    payload$assignees <- as.list(assignees)
  }

  if (!missing(labels)) {
    assert(
      is_null(labels) || is_character(labels),
      "'labels' must be NULL or a character vector:\n  ", labels
    )
    payload$labels <- as.list(labels)
  }

  if (!missing(milestone)) {
    if (is_scalar_character(milestone)) {
      milestone <- view_milestone(milestone, repo = repo, ...)$number
    }

    assert(
      is_null(milestone) || is_scalar_integerish(milestone),
      "'milestone' must be NULL or a string or integer:\n  ", milestone
    )
    payload <- c(payload, list(milestone = milestone))
  }

  if (!is_missing_or_null(state)) {
    assert(
      is_scalar_character(state) && state %in% values$issue$state,
      "'state' for milestones must be either '",
      str_c(values$issue$state, collapse = "', '"), "':\n  ", state
    )
    payload$state <- state
  }

  issue <- view_issue(issue, repo = repo, ...)

  info("Updating issue '", issue$title, "' in repository '", repo, "'")
  issue_lst <- gh_url("repos", repo, "issues", issue$number) %>%
    gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  issue_gh <- select_properties(issue_lst, properties$issue) %>%
    modify_list(
      assignees = map_chr(issue_lst$assignees, "login"),
      labels    = map_chr(issue_lst$labels, "name"),
      .before   = "milestone"
    ) %>%
    modify_list(
      pull_request = !is_null(issue_lst$pull_request),
      .before      = "creator"
    ) %>%
    modify_list(repository = repo)

  info("Done", level = 7)
  issue_gh
}


#  FUNCTION: view_issues -------------------------------------------------------
#
#' View issues within a repository or organization
#'
#' `view_issues()` summarises issues in a table with the properties as columns
#' and a row for each issue in the repository or organization. `view_issue()`
#' returns a list of all properties for a single issue. `browse_issue()` opens
#' the web page for the issue in the default browser.
#'
#' You can summarise all the issues in a repository or organization by
#' specifying the arguments. If neither are specified then all the issues
#' assigned to the authenticated user are returned. You can filter the issues
#' based on the labels, milestone, whether they have been updated since a
#' specified date or whether they are `"open"` or `"closed"`. Finally, the order
#' the results are returned can be controlled with `sort` and `direction`.
#'
#' For more details see the GitHub API documentation:
#'
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#list-issues-assigned-to-the-authenticated-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#list-organization-issues-assigned-to-the-authenticated-user",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#list-repository-issues",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#get-an-issue",
#'   ">"
#' ))
#' ```
#'
#' @param issue (string or character) The number or title of the issue.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param org (string, optional) The name of the organization.
#' @param labels (character, optional) Label names to filter by.
#' @param milestone (string or integer) Milestone number or title to filter by.
#' @param since (string, optional) A date & time to filter by. Must be in the
#'   format: `YYYY-MM-DD HH:MM:SS`.
#' @param state (string, optional) The state of the issues to return. Can be
#'   either `"open"`, `"closed"`, or `"all"`. Default: `"open"`.
#' @param sort (string, optional) The property to order the returned issues by.
#'   Can be either `"created"`, `"updated"`, or `"comments"`. Default:
#'   `"created"`.
#' @param direction (string, optional) The direction of the sort. Can be either
#'   `"asc"` or `"desc"`. Default: `"desc"`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_issues()` returns a tibble of issue properties. `view_issue()`
#'   returns a list of properties for a single issue. `browse_issue()` opens the
#'   default browser on the issue's page and returns the URL.
#'
#' **Issue Properties:**
#'
#' - **number**: The number assigned to the issue.
#' - **title**: The title of the issue.
#' - **body**: The body contents of the issue.
#' - **assignees**: The users assigned to the issue.
#' - **labels**: The labels attached to the issue.
#' - **milestone**: The milestone assigned to the issue.
#' - **state**: The state of the issue - either `"open"` or `"closed"`.
#' - **repository**: The repository the issue is in.
#' - **pull_request**: Whether the issue is a pull request.
#' - **html_url**: The URL of the issue's web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the issue was created.
#' - **updated_at**: When the issue was last updated.
#' - **closed_at**: When the issue was closed.
#'
#' @examples
#' \dontrun{
#'
#'   # View open issues in a repository
#'   view_issues("ChadGoymer/githapi")
#'
#'   # View closed issues in a repository
#'   view_issues("ChadGoymer/githapi", state = "closed")
#'
#'   # View a single issue
#'   view_issue("test issue", "ChadGoymer/githapi")
#'
#'   # Open a issue's page in a browser
#'   browse_issue("test issue", "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
view_issues <- function(
  repo,
  org,
  labels,
  milestone,
  since,
  state     = "open",
  sort      = "created",
  direction = "desc",
  n_max     = 1000,
  ...
) {
  assert(
    is_scalar_character(state) && state %in% values$issue$state,
    "'state' must be either '", str_c(values$issue$state, collapse = "', '"),
    "':\n  ", state
  )
  assert(
    is_scalar_character(sort) && sort %in% values$issue$sort,
    "'sort' must be either '", str_c(values$issue$sort, collapse = "', '"),
    "':\n  ", sort
  )
  assert(
    is_scalar_character(direction) && direction %in% values$issue$direction,
    "'direction' must be either '",
    str_c(values$issue$direction, collapse = "', '"), "':\n  ", direction
  )

  if (!is_missing_or_null(since)) {
    assert(
      is_scalar_character(since),
      "'since' must be a string:\n  ", since
    )
    since <- as.POSIXct(since, format = "%Y-%m-%d %H:%M:%S") %>%
      format("%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    assert(
      !is.na(since),
      "'since' must be specified in the format 'YYYY-MM-DD hh:mm:ss':\n  ",
      since
    )
  }
  else {
    since <- NULL
  }

  if (!is_missing_or_null(labels)) {
    assert(
      is_character(labels),
      "'labels' must be a character vector:\n  ", labels
    )
    labels <- str_c(labels, collapse = ",")
  }
  else {
    labels <- NULL
  }

  if (!is_missing_or_null(repo)) {
    assert(
      is_repo(repo),
      "'repo' must be a string in the format 'owner/repo':\n  ", repo
    )

    if (!is_missing_or_null(milestone)) {
      if (is_scalar_character(milestone)) {
        milestone <- view_milestone(
          milestone = milestone,
          repo      = repo,
          ...
        ) %>%
          pluck("number")
      }
      assert(
        is_scalar_integerish(milestone),
        "'milestone' must be a string or an integer:\n  ", milestone
      )
    }
    else {
      milestone <- NULL
    }

    info("Viewing issues for repository '", repo, "'")
    url <- gh_url(
      "repos", repo, "issues",
      labels    = labels,
      milestone = milestone,
      state     = state,
      sort      = sort,
      direction = direction,
      since     = since
    )
  }
  else if (!is_missing_or_null(org)) {
    assert(
      is_scalar_character(org),
      "'org' must be a string:\n  ", org
    )

    info("Viewing issues for organization '", org, "'")
    url <- gh_url(
      "orgs", org, "issues",
      labels    = labels,
      state     = state,
      sort      = sort,
      direction = direction,
      since     = since
    )
  }
  else {
    info("Viewing issues assigned to the authenticated user")
    url <- gh_url(
      "issues",
      labels    = labels,
      state     = state,
      sort      = sort,
      direction = direction,
      since     = since
    )
  }

  issues_lst <- gh_page(url = url, n_max = n_max, ...)

  info("Transforming results", level = 4)
  issues_gh <- bind_properties(issues_lst, properties$issue) %>%
    add_column(
      labels  = map(issues_lst, ~ map_chr(.$labels, "name")),
      .before = "milestone"
    ) %>%
    add_column(
      assignees = map(issues_lst, ~ map_chr(.$assignees, "login")),
      .before   = "labels"
    ) %>%
    add_column(
      pull_request = map_lgl(issues_lst, ~ !is_null(.$pull_request)),
      .before      = "creator"
    )

  info("Done", level = 7)
  issues_gh
}


#  FUNCTION: view_issue --------------------------------------------------------
#
#' @rdname view_issues
#' @export
#'
view_issue <- function(
  issue,
  repo,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  if (is_scalar_integerish(issue)) {
    info("Viewing issue '", issue, "' for repository '", repo, "'")
    issue_lst <- gh_url("repos", repo, "issues", issue) %>%
      gh_request("GET", ...)
  }
  else if (is_scalar_character(issue)) {
    info("Viewing issue '", issue, "' for repository '", repo, "'")
    issue_lst <- gh_url("repos", repo, "issues", state = "all") %>%
      gh_find(property  = "title", value = issue, ...)
  }
  else {
    error("'issue' must be either an integer or a string:\n  ", issue)
  }

  info("Transforming results", level = 4)
  issue_gh <- select_properties(issue_lst, properties$issue) %>%
    modify_list(
      assignees = map_chr(issue_lst$assignees, "login"),
      labels    = map_chr(issue_lst$labels, "name"),
      .before   = "milestone"
    ) %>%
    modify_list(
      pull_request = !is_null(issue_lst$pull_request),
      .before      = "creator"
    ) %>%
    modify_list(repository = repo)

  info("Done", level = 7)
  issue_gh
}


#  FUNCTION: browse_issue ------------------------------------------------------
#
#' @rdname view_issues
#' @export
#'
browse_issue <- function(
  issue,
  repo,
  ...
) {
  issue <- view_issue(issue = issue, repo = repo, ...)

  info("Browsing issue '", issue$title, "' in repository '", repo, "'")
  httr::BROWSE(issue$html_url)

  info("Done", level = 7)
  structure(
    issue$html_url,
    class   = c("github", "character"),
    url     = attr(issue, "url"),
    request = attr(issue, "request"),
    status  = attr(issue, "status"),
    header  = attr(issue, "header")
  )
}
