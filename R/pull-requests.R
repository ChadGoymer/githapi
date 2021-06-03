#  FUNCTION: create_pull_request -----------------------------------------------
#
#' Create an pull request in a repository
#'
#' This function creates a new pull request for the specified repository in
#' GitHub. It can also be used to assign the pull request to a user, request
#' reviewers and add labels or a milestone.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#create-a-pull-request",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#update-an-issue",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#request-reviewers-for-a-pull-request",
#'   ">"
#' ))
#' ```
#'
#' @param title (string) The title of the pull request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param head (string) The name of the branch where your changes are
#'   implemented. For cross-repository pull requests, prefix `head` with an
#'   owner, e.g. `"username:branch"`.
#' @param base (string) The name of the branch you want the changes pulled into.
#'   This should be an existing branch in the specified repository.
#' @param body (string, optional) The contents of the pull request.
#' @param assignees (character, optional) Logins for Users to assign to this
#'   pull request. NOTE: Only users with push access can set assignees for new
#'   pull requests.
#' @param reviewers (character, optional) Logins for Users to review this pull
#'   request. NOTE: Only users with push access can set reviewers for new pull
#'   requests.
#' @param labels (character, optional) Labels to associate with this pull
#'   request. NOTE: Only users with push access can set labels for new pull
#'   requests.
#' @param milestone (character or integer, optional) The title or number of the
#'   milestone to associate this pull request with. NOTE: Only users with push
#'   access can set the milestone for new pull requests.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_pull_request()` returns a list of the pull request's
#'   properties.
#'
#' **Pull Request Properties:**
#'
#' - **number**: The number assigned to the pull request.
#' - **title**: The title of the pull request.
#' - **body**: The body contents of the pull request.
#' - **head_sha**: The SHA of the commit to merge in.
#' - **head_ref**: The reference, or branch, to merge in.
#' - **head_repo**: The repository containing the branch to merge in.
#' - **base_sha**: The SHA of the commit to merge onto.
#' - **base_ref**: The reference, or branch, to merge onto.
#' - **merge_sha**: The SHA of the merge commit, if the merge has been
#'   completed.
#' - **assignees**: The users assigned to the pull request.
#' - **reviewers**: The users reviewing the pull request.
#' - **labels**: The labels attached to the pull request.
#' - **milestone**: The milestone assigned to the pull request.
#' - **state**: The state of the pull request - either `"open"` or `"closed"`.
#' - **repository**: The repository the pull request is in.
#' - **html_url**: The URL of the pull request's web page in GitHub.
#' - **diff_url**: The URL of the pull request's diff web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the pull request was created.
#' - **updated_at**: When the pull request was last updated.
#' - **mergeable**: Whether the pull request can be merged.
#' - **rebaseable**: Whether the pull request can be rebased.
#' - **merged**: Whether the pull request has been merged
#' - **merged_by**: Who merged the pull request.
#' - **merged_at**: When the pull request was merged.
#' - **closed_at**: When the pull request was closed.
#'
#' @examples
#' \dontrun{
#'
#'   create_pull_request(
#'     title = "test pull request",
#'     repo  = "ChadGoymer/githapi",
#'     head  = "test-pulls",
#'     base  = "main",
#'     body  = "This is a pull request to test create_pull_request()"
#'   )
#'
#'   create_pull_request(
#'     title     = "test assigned pull request",
#'     repo      = "ChadGoymer/githapi",
#'     head      = "test-pulls-2",
#'     base      = "main",
#'     body      = "This is a pull request to test create_pull_request()",
#'     assignees = "ChadGoymer",
#'     reviewers = c("BobSmith", "JaneJones"),
#'     labels    = "enhancement",
#'     milestone = "Release-1.0"
#'   )
#'
#' }
#'
#' @export
#'
create_pull_request <- function(
  title,
  repo,
  head,
  base,
  body,
  assignees,
  reviewers,
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
  assert(
    is_scalar_character(head),
    "'head' must be a string:\n  ", head
  )
  assert(
    is_scalar_character(base),
    "'base' must be a string:\n  ", base
  )

  payload <- list(title = title, head = head, base = base)

  if (!is_missing_or_null(body)) {
    assert(
      is_scalar_character(body),
      "'body' must be a string:\n  ", body
    )
    payload$body <- body
  }

  info("Creating pull request '", title, "' for repository '", repo, "'")
  pull_lst <- gh_url("repos", repo, "pulls") %>%
    gh_request("POST", payload = payload, ...)

  payload <- list()

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

  issue_lst <- list(milestone = list(title = NA_character_))
  if (length(payload) > 0) {
    info(
      "Adding assignees, labels or milestone to pull request '",
      pull_lst$title, "'"
    )
    issue_lst <- gh_url("repos", repo, "issues", pull_lst$number) %>%
      gh_request("PATCH", payload = payload, ...)
  }

  reviewers_lst <- list()
  if (!is_missing_or_null(reviewers)) {
    assert(
      is_character(reviewers),
      "'reviewers' must be a character vector:\n  ", reviewers
    )
    info("Adding reviewers to pull request '", pull_lst$title, "'")
    reviewers_lst <- gh_url(
      "repos", repo, "pulls", pull_lst$number, "requested_reviewers"
    ) %>%
      gh_request("POST", payload = list(reviewers = as.list(reviewers)), ...)
  }

  info("Transforming results", level = 4)
  pull_gh <- select_properties(pull_lst, properties$pull_request) %>%
    modify_list(
      assignees = map_chr(issue_lst$assignees, "login"),
      reviewers = map_chr(reviewers_lst$requested_reviewers, "login"),
      labels    = map_chr(issue_lst$labels, "name"),
      .before   = "milestone"
    ) %>%
    modify_list(repository = repo, milestone = issue_lst$milestone$title)

  info("Done", level = 7)
  pull_gh
}


#  FUNCTION: update_pull_request -----------------------------------------------
#
#' Update a pull request in a repository
#'
#' This function updates a pull request for the specified repository in GitHub.
#' It can be used to change the title or body, or used to close the pull
#' request. It can also be used to assign the pull request to a user, request
#' reviewers and replace labels or a milestone.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#update-a-pull-request",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#update-an-issue",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#request-reviewers-for-a-pull-request",
#'   ">"
#' ))
#'
#' @param pull_request (string or character) The number or title of the pull
#'   request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param title (string, optional) The new title of the pull request.
#' @param body (string, optional) The contents of the pull request.
#' @param assignees (character, optional) Logins for Users to assign to this
#'   pull request. NOTE: Only users with push access can set assignees for new
#'   pull requests.
#' @param reviewers (character, optional) Logins for Users to review this pull
#'   request. NOTE: Only users with push access can set reviewers for new pull
#'   requests.
#' @param labels (character, optional) Labels to associate with this pull
#'   request. NOTE: Only users with push access can set labels for new pull
#'   requests.
#' @param milestone (character or integer, optional) The title or number of the
#'   milestone to associate this pull request with. NOTE: Only users with push
#'   access can set the milestone for new pull requests.
#' @param state (string, optional) The state of the pull request. Either
#'   `"open"` or `"closed"`.
#' @param base (string, optional) The name of the branch you want the changes
#'   pulled into. This should be an existing branch on the current repository.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_pull_request()` returns a list of the pull request's
#'   properties.
#'
#' **Pull Request Properties:**
#'
#' - **number**: The number assigned to the pull request.
#' - **title**: The title of the pull request.
#' - **body**: The body contents of the pull request.
#' - **head_sha**: The SHA of the commit to merge in.
#' - **head_ref**: The reference, or branch, to merge in.
#' - **head_repo**: The repository containing the branch to merge in.
#' - **base_sha**: The SHA of the commit to merge onto.
#' - **base_ref**: The reference, or branch, to merge onto.
#' - **merge_sha**: The SHA of the merge commit, if the merge has been
#'   completed.
#' - **assignees**: The users assigned to the pull request.
#' - **reviewers**: The users reviewing the pull request.
#' - **labels**: The labels attached to the pull request.
#' - **milestone**: The milestone assigned to the pull request.
#' - **state**: The state of the pull request - either `"open"` or `"closed"`.
#' - **repository**: The repository the pull request is in.
#' - **html_url**: The URL of the pull request's web page in GitHub.
#' - **diff_url**: The URL of the pull request's diff web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the pull request was created.
#' - **updated_at**: When the pull request was last updated.
#' - **mergeable**: Whether the pull request can be merged.
#' - **rebaseable**: Whether the pull request can be rebased.
#' - **merged**: Whether the pull request has been merged
#' - **merged_by**: Who merged the pull request.
#' - **merged_at**: When the pull request was merged.
#' - **closed_at**: When the pull request was closed.
#'
#' @examples
#' \dontrun{
#'
#'   # Update a pull request's properties
#'   update_pull_request(
#'     pull_request = "test pull request",
#'     repo         = "ChadGoymer/githapi",
#'     title        = "test updated pull request",
#'     body         = "This is an updated pull request"
#'   )
#'
#'   # Close a pull request
#'   update_pull_request(
#'     pull_request = "test updated pull request",
#'     repo         = "ChadGoymer/githapi",
#'     state        = "closed"
#'   )
#'
#' }
#'
#' @export
#'
update_pull_request <- function(
  pull_request,
  repo,
  title,
  body,
  assignees,
  reviewers,
  labels,
  milestone,
  base,
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

  if (!is_missing_or_null(state)) {
    assert(
      is_scalar_character(state) && state %in% values$pull_request$state,
      "'state' for milestones must be either '",
      str_c(values$pull_request$state, collapse = "', '"), "':\n  ", state
    )
    payload$state <- state
  }

  if (!is_missing_or_null(base)) {
    assert(
      is_scalar_character(base),
      "'base' must be a string:\n  ", base
    )
    payload$base <- base
  }

  if (is_scalar_integerish(pull_request)) {
    info("Viewing pull request '", pull_request, "' for repo '", repo, "'")
    pull_lst <- gh_url("repos", repo, "pulls", pull_request) %>%
      gh_request("GET", ...)
  }
  else if (is_scalar_character(pull_request)) {
    info("Viewing pull_request '", pull_request, "' for repo '", repo, "'")
    pull_lst <- gh_url("repos", repo, "pulls", state = "all") %>%
      gh_find(property  = "title", value = pull_request, ...)
  }
  else {
    error(
      "'pull_request' must be either an integer or a string:\n  ",
      pull_request
    )
  }

  if (length(payload) > 0) {
    info(
      "Updating pull request '", pull_lst$title,
      "' in repository '", repo, "'"
    )
    pull_lst <- gh_url("repos", repo, "issues", pull_lst$number) %>%
      gh_request("PATCH", payload = payload, ...)
  }

  payload <- list()

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

  issue_lst <- list(milestone = list(title = NA_character_))
  if (length(payload) > 0) {
    info(
      "Adding assignees, labels or milestone to pull request '",
      pull_lst$title, "'"
    )
    issue_lst <- gh_url("repos", repo, "issues", pull_lst$number) %>%
      gh_request("PATCH", payload = payload, ...)
  }

  reviewers_lst <- list()
  if (!is_missing_or_null(reviewers)) {
    assert(
      is_character(reviewers),
      "'reviewers' must be a character vector:\n  ", reviewers
    )
    info("Adding reviewers to pull request '", pull_lst$title, "'")
    reviewers_lst <- gh_url(
      "repos", repo, "pulls", pull_lst$number, "requested_reviewers"
    ) %>%
      gh_request("POST", payload = list(reviewers = as.list(reviewers)), ...)
  }

  info("Transforming results", level = 4)
  pull_gh <- select_properties(pull_lst, properties$pull_request) %>%
    modify_list(
      assignees = map_chr(issue_lst$assignees, "login"),
      reviewers = map_chr(reviewers_lst$requested_reviewers, "login"),
      labels    = map_chr(issue_lst$labels, "name"),
      .before   = "milestone"
    ) %>%
    modify_list(repository = repo, milestone = issue_lst$milestone$title)

  info("Done", level = 7)
  pull_gh
}


#  FUNCTION: view_pull_requests ------------------------------------------------
#
#' View pull requests within a repository
#'
#' `view_pull_requests()` summarises pull requests in a table with the
#' properties as columns and a row for each pull request in the repository.
#' `view_pull_request()` returns a list of all properties for a single pull
#' request. `browse_pull_request()` opens the web page for the pull request in
#' the default browser.
#'
#' You can filter the pull requests by the head and base branches (the branch to
#' merge in and the branch to merge into) or the state (whether they are
#' `"open"` or `"closed"`). You can also order the results with `sort` and
#' `direction`.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#list-pull-requests",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#get-a-pull-request",
#'   ">"
#' ))
#' ```
#'
#' @param pull_request (string or character) The number or title of the pull
#'   request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param head (string, optional) Filter pull requests by the branch name. If it
#'   is not in the specified `repo` then the owner must prefix the branch name,
#'   e.g. `"owner:branch"`.
#' @param base (string, optional) Filter pull requests by the branch to be
#'   merged into.
#' @param state (string, optional) The state of the pull requests to return. Can
#'   be either `"open"`, `"closed"`, or `"all"`. Default: `"open"`.
#' @param sort (string, optional) The property to order the returned pull
#'   requests by. Can be either `"created"`, `"updated"`, `"popularity"`
#'   (comment count) or `"long-running"` (age, filtering by pulls updated in the
#'   last month). Default: `"created"`.
#' @param direction (string, optional) The direction of the sort. Can be either
#'   `"asc"` or `"desc"`. Default: `"desc"`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_pull_requests()` returns a tibble of pull request properties.
#'   `view_pull_request()` returns a list of properties for a single pull
#'   request. `browse_pull_request()` opens the default browser on the pull
#'   request's page and returns the URL.
#'
#' **Pull Request Properties:**
#'
#' - **number**: The number assigned to the pull request.
#' - **title**: The title of the pull request.
#' - **body**: The body contents of the pull request.
#' - **head_sha**: The SHA of the commit to merge in.
#' - **head_ref**: The reference, or branch, to merge in.
#' - **head_repo**: The repository containing the branch to merge in.
#' - **base_sha**: The SHA of the commit to merge onto.
#' - **base_ref**: The reference, or branch, to merge onto.
#' - **merge_sha**: The SHA of the merge commit, if the merge has been
#'   completed.
#' - **assignees**: The users assigned to the pull request.
#' - **reviewers**: The users reviewing the pull request.
#' - **labels**: The labels attached to the pull request.
#' - **milestone**: The milestone assigned to the pull request.
#' - **state**: The state of the pull request - either `"open"` or `"closed"`.
#' - **repository**: The repository the pull request is in.
#' - **html_url**: The URL of the pull request's web page in GitHub.
#' - **diff_url**: The URL of the pull request's diff web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the pull request was created.
#' - **updated_at**: When the pull request was last updated.
#' - **mergeable**: Whether the pull request can be merged.
#' - **rebaseable**: Whether the pull request can be rebased.
#' - **merged**: Whether the pull request has been merged
#' - **merged_by**: Who merged the pull request.
#' - **merged_at**: When the pull request was merged.
#' - **closed_at**: When the pull request was closed.
#'
#' Additionally, the `view_pull_request()` function also returns:
#'
#' - **commits**: Information about the commits made on the branch.
#'   - **message**: The message specified for the commit.
#'   - **author_name**: The author's name.
#'   - **author_email**: The author's email address.
#'   - **author_date**: The date/time it was authored.
#'   - **committer_name**: The committer's name.
#'   - **committer_email**: The committer's email address.
#'   - **committer_date**: The date/time it was committed.
#'   - **parent_sha**: The SHA of the parent commit(s).
#'   - **html_url**: The URL of the commit's web page in GitHub.
#'
#' - **files**: Information about the files changed.
#'   - **sha**: The SHA of the file.
#'   - **filename**: The name of the file.
#'   - **status**: The status of the file.
#'   - **additions**: The number of lines added.
#'   - **deletions**: The number of lines deleted.
#'   - **changes**: The number of lines changed.
#'   - **patch**: The patch information.
#'   - **html_url**: The URL of the file's web page in GitHub.
#'
#' - **reviews**: The reviews registered.
#'   - **body**: The contents of the review.
#'   - **state**: The state of the review.
#'   - **user**: The user who added the review.
#'   - **html_url**: Th URL of the review's web page in GitHub.
#'   - **submitted_at**: When the review was submitted.
#'
#' @examples
#' \dontrun{
#'
#'   # View all open pull requests
#'   view_pull_requests("ChadGoymer/githapi")
#'
#'   # View all closed pull requests
#'   view_pull_requests("ChadGoymer/githapi", state = "closed")
#'
#'   # View all pull requests for the "main" branch
#'   view_pull_requests("ChadGoymer/githapi", base = "main")
#'
#'   # View pull requests, sorted by the most recently updated
#'   view_pull_requests(
#'     repo      = "ChadGoymer/githapi",
#'     sort      = "updated",
#'     direction = "desc"
#'   )
#'
#'   # View single pull request
#'   view_pull_request("test pull request", repo = "ChadGoymer/githapi")
#'
#'   # Open a pull request's page in a browser
#'   browse_pull_request("test pull request", repo = "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
view_pull_requests <- function(
  repo,
  head,
  base,
  state     = "open",
  sort      = "created",
  direction = "desc",
  n_max     = 1000,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  if (!is_missing_or_null(head)) {
    assert(
      is_scalar_character(head),
      "'head' must be a string:\n  ", head
    )
    if (!str_detect(head, ":")) {
      head <- str_c(dirname(repo), ":", head)
    }
  }
  else {
    head <- NULL
  }

  if (!is_missing_or_null(base)) {
    assert(
      is_scalar_character(base),
      "'base' must be a string:\n  ", base
    )
  }
  else {
    base <- NULL
  }

  assert(
    is_scalar_character(state) && state %in% values$pull_request$state,
    "'state' must be either '",
    str_c(values$pull_request$state, collapse = "', '"), "':\n  ", state
  )
  assert(
    is_scalar_character(sort) && sort %in% values$pull_request$sort,
    "'sort' must be either '",
    str_c(values$pull_request$sort, collapse = "', '"), "':\n  ", sort
  )
  assert(
    is_scalar_character(direction) &&
      direction %in% values$pull_request$direction,
    "'direction' must be either '",
    str_c(values$pull_request$direction, collapse = "', '"), "':\n  ",
    direction
  )

  info("Viewing pull requests for repository '", repo, "'")
  pulls_lst <- gh_url(
    "repos", repo, "pulls",
    head      = head,
    base      = base,
    state     = state,
    sort      = sort,
    direction = direction
  ) %>%
    gh_page(n_max = n_max, ...)

  info("Transforming results", level = 4)
  pulls_gh <- bind_properties(pulls_lst, properties$pull_request) %>%
    add_column(
      labels  = map(pulls_lst, ~ map_chr(.$labels, "name")),
      .before = "milestone"
    ) %>%
    add_column(
      assignees = map(pulls_lst, ~ map_chr(.$assignees, "login")),
      .before   = "labels"
    ) %>%
    add_column(
      reviewers = map(pulls_lst, ~ map_chr(.$requested_reviewers, "login")),
      .before   = "labels"
    )

  info("Done", level = 7)
  pulls_gh
}


#  FUNCTION: view_pull_request -------------------------------------------------
#
#' @rdname view_pull_requests
#' @export
#'
view_pull_request <- function(
  pull_request,
  repo,
  n_max = 1000,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  if (is_scalar_integerish(pull_request)) {
    info("Viewing pull request '", pull_request, "' for repo '", repo, "'")
    pull_lst <- gh_url("repos", repo, "pulls", pull_request) %>%
      gh_request("GET", ...)
  }
  else if (is_scalar_character(pull_request)) {
    info("Viewing pull_request '", pull_request, "' for repo '", repo, "'")
    pull_lst <- gh_url("repos", repo, "pulls", state = "all") %>%
      gh_find(property  = "title", value = pull_request, ...)
  }
  else {
    error(
      "'pull_request' must be either an integer or a string:\n  ",
      pull_request
    )
  }

  info("Transforming results", level = 4)
  commits_lst <- gh_url("repos", repo, "pulls", pull_lst$number, "commits") %>%
    gh_page(n_max = n_max, ...)
  commits <- bind_properties(commits_lst, properties$pull_commits) %>%
    add_column(
      parents = map(commits_lst, ~ map_chr(.$parents, "sha")),
      .before = "html_url"
    )

  files <- gh_url("repos", repo, "pulls", pull_lst$number, "files") %>%
    gh_page(n_max = n_max, ...) %>%
    bind_properties(properties$pull_files)

  reviews <- gh_url("repos", repo, "pulls", pull_lst$number, "reviews") %>%
    gh_page(n_max = n_max, ...) %>%
    bind_properties(properties$pull_reviews)

  pull_gh <- select_properties(pull_lst, properties$pull_request) %>%
    modify_list(
      assignees = map_chr(pull_lst$assignees, "login"),
      reviewers = map_chr(pull_lst$requested_reviewers, "login"),
      labels    = map_chr(pull_lst$labels, "name"),
      .before   = "milestone"
    ) %>%
    modify_list(
      repository = repo,
      commits    = commits,
      files      = files,
      reviews    = reviews
    )

  info("Done", level = 7)
  pull_gh
}


#  FUNCTION: browse_pull_request -----------------------------------------------
#
#' @rdname view_pull_requests
#' @export
#'
browse_pull_request <- function(
  pull_request,
  repo,
  ...
) {
  pull_request <- view_pull_request(
    pull_request = pull_request,
    repo         = repo,
    ...
  )

  info("Browsing pull request '", pull_request$title, "' in repo '", repo, "'")
  httr::BROWSE(pull_request$html_url)

  info("Done", level = 7)
  structure(
    pull_request$html_url,
    class   = c("github", "character"),
    url     = attr(pull_request, "url"),
    request = attr(pull_request, "request"),
    status  = attr(pull_request, "status"),
    header  = attr(pull_request, "header")
  )
}
