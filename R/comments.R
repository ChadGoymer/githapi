#  FUNCTION: create_comment ----------------------------------------------------
#
#' Create a comment in github
#'
#' This function creates a new comment for the specified gist, issue, pull
#' request or commit in GitHub. A standard comment is created for a gist or
#' issue (note: since a pull request is also an issue a standard comment is also
#' created if the `issue` argument is set to the pull request number) and a
#' review comment is created for pull requests and commits.
#'
#' When creating a gist comment only the `body` and `gist` ID need to be
#' specified; for an issue the `body`, `issue` number and `repo` must be
#' specified; and for a pull request or commit the `body`, `pull_request` number
#' or `commit` reference, `repo`, `path` and `position` must be specified.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "gists#create-a-gist-comment",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#create-an-issue-comment",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#create-a-review-comment-for-a-pull-request",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#create-a-commit-comment",
#'   ">"
#' ))
#' ```
#'
#' @param body (string) The content of the comment.
#' @param gist (string, optional) The ID of the gist.
#' @param issue (integer, optional) The issue number.
#' @param pull_request (integer, optional) The pull request number.
#' @param commit (string, optional) Either a SHA or branch used to identify the
#'   commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param path (string, optional) The path of the file to add the comment to.
#' @param position (integer, optional) The line number in the file to attach the
#'   comment to.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_comment()` returns a list of the comment's properties.
#'
#' **Comment Properties:**
#'
#' - **id**: The id of the comment.
#' - **body**: The content of the comment.
#' - **commit**: The commit associated with the comment (only for pull request
#'   or commit comments).
#' - **path**: The path of the file to add the comment to (only for pull request
#'   or commit comments).
#' - **position**: The line number in the file to attach the comment to (only
#'   for pull request or commit comments).
#' - **user**: The comment author's account login.
#' - **html_url**: The address of the comment's web page.
#' - **created_at**: The time and date the comment was created.
#' - **published_at**: The time and date the comment was published.
#'
#' @examples
#' \dontrun{
#'
#'   # Create a comment on a gist
#'   create_comment(
#'     body = "This is a comment created by create_comment()",
#'     gist = "806dca6b09a39e7b6326a0c8137583e6"
#'   )
#'
#'   # Create a comment on an issue
#'   create_comment(
#'     body  = "This is a comment created by create_comment()",
#'     issue = "test issue",
#'     repo  = "ChadGoymer/githapi"
#'   )
#'
#'   # Create a comment on a pull request
#'   create_comment(
#'     body         = "This is a comment created by create_comment()",
#'     pull_request = "test pull request",
#'     commit       = "test-branch",
#'     repo         = "ChadGoymer/githapi",
#'     path         = "test-comments.txt",
#'     position     = 1
#'   )
#'
#'   # Create a comment on a commit
#'   create_comment(
#'     body     = "This is a comment created by create_comment()",
#'     commit   = "main",
#'     repo     = "ChadGoymer/githapi",
#'     path     = "README.md",
#'     position = 1
#'   )
#'
#' }
#'
#' @export
#'
create_comment <- function(
  body,
  gist,
  issue,
  pull_request,
  commit,
  repo,
  path,
  position,
  ...
) {
  assert(
    is_scalar_character(body),
    "'body' must be a string:\n  ", body
  )

  payload <- list(body = body)

  if (!is_missing_or_null(gist)) {
    assert(
      is_scalar_character(gist),
      "'gist' must be a string:\n  ", gist
    )

    info("Creating comment for gist '", gist, "'")
    url <- gh_url("gists", gist, "comments")
  }
  else {
    assert(
      is_repo(repo),
      "'repo' must be a string in the format 'owner/repo':\n  ", repo
    )

    if (!is_missing_or_null(issue)) {
      if (is_scalar_character(issue)) {
        issue <- view_issue(issue = issue, repo = repo, ...)$number
      }
      assert(
        is_scalar_integerish(issue),
        "'issue' must be a string or integer:\n  ", issue
      )

      info(
        "Creating comment for issue '", issue,
        "' in repository '", repo, "'"
      )
      url <- gh_url("repos", repo, "issues", issue, "comments")
    }
    else if (!is_missing_or_null(commit)) {
      if (!is_sha(commit)) {
        commit <- view_sha(ref = commit, repo = repo, ...)
      }
      assert(
        is_sha(commit),
        "'commit' must be a 40 character string:\n  ", commit
      )
      assert(
        is_scalar_character(path),
        "'path' must be a string:\n  ", path
      )
      assert(
        is_scalar_integerish(position),
        "'position' must be a string or integer:\n  ", position
      )

      payload$path <- path
      payload$position <- position

      if (is_missing_or_null(pull_request)) {
        info(
          "Creating comment for commit '", commit,
          "' in repository '", repo, "'"
        )
        url <- gh_url("repos", repo, "commits", commit, "comments")
      }
      else {
        if (is_scalar_character(pull_request)) {
          pull_request <- view_pull_request(
            pull_request = pull_request,
            repo = repo,
            ...
          ) %>%
            pluck("number")
        }
        assert(
          is_scalar_integerish(pull_request),
          "'pull_request' must be a string or integer:\n  ", pull_request
        )

        payload$commit_id <- commit

        info(
          "Creating comment for pull request '", pull_request,
          "' in repository '", repo, "'"
        )
        url <- gh_url("repos", repo, "pulls", pull_request, "comments")
      }
    }
    else {
      error("An 'issue', 'pull_request', 'commit' or 'gist' must be specified")
    }
  }

  comment_lst <- gh_request("POST", url = url, payload = payload, ...)

  info("Transforming results", level = 4)
  if (is_missing_or_null(commit) && is_missing_or_null(pull_request)) {
    comment_gh <- select_properties(comment_lst, properties$issue_comment)
  }
  else {
    comment_gh <- select_properties(comment_lst, properties$commit_comment)
  }

  info("Done", level = 7)
  comment_gh
}


#  FUNCTION: update_comment ----------------------------------------------------
#
#' Update a comment in a repository
#'
#' This function updates a comment for a gist, issue, pull request or commit. It
#' can be used to replace the content of the comment. The `repo` argument is not
#' required if a gist is specified.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "gists#update-a-gist-comment",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#update-an-issue-comment",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#update-a-review-comment-for-a-pull-request",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#update-a-commit-comment",
#'   ">"
#' ))
#' ```
#'
#' @param comment (integer) The id of the comment.
#' @param body (string) The content of the comment.
#' @param gist (string, optional) The ID of the gist.
#' @param type (string, optional) Whether the comment is for a `"gist"`,
#'   `"issue"`, `"pull_request"` or `"commit"`. Default: `"gist"`.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_comment()` returns a list of the comment properties.
#'
#' **Comment Properties:**
#'
#' - **id**: The id of the comment.
#' - **body**: The content of the comment.
#' - **commit**: The commit associated with the comment (only for pull request
#'   or commit comments).
#' - **path**: The path of the file to add the comment to (only for pull request
#'   or commit comments).
#' - **position**: The line number in the file to attach the comment to (only
#'   for pull request or commit comments).
#' - **user**: The comment author's account login.
#' - **html_url**: The address of the comment's web page.
#' - **created_at**: The time and date the comment was created.
#' - **published_at**: The time and date the comment was published.
#'
#' @examples
#' \dontrun{
#'
#'   # Update a gist comment
#'   update_comment(
#'     comment = 3281939,
#'     body    = "This comment has been updated by update_comment()",
#'     gist    = "8e5be270de9a88168372293a813543f9"
#'   )
#'
#'   # Update an issue comment
#'   update_comment(
#'     comment = 622980929,
#'     body    = "This comment has been updated by update_comment()",
#'     type    = "issue",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Update an pull request comment
#'   update_comment(
#'     comment = 418979473,
#'     body    = "This comment has been updated by update_comment()",
#'     type    = "pull_request",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Update a commit comment
#'   update_comment(
#'     comment = 38899533,
#'     body    = "This comment has been updated by update_comment()",
#'     type    = "commit",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
update_comment <- function(
  comment,
  body,
  gist,
  repo,
  type = "gist",
  ...
) {
  assert(
    is_scalar_integerish(comment),
    "'comment' must be a string or integer:\n  ", comment
  )
  assert(
    is_scalar_character(body),
    "'body' must be a string:\n  ", body
  )

  payload <- list(body = body)

  if (!is_missing_or_null(gist)) {
    info("Updating comment '", comment, "' for gist '", gist, "'")
    url <- gh_url("gists", gist, "comments", comment)
  }
  else {
    assert(
      is_repo(repo),
      "'repo' must be a string in the format 'owner/repo':\n  ", repo
    )
    assert(
      is_scalar_character(type) && type %in% values$comment$type,
      "'type' must be one of '", str_c(values$comment$type, collapse = "', '"),
      "':\n  ", type
    )

    if (identical(type, "issue")) {
      info("Updating issue comment '", comment, "' in repository '", repo, "'")
      url <- gh_url("repos", repo, "issues/comments", comment)
    }
    else if (identical(type, "pull_request")) {
      info(
        "Updating pull request comment '", comment,
        "' in repository '", repo, "'"
      )
      url <- gh_url("repos", repo, "pulls/comments", comment)
    }
    else {
      info("Updating commit comment '", comment, "' in repository '", repo, "'")
      url <- gh_url("repos", repo, "comments", comment)
    }
  }

  comment_lst <- gh_request("PATCH", url = url, payload = payload, ...)

  info("Transforming results", level = 4)
  if (!is_missing_or_null(gist) || identical(type, "issue")) {
    comment_gh <- select_properties(comment_lst, properties$issue_comment)
  }
  else {
    comment_gh <- select_properties(comment_lst, properties$commit_comment)
  }

  info("Done", level = 7)
  comment_gh
}


#  FUNCTION: view_comments -----------------------------------------------------
#
#' View comments in GitHub
#'
#' `view_comments()` summarises comments in a table with the properties as
#' columns and a row for each comment registered for the gist, issue, pull
#' request or commit. `view_comment()` returns a list of all properties for a
#' single comment. `browse_comment()` opens the web page for the comment in the
#' default browser.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "gists#list-gist-comments",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#list-issue-comments",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#list-review-comments-on-a-pull-request",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#list-commit-comments",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "gists#get-a-gist-comment",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#get-an-issue-comment",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#get-a-review-comment-for-a-pull-request",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#get-a-commit-comment",
#'   ">"
#' ))
#' ```
#'
#' @param comment (integer) The id of the comment.
#' @param gist (string, optional) The ID of the gist.
#' @param issue (integer, optional) The issue number.
#' @param pull_request (integer, optional) The pull request number.
#' @param commit (string, optional) Either a SHA or branch used to identify the
#'   commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param type (string, optional) Whether the comment is for a `"gist"`,
#'   `"issue"`, `"pull_request"` or `"commit"`. Default: `"gist"`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_comments()` returns a tibble of comment properties.
#'   `view_comment()` returns a list of properties for a single comment.
#'   `browse_comment()` opens the default browser on the comment page and
#'   returns the URL.
#'
#' **Comment Properties:**
#'
#' - **id**: The id of the comment.
#' - **body**: The content of the comment.
#' - **commit**: The commit associated with the comment (only for pull request
#'   or commit comments).
#' - **path**: The path of the file to add the comment to (only for pull request
#'   or commit comments).
#' - **position**: The line number in the file to attach the comment to (only
#'   for pull request or commit comments).
#' - **user**: The comment author's account login.
#' - **html_url**: The address of the comment's web page.
#' - **created_at**: The time and date the comment was created.
#' - **published_at**: The time and date the comment was published.
#'
#' @examples
#' \dontrun{
#'
#'   # View comments on a gist
#'   view_comments(gist = "8e5be270de9a88168372293a813543f9")
#'
#'   # View comments on an issue
#'   view_comments(
#'     issue = "test issue",
#'     repo  = "ChadGoymer/githapi"
#'   )
#'
#'   # View comments on a pull request
#'   view_comments(
#'     pull_request = "test pull request",
#'     repo         = "ChadGoymer/githapi"
#'   )
#'
#'   # View comments on a commit
#'   view_comments(
#'     commit = "main",
#'     repo   = "ChadGoymer/githapi"
#'   )
#'
#'   # View a single gist comment
#'   view_comment(
#'     comment = 622980929,
#'     gist    = "8e5be270de9a88168372293a813543f9"
#'   )
#'
#'   # View a single issue comment
#'   view_comment(
#'     comment = 622980929,
#'     type    = "issue",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # View a single pull request comment
#'   view_comment(
#'     comment = 418979473,
#'     type    = "pull_request",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # View a single commit comment
#'   view_comment(
#'     comment = 38899533,
#'     type    = "commit",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Browse an issue comment
#'   browse_comment(
#'     comment = 622980929,
#'     type    = "issue",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Browse a pull request comment
#'   browse_comment(
#'     comment = 418979473,
#'     type    = "pull_request",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Browse a commit comment
#'   browse_comment(
#'     comment = 38899533,
#'     type    = "commit",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
view_comments <- function(
  gist,
  issue,
  pull_request,
  commit,
  repo,
  n_max = 1000,
  ...
) {
  if (!is_missing_or_null(gist)) {
    info("Viewing comments for gist '", gist, "'")
    url <- gh_url("gists", gist, "comments")
  }
  else {
    assert(
      is_repo(repo),
      "'repo' must be a string in the format 'owner/repo':\n  ", repo
    )

    if (!is_missing_or_null(issue)) {
      if (is_scalar_character(issue)) {
        issue <- view_issue(issue = issue, repo = repo, ...)$number
      }
      assert(
        is_scalar_integerish(issue),
        "'issue' must be a string or integer:\n  ", issue
      )

      info(
        "Viewing comments for issue '", issue,
        "' in repository '", repo, "'"
      )
      url <- gh_url("repos", repo, "issues", issue, "comments")
    }
    else if (!is_missing_or_null(commit)) {
      if (!is_sha(commit)) {
        commit <- view_sha(ref = commit, repo = repo, ...)
      }
      assert(
        is_sha(commit),
        "'commit' must be a 40 character string:\n  ", commit
      )

      info(
        "Viewing comments for commit '", commit,
        "' in repository '", repo, "'"
      )
      url <- gh_url("repos", repo, "commits", commit, "comments")
    }
    else if (!is_missing_or_null(pull_request)) {
      if (is_scalar_character(pull_request)) {
        pull_request <- view_pull_request(
          pull_request = pull_request,
          repo         = repo,
          ...
        ) %>%
          pluck("number")
      }
      assert(
        is_scalar_integerish(pull_request),
        "'pull_request' must be a string or integer:\n  ", pull_request
      )

      info(
        "Viewing comments for pull request '", pull_request,
        "' in repository '", repo, "'"
      )
      url <- gh_url("repos", repo, "pulls", pull_request, "comments")
    }
    else {
      error("An 'issue', 'pull_request', 'commit' or 'gist' must be specified")
    }
  }

  comment_lst <- gh_page(url = url, ...)

  info("Transforming results", level = 4)
  if (is_missing_or_null(commit) && is_missing_or_null(pull_request)) {
    comment_gh <- bind_properties(comment_lst, properties$issue_comment)
  }
  else {
    comment_gh <- bind_properties(comment_lst, properties$commit_comment)
  }

  info("Done", level = 7)
  comment_gh
}


#  FUNCTION: view_comment ------------------------------------------------------
#
#' @rdname view_comments
#' @export
#'
view_comment <- function(
  comment,
  gist,
  repo,
  type = "gist",
  ...
) {
  assert(
    is_scalar_integerish(comment),
    "'comment' must be a string or integer:\n  ", comment
  )

  if (!is_missing_or_null(gist)) {
    info("Viewing comment '", comment, "' for gist '", gist, "'")
    url <- gh_url("gists", gist, "comments", comment)
  }
  else {
    assert(
      is_repo(repo),
      "'repo' must be a string in the format 'owner/repo':\n  ", repo
    )
    assert(
      is_scalar_character(type) && type %in% values$comment$type,
      "'type' must be one of '", str_c(values$comment$type, collapse = "', '"),
      "':\n  ", type
    )

    if (identical(type, "issue")) {
      info("Viewing issue comment '", comment, "' in repository '", repo, "'")
      url <- gh_url("repos", repo, "issues/comments", comment)
    }
    else if (identical(type, "pull_request")) {
      info(
        "Viewing pull request comment '", comment,
        "' in repository '", repo, "'"
      )
      url <- gh_url("repos", repo, "pulls/comments", comment)
    }
    else {
      info("Viewing commit comment '", comment, "' in repository '", repo, "'")
      url <- gh_url("repos", repo, "comments", comment)
    }
  }

  comment_lst <- gh_request("GET", url = url, ...)

  info("Transforming results", level = 4)
  if (!is_missing_or_null(gist) || identical(type, "issue")) {
    comment_gh <- select_properties(comment_lst, properties$issue_comment)
  }
  else {
    comment_gh <- select_properties(comment_lst, properties$commit_comment)
  }

  info("Done", level = 7)
  comment_gh
}


#  FUNCTION: browse_comment ----------------------------------------------------
#
#' @rdname view_comments
#' @export
#'
browse_comment <- function(
  comment,
  repo,
  type,
  ...
) {
  assert(
    is_scalar_integerish(comment),
    "'comment' must be a string or integer:\n  ", comment
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )
  assert(
    is_scalar_character(type) && type %in% values$comment$type,
    "'type' must be one of '", str_c(values$comment$type, collapse = "', '"),
    "':\n  ", type
  )

  if (identical(type, "issue")) {
    info("Browsing issue comment '", comment, "' in repository '", repo, "'")
    url <- gh_url("repos", repo, "issues/comments", comment)
  }
  else if (identical(type, "pull_request")) {
    info(
      "Browsing pull request comment '", comment,
      "' in repository '", repo, "'"
    )
    url <- gh_url("repos", repo, "pulls/comments", comment)
  }
  else {
    info("Browsing commit comment '", comment, "' in repository '", repo, "'")
    url <- gh_url("repos", repo, "comments", comment)
  }

  comment <- gh_request("GET", url = url, ...)
  httr::BROWSE(comment$html_url)

  info("Done", level = 7)
  structure(
    comment$html_url,
    class   = c("github", "character"),
    url     = attr(comment, "url"),
    request = attr(comment, "request"),
    status  = attr(comment, "status"),
    header  = attr(comment, "header")
  )
}


#  FUNCTION: delete_comment ----------------------------------------------------
#
#' Delete a comment from GitHub
#'
#' This function deletes a comment from a gist, issue, pull request or commit,
#' as long as you have appropriate permissions. Care should be taken as it will
#' not be recoverable.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "gists#delete-a-gist-comment",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#delete-an-issue-comment",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "pulls#delete-a-review-comment-for-a-pull-request",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#delete-a-commit-comment",
#'   ">"
#' ))
#' ```
#'
#' @param comment (integer) The id of the comment.
#' @param gist (string, optional) The ID of the gist.
#' @param type (string, optional) Whether the comment is for a `"gist"`,
#'   `"issue"`, `"pull_request"` or `"commit"`. Default: `"gist"`.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_comment()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   # Delete a gist comment
#'   delete_comment(
#'     comment = 622980929,
#'     gist    = "8e5be270de9a88168372293a813543f9"
#'   )
#'
#'   # Delete a issue comment
#'   delete_comment(
#'     comment = 622980929,
#'     type    = "issue",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Delete a pull request comment
#'   delete_comment(
#'     comment = 418979473,
#'     type    = "pull_request",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Delete a commit comment
#'   delete_comment(
#'     comment = 38899533,
#'     type    = "commit",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
delete_comment <- function(
  comment,
  gist,
  repo,
  type = "gist",
  ...
) {
  assert(
    is_scalar_integerish(comment),
    "'comment' must be a string or integer:\n  ", comment
  )

  if (!is_missing_or_null(gist)) {
    info("Deleting comment '", comment, "' for gist '", gist, "'")
    url <- gh_url("gists", gist, "comments", comment)
  }
  else {
    assert(
      is_repo(repo),
      "'repo' must be a string in the format 'owner/repo':\n  ", repo
    )
    assert(
      is_scalar_character(type) && type %in% values$comment$type,
      "'type' must be one of '", str_c(values$comment$type, collapse = "', '"),
      "':\n  ", type
    )

    if (identical(type, "issue")) {
      info("Deleting issue comment '", comment, "' in repository '", repo, "'")
      url <- gh_url("repos", repo, "issues/comments", comment)
    }
    else if (identical(type, "pull_request")) {
      info(
        "Deleting pull request comment '", comment,
        "' in repository '", repo, "'"
      )
      url <- gh_url("repos", repo, "pulls/comments", comment)
    }
    else {
      info("Deleting commit comment '", comment, "' in repository '", repo, "'")
      url <- gh_url("repos", repo, "comments", comment)
    }
  }

  response <- gh_request("DELETE", url = url, ...)

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
