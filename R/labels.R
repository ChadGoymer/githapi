#  FUNCTION: create_label ------------------------------------------------------
#
#' Create an issue label in a repository
#'
#' This function creates a new label for issues and pull requests for the
#' specified repository in GitHub. It can also be used to set the colour and
#' description.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#create-a-label",
#'   ">"
#' ))
#' ```
#'
#' @param name (string) The name of the label.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param description (string, optional) A description of the label.
#' @param color (string, optional) Either the color name (see
#'   [grDevices::colors()]) or a hexidecimal color code (see
#'   [color-hex.com](http://www.color-hex.com/)). If not supplied a color is
#'   chosen at random.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_label()` returns a list of the label's properties.
#'
#' **Label Properties:**
#'
#' - **name**: The name of the label.
#' - **color**: The color of the label in hexidecimal code.
#' - **description**: The description of the label.
#'
#' @examples
#' \dontrun{
#'
#'   create_label(
#'     name        = "new-label",
#'     repo        = "ChadGoymer/githapi",
#'     color       = "blue",
#'     description = "This is a detailed label"
#'   )
#'
#' }
#'
#' @export
#'
create_label <- function(
  name,
  repo,
  description,
  color = random_color(),
  ...
) {
  assert(
    is_scalar_character(name),
    "'name' must be a string:\n  ", name
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )
  assert(
    is_scalar_character(color),
    "'color' must be a string:\n  ", color
  )

  if (!is_hex(color)) {
    color <- as_hex(color)
  }
  color <- str_remove(color, "^#")

  payload <- list(name = name, color = color)

  if (!is_missing_or_null(description)) {
    assert(
      is_scalar_character(description),
      "'description' must be a string:\n  ", description
    )
    payload$description <- description
  }

  info("Creating label '", name, "' for repository '", repo, "'")
  label_lst <- gh_url("repos", repo, "labels") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  label_gh <- select_properties(label_lst, properties$label)

  info("Done", level = 7)
  label_gh
}


#  FUNCTION: update_label ------------------------------------------------------
#
#' Update a label in a repository
#'
#' This function updates a label for the specified repository in GitHub. It can
#' be used to change name, color or description.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#update-a-label",
#'   ">"
#' ))
#' ```
#'
#' @param label (string) The name of the label.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param name (string, optional) The name of the label.
#' @param color (string, optional) Either the color name (see
#'   [grDevices::colors()]) or a hexidecimal color code (see
#'   [color-hex.com](http://www.color-hex.com/)).
#' @param description (string, optional) A description of the label.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_label()` returns a list of the label properties.
#'
#' **Label Properties:**
#'
#' - **name**: The name of the label.
#' - **color**: The color of the label in hexidecimal code.
#' - **description**: The description of the label.
#'
#' @examples
#' \dontrun{
#'
#'   update_label(
#'     label       = "new-label",
#'     repo        = "ChadGoymer/githapi",
#'     name        = "updated-label",
#'     color       = "green",
#'     description = "This is an updated label"
#'   )
#'
#' }
#'
#' @export
#'
update_label <- function(
  label,
  repo,
  name,
  color,
  description,
  ...
) {
  assert(
    is_scalar_character(label),
    "'label' must be a string:\n  ", label
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  payload <- list()

  if (!is_missing_or_null(name)) {
    assert(
      is_scalar_character(name),
      "'name' must be a string:\n  ", name
    )
    payload$name <- name
  }

  if (!is_missing_or_null(color)) {
    assert(
      is_scalar_character(color),
      "'color' must be a string:\n  ", color
    )
    if (!is_hex(color)) {
      color <- as_hex(color)
    }
    payload$color <- str_remove(color, "^#")
  }

  if (!is_missing_or_null(description)) {
    assert(
      is_scalar_character(description),
      "'description' must be a string:\n  ", description
    )
    payload$description <- description
  }

  info("Updating label '", label, "' in repository '", repo, "'")
  label_lst <- gh_url("repos", repo, "labels", label) %>%
    gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  label_gh <- select_properties(label_lst, properties$label)

  info("Done", level = 7)
  label_gh
}


#  FUNCTION: view_labels -------------------------------------------------------
#
#' View labels within a repository
#'
#' `view_labels()` summarises labels in a table with the properties as columns
#' and a row for each label in the repository. It can also be used to view the
#' labels assigned to a single issue. `view_label()` returns a list of all
#' properties for a single label.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#list-labels-for-a-repository",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#list-labels-for-an-issue",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#get-a-label",
#'   ">"
#' ))
#' ```
#'
#' @param label (string) The name of the label.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param issue (string or character, optional) The number or title of the
#'   issue.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_labels()` returns a tibble of label properties. `view_label()`
#'   returns a list of properties for a single label.
#'
#' **Label Properties:**
#'
#' - **name**: The name of the label.
#' - **color**: The color of the label in hexidecimal code.
#' - **description**: The description of the label.
#'
#' @examples
#' \dontrun{
#'
#'   # View all labels in a repository
#'   view_labels("ChadGoymer/githapi")
#'
#'   # View a single label
#'   view_label("new-label", "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
view_labels <- function(
  repo,
  issue,
  n_max = 1000,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  if (is_missing_or_null(issue)) {
    info("Viewing labels for repository '", repo, "'")
    url <- gh_url("repos", repo, "labels")
  }
  else {
    issue <- view_issue(issue, repo = repo, ...)
    info(
      "Viewing labels for issue '", issue$title,
      "' in repository '", repo, "'"
    )
    url <- gh_url("repos", repo, "issues", issue$number, "labels")
  }

  labels_lst <- gh_page(url = url, n_max = n_max, ...)

  info("Transforming results", level = 4)
  labels_gh <- bind_properties(labels_lst, properties$label)

  info("Done", level = 7)
  labels_gh
}


#  FUNCTION: view_label --------------------------------------------------------
#
#' @rdname view_labels
#' @export
#'
view_label <- function(
  label,
  repo,
  ...
) {
  assert(
    is_scalar_character(label),
    "'label' must be a string:\n  ", label
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Viewing label '", label, "' for repository '", repo, "'")
  label_lst <- gh_url("repos", repo, "labels", label) %>%
    gh_request("GET", ...)

  info("Transforming results", level = 4)
  label_gh <- select_properties(label_lst, properties$label)

  info("Done", level = 7)
  label_gh
}


#  FUNCTION: delete_label ------------------------------------------------------
#
#' Delete a label from a repository
#'
#' This function deletes a label from a repository, as long as you have
#' appropriate permissions. Care should be taken as it will not be recoverable.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#delete-a-label",
#'   ">"
#' ))
#' ```
#'
#' @param label (string) The name of the label.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_label()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   delete_label("new-label", repo = "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
delete_label <- function(
  label,
  repo,
  ...
) {
  assert(
    is_scalar_character(label),
    "'label' must be a string:\n  ", label
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Deleting label '", label, "' in repository '", repo, "'")
  response <- gh_url("repos", repo, "labels", label) %>%
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


#  FUNCTION: add_labels --------------------------------------------------------
#
#' Add or remove labels from an issue or pull request
#'
#' `add_labels()` adds labels to an existing issue or pull request within a
#' GitHub repository. `remove_labels()` removes labels from an issue or pull
#' request.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#add-labels-to-an-issue",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "issues#remove-a-label-from-an-issue",
#'   ">"
#' ))
#' ```
#'
#' @param labels (character) The name of the labels.
#' @param issue (string or character) The number or title of the issue.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `add_labels()` returns a tibble of the added labels properties.
#'   `remove_labels()` returns a tibble of the remaining labels properties.
#'
#' **Label Properties:**
#'
#' - **name**: The name of the label.
#' - **color**: The color of the label in hexidecimal code.
#' - **description**: The description of the label.
#'
#' @examples
#' \dontrun{
#'
#'   add_labels(
#'     labels = c("feature", "new-label"),
#'     issue  = "Test issue",
#'     repo   = "ChadGoymer/githapi"
#'   )
#'
#'   remove_labels(
#'     labels = c("feature", "new-label"),
#'     issue  = "Test issue",
#'     repo   = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
add_labels <- function(
  labels,
  issue,
  repo,
  ...
) {
  assert(
    is_character(labels),
    "'labels' must be a character vector:\n  ", labels
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  issue <- view_issue(issue, repo = repo, ...)

  info(
    "Adding labels '", str_c(labels, collapse = "', '"),
    "' to issue '", issue$title, "' in repository '", repo, "'"
  )
  labels_lst <- gh_url("repos", repo, "issues", issue$number, "labels") %>%
    gh_request("POST", payload = list(labels = as.list(labels)), ...)

  info("Transforming results", level = 4)
  labels_gh <- bind_properties(labels_lst, properties$label)

  info("Done", level = 7)
  labels_gh
}


#  FUNCTION: remove_label ------------------------------------------------------
#
#' @rdname add_labels
#' @export
#'
remove_labels <- function(
  labels,
  issue,
  repo,
  ...
) {
  assert(
    is_character(labels),
    "'labels' must be a character vector:\n  ", labels
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  issue <- view_issue(issue, repo = repo, ...)

  info(
    "Deleting labels '", str_c(labels, collapse = "', '"),
    "' from issue '", issue$title, "' in repository '", repo, "'"
  )
  labels_lst <- try_map(labels, function(label) {
    gh_url("repos", repo, "issues", issue$number, "labels", label) %>%
      gh_request("DELETE", ...)
  })

  info("Transforming results", level = 4)
  labels_gh <- bind_properties(last(labels_lst), properties$label)

  info("Done", level = 7)
  labels_gh
}
