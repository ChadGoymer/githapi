#  FUNCTION: create_column -----------------------------------------------------
#
#' Create a column in a GitHub project
#'
#' This function creates a new column in a project in GitHub. The column will be
#' empty so you will need to add cards separately.
#'
#' You can create a column in a project associated with either a repository,
#' user or organization, by supplying them as an input, as long as you have
#' appropriate permissions.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/rest/reference/"
#' cat(paste0("- <", docs_url, "projects#create-a-project-column", ">"))
#' ```
#'
#' @param name (string) The name of the column.
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_column()` returns a list of the column properties.
#'
#' **Column Properties:**
#'
#' - **id**: The ID of the column.
#' - **name**: The name given to the column.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # Create a column in a repository project
#'   create_column(
#'     name    = "Test column",
#'     project = "Test project",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Create a column in a user's project
#'   create_column(
#'     name    = "Test column",
#'     project = "Test project",
#'     user    = "ChadGoymer"
#'   )
#'
#'   # Create a column in an organization's project
#'   create_column(
#'     name    = "Test column",
#'     project = "Test project",
#'     org     = "HairyCoos"
#'   )
#'
#' }
#'
#' @export
#'
create_column <- function(
  name,
  project,
  repo,
  user,
  org,
  ...
) {
  assert(
    is_scalar_character(name),
    "'name' must be a string:\n  ", name
  )

  project <- view_project(
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...
  )

  info("Creating column '", name, "' in project '", project$name, "'")
  column_lst <- gh_url("projects", project$id, "columns") %>%
    gh_request(
      type    = "POST",
      payload = list(name = name),
      accept  = "application/vnd.github.inertia-preview+json",
      ...
    )

  info("Transforming results", level = 4)
  column_gh <- select_properties(column_lst, properties$column)

  info("Done", level = 7)
  column_gh
}


#  FUNCTION: update_column -----------------------------------------------------
#
#' Update a column in a GitHub project
#'
#' `update_column()` can be used to change the column name in a project in
#'  GitHub. `move_column()` can be used to reorder the columns.
#'
#' You can update a column associated with either a repository, user or
#' organization, by supplying them as an input, as long as you have appropriate
#' permissions.
#'
#' You can move a column by either specifying the position, either `"first"` or
#' `"last"`, or by specifying another column to place it after.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/rest/reference/"
#' cat(paste0("- <", docs_url, "projects#update-a-project-column", ">"))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0("- <", docs_url, "projects#move-a-project-column", ">"))
#' ```
#'
#' @param column (integer or string) Either the column number or name.
#' @param name (string, optional) The new name for the column.
#' @param position (string, optional) Either `"first"` or `"last"`.
#' @param after (integer or string, optional) An ID or name of another column to
#'   place this one after.
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_column()` returns a list of the column properties.
#'
#' **Column Properties:**
#'
#' - **id**: The ID of the column.
#' - **name**: The name given to the column.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # Update the name of a column in a repository project
#'   update_column(
#'     column  = "Test column",
#'     name    = "Updated test column",
#'     project = "Test project",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Move a column to the first position in a user's project
#'   move_column(
#'     name     = "Test column",
#'     position = "first",
#'     user     = "ChadGoymer"
#'   )
#'
#'   # Move a column after another on in an organization's project
#'   move_column(
#'     name  = "Test column",
#'     after = "Test column 2",
#'     org   = "HairyCoos"
#'   )
#'
#' }
#'
#' @export
#'
update_column <- function(
  column,
  name,
  project,
  repo,
  user,
  org,
  ...
) {
  assert(
    is_scalar_character(name),
    "'name' must be a string:\n  ", name
  )

  column <- view_column(
    column  = column,
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...
  )

  info("Updating column '", column$name, "' in project '", project, "'")
  column_lst <- gh_url("projects/columns", column$id) %>%
    gh_request(
      type    = "PATCH",
      payload = list(name = name),
      accept  = "application/vnd.github.inertia-preview+json",
      ...
    )

  info("Transforming results", level = 4)
  column_gh <- select_properties(column_lst, properties$column)

  info("Done", level = 7)
  column_gh
}


#  FUNCTION: move_column -------------------------------------------------------
#
#' @rdname update_column
#' @export
#'
move_column <- function(
  column,
  position,
  after,
  project,
  repo,
  user,
  org,
  ...
) {
  if (!missing(position)) {
    assert(
      is_scalar_character(position) && position %in% values$column$position,
      "'position' must be one of '",
      str_c(values$column$position, collapse = "', '"), "':\n  ",
      position
    )

    payload <- list(position = position)
  }
  else if (!missing(after)) {
    after_column <- view_column(
      column  = after,
      project = project,
      repo    = repo,
      user    = user,
      org     = org,
      ...
    )

    payload <- list(position = str_c("after:", after_column$id))
  }
  else {
    error("Either 'position' or 'after' must be supplied")
  }

  column <- view_column(
    column  = column,
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...
  )

  info("Moving column '", column$name, "' in project '", project, "'")
  response <- gh_url("projects/columns", column$id, "moves") %>%
    gh_request(
      type    = "POST",
      payload = payload,
      accept  = "application/vnd.github.inertia-preview+json",
      ...
    )

  info("Done", level = 7)
  structure(
    column,
    class   = class(column),
    url     = attr(response, "url"),
    request = attr(response, "request"),
    status  = attr(response, "status"),
    header  = attr(response, "header")
  )
}


#  FUNCTION: view_columns ------------------------------------------------------
#
#' View columns within a GitHub project
#'
#' `view_columns()` summarises columns in a table with the properties as columns
#' and a row for each column in the project. `view_column()` returns a list of
#' all properties for a single column.
#'
#' You can summarise all the columns of a project associated with either a
#' repository, user or organization, by supplying them as an input.
#'
#' For more details see the GitHub API documentation:
#' - <https://docs.github.com/en/rest/reference/projects#list-project-columns>
#' - <https://docs.github.com/en/rest/reference/projects#get-a-project-column>
#'
#' @param column (integer or string) The number or name of the column.
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_columns()` returns a tibble of column properties.
#'   `view_column()` returns a list of properties for a single column.
#'
#' **Column Properties:**
#'
#' - **id**: The ID of the column.
#' - **name**: The name given to the column.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # View columns in a repository project
#'   view_columns(
#'     project = "Test columns",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # View columns in a user's project
#'   view_columns(
#'     project = "Test columns",
#'     user    = "ChadGoymer"
#'   )
#'
#'   # View columns in an organization's project
#'   view_column(
#'     project = "Test columns",
#'     org     = "HairyCoos"
#'   )
#'
#'   # View a column in a repository project
#'   view_column(
#'     column  = "Test column",
#'     project = "Test columns",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # View a column in a user's project
#'   view_column(
#'     column  = "Test column",
#'     project = "Test columns",
#'     user    = "ChadGoymer"
#'   )
#'
#'   # View a column in an organization's project
#'   view_column(
#'     column  = "Test column",
#'     project = "Test columns",
#'     org     = "HairyCoos"
#'   )
#'
#'   # View column by ID
#'   view_column(123456)
#'
#' }
#'
#' @export
#'
view_columns <- function(
  project,
  repo,
  user,
  org,
  n_max = 1000,
  ...
) {
  project <- view_project(
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...
  )

  info("Viewing columns in project '", project$name, "'")
  columns_lst <- gh_url("projects", project$id, "columns") %>%
    gh_page(
      accept = "application/vnd.github.inertia-preview+json",
      n_max  = n_max,
      ...
    )

  info("Transforming results", level = 4)
  columns_gh <- bind_properties(columns_lst, properties$column)

  info("Done", level = 7)
  columns_gh
}


#  FUNCTION: view_column -------------------------------------------------------
#
#' @rdname view_columns
#' @export
#'
view_column <- function(
  column,
  project,
  repo,
  user,
  org,
  ...
) {
  if (is_scalar_integerish(column)) {
    info("Viewing column '", column, "''")
    column_lst <- gh_url("projects/columns", column) %>%
      gh_request(
        type   = "GET",
        accept = "application/vnd.github.inertia-preview+json",
        ...
      )
  }
  else if (is_scalar_character(column)) {
    project <- view_project(
      project = project,
      repo    = repo,
      user    = user,
      org     = org,
      ...
    )

    info("Viewing column '", column, "' in project '", project$name, "'")
    column_lst <- gh_url("projects", project$id, "columns") %>%
      gh_find(
        property = "name",
        value    = column,
        accept   = "application/vnd.github.inertia-preview+json",
        ...
      )
  }
  else {
    error("'column' must be either an integer or a string:\n  ", column)
  }

  info("Transforming results", level = 4)
  column_gh <- select_properties(column_lst, properties$column)

  info("Done", level = 7)
  column_gh
}


#  FUNCTION: delete_column -----------------------------------------------------
#
#' Delete a column in a GitHub project
#'
#' This function deletes a column in a GitHub project. Care should be taken as
#' it will not be recoverable.
#'
#' You can delete a column associated with either a repository, user or
#' organization, by supplying them as an input, as long as you have appropriate
#' permissions.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/rest/reference/"
#' cat(paste0("- <", docs_url, "projects#delete-a-project-column", ">"))
#' ```
#'
#' @param column (integer or string) Either the column number or name.
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_column()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   # Delete a column in a repository project
#'   delete_column(
#'     column  = "Test column",
#'     project = "Test project",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Delete a column in a user's project
#'   delete_column(
#'     column  = "Test column",
#'     project = "Test project",
#'     user    = "ChadGoymer"
#'   )
#'
#'   # Delete a column in an organization's project
#'   delete_column(
#'     column  = "Test column",
#'     project = "Test project",
#'     org     = "HairyCoos"
#'   )
#'
#' }
#'
#' @export
#'
delete_column <- function(
  column,
  project,
  repo,
  user,
  org,
  ...
) {
  column <- view_column(
    column  = column,
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...
  )

  info("Deleting column '", column$name, "' in project '", project, "'")
  response <- gh_url("projects/columns", column$id) %>%
    gh_request(
      type   = "DELETE",
      accept = "application/vnd.github.inertia-preview+json",
      ...
    )

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
