% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/columns.R
\name{update_column}
\alias{update_column}
\alias{move_column}
\title{Update a column in a GitHub project}
\usage{
update_column(column, name, project, repo, user, org, ...)

move_column(column, position, after, project, repo, user, org, ...)
}
\arguments{
\item{column}{(integer or string) Either the column number or name.}

\item{name}{(string, optional) The new name for the column.}

\item{project}{(integer or string) Either the project number or name.}

\item{repo}{(string, optional) The repository specified in the format:
\code{owner/repo}.}

\item{user}{(string, optional) The login of the user.}

\item{org}{(string, optional) The name of the organization.}

\item{...}{Parameters passed to \code{\link[=gh_request]{gh_request()}}.}

\item{position}{(string, optional) Either \code{"first"} or \code{"last"}.}

\item{after}{(integer or string, optional) An ID or name of another column to
place this one after.}
}
\value{
\code{update_column()} returns a list of the column properties.

\strong{Column Properties:}
\itemize{
\item \strong{id}: The ID of the column.
\item \strong{name}: The name given to the column.
\item \strong{created_at}: When it was created.
\item \strong{updated_at}: When it was last updated.
}
}
\description{
\code{update_column()} can be used to change the column name in a project in
GitHub. \code{move_column()} can be used to reorder the columns.
}
\details{
You can update a column associated with either a repository, user or
organization, by supplying them as an input, as long as you have appropriate
permissions.

You can move a column by either specifying the position, either \code{"first"} or
\code{"last"}, or by specifying another column to place it after.

For more details see the GitHub API documentation:
\itemize{
\item \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/projects#update-an-existing-project-column}
\item \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/projects#move-a-project-column}
}
}
\examples{
\dontrun{

  # Update the name of a column in a repository project
  update_column(
    column  = "Test column",
    name    = "Updated test column",
    project = "Test project",
    repo    = "ChadGoymer/githapi"
  )

  # Move a column to the first position in a user's project
  move_column(
    name     = "Test column",
    position = "first",
    user     = "ChadGoymer"
  )

  # Move a column after another on in an organization's project
  move_column(
    name  = "Test column",
    after = "Test column 2",
    org   = "HairyCoos"
  )

}

}
