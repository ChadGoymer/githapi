% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{view_labels}
\alias{view_labels}
\alias{view_label}
\title{View labels within a repository}
\usage{
view_labels(repo, issue, n_max = 1000, ...)

view_label(label, repo, ...)
}
\arguments{
\item{repo}{(string) The repository specified in the format: \code{owner/repo}.}

\item{issue}{(string or character, optional) The number or title of the
issue.}

\item{n_max}{(integer, optional) Maximum number to return. Default: \code{1000}.}

\item{...}{Parameters passed to \code{\link[=gh_page]{gh_page()}} or \code{\link[=gh_request]{gh_request()}}.}

\item{label}{(string) The name of the label.}
}
\value{
\code{view_labels()} returns a tibble of label properties. \code{view_label()}
returns a list of properties for a single label.

\strong{Label Properties:}
\itemize{
\item \strong{name}: The name of the label.
\item \strong{color}: The color of the label in hexidecimal code.
\item \strong{description}: The description of the label.
}
}
\description{
\code{view_labels()} summarises labels in a table with the properties as columns
and a row for each label in the repository. It can also be used to view the
labels assigned to a single issue. \code{view_label()} returns a list of all
properties for a single label.
}
\details{
For more details see the GitHub API documentation:
\itemize{
\item \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/issues#list-labels-for-a-repository}
\item \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/issues#list-labels-for-an-issue}
\item \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/issues#get-a-label}
}
}
\examples{
\dontrun{

  # View all labels in a repository
  view_labels("ChadGoymer/githapi")

  # View a single label
  view_label("new-label", "ChadGoymer/githapi")

}

}
