% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commits.R
\name{view_sha}
\alias{view_sha}
\title{View the SHA for a commit}
\usage{
view_sha(ref, repo, ...)
}
\arguments{
\item{ref}{(string) Either a SHA, branch or tag used to identify the commit.}

\item{repo}{(string) The repository specified in the format: \code{owner/repo}.}

\item{...}{Parameters passed to \code{\link[=gh_request]{gh_request()}}.}
}
\value{
\code{view_sha()} returns the commit SHA as a string.
}
\description{
This function returns the commit SHA given a git reference. A reference can
be either a SHA, branch or tag. If it is a branch then the head commit is
used.
}
\details{
For more details see the GitHub API documentation:
\itemize{
\item \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#get-a-commit}
}
}
\examples{
\dontrun{

  view_sha("a-tag", repo = "ChadGoymer/githapi")
  view_sha("a-branch", repo = "ChadGoymer/githapi")

}

}
