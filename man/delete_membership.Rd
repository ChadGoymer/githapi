% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/memberships.R
\name{delete_membership}
\alias{delete_membership}
\title{Remove a member from an organization or team.}
\usage{
delete_membership(user, org, team, ...)
}
\arguments{
\item{user}{(string) The login of the user.}

\item{org}{(string) The login of the organization.}

\item{team}{(integer or string, optional) The ID or name of the team.}

\item{...}{Parameters passed to \code{\link[=gh_request]{gh_request()}}.}
}
\value{
\code{delete_membership()} returns a TRUE if successfully deleted.
}
\description{
This function removes a user from an organization or team. Removing someone
from a team does not remove them from the organization, whereas removing them
from an organization also removes them from any teams within the
organization.
}
\details{
Note: you can only remove a user if the authenticate user is an organization
"owner" or a team "maintainer".

For more details see the GitHub API documentation:
\itemize{
\item \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/orgs#remove-organization-membership-for-a-user}
\item \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/teams#remove-team-membership-for-a-user}
}
}
\examples{
\dontrun{

  # Remove a user from an organization
  delete_membership("ChadGoymer", "HairyCoos")

  # Remove a user from a team
  delete_membership("ChadGoymer", "HairyCoos", "HeadCoos")

}

}
