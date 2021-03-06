% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cards.R
\name{view_cards}
\alias{view_cards}
\alias{view_card}
\title{View cards within a GitHub project}
\usage{
view_cards(column, project, repo, user, org, n_max = 1000, ...)

view_card(card, ...)
}
\arguments{
\item{column}{(integer or string) Either the ID or name of the column.}

\item{project}{(integer or string) Either the project number or name.}

\item{repo}{(string, optional) The repository specified in the format:
\code{owner/repo}.}

\item{user}{(string, optional) The login of the user.}

\item{org}{(string, optional) The name of the organization.}

\item{n_max}{(integer, optional) Maximum number to return. Default: \code{1000}.}

\item{...}{Parameters passed to \code{\link[=gh_page]{gh_page()}} or \code{\link[=gh_request]{gh_request()}}.}

\item{card}{(integer) The ID of the card.}
}
\value{
\code{view_cards()} returns a tibble of card properties. \code{view_card()}
returns a list of properties for a single card.

\strong{Card Properties:}
\itemize{
\item \strong{id}: The ID of the card.
\item \strong{content_id}: The ID of the issue or pull request.
\item \strong{note}: The content of a note.
\item \strong{archived}: Whether the card has been archived.
\item \strong{creator}: The creator of the note.
\item \strong{created_at}: When it was created.
\item \strong{updated_at}: When it was last updated.
}
}
\description{
\code{view_cards()} summarises cards in a table with the properties as columns and
a row for each card in a column of a project. \code{view_card()} returns a list of
all properties for a single card.
}
\details{
You can summarise all the cards of a project associated with either a
repository, user or organization, by supplying them as an input.

For more details see the GitHub API documentation:
\itemize{
\item \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/projects#list-project-cards}
\item \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/projects#get-a-project-card}
}
}
\examples{
\dontrun{

  # View cards in a repository project
  cards <- view_cards(
    column  = "Test cards",
    project = "Test cards",
    repo    = "ChadGoymer/githapi"
  )

  # View cards in a user's project
  cards <- view_cards(
    column  = "Test cards",
    project = "Test cards",
    user    = "ChadGoymer"
  )

  # View cards in an organization's project
  cards <- view_cards(
    column  = "Test cards",
    project = "Test cards",
    org     = "HairyCoos"
  )

  # View a single card
  view_card(card = 123456)

}

}
