% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github-api.R
\name{gh_request}
\alias{gh_request}
\title{Send an HTTP request to the GitHub API}
\usage{
gh_request(
  url,
  type,
  payload = NULL,
  headers = NULL,
  accept = "application/vnd.github.v3+json",
  token = getOption("github.token"),
  proxy = getOption("github.proxy"),
  ...
)
}
\arguments{
\item{url}{(string) The address of the API endpoint.}

\item{type}{(string) The type of HTTP request. Either "GET", "POST", "PATCH",
"PUT" or "DELETE".}

\item{payload}{(list, optional) The information to send to the API for
"POST", "PATCH", "PUT" or "DELETE" requests. Default: \code{NULL}.}

\item{headers}{(character, optional) Headers to add to the request. Default:
\code{NULL}.}

\item{accept}{(string, optional) The mime format to accept when making the
call. Default: \code{"application/vnd.github.v3+json"}.}

\item{token}{(string or Token, optional) An authorisation token to include
with the request. If \code{NULL} the OAuth process is triggered. Default:
\code{NULL}.}

\item{proxy}{(character, optional) The proxy server to use to connect to the
github API. If \code{NULL} then no proxy is used. Can be set in the option
\code{github.proxy} or the environment variable \code{GITHUB_PROXY}. Default: \code{NULL}.}

\item{...}{Ignored.}
}
\value{
A \code{github} list object consisting of the response, parsed into a
list, with the attributes:
\itemize{
\item \strong{url}: The URL the request was sent to
\item \strong{request}: The type of HTTP request made
\item \strong{status}: The HTTP status code returned
\item \strong{header}: The HTTP header returned
}
}
\description{
This function can be used to make "GET", "POST", "PATCH", "PUT" or "DELETE"
requests to the specified URL.
}
\details{
The response is parsed from either JSON or plain text, depending on the
format received. When no response is received an empty list returned by the
function. Details of the response are recorded as attributes.

For "POST", "PATCH", "PUT" and "DELETE" requests a payload can be supplied.
It is parsed into a JSON format before being sent to the URL.

If an error is returned from the API then an error is thrown by this
function, detailing the URL, the HTTP status code and a message from the API,
if there is one.

Finally, an authorisation token can be supplied if it is required.
}
\examples{
\dontrun{

  # Create a tag
  gh_request(
    url     = "https://api.github.com/repos/ChadGoymer/githapi/git/refs",
    type    = "POST",
    payload = list(
      ref = "test-tag",
      sha = "a4b6545671455234757313a42738e44c10b0ef37"
    )
  )

  # View a tag
  gh_request(
    url  = "https://api.github.com/repos/ChadGoymer/githapi/git/test-tag",
    type = "GET"
  )

  # Update a tag
  gh_request(
    url     = "https://api.github.com/repos/ChadGoymer/githapi/git/test-tag",
    type    = "PATCH",
    payload = list(sha = "a4b6545671455234757313a42738e44c10b0ef37")
  )

  # Delete a tag
  gh_request(
    url  = "https://api.github.com/repos/ChadGoymer/githapi/git/test-tag",
    type = "DELETE"
  )

}

}
