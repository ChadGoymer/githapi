% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github-api.R
\name{gh_download}
\alias{gh_download}
\title{Download a file from GitHub}
\usage{
gh_download(
  url,
  path,
  headers = NULL,
  accept = NULL,
  token = getOption("github.token"),
  proxy = getOption("github.proxy"),
  ...
)
}
\arguments{
\item{url}{(string) The address of the API endpoint.}

\item{path}{(string) The path to download the file to.}

\item{headers}{(character, optional) Headers to add to the request. Default:
\code{NULL}.}

\item{accept}{(string, optional) The mime format to accept when making the
call. Default: \code{NULL}.}

\item{token}{(string or Token, optional) An authorisation token to include
with the request. If \code{NULL} the OAuth process is triggered. Default:
\code{NULL}.}

\item{proxy}{(character, optional) The proxy server to use to connect to the
github API. If \code{NULL} then no proxy is used. Can be set in the option
\code{github.proxy} or the environment variable \code{GITHUB_PROXY}. Default: \code{NULL}.}

\item{...}{Ignored.}
}
\value{
A \code{github} string object containing the path, with the attributes:
\itemize{
\item \strong{url}: The URLs the request was sent to
\item \strong{request}: The type of HTTP request made
\item \strong{status}: The HTTP status code returned
\item \strong{header}: The HTTP header returned
}
}
\description{
This function downloads a file from GitHub in as a binary object.
}
\examples{
\dontrun{

  # Find an issue by title
  gh_download(
    url  = "https://api.github.com/repos/ChadGoymer/githapi/zipball/main",
    path = "~/githapi-main.zip"
  )

}

}
