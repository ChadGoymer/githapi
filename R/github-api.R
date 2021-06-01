#  FUNCTION: gh_token ----------------------------------------------------------
#
#' Get a token for accessing GitHub
#'
#' This function initiates the OAuth authentication code grant flow with GitHub.
#' The user will be redirected to GitHub's login page to enter a user name and
#' password. Once complete the user can close the window and this function
#' returns a token which can be used for all calls to the githapi API.
#'
#' For non-interactive processes a personal access token can be specified,
#' either as an R option (`"github.token"`) or as an environment variable
#' (either `GITHAPI_TOKEN`, `GITHUB_PAT`, or `GITHUB_TOKEN`).
#'
#' @param token (string, optional) A personal access token. If `NULL` then the
#'   OAuth process is triggered. Can be set in the `github.token` option or the
#'   `GITHAPI_TOKEN`, `GITHUB_PAT`, or `GITHUB_TOKEN` environment variable.
#'   Default: `NULL`.
#' @param oauth (string, optional) The base URL for for the OAuth endpoint. Can
#'   be set in the `github.oauth` option or the `GITHUB_OAUTH` environment
#'   variable. Default: `"https://github.com/login/oauth"`.
#' @param proxy (character, optional) The proxy server to use to connect to the
#'   github API. If `NULL` then no proxy is used. Can be set in the option
#'   `github.proxy` or the environment variable `GITHUB_PROXY`. Default: `NULL`.
#' @param id (string, optional) The application ID for accessing GitHub. Can be
#'   set in the `githapi.id` option or the `GITHAPI_ID` environment variable.
#'   Default: The id for the githapi application in github.com.
#' @param secret (string, optional) The secret for the application to access
#'   GitHub. Can be set in the `githapi.secret` option or the `GITHAPI_SECRET`
#'   environment variable. Default: The secret for the githapi application in
#'   github.com
#' @param cache (boolean or string, optional) The location to store a cached
#'   token. If `TRUE` the cache uses the httr default; if `FALSE` it does not
#'   cache. Can be set in the `"githapi.cache"` option or the `GITHAPI_CACHE`
#'   environment variable. Default: `FALSE`.
#' @param refresh (boolean, optional) Whether to force the refresh of the cached
#'   token. Default: FALSE.
#'
#' @return A token which is either a string, for a personal access token, or a
#'   [httr::Token] object for an OAuth token.
#'
#' @examples
#' \dontrun{
#'
#'   token <- gh_token()
#'
#' }
#'
#' @export
#'
gh_token <- function(
  token   = getOption("github.token"),
  oauth   = getOption("github.oauth"),
  proxy   = getOption("github.proxy"),
  id      = getOption("githapi.id"),
  secret  = getOption("githapi.secret"),
  cache   = getOption("githapi.cache"),
  refresh = FALSE
) {
  if (!is_null(token)) {
    assert(
      is_sha(token) || "Token" %in% class(token),
      "'token' must be a SHA or a Token object:\n  ", token
    )
    info("> Using supplied token", level = 6)
    return(token)
  }

  if (!refresh && !is_null(.cache$token)) {
    info("> Retrieving cached token", level = 6)
    return(.cache$token)
  }

  assert(
    is_scalar_character(oauth),
    "'oauth' must be a string:\n  ", oauth
  )
  assert(
    is_scalar_character(id),
    "'id' must be a string:\n  ", id
  )
  assert(
    is_scalar_character(secret),
    "'secret' must be a string:\n  ", secret
  )
  assert(
    is_scalar_logical(cache) || is_scalar_character(cache),
    "'cache' must be a boolean or a string:\n  ", cache
  )

  if (!is_null(proxy)) {
    assert(
      is_scalar_character(proxy),
      "'proxy' must be a string:\n  ", proxy
    )
    httr::set_config(httr::use_proxy(proxy))
    httr::set_config(httr::config(connecttimeout = 60))
    on.exit(httr::reset_config())
  }

  github_endpoint <- httr::oauth_endpoint(
    authorize = file.path(oauth, "authorize"),
    access    = file.path(oauth, "access_token")
  )

  githapi_app <- httr::oauth_app(
    appname = "githapi",
    key     = id,
    secret  = secret
  )

  info("> Retrieving new token", level = 6)
  token <- try_catch(httr::oauth2.0_token(
    endpoint = github_endpoint,
    app      = githapi_app,
    scope    = c("admin:org", "user", "repo", "delete_repo", "gist"),
    cache    = cache
  ))

  if (
    "error" %in% names(token$credentials) &&
    nchar(token$credentials$error) > 0
  ) {
    error(
      "GitHub token request failed:\n",
      "\n[Error]       ", str_replace_all(token$credentials$error, "_", " "),
      "\n[Description] ", str_replace_all(
        token$credentials$error_description,
        "\\+", " "
      ),
      "\n[Details]     ", token$credentials$error_uri
    )
  }

  .cache$token <- token
  token
}


#  FUNCTION: gh_url ------------------------------------------------------------
#
#' Build the URL for the GitHub API
#'
#' This function is used to build the URL for the various endpoints in the
#' GitHub API. Unnamed strings are used to build the path upon the API and named
#' strings are added as queries.
#'
#' @param ... (strings, optional) unnamed strings are built up into a URL path
#'   and named parameters are added as queries.
#' @param api (string, optional) The URL of GitHub's API. Default: the value
#'   stored in the environment variable `GITHUB_API` or
#'   `https://api.github.com`.
#'
#' @return Valid URL (string)
#'
#' @examples
#' \dontrun{
#'
#'   # URL for all repositories
#'   gh_url("repos")
#'
#'   # URL for the main branch
#'   gh_url("repos", "ChadGoymer/githapi", "git/refs/heads", "main")
#'
#'   # URL for a file tree with the recursive option
#'   gh_url(
#'     c("repos", "ChadGoymer/githapi", "git/trees", "234752384"),
#'     list(recursive = 1)
#'   )
#'
#' }
#'
#' @export
#'
gh_url <- function(
  ...,
  api = getOption("github.api")
) {
  assert(
    is_url(api),
    "'api' must be a valid URL:\n  ", api
  )

  dots <- list(...) %>% compact() %>% unlist()

  path <- dots
  if (!is_null(names(path))) {
    path <- path[names(path) == ""]
  }
  if (!is.null(path)) {
    path <- str_c(path, collapse = "/") %>%
      str_split("/") %>%
      first() %>%
      map(curl::curl_escape) %>%
      str_c(collapse = "/")

    parsed_api <- httr::parse_url(api)
    if (!is_null(parsed_api$path)) {
      path <- str_c(parsed_api$path, "/", path)
    }
  }

  query <- dots[names(dots) != ""]
  if (!is.null(query)) {
    query <- as.list(query)
  }

  httr::modify_url(url = api, path = path, query = query)
}


#  FUNCTION: gh_request --------------------------------------------------------
#
#' Send an HTTP request to the GitHub API
#'
#' This function can be used to make "GET", "POST", "PATCH", "PUT" or "DELETE"
#' requests to the specified URL.
#'
#' The response is parsed from either JSON or plain text, depending on the
#' format received. When no response is received an empty list returned by the
#' function. Details of the response are recorded as attributes.
#'
#' For "POST", "PATCH", "PUT" and "DELETE" requests a payload can be supplied.
#' It is parsed into a JSON format before being sent to the URL.
#'
#' If an error is returned from the API then an error is thrown by this
#' function, detailing the URL, the HTTP status code and a message from the API,
#' if there is one.
#'
#' Finally, an authorisation token can be supplied if it is required.
#'
#' @param url (string) The address of the API endpoint.
#' @param type (string) The type of HTTP request. Either "GET", "POST", "PATCH",
#'   "PUT" or "DELETE".
#' @param payload (list, optional) The information to send to the API for
#'   "POST", "PATCH", "PUT" or "DELETE" requests. Default: `NULL`.
#' @param headers (character, optional) Headers to add to the request. Default:
#'   `NULL`.
#' @param accept (string, optional) The mime format to accept when making the
#'   call. Default: `"application/vnd.github.v3+json"`.
#' @param token (string or Token, optional) An authorisation token to include
#'   with the request. If `NULL` the OAuth process is triggered. Default:
#'   `NULL`.
#' @param proxy (character, optional) The proxy server to use to connect to the
#'   github API. If `NULL` then no proxy is used. Can be set in the option
#'   `github.proxy` or the environment variable `GITHUB_PROXY`. Default: `NULL`.
#' @param ... Ignored.
#'
#' @return A `github` list object consisting of the response, parsed into a
#'   list, with the attributes:
#'   - **url**: The URL the request was sent to
#'   - **request**: The type of HTTP request made
#'   - **status**: The HTTP status code returned
#'   - **header**: The HTTP header returned
#'
#' @examples
#' \dontrun{
#'
#'   # Create a tag
#'   gh_request(
#'     url     = "https://api.github.com/repos/ChadGoymer/githapi/git/refs",
#'     type    = "POST",
#'     payload = list(
#'       ref = "test-tag",
#'       sha = "a4b6545671455234757313a42738e44c10b0ef37"
#'     )
#'   )
#'
#'   # View a tag
#'   gh_request(
#'     url  = "https://api.github.com/repos/ChadGoymer/githapi/git/test-tag",
#'     type = "GET"
#'   )
#'
#'   # Update a tag
#'   gh_request(
#'     url     = "https://api.github.com/repos/ChadGoymer/githapi/git/test-tag",
#'     type    = "PATCH",
#'     payload = list(sha = "a4b6545671455234757313a42738e44c10b0ef37")
#'   )
#'
#'   # Delete a tag
#'   gh_request(
#'     url  = "https://api.github.com/repos/ChadGoymer/githapi/git/test-tag",
#'     type = "DELETE"
#'   )
#'
#' }
#'
#' @export
#'
gh_request <- function(
  url,
  type,
  payload = NULL,
  headers = NULL,
  accept  = "application/vnd.github.v3+json",
  token   = getOption("github.token"),
  proxy   = getOption("github.proxy"),
  ...
) {
  assert(
    is_url(url),
    "'url' must be a valid URL:\n  ", url
  )
  assert(
    is_scalar_character(type),
    "'type' must be a string:\n  ", type
  )
  assert(
    type %in% c("GET", "POST", "PATCH", "PUT", "DELETE"),
    "'type' must be either 'GET', 'POST', 'PATCH','PUT' or 'DELETE':\n  ", type
  )
  assert(
    is_null(headers) || is_character(headers),
    "'headers' must be a character vector:\n  ", headers
  )
  assert(
    is_scalar_character(accept),
    "'accept' must be a string:\n  ", accept
  )

  headers <- httr::add_headers(.headers = headers) %>%
    c(httr::accept(accept))

  if (!is_null(payload)) {
    assert(
      is_list(payload),
      "'payload' must be a list:\n  ", payload
    )
    info("> Parsing payload", level = 6)
    payload <- jsonlite::toJSON(
      x          = payload,
      auto_unbox = TRUE,
      null       = "null",
      na         = "null"
    )
    headers <- c(headers, httr::content_type_json())
  }

  token <- gh_token(proxy = proxy, token = token)
  if (is_sha(token)) {
    headers <- headers %>%
      c(httr::add_headers(Authorization = str_c("token ", token)))
  }
  else {
    headers <- headers %>%
      c(httr::config(token = token))
  }

  if (!is_null(proxy)) {
    assert(
      is_scalar_character(proxy),
      "'proxy' must be a string:\n  ", proxy
    )
    httr::set_config(httr::use_proxy(proxy))
    on.exit(httr::reset_config())
  }

  info("> ", type, ": ", url, level = 3)
  response <- switch(
    type,
    GET    = httr::GET(url,                    headers),
    POST   = httr::POST(url,   body = payload, headers),
    PATCH  = httr::PATCH(url,  body = payload, headers),
    PUT    = httr::PUT(url,    body = payload, headers),
    DELETE = httr::DELETE(url, body = payload, headers)
  )

  info("> Parsing response", level = 6)
  if (identical(httr::status_code(response), 204L)) {
    parsed_response <- list()
  }
  else if (identical(httr::http_type(response), "application/json")) {
    parsed_response <- response %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
  }
  else {
    parsed_response <- response %>%
      httr::content(as = "text", encoding = "UTF-8")
  }

  if (httr::http_error(response)) {
    message <- pluck(parsed_response, "message")
    errors  <- pluck(parsed_response, "errors") %>%
      map_chr(function(e) {
        if (is_null(e$message)) str_c(e, collapse = " ") else e$message
      })
    doc_url <- pluck(parsed_response, "documentation_url")

    msg <- str_replace_all(message, "\\n\\n", "\n  ") %>%
      c(errors) %>%
      str_c(collapse = "\n  ")

    error(
      "GitHub ", type, " request failed:\n",
      "\n[Status]  ", httr::status_code(response),
      "\n[URL]     ", url,
      "\n[Message] ", msg,
      "\n[Details] ", doc_url
    )
  }

  info("> Done", level = 9)
  structure(
    parsed_response,
    class   = c("github", class(parsed_response)),
    url     = url,
    request = type,
    status  = httr::status_code(response),
    header  = httr::headers(response)
  )
}


#  FUNCTION: gh_page -----------------------------------------------------------
#
#' Get multiple pages from the GitHub API
#'
#' This function is used when requesting a collection of entities. GitHub sets a
#' maximum page size of 100, so if more are request multiple requests are made
#' and the results are combined. Each page uses [gh_request()] to retrieve the
#' contents.
#'
#' @param url (string) The address of the API endpoint.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param page_size (integer, optional) The size of each page. Default: 100.
#' @param headers (character, optional) Headers to add to the request. Default:
#'   `NULL`.
#' @param accept (string, optional) The mime format to accept when making the
#'   call. Default: `"application/vnd.github.v3+json"`.
#' @param token (string or Token, optional) An authorisation token to include
#'   with the request. If `NULL` the OAuth process is triggered. Default:
#'   `NULL`.
#' @param proxy (character, optional) The proxy server to use to connect to the
#'   github API. If `NULL` then no proxy is used. Can be set in the option
#'   `github.proxy` or the environment variable `GITHUB_PROXY`. Default: `NULL`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A `github` list object consisting of the response, parsed into a
#'   list, with the attributes:
#'   - **url**: The URLs the request was sent to
#'   - **request**: The type of HTTP request made
#'   - **status**: The HTTP status code returned
#'   - **header**: The HTTP header returned
#'
#' @examples
#' \dontrun{
#'
#'   # First 20 users
#'   gh_page(
#'     url   = "https://api.github.com/users",
#'     n_max = 20
#'   )
#'
#'   # First 150 users (two pages)
#'   gh_page(
#'     url   = "https://api.github.com/users",
#'     n_max = 150
#'   )
#'
#' }
#'
#' @export
#'
gh_page <- function(
  url,
  n_max    = 1000,
  page_size = 100,
  headers   = NULL,
  accept    = "application/vnd.github.v3+json",
  token     = gh_token(),
  proxy     = getOption("github.proxy"),
  ...
) {
  assert(
    is_url(url),
    "'url' must be a valid URL:\n  ", url
  )
  assert(
    is_scalar_integerish(n_max) && isTRUE(n_max > 0),
    "'n_max' must be a positive integer:\n  ", n_max
  )
  assert(
    is_scalar_integerish(page_size) && isTRUE(page_size > 0),
    "'page_size' must be a positive integer:\n  ", page_size
  )
  assert(
    is_null(headers) || is_character(headers),
    "'headers' must be a character vector:\n  ", headers
  )
  assert(
    is_scalar_character(accept),
    "'accept' must be a string:\n  ", accept
  )
  assert(
    is_sha(token) || "Token" %in% class(token),
    "'token' must be a string or a Token object:\n  ", token
  )
  assert(
    is_null(proxy) || is_scalar_character(proxy),
    "'proxy' must be a string:\n  ", proxy
  )

  parsed_url <- httr::parse_url(url)
  per_page   <- c(rep(page_size, n_max %/% page_size), n_max %% page_size)

  response_list <- list()
  response_attr <- list()

  for (p in per_page[per_page > 0]) {
    parsed_url$query$per_page <- as.character(p)
    page_url <- httr::build_url(parsed_url)
    page <- gh_request(
      type    = "GET",
      url     = page_url,
      accept  = accept,
      token   = token,
      headers = headers,
      proxy   = proxy,
      ...
    )

    response_list        <- c(response_list, page)
    response_attr$url    <- c(response_attr$url, page_url)
    response_attr$status <- c(response_attr$status, attr(page, "status"))
    response_attr$header <- c(response_attr$header, attr(page, "header"))

    links <- attributes(page)[["header"]][["Link"]]
    if (
      length(response_list) < p ||
      is_null(links) ||
      !str_detect(links, "next")
    ) {
      info("> Returned ", length(response_list), level = 4)
      break
    }

    parsed_url <- links %>%
      str_split(", ") %>%
      first() %>%
      str_subset("next") %>%
      str_split(">") %>%
      pluck(1, 1) %>%
      str_remove("<") %>%
      httr::parse_url()
  }

  structure(
    response_list,
    class   = c("github", class(response_list)),
    url     = response_attr$url,
    request = "GET",
    status  = response_attr$status,
    header  = response_attr$header
  )
}


#  FUNCTION: gh_find -----------------------------------------------------------
#
#' Find an entity by matching a property value
#'
#' This function pages through a collection of entities searching for a
#' specified property value. It returns the first match found. For example, you
#' can search for an issue by specifying the title.
#'
#' @param url (string) The address of the API endpoint.
#' @param property (string) The property to search.
#' @param value (scalar) The property value to search for.
#' @param max_pages (integer, optional) The maximum number of pages to search
#'   through. Default: 100.
#' @param page_size (integer, optional) The size of each page. Default: 100.
#' @param headers (character, optional) Headers to add to the request. Default:
#'   `NULL`.
#' @param accept (string, optional) The mime format to accept when making the
#'   call. Default: `"application/vnd.github.v3+json"`.
#' @param token (string or Token, optional) An authorisation token to include
#'   with the request. If `NULL` the OAuth process is triggered. Default:
#'   `NULL`.
#' @param proxy (character, optional) The proxy server to use to connect to the
#'   github API. If `NULL` then no proxy is used. Can be set in the option
#'   `github.proxy` or the environment variable `GITHUB_PROXY`. Default: `NULL`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A `github` list object consisting of the response, parsed into a
#'   list, with the attributes:
#'   - **url**: The URLs the request was sent to
#'   - **request**: The type of HTTP request made
#'   - **status**: The HTTP status code returned
#'   - **header**: The HTTP header returned
#'
#' @examples
#' \dontrun{
#'
#'   # Find an issue by title
#'   gh_find(
#'     url      = "https://api.github.com/repos/ChadGoymer/githapi/issues",
#'     property = "title",
#'     value    = "Test issue"
#'   )
#'
#' }
#'
#' @export
#'
gh_find <- function(
  url,
  property,
  value,
  max_pages = 100,
  page_size = 100,
  headers   = NULL,
  accept    = "application/vnd.github.v3+json",
  token     = gh_token(),
  proxy     = getOption("github.proxy"),
  ...
) {
  assert(
    is_url(url),
    "'url' must be a valid URL:\n  ", url
  )
  assert(
    is_scalar_character(property),
    "'property' must be a string:\n  ", property
  )
  assert(
    is_scalar_atomic(value),
    "'value' must be a scalar:\n  ", value
  )
  assert(
    is_scalar_integerish(max_pages) && isTRUE(max_pages > 0),
    "'max_pages' must be a positive integer:\n  ", max_pages
  )
  assert(
    is_scalar_integerish(page_size) && isTRUE(page_size > 0),
    "'page_size' must be a positive integer:\n  ", page_size
  )
  assert(
    is_null(headers) || is_character(headers),
    "'headers' must be a character vector:\n  ", headers
  )
  assert(
    is_scalar_character(accept),
    "'accept' must be a string:\n  ", accept
  )
  assert(
    is_sha(token) || "Token" %in% class(token),
    "'token' must be a string or a Token object:\n  ", token
  )
  assert(
    is_null(proxy) || is_scalar_character(proxy),
    "'proxy' must be a string:\n  ", proxy
  )

  parsed_url <- httr::parse_url(url)
  parsed_url$query$per_page <- as.character(page_size)
  page_url <- httr::build_url(parsed_url)

  for (p in 1:max_pages) {
    page <- gh_request(
      type    = "GET",
      url     = page_url,
      accept  = accept,
      token   = token,
      headers = headers,
      proxy   = proxy,
      ...
    )

    matched_results <- keep(page, ~.[[property]] == as.character(value))

    if (length(matched_results) > 0) {
      result <- structure(
        matched_results[[1]],
        class   = c("github", class(matched_results)),
        url     = page_url,
        request = "GET",
        status  = attr(page, "status"),
        header  = attr(page, "header")
      )

      return(result)
    }

    if (is_null(attributes(page)[["header"]][["Link"]])) {
      break
    }

    page_url <- attributes(page)[["header"]][["Link"]] %>%
      str_split(", ") %>%
      first() %>%
      str_subset("next") %>%
      str_split(">") %>%
      pluck(1, 1) %>%
      str_remove("<")
  }

  error("Could not find an entity with '", property, "' equal to '", value, "'")
}


#  FUNCTION: gh_download -------------------------------------------------------
#
#' Download a file from GitHub
#'
#' This function downloads a file from GitHub in as a binary object.
#'
#' @param url (string) The address of the API endpoint.
#' @param path (string) The path to download the file to.
#' @param headers (character, optional) Headers to add to the request. Default:
#'   `NULL`.
#' @param accept (string, optional) The mime format to accept when making the
#'   call. Default: `NULL`.
#' @param token (string or Token, optional) An authorisation token to include
#'   with the request. If `NULL` the OAuth process is triggered. Default:
#'   `NULL`.
#' @param proxy (character, optional) The proxy server to use to connect to the
#'   github API. If `NULL` then no proxy is used. Can be set in the option
#'   `github.proxy` or the environment variable `GITHUB_PROXY`. Default: `NULL`.
#' @param ... Ignored.
#'
#' @return A `github` string object containing the path, with the attributes:
#'   - **url**: The URLs the request was sent to
#'   - **request**: The type of HTTP request made
#'   - **status**: The HTTP status code returned
#'   - **header**: The HTTP header returned
#'
#' @examples
#' \dontrun{
#'
#'   # Find an issue by title
#'   gh_download(
#'     url  = "https://api.github.com/repos/ChadGoymer/githapi/zipball/main",
#'     path = "~/githapi-main.zip"
#'   )
#'
#' }
#'
#' @export
#'
gh_download <- function(
  url,
  path,
  headers = NULL,
  accept  = NULL,
  token   = getOption("github.token"),
  proxy   = getOption("github.proxy"),
  ...
) {
  assert(
    is_url(url),
    "'url' must be a valid URL:\n  ", url
  )
  assert(
    is_dir(dirname(path)) && is_writeable(dirname(path)),
    "'path' must be a writeable path:\n  ", path
  )
  assert(
    is_null(headers) || is_character(headers),
    "'headers' must be a character vector:\n  ", headers
  )

  headers <- httr::add_headers(.headers = headers)

  if (!is_null(accept)) {
    headers <- c(headers, httr::accept(accept))
  }

  token <- gh_token(proxy = proxy, token = token)
  if (is_sha(token)) {
    headers <- c(
      headers,
      httr::add_headers(Authorization = str_c("token ", token))
    )
  }
  else {
    headers <- c(headers, httr::config(token = token))
  }

  if (!is_null(proxy)) {
    assert(
      is_scalar_character(proxy),
      "'proxy' must be a string:\n  ", proxy
    )
    httr::set_config(httr::use_proxy(proxy))
    on.exit(httr::reset_config())
  }

  info("> DOWNLOAD: ", url, level = 3)
  response <- httr::GET(
    url,
    httr::write_disk(path, overwrite = TRUE),
    headers
  )

  if (httr::http_error(response)) {
    parsed_response <- httr::content(
      x        = response,
      as       = "text",
      encoding = "UTF-8"
    ) %>%
      jsonlite::fromJSON(simplifyVector = FALSE)

    message <- pluck(parsed_response, "message")
    errors  <- pluck(parsed_response, "errors") %>%
      map_chr(function(e) {
        if (is_null(e$message)) str_c(e, collapse = " ") else e$message
      })
    doc_url <- pluck(parsed_response, "documentation_url")

    msg <- str_replace_all(message, "\\n\\n", "\n  ") %>%
      c(errors) %>%
      str_c(collapse = "\n  ")

    error(
      "GitHub GET request failed:\n",
      "\n[Status]  ", httr::status_code(response),
      "\n[URL]     ", url,
      "\n[Message] ", msg,
      "\n[Details] ", doc_url
    )
  }

  structered_path <- structure(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    class   = c("github", "character"),
    url     = url,
    request = "GET",
    status  = httr::status_code(response),
    header  = httr::headers(response)
  )

  info("> Done", level = 9)
  invisible(structered_path)
}


#  FUNCTION: print.github ------------------------------------------------------
#
#  Print method for the `github` class
#
#' @export
#'
print.github <- function(x, ...) {
  urls <- attr(x, "url")

  if (!is_null(urls)) {
    cat(
      "\033[34m",
      str_c(
        "# ", attr(x, "request"), " \033[4m",
        str_replace_all(urls[[1]], "%20", " "), "\033[24m\n"
      ),
      "\033[39m",
      sep = ""
    )
  }

  if (is.data.frame(x)) {
    class(x) <- class(x)[class(x) != "github"]
    print(x)
  }
  else if (is_list(x)) {
    utils::capture.output(
      utils::str(x, max.level = 2, give.attr = FALSE, strict.width = "cut")
    ) %>%
      str_replace("\\.\\.\\$", " ") %>%
      str_replace("\\$", "") %>%
      cat(sep = "\n")
  }
  else {
    added_attributes <- c(
      "class", "url", "request", "status", "header", "github"
    )
    attributes(x) <- attributes(x)[!names(attributes(x)) %in% added_attributes]
    print.default(x, ...)
  }

  invisible(x)
}
