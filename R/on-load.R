.onLoad <- function(libname, pkgname) {

  # Read configuration file
  if (
    Sys.getenv("GITHAPI_CONFIG") != "" &&
    file.exists(Sys.getenv("GITHAPI_CONFIG"))
  ) {
    config_path <- Sys.getenv("GITHAPI_CONFIG")
  } else {
    config_path <- system.file("config.json", package = "githapi")
  }
  config <- jsonlite::read_json(config_path, simplifyVector = TRUE)

  if (Sys.getenv("ENVIRONMENT") %in% names(config)) {
    config <- config[[Sys.getenv("ENVIRONMENT")]]
  } else {
    config <- config[[1]]
  }

  if ("github" %in% names(config)) {
    gh_cfg <- config[["github"]]
  } else {
    warning("GitHub configuration not set in specified configuration file")
    gh_cfg <- list()
  }

  if (Sys.getenv("GITHAPI_APP") %in% names(config)) {
    app_cfg <- config[[Sys.getenv("GITHAPI_APP")]]
  } else {
    app_cfg <- config[names(config) != "github"][[1]]
  }

  # Override github API URL
  if (is.null(getOption("github.api"))) {
    if (Sys.getenv("GITHUB_API_URL") != "") {
      gh_cfg$api <- Sys.getenv("GITHUB_API_URL")
    }

    options(github.api = gh_cfg$api)
  }

  # Override github OAuth URL
  if (is.null(getOption("github.oauth"))) {
    if (Sys.getenv("GITHUB_OAUTH") != "") {
      gh_cfg$oauth <- Sys.getenv("GITHUB_OAUTH")
    }

    options(github.oauth = gh_cfg$oauth)
  }

  # Override github proxy URL
  if (is.null(getOption("github.proxy"))) {
    if (Sys.getenv("GITHUB_PROXY") != "") {
      gh_cfg$proxy <- Sys.getenv("GITHUB_PROXY")
    }

    options(github.proxy = gh_cfg$proxy)
  }

  # Set github token
  if (is.null(getOption("github.token"))) {
    tokens <- c(
      Sys.getenv("GITHAPI_TOKEN"),
      Sys.getenv("GITHUB_PAT"),
      Sys.getenv("GITHUB_TOKEN")
    )

    if (any(tokens != "")) {
      gh_cfg$token <- tokens[tokens != ""][[1]]
    }

    options(github.token = gh_cfg$token)
  }

  # Override githapi application ID
  if (is.null(getOption("githapi.id"))) {
    if (Sys.getenv("GITHAPI_ID") != "") {
      app_cfg$id <- Sys.getenv("GITHAPI_ID")
    }

    options(githapi.id = app_cfg$id)
  }

  # Override githapi application secret
  if (is.null(getOption("githapi.secret"))) {
    if (Sys.getenv("GITHAPI_SECRET") != "") {
      app_cfg$secret <- Sys.getenv("GITHAPI_SECRET")
    }

    options(githapi.secret = app_cfg$secret)
  }

  # Override githapi application token cache location
  if (is.null(getOption("githapi.cache"))) {
    if (Sys.getenv("GITHAPI_CACHE") != "") {
      app_cfg$cache <- Sys.getenv("GITHAPI_CACHE")
    }

    options(githapi.cache = app_cfg$cache)
  }

}
