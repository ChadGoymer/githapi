#  FUNCTION: upload_commit -----------------------------------------------------
#
#' Upload a directory of files and create a commit
#'
#' This function uploads all the files in the directory (except those in the
#' `ignore` argument) and creates a new commit on the specified branch. Note:
#' the new commit is created with exactly the files uploaded, so if a file in
#' the parent commit has not been uploaded it will be removed in the new commit.
#'
#' The `author` and `committer` arguments are optional and if not supplied the
#' current authenticated user is used. However, if you want to set them
#' explicitly you must specify a named list with `name` and `email` as the
#' elements (see examples).
#'
#' A commit may have none, one or two parents. If none are specified and the
#' branch already exists, the head of the branch will be used as a parent. If
#' the branch does not exist, an orphan commit is created without a parent. If
#' one parent is specified the commit is added and the branch set to the new
#' commit, if it is a simple fast-forward. If you want to set the branch to the
#' new commit no matter what then set the `force` argument to `TRUE`. If two
#' parents are specified then a merge commit is created.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "git#create-a-blob",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "git#create-a-tree",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "git#create-a-commit",
#'   ">"
#' ))
#' ```
#'
#' @param path (string) The path to the directory to upload. It must be
#'   readable.
#' @param branch (string) The name of the branch to make the new commit on.
#' @param message (string) The commit message.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param author (list, optional) A the name and email address of the user who
#'   wrote the changes in the commit.
#' @param committer (list, optional) A the name and email address of the user
#'   who created the commit.
#' @param parents (character, optional) References for the commits to use as
#'   parents, can be either a SHA, branch or tag. If it is a branch then the
#'   head commit is used. See the details section for more information.
#' @param ignore (character, optional) The files to ignore in the directory.
#'   Default: `".git"`, `".Rproj.user"`, `".Rhistory"`, `".RData"` and
#'   `".Ruserdata"`.
#' @param force (boolean, optional) Whether to force the update if it is not a
#'   simple fast-forward. Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `upload_commit()` returns a list of the commit properties.
#'
#' **Commit Properties:**
#'
#' - **sha**: The commit SHA.
#' - **message**: The commit message.
#' - **author_name**: The author's name.
#' - **author_email**: The author's email address.
#' - **committer_name**: The committer's name.
#' - **committer_email**: The committer's email address.
#' - **tree_sha**: The SHA of the file tree.
#' - **parent_sha**: The commit SHA of the parent(s).
#' - **date**: The date the commit was made.
#'
#' @examples
#' \dontrun{
#'
#'   # Add a commit to the main branch
#'   upload_commit(
#'     path    = "C:/files-to-upload",
#'     branch  = "main",
#'     message = "Commit to test upload_commit()",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # override the author and committer
#'   upload_commit(
#'     path      = "C:/files-to-upload",
#'     branch    = "main",
#'     message   = "Commit to test upload_commit()",
#'     repo      = "ChadGoymer/githapi",
#'     author    = list(name = "Bob",   email = "bob@acme.com"),
#'     committer = list(name = "Jane",  email = "jane@acme.com")
#'   )
#'
#'   # Create a commit on a new branch
#'   upload_commit(
#'     path    = "C:/files-to-upload",
#'     branch  = "test-commits-1",
#'     message = "Commit to test upload_commit()",
#'     repo    = "ChadGoymer/githapi",
#'     parents = "main"
#'   )
#'
#'   # Create an orphan commit
#'   upload_commit(
#'     path    = "C:/files-to-upload",
#'     branch  = "test-commits-2",
#'     message = "Commit to test upload_commit()",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Force branch to point at the new commit
#'   upload_commit(
#'     path    = "C:/files-to-upload",
#'     branch  = "main",
#'     message = "Commit to test upload_commit()",
#'     repo    = "ChadGoymer/githapi",
#'     parents = "test-commits-1",
#'     force   = TRUE
#'   )
#'
#'   # Create a commit merging a branch into the main branch
#'   upload_commit(
#'     path    = "C:/files-to-upload",
#'     branch  = "main",
#'     message = "Commit to test upload_commit()",
#'     repo    = "ChadGoymer/githapi",
#'     parents = c("main", "test-commits-1")
#'   )
#'
#'   # Create a commit merging two branches into a new branch
#'   upload_commit(
#'     path    = "C:/files-to-upload",
#'     branch  = "test-commits-3",
#'     message = "Commit to test upload_commit()",
#'     repo    = "ChadGoymer/githapi",
#'     parents = c("main", "test-commits-2")
#'   )
#'
#' }
#'
#' @export
#'
upload_commit <- function(
  path,
  branch,
  message,
  repo,
  author,
  committer,
  parents,
  ignore = c(
    "\\.git", "\\.Rproj\\.user", "\\.Rhistory", "\\.RData", "\\.Ruserdata"
  ),
  force  = FALSE,
  ...
) {
  assert(
    is_dir(path) && is_readable(path),
    "'path' must be a readable directory path:\n  ", path
  )
  assert(
    is_ref(branch),
    "'branch' must be a valid git reference - see help(is_ref):\n  ", branch
  )
  assert(
    is_scalar_character(message),
    "'message' must be a string:\n  ", message
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )
  assert(
    is_character(ignore),
    "'ignore' must be a character vector:\n  ", ignore
  )
  assert(
    is_scalar_logical(force),
    "'force' must be a boolean:\n  ", force
  )

  payload <- list(message = message)

  if (!is_missing_or_null(author)) {
    assert(
      is_list(author) &&
        is_scalar_character(author$name) &&
        is_scalar_character(author$email),
      "'author' must be a list containing 'name' and 'email':\n ", author
    )
    payload$author <- author
  }

  if (!is_missing_or_null(committer)) {
    assert(
      is_list(committer) &&
        is_scalar_character(committer$name) &&
        is_scalar_character(committer$email),
      "'committer' must be a list containing 'name' and 'email':\n ", committer
    )
    payload$committer <- committer
  }

  info("Uploading files in '", path, "' to repository '", repo, "'")
  payload$tree <- upload_tree(
    path   = path,
    repo   = repo,
    ignore = ignore,
    ...
  ) %>%
    pluck("tree_sha")

  branch_sha <- try_catch(
    view_sha(ref = branch, repo = repo, ...),
    on_error = function(e) NULL
  )

  if (is_missing_or_null(parents)) {
    if (is_null(branch_sha)) {
      parents <- NULL
    }
    else {
      parents <- as.list(branch_sha)
    }
  }
  else {
    assert(
      is_character(parents),
      "'parents' must be a character vector:\n  ", parents
    )
    parents <- map(parents, function(p) {
      if (!is_sha(p)) view_sha(ref = p, repo = repo, ...) else p
    })
  }
  payload$parents <- parents

  info("Creating commit in repo '", repo, "'")
  commit_lst <- gh_url("repos", repo, "git/commits") %>%
    gh_request("POST", payload = payload, ...)

  if (is_null(branch_sha)) {
    create_branch(name = branch, ref = commit_lst$sha, repo = repo, ...)
  }
  else {
    update_branch(
      branch = branch,
      ref    = commit_lst$sha,
      repo   = repo,
      force  = force,
      ...
    )
  }

  view_commit(commit_lst$sha, repo = repo, ...)
}


#  FUNCTION: download_commit ---------------------------------------------------
#
#' Download a commit from GitHub
#'
#' This function downloads all the files in a commit, plus any folders, into the
#' path specified.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#download-a-repository-archive-zip",
#'   ">"
#' ))
#' ```
#'
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param path (string, optional) The path to the directory to upload. It must
#'   be readable. Default: working directory.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `download_commit()` returns the path where the commit is downloaded
#'   to.
#'
#' @examples
#' \dontrun{
#'
#'   # Download the head of the main branch to the home directory
#'   download_commit(
#'     ref  = "main",
#'     repo = "ChadGoymer/githapi",
#'     path = "~"
#'   )
#'
#' }
#'
#' @export
#'
download_commit <- function(
  ref,
  repo,
  path = getwd(),
  ...
) {
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  if (!file.exists(path)) dir.create(path, recursive = TRUE)

  archive_path <- tempfile("", tmpdir = path, fileext = ".zip")
  on.exit(unlink(archive_path, recursive = TRUE))

  info("Downloading commit '", ref, "' from repository '", repo, "'")
  path_gh <- gh_url("repos", repo, "zipball", ref) %>%
    gh_download(archive_path, ...)

  info("Unpacking commit into '", path, "'", level = 3)
  archive_paths <- utils::unzip(archive_path, list = TRUE)
  extract_path  <- archive_paths$Name[[1]]
  archive_paths <- archive_paths %>%
    mutate(
      from = file.path(path, .data$Name),
      to   = file.path(path, str_remove(.data$Name, extract_path))
    )

  utils::unzip(archive_path, exdir = path)
  on.exit(unlink(file.path(path, extract_path), recursive = TRUE), add = TRUE)

  filter(archive_paths, .data$Length == 0) %>%
    pull("to") %>%
    utils::tail(-1) %>%
    walk(dir.create)

  results <- filter(archive_paths, .data$Length > 0) %>%
    pmap_lgl(function(from, to, ...) file.rename(from, to))

  assert(all(results), "Not all files were extract successfully")

  info("Done", level = 3)
  structure(
    normalizePath(path, winslash = "/"),
    class   = class(path_gh),
    url     = attr(path_gh, "url"),
    request = attr(path_gh, "request"),
    status  = attr(path_gh, "status"),
    header  = attr(path_gh, "header")
  )
}


#  FUNCTION: view_commits ------------------------------------------------------
#
#' View commits within a repository
#'
#' `view_commits()` summarises commits in a table with the properties as columns
#' and a row for each commit in the history of the given reference.
#' `view_commit()` returns a list of all properties for a single commit.
#' `browse_commits()` and `browse_commit()` opens the web page for the commit
#' history and commit details respectively in the default browser.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#list-commits",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#get-a-commit",
#'   ">"
#' ))
#' ```
#'
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param path (string, optional) Only commits containing this file path will be
#'   returned.
#' @param author (string, optional) Author login to filter commits by.
#' @param since (string, optional) A date & time to filter by. Must be in the
#'   format: `YYYY-MM-DD HH:MM:SS`.
#' @param until (string, optional) A date & time to filter by. Must be in the
#'   format: `YYYY-MM-DD HH:MM:SS`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_commits()` returns a tibble of commit properties.
#'   `view_commit()` returns a list of properties for a single commit.
#'   `browse_commits()` and `browse_commit` opens the default browser on the
#'   commit's history or details page and returns the URL.
#'
#' **Commit Properties:**
#'
#' - **sha**: The commit SHA.
#' - **message**: The commit message.
#' - **author_login**: The author's account login.
#' - **author_name**: The author's name.
#' - **author_email**: The author's email address.
#' - **committer_login**: The committer's account login.
#' - **committer_name**: The committer's name.
#' - **committer_email**: The committer's email address.
#' - **tree_sha**: The SHA of the commit's file tree.
#' - **parents**: The SHAs of the parent commits.
#' - **date**: The time and date the commit was made.
#' - **html_url**: The address of the commit's web page.
#'
#' @examples
#' \dontrun{
#'
#'   # View the history of commits for the main branch
#'   view_commits("main", "ChadGoymer/githapi")
#'
#'   # View commits where the README.md file has been changed
#'   view_commits(
#'     ref  = "main",
#'     repo = "ChadGoymer/githapi",
#'     path = "README.md"
#'   )
#'
#'   # View commits created by an author
#'   view_commits(
#'     ref    = "main",
#'     repo   = "ChadGoymer/githapi",
#'     author = "ChadGoymer"
#'   )
#'
#'   # View commits within a time window
#'   view_commits(
#'     ref   = "main",
#'     repo  = "ChadGoymer/githapi",
#'     since = "2020-01-01 00:00:00",
#'     until = "2020-04-01 00:00:00"
#'   )
#'
#'   # View the properties of the last commit on the main branch
#'   view_commit("main", "ChadGoymer/githapi")
#'
#'   # View the properties of the commit with tag "0.8.7"
#'   view_commit("0.8.7", "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
view_commits <- function(
  ref,
  repo,
  path,
  author,
  since,
  until,
  n_max = 1000,
  ...
) {
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  if (!is_missing_or_null(path)) {
    assert(
      is_scalar_character(path),
      "'path' must be a string:\n  ", path
    )
    path <- str_c(path, collapse = ",")
  }
  else {
    path <- NULL
  }

  if (!is_missing_or_null(author)) {
    assert(
      is_scalar_character(author),
      "'author' must be a string:\n  ", author
    )
    author <- str_c(author, collapse = ",")
  }
  else {
    author <- NULL
  }

  if (!is_missing_or_null(since)) {
    assert(
      is_scalar_character(since),
      "'since' must be a string:\n  ", since
    )
    since <- as.POSIXct(since, format = "%Y-%m-%d %H:%M:%S") %>%
      format("%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    assert(
      !is.na(since),
      "'since' must be specified in the format 'YYYY-MM-DD hh:mm:ss':\n  ",
      since
    )
  }
  else {
    since <- NULL
  }

  if (!is_missing_or_null(until)) {
    assert(
      is_scalar_character(until),
      "'until' must be a string:\n  ", until
    )
    until <- as.POSIXct(until, format = "%Y-%m-%d %H:%M:%S") %>%
      format("%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    assert(
      !is.na(until),
      "'until' must be specified in the format 'YYYY-MM-DD hh:mm:ss':\n  ",
      until
    )
  }
  else {
    until <- NULL
  }

  info("Viewing commits for reference '", ref, "' in repository '", repo, "'")
  commits_lst <- gh_url(
    "repos", repo, "commits",
    sha    = ref,
    path   = path,
    author = author,
    since  = since,
    until  = until
  ) %>%
    gh_page(n_max = n_max, ...)

  info("Transforming results", level = 4)
  commits_gh <- bind_properties(commits_lst, properties$commit) %>%
    add_column(
      parents = map(commits_lst, ~ map_chr(.$parents, "sha")),
      .before = "html_url"
    )

  info("Done", level = 7)
  commits_gh
}


#  FUNCTION: view_commit -------------------------------------------------------
#
#' @rdname view_commits
#' @export
#'
view_commit <- function(
  ref,
  repo,
  ...
) {
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Viewing commit for reference '", ref, "' in repository '", repo, "'")
  commit_lst <- gh_url("repos", repo, "commits", ref) %>%
    gh_request("GET", ...)

  info("Transforming results", level = 4)
  commit_gh <- select_properties(commit_lst, properties$commit) %>%
    modify_list(
      parents = map_chr(commit_lst$parents, "sha"),
      .before = "html_url"
    )

  info("Done", level = 7)
  commit_gh
}


#  FUNCTION: browse_commits ----------------------------------------------------
#
#' @rdname view_commits
#' @export
#'
browse_commits <- function(
  ref,
  repo,
  ...
) {
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Browsing commit '", ref, "' in repository '", repo, "'")
  commit <- gh_url("repos", repo, "commits", ref) %>% gh_request("GET", ...)

  commits_url <- commit$html_url %>%
    str_replace(str_c(repo, "/", "commit"), str_c(repo, "/", "commits"))

  httr::BROWSE(commits_url)

  info("Done", level = 7)
  structure(
    commits_url,
    class   = c("github", "character"),
    url     = attr(commit, "url"),
    request = attr(commit, "request"),
    status  = attr(commit, "status"),
    header  = attr(commit, "header")
  )
}


#  FUNCTION: browse_commit -----------------------------------------------------
#
#' @rdname view_commits
#' @export
#'
browse_commit <- function(
  ref,
  repo,
  ...
) {
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Browsing commit '", ref, "' in repository '", repo, "'")
  commit <- gh_url("repos", repo, "commits", ref) %>% gh_request("GET", ...)
  httr::BROWSE(commit$html_url)

  info("Done", level = 7)
  structure(
    commit$html_url,
    class   = c("github", "character"),
    url     = attr(commit, "url"),
    request = attr(commit, "request"),
    status  = attr(commit, "status"),
    header  = attr(commit, "header")
  )
}


#  FUNCTION: view_sha ----------------------------------------------------------
#
#' View the SHA for a commit
#'
#' This function returns the commit SHA given a git reference. A reference can
#' be either a SHA, branch or tag. If it is a branch then the head commit is
#' used.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#get-a-commit",
#'   ">"
#' ))
#' ```
#'
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `view_sha()` returns the commit SHA as a string.
#'
#' @examples
#' \dontrun{
#'
#'   view_sha("a-tag", repo = "ChadGoymer/githapi")
#'   view_sha("a-branch", repo = "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
view_sha <- function(
  ref,
  repo,
  ...
) {
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Viewing SHA for ref '", ref, "' from repository '", repo, "'")
  gh_url("repos", repo, "commits", ref) %>%
    gh_request("GET", accept = "application/vnd.github.VERSION.sha", ...)
}


#  FUNCTION: compare_commits ---------------------------------------------------
#
#' View commits made between two commits
#'
#' `compare_commits()` summarises the commits made between two commits. It
#' returns a list of statistics along with a table of the commits with the
#' properties as columns and a row for each commit. The `base` commit must be in
#' the history of the `head` commit.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#compare-two-commits",
#'   ">"
#' ))
#' ```
#'
#' @param head (string) Either a SHA, branch or tag used to identify the head
#'   commit.
#' @param base (string) Either a SHA, branch or tag used to identify the base
#'   commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `compare_commits()` returns a list of comparison properties.
#'
#' **Comparison Properties:**
#'
#' - **status**: The status of the comparison, can be `"ahead"` `"behind"` or
#'   `"diverged"`.
#' - **ahead_by**: The number of commits the `head` commit is ahead of the
#'   `base` commit.
#' - **behind_by**: The number of commits the `head` commit is behind the
#'   `base` commit.
#' - **total_commits**: The total number of commits between the `head` commit
#'   and the `base` commit.
#' - **html_url**: The address of the commit comparison web page.
#' - **commits**: A tibble summarising the commits between the `head` commit
#'   and the `base` commit.
#'
#' **Commit Properties:**
#'
#' - **sha**: The commit SHA.
#' - **message**: The commit message.
#' - **author_login**: The author's account login.
#' - **author_name**: The author's name.
#' - **author_email**: The author's email address.
#' - **committer_login**: The committer's account login.
#' - **committer_name**: The committer's name.
#' - **committer_email**: The committer's email address.
#' - **tree_sha**: The SHA of the commit's file tree.
#' - **parents**: The SHAs of the parent commits.
#' - **date**: The time and date the commit was made.
#' - **html_url**: The address of the commit's web page.
#'
#' @examples
#' \dontrun{
#'
#'   # View the changes made between the current main branch and a release
#'   compare_commits("main", "0.8.7", "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
compare_commits <- function(
  base,
  head,
  repo,
  ...
) {
  assert(
    is_ref(base),
    "'base' must be a valid git reference - see help(is_ref):\n  ", base
  )
  assert(
    is_ref(head),
    "'head' must be a valid git reference - see help(is_ref):\n  ", head
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info(
    "Comparing commit '", head, "' with '", base,
    "' in repository '", repo, "'"
  )
  comparison_lst <- gh_url(
    "repos", repo, "compare", str_c(base, "...", head)
  ) %>%
    gh_request("GET", ...)

  info("Transforming results", level = 4)
  commits_gh <- bind_properties(comparison_lst$commits, properties$commit) %>%
    add_column(
      parents = map(comparison_lst$commits, ~ map_chr(.$parents, "sha")),
      .before = "html_url")

  comparison_gh <- select_properties(
    comparison_lst,
    properties$compare_commits
  ) %>%
    modify_list(commits = commits_gh)

  info("Done", level = 7)
  structure(
    comparison_gh,
    class   = c("github", class(comparison_gh)),
    url     = attr(comparison_lst, "url"),
    request = attr(comparison_lst, "request"),
    status  = attr(comparison_lst, "status"),
    header  = attr(comparison_lst, "header")
  )
}
