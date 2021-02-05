# FUNCTION: upload_blob --------------------------------------------------------
#
# Read a file and upload it to GitHub
#
# For more details see the GitHub API documentation:
#
# ```{r echo=FALSE, results='asis'}
# docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
# cat(paste0(
#   "- <", docs_url,
#   "git#create-a-blob",
#   ">"
# ))
# ```
#
# @param path (string) The path to the file to upload. It must be readable.
# @param repo (string) The repository specified in the format: `owner/repo`.
# @param ... Parameters passed to [gh_request()].
#
# @return `upload_blob()` returns a list of the blob's properties.
#
upload_blob <- function(
  path,
  repo,
  ...
) {
  assert(
    is_file(path) && is_readable(path),
    "'path' must be a readable file path:\n  ", path
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info(
    "Uploading file '", basename(path), "' to repository '", repo, "'",
    level = 2
  )
  content <- jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size))

  gh_url("repos", repo, "git/blobs") %>%
    gh_request(
      "POST",
      payload = list(content = content, encoding = "base64"),
      ...
    )
}


# FUNCTION: upload_tree --------------------------------------------------------
#
# Upload a directory of files as a tree
#
# For more details see the GitHub API documentation:
#
# ```{r echo=FALSE, results='asis'}
# docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
# cat(paste0(
#   "- <", docs_url,
#   "git#create-a-blob",
#   ">"
# ))
# ```
# ```{r echo=FALSE, results='asis'}
# cat(paste0(
#   "- <", docs_url,
#   "git#create-a-tree",
#   ">"
# ))
# ```
#
# @param path (string) The path to the directory to upload. It must be readable.
# @param repo (string) The repository specified in the format: `owner/repo`.
# @param base_commit (string, optional) Either a SHA, branch or tag used to
#   identify the commit to base the specified file change to. If not supplied
#   the tree will just contain the files specified.
# @param placeholder (boolean, optional) Whether the files are placeholders,
#   containing the SHA of the blob, of the actual contents. Default: `FALSE`.
# @param ignore (character, optional) The files to ignore in the directory.
#   Default: `".git"`, `".Rproj.user"`, `".Rhistory"`, `".RData"` and
#   `".Ruserdata"`.
# @param ... Parameters passed to [gh_request()].
#
# @return `upload_tree()` returns a list containing the tree SHA and the base
#   commit SHA.
#
upload_tree <- function(
  path,
  repo,
  base_commit = NULL,
  placeholder = FALSE,
  ignore      = c(".git", ".Rproj.user", ".Rhistory", ".RData", ".Ruserdata"),
  ...
) {
  assert(
    is_dir(path) && is_readable(path),
    "'path' must be a readable directory path:\n  ", path
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )
  assert(
    is_character(ignore),
    "'ignore' must be a character vector:\n  ", ignore
  )

  ignore <- unique(c(".", "..", ignore))

  info("Uploading files in '", path, "' to repository '", repo, "'", level = 2)
  tree <- list.files(
    path         = path,
    all.files    = TRUE,
    include.dirs = TRUE,
    full.names   = TRUE
  ) %>%
    discard(~ basename(.) %in% ignore) %>%
    file.info() %>%
    rownames_to_column("path") %>%
    mutate(sha = map2_chr(.data$path, .data$isdir, function(path, isdir) {
      if (isdir) {
        upload_tree(
          path = path,
          repo = repo,
          placeholder = placeholder,
          ignore = ignore,
          ...
        ) %>%
          pluck("tree_sha")
      }
      else {
        if (placeholder) {
          readr::read_lines(file = path)
        }
        else {
          upload_blob(path = path, repo = repo, ...)$sha
        }
      }
    })) %>%
    mutate(
      type = ifelse(.data$isdir, "tree", "blob"),
      mode = ifelse(.data$isdir, "040000", "100644"),
      path = basename(path)
    ) %>%
    select("path", "mode", "type", "sha")

  payload <- tibble(
    path = tree$path,
    mode = tree$mode,
    type = tree$type,
    sha  = tree$sha
  ) %>%
    pmap(list) %>%
    list(tree = .)

  if (!is_null(base_commit)) {
    base_commit <- try_catch(
      view_commit(ref = base_commit, repo = repo, ...),
      on_error = function(e) NULL
    )
    payload$base_tree <- base_commit$tree_sha
  }

  info("Creating tree in repository '", repo, "'", level = 2)
  tree <- gh_url("repos", repo, "git/trees") %>%
    gh_request("POST", payload = payload, ...)

  list(commit_sha = base_commit$sha, tree_sha = tree$sha)
}


#  FUNCTION: upload_files ------------------------------------------------------
#
#' Upload files and create a commit
#'
#' This function uploads the specified files to a repository in GitHub and
#' creates a commit on the specified branch.
#'
#' This function uploads the specified files to a repository and creates a new
#' commit on the specified branch. Note: the files are created, or updated if
#' they already exist, and any other files in the parent are left unchanged.
#'
#' The `author` and `committer` arguments are optional and if not supplied the
#' current authenticated user is used. However, if you want to set them
#' explicitly you must specify a named list with `name` and `email` as the
#' elements (see examples).
#'
#' If a parent has been specified the files are created or updated. If a parent
#' has not been specified, but the branch exists the current head commit is used
#' as a parent. If the branch does not exist then an orphan commit is created.
#'
#' Note: The GitHub API imposes a file size limit of 100MB for this request.
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
#' @param from_path (string) The paths to the files to upload. They must be
#'   readable.
#' @param to_path (string) The paths to write the files to, within the
#'   repository.
#' @param branch (string) The name of the branch to make the new commit on.
#' @param message (string) The commit message.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param author (list, optional) A the name and email address of the user who
#'   wrote the changes in the commit.
#' @param committer (list, optional) A the name and email address of the user
#'   who created the commit.
#' @param parent (string, optional) Reference for the commit to use as a parent,
#'   can be either a SHA, branch or tag. If it is a branch then the head commit
#'   is used. See the details section for more information.
#' @param force (boolean, optional) Whether to force the update if it is not a
#'   simple fast-forward. Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `upload_files()` returns a list of the commit properties.
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
#'   # Upload files to the main branch
#'   upload_files(
#'     from_path = c("c:/test/file1.txt", "c:/test/file2.txt"),
#'     to_path   = c("file1.txt", "file2.txt"),
#'     branch    = "main",
#'     message   = "Commit to test upload_files()",
#'     repo      = "ChadGoymer/githapi"
#'   )
#'
#'   # Upload files into directories within the main branch
#'   upload_files(
#'     from_path = c(
#'       "c:/test/file1.txt",
#'       "c:/test/file2.txt",
#'       "c:/test/file3.txt"
#'     ),
#'     to_path   = c(
#'       "dir-1/file-1.txt",
#'       "dir-1/dir-1-1/file-2.txt",
#'       "dir-2/file-3.txt"
#'     ),
#'     branch    = "main",
#'     message   = "Commit to test upload_files()",
#'     repo      = str_c("ChadGoymer/test-files-", now)
#'   )
#'
#'   # Upload files to the main branch specifying an author and committer
#'   upload_files(
#'     from_path = c("c:/test/file1.txt", "c:/test/file2.txt"),
#'     to_path   = c("file1.txt", "file2.txt"),
#'     branch    = "main",
#'     message   = "Commit to test upload_files()",
#'     repo      = "ChadGoymer/githapi",
#'     author    = list(name = "Bob",  email = "bob@acme.com"),
#'     committer = list(name = "Jane", email = "jane@acme.com")
#'   )
#'
#'   # Create a new branch from the main branch
#'   upload_files(
#'     from_path = c("c:/test/file1.txt", "c:/test/file2.txt"),
#'     to_path   = c("file1.txt", "file2.txt"),
#'     branch    = "new-branch",
#'     message   = "Commit to test upload_files()",
#'     repo      = "ChadGoymer/githapi",
#'     parent    = "main"
#'   )
#'
#' }
#'
#' @export
#'
upload_files <- function(
  from_path,
  to_path,
  branch,
  message,
  repo,
  author,
  committer,
  parent,
  force = FALSE,
  ...
) {
  assert(
    all(map_lgl(from_path, ~ is_file(.) && is_readable(.))),
    "All 'from_path' must be a readable file paths:\n  ", from_path
  )
  assert(
    is_character(to_path),
    "'to_path' must be a character vector:\n  ", to_path
  )
  assert(
    identical(length(from_path), length(to_path)),
    "'from_path' and 'to_path' must be the same length:\n  ",
    length(from_path), " vs ", length(to_path)
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
    is_scalar_logical(force),
    "'force' must be a boolean:\n  ", force
  )

  payload <- list(message = message)

  if (!missing(author)) {
    assert(
      is_list(author) &&
        is_scalar_character(author$name) &&
        is_scalar_character(author$email),
      "'author' must be a list containing 'name' and 'email':\n ", author
    )
    payload$author <- author
  }

  if (!missing(committer)) {
    assert(
      is_list(committer) &&
        is_scalar_character(committer$name) &&
        is_scalar_character(committer$email),
      "'committer' must be a list containing 'name' and 'email':\n ", committer
    )
    payload$committer <- committer
  }

  if (missing(parent)) {
    parent <- branch
  }
  assert(
    is_scalar_character(parent),
    "'parent' must be a string:\n  ", parent
  )

  info("Uploading files to repository '", repo, "'")
  blob_shas <- map_chr(from_path, ~ upload_blob(path = ., repo = repo, ...)$sha)

  temp_path <- tempfile("tree-")
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  walk2(file.path(temp_path, to_path), blob_shas, function(path, sha) {
    if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
    readr::write_lines(sha, path)
  })

  info("Uploading tree to repository '", repo, "'", level = 3)
  result <- upload_tree(
    path        = temp_path,
    repo        = repo,
    base_commit = parent,
    placeholder = TRUE,
    ...
  )

  payload$tree    <- result$tree_sha
  payload$parents <- as.list(result$commit_sha)

  info("Creating commit in repo '", repo, "'")
  commit <- gh_url("repos", repo, "git/commits") %>%
    gh_request("POST", payload = payload, ...)

  if (identical(branch, parent) && is_null(result$commit_sha)) {
    create_branch(name = branch, ref = commit$sha, repo = repo, ...)
  }
  else {
    branch_sha <- try_catch(
      view_commit(ref = branch, repo = repo, ...),
      on_error = function(e) NULL
    )

    if (is_null(branch_sha)) {
      create_branch(name = branch, ref = commit$sha, repo = repo, ...)
    }
    else {
      update_branch(
        branch = branch,
        ref    = commit$sha,
        repo   = repo,
        force  = force,
        ...
      )
    }
  }

  view_commit(commit$sha, repo = repo, ...)
}


#  FUNCTION: download_file -----------------------------------------------------
#
#' Download a file from GitHub
#'
#' This function downloads a file in the specified commit to the path provided.
#'
#' Note: The GitHub API imposes a file size limit of 100MB for this request.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#get-repository-content",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "git#get-a-blob",
#'   ">"
#' ))
#' ```
#'
#' @param from_path (string) The path to the file to download, within the
#'   repository.
#' @param to_path (string) The path to download the file to.
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `download_file()` returns the path where the file is downloaded to.
#'
#' @examples
#' \dontrun{
#'
#'   # Download the README file from the main branch
#'   download_file(
#'     from_path = "README.md",
#'     to_path   = "~/README.md",
#'     ref       = "main",
#'     repo      = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
download_file <- function(
  from_path,
  to_path,
  ref,
  repo,
  ...
) {
  assert(
    is_scalar_character(from_path),
    "'from_path' must be a string:\n  ", from_path
  )
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info(
    "Checking file '", basename(from_path),
    "' exists in repository '", repo, "'",
    level = 3
  )
  file <- gh_url("repos", repo, "contents", dirname(from_path), ref = ref) %>%
    gh_request("GET", ...) %>%
    keep(~ .$name == basename(from_path))

  assert(
    length(file) == 1,
    "Specified file cannot be found:\n  ", from_path
  )

  info(
    "Downloading file '", basename(from_path),
    "' in repository '", repo, "'"
  )
  path_gh <- gh_url("repos", repo, "git/blobs", file[[1]]$sha) %>%
    gh_download(to_path, accept = "application/vnd.github.v3.raw", ...)

  info("Done", level = 3)
  path_gh
}


#  FUNCTION: create_file -------------------------------------------------------
#
#' Create a file in a new commit
#'
#' This function adds a file in a repository in GitHub by creating a new commit
#' on the specified branch. If the branch does not already exist a `parent`
#' commit must be specified and a new branch is created from it. If the file
#' already exists `create_file()` throws an error.
#'
#' The `author` and `committer` arguments are optional and if not supplied the
#' current authenticated user is used. However, if you want to set them
#' explicitly you must specify a named list with `name` and `email` as the
#' elements (see examples).
#'
#' Note: The GitHub API imposes a file size limit of 1MB for this request. For
#' larger files use the [upload_files()] function.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#create-or-update-file-contents",
#'   ">"
#' ))
#' ```
#'
#' @param content (string) The content of the file specified as a single string.
#' @param path (string) The path to create the file at, within the repository.
#' @param branch (string) The name of the branch to make the new commit on.
#' @param message (string) The commit message.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param parent (string, optional) If creating a new branch the the parent
#'   commit must be specified as either a SHA, branch or tag.
#' @param author (list, optional) A the name and email address of the user who
#'   wrote the changes in the commit.
#' @param committer (list, optional) A the name and email address of the user
#'   who created the commit.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_file()`returns a list of the commit properties.
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
#'   # Create a new file on the main branch
#'   create_file(
#'     content = "# This is a new file\\n\\n Created by `create_file()`",
#'     path    = "new-file.md",
#'     branch  = "main",
#'     message = "Created a new file with create_file()",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Create a new file on a new branch
#'   create_file(
#'     content = "# This is a new file\\n\\n Created by `create_file()`",
#'     path    = "new-file.md",
#'     branch  = "new-branch",
#'     message = "Created a new file with create_file()",
#'     repo    = "ChadGoymer/githapi",
#'     parent  = "main"
#'   )
#'
#'   # Create a new file on the main branch specifying an author and committer
#'   create_file(
#'     content   = "# This is a new file\\n\\n Created by `create_file()`",
#'     path      = "new-file.md",
#'     branch    = "main",
#'     message   = "Created a new file with create_file()",
#'     repo      = "ChadGoymer/githapi",
#'     author    = list(name = "Bob",  email = "bob@acme.com"),
#'     committer = list(name = "Jane", email = "jane@acme.com")
#'   )
#'
#' }
#'
#' @export
#'
create_file <- function(
  content,
  path,
  branch,
  message,
  repo,
  parent,
  author,
  committer,
  ...
) {
  assert(
    is_scalar_character(content),
    "'content' must be a string:\n  ", content
  )
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
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

  payload <- list(
    content = jsonlite::base64_enc(content),
    branch  = branch,
    message = message
  )

  if (!missing(parent) && !identical(parent, branch)) {
    assert(
      is_ref(parent),
      "'parent' must be a valid git reference - see help(is_ref):\n  ", parent
    )
    create_branch(name = branch, ref = parent, repo = repo, ...)
  }

  if (!missing(author)) {
    assert(
      is_list(author) &&
        is_scalar_character(author$name) &&
        is_scalar_character(author$email),
      "'author' must be a list containing 'name' and 'email':\n ", author
    )
    payload$author <- author
  }

  if (!missing(committer)) {
    assert(
      is_list(committer) &&
        is_scalar_character(committer$name) &&
        is_scalar_character(committer$email),
      "'committer' must be a list containing 'name' and 'email':\n ", committer
    )
    payload$committer <- committer
  }

  info(
    "Checking if a file with path '", path,
    "' already exists in repository '", repo, "'",
    level = 3
  )
  file_exists <- tryCatch({
    gh_url("repos", repo, "contents", path, ref = branch) %>%
      gh_request("GET", ...)
    TRUE
  },
  error = function(e) {
    FALSE
  })

  assert(
    !file_exists,
    "A file with path '", path,
    "' already exists. To update it use update_file()"
  )

  info("Creating file '", basename(path), "' in repository '", repo, "'")
  commit <- gh_url("repos", repo, "contents", path) %>%
    gh_request("PUT", payload = payload, ...) %>%
    pluck("commit")

  view_commit(commit$sha, repo = repo, ...)
}


#  FUNCTION: update_file -------------------------------------------------------
#
#' Update a file in a new commit
#'
#' This function updates a file in a repository in GitHub by creating a new
#' commit on the specified branch. If the branch does not already exist a
#' `parent` commit must be specified and a new branch is created from it.
#'
#' The `author` and `committer` arguments are optional and if not supplied the
#' current authenticated user is used. However, if you want to set them
#' explicitly you must specify a named list with `name` and `email` as the
#' elements (see examples).
#'
#' Note: The GitHub API imposes a file size limit of 1MB for this request. For
#' larger files use the [upload_files()] function.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#create-or-update-file-contents",
#'   ">"
#' ))
#' ```
#'
#' @param content (string) The content of the file specified as a single string.
#' @param path (string) The path of the file to update, within the repository.
#' @param branch (string) The name of the branch to make the new commit on.
#' @param message (string) The commit message.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param parent (string, optional) If creating a new branch the the parent
#'   commit must be specified as either a SHA, branch or tag.
#' @param author (list, optional) A the name and email address of the user who
#'   wrote the changes in the commit.
#' @param committer (list, optional) A the name and email address of the user
#'   who created the commit.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_file()`returns a list of the commit properties.
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
#'   # Update a file on the main branch
#'   update_file(
#'     content = "# This is a file\\n\\n Updated by `update_file()`",
#'     path    = "updated-file.md",
#'     branch  = "main",
#'     message = "Updated a file with update_file()",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Update a file on a new branch
#'   update_file(
#'     content = "# This is a file\\n\\n Updated by `update_file()`",
#'     path    = "updated-file.md",
#'     branch  = "new-branch",
#'     message = "Updated a file with update_file()",
#'     repo    = "ChadGoymer/githapi",
#'     parent  = "main"
#'   )
#'
#'   # Create a new file on the main branch specifying an author and committer
#'   update_file(
#'     content   = "# This is a file\\n\\n Updated by `update_file()`",
#'     path      = "updated-file.md",
#'     branch    = "main",
#'     message   = "Updated a file with update_file()",
#'     repo      = "ChadGoymer/githapi",
#'     author    = list(name = "Bob",  email = "bob@acme.com"),
#'     committer = list(name = "Jane", email = "jane@acme.com")
#'   )
#'
#' }
#'
#' @export
#'
update_file <- function(
  content,
  path,
  branch,
  message,
  repo,
  parent,
  author,
  committer,
  ...
) {
  assert(
    is_scalar_character(content),
    "'content' must be a string:\n  ", content
  )
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
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

  payload <- list(
    content = jsonlite::base64_enc(content),
    branch  = branch,
    message = message
  )

  if (!missing(parent) && !identical(parent, branch)) {
    assert(
      is_ref(parent),
      "'parent' must be a valid git reference - see help(is_ref):\n  ", parent
    )
    create_branch(name = branch, ref = parent, repo = repo, ...)
  }

  if (!missing(author)) {
    assert(
      is_list(author) &&
        is_scalar_character(author$name) &&
        is_scalar_character(author$email),
      "'author' must be a list containing 'name' and 'email':\n ", author
    )
    payload$author <- author
  }

  if (!missing(committer)) {
    assert(
      is_list(committer) &&
        is_scalar_character(committer$name) &&
        is_scalar_character(committer$email),
      "'committer' must be a list containing 'name' and 'email':\n ", committer
    )
    payload$committer <- committer
  }

  info(
    "Checking if a file with path '", path,
    "' already exists in repository '", repo, "'",
    level = 3
  )
  file <- tryCatch({
    gh_url("repos", repo, "contents", path, ref = branch) %>%
      gh_request("GET", ...)
  },
  error = function(e) {
    NULL
  })

  assert(
    !is_null(file),
    "A file with path '", path,
    "' does not exist. To create it use create_file()"
  )
  payload$sha <- file$sha

  info("Updating file '", basename(path), "' in repository '", repo, "'")
  commit <- gh_url("repos", repo, "contents", path) %>%
    gh_request("PUT", payload = payload, ...) %>%
    pluck("commit")

  view_commit(commit$sha, repo = repo, ...)
}


#  FUNCTION: delete_file -------------------------------------------------------
#
#' Delete a file in a new commit
#'
#' This function deletes a file in a repository in GitHub by creating a new
#' commit on the specified branch. If the branch does not already exist a
#' `parent` commit must be specified and a new branch is created from it. If the
#' file does not exist `delete_file()` throws as error.
#'
#' The `author` and `committer` arguments are optional and if not supplied the
#' current authenticated user is used. However, if you want to set them
#' explicitly you must specify a named list with `name` and `email` as the
#' elements (see examples).
#'
#' Note: The GitHub API imposes a file size limit of 1MB for this request. For
#' larger files use the [upload_files()] function.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#delete-a-file",
#'   ">"
#' ))
#' ```
#'
#' @param path (string) The path of the file to delete, within the repository.
#' @param branch (string) The name of the branch to make the new commit on.
#' @param message (string) The commit message.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param parent (string, optional) If creating a new branch the the parent
#'   commit must be specified as either a SHA, branch or tag.
#' @param author (list, optional) A the name and email address of the user who
#'   wrote the changes in the commit.
#' @param committer (list, optional) A the name and email address of the user
#'   who created the commit.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_file()`returns a list of the commit properties.
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
#'   # Delete a file on the main branch
#'   delete_file(
#'     path    = "file-to-delete.md",
#'     branch  = "main",
#'     message = "Deleted a file with delete_file()",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # Delete a file on a new branch
#'   create_file(
#'     path    = "file-to-delete.md",
#'     branch  = "new-branch",
#'     message = "Deleted a file with delete_file()",
#'     repo    = "ChadGoymer/githapi",
#'     parent  = "main"
#'   )
#'
#'   # Delete a file on the main branch specifying an author and committer
#'   delete_file(
#'     path      = "file-to-delete.md",
#'     branch    = "main",
#'     message   = "Deleted a file with delete_file()",
#'     repo      = "ChadGoymer/githapi",
#'     author    = list(name = "Bob",  email = "bob@acme.com"),
#'     committer = list(name = "Jane", email = "jane@acme.com")
#'   )
#'
#' }
#'
#' @export
#'
delete_file <- function(
  path,
  branch,
  message,
  repo,
  parent,
  author,
  committer,
  ...
) {
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
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

  payload <- list(branch = branch, message = message)

  if (!missing(parent) && !identical(parent, branch)) {
    assert(
      is_ref(parent),
      "'parent' must be a valid git reference - see help(is_ref):\n  ", parent
    )
    create_branch(name = branch, ref = parent, repo = repo, ...)
  }

  if (!missing(author)) {
    assert(
      is_list(author) &&
        is_scalar_character(author$name) &&
        is_scalar_character(author$email),
      "'author' must be a list containing 'name' and 'email':\n ", author
    )
    payload$author <- author
  }

  if (!missing(committer)) {
    assert(
      is_list(committer) &&
        is_scalar_character(committer$name) &&
        is_scalar_character(committer$email),
      "'committer' must be a list containing 'name' and 'email':\n ", committer
    )
    payload$committer <- committer
  }

  info(
    "Checking if a file with path '", path,
    "' already exists in repository '", repo, "'",
    level = 3
  )
  file <- gh_url("repos", repo, "contents", path, ref = branch) %>%
    gh_request("GET", ...)
  payload$sha <- file$sha

  info("Deleting file '", basename(path), "' in repository '", repo, "'")
  commit <- gh_url("repos", repo, "contents", path) %>%
    gh_request("DELETE", payload = payload, ...) %>%
    pluck("commit")

  view_commit(commit$sha, repo = repo, ...)
}


#  FUNCTION: view_files --------------------------------------------------------
#
#' View files within a repository
#'
#' `view_files()` summarises files in a table with the properties as columns and
#' a row for each file in the repository. `view_file()` returns a list of all
#' properties for a single file. `browse_files()` and `browse_file()` open the
#' web page for the commit tree and blob respectively in the default browser.
#'
#' You can summarise all the milestones of a repository in a specified `state`
#' and change the order they are returned using `sort` and `direction`.
#'
#' For more details see the GitHub API documentation:
#'
#' ```{r echo=FALSE, results='asis'}
#' docs_url <- "https://docs.github.com/en/free-pro-team@latest/rest/reference/"
#' cat(paste0(
#'   "- <", docs_url,
#'   "git#get-a-tree",
#'   ">"
#' ))
#' ```
#' ```{r echo=FALSE, results='asis'}
#' cat(paste0(
#'   "- <", docs_url,
#'   "repos#get-repository-content",
#'   ">"
#' ))
#' ```
#'
#' @param path (string) The path to the file, within the repository.
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param recursive (boolean, optional) Whether to list files in subfolders as
#'   well. Default: `TRUE`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_files()` returns a tibble of file properties. `view_file()`
#'   returns a list of properties for a single file. `browse_files()` and
#'   `browse_file()` opens the default browser on the tree or blob page and
#'   returns the URL.
#'
#' **File Properties:**
#'
#' - **path**: The path to the file within the repository.
#' - **sha**: The SHA of the file blob.
#' - **size**: The size of the file in bytes.
#' - **html_url**: The URL of the blob's web page in GitHub.
#'
#' @examples
#' \dontrun{
#'
#'   # View files on the main branch in a repository
#'   view_files("main", "ChadGoymer/githapi")
#'
#'   # View properties of a single file in a repository
#'   view_file(
#'     path = "README.md",
#'     ref  = "main",
#'     repo = "ChadGoymer/githapi"
#'   )
#'
#'   # Open the commit's tree page in the default browser
#'   browse_files("main", "ChadGoymer/githapi")
#'
#'   # Open the file's blob page in the default browser
#'   browse_file(
#'     path = "README.md",
#'     ref  = "main",
#'     repo = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
view_files <- function(
  ref,
  repo,
  recursive = TRUE,
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
  assert(
    is_scalar_logical(recursive),
    "'recursive' must be a boolean:\n  ", recursive
  )

  if (!recursive) {
    recursive <- NULL
  }

  commit <- view_commit(ref = ref, repo = repo, ...)
  blob_base_url <- commit$html_url %>%
    str_replace(str_c(repo, "/", "commit"), str_c(repo, "/", "blob"))

  info(
    "Viewing files for commit with reference '", ref,
    "' in repository '", repo, "'"
  )
  files_lst <- gh_url(
    "repos", repo, "git/trees", commit$tree_sha, recursive = recursive
  ) %>%
    gh_request("GET", ...)

  info("Transforming results", level = 4)
  files_gh <- bind_properties(files_lst$tree, properties$file) %>%
    filter(.data$type == "blob") %>%
    select(-"type") %>%
    mutate(html_url = file.path(blob_base_url, .data$path))

  info("Done", level = 7)
  structure(
    files_gh,
    class   = class(files_gh),
    url     = attr(files_lst, "url"),
    request = attr(files_lst, "request"),
    status  = attr(files_lst, "status"),
    header  = attr(files_lst, "header")
  )
}


#  FUNCTION: view_file ---------------------------------------------------------
#
#' @rdname view_files
#' @export
#'
view_file <- function(
  path,
  ref,
  repo,
  ...
) {
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
  )
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Viewing file '", basename(path), "' in repository '", repo, "'")
  file_lst <- gh_url("repos", repo, "contents", path, ref = ref) %>%
    gh_request("GET", ...)

  info("Transforming results", level = 4)
  file_gh <- file_lst %>%
    select_properties(properties$file) %>%
    discard(names(.) == "type")

  info("Done", level = 7)
  structure(
    file_gh,
    class   = class(file_lst),
    url     = attr(file_lst, "url"),
    request = attr(file_lst, "request"),
    status  = attr(file_lst, "status"),
    header  = attr(file_lst, "header")
  )
}


#  FUNCTION: browse_files ------------------------------------------------------
#
#' @rdname view_files
#' @export
#'
browse_files <- function(
  ref,
  repo,
  ...
) {
  assert(
    is_ref(ref),
    "'ref' must be a string:\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Browsing commit '", ref, "' in repository '", repo, "'")
  commit <- gh_url("repos", repo, "commits", ref) %>%
    gh_request("GET", ...)

  tree_url <- commit$html_url %>%
    str_replace(str_c(repo, "/", "commit"), str_c(repo, "/", "tree"))

  httr::BROWSE(tree_url)

  info("Done", level = 7)
  structure(
    tree_url,
    class   = c("github", "character"),
    url     = attr(commit, "url"),
    request = attr(commit, "request"),
    status  = attr(commit, "status"),
    header  = attr(commit, "header")
  )
}


#  FUNCTION: browse_file -------------------------------------------------------
#
#' @rdname view_files
#' @export
#'
browse_file <- function(
  path,
  ref,
  repo,
  ...
) {
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
  )
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  file <- view_file(path = path, ref = ref, repo = repo, ...)
  httr::BROWSE(file$html_url)

  info("Done", level = 7)
  structure(
    file$html_url,
    class   = c("github", "character"),
    url     = attr(file, "url"),
    request = attr(file, "request"),
    status  = attr(file, "status"),
    header  = attr(file, "header")
  )
}


#  FUNCTION: write_github_file -------------------------------------------------
#
#' Write a file to a branch
#'
#' These functions writes files to a repository by creating a new commit on the
#' specified branch. `write_github_file()` writes the `content` to a text file,
#' using [readr::write_file()]; `write_github_lines()` writes to a text file,
#' using [readr::write_lines()] and `write_github_csv()` writes a CSV file,
#' using [readr::write_csv()].
#'
#' @param content (character or data.frame) The content of the file.
#' @param path (string) The path to create the file at, within the repository.
#' @param branch (string) The name of the branch to make the new commit on.
#' @param message (string) The commit message.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param author (list, optional) A the name and email address of the user who
#'   wrote the changes in the commit.
#' @param committer (list, optional) A the name and email address of the user
#'   who created the commit.
#' @param ... Parameters passed to [readr::write_file()], [readr::write_lines()]
#'   or [readr::write_csv()].
#'
#' @return `write_github_file()`, `write_github_lines()` and
#'   `write_github_csv()` return a list of the commit properties.
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
#'   write_github_file(
#'     content = "# This is a new file\\n\\n Created by githapi",
#'     path    = "new-file.md",
#'     branch  = "main",
#'     message = "Created a new file with write_github_file()",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   write_github_lines(
#'     content   = c("# This is a new file", "", "Created by githapi"),
#'     path      = "new-file.md",
#'     branch    = "main",
#'     message   = "Created a new file with write_github_lines()",
#'     repo      = "ChadGoymer/githapi"
#'   )
#'
#'   write_github_csv(
#'     content = tibble(letters = LETTERS, numbers = 1:26),
#'     path    = "new-file.md",
#'     branch  = "main",
#'     message = "Updated an existing file with write_github_csv()",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
write_github_file <- function(
  content,
  path,
  branch,
  message,
  repo,
  author,
  committer,
  ...
) {
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
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

  temp_path <- tempfile("read-file-")
  dir.create(temp_path, recursive = TRUE)
  on.exit(unlink(temp_path, recursive = TRUE))

  temp_file <- file.path(temp_path, basename(path))

  info("Writing file '", basename(path), "'")
  readr::write_file(x = content, path = temp_file, ...)

  upload_files(
    from_path = temp_file,
    to_path   = path,
    branch    = branch,
    message   = message,
    repo      = repo,
    author    = author,
    committer = committer,
    ...
  )
}


#  FUNCTION: write_github_lines ------------------------------------------------
#
#' @rdname write_github_file
#' @export
#'
write_github_lines <- function(
  content,
  path,
  branch,
  message,
  repo,
  author,
  committer,
  ...
) {
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
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

  temp_path <- tempfile("read-file-")
  dir.create(temp_path, recursive = TRUE)
  on.exit(unlink(temp_path, recursive = TRUE))

  temp_file <- file.path(temp_path, basename(path))

  info("Writing file '", basename(path), "'")
  readr::write_lines(x = content, path = temp_file, ...)

  upload_files(
    from_path = temp_file,
    to_path   = path,
    branch    = branch,
    message   = message,
    repo      = repo,
    author    = author,
    committer = committer,
    ...
  )
}


#  FUNCTION: write_github_csv --------------------------------------------------
#
#' @rdname write_github_file
#' @export
#'
write_github_csv <- function(
  content,
  path,
  branch,
  message,
  repo,
  author,
  committer,
  ...
) {
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
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

  temp_path <- tempfile("read-file-")
  dir.create(temp_path, recursive = TRUE)
  on.exit(unlink(temp_path, recursive = TRUE))

  temp_file <- file.path(temp_path, basename(path))

  info("Writing file '", basename(path), "'")
  readr::write_csv(x = content, path = temp_file, ...)

  upload_files(
    from_path = temp_file,
    to_path   = path,
    branch    = branch,
    message   = message,
    repo      = repo,
    author    = author,
    committer = committer,
    ...
  )
}


#  FUNCTION: read_github_file --------------------------------------------------
#
#' Read files from a commit
#'
#' These functions read a file from a commit in a repository.
#' `read_github_file()` reads a text file, using [readr::read_file()], and
#' returns the result as a string. `read_github_lines()` reads a text file,
#' using [readr::read_lines()], and returns the result as a character vector,
#' one element per line. `read_github_csv()` reads a CSV file, using
#' [readr::read_csv()], and returns the result as a tibble.
#'
#' @param path (string) The path to the file, within the repository.
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [readr::read_file()], [readr::read_lines()]
#'   or [readr::read_csv()].
#'
#' @return `read_github_file()` returns a string containing the file contents,
#'   `read_github_lines()` returns a character vector, and `read_github_csv()`
#'   returns a tibble.
#'
#' @examples
#' \dontrun{
#'
#'   read_github_file(
#'     path = "README.md",
#'     ref  = "main",
#'     repo = "ChadGoymer/githapi"
#'   )
#'
#'   read_github_lines(
#'     path = "README.md",
#'     ref  = "main",
#'     repo = "ChadGoymer/githapi"
#'   )
#'
#'   read_github_csv(
#'     path = "inst/test-data/test.csv",
#'     ref  = "main",
#'     repo = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
read_github_file <- function(
  path,
  ref,
  repo,
  ...
) {
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
  )
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  temp_path <- tempfile("read-file-")
  dir.create(temp_path, recursive = TRUE)
  on.exit(unlink(temp_path, recursive = TRUE))

  file_path <- download_file(
    from_path = path,
    to_path   = file.path(temp_path, basename(path)),
    ref       = ref,
    repo      = repo,
    ...
  )

  info("Reading file '", basename(path), "'")
  file_contents <- readr::read_file(file_path, ...)

  info("Done", level = 7)
  structure(
    file_contents,
    class   = c("github", class(file_contents)),
    url     = attr(file_path, "url"),
    request = attr(file_path, "request"),
    status  = attr(file_path, "status"),
    header  = attr(file_path, "header")
  )
}


#  FUNCTION: read_github_lines -------------------------------------------------
#
#' @rdname read_github_file
#' @export
#'
read_github_lines <- function(
  path,
  ref,
  repo,
  ...
) {
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
  )
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  temp_path <- tempfile("read-file-")
  dir.create(temp_path, recursive = TRUE)
  on.exit(unlink(temp_path, recursive = TRUE))

  file_path <- download_file(
    from_path = path,
    to_path   = file.path(temp_path, basename(path)),
    ref       = ref,
    repo      = repo,
    ...
  )

  info("Reading file '", basename(path), "'")
  file_contents <- readr::read_lines(file_path, ...)

  info("Done", level = 7)
  structure(
    file_contents,
    class   = c("github", class(file_contents)),
    url     = attr(file_path, "url"),
    request = attr(file_path, "request"),
    status  = attr(file_path, "status"),
    header  = attr(file_path, "header")
  )
}


#  FUNCTION: read_github_csv ---------------------------------------------------
#
#' @rdname read_github_file
#' @export
#'
read_github_csv <- function(
  path,
  ref,
  repo,
  ...
) {
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
  )
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  temp_path <- tempfile("read-file-")
  dir.create(temp_path, recursive = TRUE)
  on.exit(unlink(temp_path, recursive = TRUE))

  file_path <- download_file(
    from_path = path,
    to_path   = file.path(temp_path, basename(path)),
    ref       = ref,
    repo      = repo,
    ...
  )

  info("Reading file '", basename(path), "'")
  file_contents <- readr::read_csv(file_path, ...)

  info("Done", level = 7)
  structure(
    file_contents,
    class   = c("github", class(file_contents)),
    url     = attr(file_path, "url"),
    request = attr(file_path, "request"),
    status  = attr(file_path, "status"),
    header  = attr(file_path, "header")
  )
}


#  FUNCTION: github_source -----------------------------------------------------
#
#' Source a R script from a commit
#'
#' This function sources an R script from a commit in a repository using
#' [source()].
#'
#' @param path (string) The path to the file, within the repository.
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [source()].
#'
#' @return The result of the sourced script.
#'
#' @examples
#' \dontrun{
#'
#'   github_source(
#'     path = "inst/test-data/test-script.R",
#'     ref  = "main",
#'     repo = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
github_source <- function(
  path,
  ref,
  repo,
  ...
) {
  assert(
    is_scalar_character(path),
    "'path' must be a string:\n  ", path
  )
  assert(
    is_ref(ref),
    "'ref' must be a valid git reference - see help(is_ref):\n  ", ref
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  temp_path <- tempfile("read-file-")
  dir.create(temp_path, recursive = TRUE)
  on.exit(unlink(temp_path, recursive = TRUE))

  file_path <- download_file(
    from_path = path,
    to_path   = file.path(temp_path, basename(path)),
    ref       = ref,
    repo      = repo,
    ...
  )

  info("Sourcing file '", basename(path), "'")
  result <- source(file_path, ...)

  info("Done", level = 7)
  structure(
    result,
    class   = c("github", class(result)),
    url     = attr(file_path, "url"),
    request = attr(file_path, "request"),
    status  = attr(file_path, "status"),
    header  = attr(file_path, "header")
  )
}


#  FUNCTION: compare_files -----------------------------------------------------
#
#' View file changes made between two commits
#'
#' `compare_files()` summarises the file changes made between two commits in a
#' table with the properties as columns and a row for each file. The `base`
#' commit must be in the history of the `head` commit.
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
#' @return `compare_files()` returns a tibble of file properties.
#'
#' **File Properties:**
#'
#' - **path**: The path to the file within the repository.
#' - **sha**: The SHA of the file blob.
#' - **status**: Whether the file was "added", "modified" or "deleted".
#' - **additions**: The number of lines added in the file.
#' - **deletions**: The number of lines deleted in the file.
#' - **changes**: The number of lines changed in the file.
#' - **patch**: The git patch for the file.
#' - **html_url**: The URL of the blob's web page in GitHub.
#'
#' @examples
#' \dontrun{
#'
#'   # View the files changes made between the current main branch and a release
#'   compare_files("main", "0.8.7", "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
compare_files <- function(
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
  comparison_gh <- bind_properties(
    comparison_lst$files,
    properties$compare_files
  )

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
