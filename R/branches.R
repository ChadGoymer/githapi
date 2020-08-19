#  FUNCTION: create_branch -----------------------------------------------------
#
#' Create a branch in a repository
#'
#' This function creates a new branch in the specified repository in GitHub. It
#' must be pointed at a commit by providing a Git reference, which can be either
#' a SHA, branch or tag. For a branch, the head commit is used.
#'
#' For more details see the GitHub API documentation:
#' - <https://docs.github.com/en/rest/reference/git#create-a-reference>
#'
#' @param name (string) The name of the branch.
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_branch()` returns a list of the branch's properties.
#'
#' **Branch Properties:**
#'
#' - **name**: The name of the branch.
#' - **ref**: The full Git reference of the branch.
#' - **sha**: The commit SHA the branch is pointing at.
#'
#' @examples
#' \dontrun{
#'
#'   create_branch(
#'     name = "new-branch",
#'     ref  = "main",
#'     repo = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
create_branch <- function(
  name,
  ref,
  repo,
  ...
) {
  assert(
    is_ref(name),
    "'name' must be a valid git reference - see help(is_ref):\n  ", name
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  if (!is_sha(ref)) {
    ref <- view_sha(ref = ref, repo = repo, ...)
  }
  assert(
    is_sha(ref),
    "'ref' must be a 40 character string:\n  ", ref
  )

  payload <- list(ref = str_c("refs/heads/", name), sha = ref)

  info("Creating branch '", name, "' in repository '", repo, "'")
  branch_lst <- gh_url("repos", repo, "git/refs") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  branch_gh <- select_properties(branch_lst, properties$reference) %>%
    modify_list(name = basename(branch_lst$ref), .before = "ref")

  info("Done", level = 7)
  branch_gh
}


#  FUNCTION: update_branch -----------------------------------------------------
#
#' Update a branch in a repository
#'
#' This function updates a branch in the specified repository to point at a new
#' commit. It must be pointed at a commit by providing a Git reference, which
#' can be either a SHA, branch or tag. For a branch, the head commit is used.
#'
#' For more details see the GitHub API documentation:
#' - <https://docs.github.com/en/rest/reference/git#update-a-reference>
#'
#' @param branch (string) The name of the branch.
#' @param ref (string) Either a SHA, branch or tag used to identify the new
#'   commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param force (boolean, optional) Whether to force the update if it is not a
#'   simple fast-forward. Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_branch()` returns a list of the branch properties.
#'
#' **Branch Properties:**
#'
#' - **name**: The name of the branch.
#' - **ref**: The full Git reference of the branch.
#' - **sha**: The commit SHA the branch is pointing at.
#'
#' @examples
#' \dontrun{
#'
#'   update_branch(
#'     name = "new-branch",
#'     repo = "ChadGoymer/githapi",
#'     ref  = "6b7b5a090d47fd3ef495620513a3f80da2487b1d"
#'   )
#'
#' }
#'
#' @export
#'
update_branch <- function(
  branch,
  ref,
  repo,
  force = FALSE,
  ...
) {
  assert(
    is_ref(branch),
    "'branch' must be a valid git reference - see help(is_ref):\n  ", branch
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )
  assert(
    is_scalar_logical(force),
    "'force' must be boolean:\n  ", force
  )

  if (!is_sha(ref)) {
    ref <- view_sha(ref = ref, repo = repo, ...)
  }
  assert(
    is_sha(ref),
    "'ref' must be a 40 character string:\n  ", ref
  )

  info("Updating branch '", branch, "' in repository '", repo, "'")
  branch_lst <- gh_url("repos", repo, "git/refs/heads", branch) %>%
    gh_request("PATCH", payload = list(sha = ref, force = force), ...)

  info("Transforming results", level = 4)
  branch_gh <- select_properties(branch_lst, properties$reference) %>%
    modify_list(name = basename(branch_lst$ref), .before = "ref")

  info("Done", level = 7)
  branch_gh
}


#  FUNCTION: view_branches -----------------------------------------------------
#
#' View branches within a repository
#'
#' `view_branches()` summarises branches in a table with the properties as
#' columns and a row for each branch in the repository. `view_branch()` returns
#' a list of all properties for a single branch.
#'
#' For more details see the GitHub API documentation:
#' - <https://docs.github.com/en/rest/reference/git#list-matching-references>
#' - <https://docs.github.com/en/rest/reference/git#get-a-reference>
#'
#' @param branch (string) The name of the branch.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_branches()` returns a tibble of branch properties.
#'   `view_branch()` returns a list of properties for a single branch.
#'
#' **Branch Properties:**
#'
#' - **name**: The name of the branch.
#' - **ref**: The full Git reference of the branch.
#' - **sha**: The commit SHA the branch is pointing at.
#'
#' @examples
#' \dontrun{
#'
#'   # View all branches in a repository
#'   view_branches(repo = "ChadGoymer/githapi")
#'
#'   # View a single branch
#'   view_label(branch = "new-branch", repo = "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
view_branches <- function(
  repo,
  n_max = 1000,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Viewing branches for repository '", repo, "'")
  branches_lst <- gh_url("repos", repo, "git/refs/heads") %>%
    gh_page(n_max = n_max, ...)

  info("Transforming results", level = 4)
  branches_gh <- bind_properties(branches_lst, properties$reference) %>%
    add_column(name = basename(.$ref), .before = "ref")

  info("Done", level = 7)
  branches_gh
}


#  FUNCTION: view_branch -------------------------------------------------------
#
#' @rdname view_branches
#' @export
#'
view_branch <- function(
  branch,
  repo,
  ...
) {
  assert(
    is_ref(branch),
    "'branch' must be a valid git reference - see help(is_ref):\n  ", branch
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Viewing branch '", branch, "' in repository '", repo, "'")
  branch_lst <- gh_url("repos", repo, "git/ref/heads", branch) %>%
    gh_request("GET", ...)

  info("Transforming results", level = 4)
  branch_gh <- select_properties(branch_lst, properties$reference) %>%
    modify_list(name = basename(branch_lst$ref), .before = "ref")

  info("Done", level = 7)
  branch_gh
}


#  FUNCTION: delete_branch -----------------------------------------------------
#
#' Delete a branch from a repository
#'
#' This function deletes a branch from a repository, as long as you have
#' appropriate permissions. Care should be taken as it will not be recoverable.
#'
#' For more details see the GitHub API documentation:
#' - <https://docs.github.com/en/rest/reference/git#delete-a-reference>
#'
#' @param branch (string) The name of the branch.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_branch()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   delete_branch(branch = "new-branch", repo = "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
delete_branch <- function(
  branch,
  repo,
  ...
) {
  assert(
    is_ref(branch),
    "'branch' must be a valid git reference - see help(is_ref):\n  ", branch
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Deleting branch '", branch, "' in repository '", repo, "'")
  response <- gh_url("repos", repo, "git/refs/heads", branch) %>%
    gh_request("DELETE", ...)

  info("Done", level = 7)
  structure(
    TRUE,
    class   = c("github", "logical"),
    url     = attr(response, "url"),
    request = attr(response, "request"),
    status  = attr(response, "status"),
    header  = attr(response, "header")
  )
}
