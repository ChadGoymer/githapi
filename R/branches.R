#  FUNCTION: create_branch --------------------------------------------------------------------
#
#' Create a branch in a repository
#'
#' This function creates a new branch in the specified repository in GitHub. It must be
#' pointed at a commit by providing a Git reference, which can be either a SHA, branch or
#' tag. For a branch, the head commit is used.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/git/refs/#create-a-reference>
#'
#' @param name (string) The name of the branch.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) Either a SHA, branch or tag used to identify the commit
#'   the branch is pointing at. Default: `"master"`.
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
#'   create_branch(
#'     name = "new-branch",
#'     repo = "ChadGoymer/test-githapi",
#'     ref  = "master")
#' }
#'
#' @export
#'
create_branch <- function(
  name,
  repo,
  ref = "master",
  ...)
{
  assert(is_ref(name), "'name' must be a valid git reference - see help(is_ref):\n  ", name)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  if (!is_sha(ref))
  {
    ref <- gh_url("repos", repo, "commits", ref) %>%
      gh_request("GET", accept = "application/vnd.github.VERSION.sha")
  }
  assert(is_sha(ref), "'ref' must be a 40 character string:\n  ", ref)

  payload <- list(ref = str_c("refs/heads/", name), sha = ref)

  info("Creating branch '", name, "' in repository '", repo, "'")
  branch_lst <- gh_url("repos", repo, "git/refs") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  branch_gh <- select_properties(branch_lst, properties$branch) %>%
    modify_list(name = basename(branch_lst$ref), .before = "ref")

  info("Done", level = 7)
  structure(
    branch_gh,
    class   = class(branch_lst),
    url     = attr(branch_lst, "url"),
    request = attr(branch_lst, "request"),
    status  = attr(branch_lst, "status"),
    header  = attr(branch_lst, "header"))
}


#  FUNCTION: update_branch --------------------------------------------------------------------
#
#' Update a branch in a repository
#'
#' This function updates a branch in the specified repository to point at a new commit. It
#' must be pointed at a commit by providing a Git reference, which can be either a SHA, branch
#' or tag. For a branch, the head commit is used.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/git/refs/#update-a-reference>
#'
#' @param branch (string) The name of the branch.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string) Either a SHA, branch or tag used to identify the new commit the branch
#'   is pointing at.
#' @param force (boolean, optional) Whether to force the update if it is not a simple
#'   fast-forward. Default: `FALSE`.
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
#'   update_branch(
#'     name = "new-branch",
#'     repo = "ChadGoymer/test-githapi",
#'     ref  = "6b7b5a090d47fd3ef495620513a3f80da2487b1d")
#' }
#'
#' @export
#'
update_branch <- function(
  branch,
  repo,
  ref,
  force = FALSE,
  ...)
{
  assert(is_ref(branch), "'branch' must be a valid git reference - see help(is_ref):\n  ", branch)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
  assert(is_scalar_logical(force), "'force' must be boolean:\n  ", force)

  if (!is_sha(ref))
  {
    ref <- gh_url("repos", repo, "commits", ref) %>%
      gh_request("GET", accept = "application/vnd.github.VERSION.sha")
  }
  assert(is_sha(ref), "'ref' must be a 40 character string:\n  ", ref)

  info("Updating branch '", branch, "' in repository '", repo, "'")
  branch_lst <- gh_url("repos", repo, "git/refs/heads", branch) %>%
    gh_request("PATCH", payload = list(sha = ref, force = force), ...)

  info("Transforming results", level = 4)
  branch_gh <- select_properties(branch_lst, properties$branch) %>%
    modify_list(name = basename(branch_lst$ref), .before = "ref")

  info("Done", level = 7)
  structure(
    branch_gh,
    class   = class(branch_lst),
    url     = attr(branch_lst, "url"),
    request = attr(branch_lst, "request"),
    status  = attr(branch_lst, "status"),
    header  = attr(branch_lst, "header"))
}


#  FUNCTION: view_branches --------------------------------------------------------------------
#
#' View branches within a repository
#'
#' `view_branches()` summarises branches in a table with the properties as columns and a row
#' for each branch in the repository. `view_branch()` returns a list of all properties for a
#' single branch.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/git/refs/#list-matching-references>
#' - <https://developer.github.com/v3/git/refs/#get-a-single-reference>
#'
#' @param branch (string) The name of the branch.
#' @param repo (string) The repository specified in the format: `owner/repo`.
# @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return `view_branches()` returns a tibble of branch properties. `view_branch()`
#'   returns a list of properties for a single branch.
#'
#' **Branch Properties:**
#'
#' - **name**: The name of the branch.
#' - **ref**: The full Git reference of the branch.
#' - **sha**: The commit SHA the branch is pointing at.
#'
#' @examples
#' \dontrun{
#'   # View all branches in a repository
#'   view_branches("ChadGoymer/test-githapi")
#'
#'   # View a single branch
#'   view_label("new-branch", "ChadGoymer/test-githapi")
#' }
#'
#' @export
#'
# TODO: Uncomment in version 1.0
# view_branches <- function(
#   repo,
#   n_max = 1000,
#   ...)
# {
#   assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
#
#   info("Viewing branches for respository '", repo, "'")
#   branches_lst <- gh_url("repos", repo, "git/refs/heads") %>%
#     gh_page(n_max = n_max, ...)
#
#   info("Transforming results", level = 4)
#   branches_gh <- bind_properties(branches_lst, properties$branch) %>%
#     add_column(name = basename(.$ref), .before = "ref")
#
#   info("Done", level = 7)
#   structure(
#     branches_gh,
#     class   = class(branches_gh),
#     url     = attr(branches_lst, "url"),
#     request = attr(branches_lst, "request"),
#     status  = attr(branches_lst, "status"),
#     header  = attr(branches_lst, "header"))
# }
#
#