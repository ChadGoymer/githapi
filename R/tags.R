#  FUNCTION: create_tag --------------------------------------------------------
#
#' Create a tag in a repository
#'
#' This function creates a new tag in the specified repository in GitHub. It
#' must be pointed at a commit by providing a Git reference, which can be either
#' a SHA, branch or tag. For a branch, the head commit is used.
#'
#' For more details see the GitHub API documentation:
#'
#' - <https://docs.github.com/en/rest/reference/git#create-a-reference>
#'
#' @param name (string) The name of the tag.
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_tag()` returns a list of the tag's properties.
#'
#' **Tag Properties:**
#'
#' - **name**: The name of the tag.
#' - **sha**: The commit SHA the tag is pointing at.
#' - **html_url**: The address of the tag's web page in GitHub.
#' - **zip_url**: The address to download a zip file of the commit.
#' - **tar_url**: The address to download a tar file of the commit.
#'
#' @examples
#' \dontrun{
#'
#'   create_tag(
#'     name = "new-tag",
#'     ref  = "main",
#'     repo = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
create_tag <- function(
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

  payload <- list(ref = str_c("refs/tags/", name), sha = ref)

  info("Creating tag '", name, "' in repository '", repo, "'")
  tag_lst <- gh_url("repos", repo, "git/refs") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  tag_gh <- gh_url("repos", repo, "tags") %>%
    gh_find(property = "name", value = name, ...) %>%
    select_properties(properties$tag)

  info("Done", level = 7)
  structure(
    tag_gh,
    url     = attr(tag_lst, "url"),
    request = attr(tag_lst, "request"),
    status  = attr(tag_lst, "status"),
    header  = attr(tag_lst, "header")
  )
}


#  FUNCTION: update_tag --------------------------------------------------------
#
#' Update a tag in a repository
#'
#' This function updates a tag in the specified repository to point at a new
#' commit. It must be pointed at a commit by providing a Git reference, which
#' can be either a SHA, branch or tag. For a branch, the head commit is used.
#'
#' For more details see the GitHub API documentation:
#'
#' - <https://docs.github.com/en/rest/reference/git#update-a-reference>
#'
#' @param tag (string) The name of the tag.
#' @param ref (string) Either a SHA, branch or tag used to identify the new
#'   commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param force (boolean, optional) Whether to force the update if it is not a
#'   simple fast-forward. Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_tag()` returns a list of the tag properties.
#'
#' **Tag Properties:**
#'
#' - **name**: The name of the tag.
#' - **sha**: The commit SHA the tag is pointing at.
#' - **html_url**: The address of the tag's web page in GitHub.
#' - **zip_url**: The address to download a zip file of the commit.
#' - **tar_url**: The address to download a tar file of the commit.
#'
#' @examples
#' \dontrun{
#'
#'   update_tag(
#'     tag  = "new-tag",
#'     ref  = "6b7b5a090d47fd3ef495620513a3f80da2487b1d",
#'     repo = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
update_tag <- function(
  tag,
  ref,
  repo,
  force = FALSE,
  ...
) {
  assert(
    is_ref(tag),
    "'tag' must be a valid git reference - see help(is_ref):\n  ", tag
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

  info("Updating tag '", tag, "' in repository '", repo, "'")
  tag_lst <- gh_url("repos", repo, "git/refs/tags", tag) %>%
    gh_request("PATCH", payload = list(sha = ref, force = force), ...)

  info("Transforming results", level = 4)
  tag_gh <- gh_url("repos", repo, "tags") %>%
    gh_find(property = "name", value = tag, ...) %>%
    select_properties(properties$tag)

  info("Done", level = 7)
  structure(
    tag_gh,
    url     = attr(tag_lst, "url"),
    request = attr(tag_lst, "request"),
    status  = attr(tag_lst, "status"),
    header  = attr(tag_lst, "header")
  )
}


#  FUNCTION: view_tags ---------------------------------------------------------
#
#' View tags within a repository
#'
#' `view_tags()` summarises tags in a table with the properties as columns and a
#' row for each tag in the repository. `view_tag()` returns a list of all
#' properties for a single tag.
#'
#' For more details see the GitHub API documentation:
#'
#' - <https://docs.github.com/en/rest/reference/repos#list-repository-tags>
#'
#' @param tag (string) The name of the tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_tags()` returns a tibble of tag properties. `view_tag()`
#'   returns a list of properties for a single tag.
#'
#' **Tag Properties:**
#'
#' - **name**: The name of the tag.
#' - **sha**: The commit SHA the tag is pointing at.
#' - **html_url**: The address of the tag's web page in GitHub.
#' - **zip_url**: The address to download a zip file of the commit.
#' - **tar_url**: The address to download a tar file of the commit.
#'
#' @examples
#' \dontrun{
#'
#'   # View all tags in a repository
#'   view_tags("ChadGoymer/githapi")
#'
#'   # View a single tag
#'   view_tag("new-tag", "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
view_tags <- function(
  repo,
  n_max = 1000,
  ...
) {
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Viewing tags for repository '", repo, "'")
  tags_lst <- gh_url("repos", repo, "tags") %>%
    gh_page(n_max = n_max, ...)

  info("Transforming results", level = 4)
  tags_gh <- bind_properties(tags_lst, properties$tags)

  info("Done", level = 7)
  tags_gh
}


#  FUNCTION: view_tag ----------------------------------------------------------
#
#' @rdname view_tags
#' @export
#'
view_tag <- function(
  tag,
  repo,
  ...
) {
  assert(
    is_ref(tag),
    "'tag' must be a valid git reference - see help(is_ref):\n  ", tag
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Viewing tag '", tag, "' in repository '", repo, "'")
  tag_lst <- gh_url("repos", repo, "tags") %>%
    gh_find(property = "name", value = tag, ...)

  info("Transforming results", level = 4)
  tag_gh <- select_properties(tag_lst, properties$tag)

  info("Done", level = 7)
  tag_gh
}


#  FUNCTION: delete_tag --------------------------------------------------------
#
#' Delete a tag from a repository
#'
#' This function deletes a tag from a repository, as long as you have
#' appropriate permissions. Care should be taken as it will not be recoverable.
#'
#' For more details see the GitHub API documentation:
#'
#' - <https://docs.github.com/en/rest/reference/git#delete-a-reference>
#'
#' @param tag (string) The name of the tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_tag()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   delete_tag("new-tag", repo = "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
delete_tag <- function(
  tag,
  repo,
  ...
) {
  assert(
    is_ref(tag),
    "'tag' must be a valid git reference - see help(is_ref):\n  ", tag
  )
  assert(
    is_repo(repo),
    "'repo' must be a string in the format 'owner/repo':\n  ", repo
  )

  info("Deleting tag '", tag, "' in repository '", repo, "'")
  response <- gh_url("repos", repo, "git/refs/tags", tag) %>%
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
