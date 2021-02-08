context("repositories")


# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

setup(suppressMessages({

  create_team(
    name        = str_c("test-repositories-", suffix),
    org         = "HairyCoos",
    description = "This is a team to test repositories"
  )

}))

teardown(suppressMessages({

  try(silent = TRUE, {
    delete_team(str_c("test-repositories-", suffix), org = "HairyCoos")
  })

  try(silent = TRUE, {
    delete_repository(str_c("ChadGoymer/test-user-repository-", suffix))
  })

  try(silent = TRUE, {
    delete_repository(str_c("HairyCoos/test-org-repository-", suffix))
  })

  try(silent = TRUE, {
    delete_repository(str_c("ChadGoymer/test-updated-user-repository-", suffix))
  })

  try(silent = TRUE, {
    delete_repository(str_c("HairyCoos/test-updated-org-repository-", suffix))
  })

}))

# TEST: create_repository ------------------------------------------------------

test_that("create_repository creates a repository and returns its properties", {

  user_repo <- create_repository(
    name        = str_c("test-user-repository-", suffix),
    description = "This is a user repository",
    homepage    = "https://user-repository.com",
    auto_init   = TRUE
  )

  Sys.sleep(1)

  expect_is(user_repo, "list")
  expect_identical(attr(user_repo, "status"), 201L)
  expect_identical(
    map_chr(user_repo, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_identical(
    user_repo$full_name,
    str_c("ChadGoymer/test-user-repository-", suffix)
  )
  expect_identical(user_repo$description, "This is a user repository")
  expect_identical(user_repo$homepage, "https://user-repository.com")


  org_repo <- create_repository(
    name        = str_c("test-org-repository-", suffix),
    org         = "HairyCoos",
    description = "This is an organization respository",
    homepage    = "https://org-repository.com"
  )

  expect_is(org_repo, "list")
  expect_identical(attr(org_repo, "status"), 201L)
  expect_identical(
    map_chr(org_repo, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_identical(
    org_repo$full_name,
    str_c("HairyCoos/test-org-repository-", suffix)
  )
  expect_identical(org_repo$description, "This is an organization respository")
  expect_identical(org_repo$homepage, "https://org-repository.com")

})


# TEST: update_repository ------------------------------------------------------

test_that("update_repository changes a repository's properties", {

  user_repo <- update_repository(
    repo           = str_c("ChadGoymer/test-user-repository-", suffix),
    name           = str_c("test-updated-user-repository-", suffix),
    description    = "This is an updated user respository",
    homepage       = "https://updated-user-repository.com",
    has_issues     = FALSE,
    has_projects   = FALSE,
    has_wiki       = FALSE,
    default_branch = "main"
  )

  expect_is(user_repo, "list")
  expect_identical(attr(user_repo, "status"), 200L)
  expect_identical(
    map_chr(user_repo, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_identical(
    user_repo$full_name,
    str_c("ChadGoymer/test-updated-user-repository-", suffix)
  )
  expect_identical(user_repo$description, "This is an updated user respository")
  expect_identical(user_repo$homepage, "https://updated-user-repository.com")
  expect_false(user_repo$has_issues)
  expect_false(user_repo$has_projects)
  expect_false(user_repo$has_wiki)
  expect_identical(user_repo$default_branch, "main")


  org_repo <- update_repository(
    repo                   = str_c("HairyCoos/test-org-repository-", suffix),
    name                   = str_c("test-updated-org-repository-", suffix),
    description            = "This is an updated organization respository",
    homepage               = "https://updated-org-repository.com",
    private                = FALSE,
    allow_squash_merge     = FALSE,
    allow_merge_commit     = FALSE,
    allow_rebase_merge     = TRUE,
    delete_branch_on_merge = TRUE
  )

  expect_is(org_repo, "list")
  expect_identical(attr(org_repo, "status"), 200L)
  expect_identical(
    map_chr(org_repo, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_identical(
    org_repo$full_name,
    str_c("HairyCoos/test-updated-org-repository-", suffix)
  )
  expect_identical(
    org_repo$description,
    "This is an updated organization respository"
  )
  expect_identical(org_repo$homepage, "https://updated-org-repository.com")
  expect_false(org_repo$private)
  expect_false(org_repo$allow_squash_merge)
  expect_false(org_repo$allow_merge_commit)
  expect_true(org_repo$allow_rebase_merge)


  archived_repo <- update_repository(
    repo     = str_c("ChadGoymer/test-updated-user-repository-", suffix),
    archived = TRUE
  )

  expect_is(archived_repo, "list")
  expect_identical(attr(archived_repo, "status"), 200L)
  expect_identical(
    map_chr(archived_repo, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_true(archived_repo$archived)


  added_team_repo <- update_repository(
    repo       = str_c("HairyCoos/test-updated-org-repository-", suffix),
    team       = str_c("test-repositories-", suffix),
    permission = "pull"
  )

  expect_is(added_team_repo, "list")
  expect_identical(attr(added_team_repo, "status"), 200L)
  expect_identical(
    map_chr(added_team_repo, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  read_repo <- view_repository(
    repo = str_c("HairyCoos/test-updated-org-repository-", suffix),
    team = str_c("test-repositories-", suffix),
    org  = "HairyCoos"
  )

  expect_identical(read_repo$permission, "pull")


  updated_team_repo <- update_repository(
    repo       = str_c("HairyCoos/test-updated-org-repository-", suffix),
    team       = str_c("test-repositories-", suffix),
    permission = "maintain"
  )

  expect_is(updated_team_repo, "list")
  expect_identical(attr(updated_team_repo, "status"), 200L)
  expect_identical(
    map_chr(updated_team_repo, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  maintain_repo <- view_repository(
    repo = str_c("HairyCoos/test-updated-org-repository-", suffix),
    team = str_c("test-repositories-", suffix),
    org  = "HairyCoos"
  )

  expect_identical(maintain_repo$permission, "maintain")

})


# TEST: view_repositories ------------------------------------------------------

test_that("view_repositories returns a tibble summarising the repositories", {

  user_repos <- view_repositories(user = "ChadGoymer", n_max = 10)

  expect_is(user_repos, "tbl")
  expect_identical(attr(user_repos, "status"), 200L)
  expect_identical(
    map_chr(user_repos, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_true(
    str_c("test-updated-user-repository-", suffix) %in% user_repos$name
  )
  expect_identical(
    sort(user_repos$created_at, decreasing = TRUE),
    user_repos$created_at
  )


  ordered_repos <- view_repositories(
    user      = "ChadGoymer",
    sort      = "full_name",
    direction = "asc",
    n_max     = 10
  )

  expect_is(ordered_repos, "tbl")
  expect_identical(attr(ordered_repos, "status"), 200L)
  expect_identical(
    map_chr(ordered_repos, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_identical(sort(ordered_repos$full_name), ordered_repos$full_name)


  org_repos <- view_repositories(org = "HairyCoos", n_max = 10)

  expect_is(org_repos, "tbl")
  expect_identical(attr(org_repos, "status"), 200L)
  expect_identical(
    map_chr(org_repos, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_true(str_c("test-updated-org-repository-", suffix) %in% org_repos$name)
  expect_identical(
    org_repos %>%
      filter(name == str_c("test-updated-org-repository-", suffix)) %>%
      pull("permission"),
    "admin"
  )


  team_repos <- view_repositories(
    team  = str_c("test-repositories-", suffix),
    org   = "HairyCoos",
    n_max = 10
  )

  expect_is(team_repos, "tbl")
  expect_identical(attr(team_repos, "status"), 200L)
  expect_identical(
    map_chr(team_repos, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_true(
    str_c("test-updated-org-repository-", suffix) %in% team_repos$name
  )
  expect_identical(
    team_repos %>%
      filter(name == str_c("test-updated-org-repository-", suffix)) %>%
      pull("permission"),
    "maintain"
  )


  auth_repos <- view_repositories(n_max = 10)

  expect_is(auth_repos, "tbl")
  expect_identical(attr(auth_repos, "status"), 200L)
  expect_identical(
    map_chr(auth_repos, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_true(
    str_c("test-updated-user-repository-", suffix) %in% auth_repos$name
  )

})


# TEST: view_repository --------------------------------------------------------

test_that("view_repository returns a list of repository properties", {

  test_repo <- view_repository(
    repo = str_c("ChadGoymer/test-updated-user-repository-", suffix)
  )

  expect_is(test_repo, "list")
  expect_identical(attr(test_repo, "status"), 200L)
  expect_identical(
    map_chr(test_repo, ~ class(.)[[1]]),
    c(
      id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      watchers_count     = "integer",
      stargazers_count   = "integer",
      forks_count        = "integer",
      html_url           = "character",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"
    )
  )

  expect_identical(
    test_repo$name,
    str_c("test-updated-user-repository-", suffix)
  )

})


# TEST: browse_repository ------------------------------------------------------

test_that("browse_repository opens the repository's page in the browser", {

  skip_if(!interactive(), "browse_repository must be tested manually")

  repo <- browse_repository(
    repo = str_c("ChadGoymer/test-updated-user-repository-", suffix)
  )

  expect_is(repo, "character")
  expect_identical(attr(repo, "status"), 200L)
  expect_identical(
    as.character(repo),
    str_c("https://github.com/ChadGoymer/test-updated-user-repository-", suffix)
  )

})


# TEST: delete_repository ------------------------------------------------------

test_that("delete_repository removes a repository and returns TRUE", {

  user_repo <- delete_repository(
    repo = str_c("ChadGoymer/test-updated-user-repository-", suffix)
  )

  expect_is(user_repo, "logical")
  expect_identical(attr(user_repo, "status"), 204L)
  expect_identical(as.logical(user_repo), TRUE)

  org_repo <- delete_repository(
    repo = str_c("HairyCoos/test-updated-org-repository-", suffix)
  )

  expect_is(org_repo, "logical")
  expect_identical(attr(org_repo, "status"), 204L)
  expect_identical(as.logical(org_repo), TRUE)

})
