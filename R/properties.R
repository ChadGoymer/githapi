# LIST: properties -------------------------------------------------------------
#
# The properties to show for all GitHub entities
#
# This list is used in the `view_*()` functions to extract the required
# properties and convert to the desired column type. The settings can also be
# used to rename properties.
#
properties <- list(

  project = list(
    id                              = c("id",                              as = "integer"),
    number                          = c("number",                          as = "integer"),
    name                            = c("name",                            as = "character"),
    body                            = c("body",                            as = "character"),
    state                           = c("state",                           as = "character"),
    creator                         = c("creator", "login",                as = "character"),
    html_url                        = c("html_url",                        as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime")),

  column = list(
    id                              = c("id",                              as = "integer"),
    name                            = c("name",                            as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime")),

  card = list(
    id                              = c("id",                              as = "integer"),
    note                            = c("note",                            as = "character"),
    archived                        = c("archived",                        as = "logical"),
    creator                         = c("creator", "login",                as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime"),
    content_url                     = c("content_url",                     as = "character")),

  users = list(
    id                              = c("id",                              as = "integer"),
    login                           = c("login",                           as = "character"),
    site_admin                      = c("site_admin",                      as = "logical"),
    html_url                        = c("html_url",                        as = "character")),

  user = list(
    id                              = c("id",                              as = "integer"),
    login                           = c("login",                           as = "character"),
    name                            = c("name",                            as = "character"),
    email                           = c("email",                           as = "character"),
    blog                            = c("blog",                            as = "character"),
    company                         = c("company",                         as = "character"),
    location                        = c("location",                        as = "character"),
    hireable                        = c("hireable",                        as = "logical"),
    bio                             = c("bio",                             as = "character"),
    site_admin                      = c("site_admin",                      as = "logical"),
    html_url                        = c("html_url",                        as = "character")),

  organizations = list(
    id                              = c("id",                              as = "integer"),
    login                           = c("login",                           as = "character"),
    description                     = c("description",                     as = "character")),

  organization = list(
    id                              = c("id",                              as = "integer"),
    login                           = c("login",                           as = "character"),
    name                            = c("name",                            as = "character"),
    description                     = c("description",                     as = "character"),
    company                         = c("company",                         as = "character"),
    blog                            = c("blog",                            as = "character"),
    location                        = c("location",                        as = "character"),
    email                           = c("email",                           as = "character"),
    is_verified                     = c("is_verified",                     as = "logical"),
    has_organization_projects       = c("has_organization_projects",       as = "logical"),
    has_repository_projects         = c("has_repository_projects",         as = "logical"),
    public_repos                    = c("public_repos",                    as = "integer"),
    public_gists                    = c("public_gists",                    as = "integer"),
    total_private_repos             = c("total_private_repos",             as = "integer"),
    owned_private_repos             = c("owned_private_repos",             as = "integer"),
    private_gists                   = c("private_gists",                   as = "integer"),
    disk_usage                      = c("disk_usage",                      as = "numeric"),
    collaborators                   = c("collaborators",                   as = "integer"),
    billing_email                   = c("billing_email",                   as = "character"),
    plan_name                       = c("plan", "name",                    as = "character"),
    plan_space                      = c("plan", "space",                   as = "integer"),
    plan_private_repos              = c("plan", "private_repos",           as = "integer"),
    default_repository_permission   = c("default_repository_permission",   as = "character"),
    two_factor_requirement_enabled  = c("two_factor_requirement_enabled",  as = "logical"),
    members_can_create_repositories = c("members_can_create_repositories", as = "logical"),
    html_url                        = c("html_url",                        as = "character"),
    created_at                      = c("created_at",                      as = "datetime")),

  memberships = list(
    user                            = c("user", "login",                   as = "character"),
    organization                    = c("organization", "login",           as = "character"),
    role                            = c("role",                            as = "character"),
    state                           = c("state",                           as = "character")),

  teams = list(
    id                              = c("id",                              as = "integer"),
    name                            = c("name",                            as = "character"),
    slug                            = c("slug",                            as = "character"),
    description                     = c("description",                     as = "character"),
    privacy                         = c("privacy",                         as = "character"),
    permission                      = c("permission",                      as = "character"),
    parent                          = c("parent", "name",                  as = "character"),
    html_url                        = c("html_url",                        as = "character")),

  team = list(
    id                              = c("id",                              as = "integer"),
    name                            = c("name",                            as = "character"),
    slug                            = c("slug",                            as = "character"),
    description                     = c("description",                     as = "character"),
    privacy                         = c("privacy",                         as = "character"),
    permission                      = c("permission",                      as = "character"),
    parent                          = c("parent", "name",                  as = "character"),
    organization                    = c("organization", "login",           as = "character"),
    members_count                   = c("members_count",                   as = "integer"),
    repos_count                     = c("repos_count",                     as = "integer"),
    html_url                        = c("html_url",                        as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime")),

  repository = list(
    id                              = c("id",                              as = "integer"),
    name                            = c("name",                            as = "character"),
    full_name                       = c("full_name",                       as = "character"),
    description                     = c("description",                     as = "character"),
    owner                           = c("owner", "login",                  as = "character"),
    homepage                        = c("homepage",                        as = "character"),
    language                        = c("language",                        as = "character"),
    size                            = c("size",                            as = "numeric"),
    default_branch                  = c("default_branch",                  as = "character"),
    private                         = c("private",                         as = "logical"),
    has_issues                      = c("has_issues",                      as = "logical"),
    has_projects                    = c("has_projects",                    as = "logical"),
    has_wiki                        = c("has_wiki",                        as = "logical"),
    has_pages                       = c("has_pages",                       as = "logical"),
    has_downloads                   = c("has_downloads",                   as = "logical"),
    allow_squash_merge              = c("allow_squash_merge",              as = "logical"),
    allow_merge_commit              = c("allow_merge_commit",              as = "logical"),
    allow_rebase_merge              = c("allow_rebase_merge",              as = "logical"),
    fork                            = c("fork",                            as = "logical"),
    archived                        = c("archived",                        as = "logical"),
    disabled                        = c("disabled",                        as = "logical"),
    watchers_count                  = c("watchers_count",                  as = "integer"),
    stargazers_count                = c("stargazers_count",                as = "integer"),
    forks_count                     = c("forks_count",                     as = "integer"),
    html_url                        = c("html_url",                        as = "character"),
    pushed_at                       = c("pushed_at",                       as = "datetime"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime")),

  label = list(
    name                            = c("name",                            as = "character"),
    color                           = c("color",                           as = "character"),
    description                     = c("description",                     as = "character")),

  milestone = list(
    number                          = c("number",                          as = "integer"),
    title                           = c("title",                           as = "character"),
    description                     = c("description",                     as = "character"),
    state                           = c("state",                           as = "character"),
    open_issues                     = c("open_issues",                     as = "integer"),
    closed_issues                   = c("closed_issues",                   as = "integer"),
    creator                         = c("creator", "login",                as = "character"),
    html_url                        = c("html_url",                        as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime"),
    due_on                          = c("due_on",                          as = "datetime"),
    closed_at                       = c("closed_at",                       as = "datetime")),

  issue = list(
    number                          = c("number",                          as = "integer"),
    title                           = c("title",                           as = "character"),
    body                            = c("body",                            as = "character"),
    milestone                       = c("milestone", "title",              as = "character"),
    state                           = c("state",                           as = "character"),
    repository                      = c("repo", "full_name",               as = "character"),
    creator                         = c("user", "login",                   as = "character"),
    html_url                        = c("html_url",                        as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime"),
    closed_at                       = c("closed_at",                       as = "datetime")),

  pull_request = list(
    number                          = c("number",                          as = "integer"),
    title                           = c("title",                           as = "character"),
    body                            = c("body",                            as = "character"),
    head_sha                        = c("head", "sha",                     as = "character"),
    head_ref                        = c("head", "ref",                     as = "character"),
    head_repo                       = c("head", "repo", "full_name",       as = "character"),
    base_sha                        = c("base", "sha",                     as = "character"),
    base_ref                        = c("base", "ref",                     as = "character"),
    merge_sha                       = c("merge_commit_sha",                as = "character"),
    milestone                       = c("milestone", "title",              as = "character"),
    state                           = c("state",                           as = "character"),
    repository                      = c("repo", "full_name",               as = "character"),
    diff_url                        = c("diff_url",                        as = "character"),
    creator                         = c("user", "login",                   as = "character"),
    mergeable                       = c("mergeable",                       as = "logical"),
    rebaseable                      = c("rebaseable",                      as = "logical"),
    merged                          = c("merged",                          as = "logical"),
    merged_by                       = c("merged_by", "login",              as = "character"),
    html_url                        = c("html_url",                        as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime"),
    merged_at                       = c("merged_at",                       as = "datetime"),
    closed_at                       = c("closed_at",                       as = "datetime")),

  pull_commits = list(
    message                         = c("commit", "message",               as = "character"),
    author_name                     = c("commit", "author", "name",        as = "character"),
    author_email                    = c("commit", "author", "email",       as = "character"),
    author_date                     = c("commit", "author", "date",        as = "datetime"),
    committer_name                  = c("commit", "committer", "name",     as = "character"),
    committer_email                 = c("commit", "committer", "email",    as = "character"),
    committer_date                  = c("commit", "committer", "date",     as = "datetime"),
    html_url                        = c("html_url",                        as = "character")),

  pull_files = list(
    sha                             = c("sha",                             as = "character"),
    filename                        = c("filename",                        as = "character"),
    status                          = c("status",                          as = "character"),
    additions                       = c("additions",                       as = "integer"),
    deletions                       = c("deletions",                       as = "integer"),
    changes                         = c("changes",                         as = "integer"),
    patch                           = c("patch",                           as = "character"),
    html_url                        = c("blob_url",                        as = "character")),

  pull_reviews = list(
    body                            = c("body",                            as = "character"),
    state                           = c("state",                           as = "character"),
    user                            = c("user", "login",                   as = "character"),
    html_url                        = c("html_url",                        as = "character"),
    submitted_at                    = c("submitted_at",                    as = "datetime")),

  branch = list(
    name                            = c("name",                            as = "character"),
    sha                             = c("commit", "sha",                   as = "character"),
    protected                       = c("protected",                       as = "logical"),
    html_url                        = c("commit", "url",                   as = "character")),

  tag = list(
    name                            = c("name",                            as = "character"),
    sha                             = c("commit", "sha",                   as = "character"),
    html_url                        = c("commit", "url",                   as = "character"),
    zip_url                         = c("zipball_url",                     as = "character"),
    tar_url                         = c("tarball_url",                     as = "character")),

  commit = list(
    sha                             = c("sha",                             as = "character"),
    message                         = c("commit", "message",               as = "character"),
    author_login                    = c("author", "login",                 as = "character"),
    author_name                     = c("commit", "author", "name",        as = "character"),
    author_email                    = c("commit", "author", "email",       as = "character"),
    author_date                     = c("commit", "author", "date",        as = "datetime"),
    committer_login                 = c("committer", "login",              as = "character"),
    committer_name                  = c("commit", "committer", "name",     as = "character"),
    committer_email                 = c("commit", "committer", "email",    as = "character"),
    committer_date                  = c("commit", "committer", "date",     as = "datetime"),
    tree_sha                        = c("commit", "tree", "sha",           as = "character"),
    html_url                        = c("html_url",                        as = "character")),

  file = list(
    path                            = c("path",                            as = "character"),
    type                            = c("type",                            as = "character"),
    sha                             = c("sha",                             as = "character"),
    size                            = c("size",                            as = "numeric"),
    html_url                        = c("html_url",                        as = "character")),

  compare_commits = list(
    status                          = c("status",                          as = "character"),
    ahead_by                        = c("ahead_by",                        as = "integer"),
    behind_by                       = c("behind_by",                       as = "integer"),
    total_commits                   = c("total_commits",                   as = "integer"),
    html_url                        = c("html_url",                        as = "character")),

  compare_files = list(
    path                            = c("filename",                        as = "character"),
    sha                             = c("sha",                             as = "character"),
    status                          = c("status",                          as = "character"),
    additions                       = c("additions",                       as = "integer"),
    deletions                       = c("deletions",                       as = "integer"),
    changes                         = c("changes",                         as = "integer"),
    patch                           = c("patch",                           as = "character"),
    html_url                        = c("blob_url",                        as = "character")),

  release = list(
    id                              = c("id",                              as = "integer"),
    tag                             = c("tag_name",                        as = "character"),
    name                            = c("name",                            as = "character"),
    body                            = c("body",                            as = "character"),
    commit                          = c("target_commitish",                as = "character"),
    draft                           = c("draft",                           as = "logical"),
    prerelease                      = c("prerelease",                      as = "logical"),
    author_login                    = c("author", "login",                 as = "character"),
    html_url                        = c("html_url",                        as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    published_at                    = c("published_at",                    as = "datetime")),

  gist = list(
    id                              = c("id",                              as = "character"),
    description                     = c("description",                     as = "character"),
    owner                           = c("owner", "login",                  as = "character"),
    public                          = c("public",                          as = "logical"),
    html_url                        = c("html_url",                        as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime")),

  gist_file = list(
    filename                        = c("filename",                        as = "character"),
    type                            = c("type",                            as = "character"),
    content                         = c("content",                         as = "character"),
    size                            = c("size",                            as = "integer"),
    truncated                       = c("truncated",                       as = "logical")),

  issue_comment = list(
    id                              = c("id",                              as = "integer"),
    body                            = c("body",                            as = "character"),
    user                            = c("user", "login",                   as = "character"),
    html_url                        = c("html_url",                        as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime")),

  commit_comment = list(
    id                              = c("id",                              as = "integer"),
    body                            = c("body",                            as = "character"),
    commit                          = c("commit_id",                       as = "character"),
    path                            = c("path",                            as = "character"),
    position                        = c("position",                        as = "integer"),
    user                            = c("user", "login",                   as = "character"),
    html_url                        = c("html_url",                        as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime")),

  status = list(
    id                              = c("id",                              as = "character"),
    state                           = c("state",                           as = "character"),
    description                     = c("description",                     as = "character"),
    target_url                      = c("target_url",                      as = "character"),
    context                         = c("context",                         as = "character"),
    creator                         = c("creator", "login",                as = "character"),
    created_at                      = c("created_at",                      as = "datetime"),
    updated_at                      = c("updated_at",                      as = "datetime"))

)


# LIST: values -----------------------------------------------------------------
#
# List of allowed values
#
# This list contains the allowed values the specified properties, as defined by
# enumerations in the GitHub documentation. They are used to validate
# parameters.
#
values <- list(

  project = list(
    state                         = c("open", "closed", "all"),
    permission                    = c("read", "write", "admin")),

  column = list(
    position                      = c("first", "last")),

  card = list(
    content_type                  = c("Issue", "PullRequest"),
    archived_state                = c("archived", "not_archived", "all"),
    position                      = c("top", "bottom")),

  user = list(
    role                          = c("member", "admin", "all")),

  organization = list(
    default_repository_permission = c("read", "write", "admin", "none")),

  team = list(
    privacy                       = c("secret", "closed")),

  membership = list(
    state                         = c("active", "pending"),
    org_role                      = c("member", "admin"),
    team_role                     = c("member", "maintainer")),

  collaborator = list(
    affiliation                   = c("outside", "direct", "all")),

  repository = list(
    sort                          = c("created", "updated", "pushed", "full_name"),
    direction                     = c("asc", "desc"),
    permission                    = c("pull", "push", "admin"),
    team_permission               = c("pull", "triage", "push", "maintain", "admin")),

  milestone = list(
    state                         = c("open", "closed", "all"),
    sort                          = c("due_on", "completeness"),
    direction                     = c("asc", "desc")),

  issue = list(
    state                         = c("open", "closed", "all"),
    sort                          = c("created", "updated", "comments"),
    direction                     = c("asc", "desc")),

  pull_request = list(
    state                         = c("open", "closed", "all"),
    sort                          = c("created", "updated", "popularity", "long-running"),
    direction                     = c("asc", "desc")),

  comment = list(
    type                          = c("issue", "pull_request", "commit")),

  status = list(
    state                         = c("error", "failure", "pending", "success"))

)
