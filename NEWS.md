# githapi 0.10.23

- Ignore empty directories when uploading a commit (or tree)

# githapi 0.10.22

- Only set githapi options on-load if they are not already set

# githapi 0.10.21

- Allowed all optional arguments to be set to `NULL`

# githapi 0.10.20

- Fixed `download_file()` so it can download files over 1 MB

# githapi 0.10.19

- Allowed `create_file()` to create files in subfolders
- When checking for file existence use a `tryCatch()` rather than listing
  all files

# githapi 0.10.18

- Changed app key to app ID in configuration
- Added environment variable overrides for configuration settings

# githapi 0.10.17

- Updated function documentation

# githapi 0.10.16

- Expanded `compare_commits()` to return summary statistics

# githapi 0.10.15

- Added ref argument to URL when locating files in `download_file()`, 
  `create_file()`, `update_file()`, `delete_file()` and `view_file()`

# githapi 0.10.14

- Added watch, star and fork counts to `view_repositories()` and
  `view_repository()`

# githapi 0.10.13

- Fixed bug with `gh_url()` for GitHub Enterprise

# githapi 0.10.12

- Fixed bug with label functions so they work with spaces in names
- Updated `gh_url()` so it encodes URLs

# githapi 0.10.11

- Removed all references to "master" in functions and documentation

# githapi 0.10.10

- Added config file and read on load

# githapi 0.10.9

- Added `create_status()`
- Added `view_statuses()`
- Added `view_status()`

# githapi 0.10.8

- Split R-CMD-check into separate file for each OS

# githapi 0.10.7

- Fixed bug in `download_commit()` that deleted all other files and folders in the target path

# githapi 0.10.6

- Shortened temporary path in `download_commit()`

# githapi 0.10.5

- Use `update_repository()` to set team permissions
- Removed `update_team_repository()`
- Removed `remove_team_repository()`

# githapi 0.10.4

- Set R version to 'release' for pkgdown workflow

# githapi 0.10.3

- Added `update_team_repository()`
- Added `remove_team_repository()`
- Updated `view_repositories()` to include `team`
- Updated `view_repository()` to include `team`

# githapi 0.10.2

- Replaced Travis and Appveyor with GitHub Actions

# githapi 0.10.1

- Fixed bug where `gh_page()` sometimes calls github one too many times

# githapi 0.10.0

- Removed deprecated functions
- Removed msgr settings from on-load
- Updated README.md

# githapi 0.9.28

- Removed `try()` from `setup()` and `teardown()` in all tests
- Ensured no tests depend on the "test-githapi" repo or old functions
- Set attributes in the `modify_list()` utility function
- Ensured consistent use of `structure()` throughout
- Ensured consistent positioning of curly braces
- Ensured all uses of githapi functions within functions pass through`...`
- Ensured order of properties are consistent
- Allowed `create_file()`, `update_file()` and `delete_file()` to create a new branch
- Ensured examples have correct spacing
- Replaced "test-githapi" with "githapi in all documentation
- Updated documentation so all `...` are passed to `gh_request()`, except for `view_*s()` functions where it is `gh_page()`
- Corrected spelling errors

# githapi 0.9.27

- Added `create_comment()`
- Added `update_comment()`
- Added `view_comments()`
- Added `view_comment()`
- Added `browse_comment()`
- Added `delete_comment()`

# githapi 0.9.26

- Added `create_gist()`
- Added `update_gist()`
- Added `view_gists()`
- Added `view_gist()`
- Added `browse_gist()`
- Added `delete_gist()`
- Added `download_gist()`
- Added `source_gist()`

# githapi 0.9.25

- Added `create_release()`
- Added `update_release()`
- Added `.view_releases()` commented out
- Added `view_release()`
- Added `browse_release()`
- Added `delete_release()`

# githapi 0.9.24

- Added `.compare_commits()`
- Added `.compare_files()`

# githapi 0.9.23

- Added `write_github_file()`
- Added `write_github_lines()`
- Added `write_github_csv()`
- Added `read_github_file()`
- Added `read_github_lines()`
- Added `read_github_csv()`
- Added `github_source()`
- Added `...` argument to `gh_request()`, `gh_page()`, `gh_find()` and `gh_download()`

# githapi 0.9.22

- Renamed `view_branches()` to `.view_branches()`
- Renamed `view_tags()` to `.view_tags()`
- Renamed `upload_commit()` to `.upload_commit()`
- Renamed `download_commit()` to `.download_commit()`
- Renamed `view_commits()` to `.view_commits()`
- Renamed `gh_download()` to `.gh_download()`
- Added `upload_files()`
- Added `download_file()`
- Added `create_file()`
- Added `update_file()`
- Added `delete_file()`
- Added `.view_files()`
- Added `view_file()`
- Added `browse_files()`
- Added `browse_file()`

# githapi 0.9.21

- Added `upload_commit()` commented out
- Added `download_commit()` commented out
- Added `view_commits()` commented out
- Added `view_commit()`
- Added `browse_commits()`
- Added `browse_commit()`
- Added `view_sha()`
- Reorganised arguments in branch and tag functions
- Removed collapsing of properties with more than one value in issue and pull request functions

# githapi 0.9.20

- Added `create_tag()`
- Added `update_tag()`
- Added `view_tags()` commented out
- Added `view_tag()`
- Added `delete_tag()`
- Fixed OAuth error by setting scope

# githapi 0.9.19

- Added `create_branch()`
- Added `update_branch()`
- Added `view_branches()` commented out
- Added `view_branch()`
- Added `delete_branch()`
- Added `is_ref()`

# githapi 0.9.18

- Added `add_labels()`
- Added `remove_labels()`
- Updated `view_labels()` to allow viewing labels on an issue

# githapi 0.9.17

- Added `create_pull_request()`
- Added `update_pull_request()`
- Added `view_pull_requests()`
- Added `view_pull_request()`
- Added `browse_pull_request()`

# githapi 0.9.16

- Added `create_issue()`
- Added `update_issue()`
- Added `view_issues()`
- Added `view_issue()`
- Added `browse_issue()`
- Corrected `gh_request`() error message
- Made default color random in `create_label()`

# githapi 0.9.15

- Added `create_label()`
- Added `update_label()`
- Added `view_labels()`
- Added `view_label()`
- Added `delete_label()`
- Removed `ID` from milestone properties

# githapi 0.9.14

- Added `create_milestone()`
- Added `update_milestone()`
- Added `view_milestones()`
- Added `view_milestone()`
- Added `browse_milestone()`
- Added `delete_milestone()`

# githapi 0.9.13

- Added `create_repository()`
- Added `update_repository()`
- Added `view_repositories()`
- Added `view_repository()`
- Added `browse_repository()`
- Added `delete_repository()`
- Fixed filter on `view_collaborators()`

# githapi 0.9.12

- Added `update_collaborator()`
- Added `view_collaborators()`
- Added `view_collaborator()`
- Added `delete_collaborator()`

# githapi 0.9.11

- Added `update_membership()`
- Added `view_memberships()`
- Added `view_membership()`
- Added `delete_membership()`

# githapi 0.9.10

Updated the following functions:
- `create_project()`: Only include organization properties if organization project is created
- `update_project()`: Add or update team permissions on an organization project
- `view_projects()`: View the projects a team has access to
- `view_project()`: View a project a team has access to
- `browse_project()`: Browse a project a team has access to
- `delete_project()`: Remove a team from an organization project

# githapi 0.9.9

Added the following functions:
- `create_team()`
- `update_team()`
- `view_teams()`
- `view_team()`
- `browse_team()`
- `delete_team()`

Updated the following functions:
- `update_project()`: no longer has `NULL` defaults
- `update_card()`: no longer has `NULL` defaults
- `update_user()`: no longer has `NULL` defaults
- `update_organization()`: no longer has `NULL` defaults
- Used `org` argument in organization functions for consistency
- Removed beta properties from all organizations functions

Deprecated the following functions:
- `gh_team()`
- `gh_teams()`
- `is_member()`
- `is_manager()`

# githapi 0.9.8

Added the following functions:
- `view_organizations()`
- `view_organization()`
- `browse_organization()`
- `update_organization()`
- `view_memberships()`
- `view_membership()`

Deprecated the following functions:
- `gh_organizations()`
- `gh_organization()`
- `gh_memberships()`
- `gh_membership()`
- `gh_members()`

# githapi 0.9.7

- Added `view_users()`
- Added `view_user()`
- Added `browse_user()`
- Added `update_user()`
- In all `update_*()` functions set default values to `NULL`
- When printing a `github` object, only show URL if the attribute exists
- Deprecated `gh_user()`, `gh_users()` and `gh_user_email()`

# githapi 0.9.6

- Added `create_card()`
- Added `update_card()`
- Added `move_card()`
- Added `view_cards()`
- Added `view_card()`
- Added `delete_card()`
- Use direct URL in `view_column()` when given an ID
- Added more detailed error message in `gh_request()`
- Deprecated `gh_card()` and `gh_cards()`

# githapi 0.9.5

- Added `create_column()`
- Added `update_column()`
- Added `move_column()`
- Added `view_columns()`
- Added `view_column()`
- Added `delete_column()`
- Updated documentation and messages for project functions
- Added scripts for testing and deployment
- Deprecated `gh_column()` and `gh_columns()`

# githapi 0.9.4

- Added `create_project()`
- Added `update_project()`
- Added `view_projects()`
- Added `view_project()`
- Added `browse_project()`
- Added `delete_project()`
- Added utility functions for managing properties
- Deprecated `gh_project()` and `gh_projects()`

# githapi 0.9.3

- Use proxy in `gh_token()`
- Cleaned up use of tokens in `gh_request()`
- Updated `gh_page()` to use new approach
- Added `gh_find()`
- Added tests for `print.github()`
- Corrected spelling mistakes

# githapi 0.9.2

- Ensure all tests pass and leave the test repository unchanged
- Updated issues tests for new labels

# githapi 0.9.1

- Updated `gh_url()` to append path to API rather than replace it
- Updated issues tests for new labels

# githapi 0.9.0

- Imported rlang, stringr, purrr and tibble
- Updated `gh_url()` and `gh_request()` to use above packages
- Updated `gh_token()` to use OAuth

# githapi 0.8.10

- Fixed `upload_tree()` so mode is set correctly when uploading an executable.

# githapi 0.8.9

- Replaced `skip()` with `skip_on_travis()` in tests
- Fixed tests after project name changes
- Ensured package passes `devtools::check()`

# githapi 0.8.8

- Added a NEWS.md file

# githapi 0.8.7

- Fixed bug introduced into `read_files()` which looked for SHA in the wrong repo

# githapi 0.8.6

- Fixed bug in `read_files()` and `download_files()` which prevented them from accessing files 
  in folders

# githapi 0.8.5

- Replaced all the logr functions with the msgr package
- Added test coverage reporting
- Added:
  - `files_exist()`
  - `releases_exist()`

# githapi 0.8.2

- Refactored the new Git and Repository functions changing the order of the parameters
- Deprecated all the old Git and Repository functions that have been replaced
- added:
  - `shas_exist()`
  - `source_files()`
- removed
  - `view_readme()`

# githapi 0.7.3

Added repository commits functions:

- `download_commit()`
- `view_commits()`
- `view_shas()`
- `compare_commits()`
- `compare_files()`

Added git blobs functions:

- `view_blobs()`
- `create_blobs()`
- `update_blobs()`
- `read_files()`
- `download_files()`
- `blob_exists()`

Added git trees functions:

- `view_trees()`
- `create_tree()`
- `upload_tree()`
- `tree_exists()`

Added git commits functions:

- `view_commits()`
- `create_commit()`
- `upload_commit()`
- `commit_exists()`

Added git branches functions:

- `view_branches()`
- `create_branches()`
- `update_branches()`
- `delete_branches()`
- `branch_exists()`

Added git tags functions:

- `tag_exists()`

# githapi 0.7.2

Added:
- `view_files()`
- `create_files()`
- `update_files()`
- `delete_files()`
- `view_readme()`
- `view_contents()`

# githapi 0.7.1

No functional changes, change fixed broken tests and refactored new functions.

# githapi 0.7.0

This release starts the process of replacing the naming convention of functions and adding 
support for creating, updated and deleting objects in GitHub. It also adds useful messages, 
and logging.

- Added `info()`, `warn()` and `error()` functions which give nice messages to the users and 
  optionally logs them to a file.
- Added `view_tags()`, `create_tags()`, `update_tags()` and `delete_tags()`
- Added `view_releases()`, `create_releases()`, `update_releases()` and `delete_releases()`

# githapi 0.6.3

Resulting from the change to the README in the master branch

# githapi 0.6.2

Added configuration files to get Travis working for continuous integration. Also, fixed a number 
of issues which were causing the build to fail.

# githapi 0.6.1

Converted documentation to markdown and generated a website using `pkgdown`.

# githapi 0.6.0

This release completed replaced the underlying functions to remove dependencies on unnecessary 
packages. As a result, the package loads faster and with fewer conflicts. Tables are generally 
faster to parse also.

# githapi 0.5.2

Fixed a bug where empty columns threw an error. For example requesting the cards, using 
`gh_cards()`, in an empty column of a project.

# githapi 0.5.1

 - `gh_commit()` and `gh_tag()` now return an extra list item called "verification"
 - Update the `gh_get()` test due to change in README

# githapi 0.5.0

 - Added functions for saving and sourcing files from GitHub
 - Added functions to test for existence
 - Added a safe version of select function

# githapi 0.4.0

- added `gh_json()`
- fixed `gh_page()` to return correct number
- added `gh_tibble()` and made gh_page more robust
- added `gh_projects()`
- added `gh_project()`
- added `gh_columns()`
- added `gh_cards()`
- added `gh_column()`
- added `gh_card()`
- `gh_page()` now uses `n_max`
- removed `gh_tibble()`, `gh_json()` and flatten
- added `gh_rate_limit()` and warning in `gh_get()`
- `gh_issues()`: collapsed assignees login to comma-separated string
- update docs for `n_max` parameter
- added `gh_gists()`
- added `gh_gist()`
- added `gh_gist_commits()`
- added `is_gist_starred()`
- Changed all functions returning booleans to `is_*()`
- added `gh_gist_forks()`
- added `gh_events()`
- added `gh_event()`
- added `gh_commit_comments()`
- added `gh_commit_comment()`
- added `gh_contributers()`
- extended `gh_teams()` to include repos, without tests
- added `gh_languages()`
- added `gh_releases()` and collapse_list
- added `gh_release()`
- added `gh_assets()`
- added `gh_asset()`
- added `gh_gist_comments()`
- added `gh_gist_comment()`

# githapi 0.3.0

- added `gh_label()`
- added `gh_labels()`
- expanded `gh_labels()` to include issues and milestones
- added `gh_milestone()`
- added `gh_milestones()`
- added `gh_organizations()`
- added `gh_organization()`
- added `gh_member()`
- added `gh_members()`
- added `gh_membership()` and `gh_memberships()` without tests
- added `gh_team()` and `gh_teams()` without tests
- extended `gh_members()` and `gh_membership()` to include teams
- Added the "closed_at" column to `gh_issues()`
- extended `gh_repositories()` to include teams
- added `gh_manager()` without tests
- extended `gh_teams()` to include users teams without tests
- added `gh_pull_requests()`
- added `gh_pull_request()`
- added `gh_pull_commits()`
- added `gh_pull_files()`
- added `gh_pull_merged()`
- added `gh_pull_review()` without tests
- added `gh_pull_reviews()` without tests
- added `gh_pull_comments()` without tests
- added `gh_pull_comment()` without tests
- added `gh_pull_review_requests()` without tests
- added `gh_collaborators()`
- added `gh_collaborator()`
- added `gh_permissions()`
- added `gh_user_email()`

# githapi 0.2.0

Added functions for accessing the Git Data API
