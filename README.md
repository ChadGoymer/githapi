# {githapi}

<!-- badges: start -->
[![Windows](https://github.com/ChadGoymer/githapi/workflows/Windows/badge.svg)](https://github.com/ChadGoymer/githapi/actions?query=workflow%3AWindows) [![MacOS](https://github.com/ChadGoymer/githapi/workflows/MacOS/badge.svg)](https://github.com/ChadGoymer/githapi/actions?query=workflow%3AMacOS) [![Ubuntu](https://github.com/ChadGoymer/githapi/workflows/Ubuntu/badge.svg)](https://github.com/ChadGoymer/githapi/actions?query=workflow%3AUbuntu) [![Coverage status](https://codecov.io/gh/ChadGoymer/githapi/branch/main/graph/badge.svg)](https://codecov.io/github/ChadGoymer/githapi?branch=main)
<!-- badges: end -->

User-friendly access to the GitHub API for R, consistent with the tidyverse.

## Overview

GitHub is a popular host for version controlled code, but also data. This 
package provides a user-friendly way to access content within GitHub through v3 
of its [API](https://developer.github.com/v3/). The functions are also 
consistent with the [{tidyverse}](http://www.tidyverse.org/) approach and 
return tibbles where possible.

Detailed documentation can be found at: 
[goymer.me/githapi](https://goymer.me/githapi)

## Installation

The easiest way to install [{githapi}](http://goymer.me/githapi) is with the 
devtools package:

```r
library(devtools)
install_github("ChadGoymer/githapi")
```

## Authorisation

In order to access GitHub authentication is required. The first time you use 
[{githapi}](http://goymer.me/githapi) in an R session you will be redirected 
to the GitHub login page. Enter your details and allow 
[{githapi}](http://goymer.me/githapi) the access required. The token is cached
to avoid having to authorise every time a new R session is opened. The cache is
is saved as a file called `.githapi.oauth` in your `HOME` directory, but this 
can be changed in the 
[configuration](http://goymer.me/githapi/articles/configuration.html)

Alternatively, you can create a 
[personal access token](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token) 
and set it as an environment variable, consistent with the 
[{gh}](https://gh.r-lib.org/) package. You can use either `GITHAPI_TOKEN`, 
`GITHUB_PAT` or `GITHUB_TOKEN` (they are checked in that order) to set the 
token.

More details can be found in the 
[configuration](http://goymer.me/githapi/articles/configuration.html) article.

## Usage

[{githapi}](http://goymer.me/githapi) works best if used as part of the 
[tidyverse](http://www.tidyverse.org/).

```r
library(tidyverse)
library(githapi)
```

### Viewing repositories

[{githapi}](http://goymer.me/githapi) provides many functions for extracting information about repositories. 
For example, to first get a summary of the repositories you can use
[`view_repositories()`](http://goymer.me/githapi/reference/view_repositories.html):

```r
view_repositories("ChadGoymer")
```
```
# GET https://api.github.com/users/ChadGoymer/repos?type=all&sort=created&direction=desc&per_page=100
# A tibble: 12 x 29
       id name  full_name description owner homepage language  size default_branch permission private
    <int> <chr> <chr>     <chr>       <chr> <chr>    <chr>    <dbl> <chr>          <chr>      <lgl>  
 1 3.04e8 user~ ChadGoym~ This is a ~ Chad~ "https:~ NA           0 main           admin      FALSE  
 2 3.04e8 earl~ ChadGoym~ NA          Chad~  NA      NA           1 main           admin      FALSE  
 3 2.63e8 gith~ ChadGoym~ Interface ~ Chad~ ""       NA        3708 master         admin      FALSE  
 4 2.24e8 .git~ ChadGoym~ Issue and ~ Chad~  NA      NA           1 master         admin      FALSE  
 5 1.68e8 msgr  ChadGoym~ An R packa~ Chad~ "http:/~ R          307 master         admin      FALSE  
 6 1.14e8 r-be~ ChadGoym~ A set of g~ Chad~  NA      CSS      41205 master         admin      FALSE  
 7 9.69e7 git-~ ChadGoym~ Simple gui~ Chad~  NA      NA           2 master         admin      FALSE  
 8 9.34e7 gith~ ChadGoym~ User-frien~ Chad~ "http:/~ R         6364 main           admin      FALSE  
 9 8.08e7 r-tr~ ChadGoym~ Training m~ Chad~  NA      R         1229 master         admin      FALSE  
10 4.24e7 r-py~ ChadGoym~ NA          Chad~  NA      HTML      1172 master         admin      FALSE  
11 4.24e7 shin~ ChadGoym~ NA          Chad~  NA      HTML     13408 master         admin      FALSE  
12 3.93e7 chad~ ChadGoym~ My persona~ Chad~  NA      HTML         2 master         admin      FALSE  
# ... with 18 more variables: has_issues <lgl>, has_projects <lgl>, has_wiki <lgl>, has_pages <lgl>,
#   has_downloads <lgl>, allow_squash_merge <lgl>, allow_merge_commit <lgl>, allow_rebase_merge <lgl>,
#   fork <lgl>, archived <lgl>, disabled <lgl>, watchers_count <int>, stargazers_count <int>,
#   forks_count <int>, html_url <chr>, pushed_at <dttm>, created_at <dttm>, updated_at <dttm>
```

For a particular repository you can view a summary of the branches using 
[`view_branches()`](http://goymer.me/githapi/reference/view_branches.html):

```r
view_branches("ChadGoymer/githapi")
```
```
# GET https://api.github.com/repos/ChadGoymer/githapi/git/refs/heads?per_page=100
# A tibble: 2 x 3
  name     ref                 sha                                     
  <chr>    <chr>               <chr>                                   
1 gh-pages refs/heads/gh-pages 06c024809a91384399b4ab53ec22cec6fced9517
2 main     refs/heads/main     3190b02c655b615ad16ecc6cb73ae4ea6ee7b7de
```

Similarly, to get the history of commits for a particular branch use 
[`view_commits()`](http://goymer.me/githapi/reference/view_commits.html):

```r
view_commits("main", "ChadGoymer/githapi")
```
```
# GET https://api.github.com/repos/ChadGoymer/githapi/commits?sha=main&per_page=100
# GET https://api.github.com/repositories/93376315/commits?sha=main&per_page=100&page=2
# GET ...
# A tibble: 888 x 13
   sha   message author_login author_name author_email author_date         committer_login
   <chr> <chr>   <chr>        <chr>       <chr>        <dttm>              <chr>          
 1 3190~ "Merge~ ChadGoymer   Chad Goymer chad.goymer~ 2020-10-28 19:16:50 web-flow       
 2 0d61~ "Remov~ ChadGoymer   Chad Goymer chad.goymer~ 2020-10-28 08:47:09 ChadGoymer     
 3 3946~ "Updat~ ChadGoymer   Chad Goymer chad.goymer~ 2020-10-28 08:41:30 ChadGoymer     
 4 de88~ "Add t~ ChadGoymer   Chad Goymer chad.goymer~ 2020-10-28 08:39:01 ChadGoymer     
 5 c241~ "Repla~ ChadGoymer   Chad Goymer chad.goymer~ 2020-10-28 08:38:21 ChadGoymer     
 6 83c2~ "Merge~ ChadGoymer   Chad Goymer chad.goymer~ 2020-10-24 09:02:33 web-flow       
 7 3478~ "Remov~ ChadGoymer   Chad Goymer chad.goymer~ 2020-10-23 17:34:24 ChadGoymer     
 8 1260~ "Updat~ ChadGoymer   Chad Goymer chad.goymer~ 2020-10-21 09:46:57 ChadGoymer     
 9 0260~ "Added~ ChadGoymer   Chad Goymer chad.goymer~ 2020-10-21 09:45:18 ChadGoymer     
10 5f96~ "Added~ ChadGoymer   Chad Goymer chad.goymer~ 2020-10-21 09:44:49 ChadGoymer     
# ... with 878 more rows, and 6 more variables: committer_name <chr>, committer_email <chr>,
#   committer_date <dttm>, tree_sha <chr>, parents <list>, html_url <chr>
```

Note that, by default, when viewing many items, only the last 1000 are returned. 
If you wish to view more you can set the `n_max` parameter. To get information 
about the files in a particular commit use 
[`view_files()`](http://goymer.me/githapi/reference/view_files.html):

```r
view_files("main", "ChadGoymer/githapi")
```
```
# GET https://api.github.com/repos/ChadGoymer/githapi/git/trees/8d7b90dc23859a707b526d5d2c05c946607fbb77?recursive=TRUE
# A tibble: 165 x 4
   path                   sha                    size html_url                                         
 * <chr>                  <chr>                 <dbl> <chr>                                            
 1 .Rbuildignore          3780f7e3591c87864c21~   163 https://github.com/ChadGoymer/githapi/blob/3190b~
 2 .github/.gitignore     2d19fc766d98a08d9d14~     7 https://github.com/ChadGoymer/githapi/blob/3190b~
 3 .github/ISSUE_TEMPLAT~ f5c658bb37d5dbbc045b~   451 https://github.com/ChadGoymer/githapi/blob/3190b~
 4 .github/ISSUE_TEMPLAT~ ec1f4be1460643e6b548~   479 https://github.com/ChadGoymer/githapi/blob/3190b~
 5 .github/pull_request_~ 7f6c4324235f6d424d10~   140 https://github.com/ChadGoymer/githapi/blob/3190b~
 6 .github/workflows/mac~ b6572fbfc1548e1b624a~  1728 https://github.com/ChadGoymer/githapi/blob/3190b~
 7 .github/workflows/pkg~ 085f013d495bdf84b966~  1509 https://github.com/ChadGoymer/githapi/blob/3190b~
 8 .github/workflows/tes~ 20e567d838fc66506810~  1831 https://github.com/ChadGoymer/githapi/blob/3190b~
 9 .github/workflows/ubu~ 3fd659077a332c1eb7f6~  2112 https://github.com/ChadGoymer/githapi/blob/3190b~
10 .github/workflows/win~ 6309a8159cf1cb59f384~  1426 https://github.com/ChadGoymer/githapi/blob/3190b~
# ... with 155 more rows
```

Note that we have used a branch name here rather than a commit. That is because 
most functions in [{githapi}](http://goymer.me/githapi) allow git references to identify commits. A git 
reference is either a branch, in which case the head commit on the branch is 
used, a tag or the SHA-1 of the commit.

Information about tags and releases can also be viewed by using 
[`view_tags()`](http://goymer.me/githapi/reference/view_tags.html) and 
[`view_releases()`](http://goymer.me/githapi/reference/view_releases.html).

```r
view_tags("ChadGoymer/githapi")
```
```
# GET https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags?per_page=100
# A tibble: 22 x 3
   name     ref                sha                                     
   <chr>    <chr>              <chr>                                   
 1 test-tag refs/tags/test-tag 30426b4f967d8c253b1bb5a67c5838dc306aab50
 2 v0.0.0   refs/tags/v0.0.0   ad7e70df7c81ab7c0edbb26725ae7cf4b2ce8964
 3 v0.1.0   refs/tags/v0.1.0   7ca61bb71f877f462c0b6132759d7c5e507c921f
 4 v0.2.0   refs/tags/v0.2.0   7ee36b18186f0c09389af0eb0e4a6843c2ac853c
 5 v0.3.0   refs/tags/v0.3.0   d378328243626794ca725946c4e0662622aeb933
 6 v0.4.0   refs/tags/v0.4.0   7a22d6dd5520f5238677483689b5f255944b6764
 7 v0.5.0   refs/tags/v0.5.0   dcf71d189010f4ad7ef9630200cf432c61ffa04b
 8 v0.5.1   refs/tags/v0.5.1   e45eb21203e2f77da8d8b81df2a1b109fae01159
 9 v0.5.2   refs/tags/v0.5.2   099944f501b2c2fba940f807b1028dbc5349f29c
10 v0.6.0   refs/tags/v0.6.0   702c033f59081cd12b364d64763d22e12701c62f
# ... with 12 more rows
```

```r
view_releases("ChadGoymer/githapi")
```
```
# GET https://api.github.com/repos/ChadGoymer/githapi/releases?per_page=100
# A tibble: 21 x 12
       id tag   name  body  commit draft prerelease author_login assets html_url created_at         
    <int> <chr> <chr> <chr> <chr>  <lgl> <lgl>      <chr>        <list> <chr>    <dttm>             
 1 2.95e7 0.10~ Rele~ "The~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2020-08-10 17:39:14
 2 1.67e7 0.8.7 Fixe~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2019-04-10 13:12:31
 3 1.64e7 0.8.6 Fixe~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2019-03-27 15:30:50
 4 1.59e7 0.8.5 Usin~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2019-03-05 18:47:00
 5 1.56e7 0.8.2 Depr~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2019-02-18 18:17:18
 6 1.55e7 0.7.3 Add ~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-12-06 07:33:25
 7 1.55e7 0.7.2 Crea~ "# R~ master FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-11-06 14:26:49
 8 1.37e7 0.7.1 The ~ "# R~ 31142~ FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-10-31 06:57:38
 9 1.37e7 0.7.0 Star~ "# R~ 59ef5~ FALSE FALSE      ChadGoymer   <chr ~ https:/~ 2018-10-26 18:02:49
10 1.35e7 test~ Test~ "Thi~ 9bb32~ TRUE  TRUE       ChadGoymer   <chr ~ https:/~ 2018-10-18 07:53:42
# ... with 11 more rows, and 1 more variable: published_at <dttm>
```


### Downloading files from a repository

In addition to getting information about the contents of a repository, [{githapi}](http://goymer.me/githapi) 
can also download files and commits. To download individual files use
[`download_file()`](http://goymer.me/githapi/reference/download_file.html)

```r
download_file(
  from_path = "README.md",
  to_path   = "~/README.md",
  ref       = "main",
  repo      = "ChadGoymer/githapi"
)
```

To download an entire commit use
[`download_commit()`](http://goymer.me/githapi/reference/download_commit.html)

```r
download_commit(
  path = "~/githapi",
  ref  = "main",
  repo = "ChadGoymer/githapi"
)
```

You can also read text files from a commit directly using
[`read_github_file()`](http://goymer.me/githapi/reference/read_github_file.html)

```r
read_github_file("README.md", ref = "main", repo = "ChadGoymer/githapi")
```

and source R scripts directly from GitHub using
[`github_source()`](http://goymer.me/githapi/reference/github_source.html)

```r
github_source(  
  path = "inst/test-data/test-source.R", 
  ref  = "main", 
  repo = "ChadGoymer/githapi"
)
```


### Updating a repository

[{githapi}](http://goymer.me/githapi) also provides a set of functions for updating repositories, for adding 
branches, tags as well as new commits. To create or update a file use
[`create_file()`](http://goymer.me/githapi/reference/create_file.html) or
[`update_file()`](http://goymer.me/githapi/reference/update_file.html)

```r
create_file(
  content = "# This is a new file\\n\\n Created by `create_file()`",
  path    = "new-file.md",
  branch  = "main",
  message = "Created a new file with create_file()",
  repo    = "ChadGoymer/githapi"
)
```

```r
update_file(
  content = "# This is a file\\n\\n Updated by `update_file()`",
  path    = "new-file.md",
  branch  = "main",
  message = "Updated a file with update_file()",
  repo    = "ChadGoymer/githapi"
)
```

These functions create a new commit and specify the contents as strings. To 
upload many files, and/or binary files, use 
[`upload_commit()`](http://goymer.me/githapi/reference/upload_commit.html)

```r
upload_commit(
  path    = "C:/files-to-upload",
  branch  = "main",
  message = "Commit to test upload_commit()",
  repo    = "ChadGoymer/githapi"
)
```

This function, by default, makes a new commit containing the contents of the 
specified directory (see documentation for details).

Finally, tags and releases can be created using 
[`create_tag()`](http://goymer.me/githapi/reference/create_tag.html) and
[`create_release()`](http://goymer.me/githapi/reference/create_release.html) 
respectively.

```r
create_tag(
  name = "new-tag",
  ref  = "main",
  repo = "ChadGoymer/githapi"
)
```

```r
create_release(
  tag  = "1.0.0",
  repo = "ChadGoymer/githapi",
  name = "Initial production release",
  body = "This is a release created by create_release()"
)
```
