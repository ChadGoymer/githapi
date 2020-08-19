#  FUNCTION: create_card -------------------------------------------------------
#
#' Create a card in a GitHub project
#'
#' This function creates a new card in the specified column in a project in
#' GitHub. The card can either contain an existing issue or pull request or you
#' can create a note.
#'
#' You can create a card in a project associated with either a repository, user
#' or organization, by supplying them as an input, as long as you have
#' appropriate permissions.
#'
#' For more details see the GitHub API documentation:
#' - <https://docs.github.com/en/rest/reference/projects#create-a-project-card>
#'
#' @param content_id (integer, optional) The issue or pull request id associated
#'   with this card. Note: `content_type` is also required.
#' @param content_type (string, optional) Whether the `content_id` identifies an
#'   `"Issue"` or a `"PullRequest"`.
#' @param note (string, optional) The card's note content. Not used if
#'   `content_id` is specified.
#' @param column (integer or string) Either the ID or name of the column.
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_card()` returns a list of the card properties.
#'
#' **Card Properties:**
#'
#' - **id**: The ID of the card.
#' - **content_id**: The ID of the issue or pull request.
#' - **note**: The content of a note.
#' - **archived**: Whether the card has been archived.
#' - **creator**: The creator of the note.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # Create an issue card in a repository project
#'   create_card(
#'     content_id   = 1,
#'     content_type = "Issue",
#'     column       = "Test column",
#'     project      = "Test project",
#'     repo         = "ChadGoymer/githapi"
#'   )
#'
#'   # Create a pull request card in a user's project
#'   create_card(
#'     content_id   = 2,
#'     content_type = "PullRequest",
#'     column       = "Test column",
#'     project      = "Test project",
#'     user         = "ChadGoymer"
#'   )
#'
#'   # Create a note card in an organization's project
#'   create_card(
#'     note    = "This is a note",
#'     column  = "Test column",
#'     project = "Test project",
#'     org     = "HairyCoos"
#'   )
#'
#' }
#'
#' @export
#'
create_card <- function(
  content_id,
  content_type,
  note,
  column,
  project,
  repo,
  user,
  org,
  ...
) {
  if (!missing(content_id)) {
    assert(
      is_scalar_integerish(content_id),
      "'content_id' must be an integer:\n  ", content_id
    )
    assert(
      is_scalar_character(content_type),
      "'content_type' must be a string:\n  ", content_type
    )
    assert(
      content_type %in% values$card$content_type,
      "'content_type' must be one of '",
      str_c(values$card$content_type, collapse = "', '"),
      "':\n  ", content_type
    )

    issue <- switch(
      content_type,
      Issue       = gh_url("repos", repo, "issues", content_id) %>%
        gh_request("GET", ...),
      PullRequest = gh_url("repos", repo, "pulls", content_id) %>%
        gh_request("GET", ...)
    )

    info("Creating card for ", tolower(content_type), " '", content_id, "'")
    payload <- list(content_id = issue$id, content_type = content_type)
  }
  else if (!missing(note)) {
    assert(
      is_scalar_character(note),
      "'note' must be an integer:\n  ", note
    )

    info("Creating card with specified note")
    payload    <- list(note = note)
    content_id <- NA
  }
  else {
    error("Either 'content_id' or 'note' must be supplied")
  }

  column <- view_column(
    column  = column,
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...
  )

  card_lst <- gh_url("projects/columns", column$id, "cards") %>%
    gh_request(
      type    = "POST",
      payload = payload,
      accept  = "application/vnd.github.inertia-preview+json",
      ...
    )

  info("Transforming results", level = 4)
  card_gh <- select_properties(card_lst, properties$card) %>%
    modify_list(content_id   = as.integer(content_id), .after = "id") %>%
    discard(names(.) == "content_url")

  info("Done", level = 7)
  structure(
    card_gh,
    class   = class(card_lst),
    url     = attr(card_lst, "url"),
    request = attr(card_lst, "request"),
    status  = attr(card_lst, "status"),
    header  = attr(card_lst, "header")
  )
}


#  FUNCTION: update_card -------------------------------------------------------
#
#' Update a card in a GitHub project
#'
#' `update_card()` can be used to change a card's note in a project in GitHub or
#' archive it. `move_card()` can be used to reorder the cards or move them to
#' other columns.
#'
#' You can update a card associated with either a repository, user or
#' organization, by supplying them as an input, as long as you have appropriate
#' permissions.
#'
#' You can move a card by either specifying the position, either `"top"` or
#' `"bottom"`, by specifying another card to place it after, or by specifying a
#' column to move it to.
#'
#' For more details see the GitHub API documentation:
#' - <https://docs.github.com/en/rest/reference/projects#update-a-project-card>
#' - <https://docs.github.com/en/rest/reference/projects#move-a-project-card>
#'
#' @param card (integer) The ID of the card.
#' @param note (string, optional) The new note for the card.
#' @param archived (boolean, optional) Whether to archive the card.
#' @param position (string, optional) Either `"top"` or `"bottom"`.
#' @param after (integer, optional) An ID of another card to place this one
#'   after.
#' @param column (integer or string, optional) Either the ID or name of the
#'   column.
#' @param project (integer or string) Either the project number or name. Only
#'   required if moving the column.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_card()` returns a list of the card properties.
#'
#' **Card Properties:**
#'
#' - **id**: The ID of the card.
#' - **content_id**: The ID of the issue or pull request.
#' - **note**: The content of a note.
#' - **archived**: Whether the card has been archived.
#' - **creator**: The creator of the note.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # Update a note in a card
#'   update_card(card = 123456, note = "This is an updated note")
#'
#'   # Archive a card
#'   update_card(card = 654321, archived = TRUE)
#'
#'   # Move a card to the top of a column
#'   move_card(card = 123456, position = "top")
#'
#'   # Move a card after another card
#'   move_card(card  = 123456, after = 654321)
#'
#'   # Move card to another column
#'   move_card(
#'     card     = 123456,
#'     position = "top",
#'     column   = "Test column 2",
#'     project  = "Test project",
#'     repo     = "ChadGoymer/githapi"
#'   )
#'
#' }
#'
#' @export
#'
update_card <- function(
  card,
  note,
  archived,
  ...
) {
  payload <- list()

  if (!missing(note)) {
    assert(
      is_scalar_character(note),
      "'note' must be a string:\n  ", note
    )
    payload$note <- note
  }

  if (!missing(archived)) {
    assert(
      is_scalar_logical(archived),
      "'archived' must be a boolean:\n  ", archived
    )
    payload$archived <- archived
  }

  info("Updating card '", card, "'")
  card_lst <- gh_url("projects/columns/cards", card) %>%
    gh_request(
      type    = "PATCH",
      payload = payload,
      accept  = "application/vnd.github.inertia-preview+json",
      ...
    )

  info("Transforming results", level = 4)
  card_gh <- select_properties(card_lst, properties$card) %>%
    modify_list(
      content_id   = as.integer(basename(.$content_url)),
      .after = "id"
    ) %>%
    discard(names(.) == "content_url")

  info("Done", level = 7)
  structure(
    card_gh,
    class   = class(card_lst),
    url     = attr(card_lst, "url"),
    request = attr(card_lst, "request"),
    status  = attr(card_lst, "status"),
    header  = attr(card_lst, "header")
  )
}


#  FUNCTION: move_card ---------------------------------------------------------
#
#' @rdname update_card
#' @export
#'
move_card <- function(
  card,
  position,
  after,
  column,
  project,
  repo,
  user,
  org,
  ...
) {
  payload <- list()

  if (!missing(position)) {
    assert(
      is_scalar_character(position) && position %in% values$card$position,
      "'position' must be one of '",
      str_c(values$card$position, collapse = "', '"), "':\n  ", position
    )

    payload <- list(position = position)
  }
  else if (!missing(after)) {
    assert(
      is_scalar_integerish(after),
      "'after' must be an integer:\n  ", after
    )

    payload <- list(position = str_c("after:", after))
  }
  else {
    error("Either 'position' or 'after' must be supplied")
  }

  if (!missing(column)) {
    column <- view_column(
      column  = column,
      project = project,
      repo    = repo,
      user    = user,
      org     = org,
      ...
    )

    payload <- c(payload, column_id = column$id)
  }

  info("Moving card '", card, "'")
  response <- gh_url("projects/columns/cards", card, "moves") %>%
    gh_request(
      type    = "POST",
      payload = payload,
      accept  = "application/vnd.github.inertia-preview+json",
      ...
    )

  card <- view_card(card, ...)

  info("Done", level = 7)
  structure(
    card,
    class   = class(card),
    url     = attr(response, "url"),
    request = attr(response, "request"),
    status  = attr(response, "status"),
    header  = attr(response, "header")
  )
}


#  FUNCTION: view_cards --------------------------------------------------------
#
#' View cards within a GitHub project
#'
#' `view_cards()` summarises cards in a table with the properties as columns and
#' a row for each card in a column of a project. `view_card()` returns a list of
#' all properties for a single card.
#'
#' You can summarise all the cards of a project associated with either a
#' repository, user or organization, by supplying them as an input.
#'
#' For more details see the GitHub API documentation:
#' - <https://docs.github.com/en/rest/reference/projects#list-project-cards>
#' - <https://docs.github.com/en/rest/reference/projects#get-a-project-card>
#'
#' @param card (integer) The ID of the card.
#' @param column (integer or string) Either the ID or name of the column.
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format:
#'   `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()] or [gh_request()].
#'
#' @return `view_cards()` returns a tibble of card properties. `view_card()`
#'   returns a list of properties for a single card.
#'
#' **Card Properties:**
#'
#' - **id**: The ID of the card.
#' - **content_id**: The ID of the issue or pull request.
#' - **note**: The content of a note.
#' - **archived**: Whether the card has been archived.
#' - **creator**: The creator of the note.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # View cards in a repository project
#'   cards <- view_cards(
#'     column  = "Test cards",
#'     project = "Test cards",
#'     repo    = "ChadGoymer/githapi"
#'   )
#'
#'   # View cards in a user's project
#'   cards <- view_cards(
#'     column  = "Test cards",
#'     project = "Test cards",
#'     user    = "ChadGoymer"
#'   )
#'
#'   # View cards in an organization's project
#'   cards <- view_cards(
#'     column  = "Test cards",
#'     project = "Test cards",
#'     org     = "HairyCoos"
#'   )
#'
#'   # View a single card
#'   view_card(card = 123456)
#'
#' }
#'
#' @export
#'
view_cards <- function(
  column,
  project,
  repo,
  user,
  org,
  n_max = 1000,
  ...
) {
  column <- view_column(
    column  = column,
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...
  )

  info("Viewing cards in column '", column$name, "'")
  cards_lst <- gh_url("projects/columns", column$id, "cards") %>%
    gh_page(
      accept = "application/vnd.github.inertia-preview+json",
      n_max  = n_max,
      ...
    )

  info("Transforming results", level = 4)
  cards_gh <- bind_properties(cards_lst, properties$card) %>%
    mutate(content_id = as.integer(basename(.data$content_url))) %>%
    select("id", "content_id", everything(), -"content_url")

  info("Done", level = 7)
  structure(
    cards_gh,
    class   = c("github", class(cards_gh)),
    url     = attr(cards_lst, "url"),
    request = attr(cards_lst, "request"),
    status  = attr(cards_lst, "status"),
    header  = attr(cards_lst, "header")
  )
}


#  FUNCTION: view_card ---------------------------------------------------------
#
#' @rdname view_cards
#' @export
#'
view_card <- function(
  card,
  ...
) {
  assert(
    is_scalar_integerish(card),
    "'card' must be an integer:\n  ", card
  )

  info("Viewing card '", card, "'")
  card_lst <- gh_url("projects/columns/cards", card) %>%
    gh_request(
      type   = "GET",
      accept = "application/vnd.github.inertia-preview+json",
      ...
    )

  info("Transforming results", level = 4)
  card_gh <- select_properties(card_lst, properties$card) %>%
    modify_list(
      content_id = as.integer(basename(.$content_url)),
      .after = "id"
    ) %>%
    discard(names(.) == "content_url")

  info("Done", level = 7)
  structure(
    card_gh,
    class   = class(card_lst),
    url     = attr(card_lst, "url"),
    request = attr(card_lst, "request"),
    status  = attr(card_lst, "status"),
    header  = attr(card_lst, "header")
  )
}


#  FUNCTION: delete_card -------------------------------------------------------
#
#' Delete a card in a GitHub project
#'
#' This function deletes a card in a GitHub project. Care should be taken as it
#' will not be recoverable. Instead, you may way to archive the card with
#' [update_card()].
#'
#' You can delete a card associated with either a repository, user or
#' organization, by supplying them as an input, as long as you have appropriate
#' permissions.
#'
#' For more details see the GitHub API documentation:
#' - <https://docs.github.com/en/rest/reference/projects#delete-a-project-card>
#'
#' @param card (integer) The ID of the card.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_card()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'
#'   # Delete a card
#'   delete_card(card = 123456)
#'
#' }
#'
#' @export
#'
delete_card <- function(
  card,
  ...
) {
  info("Deleting card '", card, "'")
  response <- gh_url("projects/columns/cards", card) %>%
    gh_request(
      type   = "DELETE",
      accept = "application/vnd.github.inertia-preview+json",
      ...
    )

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
