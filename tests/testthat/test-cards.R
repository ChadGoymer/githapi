context("cards")


# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

setup(suppressMessages({

  create_repository(
    name        = str_c("test-cards-", suffix),
    description = "This is a repository to test cards",
    auto_init   = TRUE
  )

  Sys.sleep(1)

  create_project(
    name = str_c("Test cards ", suffix),
    body = "A project to test card functions",
    repo = str_c("ChadGoymer/test-cards-", suffix)
  )

  create_column(
    name    = str_c("Test cards ", suffix),
    project = str_c("Test cards ", suffix),
    repo    = str_c("ChadGoymer/test-cards-", suffix)
  )

  create_issue(
    title = "This is an issue to test cards",
    repo  = str_c("ChadGoymer/test-cards-", suffix),
    body  = "This is an issue to test cards"
  )

  create_file(
    content = "This is a commit to test cards",
    path    = str_c("test-cards-", suffix, ".txt"),
    branch  = str_c("test-cards-", suffix),
    message = "Commit to test cards",
    repo    = str_c("ChadGoymer/test-cards-", suffix),
    parent  = "master"
  )

  create_pull_request(
    title = "This is a pull request to test cards",
    repo  = str_c("ChadGoymer/test-cards-", suffix),
    head  = str_c("test-cards-", suffix),
    base  = "master",
    body  = "This is a pull request to test cards"
  )

}))

teardown(suppressMessages({

  delete_repository(str_c("ChadGoymer/test-cards-", suffix))

}))


# TEST: create_card ------------------------------------------------------------

test_that("create_cards creates a card and returns its properties", {

  issue_card <- create_card(
    content_id   = 1,
    content_type = "Issue",
    column       = str_c("Test cards ", suffix),
    project      = str_c("Test cards ", suffix),
    repo         = str_c("ChadGoymer/test-cards-", suffix)
  )

  expect_is(issue_card, "list")
  expect_identical(attr(issue_card, "status"), 201L)
  expect_identical(
    map_chr(issue_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(issue_card$content_id, 1L)

  pull_card <- create_card(
    content_id   = 2,
    content_type = "PullRequest",
    column       = str_c("Test cards ", suffix),
    project      = str_c("Test cards ", suffix),
    repo         = str_c("ChadGoymer/test-cards-", suffix)
  )

  expect_is(pull_card, "list")
  expect_identical(attr(pull_card, "status"), 201L)
  expect_identical(
    map_chr(pull_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(pull_card$content_id, 2L)

  note_card <- create_card(
    note    = "Note Title\nThis is a note",
    column  = str_c("Test cards ", suffix),
    project = str_c("Test cards ", suffix),
    repo    = str_c("ChadGoymer/test-cards-", suffix)
  )

  expect_is(note_card, "list")
  expect_identical(attr(note_card, "status"), 201L)
  expect_identical(
    map_chr(note_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(note_card$note, "Note Title\nThis is a note")

})

test_that("create_card throws an error in invalid arguments are supplied", {

  expect_error(
    create_card(
      column  = str_c("Test cards ", suffix),
      project = str_c("Test cards ", suffix),
      repo    = str_c("ChadGoymer/test-cards-", suffix)
    ),
    "Either 'content_id' or 'note' must be supplied"
  )

})


# TEST: update_card ------------------------------------------------------------

suppressMessages({
  cards <- view_cards(
    column  = str_c("Test cards ", suffix),
    project = str_c("Test cards ", suffix),
    repo    = str_c("ChadGoymer/test-cards-", suffix)
  )
})

issue_card_id <- filter(cards, .data$content_id == 1) %>% pull(id)
pull_card_id  <- filter(cards, .data$content_id == 2) %>% pull(id)
note_card_id  <- filter(cards, is.na(.data$content_id)) %>% pull(id)

test_that("update_card updates a card and returns a list of the properties", {

  archived_card <- update_card(card = issue_card_id, archived = TRUE)

  expect_is(archived_card, "list")
  expect_identical(attr(archived_card, "status"), 200L)
  expect_identical(
    map_chr(archived_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_true(archived_card$archived)

  unarchived_card <- update_card(card = issue_card_id, archived = FALSE)

  expect_is(unarchived_card, "list")
  expect_identical(attr(unarchived_card, "status"), 200L)
  expect_identical(
    map_chr(unarchived_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_false(unarchived_card$archived)

  note_card <- update_card(
    card = note_card_id,
    note = "Note Title\nThis is an updated note"
  )

  expect_is(note_card, "list")
  expect_identical(attr(note_card, "status"), 200L)
  expect_identical(
    map_chr(note_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(note_card$note, "Note Title\nThis is an updated note")

})


# TEST: move_card --------------------------------------------------------------

test_that("move_card changes the position of a card", {

  first_card <- move_card(card = issue_card_id, position = "top")

  expect_is(first_card, "list")
  expect_identical(attr(first_card, "status"), 201L)
  expect_identical(
    map_chr(first_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  last_card <- move_card(card = note_card_id, position = "bottom")

  expect_is(last_card, "list")
  expect_identical(attr(last_card, "status"), 201L)
  expect_identical(
    map_chr(last_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  after_card <- move_card(card  = pull_card_id, after = note_card_id)

  expect_is(after_card, "list")
  expect_identical(attr(after_card, "status"), 201L)
  expect_identical(
    map_chr(after_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

})

test_that("move_card changes the column a card is in", {

  column2 <- create_column(
    name    = str_c("Test cards 2 ", suffix),
    project = str_c("Test cards ", suffix),
    repo    = str_c("ChadGoymer/test-cards-", suffix)
  )

  column_card <- move_card(
    card     = issue_card_id,
    position = "top",
    column   = str_c("Test cards 2 ", suffix),
    project  = str_c("Test cards ", suffix),
    repo     = str_c("ChadGoymer/test-cards-", suffix)
  )

  expect_is(column_card, "list")
  expect_identical(attr(column_card, "status"), 201L)
  expect_identical(
    map_chr(column_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

})

test_that("move_card throws an error in invalid arguments are supplied", {

  expect_error(
    move_card(
      card = issue_card_id,
      repo = str_c("ChadGoymer/test-cards-", suffix)
    ),
    "Either 'position' or 'after' must be supplied"
  )

})


# TEST: view_cards -------------------------------------------------------------

test_that("view_cards returns a tibble summarising the cards", {

  cards <- view_cards(
    column  = str_c("Test cards ", suffix),
    project = str_c("Test cards ", suffix),
    repo    = str_c("ChadGoymer/test-cards-", suffix),
    n_max   = 10
  )

  expect_is(cards, "tbl")
  expect_identical(attr(cards, "status"), 200L)
  expect_identical(
    map_chr(cards, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_true(2L %in% cards$content_id)
  expect_true("Note Title\nThis is an updated note" %in% cards$note)

})


# TEST: view_card --------------------------------------------------------------

test_that("view_card returns a list of card properties", {

  issue_card <- view_card(card = cards$id[[3]])

  expect_is(issue_card, "list")
  expect_identical(attr(issue_card, "status"), 200L)
  expect_identical(
    map_chr(issue_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(issue_card$content_id, 1L)

  pull_card <- view_card(card = cards$id[[2]])

  expect_is(pull_card, "list")
  expect_identical(attr(pull_card, "status"), 200L)
  expect_identical(
    map_chr(pull_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(pull_card$content_id, 2L)

  note_card <- view_card(card = cards$id[[1]])

  expect_is(note_card, "list")
  expect_identical(attr(note_card, "status"), 200L)
  expect_identical(
    map_chr(note_card, ~ class(.)[[1]]),
    c(
      id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"
    )
  )

  expect_identical(note_card$note, "Note Title\nThis is an updated note")

})


# TEST: delete_card ------------------------------------------------------------

test_that("delete_card deletes the cards and returns TRUE", {

  issue_card <- delete_card(issue_card_id)

  expect_is(issue_card, "logical")
  expect_identical(attr(issue_card, "status"), 204L)
  expect_identical(as.logical(issue_card), TRUE)

  pull_card <- delete_card(pull_card_id)

  expect_is(pull_card, "logical")
  expect_identical(attr(pull_card, "status"), 204L)
  expect_identical(as.logical(pull_card), TRUE)

  note_card <- delete_card(note_card_id)

  expect_is(note_card, "logical")
  expect_identical(attr(note_card, "status"), 204L)
  expect_identical(as.logical(note_card), TRUE)

})
