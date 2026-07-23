test_that("upsetly creates a plotly widget and excludes the empty intersection", {
  input <- data.frame(
    id = c("one", "two", "three", "none"),
    A = c(1, 1, 0, 0),
    B = c(0, 1, 1, 0),
    stringsAsFactors = FALSE
  )

  widget <- upsetly(input, set_cols = c("A", "B"), id_col = "id")
  expect_s3_class(widget, "plotly")

  built <- plotly::plotly_build(widget)
  intersection_sizes <- as.numeric(built$x$data[[1]]$y)
  expect_equal(sort(intersection_sizes), c(1, 1, 1))
})

test_that("membership coercion handles logical, numeric, and false strings", {
  input <- data.frame(
    id = letters[1:4],
    logical_set = c(TRUE, FALSE, NA, TRUE),
    numeric_set = c(0, 2, NA, -1),
    character_set = c("false", "yes", "0", "member"),
    stringsAsFactors = FALSE
  )

  widget <- upsetly(input, id_col = "id")
  expect_s3_class(widget, "plotly")
})

test_that("display limits and validation are enforced", {
  input <- data.frame(
    id = letters[1:4],
    A = c(1, 1, 0, 0),
    B = c(0, 1, 1, 0),
    stringsAsFactors = FALSE
  )

  limited <- upsetly(
    input,
    set_cols = c("A", "B"),
    id_col = "id",
    max_n_intersections = 2
  )
  built <- plotly::plotly_build(limited)
  expect_length(built$x$data[[1]]$y, 2)

  expect_error(upsetly(as.matrix(input)), "data frame", fixed = TRUE)
  expect_error(upsetly(input, set_cols = character()), "non-empty")
  expect_error(upsetly(input, set_cols = c("A", "A")), "duplicate")
  expect_error(upsetly(input, set_cols = "missing"), "not in `x`")
  expect_error(
    upsetly(input, set_cols = "A", id_col = "A"),
    "must not also"
  )
  expect_error(upsetly(input, min_intersection_size = 0), "positive integer")
  expect_error(upsetly(input, members_per_line = 1.5), "positive integer")
})

test_that("box ids are safely JSON encoded in the render hook", {
  input <- data.frame(id = c("a", "b"), A = c(1, 1))
  box_id <- 'details";window.unwanted=true;//'
  widget <- upsetly(input, set_cols = "A", id_col = "id", box_id = box_id)

  hook_code <- widget$jsHooks$render[[1]]$code
  encoded <- as.character(jsonlite::toJSON(box_id, auto_unbox = TRUE))
  expect_match(hook_code, encoded, fixed = TRUE)
  expect_match(hook_code, "removeListener", fixed = TRUE)
  expect_match(hook_code, "replace(/<br", fixed = TRUE)
})

test_that("member identifiers and set names are escaped in HTML tooltips", {
  input <- data.frame(
    id = c("<script>alert(1)</script>", "safe"),
    `A&B` = c(1, 1),
    check.names = FALSE
  )
  widget <- upsetly(input, set_cols = "A&B", id_col = "id")
  built <- plotly::plotly_build(widget)
  hover <- paste(built$x$data[[1]]$hovertext, collapse = " ")

  expect_match(hover, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", hover, fixed = TRUE))
  expect_match(hover, "A&amp;B", fixed = TRUE)
})
