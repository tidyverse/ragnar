test_that("vec_frame_flattened_tree works", {
  vec <- c(
    "h1" = "Level 1 Heading",
    "unnamed text",
    "h2" = "Level 2 Heading",
    "some name" = "some name text",
    "some name" = "moresome name text"
  )

  expected <- vctrs::data_frame(
    h1 = c("Level 1 Heading", "Level 1 Heading", "Level 1 Heading"),
    h2 = c(NA, "Level 2 Heading", "Level 2 Heading"),
    .name = c("", "some name", "some name"),
    .content = c("unnamed text", "some name text", "moresome name text"),
  )

  frame <- vec_frame_flattened_tree(vec, c("h1", "h2"))
  expect_equal(frame, expected)

  # check specifying names
  frame <- vec_frame_flattened_tree(
    vec,
    c("h1", "h2"),
    leaves = "text",
    names = "tag"
  )
  colnames(expected) <- c("h1", "h2", "tag", "text")

  expect_equal(frame, expected)
})

test_that("vec_frame_flattened_tree works 2", {
  vec <- c(
    "h1" = "Level 1 Heading",
    "abc",
    "h2" = "Level 2 Heading",
    "some name" = "def",
    "some name" = "ghi",
    "h2" = "second Level 2 Heading",
    "lmn"
  )

  expected <- vctrs::data_frame(
    h1 = c(
      "Level 1 Heading",
      "Level 1 Heading",
      "Level 1 Heading",
      "Level 1 Heading"
    ),
    h2 = c(NA, "Level 2 Heading", "Level 2 Heading", "second Level 2 Heading"),
    .name = c("", "some name", "some name", ""),
    .content = c("abc", "def", "ghi", "lmn")
  )

  frame <- vec_frame_flattened_tree(vec, c("h1", "h2"))
  expect_equal(frame, expected)

  frame <- vec_frame_flattened_tree(vec, c("h1", "h2", "h3"))
  expected$h3 <- NA_character_
  expected <- expected[c("h1", "h2", "h3", ".name", ".content")] # reorder
  expect_equal(frame, expected)
})


test_that("vec_frame_flattened_tree works 2", {
  vec <- c(
    "h1" = "Level 1 Heading",
    "abc",
    "h2" = "Level 2 Heading",
    "some name" = "def",
    "some name" = "ghi",
    "h2" = "second Level 2 Heading"
  )

  expected <- vctrs::data_frame(
    h1 = c(
      "Level 1 Heading",
      "Level 1 Heading",
      "Level 1 Heading",
      "Level 1 Heading"
    ),
    h2 = c(NA, "Level 2 Heading", "Level 2 Heading", "second Level 2 Heading"),
    .name = c("", "some name", "some name", NA),
    .content = c("abc", "def", "ghi", NA)
  )

  frame <- vec_frame_flattened_tree(vec, c("h1", "h2"))
  expect_equal(frame, expected)

  frame <- vec_frame_flattened_tree(vec, c("h1", "h2", "h3"))
  expected$h3 <- NA_character_
  expected <- expected[c("h1", "h2", "h3", ".name", ".content")] # reorder
  expect_equal(frame, expected)
})


test_that("vec_frame_flattened_tree works 3", {
  vec <- c(
    "h1" = "Level 1 Heading",
    "abc",
    "h2" = "Level 2 Heading",
    "some name" = "def",
    "some name" = "ghi",
    "h3" = "level 3 heading",
    "h2" = "second Level 2 Heading",
    "lmn"
  )
  expected <- vctrs::data_frame(
    h1 = rep("Level 1 Heading", 5),
    h2 = c(NA, rep("Level 2 Heading", 3), "second Level 2 Heading"),
    h3 = c(NA, NA, NA, "level 3 heading", NA),
    .name = c("", "some name", "some name", NA, ""),
    .content = c("abc", "def", "ghi", NA, "lmn")
  )

  frame <- vec_frame_flattened_tree(vec, c("h1", "h2", "h3"))
  expect_equal(frame, expected)
  expect_true(is.na(last(frame$h3)))
})


test_that("vec_frame_flattened_tree works 4", {
  frame <- vec_frame_flattened_tree(
    c(h1 = "# Quarto – executable-code-figure-size"),
    nodes = c("h1", "h2", "h3")
  )

  expected <- vctrs::data_frame(
    h1 = "# Quarto – executable-code-figure-size",
    h2 = NA_character_,
    h3 = NA_character_,
    .name = NA_character_,
    .content = NA_character_
  )
  expect_equal(frame, expected)
})
