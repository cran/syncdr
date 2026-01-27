# Test print method for syncdr_status ####
toy_dirs()

# Copy temp env
left  <- .syncdrenv$left
right <- .syncdrenv$right

test_that("print.syncdr_status works without error for all comparison modes", {

  res_by_date <- compare_directories(left, right)

  res_by_date_content <- compare_directories(
    left,
    right,
    by_content = TRUE
  )

  res_by_content <- compare_directories(
    left,
    right,
    by_date = FALSE,
    by_content = TRUE
  )

  expect_invisible(print(res_by_date))
  expect_invisible(print(res_by_date_content))
  expect_invisible(print(res_by_content))
})

test_that("print formats common files correctly when comparing by date", {

  res <- compare_directories(left, right)

  printed <- print(res)

  expect_equal(class(printed), "syncdr_status")

  expect_true(
    all(c(
      "path",
      "modification_time_left",
      "modification_time_right",
      "modified"
    ) %in% names(printed$common_files))
  )
})

test_that("print formats common files correctly when comparing by date and content", {

  res <- compare_directories(left, right, by_content = TRUE)

  printed <- print(res)

  expect_true(
    all(c(
      "path",
      "modification_time_left",
      "modification_time_right",
      "modified",
      "sync_status"
    ) %in% names(printed$common_files))
  )
})

test_that("print formats common files correctly when comparing by content only", {

  res <- compare_directories(
    left,
    right,
    by_date = FALSE,
    by_content = TRUE
  )

  printed <- print(res)

  expect_equal(
    names(printed$common_files),
    c("path", "sync_status")
  )
})

test_that("remove_root removes directory root correctly", {

  expect_equal(
    remove_root("/tmp/left", "/tmp/left/a/b.txt"),
    "/left/a/b.txt"
  )

  expect_equal(
    remove_root("/tmp/right", "/tmp/right/file.txt"),
    "/right/file.txt"
  )
})
