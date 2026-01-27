# Test workhorse function ####

toy_dirs()

# Copy temp env
left  <- .syncdrenv$left
right <- .syncdrenv$right

# Test error when invalid paths
test_that("compare_directories throws an error with invalid dir
          paths", {
  # Define invalid directory paths for testing
  invalid_path <- "invalid_directory_path"

  # Run the function and expect an error
  expect_error(compare_directories(invalid_path, invalid_path))
})

# Test output class and content
test_that("compare directories retrun sync status", {

  compare_directories(left, right) |>
    expect_no_error()

  compare_directories(left, right, by_content = TRUE) |>
    expect_no_error()

  compare_directories(left, right, by_date = FALSE, by_content = TRUE) |>
    expect_no_error()

  res_by_date <- compare_directories(left, right)
  res_by_date_content <- compare_directories(left, right, by_content = TRUE)
  res_by_content <- compare_directories(left, right, by_date = FALSE, by_content = TRUE)

  class(res_by_date) |>
    expect_equal("syncdr_status")

  class(res_by_date_content) |>
    expect_equal(class(res_by_date))

  class(res_by_content) |>
    expect_equal(class(res_by_date))

  names(res_by_date) |>
    expect_equal(c("common_files",
                 "non_common_files",
                 "left_path",
                 "right_path"))

  names(res_by_date_content) |>
    expect_equal(names(res_by_date))

  names(res_by_content) |>
    expect_equal(names(res_by_date))

})

