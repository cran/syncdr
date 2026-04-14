library(testthat)
test_that("display_sync_status returns a DT datatable", {
  skip_on_cran()

  tmp_left  <- withr::local_tempdir()
  tmp_right <- withr::local_tempdir()

  # Create dummy files
  left_file  <- file.path(tmp_left, "a.txt")
  right_file <- file.path(tmp_right, "a.txt")
  writeLines("x", left_file)
  writeLines("x", right_file)

  # Simple sync_status data frame
  df <- data.frame(
    path_left  = left_file,
    path_right = right_file,
    is_new_left = FALSE,
    is_new_right = FALSE,
    sync_status = "same",
    stringsAsFactors = FALSE
  )

  dt <- display_sync_status(df, left_path = tmp_left, right_path = tmp_right)

  # It should be a datatable
  expect_s3_class(dt, "datatables")

  # Column names should exist
  expect_true(all(c("path_left", "path_right", "is_new_left", "is_new_right", "sync_status") %in% colnames(df)))
})

test_that("display_dir_tree prints without error", {
  skip_on_cran()

  tmp_left  <- withr::local_tempdir()
  tmp_right <- withr::local_tempdir()

  # Create simple structure
  dir.create(file.path(tmp_left, "sub"))
  file.create(file.path(tmp_left, "sub", "file.txt"))
  file.create(file.path(tmp_right, "right.txt"))

  # capture output to check invisibility
  expect_invisible({
    output <- capture.output(display_dir_tree(path_left = tmp_left, path_right = tmp_right))
  })

  # Should contain some lines
  expect_gt(length(output), 0)
})

test_that("display_file_actions prints correct table", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  file_path <- file.path(tmp, "foo.txt")
  writeLines("abc", file_path)

  df <- data.frame(Paths = file_path, stringsAsFactors = FALSE)

  # Test copy action
  output_copy <- capture.output(syncdr:::display_file_actions(df, directory = tmp, action = "copy"))
  expect_true(any(grepl("To be copied", output_copy)))

  # Test delete action
  output_delete <- capture.output(syncdr:::display_file_actions(df, directory = tmp, action = "delete"))
  expect_true(any(grepl("To be deleted", output_delete)))
})

# VUL-32: display_dir_tree passes recurse to fs::dir_tree ----
test_that("display_dir_tree passes recurse=FALSE to fs::dir_tree", {
  skip_on_cran()

  tmp_left  <- withr::local_tempdir()
  tmp_right <- withr::local_tempdir()

  subdir <- file.path(tmp_left, "sub")
  dir.create(subdir)
  writeLines("nested", file.path(subdir, "nested.txt"))
  writeLines("toplevel", file.path(tmp_left, "top.txt"))

  called_args <- list()
  with_mocked_bindings(
    dir_tree = function(path, recurse = TRUE, ...) {
      called_args[[length(called_args) + 1L]] <<- list(path = path, recurse = recurse)
      invisible(NULL)
    },
    .package = "fs",
    {
      display_dir_tree(path_left = tmp_left, path_right = tmp_right, recurse = FALSE)
    }
  )

  # both calls should have received recurse = FALSE
  recurse_vals <- vapply(called_args, function(x) x$recurse, logical(1))
  expect_true(all(!recurse_vals))
})
