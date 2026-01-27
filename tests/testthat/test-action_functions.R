
# Test copy to right function ####
toy_dirs()

# Copy original paths to test functions on copies
syncdr_temp <- copy_temp_environment()
left  <- syncdr_temp$left
right <- syncdr_temp$right

# Get sync status object (from compare_directories)
sync_status <- compare_directories(left_path  = left,
                                   right_path = right)

test_that("copy files to right works", {
  # create an isolated environment and files explicitly so tests are deterministic
  env <- copy_temp_environment()
  left <- env$left
  right <- env$right

  # single file
  f1 <- fs::path(left, "one.txt")
  writeLines("one", f1)
  df1 <- data.table::data.table(path_left = f1)

  copy_files_to_right(left_dir = left, right_dir = right, files_to_copy = df1)
  expect_true(fs::file_exists(fs::path(right, fs::path_rel(f1, start = left))))

  # multiple files including nested
  fs::dir_create(fs::path(left, "sub"))
  f2 <- fs::path(left, "sub", "two.txt")
  writeLines("two", f2)
  df2 <- data.table::data.table(path_left = c(f1, f2))

  copy_files_to_right(left_dir = left, right_dir = right, files_to_copy = df2)
  rels <- fs::path_rel(df2$path_left, start = left)
  expect_true(all(fs::file_exists(fs::path(right, rels))))

})

# Test copy to left function ####

# restart
syncdr_temp <- copy_temp_environment()
left  <- syncdr_temp$left
right <- syncdr_temp$right

# compare directories
sync_status <- compare_directories(left,
                                   right)

test_that("copy files to left works", {
  # create explicit files in right and copy them to left
  env <- copy_temp_environment()
  left <- env$left
  right <- env$right

  r1 <- fs::path(right, "r_one.txt")
  writeLines("r1", r1)
  df1 <- data.table::data.table(path_right = r1)

  copy_files_to_left(left_dir = left, right_dir = right, files_to_copy = df1)
  expect_true(fs::file_exists(fs::path(left, fs::path_rel(r1, start = right))))

  # multiple files including nested
  fs::dir_create(fs::path(right, "sub"))
  r2 <- fs::path(right, "sub", "r_two.txt")
  writeLines("r2", r2)
  df2 <- data.table::data.table(path_right = c(r1, r2))

  copy_files_to_left(left_dir = left, right_dir = right, files_to_copy = df2)
  rels <- fs::path_rel(df2$path_right, start = right)
  expect_true(all(fs::file_exists(fs::path(left, rels))))

})

# Additional tests####
test_that("copy_files_to_right works when recurse = FALSE", {
  env <- copy_temp_environment()
  left <- env$left
  right <- env$right

  # create a nested file and copy without recursion -> should land in top-level right
  fs::dir_create(fs::path(left, "nested"))
  f <- fs::path(left, "nested", "flat.txt")
  writeLines("flat", f)

  df <- data.table::data.table(path_left = f)

  copy_files_to_right(
    left_dir = left,
    right_dir = right,
    files_to_copy = df,
    recurse = FALSE
  )

  expected <- fs::path(right, fs::path_file(f))
  expect_true(fs::file_exists(expected))
})

test_that("copy_files_to_right handles empty files_to_copy", {
  env <- copy_temp_environment()
  left <- env$left
  right <- env$right

  empty_df <- data.table::data.table(
    path_left = character()
  )

  expect_silent(
    copy_files_to_right(left, right, empty_df)
  )
})

test_that("copy_files_to_right creates needed subdirectories", {
  env <- copy_temp_environment()
  left  <- env$left
  right <- fs::path_temp() |> fs::path("nonexistent_dir")

  sync_status <- compare_directories(left, env$right)
  to_copy <- sync_status$non_common_files |>
    fsubset(!is.na(path_left)) |>
    fsubset(1)

  copy_files_to_right(left, right, to_copy)

  rel <- fs::path_rel(to_copy$path_left, start = left)
  expect_true(fs::dir_exists(fs::path_dir(fs::path(right, rel))))
})

test_that("copy_files_to_right overwrites existing files", {
  env <- copy_temp_environment()
  left <- env$left
  right <- env$right

  src     <- fs::path(left, "file.txt")
  dest    <- fs::path(right, "file.txt")

  writeLines("original", src)
  writeLines("old", dest)

  df <- data.table::data.table(path_left = src)
  copy_files_to_right(left, right, df)
  expect_equal(readLines(dest), "original")
})

test_that("copy_files_to_right errors when left_dir does not exist", {
  env <- copy_temp_environment()
  right <- env$right

  df <- data.table::data.table(path_left = "missing.txt")

  expect_error(
    copy_files_to_right("idontexist", right, df),
    regexp = ".*" # adjust based on actual error
  )
})

test_that("copy_files_to_right errors if path_from does not exist", {
  env <- copy_temp_environment()
  left <- env$left
  right <- env$right

  df <- data.table::data.table(path_left = fs::path(left, "no_such_file"))

  expect_error(
    copy_files_to_right(left, right, df)
  )
})

test_that("copy_files_to_right returns invisible(TRUE)", {
  env <- copy_temp_environment()
  left <- env$left
  right <- env$right
  sync_status <- compare_directories(left, right)

  to_copy <- sync_status$non_common_files[1, , drop = FALSE]

  # function returns invisible(TRUE) - capture with withVisible
  vis <- withVisible(copy_files_to_right(left, right, to_copy))
  expect_true(vis$visible == FALSE)
  expect_true(vis$value)
})

test_that("copy_files_to_right handles spaces and special chars", {
  env <- copy_temp_environment()
  left <- env$left
  right <- env$right

  special <- fs::path(left, "my file @#$%.txt")
  writeLines("data", special)

  df <- data.table::data.table(path_left = special)

  copy_files_to_right(left, right, df)

  expect_true(fs::file_exists(fs::path(right, "my file @#$%.txt")))
})


