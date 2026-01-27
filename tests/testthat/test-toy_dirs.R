# Create .syncdrenv with toy dirs and files
toy_dirs()

# Test toy_dirs() function ####
test_that("toy_dirs creates .syncdrenv", {

  # Check if the environment exists
  expect_true(exists(".syncdrenv"))

  # Check paths exist
  expect_true(exists("left",
                     envir = .syncdrenv))
  expect_true(exists("right",
                     envir = .syncdrenv))

  left <- .syncdrenv$left
  right <- .syncdrenv$right

  # Check if the directories exist
  expect_true(fs::dir_exists(left))
  expect_true(fs::dir_exists(right))

  # Check dirs are not empty
  expect_true(length(fs::dir_ls(left,
                            recurse = TRUE)) > 0)
  expect_true(length(fs::dir_ls(right,
                            recurse = TRUE)) > 0)
})

# Test copy_temp_environment function ####
test_that("copy original env works", {

  # Copy the original environment
  temp_env <- copy_temp_environment()

  # Getpaths to the copied directories
  temp_left  <- temp_env$left
  temp_right <- temp_env$right

  # Check that the directories exist
  expect_true(fs::dir_exists(temp_left))
  expect_true(fs::dir_exists(temp_right))

  # Check that the directories are not empty
  expect_true(length(
    fs::dir_ls(temp_left, recurse = TRUE)) > 0
    )

  expect_true(length(
    fs::dir_ls(temp_right, recurse = TRUE)) > 0
    )

  # Compare file structures and contents
  original_env  <- get(".syncdrenv")
  original_left <- original_env$left
  original_right <- original_env$right

  original_files_left <- fs::dir_ls(original_left,
                                recurse = TRUE)
  copied_files_left  <- fs::dir_ls(temp_left,
                               recurse = TRUE)

  expect_equal(basename(original_files_left),
               basename(copied_files_left))

  original_files_right <- fs::dir_ls(original_right,
                                 recurse = TRUE)
  copied_files_right <- fs::dir_ls(temp_right,
                               recurse = TRUE)
  expect_equal(basename(original_files_right),
               basename(copied_files_right))
})
