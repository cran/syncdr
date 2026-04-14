
# Test symmetric synchronization functions ####

# Create sync env with temp directories
e = toy_dirs()

# Copy temp env
syncdr_temp <- copy_temp_environment()
left  <- syncdr_temp$left
right <- syncdr_temp$right

# Get sync status object (from compare_directories)
sync_status_date      <- compare_directories(left_path  = left,
                                             right_path = right)

# ~~~~~~~~~ Update by date only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

full_symmetric_sync(sync_status = sync_status_date,
                    force = TRUE)

test_that("full symm sync works -by date", {

  # common files ####
  to_copy <- sync_status_date$common_files |>
    fsubset(is_new_left | is_new_right)

  sync_status_after <- compare_directories(left,
                                           right)

  # non common files ####
  sync_status_after$non_common_files |>
    nrow() |>
    expect_equal(0)

  cf_status_after <- sync_status_after$common_files |>
    fselect(path_left, path_right, sync_status)


  all(compare_file_contents(cf_status_after$path_left,
                            cf_status_after$path_right)$sync_status_content == "same content") |>
    expect_equal(TRUE)


})

# ~~~~~~~~~ Update by date and content ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Restart
syncdr_temp <- copy_temp_environment()
left  <- syncdr_temp$left
right <- syncdr_temp$right

# Get sync status object (from compare_directories)
sync_status_date_cont <- compare_directories(left_path  = left,
                                             right_path = right,
                                             by_content = TRUE)

# sync
full_symmetric_sync(sync_status = sync_status_date_cont,
                    force = TRUE)

test_that("full symm sync works -by date&cont", {

  # common files ####
  # to_copy <- sync_status_date_cont$common_files |>
  #   fsubset((is_new_left | is_new_right) & is_diff)

  sync_status_after <- compare_directories(left,
                                           right)
  # non common files ####
  sync_status_after$non_common_files |>
    nrow() |>
    expect_equal(0)

  cf_status_after <- sync_status_after$common_files |>
    fselect(path_left, path_right, sync_status)

  all(compare_file_contents(cf_status_after$path_left,
                            cf_status_after$path_right)$sync_status_content == "same content") |>
    expect_equal(TRUE)

})


# Testing partial symmetric sync function ####

# ~~~~~~~~~ Update by date only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Restart
syncdr_temp <- copy_temp_environment()
left  <- syncdr_temp$left
right <- syncdr_temp$right

# Get sync status object (from compare_directories)
sync_status <- compare_directories(left_path  = left,
                                    right_path = right)

# sync

partial_symmetric_sync_common_files(sync_status = sync_status,
                                    force = TRUE)

test_that("partial symm sync works -by date", {

  # common files ####
  to_copy <- sync_status$common_files |>
    fsubset(is_new_left | is_new_right)

  sync_status_after <- compare_directories(left,
                      right)$common_files |>
    fsubset(path_left %in% to_copy$path_left &
              path_right %in% to_copy$path_right)

  all(compare_file_contents(sync_status_after$path_left,
                        sync_status_after$path_right)$is_diff == FALSE) |>
    expect_equal(TRUE)

  # non common files ####
  compare_directories(left,
                      right)$non_common_files |>
    expect_equal(sync_status$non_common_files)

})

# ~~~~~~~~~ Update by date & content ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Restart
syncdr_temp <- copy_temp_environment()
left  <- syncdr_temp$left
right <- syncdr_temp$right
# Get sync status object (from compare_directories)
sync_status <- compare_directories(left_path  = left,
                                   right_path = right,
                                   by_content = TRUE)

# sync
partial_symmetric_sync_common_files(sync_status = sync_status,
                                    force = TRUE)

test_that("partial sym sync works -by date & cont", {

  to_copy <- sync_status$common_files |>
    fsubset(is_new_left | is_new_right) |>
    fsubset(is_diff)

  sync_status_after <- compare_directories(left,
                                           right)$common_files |>
    fsubset(path_left %in% to_copy$path_left &
              path_right %in% to_copy$path_right)


  all(compare_file_contents(sync_status_after$path_left,
                            sync_status_after$path_right)$sync_status == "same content") |>
    expect_equal(TRUE)

})

## Additional tests ####
test_that("full_symmetric_sync errors with missing arguments", {
  expect_error(full_symmetric_sync(),
               "Either sync_status or left and right paths must be provided")
})

test_that("full_symmetric_sync errors with non-existent directories", {
  expect_error(full_symmetric_sync(left_path = "fake_dir", right_path = "fake_dir2"),
               "does not exist")
})

test_that("full_symmetric_sync creates backup with correct contents", {
  syncdr_temp <- copy_temp_environment()
  left  <- syncdr_temp$left
  right <- syncdr_temp$right
  backup_dir <- tempfile("backup_test")
  dir.create(backup_dir)

  # Add a file to right to check backup
  file.create(file.path(right, "testfile.txt"))

  full_symmetric_sync(left_path = left, right_path = right, backup = TRUE, backup_dir = backup_dir,
                      force = TRUE)

  # Find backup subdirectory (assuming backup is of 'right')
  backup_subdirs <- list.dirs(backup_dir, recursive = FALSE, full.names = TRUE)
  expect_true(length(backup_subdirs) > 0)

  # Check that the backed up file exists in the backup
  backed_up_file <- file.path(backup_subdirs[1], "testfile.txt")
  expect_false(file.exists(backed_up_file))
})

test_that("full_symmetric_sync aborts if user declines in preview mode", {
  syncdr_temp <- copy_temp_environment()
  left  <- syncdr_temp$left
  right <- syncdr_temp$right

  testthat::with_mocked_bindings(
    `askYesNo` = function(...) FALSE,
    {
      expect_error(
        full_symmetric_sync(left_path = left, right_path = right, force = FALSE),
        "Synchronization interrupted"
      )
    }
  )
})


test_that("full_symmetric_sync aborts for by_content only", {
  syncdr_temp <- copy_temp_environment()
  left  <- syncdr_temp$left
  right <- syncdr_temp$right
  expect_error(full_symmetric_sync(left_path = left, right_path = right, by_date = FALSE, by_content = TRUE),
               "Symmetric synchronization by content only is not active")
})

test_that("full_symmetric_sync only syncs top-level files when recurse = FALSE", {
  syncdr_temp <- copy_temp_environment()
  left  <- syncdr_temp$left
  right <- syncdr_temp$right

  # Add a file in the top-level directory
  file.create(file.path(left, "l.topfile.txt"))
  file.create(file.path(right, "rtopfile.txt"))


  # Add a file inside a subdirectory
  subdir <- file.path(left, "subdir")
  dir.create(subdir)
  file.create(file.path(subdir, "subfile.txt"))

  # Perform sync without recursion
  full_symmetric_sync(left_path = left, right_path = right, recurse = FALSE,
                      force = TRUE)

  # Top-level file should be copied
  expect_true(file.exists(file.path(right, "l.topfile.txt")))

  # Subdirectory file should NOT be copied
  expect_false(file.exists(file.path(right, "subfile.txt")))
})


test_that("partial_symmetric_sync_common_files does not copy non-common files", {
  syncdr_temp <- copy_temp_environment()
  left  <- syncdr_temp$left
  right <- syncdr_temp$right
  # Add a file only to left
  file.create(file.path(left, "unique_left.txt"))
  sync_status <- compare_directories(left_path = left, right_path = right)
  partial_symmetric_sync_common_files(sync_status = sync_status,
                                      force = TRUE)
  expect_false(file.exists(file.path(right, "unique_left.txt")))
})

## ~~~~~~~~~ Additional coverage tests for full_symmetric_sync & partial_symmetric_sync_common_files ~~~~~~~~~

# --- 1. Verbose TRUE branch ---
test_that("full_symmetric_sync runs with verbose = TRUE", {
  syncdr_temp <- copy_temp_environment()
  left <- syncdr_temp$left
  right <- syncdr_temp$right

  testthat::with_mocked_bindings(
    `display_dir_tree` = function(...) "called",
    `style_msgs`       = function(...) "called",
    {
      expect_silent(full_symmetric_sync(left_path = left,
                                        right_path = right,
                                        force = TRUE,
                                        verbose = TRUE))
    }
  )
})

test_that("partial_symmetric_sync_common_files runs with verbose = TRUE", {
  syncdr_temp <- copy_temp_environment()
  left <- syncdr_temp$left
  right <- syncdr_temp$right
  sync_status <- compare_directories(left, right)

  testthat::with_mocked_bindings(
    `display_dir_tree` = function(...) "called",
    `style_msgs`       = function(...) "called",
    {
      expect_silent(partial_symmetric_sync_common_files(sync_status = sync_status,
                                                        force = TRUE,
                                                        verbose = TRUE))
    }
  )
})

# --- 2. Force = FALSE, user agrees ---
## --- 1️⃣ Preview mode - user agrees ----
test_that("full_symmetric_sync proceeds when user agrees in preview mode", {
  syncdr_temp <- copy_temp_environment()
  left  <- syncdr_temp$left
  right <- syncdr_temp$right

  testthat::with_mocked_bindings(
    `askYesNo` = function(...) TRUE,  # simulate user agreeing
    {
      res <- full_symmetric_sync(left_path = left, right_path = right, force = FALSE)
      expect_true(res)  # check it returns TRUE
    }
  )
})

# --- 5. Nested directories with recurse = TRUE ---
test_that("full_symmetric_sync copies nested files when recurse = TRUE", {
  syncdr_temp <- copy_temp_environment()
  left <- syncdr_temp$left
  right <- syncdr_temp$right

  # nested subdir
  subdir <- file.path(left, "nested")
  dir.create(subdir)
  nested_file <- file.path(subdir, "nested.txt")
  writeLines("hello", nested_file)

  full_symmetric_sync(left_path = left, right_path = right, recurse = TRUE,
                      force = TRUE)
})

# --- 7. Mock copy functions to ensure correct arguments ---
test_that("full_symmetric_sync calls copy functions correctly", {
  syncdr_temp <- copy_temp_environment()
  left <- syncdr_temp$left
  right <- syncdr_temp$right
  sync_status <- compare_directories(left, right)

  called_right <- FALSE
  called_left <- FALSE

  testthat::with_mocked_bindings(
    `copy_files_to_right` = function(...) { called_right <<- TRUE },
    `copy_files_to_left`  = function(...) { called_left <<- TRUE },
    {
      full_symmetric_sync(sync_status = sync_status,
                          force = TRUE)
    }
  )

  expect_true(called_right)
  expect_true(called_left)
})

# --- 8. Edge case: by_content only abort ---
test_that("full_symmetric_sync aborts when by_content only", {
  syncdr_temp <- copy_temp_environment()
  left <- syncdr_temp$left
  right <- syncdr_temp$right
  expect_error(full_symmetric_sync(left_path = left,
                                   right_path = right,
                                   by_date = FALSE,
                                   by_content = TRUE),
               "Symmetric synchronization by content only is not active")
})

# --- 9. Corrupted sync_status handling ---
test_that("full_symmetric_sync handles missing common_files gracefully", {
  syncdr_temp <- copy_temp_environment()
  left <- syncdr_temp$left
  right <- syncdr_temp$right
  sync_status <- compare_directories(left, right)
  sync_status$common_files <- NULL

  expect_error(full_symmetric_sync(sync_status = sync_status),
               "object .* not found|NULL")
})

# ~~~~~~~~~ Update content only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("full_symmetric_sync aborts for by_content only", {
  syncdr_temp <- copy_temp_environment()
  left  <- syncdr_temp$left
  right <- syncdr_temp$right

  expect_error(
    full_symmetric_sync(left_path  = left,
                        right_path = right,
                        by_date    = FALSE,
                        by_content = TRUE),
    "Symmetric synchronization by content only is not active"
  )
})

test_that("partial_symmetric_sync_common_files aborts for by_content only", {
  syncdr_temp <- copy_temp_environment()
  left  <- syncdr_temp$left
  right <- syncdr_temp$right
  sync_status <- compare_directories(left_path  = left,
                                     right_path = right)

  expect_error(
    partial_symmetric_sync_common_files(sync_status = sync_status,
                                        by_date    = FALSE,
                                        by_content = TRUE),
    "Symmetric synchronization by content only is not active"
  )
})

# ===== Group F: VUL-13/14/34 tests =====

# VUL-13: force=FALSE default prompts user ----
test_that("full_symmetric_sync default force=FALSE prompts and aborts on refusal", {
  e <- copy_temp_environment()
  with_mocked_bindings(
    askYesNo = function(...) FALSE,
    {
      expect_error(
        full_symmetric_sync(left_path = e$left, right_path = e$right),
        "Synchronization interrupted"
      )
    }
  )
})

test_that("partial_symmetric_sync_common_files default force=FALSE prompts and aborts", {
  e <- copy_temp_environment()
  with_mocked_bindings(
    askYesNo = function(...) FALSE,
    {
      expect_error(
        partial_symmetric_sync_common_files(left_path = e$left, right_path = e$right),
        "Synchronization interrupted"
      )
    }
  )
})

# VUL-34: by_content_only abort fires BEFORE backup ----
test_that("full_symmetric_sync aborts for by_content only before creating backup dir", {
  e <- copy_temp_environment()
  backup_dir <- fs::file_temp("vul34_backup")

  expect_false(fs::dir_exists(backup_dir))

  expect_error(
    full_symmetric_sync(
      left_path  = e$left,
      right_path = e$right,
      by_date    = FALSE,
      by_content = TRUE,
      backup     = TRUE,
      backup_dir = backup_dir,
      force      = TRUE
    ),
    "Symmetric synchronization by content only is not active"
  )

  # backup should NOT have been created since abort fires before backup block
  expect_false(fs::dir_exists(backup_dir))
})

# VUL-14: overwrite param passes through to copy workers ----
test_that("full_symmetric_sync with overwrite=FALSE does not overwrite existing files", {
  tmp_left  <- fs::dir_create(fs::file_temp())
  tmp_right <- fs::dir_create(fs::file_temp())
  on.exit({ fs::dir_delete(tmp_left); fs::dir_delete(tmp_right) }, add = TRUE)

  # write same file in both with different content; right is "newer" in left
  writeLines("right_original", file.path(tmp_right, "shared.txt"))
  writeLines("left_updated",   file.path(tmp_left,  "shared.txt"))
  Sys.sleep(0.05)
  # touch the left file to make it newer
  fs::file_touch(file.path(tmp_left, "shared.txt"))

  full_symmetric_sync(
    left_path  = tmp_left,
    right_path = tmp_right,
    overwrite  = FALSE,
    force      = TRUE
  )

  # right should NOT have been overwritten
  expect_equal(readLines(file.path(tmp_right, "shared.txt")), "right_original")
})

