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
                 "right_path",
                 "created_at"))

  names(res_by_date_content) |>
    expect_equal(names(res_by_date))

  names(res_by_content) |>
    expect_equal(names(res_by_date))

})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tests for Fix Group B — input validation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# --- VUL-11: validate_path_arg() rejects bad inputs -------------------------

test_that("compare_directories rejects NA left_path (VUL-11)", {
  right <- fs::path_temp("vul11_right")
  fs::dir_create(right)
  on.exit(fs::dir_delete(right), add = TRUE)

  expect_error(
    compare_directories(NA_character_, right),
    regexp = "left_path"
  )
})

test_that("compare_directories rejects empty string left_path (VUL-11)", {
  right <- fs::path_temp("vul11b_right")
  fs::dir_create(right)
  on.exit(fs::dir_delete(right), add = TRUE)

  expect_error(
    compare_directories("", right),
    regexp = "left_path"
  )
})

test_that("compare_directories rejects non-existent right_path (VUL-11)", {
  left <- fs::path_temp("vul11c_left")
  fs::dir_create(left)
  on.exit(fs::dir_delete(left), add = TRUE)

  expect_error(
    compare_directories(left, "/this/does/not/exist/at/all"),
    regexp = "right_path"
  )
})

# --- VUL-12: save_sync_status() validates dir_path --------------------------

test_that("save_sync_status rejects non-existent dir_path (VUL-12)", {
  expect_error(
    save_sync_status("/this/does/not/exist/at/all"),
    regexp = "dir_path"
  )
})

test_that("save_sync_status rejects NA dir_path (VUL-12)", {
  expect_error(
    save_sync_status(NA_character_),
    regexp = "dir_path"
  )
})

# --- VUL-05: identical left/right path guard ---------------------------------

test_that("compare_directories rejects identical left and right paths (VUL-05)", {
  dir <- fs::path_temp("vul05_same")
  fs::dir_create(dir)
  on.exit(fs::dir_delete(dir), add = TRUE)

  expect_error(
    compare_directories(dir, dir),
    regexp = "same directory"
  )
})

test_that("compare_directories rejects identical paths differing by trailing slash (VUL-05)", {
  dir <- fs::path_temp("vul05_slash")
  fs::dir_create(dir)
  on.exit(fs::dir_delete(dir), add = TRUE)

  expect_error(
    compare_directories(dir, paste0(dir, "/")),
    regexp = "same directory"
  )
})

# --- VUL-06: nested path guard -----------------------------------------------

test_that("compare_directories rejects right nested inside left (VUL-06)", {
  base  <- fs::path_temp("vul06_nest")
  left_ <- fs::path(base, "left")
  right_<- fs::path(base, "left", "right")
  fs::dir_create(left_)
  fs::dir_create(right_)
  on.exit(fs::dir_delete(base), add = TRUE)

  expect_error(
    compare_directories(left_, right_),
    regexp = "[Nn]ested"
  )
})

test_that("compare_directories rejects left nested inside right (VUL-06)", {
  base  <- fs::path_temp("vul06_nest2")
  right_<- fs::path(base, "right")
  left_ <- fs::path(base, "right", "left")
  fs::dir_create(right_)
  fs::dir_create(left_)
  on.exit(fs::dir_delete(base), add = TRUE)

  expect_error(
    compare_directories(left_, right_),
    regexp = "[Nn]ested"
  )
})

# --- VUL-30: absolute paths stored in sync_status ---------------------------

test_that("compare_directories stores absolute paths in sync_status (VUL-30)", {
  base  <- fs::path_temp("vul30_abs")
  left_ <- fs::path(base, "left")
  right_<- fs::path(base, "right")
  fs::dir_create(left_)
  fs::dir_create(right_)
  on.exit(fs::dir_delete(base), add = TRUE)

  # call with a path that could be relative if cwd were base
  result <- compare_directories(left_, right_)

  expect_true(fs::is_absolute_path(result$left_path))
  expect_true(fs::is_absolute_path(result$right_path))
})

# --- VUL-09: sync_status class check in sync functions ----------------------

test_that("full_asym_sync_to_right rejects non-syncdr_status object (VUL-09)", {
  bad_status <- list(common_files = NULL, non_common_files = NULL,
                     left_path = "x", right_path = "y")
  expect_error(
    full_asym_sync_to_right(sync_status = bad_status),
    regexp = "syncdr_status"
  )
})

test_that("full_symmetric_sync rejects non-syncdr_status object (VUL-09)", {
  bad_status <- list(common_files = NULL, non_common_files = NULL,
                     left_path = "x", right_path = "y")
  expect_error(
    full_symmetric_sync(sync_status = bad_status),
    regexp = "syncdr_status"
  )
})

test_that("update_missing_files_asym_to_right rejects non-syncdr_status (VUL-09)", {
  bad_status <- list(common_files = NULL, non_common_files = NULL,
                     left_path = "x", right_path = "y")
  expect_error(
    update_missing_files_asym_to_right(sync_status = bad_status),
    regexp = "syncdr_status"
  )
})

# --- VUL-10: backup_dir overlap check ----------------------------------------

test_that("full_asym_sync_to_right rejects backup_dir equal to right_path (VUL-10)", {
  base  <- fs::path_temp("vul10_eq")
  left_ <- fs::path(base, "left")
  right_<- fs::path(base, "right")
  fs::dir_create(left_)
  fs::dir_create(right_)
  writeLines("x", fs::path(left_, "f.csv"))
  on.exit(fs::dir_delete(base), add = TRUE)

  expect_error(
    full_asym_sync_to_right(left_path  = left_,
                             right_path = right_,
                             backup     = TRUE,
                             backup_dir = right_),
    regexp = "overlap"
  )
})

test_that("full_asym_sync_to_right rejects backup_dir inside right_path (VUL-10)", {
  base   <- fs::path_temp("vul10_inside")
  left_  <- fs::path(base, "left")
  right_ <- fs::path(base, "right")
  bad_bk <- fs::path(base, "right", "backup")
  fs::dir_create(left_)
  fs::dir_create(right_)
  writeLines("x", fs::path(left_, "f.csv"))
  on.exit(fs::dir_delete(base), add = TRUE)

  expect_error(
    full_asym_sync_to_right(left_path  = left_,
                             right_path = right_,
                             backup     = TRUE,
                             backup_dir = bad_bk),
    regexp = "overlap"
  )
})

test_that("full_symmetric_sync rejects backup_dir equal to left_path (VUL-10)", {
  base  <- fs::path_temp("vul10_sym")
  left_ <- fs::path(base, "left")
  right_<- fs::path(base, "right")
  fs::dir_create(left_)
  fs::dir_create(right_)
  writeLines("x", fs::path(left_, "f.csv"))
  on.exit(fs::dir_delete(base), add = TRUE)

  expect_error(
    full_symmetric_sync(left_path  = left_,
                        right_path = right_,
                        backup     = TRUE,
                        backup_dir = left_),
    regexp = "overlap"
  )
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tests for Fix Group C — stale sync_status (VUL-22) and race conditions (VUL-23)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# --- VUL-22: created_at timestamp ----------------------------------------

test_that("compare_directories output contains created_at of class POSIXct (VUL-22)", {
  e     <- toy_dirs()
  left  <- e$left
  right <- e$right

  res <- compare_directories(left, right)

  expect_true("created_at" %in% names(res))
  expect_s3_class(res$created_at, "POSIXct")
  # Timestamp should be within 10 seconds of now
  expect_lt(
    as.numeric(difftime(Sys.time(), res$created_at, units = "secs")),
    10
  )
})

test_that("no staleness warning for a fresh sync_status (VUL-22)", {
  e     <- toy_dirs()
  left  <- e$left
  right <- e$right

  fresh_status <- compare_directories(left, right)

  withr::local_options(list(syncdr.staleness_threshold_secs = 3600L))
  # A freshly-created object must not trigger the staleness warning
  expect_no_warning(check_sync_status_staleness(fresh_status))
})

test_that("staleness warning fires when sync_status exceeds threshold (VUL-22)", {
  e     <- toy_dirs()
  left  <- e$left
  right <- e$right

  old_status <- compare_directories(left, right)
  # Back-date the timestamp so it looks 2 hours old
  old_status$created_at <- Sys.time() - 7200

  withr::local_options(list(syncdr.staleness_threshold_secs = 3600L))

  # cli::cli_warn() emits a classed warning — catch via tryCatch
  caught <- tryCatch(
    check_sync_status_staleness(old_status),
    warning = function(w) w
  )
  expect_s3_class(caught, "warning")
  expect_match(conditionMessage(caught), "minute(s) old", fixed = TRUE)
})

test_that("staleness check is skipped when created_at is absent (VUL-22 backward compat)", {
  e     <- toy_dirs()
  left  <- e$left
  right <- e$right

  # Simulate a legacy sync_status without the created_at field
  legacy_status <- compare_directories(left, right)
  legacy_status$created_at <- NULL

  expect_no_warning(check_sync_status_staleness(legacy_status))
})

# --- VUL-23: skip-not-error on externally deleted file -------------------

test_that("full_asym_sync_to_right skips a file deleted externally mid-loop (VUL-23)", {
  # Build a scenario where right has extra files (will be deleted by asym sync)
  base   <- fs::path_temp("vul23_asym")
  left_  <- fs::path(base, "left")
  right_ <- fs::path(base, "right")
  fs::dir_create(c(left_, right_))
  saveRDS(1L, fs::path(left_,  "keep.Rds"))
  saveRDS(2L, fs::path(right_, "delete_me.Rds"))
  saveRDS(3L, fs::path(right_, "also_delete.Rds"))
  on.exit(fs::dir_delete(base), add = TRUE)

  status <- compare_directories(left_, right_)

  # Delete one of the right-only files *before* the sync runs —
  # simulating an external deletion between compare and sync.
  fs::file_delete(fs::path(right_, "delete_me.Rds"))

  # The sync should complete without error (skipping the already-gone file)
  expect_no_error(
    suppressWarnings(
      full_asym_sync_to_right(
        sync_status     = status,
        force           = TRUE,
        delete_in_right = TRUE,
        backup          = FALSE
      )
    )
  )

  # The file that was not pre-deleted should now be gone
  expect_false(fs::file_exists(fs::path(right_, "also_delete.Rds")))
})


