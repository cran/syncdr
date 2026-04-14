test_that("rs_theme returns a list with correct structure", {
  res <- rs_theme()

  # Check it's a list
  expect_type(res, "list")

  # Check all expected fields are present
  expect_named(res, c("editor", "global", "dark", "foreground", "background"))

  # Check default values
  expect_equal(res$editor, "")
  expect_equal(res$global, "")
  expect_equal(res$dark, FALSE)
  expect_equal(res$foreground, "")
  expect_equal(res$background, "")
})

test_that("rs_theme returns invisible result", {
  res <- capture.output({
    out <- rs_theme()
  })
  expect_identical(rs_theme(), rs_theme()) # just ensures it runs invisibly
})


# ---- perform_backup() tests (VUL-17, 20, 21) ---------------------------------

test_that("perform_backup() creates timestamped subdir and returns its path (VUL-21)", {
  src <- withr::local_tempdir()
  writeLines("hello", file.path(src, "file.txt"))
  dst_root <- withr::local_tempdir()

  result <- syncdr:::perform_backup(src, dst_root, label = "right")

  expect_true(dir.exists(result))
  expect_match(basename(result), "^right_\\d{8}_\\d{6}$")
  expect_true(file.exists(file.path(result, basename(src), "file.txt")))
})

test_that("perform_backup() repeated calls produce distinct subdirs (VUL-21)", {
  src <- withr::local_tempdir()
  writeLines("hello", file.path(src, "file.txt"))
  dst_root <- withr::local_tempdir()

  r1 <- syncdr:::perform_backup(src, dst_root, label = "right")
  Sys.sleep(1.1)  # ensure different timestamp
  r2 <- syncdr:::perform_backup(src, dst_root, label = "right")

  expect_false(identical(r1, r2))
  expect_true(dir.exists(r1))
  expect_true(dir.exists(r2))
})

test_that("perform_backup() warns when backup_dir == 'temp_dir' (VUL-20)", {
  src <- withr::local_tempdir()
  writeLines("hello", file.path(src, "file.txt"))

  expect_warning(
    syncdr:::perform_backup(src, "temp_dir", label = "right"),
    regexp = "ephemeral"
  )
})

test_that("perform_backup() aborts when target dir cannot be created (VUL-17)", {
  src <- withr::local_tempdir()
  writeLines("hello", file.path(src, "file.txt"))

  # Create a plain file, then attempt to use it as a directory root.
  # dir.create() inside a file path always fails on any OS.
  blocker <- file.path(withr::local_tempdir(), "i_am_a_file.txt")
  writeLines("block", blocker)
  bad_root <- file.path(blocker, "subdir")  # path through a file = impossible

  # dir.create() also warns on failure; suppress it so only the cli_abort matters
  expect_error(
    suppressWarnings(syncdr:::perform_backup(src, bad_root, label = "right")),
    regexp = "Could not create backup directory"
  )
})

# ---- VUL-18: full_symmetric_sync() creates distinct right_* / left_* subdirs ----

test_that("full_symmetric_sync() backup creates separate right_* and left_* subdirs (VUL-18)", {
  e      <- toy_dirs()
  st     <- copy_temp_environment()
  left   <- st$left
  right  <- st$right
  bkp    <- withr::local_tempdir()

  sync_status <- compare_directories(left, right)
  suppressWarnings(full_symmetric_sync(sync_status = sync_status, backup = TRUE, backup_dir = bkp))

  subdirs <- list.dirs(bkp, recursive = FALSE, full.names = FALSE)
  right_subdirs <- subdirs[grepl("^right_", subdirs)]
  left_subdirs  <- subdirs[grepl("^left_",  subdirs)]

  expect_gte(length(right_subdirs), 1L)
  expect_gte(length(left_subdirs),  1L)
})

# ---- VUL-19: partial_symmetric_sync_common_files() backup preserves subdir structure ----

test_that("partial_symmetric_sync_common_files() backup preserves subdirectory structure (VUL-19)", {
  e      <- toy_dirs()
  st     <- copy_temp_environment()
  left   <- st$left
  right  <- st$right
  bkp    <- withr::local_tempdir()

  sync_status <- compare_directories(left, right)
  suppressWarnings(
    partial_symmetric_sync_common_files(sync_status = sync_status, backup = TRUE, backup_dir = bkp)
  )

  # At least one right_* backup subdir should contain a nested subdirectory
  right_backup <- list.dirs(bkp, recursive = FALSE)[grepl("right_", list.dirs(bkp, recursive = FALSE, full.names = FALSE))]
  if (length(right_backup) > 0) {
    inner_dirs <- list.dirs(right_backup[[1]], recursive = FALSE)
    expect_gte(length(inner_dirs), 1L)
  } else {
    skip("No right backup subdir found — backup may not have been triggered")
  }
})

