# Testing auxiliary functions

toy_dirs()

# Copy original paths to test functions on copies
syncdr_temp <- copy_temp_environment()
left  <- syncdr_temp$left
right <- syncdr_temp$right

# Get sync status object (from compare_directories)
sync_status_date      <- compare_directories(left_path  = left,
                                             right_path = right)

sync_status_date_cont <- compare_directories(left_path  = left,
                                             right_path = right,
                                             by_content = TRUE)

sync_status_content   <- compare_directories(left_path  = left,
                                             right_path = right,
                                             by_date    = FALSE,
                                             by_content = TRUE)

# Test filter common files ####

# ~~~~~~~~~ filter by date only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("filter common files works -by date", {

  # ----------- left dir --------------------------

  to_filter <- sync_status_date$common_files |>
    fsubset(is_new_left) |>
    fselect(path_left, path_right)

  res_left <- filter_common_files(sync_status_date$common_files,
                                  dir = "left") |>
    fselect(path_left, path_right)

  expect_equal(to_filter,
               res_left)


  # ---------- right dir ---------------------------
  to_filter <- sync_status_date$common_files |>
    fsubset(is_new_right) |>
    fselect(path_left, path_right)

  res_right <- filter_common_files(sync_status_date$common_files,
                                  dir = "right") |>
    fselect(path_left, path_right)

  expect_equal(to_filter,
               res_right)

  # ---------- both dir ---------------------------

  to_filter <- sync_status_date$common_files |>
    fsubset(is_new_right | is_new_left) |>
    fselect(path_left, path_right)

  res_all <- filter_common_files(sync_status_date$common_files,
                                   dir = "all") |>
    fselect(path_left, path_right)

  expect_equal(to_filter,
               res_all)

})

# ~~~~~~~~~ filter by date and content ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("filter common files works -by date&cont", {

  # ----------- left dir --------------------------
  to_filter <- sync_status_date_cont$common_files |>
    fsubset(is_new_left) |>
    fsubset(is_diff) |>
    fselect(path_left, path_right)

  res_left <- filter_common_files(sync_status_date_cont$common_files,
                                  dir = "left",
                                  by_content = TRUE) |>
    fselect(path_left, path_right)

  expect_equal(to_filter,
               res_left)

  # ---------- right dir ---------------------------
  to_filter <- sync_status_date_cont$common_files |>
    fsubset(is_new_right) |>
    fsubset(is_diff) |>
    fselect(path_left, path_right)

  res_right <- filter_common_files(sync_status_date_cont$common_files,
                                   dir = "right",
                                   by_content = TRUE) |>
    fselect(path_left, path_right)

  expect_equal(to_filter,
               res_right)

  # ---------- both dir ---------------------------

  to_filter <- sync_status_date_cont$common_files |>
    fsubset(is_new_right | is_new_left) |>
    fsubset(is_diff) |>
    fselect(path_left, path_right)

  res_all <- filter_common_files(sync_status_date_cont$common_files,
                                 dir = "all",
                                 by_content = TRUE) |>
    fselect(path_left, path_right)

  expect_equal(to_filter,
               res_all)

})

# ~~~~~~~~~ filter by content only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("filter common files works -by cont", {

  to_filter <- sync_status_content$common_files |>
      fsubset(is_diff) |>
      fselect(path_left, path_right)

  res_left <- filter_common_files(sync_status_content$common_files,
                                  dir = "left",
                                  by_date = FALSE,
                                  by_content = TRUE) |>
    fselect(path_left, path_right)

  expect_equal(to_filter,
               res_left)


  res_right <- filter_common_files(sync_status_content$common_files,
                                   dir = "right",
                                   by_date = FALSE,
                                   by_content = TRUE) |>
    fselect(path_left, path_right)

  expect_equal(res_left,
               res_right)

  res_all <- filter_common_files(sync_status_content$common_files,
                                 dir = "all",
                                 by_date = FALSE,
                                 by_content = TRUE) |>
    fselect(path_left, path_right)

  expect_equal(res_right,
               res_all)

})

# Test filter non common files ####

test_that("filter non common files works", {

  # ---------- left dir ---------------------------

  to_filter <- sync_status_content$non_common_files |>
    fsubset(!is.na(path_left) & is.na(path_right)) |>
    fselect(path_left, path_right)

  res_left <- filter_non_common_files(sync_status_date$non_common_files,
                                      dir = "left") |>
    fselect(path_left, path_right)

  expect_equal(to_filter,
               res_left)

  filter_non_common_files(sync_status_date_cont$non_common_files,
                          dir = "left") |>
    fselect(path_left, path_right) |>
    expect_equal(res_left)

  filter_non_common_files(sync_status_content$non_common_files,
                          dir = "left") |>
    fselect(path_left, path_right) |>
    expect_equal(res_left)

  # ---------- right dir ---------------------------
  to_filter <- sync_status_content$non_common_files |>
    fsubset(is.na(path_left) & !is.na(path_right)) |>
    fselect(path_left, path_right)

  res_right <- filter_non_common_files(sync_status_date$non_common_files,
                                      dir = "right") |>
    fselect(path_left, path_right)

  expect_equal(to_filter,
               res_right)

  filter_non_common_files(sync_status_date_cont$non_common_files,
                          dir = "right") |>
    fselect(path_left, path_right) |>
    expect_equal(res_right)

  filter_non_common_files(sync_status_content$non_common_files,
                          dir = "right") |>
    fselect(path_left, path_right) |>
    expect_equal(res_right)


  # ---------- both dir ---------------------------
  to_filter <- sync_status_content$non_common_files |>
    fselect(path_left, path_right)

  res <- filter_non_common_files(sync_status_date$non_common_files,
                                       dir = "all") |>
    fselect(path_left, path_right)

  expect_equal(to_filter,
               res)

  filter_non_common_files(sync_status_date_cont$non_common_files,
                          dir = "all") |>
    fselect(path_left, path_right) |>
    expect_equal(res)

  filter_non_common_files(sync_status_content$non_common_files,
                          dir = "all") |>
    fselect(path_left, path_right) |>
    expect_equal(res)

})

# Test search of duplicate files ####
test_that("search duplicates works as expected", {

  # check it stops if path does not exist
  invalid_path <- "iinvalid_path"
  search_duplicates(invalid_path) |>
    expect_error()

  # create a duplicate file in left directory
  fs::file_copy(path      = paste0(left, "/C/C1.Rds") ,
                new_path  = paste0(left, "/C/C1_dup.Rds"),
                overwrite = FALSE)

  duplicates <- c(paste0(left, "/C/C1.Rds"),
                  paste0(left, "/C/C1_dup.Rds"))

  search_duplicates(left) |>
    expect_no_error()

  res <- search_duplicates(left)

  # check class and names
  inherits(res,
           "data.frame") |>
    expect_equal(TRUE)

  names(res) |>
    expect_equal(c("path", "hash"))

  # checks duplicates
  all(duplicates %in% res$path) |>
    expect_equal(TRUE)

})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tests for VUL-01 / VUL-31: path-metacharacter safety in directory_info()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_that("directory_info: wo_root is correct for paths with dots in dir name", {
  # Dots are the most common metacharacter trigger: user.name, v1.0, etc.
  base  <- fs::path_temp("di_dot_test")
  dir_  <- fs::path(base, "user.name")
  sub_  <- fs::path(dir_, "subdir")
  fs::dir_create(sub_)
  f1 <- fs::path(dir_, "report.csv")
  f2 <- fs::path(sub_, "data.csv")
  writeLines("a", f1)
  writeLines("b", f2)
  on.exit(fs::dir_delete(base), add = TRUE)

  info <- directory_info(dir_)

  # wo_root should be the path relative to dir_, not a regex-stripped mess
  expected_roots <- sort(c(
    as.character(fs::path_rel(f1, start = dir_)),
    as.character(fs::path_rel(f2, start = dir_))
  ))
  expect_equal(sort(as.character(info$wo_root)), expected_roots)
})

test_that("directory_info: wo_root is correct for paths with parentheses in dir name", {
  base  <- fs::path_temp("di_paren_test")
  dir_  <- fs::path(base, "data (copy)")
  fs::dir_create(dir_)
  f <- fs::path(dir_, "file.csv")
  writeLines("x", f)
  on.exit(fs::dir_delete(base), add = TRUE)

  info <- directory_info(dir_)

  expect_equal(as.character(info$wo_root), as.character(fs::path_rel(f, start = dir_)))
})

test_that("directory_info: wo_root is correct for paths with plus sign in dir name", {
  base  <- fs::path_temp("di_plus_test")
  dir_  <- fs::path(base, "project+files")
  fs::dir_create(dir_)
  f <- fs::path(dir_, "output.csv")
  writeLines("y", f)
  on.exit(fs::dir_delete(base), add = TRUE)

  info <- directory_info(dir_)

  expect_equal(as.character(info$wo_root), as.character(fs::path_rel(f, start = dir_)))
})

test_that("directory_info: wo_root is stable with trailing slash on dir", {
  base  <- fs::path_temp("di_slash_test")
  dir_  <- fs::path(base, "mydir")
  fs::dir_create(dir_)
  f <- fs::path(dir_, "file.csv")
  writeLines("z", f)
  on.exit(fs::dir_delete(base), add = TRUE)

  # Call once with trailing slash (after path_norm it should be equivalent)
  info_no_slash   <- directory_info(dir_)
  info_with_slash <- directory_info(paste0(dir_, "/"))

  # wo_root must be identical regardless of trailing slash
  expect_equal(info_no_slash$wo_root, info_with_slash$wo_root)
})

test_that("compare_directories: files match correctly when dir names contain dots", {
  base  <- fs::path_temp("cd_dot_test")
  left_ <- fs::path(base, "v1.0", "left")
  right_<- fs::path(base, "v1.0", "right")
  fs::dir_create(left_)
  fs::dir_create(right_)
  writeLines("same", fs::path(left_, "common.csv"))
  writeLines("same", fs::path(right_, "common.csv"))
  writeLines("only_left", fs::path(left_, "extra.csv"))
  on.exit(fs::dir_delete(base), add = TRUE)

  result <- compare_directories(left_, right_)

  # common.csv should be in common_files (not phantom non-common)
  expect_equal(nrow(result$common_files), 1L)
  # extra.csv should be the sole non-common file
  expect_equal(nrow(result$non_common_files), 1L)
})



