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



