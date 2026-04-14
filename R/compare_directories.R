#' Compare Two Directories for Synchronization Status
#'
#' This function compares two directories, typically referred to as 'left' and 'right', to determine their synchronization status at the file level.
#' The primary goal is to identify the synchronization status of files present in both directories and those exclusive to either directory.
#'
#'
#' @section Sync Status Types:
#' The synchronization status is determined for files present in both directories, as well as for files exclusive to either directory.
#' It can be computed based on modification date only, content only, or both.
#'
#' For Common Files:
#' * When comparing by date: 'new', 'old', or 'same'.
#' * When comparing by date and content: 'new and different', 'new and same', 'old and different', 'old and same', 'same and different', or 'same and same'.
#' * When comparing by content only: 'different' or 'same'.
#'
#' For Non-Common Files:
#' * When comparing by date (or by date and content, or by content only): 'only in left' or 'only in right'.
#'
#' @param left_path Path to the left/first directory.
#' @param right_path Path to the right/second directory.
#' @param by_date Logical. If TRUE (default), compares directories based on the modification date of common files.
#' @param by_content Logical. If TRUE, compares directories based on the hashed content of common files. Default is FALSE
#' @param recurse If TRUE, fully recurses through subdirectories. If a positive integer, specifies the number of levels to recurse.
#' @param verbose Logical. If TRUE display additional info on the comparison process. Default is FALSE
#'
#' @return A list of class "syncdr_status" containing the following elements:
#'   - Non-common files: Paths and synchronization status of files exclusive to either directory.
#'   - Common files: Paths and synchronization status of files present in both directories.
#'   - Path of the left directory.
#'   - Path of the right directory.
#'   - `created_at`: A `POSIXct` timestamp recording when the comparison was performed,
#'     used by sync functions to detect stale results.
#'
#' @export
#' @examples
#' \donttest{
#' e <- toy_dirs()
#' left  <- e$left
#' right <- e$right
#' compare_directories(left, right)
#' compare_directories(left, right, by_content = TRUE)
#' compare_directories(left, right, by_content = TRUE, by_date = FALSE)
#' }
compare_directories <- function(left_path,
                                right_path,
                                recurse     = TRUE,
                                by_date     = TRUE,
                                by_content  = FALSE,
                                verbose    = getOption("syncdr.verbose")){
                                #short_paths = getOption("syncdr.short_paths")) {

  # VUL-11: validate type/length/NA/empty before any fs call
  validate_path_arg(left_path,  "left_path")
  validate_path_arg(right_path, "right_path")

  # VUL-05: reject identical paths (self-sync)
  if (identical(fs::path_norm(left_path), fs::path_norm(right_path))) {
    cli::cli_abort(
      c(
        "{.arg left_path} and {.arg right_path} point to the same directory.",
        "x" = "Self-synchronization would corrupt the directory.",
        "i" = "Provide two distinct directories."
      )
    )
  }

  # VUL-06: reject nested paths (one is an ancestor/descendant of the other)
  left_norm  <- fs::path_norm(left_path)
  right_norm <- fs::path_norm(right_path)
  if (startsWith(right_norm, paste0(left_norm, "/")) ||
      startsWith(left_norm,  paste0(right_norm, "/"))) {
    cli::cli_abort(
      c(
        "One of the paths is nested inside the other.",
        "x" = "{.path {left_path}} vs {.path {right_path}}",
        "i" = "Nested synchronization can cause recursive copies and data loss."
      )
    )
  }

  # VUL-30: normalise to absolute paths so sync_status is portable
  left_path  <- as.character(fs::path_abs(left_path))
  right_path <- as.character(fs::path_abs(right_path))

  # Get info on directory 1, i.e. left
  info_left <- directory_info(dir     = left_path,
                              recurse = recurse)
  # Get info on directory 2, i.e., right
  info_right <- directory_info(dir     = right_path,
                               recurse = recurse)

  join_info <-
    joyn::joyn(
      x                = info_left,
      y                = info_right,
      by               = "wo_root",
      keep_common_vars = TRUE,
      suffixes         = c("_left", "_right"),
      match_type       = "1:1",
      reportvar        = ".joyn",
      verbose          = FALSE
    )

  # Unique file status -? as data frame ?
  non_common_files <- join_info |>
    fsubset(.joyn == "y" | .joyn == "x") |>
    fselect(path_left, path_right) |>
    ftransform(sync_status = ifelse(
      (is.na(path_left) & !is.na(path_right)), "only in right",
      "only in left")
    )

  # Compare common files
  common_files <- join_info |>
    fsubset(.joyn == "x & y") |>
    fselect(path_left,
            path_right,
            modification_time_left,
            modification_time_right)

  # If comparing by date
  if (by_date) {

    compared_times <- compare_modification_times(common_files$modification_time_left,
                                                 common_files$modification_time_right)
    common_files   <- cbind(common_files,
                            compared_times) |>
      ftransform(sync_status      = sync_status_date,
                 sync_status_date = NULL)

  }

  # If comparing by content
  if (by_content) {

    # If by_date TRUE, first filter files that are new in either left or right directory
    if(by_date) {
      common_files <- common_files |>
        fsubset(is_new_left == TRUE | is_new_right == TRUE)
    }

    compared_contents <- compare_file_contents(common_files$path_left,
                                               common_files$path_right,
                                               verbose = verbose)

    common_files      <- cbind(common_files,
                               compared_contents) |>
      ftransform(sync_status         = sync_status_content,
                 sync_status_content = NULL)
  }

  sync_status = list(
    common_files     = common_files,
    non_common_files = non_common_files,
    left_path        = left_path,
    right_path       = right_path,
    created_at       = Sys.time()   # VUL-22: timestamp for staleness detection
  )

  # Display directories structure if verbose is TRUE
  if(verbose) {
    display_dir_tree(left_path,
                     right_path)
  }

  # assign class 'syncdr_status'
  class(sync_status) <- "syncdr_status"

  return(sync_status)
}


