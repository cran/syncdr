#' Copy files from left to right directory
#'
#' This function copies files from a source directory (left_dir) to a destination directory (right_dir) based on a provided data frame containing file paths and synchronization status.
#'
#' The function performs the following steps:
#' 1. Checks if the source and destination directories exist and creates the destination directory if it doesn't already exist.
#' 2. Copies files from the source directory to the corresponding subdirectory in the destination directory based on the provided file paths.
#'
#' @param left_dir Path of the source directory (left/leader).
#' @param right_dir Path of the destination directory (right/follower).
#' @param files_to_copy Data frame containing paths of files to copy and their synchronization status.
#' @param recurse Logical, default is TRUE.
#'   - If TRUE: Files are copied into corresponding sub-directories in the destination folder. If a subdirectory doesn't exist in the destination, it will be created.
#'   - If FALSE: Files are copied to the top level of the destination directory.
#' @param overwrite Logical, default is TRUE. If TRUE, existing files at the
#'   destination are overwritten. If FALSE, existing destination files are
#'   preserved and the copy is skipped.
#'
#' @return Invisible TRUE. Aborts with an error if the destination directory
#'   is not writable (VUL-27). If individual file copies fail, a warning is
#'   issued listing all failed files and copying continues for the remainder
#'   (VUL-24/VUL-26).
#'
#' @keywords internal
#'
copy_files_to_right <- function(left_dir,
                                right_dir,
                                files_to_copy,
                                recurse   = TRUE,
                                overwrite = TRUE) {

  if (recurse == TRUE) {

    # Use fs::path_rel() instead of gsub() to compute the relative path.
    # gsub() treats left_dir as a regex pattern, which silently produces wrong
    # results for paths with metacharacters (e.g. 'user.name', 'data (copy)',
    # 'project+files'). fs::path_rel() is safe for any path.
    files_to_copy <- files_to_copy |>
      ftransform(wo_root_left = fs::path_rel(path_left, start = left_dir)) |>
      ftransform(path_from    = path_left,
                 path_to      = fs::path(right_dir, wo_root_left))
  }

  else {
    files_to_copy <- files_to_copy |>
      ftransform(path_from    = path_left,
                 path_to      = right_dir)
  }

  # VUL-27: pre-flight write-permission check before touching any file.
  # Catching a permission error after 50 files have been copied leaves the
  # directory in a partially-updated state with no way to undo.
  if (!fs::file_access(right_dir, mode = "write")) {
    cli::cli_abort(c(
      "No write permission on destination directory {.path {right_dir}}.",
      "x" = "Sync aborted before any files were modified.",
      "i" = "Check that the directory exists and that you have write access."
    ))
  }

  # Ensure destination subdirectories exist
  unique_dirs <- unique(fs::path_dir(files_to_copy$path_to))
  fs::dir_create(unique_dirs)

  # VUL-24/26: wrap each copy in tryCatch so a single failure does not abort
  # the whole loop, leaving the directory in an inconsistent mid-sync state.
  # All failures are collected and reported together at the end.
  failures <- character(0)

  invisible(
    lapply(
      cli::cli_progress_along(files_to_copy$path_from, name = "Copying files"),
      function(i) {
        tryCatch(
          fs::file_copy(
            path      = files_to_copy$path_from[i],
            new_path  = files_to_copy$path_to[i],
            overwrite = overwrite
          ),
          error = function(e) {
            failures <<- c(failures, files_to_copy$path_from[i])
            cli::cli_warn(c(
              "Could not copy {.path {files_to_copy$path_from[i]}}.",
              "x" = conditionMessage(e),
              "i" = "File skipped - all other files will still be processed."
            ))
          }
        )
      }
    )
  )

  if (length(failures) > 0) {
    cli::cli_warn(c(
      "{length(failures)} file{?s} could not be copied to {.path {right_dir}}.",
      "i" = "Check permissions, disk space, and whether source files are locked.",
      "i" = "Failed file{?s}: {.path {failures}}"
    ))
  }

  invisible(TRUE)

}

#' Copy files from right directory to left directory
#'
#' This function copies files from a right (source) directory to a left (destination) directory based on a provided data frame containing file paths and synchronization status.
#'
#' The function performs the following steps:
#' 1. Checks if the source (right) and destination (left) directories exist and creates the destination directory if it doesn't already exist.
#' 2. Copies files from the right directory to the corresponding subdirectory in the left directory based on the provided file paths.
#'
#' @param left_dir Path of the left (destination) directory.
#' @param right_dir Path of the right (source) directory.
#' @param files_to_copy Data frame containing paths of files to copy and their synchronization status.
#' @param recurse Logical, default is TRUE.
#'   - If TRUE: Files are copied into corresponding subdirectories in the left directory. If a subdirectory doesn't exist in the left directory, it will be created.
#'   - If FALSE: Files are copied to the top level of the left directory.
#' @param overwrite Logical, default is TRUE. If TRUE, existing files at the
#'   destination are overwritten. If FALSE, existing destination files are
#'   preserved and the copy is skipped.
#'
#' @return Invisible TRUE. Aborts with an error if the destination directory
#'   is not writable (VUL-27). If individual file copies fail, a warning is
#'   issued listing all failed files and copying continues for the remainder
#'   (VUL-24/VUL-26).
#'
#' @keywords internal
#'
#'
copy_files_to_left <- function(left_dir,
                               right_dir,
                               files_to_copy,
                               recurse   = TRUE,
                               overwrite = TRUE) {


  # Check/create source and destination path

  if (isTRUE(recurse)) {

    # Use fs::path_rel() instead of gsub() to compute the relative path.
    # gsub() treats right_dir as a regex pattern, which silently produces wrong
    # results for paths with metacharacters. fs::path_rel() is safe for any path.
    files_to_copy <- files_to_copy |>
      ftransform(wo_root_right = fs::path_rel(path_right, start = right_dir)) |>
      ftransform(path_from    = path_right,
                 path_to      = fs::path(left_dir, wo_root_right))

  } else {

    files_to_copy <- files_to_copy |>
      ftransform(path_from    = path_right,
                 path_to      = left_dir)

  }

  # VUL-27: pre-flight write-permission check before touching any file.
  if (!fs::file_access(left_dir, mode = "write")) {
    cli::cli_abort(c(
      "No write permission on destination directory {.path {left_dir}}.",
      "x" = "Sync aborted before any files were modified.",
      "i" = "Check that the directory exists and that you have write access."
    ))
  }

  # Ensure destination subdirectories exist
  unique_dirs <- unique(fs::path_dir(files_to_copy$path_to))
  fs::dir_create(unique_dirs)

  # VUL-24/26: wrap each copy in tryCatch so a single failure does not abort
  # the whole loop. All failures are collected and reported together at the end.
  failures <- character(0)

  invisible(
    lapply(
      cli::cli_progress_along(files_to_copy$path_from, name = "Copying files"),
      function(i) {
        tryCatch(
          fs::file_copy(
            path      = files_to_copy$path_from[i],
            new_path  = files_to_copy$path_to[i],
            overwrite = overwrite
          ),
          error = function(e) {
            failures <<- c(failures, files_to_copy$path_from[i])
            cli::cli_warn(c(
              "Could not copy {.path {files_to_copy$path_from[i]}}.",
              "x" = conditionMessage(e),
              "i" = "File skipped - all other files will still be processed."
            ))
          }
        )
      }
    )
  )

  if (length(failures) > 0) {
    cli::cli_warn(c(
      "{length(failures)} file{?s} could not be copied to {.path {left_dir}}.",
      "i" = "Check permissions, disk space, and whether source files are locked.",
      "i" = "Failed file{?s}: {.path {failures}}"
    ))
  }

  invisible(TRUE)
}


