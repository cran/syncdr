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
#'
#' @return Invisible TRUE upon successful completion of the file copying process.
#'
#' @keywords internal
#'
copy_files_to_right <- function(left_dir,
                                right_dir,
                                files_to_copy,
                                recurse = TRUE) {

  if (recurse == TRUE) {

    files_to_copy <- files_to_copy |>
      ftransform(wo_root_left = gsub(left_dir, "", path_left)) |>
      ftransform(path_from    = path_left,
                 path_to      = fs::path(right_dir, wo_root_left))
  }

  else {
    files_to_copy <- files_to_copy |>
      ftransform(path_from    = path_left,
                 path_to      = right_dir)
  }

  # Ensure destination subdirectories exist
  unique_dirs <- unique(fs::path_dir(files_to_copy$path_to))
  fs::dir_create(unique_dirs)

  # progress-enabled iteration
  invisible(
    lapply(
      cli::cli_progress_along(files_to_copy$path_from, name = "Copying files"),
      function(i) {
        fs::file_copy(
          path     = files_to_copy$path_from[i],
          new_path = files_to_copy$path_to[i],
          overwrite = TRUE
        )
      }
    )
  )

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
#'
#' @return Invisible TRUE upon successful completion of the file copying process.
#'
#' @keywords internal
#'
#'
copy_files_to_left <- function(left_dir,
                               right_dir,
                               files_to_copy,
                               recurse = TRUE) {


  # Check/create source and destination path

  if (recurse == TRUE) {

    files_to_copy <- files_to_copy |>
      ftransform(wo_root_right = gsub(right_dir, "", path_right)) |>
      ftransform(path_from    = path_right,
                 path_to      = fs::path(left_dir, wo_root_right))

  } else {

    files_to_copy <- files_to_copy |>
      ftransform(path_from    = path_right,
                 path_to      = left_dir)

  }

  # Ensure destination subdirectories exist
  unique_dirs <- unique(fs::path_dir(files_to_copy$path_to))
  fs::dir_create(unique_dirs)

  # progress-enabled iteration
  invisible(
    lapply(
      cli::cli_progress_along(files_to_copy$path_from, name = "Copying files"),
      function(i) {
        fs::file_copy(
          path     = files_to_copy$path_from[i],
          new_path = files_to_copy$path_to[i],
          overwrite = TRUE
        )
      }
    )
  )

  invisible(TRUE)
}


