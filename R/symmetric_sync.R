#' Full symmetric synchronization
#'
#' This function updates directories in the following way:
#' * For common files:
#'   - if by date: If the file in one directory is newer than the corresponding file in the other directory,
#'                 it will be copied over to update the older version. If modification dates are the same, no action is taken
#'   - if by date and content: If the file in one directory is newer AND different than the corresponding file in the other directory,
#'                             it will be copied over to update the older version. If modification dates/contents are the same, no action is taken
#'   - if by content only: this option is not active
#' * For non common files:
#'   - if a file exists in one but not in the other it is copied to the other directory
#'
#' @param left_path Path to the left/first directory.
#' @param right_path Path to the right/second directory.
#' @param sync_status Object of class "syncdr_status", output of `compare_directories()`.
#' @param by_date logical, TRUE by default
#' @param by_content logical, FALSE by default
#' @param recurse logical, TRUE by default.
#'  If recurse is TRUE: when copying a file from source folder to destination folder, the file will be copied into the corresponding (sub)directory.
#'  If the sub(directory) where the file is located does not exist in destination folder (or you are not sure), set recurse to FALSE,
#'  and the file will be copied at the top level
#' @param force Logical. If TRUE (by default), directly perform synchronization of the directories.
#'                       If FALSE, displays a preview of actions and prompts the user for confirmation before proceeding. Synchronization is aborted if the user does not agree.
#' @param backup Logical. If TRUE, creates a backup of the right directory before synchronization. The backup is stored in the location specified by `backup_dir`.
#' @param backup_dir Path to the directory where the backup of the original right directory will be stored. If not specified, the backup is stored in temporary directory (`tempdir`).
#' @param verbose logical. If TRUE, display directory tree before and after synchronization. Default is FALSE
#' @return Invisible TRUE indicating successful synchronization.
#' @export
#' @examples
#' # Create a temporary synchronization environment
#'
#' \donttest{
#' e <- toy_dirs()
#' left  <- e$left
#' right <- e$right
#' # Symmetric synchronization by date and content
#' # Option 1: provide left and right paths
#' full_symmetric_sync(
#'   left_path  = left,
#'   right_path = right,
#'   by_date    = TRUE,
#'   by_content = TRUE
#' )
#'
#' # Option 2: provide a precomputed sync_status object
#' sync_status <- compare_directories(
#'   left_path  = left,
#'   right_path = right
#' )
#' full_symmetric_sync(sync_status = sync_status)
#' }
full_symmetric_sync <- function(left_path   = NULL,
                                right_path  = NULL,
                                sync_status = NULL,
                                by_date     = TRUE,
                                by_content  = FALSE,
                                recurse     = TRUE,
                                force       = TRUE,
                                backup      = FALSE,
                                backup_dir  = "temp_dir",
                                verbose     = getOption("syncdr.verbose")) {
  if (verbose == TRUE) {
    # Display folder structure before synchronization
    style_msgs(color_name = "blue",
               text = "Directories structure BEFORE synchronization:\n")
    display_dir_tree(path_left  = left_path,
                     path_right = right_path)
  }


  # --- Check validity of arguments -----------------

  # Either sync_status is null, and both right and left path are provided,
  # or sync_status is provided and left and right are NULL

  if(!(
    is.null(sync_status) && !is.null(left_path) && !is.null(right_path) ||
    !is.null(sync_status) && is.null(left_path) && is.null(right_path)
  )) {

    style_msgs(color_name = "purple",
               text = "Incorrect arguments specification!\n")

    cli::cli_abort("Either sync_status or left and right paths must be provided")

  }

  # --- Get sync_status ----

  # If sync_status is null, but left and right paths are provided
  # get sync_status object -internal call to compare_directories()

  if(is.null(sync_status)) {

    # --- first check directories path ---
    stopifnot(exprs = {
      fs::dir_exists(left_path)
      fs::dir_exists(right_path)
    })

    # --- get sync_status ---
    sync_status <- compare_directories(left_path  = left_path,
                                       right_path = right_path,
                                       by_date    = by_date,
                                       by_content = by_content,
                                       recurse    = recurse,
                                       verbose    = FALSE
    )
  } else {

    # If sync_status is already provided, retrieve left, right, by_date and by_content arguments from it
    left_path  <- sync_status$left_path
    right_path <- sync_status$right_path

    by_date    <- fifelse(is.null(sync_status$common_files$is_new_right),
                          FALSE,
                          by_date)

    by_content <- fifelse(!(is.null(sync_status$common_files$is_diff)),
                          TRUE,
                          by_content)

  }

  # Identify files to copy ####

  # Identify files to copy to right:
  # -- those that are newer/different content in the left directory --
  files_to_right <- sync_status$common_files |>
    filter_common_files(by_date    = by_date,
                        by_content = by_content,
                        dir = "left") |>
    # -- and those that are only in left directory --
    rowbind(
      filter_non_common_files(sync_status$non_common_files,
                              dir = "left")
    )

  # Identify files to copy to left:
  # -- those that are newer/different content in the right directory --
  files_to_left <- sync_status$common_files |>
    filter_common_files(by_date    = by_date,
                        by_content = by_content,
                        dir = "right") |>
    # -- and those that are only in right directory --
    rowbind(
      filter_non_common_files(sync_status$non_common_files,
                              dir = "right")
    )


  # --- Force option ----

  if (force == FALSE) {

    if (nrow(files_to_right) >0 ) {
      style_msgs("blue",
                 text = "These files will be COPIED (overwriting if present) to RIGHT \n")
      display_file_actions(path_to_files = files_to_right |> fselect(1),
                           directory     = left_path,
                           action        = "copy"
      )
    }

    if (nrow(files_to_left) >0 ) {
      style_msgs("blue",
                 text = "These files will be COPIED (overwriting if present) to LEFT \n")
      display_file_actions(path_to_files = files_to_left |> fselect(2),
                           directory     = right_path,
                           action        = "copy"
      )
    }

    # Ask for agreement
    Ask <- askYesNo(msg     = "Do you want to proceed? Type your answer",
                    default = TRUE,
                    prompts = c("Yes", "No", "Cancel"))

    if (Ask == FALSE | is.na(Ask)) {
      cli::cli_abort(message = "Synchronization interrupted.
                                No action taken on directories")}

  }


  # --- Backup ----

  # Copy right and left in backup directory
  if (backup) {

    backup_right <- fifelse(backup_dir == "temp_dir", # the default

                          #tempdir(),
                          file.path(tempdir(),
                                    "backup_right"),
                          backup_dir) # path provided by the user
    backup_left <- fifelse(backup_dir == "temp_dir", # the default

                            #tempdir(),
                            file.path(tempdir(),
                                      "backup_left"),
                            backup_dir) # path provided by the user

    # create the target directory if it does not exist
    if (!dir.exists(backup_right)) {
      dir.create(backup_right,
                 recursive = TRUE)
    }

    if (!dir.exists(backup_left)) {
      dir.create(backup_left,
                 recursive = TRUE)
    }


    # copy dir content
    file.copy(from      = right_path,
              to        = backup_right,
              recursive = TRUE)
    file.copy(from      = left_path,
              to        = backup_left,
              recursive = TRUE)

  }


  # Inform user that sync by content only is not active and stop
  if (by_date == FALSE & by_content == TRUE) {
    cli::cli_abort(message = "Symmetric synchronization by content only is not active
                               -no action will be executed, directories unchanged")
  }

  # --- Synchronization ----

  # copy files from left to right folder
  copy_files_to_right(left_dir      = sync_status$left_path,
                      right_dir     = sync_status$right_path,
                      files_to_copy = files_to_right)

  # copy files from left to right folder
  copy_files_to_left(left_dir      = sync_status$left_path,
                     right_dir     = sync_status$right_path,
                     files_to_copy = files_to_left,
                     recurse       = recurse)

  if (verbose == TRUE) {

    # Display folder structure AFTER synchronization
    style_msgs(color_name = "blue",
               text = "Directories structure AFTER synchronization:\n")
    display_dir_tree(path_left  = left_path,
                     path_right = right_path)
  }

  style_msgs(color_name = "green",
             text = paste0("\u2714", " synchronized\n"))
  invisible(TRUE)

}

#' Partial symmetric synchronization -common files only
#'
#' This function updates directories in the following way:
#' * For common files:
#'   - if by date: If the file in one directory is newer than the corresponding file in the other directory,
#'                 it will be copied over to update the older version. If modification dates are the same, nothing is done
#'   - if by date and content: If the file in one directory is newer AND different than the corresponding file in the other directory,
#'                             it will be copied over to update the older version. If modification dates/contents are the same, nothing is done
#'   - if by content only: this option is not active
#' * For non common files: unchanged, i.e.,
#'   - keep in right those that are only in right
#'   - keep in left those that are only in left
#'
#' @param left_path Path to the left/first directory.
#' @param right_path Path to the right/second directory.
#' @param sync_status Object of class "syncdr_status", output of `compare_directories()`.
#' @param by_date logical, TRUE by default
#' @param by_content logical, FALSE by default
#' @param recurse logical, TRUE by default.
#'  If recurse is TRUE: when copying a file from source folder to destination folder, the file will be copied into the corresponding (sub)directory.
#'  If the sub(directory) where the file is located does not exist in destination folder (or you are not sure), set recurse to FALSE,
#'  and the file will be copied at the top level
#' @param force Logical. If TRUE (by default), directly perform synchronization of the directories.
#'                       If FALSE, displays a preview of actions and prompts the user for confirmation before proceeding. Synchronization is aborted if the user does not agree.
#' @param backup Logical. If TRUE, creates a backup of the right directory before synchronization. The backup is stored in the location specified by `backup_dir`.
#' @param backup_dir Path to the directory where the backup of the original right directory will be stored. If not specified, the backup is stored in temporary directory (`tempdir`).
#' @param verbose logical. If TRUE, display directory tree before and after synchronization. Default is FALSE
#' @return Invisible TRUE indicating successful synchronization.
#' @export
#' @examples
#' # Create a temporary synchronization environment
#'
#' \donttest{
#' e <- toy_dirs()
#' left  <- e$left
#' right <- e$right
#'
#' # Partial symmetric synchronization of common files
#' # Option 1: provide left and right paths
#' partial_symmetric_sync_common_files(
#'   left_path  = left,
#'   right_path = right,
#'   by_date    = TRUE
#' )
#'
#' # Option 2: provide a precomputed sync_status object
#' sync_status <- compare_directories(
#'   left_path  = left,
#'   right_path = right
#' )
#' partial_symmetric_sync_common_files(sync_status = sync_status)
#' }
partial_symmetric_sync_common_files <- function(left_path = NULL,
                                               right_path  = NULL,
                                               sync_status = NULL,
                                               by_date     = TRUE,
                                               by_content  = FALSE,
                                               recurse     = TRUE,
                                               force       = TRUE,
                                               backup      = FALSE,
                                               backup_dir  = "temp_dir",
                                               verbose     = getOption("syncdr.verbose")) {

  if(verbose == TRUE) {

    style_msgs(color_name = "blue",
               text = "Directories structure BEFORE synchronization:\n")
    display_dir_tree(path_left  = left_path,
                     path_right = right_path)

  }

  # --- Check validity of arguments -----------------

  # Either sync_status is null, and both right and left path are provided,
  # or sync_status is provided and left and right are NULL

  if(!(
    is.null(sync_status) && !is.null(left_path) && !is.null(right_path) ||
    !is.null(sync_status) && is.null(left_path) && is.null(right_path)
  )) {

    style_msgs(color_name = "purple",
               text = "Incorrect arguments specification!\n")

    cli::cli_abort("Either sync_status or left and right paths must be provided")

  }

  # --- Get sync_status ----

  if(is.null(sync_status)) {

    # --- first check directories path ---
    stopifnot(exprs = {
      fs::dir_exists(left_path)
      fs::dir_exists(right_path)
    })

    # --- get sync_status ---
    sync_status <- compare_directories(left_path  = left_path,
                                       right_path = right_path,
                                       by_date    = by_date,
                                       by_content = by_content,
                                       recurse    = recurse,
                                       verbose    = FALSE
    )
  } else {

    # If sync_status is already provided, retrieve left, right, by_date and by_content arguments from it
    left_path  <- sync_status$left_path
    right_path <- sync_status$right_path
    by_date    <- fifelse(is.null(sync_status$common_files$is_new_right),
                          FALSE,
                          by_date)

    by_content <- fifelse(!(is.null(sync_status$common_files$is_diff)),
                          TRUE,
                          by_content)

  }

  # Inform user that sync by content only is not active
  if (by_date == FALSE & by_content == TRUE) {
    cli::cli_abort(message = "Symmetric synchronization by content only is not active
                               -no action will be executed, directories unchanged")
  }

  # Identify files to copy ####
  files_to_right <- sync_status$common_files |>
    filter_common_files(by_date    = by_date,
                        by_content = by_content,
                        dir = "left")

  files_to_left <- sync_status$common_files |>
    filter_common_files(by_date    = by_date,
                        by_content = by_content,
                        dir = "right")


  # --- Force option ----

  if (force == FALSE) {

    if (nrow(files_to_right) >0 ) {
      style_msgs("blue",
                 text = "These files will be COPIED (overwriting if present) to RIGHT \n")
      display_file_actions(path_to_files = files_to_right |> fselect(1),
                           directory     = left_path,
                           action        = "copy"
      )
    }

    if (nrow(files_to_left) >0 ) {
      style_msgs("blue",
                 text = "These files will be COPIED (overwriting if present) to LEFT \n")
      display_file_actions(path_to_files = files_to_left |> fselect(2),
                           directory     = right_path,
                           action        = "copy"
      )
    }

    # Ask for agreement
    Ask <- askYesNo(msg     = "Do you want to proceed? Type your answer",
                    default = TRUE,
                    prompts = c("Yes", "No", "Cancel"))

    if (Ask == FALSE | is.na(Ask))
      cli::cli_abort(message = "Synchronization interrupted.
                                No action taken on directories")

  }

  # --- Backup ----

  if (backup) {
    base_backup_dir <- if (backup_dir == "temp_dir") tempdir() else backup_dir

    backup_right <- file.path(base_backup_dir, "backup_right")
    backup_left  <- file.path(base_backup_dir, "backup_left")

    # create backup directories if they don't exist
    if (!dir.exists(backup_right)) dir.create(backup_right, recursive = TRUE)
    if (!dir.exists(backup_left))  dir.create(backup_left, recursive = TRUE)

    # Copy contents of directories, not the directory itself
    right_files <- list.files(right_path, full.names = TRUE, recursive = TRUE)
    left_files  <- list.files(left_path,  full.names = TRUE, recursive = TRUE)

    file.copy(from = right_files,
              to   = backup_right,
              recursive = TRUE,
              copy.date = TRUE)

    file.copy(from = left_files,
              to   = backup_left,
              recursive = TRUE,
              copy.date = TRUE)
  }



  # --- Synchronize -----

  # copy those that are new in left to right

  copy_files_to_right(left_dir      = sync_status$left_path,
                      right_dir     = sync_status$right_path,
                      files_to_copy = files_to_right,
                      recurse       = recurse)

  # copy those that are new in right to left

  copy_files_to_left(left_dir      = sync_status$left_path,
                     right_dir     = sync_status$right_path,
                     files_to_copy = files_to_left,
                     recurse       = recurse)

  if(verbose == TRUE) {
  # Display folder structure AFTER synchronization
  style_msgs(color_name = "blue",
               text = "Directories structure AFTER synchronization:\n")
  display_dir_tree(path_left  = left_path,
                   path_right = right_path)}

  style_msgs(color_name = "green",
             text = paste0("\u2714", " synchronized\n"))
  invisible(TRUE)

}

