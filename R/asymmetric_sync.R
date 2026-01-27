#' Full asymmetric synchronization to right directory
#'
#' This function performs a full asymmetric synchronization of the right directory
#' based on the left directory. It includes the following synchronization steps (see Details below):
#'
#' * For common files:
#'   - If comparing by date only (`by_date = TRUE`): Copy files that are newer in the left directory to the right directory.
#'   - If comparing by date and content (`by_date = TRUE` and `by_content = TRUE`): Copy files that are newer and different in the left directory to the right directory.
#'   - If comparing by content only (`by_content = TRUE`): Copy files that are different in the left directory to the right directory.
#' * Copy to the right directory those files that exist only in the left directory.
#' * Delete from the right directory those files that are exclusive in the right directory (i.e., missing in the left directory)
#'
#' @param left_path Path to the left/first directory.
#' @param right_path Path to the right/second directory.
#' @param sync_status Object of class "syncdr_status", output of `compare_directories()`.
#' @param by_date Logical. If TRUE, synchronize based on file modification dates (default is TRUE).
#' @param by_content Logical. If TRUE, synchronize based on file contents (default is FALSE).
#' @param recurse Logical. If TRUE (default), files are copied to corresponding subdirectories
#'                in the destination folder. If FALSE, files are copied to the top level of the destination folder
#'                without creating subdirectories if they do not exist.
#' @param delete_in_right Logical. If TRUE (default), files that exist only in the
#'        right directory (i.e., absent from the left directory) are deleted during
#'        synchronization. If FALSE, no files are removed from the right directory,
#'        even if they are exclusive to it.
#' @param force Logical. If TRUE (by default), directly perform synchronization of the directories.
#'                        If FALSE, Displays a preview of actions and prompts the user for confirmation before proceeding. Synchronization is aborted if the user does not agree.
#' @param backup Logical. If TRUE, creates a backup of the right directory before synchronization. The backup is stored in the location specified by `backup_dir`.
#' @param backup_dir Path to the directory where the backup of the original right directory will be stored. If not specified, the backup is stored in temporary directory (`tempdir`).
#' @param verbose logical. If TRUE, display directory tree before and after synchronization. Default is FALSE
#' @return Invisible TRUE indicating successful synchronization.
#'
#' @export
#' @examples
#' \donttest{
#' e <- toy_dirs(fast = TRUE)
#' left  <- e$left
#' right <- e$right
#' full_asym_sync_to_right(
#'   left_path  = left,
#'   right_path = right,
#'   by_date    = FALSE,
#'   by_content = TRUE
#' )
#' sync_status <- compare_directories(left_path = left, right_path = right)
#' full_asym_sync_to_right(sync_status = sync_status)
#' }
full_asym_sync_to_right <- function(left_path       = NULL,
                                    right_path      = NULL,
                                    sync_status     = NULL,
                                    by_date         = TRUE,
                                    by_content      = FALSE,
                                    recurse         = TRUE,
                                    force           = TRUE,
                                    delete_in_right = TRUE,
                                    backup          = FALSE,
                                    backup_dir      = "temp_dir",
                                    verbose         = getOption("syncdr.verbose")) {


  # Display folder structure before synchronization
  if (verbose == TRUE) {

    style_msgs(color_name = "blue",
               text = "Directories structure BEFORE synchronization:\n")

    display_dir_tree(path_left  = left_path,
                     path_right = right_path)
  }

  # --- Check validity of arguments ----

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


  # If sync_status is null, but left and right paths are provided
  # get sync_status object -internal call to compare_directories()

  if(is.null(sync_status)) {

    # --- first check directories path ---
    stopifnot(exprs = {
      fs::dir_exists(left_path)
      fs::dir_exists(right_path)
    })

    # --- Get sync_status ----
    sync_status <- compare_directories(left_path  = left_path,
                                       right_path = right_path,
                                       by_date    = by_date,
                                       by_content = by_content,
                                       recurse    = recurse,
                                       verbose    = FALSE
    )
  } else {

    # If sync_status is already provided,
    # retrieve paths of left and right directory as well as by_date and by_content arguments

    left_path  <- sync_status$left_path
    right_path <- sync_status$right_path

    by_date    <- fifelse(is.null(sync_status$common_files$is_new_right),
                           FALSE,
                           by_date)
    by_content <- fifelse(!(is.null(sync_status$common_files$is_diff)),
                          TRUE,
                          by_content)

  }

  # --- Backup ----

  # Copy right directory in backup directory
  if (backup) {
    backup_dir <- ifelse(backup_dir == "temp_dir", # the default

                          #tempdir(),
                          file.path(tempdir(),
                                    "backup_directory"),
                          backup_dir) # path provided by the user

    # create the target directory if it does not exist
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }


    # copy dir content
    file.copy(from      = right_path,
              to        = backup_dir,
              recursive = TRUE)


  }

  # --- Identify files to copy/delete/move ----

  # files to copy  -from common files
  files_to_copy <- sync_status$common_files |>
    filter_common_files(by_date    = by_date,
                        by_content = by_content,
                        dir = "left") #syncdr aux function

  # files to copy  -from non common files
  files_to_copy <- files_to_copy |>
    rowbind(
      filter_non_common_files(sync_status$non_common_files,
                              dir = "left")
    ) # files only in left

  # files to delete, i.e., missing in left
  files_to_delete <- sync_status$non_common_files |>
    filter_non_common_files(dir = "right") |>
    fselect(path_right)

  # --- Force option ----

  if (force == FALSE) {

    if (nrow(files_to_delete) > 0 ) {
      style_msgs("orange",
                 text = "These files will be DELETED in right")

      display_file_actions(path_to_files = files_to_delete,
                           directory     = right_path,
                           action        = "delete"
      )
    }


    if (nrow(files_to_copy) >0 ) {
      style_msgs("blue",
                 text = "These files will be COPIED (overwriting if present) to right \n")
      display_file_actions(path_to_files = files_to_copy |> fselect(1),
                           directory     = left_path,
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

  # --- Synchronization ----


  ## Copy files ####
  copy_files_to_right(left_dir      = sync_status$left_path,
                      right_dir     = sync_status$right_path,
                      files_to_copy = files_to_copy,
                      recurse       = recurse)


  ## Delete Files
  if (delete_in_right == TRUE) {
    if (NROW(files_to_delete) > 0) {
      invisible(
        lapply(
          cli::cli_progress_along(
            files_to_delete$path_right, name = "Deleting files"
          ),
          function(i) fs::file_delete(files_to_delete$path_right[i])
        )
      )
    } else if (verbose) {
      cli::cli_alert_info("No files deleted (all excluded or none to delete).")
    }
  }


  if(verbose == TRUE) {

    style_msgs(color_name = "blue",
               text = "Directories structure AFTER synchronization:\n")
    display_dir_tree(path_left  = left_path,
                     path_right = right_path)

  }

  style_msgs(color_name = "green",
             text = paste0("\u2714", " synchronized\n"))

  invisible(TRUE)

}


#' Partial asymmetric synchronization to right (update common files)
#'
#' Partially synchronize right directory based on left one -i.e., the function will:
#' * for common_files:
#'    - if by date only: copy files that are newer in left to right
#'    - if by date and content: copy files that are newer and different in left to right
#'    - if by content only: copy files that are different in left to right
#' * for non common files, nothing changes: i.e.,
#'    - disregard those files that are only in left
#'    - keep in right those files that are only in right (i.e., files 'missing in left')
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
#'                       If FALSE, Displays a preview of actions and prompts the user for confirmation before proceeding. Synchronization is aborted if the user does not agree.
#' @param backup Logical. If TRUE, creates a backup of the right directory before synchronization. The backup is stored in the location specified by `backup_dir`.
#' @param backup_dir Path to the directory where the backup of the original right directory will be stored. If not specified, the backup is stored in temporary directory (`tempdir`).
#' @param verbose logical. If TRUE, display directory tree before and after synchronization. Default is FALSE
#' @return Invisible TRUE indicating successful synchronization.
#' @export
#' @examples
#' # Asymmetric synchronization of common files
#'
#' \donttest{
#' e <- toy_dirs()
#' left  <- e$left
#' right <- e$right
#' # Synchronize common files by content only
#' common_files_asym_sync_to_right(
#'   left_path  = left,
#'   right_path = right,
#'   by_date    = FALSE,
#'   by_content = TRUE
#' )
#' }
common_files_asym_sync_to_right <- function(left_path   = NULL,
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
  # Display folder structure before synchronization
  style_msgs(color_name = "blue",
             text = "Directories structure BEFORE synchronization:\n")
  display_dir_tree(path_left  = left_path,
                   path_right = right_path)}

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

  if (is.null(sync_status)) {

    # --- get sync_status ---
    sync_status <- compare_directories(left_path  = left_path,
                                       right_path = right_path,
                                       by_date    = by_date,
                                       by_content = by_content,
                                       recurse    = recurse,
                                       verbose    = FALSE
    )
  } else {

    # If sync_status is already provided, retrieve by_date and by_content arguments from it

    left_path  <- sync_status$left_path
    right_path <- sync_status$right_path

    by_date    <- fifelse(is.null(sync_status$common_files$is_new_right),
                          FALSE,
                          by_date)

    by_content <- fifelse(!(is.null(sync_status$common_files$is_diff)),
                          TRUE,
                          by_content)

  }

  # Identify files to copy -from common files ####
  files_to_copy <- sync_status$common_files |>
    filter_common_files(by_date    = by_date,
                        by_content = by_content,
                        dir = "left")


  # --- Backup ----

  # Copy right directory in backup directory
  if (backup) {
    backup_dir <- fifelse(backup_dir == "temp_dir", # the default

                          #tempdir(),
                          file.path(tempdir(),
                                    "backup_directory"),
                          backup_dir) # path provided by the user

    # create the target directory if it does not exist
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }


    # copy dir content
    file.copy(from      = right_path,
              to        = backup_dir,
              recursive = TRUE)


  }

  # --- Force option ----

  if (force == FALSE) {

    if (nrow(files_to_copy) > 0 ) {
      style_msgs("blue",
                 text = "These files will be COPIED (overwriting if present) from left to right \n")
      display_file_actions(path_to_files = files_to_copy |>
                             fselect(1),
                           directory     = left_path,
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

  # --- Synchronization ----


  ## Copy files ####
  copy_files_to_right(left_dir      = sync_status$left_path,
                      right_dir     = sync_status$right_path,
                      files_to_copy = files_to_copy,
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

#' Full asymmetric synchronization of non common files
#'
#' update non common files in right directory based on left one -i.e., the function will:
#' * for common_files:
#'    - do nothing, left unchanged
#' * for non common files,
#'    - copy those files that are only in left to right
#'    - delete in right those files that are only in right (i.e., files 'missing in left')
#'
#' @param left_path Path to the left/first directory.
#' @param right_path Path to the right/second directory.
#' @param sync_status Object of class "syncdr_status", output of `compare_directories()`.
#' @param recurse logical, TRUE by default.
#'  If recurse is TRUE: when copying a file from source folder to destination folder, the file will be copied into the corresponding (sub)directory.
#'  If the sub(directory) where the file is located does not exist in destination folder (or you are not sure), set recurse to FALSE,
#'  and the file will be copied at the top level
#' @param copy_to_right Logical, default is TRUE.
#'   If TRUE, files that exist only in the left directory are copied to the right directory.
#'   If FALSE, such files are not copied and remain absent from the right directory.
#'
#' @param delete_in_right Logical, default is TRUE.
#'   If TRUE, files that exist only in the right directory (i.e., not present in the left) are deleted.
#'   If FALSE, these right-only files are preserved.
#' @param exclude_delete Character vector of file names or dir names to protect from deletion.
#'   These files will be kept in the right directory even if `delete = TRUE`.
#' @param force Logical. If TRUE (by default), directly perform synchronization of the directories.
#'                       If FALSE, Displays a preview of actions and prompts the user for confirmation before proceeding. Synchronization is aborted if the user does not agree.
#
#' @param backup Logical. If TRUE, creates a backup of the right directory before synchronization. The backup is stored in the location specified by `backup_dir`.
#' @param backup_dir Path to the directory where the backup of the original right directory will be stored. If not specified, the backup is stored in temporary directory (`tempdir`).
#' @param verbose logical. If TRUE, display directory tree before and after synchronization. Default is FALSE
#' @return Invisible TRUE indicating successful synchronization.
#' @export
#' @examples
#' # Create a temporary synchronization environment
#' \donttest{
#' e <- toy_dirs()
#' left  <- e$left
#' right <- e$right
#'
#' # Update missing files asymmetrically (left → right)
#' # Option 1: provide left and right paths
#' update_missing_files_asym_to_right(
#'   left_path  = left,
#'   right_path = right
#' )
#'
#' # Option 2: provide a precomputed sync_status object
#' sync_status <- compare_directories(
#'   left_path  = left,
#'   right_path = right
#' )
#' update_missing_files_asym_to_right(sync_status = sync_status)
#' }
update_missing_files_asym_to_right <- function(left_path   = NULL,
                                               right_path  = NULL,
                                               sync_status = NULL,
                                               recurse     = TRUE,
                                               force       = TRUE,
                                               backup      = FALSE,
                                               backup_dir  = "temp_dir",
                                               copy_to_right = TRUE,
                                               delete_in_right = TRUE,
                                               exclude_delete = NULL,
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
                                       recurse    = recurse,
                                       verbose    = FALSE
    )
  } else {
    left_path  <- sync_status$left_path
    right_path <- sync_status$right_path
  }

  # Identify files to copy/delete ####
  files_to_copy <- sync_status$non_common_files |>
    filter_non_common_files(dir = "left")

  # Get files to delete
  files_to_delete <- sync_status$non_common_files |>
    filter_non_common_files(dir = "right") |>
    fselect(path_right)

  if (is.null(files_to_delete)) {
    files_to_delete <- data.frame(path_right = character())
  }

  if (!is.data.frame(files_to_delete)) {
    files_to_delete <- data.frame(path_right = as.character(files_to_delete))
  }

  # --- Backup ----

  # Copy right directory in backup directory
  if (backup) {
    backup_dir <- ifelse(backup_dir == "temp_dir", # the default

                          #tempdir(),
                          file.path(tempdir(),
                                    "backup_directory"),
                          backup_dir) # path provided by the user

    # create the target directory if it does not exist
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }

    # copy dir content
    file.copy(from      = right_path,
              to        = backup_dir,
              recursive = TRUE)


  }

  # Select files to delete
  if (delete_in_right == TRUE) {

    # Validate exclude_delete
    if (!is.null(exclude_delete)) {
      if (!is.character(exclude_delete)) {
        stop("'exclude_delete' must be a character vector or NULL")
      }
      if (length(exclude_delete) == 0) {
        exclude_delete <- NULL  # treat empty character vector as NULL
      }
    }

    if (!is.null(exclude_delete)) {

      keep_idx <- vapply(files_to_delete$path_right, function(p) {
        fname <- basename(p)
        path_parts <- strsplit(fs::path_norm(p), .Platform$file.sep)[[1]]
        any(exclude_delete %in% fname) || any(exclude_delete %in% path_parts)
      }, logical(1))

      if (any(keep_idx)) {
        files_to_delete <- files_to_delete[!keep_idx, , drop = FALSE]
      }
    }

  }


  # --- Force option ----

  if (force == FALSE) {

    if (nrow(files_to_delete) > 0 ) {
      style_msgs("orange",
                 text = "These files will be DELETED in right if delete is TRUE")

      display_file_actions(path_to_files = files_to_delete,
                           directory     = right_path,
                           action        = "delete"
      )
    }


    if (copy_to_right == TRUE && nrow(files_to_copy) >0 ) {
      style_msgs("blue",
                 text = "These files will be COPIED (overwriting if present) to right \n")
      display_file_actions(path_to_files = files_to_copy |> fselect(1),
                           directory     = left_path,
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

  # --- Synchronization ----

  ## Copy files ####

  if (copy_to_right == TRUE) {
    copy_files_to_right(left_dir      = sync_status$left_path,
                        right_dir     = sync_status$right_path,
                        files_to_copy = files_to_copy,
                        recurse       = recurse)
  } else {
    if (verbose) cli::cli_alert_info("Non common files to copy skipped")
  }

  ## Delete Files
  if (delete_in_right == TRUE) {
    if (NROW(files_to_delete) > 0) {
      invisible(
        lapply(
          cli::cli_progress_along(
            files_to_delete$path_right, name = "Deleting files"
            #format = "Deleting files [:bar] :current/:total (:percent)"
          ),
          function(i) fs::file_delete(files_to_delete$path_right[i])
        )
      )
    } else if (verbose) {
      cli::cli_alert_info("No files deleted (all excluded or none to delete).")
    }
  }



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

#' Partial asymmetric asymmetric synchronization of non common files
#'
#' update non common files in right directory based on left one -i.e., the function will:
#' * for common_files:
#'    - do nothing, left unchanged
#' * for non common files,
#'    - copy those files that are only in left to right
#'    - keep in right those files that are only in right (i.e., files 'missing in left')
#' @param left_path Path to the left/first directory.
#' @param right_path Path to the right/second directory.
#' @param sync_status Object of class "syncdr_status", output of `compare_directories()`.
#' @param recurse logical, TRUE by default.
#'  If recurse is TRUE: when copying a file from source folder to destination folder, the file will be copied into the corresponding (sub)directory.
#'  If the sub(directory) where the file is located does not exist in destination folder (or you are not sure), set recurse to FALSE,
#'  and the file will be copied at the top level
#' @param verbose logical. If TRUE, display directory tree before and after synchronization. Default is FALSE
#' @param force Logical. If TRUE (by default), directly perform synchronization of the directories.
#'                       If FALSE, Displays a preview of actions and prompts the user for confirmation before proceeding. Synchronization is aborted if the user does not agree.
#' @param backup Logical. If TRUE, creates a backup of the right directory before synchronization. The backup is stored in the location specified by `backup_dir`.
#' @param backup_dir Path to the directory where the backup of the original right directory will be stored. If not specified, the backup is stored in temporary directory (`tempdir`).
#' @return Invisible TRUE indicating successful synchronization.
#' @export
#' @examples
#' # Create a temporary synchronization environment
#' \donttest{
#' e <- toy_dirs()
#' left  <- e$left
#' right <- e$right
#'
#' # Partially update missing files asymmetrically (left → right)
#' # Option 1: provide left and right paths
#' partial_update_missing_files_asym_to_right(
#'   left_path  = left,
#'   right_path = right
#' )
#'
#' # Option 2: provide a precomputed sync_status object
#' sync_status <- compare_directories(
#'   left_path  = left,
#'   right_path = right
#' )
#' partial_update_missing_files_asym_to_right(sync_status = sync_status)
#' }
partial_update_missing_files_asym_to_right <- function(left_path   = NULL,
                                                       right_path  = NULL,
                                                       sync_status = NULL,
                                                       recurse     = TRUE,
                                                       force       = TRUE,
                                                       backup      = FALSE,
                                                       backup_dir  = "temp_dir",
                                                       verbose     = getOption("syncdr.verbose")) {


  if(verbose == TRUE) {
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
                                       recurse    = recurse,
                                       verbose    = FALSE
    )
  } else {
    left_path  <- sync_status$left_path
    right_path <- sync_status$right_path
  }

  # Identify files to copy/delete ####
  files_to_copy <- sync_status$non_common_files |>
    filter_non_common_files(dir = "left")

  # --- Force option ----

  if (force == FALSE) {

    if (nrow(files_to_copy) > 0 ) {
      style_msgs("blue",
                 text = "These files will be COPIED (overwriting if present) to right \n")
      display_file_actions(path_to_files = files_to_copy |>
                             fselect(1),
                           directory     = left_path,
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

  # Copy right directory in backup directory
  if (backup) {
    backup_dir <- fifelse(backup_dir == "temp_dir", # the default

                          #tempdir(),
                          file.path(tempdir(),
                                    "backup_directory"),
                          backup_dir) # path provided by the user

    # create the target directory if it does not exist
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }

    # copy dir content
    file.copy(from      = right_path,
              to        = backup_dir,
              recursive = TRUE)


  }

  # --- Synchronization ----


  ## Copy files ####
  copy_files_to_right(left_dir      = sync_status$left_path,
                      right_dir     = sync_status$right_path,
                      files_to_copy = files_to_copy,
                      recurse       = recurse)

  if(verbose == TRUE) {
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


