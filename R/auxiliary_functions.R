#' Filter common files in a syncdr_status object based on specified criteria
#'
#' This function filters common files within a "syncdr_status" object, which is the result of 'compare_directories()',
#' according to the specified filtering criteria:
#' Filtering is dependent on the 'dir' argument, determining the primary directory for comparison
#'
#' Filtering Options:
#' * by_date: Filters files that are new in the specified primary directory ('left', 'right', or both).
#' * by_date_and_content: Filters files that are either new or different in the specified primary directory ('left', 'right', or both).
#' * by_content_only: Filters files that are different between the two directories.
#'
#' @param sync_status An object of class 'syncdr_status' containing synchronization status and directory comparison results (common files only).
#' @param by_date Logical; if TRUE, filters based on new files in the specified directory. Default is TRUE.
#' @param by_content Logical; if TRUE, filters based on new or different files in the specified directory. Default is FALSE.
#' @param dir Character vector specifying the primary directory for comparison ('left', 'right', or 'all').
#' @return A 'syncdr_status' object filtered according to the specified criteria.
#' @keywords internal
#'
#' @seealso
#' \code{\link{compare_directories}} for directory comparison and sync status creation.
filter_common_files <- function(sync_status,
                                by_date    = TRUE,
                                by_content = FALSE,
                                dir        = "left") {

  # Check argument
  stopifnot(
    dir %in% c("left", "right", "all")
  )

  # Define date filter based on arguments
  date_filter <- if (by_date) {

    if (dir == "left") {
      sync_status$is_new_left
    } else if (dir == "right") {
      sync_status$is_new_right
    } else if(dir == "all") {
      sync_status$is_new_left | sync_status$is_new_right
    }
    } else {
    TRUE  # If by_date is false, include all dates
  }

  # Define content filter based on arguments
  content_filter <- if (by_content) {
    sync_status$is_diff
  } else {
    TRUE
    }

  # Filter sync_status accordingly
  sync_status <- sync_status |>
    fsubset(date_filter & content_filter) |>
    fselect(path_left,
            path_right,
            sync_status)

  return(sync_status)
}

#' Filter files in a syncdr_status object that are NOT common between two directories compared
#'
#' This function filters files that are not common between the directories compared
#' in the 'sync_status' object resulting from 'compare_directories()'.
#'
#' @param sync_status An object of class 'syncdr_status' containing information
#'                    about synchronization status and directory comparisons.
#' @param dir Character string specifying the directory to filter:
#'            - "left" for files unique to the left directory
#'            - "right" for files unique to the right directory
#'            - "all" for files unique to either directory
#' @return An updated 'syncdr_status' object with filtered files according to the specified criteria.
#' @keywords internal
filter_non_common_files <- function(sync_status,
                                    dir = "left") {

  stopifnot(expr = {
    dir %in% c("left", "right" , "all")
  })

  if (dir == "left") {

    sync_status <- sync_status |>
      fsubset(!is.na(path_left)) |>
      fselect(path_left, path_right, sync_status)

  } else if (dir == "right") {

    sync_status <- sync_status |>
      fsubset(!is.na(path_right)) |>
      fselect(path_left, path_right, sync_status)

  } else {sync_status <- sync_status |>
    fselect(path_left, path_right, sync_status)}

  return(sync_status)

}


#' Retrieve information about files in a directory
#'
#' This function retrieves information about files in a specified directory
#'
#' @param dir A character string representing the path of the directory.
#' @param recurse Logical. If TRUE, fully recurse into subdirectories. If a positive number,
#'               specifies the number of levels to recurse.
#' @return A data frame containing detailed information about the files in the directory,
#'         including all information produced by `fs::file_info()`.
#'
#' @keywords internal
#'
directory_info <- function(dir,
                           recurse = TRUE) {

  # List of files -also in sub-directories
  files <- fs::dir_ls(path = dir,
                      type = "file",
                      recurse = recurse)

  # Filtering out special files
  files <- files[!grepl("^\\.\\.$|^\\.$", files)]

  # Get all dir info available in file_info
  info_df <- fs::file_info(files) |>
    ftransform(wo_root = gsub(dir, "", path),
               modification_time = as.POSIXct(modification_time)) #add without root var

  return(info_df)

}

#' Compare modification times of two files and determine synchronization status
#'
#' This function compares the date of last modification of two files and determines
#' their synchronization status
#'
#' @param modification_time_left modification time of the file in the left directory
#' @param modification_time_right modification time of the file in the right directory
#' @return A list containing the following components:
#'   \item{is_new_left}{Logical. Indicates if the file in the left directory is newer
#'                      (i.e., has a later modification time) than the file in the right directory}
#'   \item{is_new_right}{Logical. Indicates if the file in the right directory is newer
#'                       (i.e., has a later modification time) than the file in the left directory}
#'   \item{sync_status_date}{Character. Describes the synchronization status between the
#'                            two files based on their modification times:
#'                            - "newer in left, older in right dir": Left file is newer than right file.
#'                            - "older in left, newer in right dir": Right file is newer than left file.
#'                            - "same date": Both files have the same modification time.}
#'
#' @keywords internal
#'
compare_modification_times <- function(modification_time_left,
                                       modification_time_right) {

  is_new_left  <- modification_time_left  > modification_time_right
  is_new_right <- modification_time_right > modification_time_left

  sync_status_date <- ifelse(
    is_new_left & !is_new_right, "newer in left, older in right dir",
    ifelse(!is_new_left & is_new_right, "older in left, newer in right dir", "same date")
  )

  return(list(
    is_new_left      = is_new_left,
    is_new_right     = is_new_right,
    sync_status_date = sync_status_date)
    )

}

#' Compare contents of two files and determine synchronization status
#'
#' This function compares the contents of two files located at specified paths
#' and determines their synchronization status based on their content
#'
#' @param path_left A character string specifying the path to the file in the left directory.
#' @param path_right A character string specifying the path to the file in the right directory.
#' @param verbose logical; if TRUE display progress status of hashing files' contents, in seconds. Default is FALSE
#' @return A list containing the following components:
#'   \item{is_diff}{Logical. Indicates whether the contents of the two files are different (`TRUE`)
#'                 or identical (`FALSE`).}
#'   \item{sync_status_content}{Character. Describes the synchronization status between the
#'                              two files based on their content:
#'                              - "different content": Contents of the files are not identical.
#'                              - "same content": Contents of the files are identical.}
#'
#' @keywords internal
#'
compare_file_contents <- function(path_left,
                                  path_right,
                                  verbose    = getOption("syncdr.verbose")) {

  hash_left  <- hash_files(path_left,
                           verbose = verbose)
  hash_right <- hash_files(path_right,
                           verbose = verbose)

  is_diff <- (hash_left != hash_right)

  sync_status_content <- ifelse(
    is_diff, "different content",
    "same content"
  )
  return(list(is_diff             = is_diff,
              sync_status_content = sync_status_content))
}

#' Hash files by content
#' @param files_path character vector of paths of files to hash
#' @param verbose logical; if TRUE display progress status of hashing. Default is FALSE
#' @return hashes of files
#' @keywords internal
hash_files <- function(files_path,
                       verbose    = getOption("syncdr.verbose")) {

  if (verbose) {
    # Initialize progress bars
    pb <- cli::cli_progress_bar("Hashing files -by content",
                                total = length(files_path))
    # Start timing
    start_time <- Sys.time()
  }

  # Compute hash for files
  hashes <- lapply(files_path, function(path) {

    hash <- secretbase::siphash13(file = path)

    if (verbose) {
      cli::cli_progress_step(pb,
                             msg_done = {basename(path)},
                             spinner  = TRUE)
    }
    hash
  })

  if (verbose) {

    # end cli progress
    cli::cli_progress_done(pb)

    # end timing & display it
    end_time   <- Sys.time()
    total_time <- format(end_time - start_time,
                         units = "secs")
    cli::cli_h2("Hashing completed! Total time spent: {.emph {total_time}}")

  }

  return(unlist(hashes))
}

#' Hash files in a directory based on content
#'
#' This function calculates hashes for files in a specified directory based on their content.
#'
#' @param dir_path A character string of the path to the directory containing files
#'                 for which hashes will be calculated
#' @return A data frame containing file paths and their corresponding SHA-256 hashes.
#'
#' @importFrom fs dir_ls
#' @importFrom digest digest
#' @keywords internal
hash_files_in_dir <- function(dir_path) {

  dir_files <- fs::dir_ls(dir_path, type = "file", recurse = TRUE)

  # Create a data frame with file paths
  files_df <- data.frame(path = dir_files)

  # Calculate hashes for each file path
  files_df$hash <- lapply(files_df$path, function(p) {
    digest::digest(p, algo = "xxhash32", file = TRUE)
  })

  return(files_df)
}

#' Search for duplicate files in a directory
#'
#' This function searches for duplicate files within a directory based on their content.
#' Duplicate files are identified by having either the same filename and same content
#' or different filenames but same content.
#'
#' @param dir_path A character string representing the path to the directory to search for duplicates
#' @param verbose Logical. If TRUE, displays a list of duplicate files found (default is TRUE)
#' @return A data frame containing information about duplicate files (invisible by default)
#'
#' @importFrom fs dir_ls
#' @importFrom cli cli_h1 cli_text cli_alert_success
#'
#' @export
#' @examples
#' # Search for duplicate files in a directory
#'
#' \donttest{
#' e <- toy_dirs()
#' search_duplicates(dir_path = e$left)
#' }
search_duplicates <- function(dir_path,
                              verbose = TRUE) {

  # check path exists
  stopifnot(exprs = {
    fs::dir_exists(dir_path)
  })

  # Hash files contents
  file_hashes <- hash_files_in_dir(dir_path)

  duplicates <- duplicated(file_hashes$hash) |
    duplicated(file_hashes$hash, fromLast = TRUE)

  # Step 2: Filter the dataframe to keep only rows with duplicated hashes
  filtered_files <- file_hashes[duplicates, ]

  if (verbose) {

    cli::cli_h1("Duplicates in {.path {dir_path}}")

    # add here paths of files found in filtered files
    lapply(filtered_files$path, function(file_path) {
      #cli::cli_text(basename(file_path))
      cli::cli_text(paste0("*",
                           gsub(dir_path, "", file_path)))
    })
  }

  else {style_msgs(color_name = "green",
                   text = "identification of duplicates completed!")}

  invisible(filtered_files)
}



#' Save sync_status file
#' @param dir_path path to directory
#' @return the file is saved in a `_syncdr` subdirectory within the specified directory
#' @examples
#' \donttest{
#' # Set the directory path
#' e = toy_dirs()
#' left <- e$left
#' 
#' save_sync_status(dir_path = left)
#' }
#' @export
#'
save_sync_status <- function(dir_path) {

  hashes           <- hash_files_in_dir(dir_path)
  rownames(hashes) <- NULL
  hashes$hash <- as.character(hashes$hash)

  dates            <- directory_info(dir_path) |>
    fselect(path, modification_time)

  sync_status_table <- joyn::joyn(hashes,
                                 dates,
                                 by        = "path",
                                 reportvar = FALSE)


  # Create subdirectory "_syncdr" if it doesn't exist
  syncdr_path <- file.path(dir_path, "_syncdr")
  if (!dir.exists(syncdr_path)) {
    dir.create(syncdr_path)
  }

  # Get save format

  format <- getOption("syncdr.save_format")

  format <-
    if (format == "fst") {
      ifelse(requireNamespace("fst",
                              quietly = TRUE),
             'fst', 'Rds')
    } else if (format == "csv") {
      ifelse(requireNamespace("data.table", quietly = TRUE),
             'csv', 'Rds')
    } else {
      style_msgs(color_name = "orange",
                 text = "{syncdr} Save_format option raised an error")
      cli::cli_abort("{.field saving in {format}} format not allowed")
    }

  # Generate filename with current system time
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_name <- paste0("_SyncStatus_",
                      timestamp, ".", format)

  file_path <- file.path(syncdr_path,
                         file_name)


  save_fun <- switch (format,
                     "fst" = fst::write_fst(x    = sync_status_table,
                                            path = file_path),
                     "csv" = fwrite(x    = sync_status_table,
                                    file = file_path),
                     "rds" = saveRDS(object = sync_status_table,
                                     file   = file_path)
  )

  return(file_path)
}
