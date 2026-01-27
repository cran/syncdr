#' Display status of synchronization/comparison info between two directories in DT table
#'
#' @param sync_status_files object of `compare_directories()` output, either common_files or non_common_files
#' @param left_path A character string specifying the path to left directory.
#' @param right_path A character string specifying the path to right directory.
#' @return DT table showing the comparison between the two directories
#'         together with their synchronization status
#' @export
display_sync_status <- function(sync_status_files,
                                left_path,
                                right_path) {

  # clean display of paths
  sync_status_files <- sync_status_files |>
    fmutate(path_left = gsub(left_path, "", path_left)) |>
    fmutate(path_right = gsub(right_path, "", path_right))

  # Build DT table
  DT::datatable(sync_status_files,
                options = list(
                  pageLength = 10, # number of rows to display per page
                  columnDefs = list(
                    list(targets = grep("^is_", colnames(sync_status_files), value = TRUE),
                         createdCell = DT::JS(
                           "function(td, cellData, rowData, row, col) {
                            if (cellData === true) {
                              $(td).css({'background-color': '#F8F4FF'});
                            } else {
                              $(td).css({'background-color': '#F0F8FF'});
                            }
                          }"
                         )
                    ),
                    list(targets = grep("sync_status", colnames(sync_status_files), value = TRUE),
                         createdCell = DT::JS(
                           "function(td, cellData, rowData, row, col) {
                             if (cellData.includes('different content') ||
                                 cellData.includes('same date') ||
                                 cellData.includes('only in right')) {
                              $(td).css({'background-color': '#a9def9'});
                            } else {
                              $(td).css({'background-color': '#e4c1f9'});
                            }
                          }"
                         )
                    )
                  )
                )
  )

}


#' Display tree structure of one (or two) directory
#'
#' @param path_left path of left directory
#' @param path_right path of right directory
#' @param recurse logical, default to TRUE: show also sub-directories
#'
#' @return directories tree
#' @export
#' @examples
#' # Create a temporary directory structure
#'
#' \donttest{
#' e <- toy_dirs()
#' left  <- e$left
#' right <- e$right
#'
#' display_dir_tree(
#'   path_left  = left,
#'   path_right = right
#' )
#'
#' display_dir_tree(path_right = right)
#' }
display_dir_tree <- function(path_left  = NULL,
                             path_right = NULL,
                             recurse = TRUE) {

  if (!is.null(path_left)) {
    style_msgs(color_name = "pink",
               text = paste0("(\u2190)", "Left directory structure:\n"))
    fs::dir_tree(path_left)

  }

  if (!is.null(path_right)) {
    style_msgs(color_name = "pink",
               text = paste0("(\u2192)", "Right directory structure:\n"))
    fs::dir_tree(path_right)

  }


  invisible(TRUE)
}



#' Display file actions in table
#'
#' This function displays actions (either "copy" or "delete") to be performed on a list of files.
#'
#' @param path_to_files Data frame containing the paths to the files. The data frame should have a column named "Paths".
#' @param directory Character string specifying path to directory where action is taken
#' @param action Character vector specifying the action to be performed on the files.
#'               Options are "copy" (default) or "delete".
#'
#' @return console-friendly table with files and actions
#' @importFrom utils askYesNo
#' @keywords internal
#'
display_file_actions <- function(path_to_files,
                                 directory,
                                 #target = c("left", "right"),
                                 action = c("copy", "delete")) {

  action <- match.arg(action) |>
    switch("copy" = "To be copied",
           "delete" = "To be deleted")

  path_to_files$Action <- action

  colnames(path_to_files) <- c("Paths", "Action")

  path_to_files <- path_to_files |>
    fmutate(Paths = gsub(directory,
                       "",
                       Paths))

  # Print the table
  print(
    knitr::kable(path_to_files,
                 format = "pipe",
                 col.names = c("Files",
                               "Action")))
}

