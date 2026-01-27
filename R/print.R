#' Print Synchronization Status
#'
#' @param x object of syncdr_status class created in [compare_directories]
#' @param ... additional arguments
#'
#' @return prints syncdr_status object
#' @export
print.syncdr_status <- function(x, ...) {

  # retrieve by date and by content arguments

  by_date    <- fifelse(is.null(x$common_files$is_new_right),
                        FALSE,
                        TRUE)

  by_content <- fifelse(!(is.null(x$common_files$is_diff)),
                        TRUE,
                        FALSE)

  compare_by <- switch(
    paste(by_date, by_content, sep = "-"),
    "TRUE-TRUE" = "date & content",
    "TRUE-FALSE" = "date",
    "FALSE-TRUE" = "content"
  )


  # synchronization summary

  cli::cli_h1("Synchronization Summary")
  cli::cli_li("Left Directory: {.path {x$left_path}}")
  cli::cli_li("Right Directory: {.path {x$right_path}}")
  cli::cli_li("Total Common Files: {.strong {nrow(x$common_files)}}")
  cli::cli_li("Total Non-common Files: {.strong {nrow(x$non_common_files)}}")
  cli::cli_li("Compare files by: {.strong {compare_by}} ")


  ## common files -----------

  cli::cli_h1("Common files")

  x$common_files <- x$common_files |>
    fmutate(path = remove_root(x$left_path, path_left)) |>
    fselect(-c(path_left, path_right))

  if (compare_by == 'date') {
    x$common_files <- x$common_files |>
          fmutate(modified = fcase(is_new_right == TRUE, "right",
                                   is_new_left == TRUE, "left",
                                   default = "same date"))  |>
          fselect(path, modification_time_left, modification_time_right, modified)

  }

  else if (compare_by == "date & content") {
    x$common_files <- x$common_files |>
      fmutate(modified = fcase(is_new_right == TRUE, "right",
                               is_new_left == TRUE, "left",
                               default = "same date"))  |>
      fselect(path, modification_time_left, modification_time_right, modified, sync_status)

  } else {
    x$common_files <- x$common_files |>
          fselect(path, sync_status)

  }

  print(x$common_files)

  ## non-common files -----------
  cli::cli_h1("Non-common files")

  ncf <- x$non_common_files |>
    fmutate(path_left = remove_root(x$left_path, path_left)) |>
    fmutate(path_right = remove_root(x$right_path, path_right))

  cli::cli_h2("Only in left")
  ncf |>
    fselect(path_left) |>
    fsubset(!is.na(path_left)) |>
    print()

  cat("\n")

  cli::cli_h2("Only in right")
  ncf |>
    fselect(path_right) |>
    fsubset(!is.na(path_right)) |>
    print()

  invisible(x)
}

remove_root <- \(root_path, new_path) {
  gsub(fs::path_dir(root_path), "", new_path)
}
