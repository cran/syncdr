#' Check sync_status for staleness
#'
#' Issues a `cli::cli_warn()` when the `sync_status` object was created more
#' than `getOption("syncdr.staleness_threshold_secs", 3600L)` seconds ago.
#' Silently does nothing if `sync_status$created_at` is absent (e.g., objects
#' built before this field was introduced).
#'
#' @param sync_status A `syncdr_status` object.
#' @return Invisibly returns `sync_status`.
#' @keywords internal
check_sync_status_staleness <- function(sync_status) {
  if (is.null(sync_status$created_at)) return(invisible(sync_status))
  threshold <- getOption("syncdr.staleness_threshold_secs", 3600L)
  age_secs  <- as.numeric(
    difftime(Sys.time(), sync_status$created_at, units = "secs")
  )
  if (age_secs > threshold) {
    cli::cli_warn(c(
      "The {.arg sync_status} object is {.val {round(age_secs / 60, 1)}} minute(s) old.",
      "i" = "Re-run {.fn compare_directories} to ensure results are current.",
      "i" = "Threshold: {threshold} seconds. Adjust via {.code options(syncdr.staleness_threshold_secs = ...)}."
    ))
  }
  invisible(sync_status)
}


#' Perform a timestamped directory backup
#'
#' Creates a timestamped backup of `source_dir` inside `backup_dir`. Each call
#' produces a unique subdirectory named `<label>_<YYYYMMDD_HHMMSS>` so
#' repeated syncs never overwrite an earlier backup (VUL-21). When the default
#' `"temp_dir"` sentinel is used, the backup lands in `tempdir()` and a warning
#' is emitted to remind the user that the backup is ephemeral (VUL-20). The
#' return value of `file.copy()` is checked; if any file failed to copy the
#' function aborts with an informative error rather than proceeding to
#' destructive operations (VUL-17).
#'
#' @param source_dir  Path to the directory to back up.
#' @param backup_dir  Destination root: either the user-supplied path or the
#'   sentinel `"temp_dir"` (the package default) which resolves to
#'   `tempdir()`.
#' @param label       A short string identifying the backup ("right", "left",
#'   etc.).  Becomes the prefix of the timestamped subdirectory name.
#' @return Invisibly returns the path of the backup directory that was
#'   created.
#' @keywords internal
perform_backup <- function(source_dir, backup_dir, label = "backup") {

  # VUL-20: warn when the default ephemeral tempdir is used
  using_tempdir <- identical(as.character(backup_dir), "temp_dir")
  if (using_tempdir) {
    cli::cli_warn(c(
      "Backup stored in {.fn tempdir} - this location is ephemeral.",
      "i" = "The backup will be lost when the R session ends.",
      "i" = "Supply a permanent {.arg backup_dir} to keep a persistent backup."
    ))
    backup_root <- tempdir()
  } else {
    backup_root <- as.character(backup_dir)
  }

  # VUL-21: stamp each backup so repeated syncs don't clobber the previous one
  timestamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
  target_dir <- file.path(backup_root, paste0(label, "_", timestamp))

  if (!dir.exists(target_dir)) {
    ok <- dir.create(target_dir, recursive = TRUE)
    if (!ok) {
      cli::cli_abort(c(
        "Could not create backup directory {.path {target_dir}}.",
        "x" = "Check that {.path {backup_root}} is writable.",
        "i" = "Sync aborted - no files have been modified."
      ))
    }
  }

  # VUL-17: verify every file copied successfully before returning
  results <- file.copy(from      = source_dir,
                       to        = target_dir,
                       recursive = TRUE)

  if (!all(results)) {
    n_failed <- sum(!results)
    cli::cli_abort(c(
      "Backup of {.path {source_dir}} to {.path {target_dir}} failed for {n_failed} item(s).",
      "x" = "Backup may be incomplete - aborting sync to protect your data.",
      "i" = "Check disk space and write permissions on {.path {backup_root}}."
    ))
  }

  invisible(target_dir)
}


#' Validate a sync_status argument
#'
#' Checks that a `sync_status` argument is an object of class `"syncdr_status"`.
#' Throws an informative `cli::cli_abort()` if not.
#'
#' @param sync_status The value to validate.
#' @return Invisibly returns `sync_status` if the check passes.
#' @keywords internal
validate_sync_status_arg <- function(sync_status) {
  if (!inherits(sync_status, "syncdr_status")) {
    cli::cli_abort(
      c(
        "{.arg sync_status} must be an object of class {.cls syncdr_status}.",
        "x" = "Got an object of class {.cls {class(sync_status)}}.",
        "i" = "Create one with {.fn compare_directories}."
      )
    )
  }
  invisible(sync_status)
}


#' Validate a backup_dir argument against the sync directories
#'
#' Checks that `backup_dir` (when user-supplied) is not identical to, or
#' nested inside, `left_path` or `right_path`.
#'
#' @param backup_dir The backup directory path supplied by the user (or
#'   `"temp_dir"` sentinel which is skipped).
#' @param left_path  Absolute path of the left sync directory.
#' @param right_path Absolute path of the right sync directory.
#' @return Invisibly returns `backup_dir` if all checks pass.
#' @keywords internal
validate_backup_dir <- function(backup_dir, left_path, right_path) {
  # "temp_dir" is the internal sentinel meaning tempdir(); skip validation
  if (identical(backup_dir, "temp_dir")) return(invisible(backup_dir))

  # cast to plain character so fs_path / path objects compare correctly
  b <- as.character(fs::path_norm(backup_dir))
  l <- as.character(fs::path_norm(left_path))
  r <- as.character(fs::path_norm(right_path))

  for (sync_path in c(l, r)) {
    if (identical(b, sync_path) ||
        startsWith(b, paste0(sync_path, "/")) ||
        startsWith(sync_path, paste0(b, "/"))) {
      cli::cli_abort(
        c(
          "{.arg backup_dir} must not overlap with the directories being synced.",
          "x" = "{.path {backup_dir}} overlaps with {.path {sync_path}}.",
          "i" = "Use a separate, unrelated backup location."
        )
      )
    }
  }
  invisible(backup_dir)
}


#' Validate a single directory path argument
#'
#' Checks that a path argument is a non-NA, non-empty, length-1 character
#' string that refers to an existing directory. Throws an informative
#' `cli::cli_abort()` if any check fails.
#'
#' @param path The value to validate.
#' @param arg_name A string naming the argument (used in error messages).
#' @return Invisibly returns `path` if all checks pass.
#' @keywords internal
validate_path_arg <- function(path, arg_name = "path") {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} must be a single non-empty character string.",
        "x" = "Got: {.val {path}}"
      )
    )
  }
  if (!fs::dir_exists(path)) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} does not exist or is not a directory.",
        "x" = "Path: {.path {path}}"
      )
    )
  }
  invisible(path)
}


#' Set theme for colorDF
#'
#' @return invisible RStudio theme
#' @keywords internal
rs_theme <- function() {
  # set display options ------
  # Check if running in RStudio
  rstudio_theme <- template <-
    list(editor     = "",
         global     = "",
         dark       = FALSE,
         foreground = "",
         background = "")

  if (Sys.getenv("RSTUDIO") == "1") {
    # Attempt to infer theme or notify the user to set the theme if using a
    # newer RStudio version without `rstudioapi` support
    # If possible, use `rstudioapi` to get theme information (works only in certain versions)

    if (requireNamespace("rstudioapi", quietly = TRUE)) {
  rstudio_theme <- tryCatch(rstudioapi::getThemeInfo(),
                            error = \(e) template,
                            silent = TRUE)
}
  }
  # return
  invisible(rstudio_theme)
}

