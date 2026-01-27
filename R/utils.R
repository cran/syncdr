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

