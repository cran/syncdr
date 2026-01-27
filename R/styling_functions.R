#' Apply Custom Style to Text
#'
#' This function applies a custom color and bold style to a given text string.
#'
#' @param color_name Character. Name of the color to apply. Each color name is associated with a specifically chosen color code. Available options for now are "pink", "blue", "purple", and "green".
#' @param text Character. The text string to which the style will be applied.
#' @return The styled text is printed to the console.
#'
#' @keywords internal
#'
style_msgs <- function(color_name,
                       text) {

  # Validate the color_name argument
  color_name <- match.arg(arg = color_name,
                          choices = c("pink", "blue", "purple", "green", "orange"),
                          several.ok = FALSE)

  # Create custom ANSI style for colors
  color_code <- switch(color_name,
                       "orange" = "#FE5A1D",
                       "pink"   = "#FF1493",
                       "blue"   = "#00BFFF",
                       "purple" = "#BF00FF",
                       "green"  = "#06d6a0")

  color_fun <- cli::make_ansi_style(color_code)

  #Apply color to text and make it bold
  styled_text <- color_fun(text) |>
    cli::style_bold()

  # Return styled text
  cat(styled_text)
}

