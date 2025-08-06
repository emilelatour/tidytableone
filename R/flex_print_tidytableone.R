
#' @title
#' Format a tidytableone summary table with indentation-aware padding
#'
#' @description
#' This function formats a summary table created with `tidytableone` using `lamisc::flexprint()` and adds
#' left padding to rows based on indentation in a specified column (e.g., to visually group variables).
#'
#' @param tidy_t1 A data frame or tibble returned by `create_tidy_table_one()` and optionally modified
#'   by `adorn_tidytableone()`.
#' @param column1 Character. The name of the column to check for indentation (default: `"var"`).
#' @param padding Numeric. Amount of left padding (in pts) to apply to indented rows (default: 20).
#' @param ... Additional arguments passed to `lamisc::flexprint()`.
#'
#' @return A `flextable` object with formatting and conditional left padding applied.
#' @export
#'
#' @examples
#' \dontrun{
#' t1 <- tidytableone::create_tidy_table_one(data = baseline_df, vars = table1_vars)
#' t1 <- tidytableone::adorn_tidytableone(t1)
#' t1 <- dplyr::rename(t1, Characteristic = var)
#' flex_print_tidytableone(t1, column1 = "Characteristic")
#' }
flex_print_tidytableone <- function(tidy_t1, 
                                    column1 = "var", 
                                    padding = 20, 
                                    ...) {
  
  if (!column1 %in% names(tidy_t1)) {
    stop(glue::glue("Column '{column1}' not found in the input table."))
  }
  
  indent_col <- dplyr::pull(tidy_t1, !!rlang::ensym(column1)) |> 
    tidyr::replace_na("")
  
  indent_rows <- which(stringr::str_detect(indent_col, "^  "))
  
  tidy_t1 |> 
    .flex_print(...) |> 
    flextable::padding(i = indent_rows, 
                       j = 1, 
                       padding.left = padding)
}


#' Internal helper to format a flextable with preferred default styling
#' 
#' @keywords internal
#'
#' @description
#' A wrapper for `flextable::flextable()` that applies custom styling, alignment,
#' font settings, optional headers/footers, and NA defaults.
#'
#' @param x A data frame or tibble.
#' @param font_name Character. Font name to use.
#' @param font_size Integer. Font size in points.
#' @param align Alignment for each column ('l', 'c', 'r', or 'j').
#' @param align_j Optional vector of column indices for alignment.
#' @param auto_fit Logical. Whether to auto fit column widths.
#' @param width Column widths in inches.
#' @param width_j Column indices to apply widths to.
#' @param width_unit One of "in", "cm", or "mm".
#' @param col_nms Names of columns to rename.
#' @param col_lbls New labels to apply to renamed columns.
#' @param title,subtitle,footer Optional text to add above or below the table.
#' @param title_size,subtitle_size,footer_size Font sizes for text decorations.
#' @param na_dflt,nan_dflt Strings to display for NA and NaN values.
#' 
#' @importFrom flextable flextable autofit width align set_header_labels fontsize font bold
#' @importFrom flextable add_header_lines add_footer_lines hline hline_top border_inner_h
#' @importFrom flextable set_flextable_defaults fp_border_default
#' @importFrom officer fp_border
#' @importFrom stats setNames
#'
#' @return A `flextable` object.
.flex_print <- function(x,
                       font_name = "Arial",
                       font_size = 11,
                       align = NULL,
                       align_j = NULL,
                       auto_fit = TRUE,
                       width = NULL,
                       width_j = NULL,
                       width_unit = "in",
                       col_nms = NULL,
                       col_lbls = NULL,
                       title = NULL,
                       subtitle = NULL,
                       footer = NULL,
                       title_size = 16,
                       subtitle_size = 11,
                       footer_size = 11,
                       na_dflt = "NA",
                       nan_dflt = "NaN") {


  # Set the defaults for NA and NaN
  na_def <- flextable::set_flextable_defaults(na_str = na_dflt, nan_str = nan_dflt)

  # Get the dimensions of x
  x_dim <- dim(x)

  # Get column names for later maybe
  if (is.null(col_nms)) {
    col_nms <- names(x)
  }


  # Make a flextable object
  x <- x |>
    flextable::flextable()

  # Autofit if TRUE
  if (auto_fit) {
    x <- x |>
      flextable::autofit()
  }


  # Set columns width
  if (!is.null(width)) {
    x <- x |>
      flextable::width(j = width_j,
                       width = width,
                       unit = width_unit)
  }


  # Set text alignment
  if (!is.null(align)) {

    align <- unlist(strsplit(align, ""))

    n_cols <- x_dim[[2]]

    # Pad the end with the last element in align
    align_pad <- n_cols - length(align)
    align_pad <- max(0, align_pad)

    align <- c(align,
               rep(align[length(align)], align_pad))

    align <- align[1:n_cols]

    if (is.null(align_j)) {
      align_j <- c(1:length(align))
    }

    # "left", "center", "right", "justify"
    align <- dplyr::case_match(align,
                               "l" ~ "left",
                               "c" ~ "center",
                               "r" ~ "right",
                               "j" ~ "justify")


    x <- x |>
      flextable::align(j = align_j,
                       align = align,
                       part = "all")
  }


  # Column labels
  if (!is.null(col_lbls)) {

    header_list <- rlang::list2(!!! setNames(col_lbls, col_nms))

    x <- x |>
      flextable::set_header_labels(values = header_list)

  }


  # Apply font size
  x <- x |>
    flextable::fontsize(size = font_size,
                        part = "all")

  # Apply font name
  x <- x |>
    flextable::font(fontname = font_name,
                    part = "all")


  # Bold column labels
  x <- x |>
    flextable::bold(i = 1, bold = TRUE, part = "header")


  # Subtitle and title
  if (!is.null(subtitle) & !is.null(title)) {

    title <- stringr::str_squish(title)
    subtitle <- stringr::str_squish(subtitle)

    x <- x |>
      flextable::add_header_lines(values = "") |>  # add Padding
      flextable::add_header_lines(values = subtitle) |>  # add subtitle
      flextable::add_header_lines(values = title) |>  # add title
      flextable::hline_top(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::border_inner_h(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::align(i = c(1, 2), j = 1, align = "left", part = "header") |>
      flextable::fontsize(i = 1, size = title_size, part = "header") |>
      flextable::fontsize(i = 2, size = subtitle_size, part = "header") |>
      flextable::bold(i = 2, bold = FALSE, part = "header")


    x <- x |>
      flextable::hline_top(part = "header",
                           border = officer::fp_border(color = "black",
                                                       width = 2.0,
                                                       style = "solid")) |>
      flextable::hline(i = 3,
                       part = "header",
                       border = officer::fp_border(color = "#666666",
                                                   width = 1.5,
                                                   style = "solid"))


  }

  # title only
  if (is.null(subtitle) & !is.null(title)) {

    title <- stringr::str_squish(title)

    x <- x |>
      flextable::add_header_lines(values = "") |>  # add Padding
      flextable::add_header_lines(values = title) |>  # add title
      flextable::hline_top(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::border_inner_h(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::align(i = 1, j = 1, align = "left", part = "header") |>
      flextable::fontsize(i = 1, size = title_size, part = "header")


    x <- x |>
      flextable::hline_top(part = "header",
                           border = officer::fp_border(color = "black",
                                                       width = 2.0,
                                                       style = "solid")) |>
      flextable::hline(i = 2,
                       part = "header",
                       border = officer::fp_border(color = "#666666",
                                                   width = 1.5,
                                                   style = "solid"))

  }

  if (!is.null(footer)) {

    footer <- stringr::str_squish(footer)

    x <- x |>
      flextable::add_footer_lines(footer) |>
      # flextable::hline_bottom(part = "footer",
      #                         border = officer::fp_border(color = "#666666",
      #                                                     width = 0.25,
      #                                                     style = "solid")) |>
      flextable::font(fontname = font_name,
                      part = "footer") |>
      flextable::fontsize(i = 1, size = footer_size, part = "footer")


  }


  # Set the defaults for NA and NaN back to normal
  do.call(flextable::set_flextable_defaults, na_def)


  return(x)

}