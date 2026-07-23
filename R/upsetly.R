#' Create an interactive UpSet plot
#'
#' @description
#' `upsetly()` creates an interactive UpSet-style visualization with three
#' coordinated parts: set-size bars, intersection-size bars, and a membership
#' matrix. Intersection tooltips can include member identifiers. In HTML
#' documents, complete intersection details can optionally be synchronized to
#' a separate element so that readers can select and copy the text.
#'
#' @param x A data frame containing set-membership columns and, optionally, an
#'   identifier column.
#' @param set_cols A non-empty character vector naming the membership columns
#'   in `x`. Logical values and non-zero numeric values are treated as members.
#'   For character and factor columns, empty strings and common false values
#'   such as `"0"`, `"false"`, and `"no"` are treated as non-members; other
#'   non-empty values are treated as members. If `NULL`, all columns other than
#'   `id_col` are used.
#' @param id_col An optional single column name containing element identifiers.
#'   If `NULL`, sequential identifiers are generated.
#' @param min_intersection_size A positive integer giving the smallest
#'   intersection to retain.
#' @param max_n_intersections An optional positive integer giving the maximum
#'   number of intersections to display, ordered by decreasing size. For
#'   compatibility with earlier versions, it also limits the number of member
#'   identifiers shown in each hover tooltip. Complete details synchronized to
#'   `box_id` are not truncated.
#' @param point_size A positive number giving the marker size in the membership
#'   matrix.
#' @param bar_color_sets Color used for the set-size bars.
#' @param bar_color_inters Color used for the intersection-size bars.
#' @param active_color Color used for active membership markers.
#' @param inactive_color Color used for inactive membership markers.
#' @param line_color Color used for lines joining active membership markers.
#' @param title A single character string used as the plot title.
#' @param height A positive number giving the widget height in pixels.
#' @param width An optional positive number giving the widget width in pixels.
#' @param members_per_line A positive integer giving the number of member
#'   identifiers placed on each tooltip line.
#' @param box_id An optional single character string containing the HTML `id`
#'   of an element, commonly a `pre` element. Hovering over an intersection bar
#'   updates the element, clicking locks its content, and double-clicking the
#'   plot unlocks and clears it.
#'
#' @return A `plotly` htmlwidget.
#'
#' @examples
#' set.seed(1)
#' memberships <- data.frame(
#'   gene = paste0("g", seq_len(40)),
#'   A = stats::rbinom(40, 1, 0.35),
#'   B = stats::rbinom(40, 1, 0.40),
#'   C = stats::rbinom(40, 1, 0.25),
#'   stringsAsFactors = FALSE
#' )
#'
#' upsetly(
#'   memberships,
#'   set_cols = c("A", "B", "C"),
#'   id_col = "gene",
#'   max_n_intersections = 12
#' )
#'
#' @export
upsetly <- function(
    x,
    set_cols = NULL,
    id_col = NULL,
    min_intersection_size = 1,
    max_n_intersections = NULL,
    point_size = 8,
    bar_color_sets = "black",
    bar_color_inters = "black",
    active_color = "black",
    inactive_color = "lightgray",
    line_color = "black",
    title = "UpSet (plotly)",
    height = 500,
    width = NULL,
    members_per_line = 20,
    box_id = NULL) {
  abort <- function(message) {
    stop(message, call. = FALSE)
  }

  is_scalar_string <- function(value, allow_empty = FALSE) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      (allow_empty || nzchar(value))
  }

  validate_positive_number <- function(value, name, integer = FALSE) {
    valid <- is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value > 0
    if (integer) {
      valid <- valid && value == floor(value) && value <= .Machine$integer.max
    }
    if (!valid) {
      qualifier <- if (integer) "a positive integer" else "a positive number"
      abort(sprintf("`%s` must be %s.", name, qualifier))
    }
    invisible(value)
  }

  if (!is.data.frame(x)) {
    abort("`x` must be a data frame.")
  }
  if (!is.null(id_col) && !is_scalar_string(id_col)) {
    abort("`id_col` must be NULL or a single non-empty column name.")
  }
  if (!is.null(id_col) && !id_col %in% names(x)) {
    abort(sprintf("`id_col` is not in `x`: %s", id_col))
  }

  if (is.null(set_cols)) {
    set_cols <- setdiff(names(x), if (is.null(id_col)) character() else id_col)
  }
  if (!is.character(set_cols) || !length(set_cols) || anyNA(set_cols) ||
      any(!nzchar(set_cols))) {
    abort("`set_cols` must be a non-empty character vector of column names.")
  }
  if (anyDuplicated(set_cols)) {
    abort("`set_cols` must not contain duplicate column names.")
  }
  missing_cols <- setdiff(set_cols, names(x))
  if (length(missing_cols)) {
    abort(paste0(
      "These `set_cols` are not in `x`: ",
      paste(missing_cols, collapse = ", ")
    ))
  }
  if (!is.null(id_col) && id_col %in% set_cols) {
    abort("`id_col` must not also be included in `set_cols`.")
  }

  validate_positive_number(
    min_intersection_size,
    "min_intersection_size",
    integer = TRUE
  )
  if (!is.null(max_n_intersections)) {
    validate_positive_number(
      max_n_intersections,
      "max_n_intersections",
      integer = TRUE
    )
  }
  validate_positive_number(point_size, "point_size")
  validate_positive_number(height, "height")
  if (!is.null(width)) {
    validate_positive_number(width, "width")
  }
  validate_positive_number(members_per_line, "members_per_line", integer = TRUE)

  if (!is_scalar_string(title, allow_empty = TRUE)) {
    abort("`title` must be a single character string.")
  }
  colors <- list(
    bar_color_sets = bar_color_sets,
    bar_color_inters = bar_color_inters,
    active_color = active_color,
    inactive_color = inactive_color,
    line_color = line_color
  )
  invalid_colors <- names(colors)[!vapply(colors, is_scalar_string, logical(1))]
  if (length(invalid_colors)) {
    abort(paste0(
      "Color arguments must be single non-empty character strings: ",
      paste(invalid_colors, collapse = ", ")
    ))
  }
  if (!is.null(box_id) && !is_scalar_string(box_id)) {
    abort("`box_id` must be NULL or a single non-empty HTML element id.")
  }

  if (is.null(id_col)) {
    id_col <- ".elem_id"
    while (id_col %in% names(x)) {
      id_col <- paste0(".", id_col)
    }
    x[[id_col]] <- seq_len(nrow(x))
  }

  coerce_membership <- function(value) {
    if (is.logical(value)) {
      return(ifelse(is.na(value), 0L, as.integer(value)))
    }
    if (is.numeric(value)) {
      return(ifelse(is.na(value) | value == 0, 0L, 1L))
    }

    normalized <- tolower(trimws(as.character(value)))
    false_values <- c("", "0", "false", "f", "no", "n", "off")
    ifelse(is.na(value) | normalized %in% false_values, 0L, 1L)
  }

  mat <- lapply(x[set_cols], coerce_membership)
  mat <- as.data.frame(mat, check.names = FALSE, stringsAsFactors = FALSE)
  set_sizes <- vapply(mat, sum, numeric(1))
  if (!length(set_sizes) || all(set_sizes == 0)) {
    abort("All set columns are empty; no elements belong to the supplied sets.")
  }

  set_order <- order(set_sizes, decreasing = TRUE)
  set_cols <- set_cols[set_order]
  set_sizes <- set_sizes[set_order]
  mat <- mat[set_cols]
  set_levels <- names(set_sizes)

  set_sizes_df <- data.frame(
    set = factor(set_levels, levels = set_levels),
    size = as.numeric(set_sizes),
    position = seq_along(set_levels),
    stringsAsFactors = FALSE
  )
  set_sizes_df$negative_size <- -set_sizes_df$size
  set_sizes_df$hover_set <- paste0(
    "Set: ",
    htmltools::htmlEscape(as.character(set_sizes_df$set)),
    "<br>Size: ",
    set_sizes_df$size
  )

  active_rows <- rowSums(as.matrix(mat)) > 0
  active_mat <- mat[active_rows, , drop = FALSE]
  active_ids <- as.character(x[[id_col]][active_rows])
  combo_keys <- apply(as.matrix(active_mat), 1L, paste, collapse = "|")
  row_groups <- split(seq_along(combo_keys), combo_keys, drop = TRUE)
  group_sizes <- lengths(row_groups)
  keep <- group_sizes >= min_intersection_size
  row_groups <- row_groups[keep]
  group_sizes <- group_sizes[keep]
  if (!length(row_groups)) {
    abort("No intersections meet `min_intersection_size`.")
  }

  combo_matrix <- do.call(
    rbind,
    lapply(strsplit(names(row_groups), "|", fixed = TRUE), as.integer)
  )
  if (is.null(dim(combo_matrix))) {
    combo_matrix <- matrix(combo_matrix, nrow = 1L)
  }
  colnames(combo_matrix) <- set_cols

  inter_df <- data.frame(
    .combo_key = names(row_groups),
    count = as.integer(group_sizes),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  inter_df <- cbind(
    inter_df,
    as.data.frame(combo_matrix, check.names = FALSE, stringsAsFactors = FALSE)
  )
  member_vectors <- lapply(row_groups, function(rows) active_ids[rows])

  format_members <- function(values, n_per_line, max_ids = NULL, html = FALSE) {
    values[is.na(values)] <- "NA"
    values <- as.character(values)
    truncated <- FALSE
    omitted <- 0L
    if (!is.null(max_ids) && length(values) > max_ids) {
      omitted <- length(values) - max_ids
      values <- values[seq_len(max_ids)]
      truncated <- TRUE
    }
    if (html) {
      values <- htmltools::htmlEscape(values)
    }
    groups <- ceiling(seq_along(values) / n_per_line)
    lines <- vapply(
      split(values, groups),
      paste,
      collapse = ", ",
      FUN.VALUE = character(1)
    )
    separator <- if (html) "<br>" else "\n"
    result <- paste(lines, collapse = separator)
    if (truncated) {
      result <- paste0(
        result,
        separator,
        "... (",
        omitted,
        " more)"
      )
    }
    result
  }

  inter_df$combo_name <- apply(
    inter_df[set_cols],
    1L,
    function(row) {
      included <- set_cols[as.integer(row) == 1L]
      if (!length(included)) "None" else paste(included, collapse = " & ")
    }
  )
  combo_name_html <- htmltools::htmlEscape(inter_df$combo_name)
  members_html <- vapply(
    member_vectors,
    format_members,
    FUN.VALUE = character(1),
    n_per_line = members_per_line,
    max_ids = max_n_intersections,
    html = TRUE
  )
  members_plain <- vapply(
    member_vectors,
    format_members,
    FUN.VALUE = character(1),
    n_per_line = members_per_line,
    max_ids = NULL,
    html = FALSE
  )
  inter_df$hover_bar <- paste0(
    "Intersection: ",
    combo_name_html,
    "<br>Size: ",
    inter_df$count,
    "<br>Members:<br>",
    members_html
  )
  inter_df$full_text_for_box <- paste0(
    "Intersection: ",
    inter_df$combo_name,
    "\nSize: ",
    inter_df$count,
    "\nMembers:\n",
    members_plain
  )

  intersection_order <- order(-inter_df$count, inter_df$combo_name)
  inter_df <- inter_df[intersection_order, , drop = FALSE]
  if (!is.null(max_n_intersections)) {
    inter_df <- inter_df[
      seq_len(min(max_n_intersections, nrow(inter_df))),
      ,
      drop = FALSE
    ]
  }
  inter_df$combo_index <- seq_len(nrow(inter_df))

  dot_df <- expand.grid(
    combo_index = inter_df$combo_index,
    set = set_levels,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  dot_df$value <- vapply(
    seq_len(nrow(dot_df)),
    function(index) {
      inter_df[[dot_df$set[[index]]]][dot_df$combo_index[[index]]]
    },
    integer(1)
  )
  dot_df$y <- match(dot_df$set, set_levels)
  dot_df$set <- factor(dot_df$set, levels = set_levels)
  dot_df$hover_dot <- paste0(
    "Set: ",
    htmltools::htmlEscape(as.character(dot_df$set)),
    "<br>Intersection: ",
    htmltools::htmlEscape(
      inter_df$combo_name[match(dot_df$combo_index, inter_df$combo_index)]
    ),
    "<br>Included: ",
    ifelse(dot_df$value == 1L, "Yes", "No")
  )

  active_dots <- dot_df[dot_df$value == 1L, , drop = FALSE]
  if (nrow(active_dots)) {
    y_groups <- split(active_dots$y, active_dots$combo_index)
    seg_df <- data.frame(
      combo_index = as.integer(names(y_groups)),
      y_min = vapply(y_groups, min, numeric(1)),
      y_max = vapply(y_groups, max, numeric(1)),
      stringsAsFactors = FALSE
    )
  } else {
    seg_df <- data.frame(
      combo_index = integer(),
      y_min = numeric(),
      y_max = numeric()
    )
  }

  fig <- plotly::plot_ly(height = height, width = width)
  fig <- plotly::add_bars(
    fig,
    data = inter_df,
    x = ~combo_index,
    y = ~count,
    marker = list(color = bar_color_inters),
    hovertext = ~hover_bar,
    hoverinfo = "text",
    customdata = ~full_text_for_box,
    name = "Intersections",
    yaxis = "y1"
  )
  fig <- plotly::add_bars(
    fig,
    data = set_sizes_df,
    x = ~negative_size,
    y = ~position,
    orientation = "h",
    marker = list(color = bar_color_sets),
    hovertext = ~hover_set,
    hoverinfo = "text",
    name = "Set size",
    xaxis = "x2",
    yaxis = "y2"
  )
  if (nrow(seg_df)) {
    fig <- plotly::add_segments(
      fig,
      data = seg_df,
      x = ~combo_index,
      xend = ~combo_index,
      y = ~y_min,
      yend = ~y_max,
      line = list(color = line_color, width = 1),
      hoverinfo = "none",
      xaxis = "x1",
      yaxis = "y2",
      showlegend = FALSE
    )
  }
  fig <- plotly::add_trace(
    fig,
    data = dot_df,
    x = ~combo_index,
    y = ~y,
    type = "scatter",
    mode = "markers",
    marker = list(
      color = ifelse(dot_df$value == 1L, active_color, inactive_color),
      size = point_size
    ),
    hovertext = ~hover_dot,
    hoverinfo = "text",
    xaxis = "x1",
    yaxis = "y2",
    showlegend = FALSE
  )

  max_size <- max(set_sizes_df$size)
  pretty_breaks <- pretty(c(0, max_size))
  fig <- plotly::layout(
    fig,
    title = list(text = title),
    xaxis = list(
      domain = c(0.25, 1),
      anchor = "y1",
      title = "",
      showticklabels = FALSE,
      tickfont = list(color = "black")
    ),
    xaxis2 = list(
      domain = c(0, 0.2),
      anchor = "y2",
      title = list(text = "Set size", font = list(color = "black")),
      range = c(-max_size, 0),
      tickmode = "array",
      tickvals = -pretty_breaks,
      ticktext = pretty_breaks,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      tickfont = list(color = "black")
    ),
    yaxis = list(
      domain = c(0.5, 1),
      title = list(text = "Intersection size", font = list(color = "black")),
      tickfont = list(color = "black")
    ),
    yaxis2 = list(
      domain = c(0, 0.5),
      title = "",
      tickmode = "array",
      tickvals = seq_along(set_levels),
      ticktext = set_levels,
      anchor = "x2",
      side = "right",
      tickfont = list(color = "black")
    ),
    showlegend = FALSE
  )
  fig <- plotly::config(fig, displaylogo = FALSE, responsive = TRUE)

  box_id_json <- if (is.null(box_id)) {
    "null"
  } else {
    as.character(jsonlite::toJSON(box_id, auto_unbox = TRUE))
  }
  javascript <- sprintf(
    paste0(
      "function(el, x) {",
      "var boxId=%s;",
      "if(!boxId)return;",
      "var box=document.getElementById(boxId);",
      "if(!box||!el||typeof el.on!=='function')return;",
      "if(el.__upsetlyHandlers&&typeof el.removeListener==='function'){",
      "el.removeListener('plotly_hover',el.__upsetlyHandlers.hover);",
      "el.removeListener('plotly_click',el.__upsetlyHandlers.click);",
      "el.removeListener('plotly_doubleclick',el.__upsetlyHandlers.doubleclick);",
      "}",
      "var locked=false;",
      "function details(event){",
      "if(!event||!event.points||!event.points.length)return null;",
      "var value=event.points[0].customdata;",
      "return typeof value==='string'?value.replace(/<br\\s*\\/?>/gi,'\\n'):null;",
      "}",
      "var hoverHandler=function(event){",
      "if(locked)return;var value=details(event);",
      "if(value!==null)box.textContent=value;",
      "};",
      "var clickHandler=function(event){",
      "var value=details(event);if(value===null)return;",
      "locked=true;box.textContent=value;",
      "};",
      "var doubleClickHandler=function(){",
      "locked=false;box.textContent='';",
      "};",
      "el.on('plotly_hover',hoverHandler);",
      "el.on('plotly_click',clickHandler);",
      "el.on('plotly_doubleclick',doubleClickHandler);",
      "el.__upsetlyHandlers={",
      "hover:hoverHandler,click:clickHandler,doubleclick:doubleClickHandler",
      "};",
      "}"
    ),
    box_id_json
  )

  htmlwidgets::onRender(fig, htmlwidgets::JS(javascript))
}
