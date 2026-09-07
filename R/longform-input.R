#' Prepare long-form forest inventory data
#'
#' Internal Stage 1 input layer for the expanded NepInventory workflow. This
#' helper validates a one-sheet long-form dataset while keeping the existing
#' plot-level `assess_inventory()` workflow unchanged.
#'
#' The long-form convention separates three concepts:
#' * optional defined-boundary information (`boundary_id`, `boundary_area_ha`),
#' * plot identity (`plot_id`), and
#' * optional research grouping variables (any columns beginning with `group_`).
#'
#' Vegetation records may be individual observations (`count = 1`) or
#' aggregated counts, such as seedling or sapling counts. The sampled support
#' for each record is stored in `sample_plot_size_m2`, which may differ within
#' a plot for nested sampling.
#'
#' @keywords internal
.prepare_longform_inventory <- function(data) {
  if (is.character(data)) data <- .read_inventory_file(data)
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or a single CSV/Excel file path.", call. = FALSE)
  }
  if (nrow(data) < 1L) stop("Long-form inventory data contain no rows.", call. = FALSE)

  required <- c("plot_id", "species_name", "sample_plot_size_m2")
  missing_required <- setdiff(required, names(data))
  if (length(missing_required)) {
    stop(
      sprintf("Missing required column(s): %s.", paste(sprintf("`%s`", missing_required), collapse = ", ")),
      call. = FALSE
    )
  }

  records <- data
  records$plot_id <- trimws(as.character(records$plot_id))
  if (any(is.na(records$plot_id) | !nzchar(records$plot_id))) {
    stop("`plot_id` cannot be missing or blank.", call. = FALSE)
  }

  records$species_name <- trimws(as.character(records$species_name))

  if (!"count" %in% names(records)) records$count <- 1
  count <- suppressWarnings(as.numeric(records$count))
  if (any(!is.finite(count) | count < 0)) {
    stop("`count` must contain non-negative numeric values.", call. = FALSE)
  }
  records$count <- count

  support <- suppressWarnings(as.numeric(records$sample_plot_size_m2))
  if (any(!is.finite(support) | support <= 0)) {
    stop("`sample_plot_size_m2` must contain positive numeric values.", call. = FALSE)
  }
  records$sample_plot_size_m2 <- support

  # A blank species is allowed only as an explicit zero-observation row. This
  # preserves sampled plots/categories with zero stems in a one-sheet format.
  blank_species <- is.na(records$species_name) | !nzchar(records$species_name)
  if (any(blank_species & records$count > 0)) {
    stop("`species_name` may be blank only when `count = 0`.", call. = FALSE)
  }

  if ("boundary_area_ha" %in% names(records)) {
    area <- suppressWarnings(as.numeric(records$boundary_area_ha))
    bad <- !is.na(area) & (!is.finite(area) | area <= 0)
    if (any(bad)) stop("`boundary_area_ha` must be positive when supplied.", call. = FALSE)
    records$boundary_area_ha <- area
  }

  if ("boundary_id" %in% names(records)) {
    records$boundary_id <- trimws(as.character(records$boundary_id))
    records$boundary_id[!nzchar(records$boundary_id)] <- NA_character_
  }

  group_columns <- grep("^group_", names(records), value = TRUE)

  # Plot identity is boundary_id + plot_id when a boundary is supplied;
  # otherwise plot_id itself is the plot key.
  if ("boundary_id" %in% names(records) && any(!is.na(records$boundary_id))) {
    boundary_key <- ifelse(is.na(records$boundary_id), "<no_boundary>", records$boundary_id)
    plot_key <- paste(boundary_key, records$plot_id, sep = "::")
  } else {
    plot_key <- records$plot_id
  }

  # Individual tree IDs may restart in each plot. Duplicate nonblank tree IDs
  # within the same plot are not allowed for positive-count individual rows.
  if ("tree_id" %in% names(records)) {
    tree_id <- trimws(as.character(records$tree_id))
    tree_id[is.na(tree_id) | !nzchar(tree_id)] <- NA_character_
    records$tree_id <- tree_id
    individual <- !is.na(tree_id) & records$count > 0
    duplicate_tree <- duplicated(paste(plot_key[individual], tree_id[individual], sep = "::"))
    if (any(duplicate_tree)) {
      stop("`tree_id` must be unique within each plot for individual-tree records.", call. = FALSE)
    }
  }

  # These fields describe the plot and therefore must not vary among vegetation
  # records belonging to the same plot. group_* columns are included because
  # they describe user-defined plot strata/classes.
  plot_fields <- intersect(c("elevation_m", "x", "y", "epsg", group_columns), names(records))
  .check_constant_within_key(records, plot_key, plot_fields, "plot")

  # Boundary area is metadata for the defined population and must be constant
  # within each boundary when both fields are supplied.
  if (all(c("boundary_id", "boundary_area_ha") %in% names(records))) {
    keep <- !is.na(records$boundary_id) & !is.na(records$boundary_area_ha)
    if (any(keep)) {
      .check_constant_within_key(
        records[keep, , drop = FALSE],
        records$boundary_id[keep],
        "boundary_area_ha",
        "boundary"
      )
    }
  }

  first <- !duplicated(plot_key)
  plot_columns <- unique(c("boundary_id", "boundary_area_ha", "plot_id", plot_fields))
  plot_columns <- intersect(plot_columns, names(records))
  plots <- records[first, plot_columns, drop = FALSE]
  rownames(plots) <- NULL

  boundary_available <- all(c("boundary_id", "boundary_area_ha") %in% names(records)) &&
    any(!is.na(records$boundary_id) & !is.na(records$boundary_area_ha))

  availability <- list(
    defined_boundary = boundary_available,
    grouping = length(group_columns) > 0L,
    groups = group_columns,
    dbh = "dbh_cm" %in% names(records) && any(is.finite(suppressWarnings(as.numeric(records$dbh_cm)))),
    height = "height_m" %in% names(records) && any(is.finite(suppressWarnings(as.numeric(records$height_m)))),
    coordinates = all(c("x", "y") %in% names(records)),
    plant_category = "plant_category" %in% names(records),
    tree_quality = "tree_quality" %in% names(records)
  )

  list(
    records = records,
    plots = plots,
    group_columns = group_columns,
    availability = availability
  )
}

#' Check that metadata are constant within an identifier
#'
#' @keywords internal
.check_constant_within_key <- function(data, key, fields, level = "unit") {
  if (!length(fields)) return(invisible(TRUE))

  for (field in fields) {
    values <- data[[field]]
    split_values <- split(values, key, drop = TRUE)
    bad <- vapply(split_values, function(x) {
      if (is.character(x)) x <- trimws(x)
      x <- x[!is.na(x)]
      if (is.character(x)) x <- x[nzchar(x)]
      length(unique(x)) > 1L
    }, logical(1))

    if (any(bad)) {
      ids <- names(bad)[bad]
      stop(
        sprintf(
          "Column `%s` must be constant within each %s. Inconsistent value(s) found for: %s.",
          field, level, paste(ids, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}
