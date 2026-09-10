#' Prepare long-form forest inventory data
#'
#' Internal Stage 1 input layer for the expanded NepInventory workflow. This
#' helper validates a one-sheet long-form dataset while keeping the existing
#' plot-level `assess_inventory()` workflow unchanged.
#'
#' The long-form convention separates four concepts:
#' * optional defined-boundary information (`boundary_id`, `boundary_area_ha`),
#' * plot identity (`plot_id`),
#' * optional research grouping variables (any columns beginning with `group_`),
#' * optional vegetation/taxonomic/tree measurements used only when available.
#'
#' Only `plot_id` is universally required. Other columns unlock analyses rather
#' than causing the entire workflow to fail when they are absent. For example,
#' `sample_plot_size_m2` is needed for area-standardized density, `dbh_cm` for
#' basal area/QMD, and `family`/`genus` for higher-taxonomic summaries.
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

  if (!"plot_id" %in% names(data)) {
    stop("Missing required column `plot_id`.", call. = FALSE)
  }

  records <- data
  records$plot_id <- trimws(as.character(records$plot_id))
  if (any(is.na(records$plot_id) | !nzchar(records$plot_id))) {
    stop("`plot_id` cannot be missing or blank.", call. = FALSE)
  }

  # Normalize optional character fields when present.
  character_fields <- intersect(
    c("boundary_id", "tree_id", "family", "genus", "species_name",
      "plant_category", "tree_quality"),
    names(records)
  )
  for (field in character_fields) {
    x <- trimws(as.character(records[[field]]))
    x[is.na(x) | !nzchar(x)] <- NA_character_
    records[[field]] <- x
  }

  # Individual records default to count = 1. Aggregated vegetation records can
  # explicitly supply larger counts. Missing count values are treated as 1 only
  # when an observed taxon/tree record is present; explicit empty rows should
  # use count = 0.
  if (!"count" %in% names(records)) {
    records$count <- 1
  } else {
    count <- suppressWarnings(as.numeric(records$count))
    observed_record <- rep(FALSE, nrow(records))
    if ("species_name" %in% names(records)) observed_record <- observed_record | !is.na(records$species_name)
    if ("tree_id" %in% names(records)) observed_record <- observed_record | !is.na(records$tree_id)
    count[is.na(count) & observed_record] <- 1
    if (any(!is.na(count) & (!is.finite(count) | count < 0))) {
      stop("`count` must contain non-negative numeric values when supplied.", call. = FALSE)
    }
    records$count <- count
  }

  # A positive count requires a taxon name for species-level analyses. Blank
  # species names remain valid for explicit zero-observation rows and for data
  # intended only for non-taxonomic analyses.
  if (all(c("species_name", "count") %in% names(records))) {
    bad_species <- !is.na(records$count) & records$count > 0 & is.na(records$species_name)
    if (any(bad_species)) {
      warning(
        "Some positive-count records have no `species_name`; species-level analyses will exclude those records.",
        call. = FALSE
      )
    }
  }

  if ("sample_plot_size_m2" %in% names(records)) {
    support <- suppressWarnings(as.numeric(records$sample_plot_size_m2))
    bad <- !is.na(support) & (!is.finite(support) | support <= 0)
    if (any(bad)) {
      stop("`sample_plot_size_m2` must be positive when supplied.", call. = FALSE)
    }
    records$sample_plot_size_m2 <- support
  }

  numeric_optional <- intersect(
    c("boundary_area_ha", "elevation_m", "x", "y", "epsg",
      "dbh_cm", "height_m"),
    names(records)
  )
  for (field in numeric_optional) {
    x <- suppressWarnings(as.numeric(records[[field]]))
    if (field == "boundary_area_ha") {
      bad <- !is.na(x) & (!is.finite(x) | x <= 0)
      if (any(bad)) stop("`boundary_area_ha` must be positive when supplied.", call. = FALSE)
    } else if (field %in% c("dbh_cm", "height_m")) {
      bad <- !is.na(x) & (!is.finite(x) | x <= 0)
      if (any(bad)) stop(sprintf("`%s` must be positive when supplied.", field), call. = FALSE)
    }
    records[[field]] <- x
  }

  group_columns <- grep("^group_", names(records), value = TRUE)

  # Plot identity is boundary_id + plot_id when a boundary identifier is
  # supplied; otherwise plot_id itself is the plot key.
  if ("boundary_id" %in% names(records) && any(!is.na(records$boundary_id))) {
    boundary_key <- ifelse(is.na(records$boundary_id), "<no_boundary>", records$boundary_id)
    plot_key <- paste(boundary_key, records$plot_id, sep = "::")
  } else {
    plot_key <- records$plot_id
  }

  # Individual tree IDs may restart in each plot. Duplicate nonblank tree IDs
  # within the same plot are not allowed for positive-count individual rows.
  if ("tree_id" %in% names(records)) {
    individual <- !is.na(records$tree_id) & !is.na(records$count) & records$count > 0
    tree_keys <- paste(plot_key[individual], records$tree_id[individual], sep = "::")
    if (anyDuplicated(tree_keys)) {
      stop("`tree_id` must be unique within each plot for individual-tree records.", call. = FALSE)
    }
  }

  # These fields describe the plot and therefore must not vary among vegetation
  # records belonging to the same plot. `group_*` columns are treated as
  # user-defined plot-level strata/classes. sample_plot_size_m2 is deliberately
  # excluded because nested sampling can use different support areas in one plot.
  plot_fields <- intersect(c("elevation_m", "x", "y", "epsg", group_columns), names(records))
  .check_constant_within_key(records, plot_key, plot_fields, "plot")

  # Boundary area must be constant within a named boundary when supplied.
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

  has_nonmissing <- function(name) {
    name %in% names(records) && any(!is.na(records[[name]]))
  }
  has_positive_count <- "count" %in% names(records) && any(!is.na(records$count) & records$count > 0)
  has_species <- has_nonmissing("species_name")
  has_sample_area <- has_nonmissing("sample_plot_size_m2")
  has_dbh <- has_nonmissing("dbh_cm")
  has_height <- has_nonmissing("height_m")
  has_boundary <- all(c("boundary_id", "boundary_area_ha") %in% names(records)) &&
    any(!is.na(records$boundary_id) & !is.na(records$boundary_area_ha))
  has_coordinates <- all(c("x", "y") %in% names(records)) &&
    any(!is.na(records$x) & !is.na(records$y))

  availability <- list(
    defined_boundary = has_boundary,
    grouping = length(group_columns) > 0L,
    groups = group_columns,
    species = has_species,
    genus = has_nonmissing("genus"),
    family = has_nonmissing("family"),
    count = has_positive_count,
    sample_plot_size = has_sample_area,
    dbh = has_dbh,
    height = has_height,
    coordinates = has_coordinates,
    plant_category = has_nonmissing("plant_category"),
    tree_quality = has_nonmissing("tree_quality"),
    analyses = list(
      composition = has_species && has_positive_count,
      diversity = has_species && has_positive_count,
      density = has_species && has_positive_count && has_sample_area,
      frequency = has_species,
      basal_area = has_dbh && has_sample_area,
      dominance = has_species && has_dbh && has_sample_area,
      ivi = has_species && has_positive_count && has_sample_area && has_dbh,
      stand_structure = has_dbh || has_height,
      regeneration = has_species && has_positive_count && has_sample_area && has_nonmissing("plant_category"),
      sampling_intensity = has_boundary && has_sample_area,
      elevation_relationships = has_nonmissing("elevation_m")
    )
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
