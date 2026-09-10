#' Forest taxonomic composition analysis
#'
#' Summarizes observed abundance and relative abundance for a selected
#' taxonomic level. This analysis does not require plot area and therefore can
#' run when only plot identifiers, taxa and counts are available.
#'
#' @inheritParams forest_density
#' @return An object of class `forest_composition_result` containing the
#'   composition table, plot-level abundance data, figure data, interpretation,
#'   and settings.
#' @export
forest_composition <- function(data, by = NULL, taxon = "species_name", plant_category = NULL) {
  x <- .prepare_longform_inventory(data)
  d <- .m1_filter_category(x$records, plant_category)
  by <- .m1_validate_by(d, by)
  .m1_require(d, c(taxon, "count"), "Forest composition")
  d <- .m1_add_plot_key(d)
  category_col <- .m1_add_category(d)
  if (!is.null(category_col)) d$.plant_category <- ifelse(is.na(d$plant_category), "Unclassified", d$plant_category)

  usable <- !is.na(d[[taxon]]) & !is.na(d$count) & d$count > 0
  if (!any(usable)) stop("Forest composition cannot be calculated because no positive-count taxon records were found.", call. = FALSE)
  d <- d[usable, , drop = FALSE]

  keys <- c(by, category_col, taxon)
  out <- stats::aggregate(d$count, d[keys], sum, na.rm = TRUE)
  names(out)[ncol(out)] <- "abundance"

  rel_keys <- c(by, category_col)
  if (!length(rel_keys)) {
    total <- sum(out$abundance, na.rm = TRUE)
    out$relative_abundance_pct <- out$abundance / total * 100
  } else {
    rk <- interaction(out[rel_keys], drop = TRUE, lex.order = TRUE, sep = "\r")
    out$relative_abundance_pct <- ave(out$abundance, rk,
      FUN = function(v) v / sum(v, na.rm = TRUE) * 100)
  }

  plot_keys <- c(".plot_key", "plot_id", intersect("boundary_id", names(d)), by, category_col, taxon)
  plot_data <- stats::aggregate(d$count, d[plot_keys], sum, na.rm = TRUE)
  names(plot_data)[ncol(plot_data)] <- "abundance"

  out <- .m1_restore_names(out)
  plot_data <- .m1_restore_names(plot_data)
  out <- out[order(out$relative_abundance_pct, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL

  ans <- list(
    result = out,
    plot_data = plot_data,
    figure_data = out,
    interpretation = sprintf("Taxonomic composition was summarized at the `%s` level using observed abundance. Relative abundance is each taxon's percentage of the total abundance within the analyzed group or plant category.", taxon),
    settings = list(by = by, taxon = taxon, plant_category = plant_category)
  )
  class(ans) <- "forest_composition_result"
  ans
}

#' @export
print.forest_composition_result <- function(x, ...) {
  cat("NepInventory forest composition\n")
  cat("-------------------------------\n")
  print(x$result, row.names = FALSE)
  cat("\n", x$interpretation, "\n", sep = "")
  invisible(x)
}
