#' Forest density analysis
#'
#' Calculates total and taxon-specific stem density (stems per hectare) from
#' long-form inventory data. Relative density is also returned for the selected
#' taxonomic level. Counts are standardized using `sample_plot_size_m2`.
#'
#' @param data A data.frame or CSV/Excel file path using the NepInventory
#'   long-form input convention.
#' @param by Optional character vector of grouping columns.
#' @param taxon Taxonomic column used for the breakdown. One of
#'   `species_name`, `genus`, or `family` when available.
#' @param plant_category Optional plant-category filter.
#'
#' @return An object of class `forest_density_result` with `result`,
#'   `plot_data`, `figure_data`, `interpretation`, and `settings`.
#' @export
forest_density <- function(data, by = NULL, taxon = "species_name", plant_category = NULL) {
  x <- .prepare_longform_inventory(data)
  d <- .m1_filter_category(x$records, plant_category)
  by <- .m1_validate_by(d, by)
  .m1_require(d, c(taxon, "count", "sample_plot_size_m2"), "Forest density")

  usable <- !is.na(d$count) & !is.na(d$sample_plot_size_m2)
  if (!any(usable)) stop("Forest density cannot be calculated because no usable count/area records were found.", call. = FALSE)
  d <- d[usable, , drop = FALSE]
  d <- .m1_add_plot_key(d)
  category_col <- .m1_add_category(d)
  if (!is.null(category_col)) d$.plant_category <- ifelse(is.na(d$plant_category), "Unclassified", d$plant_category)

  plot_base <- .m1_support_table(d, by, category_col)
  total_keys <- c(".plot_key", by, category_col)
  total_count <- stats::aggregate(d$count, d[total_keys], sum, na.rm = TRUE)
  names(total_count)[ncol(total_count)] <- "count"
  total_plot <- merge(plot_base, total_count, by = total_keys, all.x = TRUE, sort = FALSE)
  total_plot$count[is.na(total_plot$count)] <- 0
  total_plot$density_ha <- total_plot$count / total_plot$sample_plot_size_m2 * 10000

  named <- !is.na(d[[taxon]]) & nzchar(trimws(as.character(d[[taxon]])))
  taxa <- sort(unique(as.character(d[[taxon]][named])))
  taxon_plot <- NULL

  if (length(taxa)) {
    frames <- lapply(taxa, function(tt) {
      z <- plot_base
      z[[taxon]] <- tt
      z
    })
    frame <- do.call(rbind, frames)
    dn <- d[named, , drop = FALSE]
    keys <- c(".plot_key", by, category_col, taxon)
    cnt <- stats::aggregate(dn$count, dn[keys], sum, na.rm = TRUE)
    names(cnt)[ncol(cnt)] <- "count"
    taxon_plot <- merge(frame, cnt, by = keys, all.x = TRUE, sort = FALSE)
    taxon_plot$count[is.na(taxon_plot$count)] <- 0
    taxon_plot$density_ha <- taxon_plot$count / taxon_plot$sample_plot_size_m2 * 10000
  }

  summary_keys <- c(by, category_col)
  total_summary <- .m1_summary_numeric(total_plot, "density_ha", summary_keys, "density_ha")
  taxon_summary <- NULL
  if (!is.null(taxon_plot)) {
    taxon_summary <- .m1_summary_numeric(taxon_plot, "density_ha", c(summary_keys, taxon), "density_ha")

    rel_keys <- summary_keys
    if (!length(rel_keys)) {
      den <- sum(taxon_summary$mean_density_ha, na.rm = TRUE)
      taxon_summary$relative_density_pct <- if (den > 0) taxon_summary$mean_density_ha / den * 100 else NA_real_
    } else {
      rk <- interaction(taxon_summary[rel_keys], drop = TRUE, lex.order = TRUE, sep = "\r")
      taxon_summary$relative_density_pct <- ave(
        taxon_summary$mean_density_ha, rk,
        FUN = function(v) if (sum(v, na.rm = TRUE) > 0) v / sum(v, na.rm = TRUE) * 100 else NA_real_
      )
    }
  }

  total_summary <- .m1_restore_names(total_summary)
  taxon_summary <- .m1_restore_names(taxon_summary)
  total_plot <- .m1_restore_names(total_plot)
  taxon_plot <- .m1_restore_names(taxon_plot)

  fig <- taxon_summary
  if (!is.null(fig)) {
    keep <- intersect(c(by, "plant_category", taxon, "mean_density_ha", "relative_density_pct"), names(fig))
    fig <- fig[keep]
  }

  nplots <- length(unique(d$.plot_key))
  interpretation <- sprintf(
    "Forest density was estimated from %d sampled plot%s and standardized to stems per hectare using the sampled support area. Relative density expresses each taxon's share of total mean density.",
    nplots, if (nplots == 1L) "" else "s"
  )

  out <- list(
    result = list(total = total_summary, by_taxon = taxon_summary),
    plot_data = list(total = total_plot, by_taxon = taxon_plot),
    figure_data = fig,
    interpretation = interpretation,
    settings = list(by = by, taxon = taxon, plant_category = plant_category)
  )
  class(out) <- "forest_density_result"
  out
}

#' @export
print.forest_density_result <- function(x, ...) {
  cat("NepInventory forest density\n")
  cat("---------------------------\n")
  print(x$result$total, row.names = FALSE)
  if (!is.null(x$result$by_taxon)) {
    cat("\nTaxon-specific density:\n")
    print(x$result$by_taxon, row.names = FALSE)
  }
  cat("\n", x$interpretation, "\n", sep = "")
  invisible(x)
}
