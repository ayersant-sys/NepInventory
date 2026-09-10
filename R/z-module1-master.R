# Final Module 1 orchestration. This file is collated after the component files
# and therefore defines the public master runner used by the package.

#' Run Module 1: forest structure, composition and diversity
#'
#' Runs every Module 1 analysis supported by the supplied one-sheet inventory.
#' Missing optional inputs skip only the dependent analysis. Any component-level
#' error is captured as a skipped-analysis reason so the overall module remains
#' adaptive rather than failing as a whole.
#'
#' In addition to the component analysis objects, the returned object contains
#' consolidated tables, exact figure-source data, plotting specifications, and
#' deterministic written results. These outputs are designed to feed future
#' Excel and GUI layers without recalculating the underlying forest metrics.
#'
#' @param data A long-form NepInventory dataset or CSV/Excel file path.
#' @param by Optional character vector of grouping columns.
#' @param plant_category Optional plant-category filter.
#' @return An object of class `forest_structure_analysis`.
#' @export
structure_analysis <- function(data, by = NULL, plant_category = NULL) {
  x <- .prepare_longform_inventory(data)
  by <- .m1_validate_by(x$records, by)

  out <- list()
  skipped <- character(0)

  run_component <- function(name, expr) {
    value <- tryCatch(expr, error = function(e) e)
    if (inherits(value, "error")) {
      skipped[name] <<- conditionMessage(value)
    } else {
      out[[name]] <<- value
    }
    invisible(NULL)
  }

  # Composition can run without sampled area.
  if (isTRUE(x$availability$analyses$composition)) {
    run_component("composition", forest_composition(data, by = by, plant_category = plant_category))
  } else skipped["composition"] <- "Requires species_name and positive counts."

  if (isTRUE(x$availability$analyses$density)) {
    run_component("density", forest_density(data, by = by, plant_category = plant_category))
  } else skipped["density"] <- "Requires species_name, positive counts and sample_plot_size_m2."

  if (isTRUE(x$availability$species) && isTRUE(x$availability$count)) {
    run_component("frequency", forest_frequency(data, by = by, plant_category = plant_category))
  } else skipped["frequency"] <- "Requires species_name and positive counts."

  if (isTRUE(x$availability$dbh) && isTRUE(x$availability$sample_plot_size)) {
    run_component("basal_area", forest_basal_area(data, by = by, plant_category = plant_category))
  } else skipped["basal_area"] <- "Requires dbh_cm and sample_plot_size_m2."

  if (isTRUE(x$availability$analyses$ivi)) {
    run_component("ivi", forest_ivi(data, by = by, plant_category = plant_category))
  } else skipped["ivi"] <- "Requires species_name, positive counts, sample_plot_size_m2 and dbh_cm."

  if (isTRUE(x$availability$analyses$diversity)) {
    run_component("diversity", forest_diversity(data, by = by, plant_category = plant_category))
  } else skipped["diversity"] <- "Requires species_name and positive counts."

  if (isTRUE(x$availability$analyses$stand_structure)) {
    run_component("stand_structure", forest_structure(data, by = by, plant_category = plant_category))
  } else skipped["stand_structure"] <- "Requires dbh_cm and/or height_m."

  if (isTRUE(x$availability$analyses$regeneration)) {
    run_component("regeneration", forest_regeneration(data, by = by, plant_category = plant_category))
  } else skipped["regeneration"] <- "Requires plant_category, species_name, positive counts and sample_plot_size_m2."

  tables <- .m1_collect_tables(out)
  figure_data <- .m1_collect_figure_data(out)
  figure_specs <- .m1_figure_specs(out)
  results_text <- .m1_results_text(out, skipped)

  ans <- list(
    analyses = out,
    tables = tables,
    figure_data = figure_data,
    figure_specs = figure_specs,
    results_text = results_text,
    skipped = skipped,
    availability = x$availability,
    interpretation = sprintf(
      "Module 1 completed %d supported analysis component%s. Missing or unsuitable optional inputs affected only the dependent outputs. Consolidated tables, figure-source data, and written results are available directly from this object.",
      length(out), if (length(out) == 1L) "" else "s"
    ),
    settings = list(by = by, plant_category = plant_category)
  )
  class(ans) <- "forest_structure_analysis"
  ans
}

#' @export
print.forest_structure_analysis <- function(x, ...) {
  cat("NepInventory Module 1: Forest Structure, Composition & Diversity\n")
  cat("----------------------------------------------------------------\n")
  cat("Completed:", if (length(x$analyses)) paste(names(x$analyses), collapse = ", ") else "none", "\n")
  if (length(x$skipped)) {
    cat("Skipped:\n")
    for (nm in names(x$skipped)) cat(" - ", nm, ": ", x$skipped[[nm]], "\n", sep = "")
  }
  cat("Tables:", length(x$tables), "\n")
  cat("Figures available:", if (length(x$figure_specs)) paste(names(x$figure_specs), collapse = ", ") else "none", "\n")
  cat("\n", x$interpretation, "\n", sep = "")
  invisible(x)
}
