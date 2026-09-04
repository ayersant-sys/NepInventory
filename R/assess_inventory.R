#' Assess forest inventory sampling intensity and precision
#'
#' @description
#' Post-inventory assessment for equal-area tree plots using plot-level forest
#' attributes. Precision calculations use the simple-random-sampling (SRS)
#' variance formula. For systematic/fishnet inventories, this is reported as an
#' SRS approximation rather than an exact design-based variance estimator.
#'
#' @param data A data.frame with one row per plot, or a path to a `.csv`, `.xlsx`,
#'   or `.xls` inventory file. File input should contain `forest_area_ha`,
#'   `plot_area_m2`, and `design` as constant columns across plots.
#' @param forest_area_ha Total forest area in hectares. Optional when supplied
#'   as a constant `forest_area_ha` column in a file.
#' @param plot_area_m2 Area of one fixed plot in square metres. Optional when
#'   supplied as a constant `plot_area_m2` column in a file.
#' @param variables Character vector of numeric plot-level variables to assess.
#'   If NULL, all numeric columns except metadata columns are used.
#' @param confidence Confidence level, default 0.95.
#' @param precision_targets Relative-error targets in percent, default c(5,10,15).
#' @param guideline_intensity_pct Sampling-intensity benchmark in percent. The default is 0.5, used as the general Nepal Community Forest reference benchmark; users may change this value when another benchmark is appropriate.
#' @param design Either "srs" or "systematic". For file input, if omitted, the
#'   value is read from the constant `design` column. Systematic uses the SRS
#'   variance formula as an approximation and is reported accordingly.
#' @param fpc Logical. Apply a finite population correction using
#'   floor(forest area / plot area) as the finite frame size. Default FALSE.
#'
#' @return An object of class `forest_adequacy` containing inventory metadata,
#'   an attribute-level precision summary, required plot counts, target-status indicators, and notes.
#' @export
assess_inventory <- function(data,
                             forest_area_ha = NULL,
                             plot_area_m2 = NULL,
                             variables = NULL,
                             confidence = 0.95,
                             precision_targets = c(5, 10, 15),
                             guideline_intensity_pct = 0.5,
                             design = NULL,
                             fpc = FALSE) {
  file_input <- is.character(data) && length(data) == 1L
  if (file_input) {
    data <- .read_inventory_file(data)
  }
  if (!is.data.frame(data)) stop("`data` must be a data.frame or a CSV/Excel file path.", call. = FALSE)
  if (nrow(data) < 2L) stop("At least two plots are required.", call. = FALSE)

  if (is.null(forest_area_ha)) forest_area_ha <- .extract_inventory_metadata(data, "forest_area_ha")
  if (is.null(plot_area_m2)) plot_area_m2 <- .extract_inventory_metadata(data, "plot_area_m2")
  if (is.null(design)) {
    design <- if ("design" %in% names(data)) .extract_inventory_design(data) else "srs"
  }

  .validate_scalar_positive(forest_area_ha, "forest_area_ha")
  .validate_scalar_positive(plot_area_m2, "plot_area_m2")
  .validate_pct(precision_targets, "precision_targets")
  .validate_pct(guideline_intensity_pct, "guideline_intensity_pct")

  if (length(confidence) != 1L || !is.numeric(confidence) || is.na(confidence) ||
      confidence <= 0 || confidence >= 1) {
    stop("`confidence` must be a single number between 0 and 1.", call. = FALSE)
  }

  design <- match.arg(design, c("srs", "systematic"))
  if (!is.logical(fpc) || length(fpc) != 1L || is.na(fpc)) {
    stop("`fpc` must be TRUE or FALSE.", call. = FALSE)
  }

  metadata_cols <- intersect(c("forest_area_ha", "plot_area_m2"), names(data))
  if (is.null(variables)) {
    variables <- names(data)[vapply(data, is.numeric, logical(1))]
    variables <- setdiff(variables, metadata_cols)
  }
  if (!length(variables)) stop("No numeric variables were selected.", call. = FALSE)
  missing_vars <- setdiff(variables, names(data))
  if (length(missing_vars)) {
    stop(sprintf("Variables not found: %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
  }
  non_numeric <- variables[!vapply(data[variables], is.numeric, logical(1))]
  if (length(non_numeric)) {
    stop(sprintf("Selected variables must be numeric: %s", paste(non_numeric, collapse = ", ")), call. = FALSE)
  }

  n_current <- nrow(data)
  sampled_area_ha <- n_current * plot_area_m2 / 10000
  observed_intensity_pct <- 100 * sampled_area_ha / forest_area_ha
  guideline_sampled_area_ha <- forest_area_ha * guideline_intensity_pct / 100
  guideline_required_plots <- ceiling(guideline_sampled_area_ha / (plot_area_m2 / 10000))
  guideline_met <- observed_intensity_pct >= guideline_intensity_pct

  N_frame <- floor(forest_area_ha * 10000 / plot_area_m2)
  if (N_frame < n_current) {
    stop("Current plot count exceeds the finite frame implied by forest and plot areas.", call. = FALSE)
  }
  N_for_calc <- if (fpc) N_frame else Inf

  rows <- lapply(variables, function(v) {
    sm <- .metric_summary(data[[v]], confidence, N_for_calc, fpc, precision_targets)
    out <- data.frame(
      variable = v,
      n_used = sm$n,
      mean = sm$mean,
      sd = sm$sd,
      cv_pct = sm$cv_pct,
      se = sm$se,
      ci_low = sm$ci_low,
      ci_high = sm$ci_high,
      relative_error_pct = sm$relative_error_pct,
      stringsAsFactors = FALSE
    )
    for (p in precision_targets) {
      req <- unname(sm$required[as.character(p)])
      out[[paste0("n_required_", gsub("\\.", "_", as.character(p)), "pct")]] <- req
      tag <- gsub("\\.", "_", as.character(p))
      out[[paste0("additional_", tag, "pct")]] <- if (is.na(req)) NA_integer_ else max(0L, req - n_current)
      out[[paste0("status_", tag, "pct")]] <- if (is.na(sm$relative_error_pct)) {
        NA_character_
      } else if (sm$relative_error_pct <= p) {
        "MET"
      } else {
        "NOT MET"
      }
    }
    out
  })
  summary_tbl <- do.call(rbind, rows)
  rownames(summary_tbl) <- NULL

  notes <- c(
    "Precision is based on between-plot variability of the selected plot-level attributes.",
    "Required plot counts assume the currently observed coefficient of variation remains representative as sample size increases.",
    "The current release assumes one common fixed-plot area and does not implement nested-plot, variable-area, or combined stratified estimators."
  )
  if (design == "systematic") {
    notes <- c(notes, "Systematic/fishnet inventories are evaluated using the SRS variance formula as an approximation; spatial ordering is not explicitly modeled.")
  }
  if (!fpc) {
    notes <- c(notes, "Finite population correction is not applied by default.")
  }

  result <- list(
    call = match.call(),
    design = design,
    confidence = confidence,
    precision_targets = precision_targets,
    inventory = data.frame(
      forest_area_ha = forest_area_ha,
      plot_area_m2 = plot_area_m2,
      current_plots = n_current,
      sampled_area_ha = sampled_area_ha,
      observed_sampling_intensity_pct = observed_intensity_pct,
      guideline_intensity_pct = guideline_intensity_pct,
      guideline_required_plots = guideline_required_plots,
      guideline_met = guideline_met,
      stringsAsFactors = FALSE
    ),
    summary = summary_tbl,
    notes = notes,
    data = data,
    variables = variables,
    fpc = fpc,
    finite_frame_N = N_frame
  )
  class(result) <- "forest_adequacy"
  result
}

#' @export
print.forest_adequacy <- function(x, ...) {
  inv <- x$inventory
  cat("NepInventory - Inventory Precision Assessment\n")
  cat("=============================================\n\n")
  cat("Inventory\n")
  cat(sprintf("Forest area:                 %.2f ha\n", inv$forest_area_ha))
  cat(sprintf("Fixed plot area:             %.0f m2\n", inv$plot_area_m2))
  cat(sprintf("Plots measured:              %d\n", inv$current_plots))
  cat(sprintf("Observed sampling intensity: %.4f%%\n", inv$observed_sampling_intensity_pct))
  cat(sprintf("Nepal CF general reference:  %.4f%%   [%s]\n\n",
              inv$guideline_intensity_pct,
              if (isTRUE(inv$guideline_met)) "MET" else "NOT MET"))

  target_tags <- gsub("\\.", "_", as.character(x$precision_targets))
  target_cols <- paste0("n_required_", target_tags, "pct")
  status_cols <- paste0("status_", target_tags, "pct")
  compact <- data.frame(
    Attribute = .pretty_variable(x$summary$variable),
    Error = sprintf("%.2f%%", x$summary$relative_error_pct),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (i in seq_along(x$precision_targets)) {
    compact[[paste0(x$precision_targets[i], "%")]] <- x$summary[[status_cols[i]]]
    compact[[paste0("n(", x$precision_targets[i], "%)")]] <- x$summary[[target_cols[i]]]
  }

  cat("Precision assessment\n")
  cat("--------------------\n")
  print(compact, row.names = FALSE, right = FALSE)
  cat(sprintf("\nCurrent n = %d\n\n", inv$current_plots))

  cat("Interpretation\n")
  cat(sprintf("- Nepal CF general sampling-intensity reference: %s.\n",
              if (isTRUE(inv$guideline_met)) "met" else "not met"))
  cat("- Precision differs among inventory attributes; meeting the benchmark does not guarantee a chosen statistical precision.\n")
  if (x$design == "systematic") {
    cat("- Systematic/fishnet design: precision uses an SRS variance approximation.\n")
  }
  cat("\nUse `$summary` for the full statistical table.\n")
  invisible(x)
}
