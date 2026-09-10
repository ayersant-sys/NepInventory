#' Forest frequency analysis
#'
#' Calculates absolute frequency (percentage of sampled plots in which a taxon
#' occurs) and relative frequency for a selected taxonomic level.
#'
#' @inheritParams forest_density
#' @return An object of class `forest_frequency_result`.
#' @export
forest_frequency <- function(data, by = NULL, taxon = "species_name", plant_category = NULL) {
  x <- .prepare_longform_inventory(data)
  d <- .m1_filter_category(x$records, plant_category)
  by <- .m1_validate_by(d, by)
  .m1_require(d, c(taxon, "count"), "Forest frequency")
  d <- .m1_add_plot_key(d)
  category_col <- .m1_add_category(d)
  if (!is.null(category_col)) d$.plant_category <- ifelse(is.na(d$plant_category), "Unclassified", d$plant_category)

  named <- !is.na(d[[taxon]]) & !is.na(d$count) & d$count > 0
  taxa <- sort(unique(as.character(d[[taxon]][named])))
  if (!length(taxa)) stop("Forest frequency cannot be calculated because no positive-count taxon records were found.", call. = FALSE)

  sample_keys <- c(".plot_key", by, category_col)
  sampled <- unique(d[sample_keys])
  presence <- unique(d[named, c(sample_keys, taxon), drop = FALSE])

  frames <- lapply(taxa, function(tt) {
    z <- sampled
    z[[taxon]] <- tt
    z
  })
  frame <- do.call(rbind, frames)
  pkey <- do.call(paste, c(presence[c(sample_keys, taxon)], sep = "\r"))
  fkey <- do.call(paste, c(frame[c(sample_keys, taxon)], sep = "\r"))
  frame$present <- as.integer(fkey %in% pkey)

  keys <- c(by, category_col, taxon)
  one <- function(z) {
    n <- length(unique(z$.plot_key))
    np <- sum(z$present)
    data.frame(n_sampled_plots = n, n_present_plots = np,
               frequency_pct = if (n > 0) np / n * 100 else NA_real_)
  }
  out <- .m1_split_apply(frame, keys, one)

  rel_keys <- c(by, category_col)
  if (!length(rel_keys)) {
    den <- sum(out$frequency_pct, na.rm = TRUE)
    out$relative_frequency_pct <- if (den > 0) out$frequency_pct / den * 100 else NA_real_
  } else {
    rk <- interaction(out[rel_keys], drop = TRUE, lex.order = TRUE, sep = "\r")
    out$relative_frequency_pct <- ave(out$frequency_pct, rk,
      FUN = function(v) if (sum(v, na.rm = TRUE) > 0) v / sum(v, na.rm = TRUE) * 100 else NA_real_)
  }
  out <- .m1_restore_names(out)
  frame <- .m1_restore_names(frame)

  ans <- list(
    result = out,
    plot_data = frame,
    figure_data = out,
    interpretation = "Frequency is the percentage of sampled plots in which each taxon occurred. Relative frequency expresses each taxon's frequency as a percentage of the summed frequencies of all taxa.",
    settings = list(by = by, taxon = taxon, plant_category = plant_category)
  )
  class(ans) <- "forest_frequency_result"
  ans
}

#' Forest basal area and dominance analysis
#'
#' Calculates basal area from DBH and summarizes basal area per hectare overall
#' and by taxon. Relative dominance is based on taxon basal area per hectare.
#'
#' @inheritParams forest_density
#' @return An object of class `forest_basal_area_result`.
#' @export
forest_basal_area <- function(data, by = NULL, taxon = "species_name", plant_category = NULL) {
  x <- .prepare_longform_inventory(data)
  d <- .m1_filter_category(x$records, plant_category)
  by <- .m1_validate_by(d, by)
  .m1_require(d, c("dbh_cm", "sample_plot_size_m2", "count"), "Basal area")
  d <- .m1_add_plot_key(d)
  category_col <- .m1_add_category(d)
  if (!is.null(category_col)) d$.plant_category <- ifelse(is.na(d$plant_category), "Unclassified", d$plant_category)

  usable <- !is.na(d$dbh_cm) & !is.na(d$sample_plot_size_m2) & !is.na(d$count) & d$count > 0
  if (!any(usable)) stop("Basal area cannot be calculated because no usable DBH/count/area records were found.", call. = FALSE)
  d <- d[usable, , drop = FALSE]

  d$basal_area_m2 <- pi * d$dbh_cm^2 / 40000 * d$count
  plot_base <- .m1_support_table(d, by, category_col)
  total_keys <- c(".plot_key", by, category_col)
  ba <- stats::aggregate(d$basal_area_m2, d[total_keys], sum, na.rm = TRUE)
  names(ba)[ncol(ba)] <- "basal_area_m2"
  total_plot <- merge(plot_base, ba, by = total_keys, all.x = TRUE, sort = FALSE)
  total_plot$basal_area_ha <- total_plot$basal_area_m2 / total_plot$sample_plot_size_m2 * 10000
  total_summary <- .m1_summary_numeric(total_plot, "basal_area_ha", c(by, category_col), "basal_area_ha")

  taxon_summary <- taxon_plot <- NULL
  if (taxon %in% names(d)) {
    named <- !is.na(d[[taxon]])
    if (any(named)) {
      taxa <- sort(unique(as.character(d[[taxon]][named])))
      frames <- lapply(taxa, function(tt) { z <- plot_base; z[[taxon]] <- tt; z })
      frame <- do.call(rbind, frames)
      dn <- d[named, , drop = FALSE]
      keys <- c(".plot_key", by, category_col, taxon)
      z <- stats::aggregate(dn$basal_area_m2, dn[keys], sum, na.rm = TRUE)
      names(z)[ncol(z)] <- "basal_area_m2"
      taxon_plot <- merge(frame, z, by = keys, all.x = TRUE, sort = FALSE)
      taxon_plot$basal_area_m2[is.na(taxon_plot$basal_area_m2)] <- 0
      taxon_plot$basal_area_ha <- taxon_plot$basal_area_m2 / taxon_plot$sample_plot_size_m2 * 10000
      taxon_summary <- .m1_summary_numeric(taxon_plot, "basal_area_ha", c(by, category_col, taxon), "basal_area_ha")

      rel_keys <- c(by, category_col)
      if (!length(rel_keys)) {
        den <- sum(taxon_summary$mean_basal_area_ha, na.rm = TRUE)
        taxon_summary$relative_dominance_pct <- if (den > 0) taxon_summary$mean_basal_area_ha / den * 100 else NA_real_
      } else {
        rk <- interaction(taxon_summary[rel_keys], drop = TRUE, lex.order = TRUE, sep = "\r")
        taxon_summary$relative_dominance_pct <- ave(taxon_summary$mean_basal_area_ha, rk,
          FUN = function(v) if (sum(v, na.rm = TRUE) > 0) v / sum(v, na.rm = TRUE) * 100 else NA_real_)
      }
    }
  }

  total_summary <- .m1_restore_names(total_summary)
  taxon_summary <- .m1_restore_names(taxon_summary)
  total_plot <- .m1_restore_names(total_plot)
  taxon_plot <- .m1_restore_names(taxon_plot)

  ans <- list(
    result = list(total = total_summary, by_taxon = taxon_summary),
    plot_data = list(total = total_plot, by_taxon = taxon_plot),
    figure_data = taxon_summary,
    interpretation = "Basal area was calculated from DBH and standardized to square metres per hectare using the sampled support area. Relative dominance is each taxon's share of total mean basal area.",
    settings = list(by = by, taxon = taxon, plant_category = plant_category)
  )
  class(ans) <- "forest_basal_area_result"
  ans
}

#' Forest importance value index analysis
#'
#' Combines relative density, relative frequency, and relative dominance into
#' the conventional tree-layer importance value index (IVI; maximum 300).
#'
#' @inheritParams forest_density
#' @return An object of class `forest_ivi_result`.
#' @export
forest_ivi <- function(data, by = NULL, taxon = "species_name", plant_category = NULL) {
  den <- forest_density(data, by = by, taxon = taxon, plant_category = plant_category)$result$by_taxon
  fre <- forest_frequency(data, by = by, taxon = taxon, plant_category = plant_category)$result
  dom <- forest_basal_area(data, by = by, taxon = taxon, plant_category = plant_category)$result$by_taxon
  if (is.null(den) || is.null(dom)) stop("IVI requires taxon, count, sampled area, and DBH information.", call. = FALSE)

  common <- intersect(c(by, "plant_category", taxon), Reduce(intersect, list(names(den), names(fre), names(dom))))
  keep_den <- c(common, "relative_density_pct")
  keep_fre <- c(common, "relative_frequency_pct")
  keep_dom <- c(common, "relative_dominance_pct")
  out <- merge(den[keep_den], fre[keep_fre], by = common, all = TRUE, sort = FALSE)
  out <- merge(out, dom[keep_dom], by = common, all = TRUE, sort = FALSE)
  out$ivi <- out$relative_density_pct + out$relative_frequency_pct + out$relative_dominance_pct
  out <- out[order(out$ivi, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL

  ans <- list(
    result = out,
    plot_data = NULL,
    figure_data = out,
    interpretation = "IVI combines relative density, relative frequency, and relative dominance. Higher values indicate greater structural and compositional importance within the analyzed vegetation layer.",
    settings = list(by = by, taxon = taxon, plant_category = plant_category)
  )
  class(ans) <- "forest_ivi_result"
  ans
}

#' Forest diversity analysis
#'
#' Calculates plot-level taxon richness, Shannon diversity, Simpson diversity
#' (1 - sum p^2), and Pielou evenness, with optional grouped summaries.
#'
#' @inheritParams forest_density
#' @return An object of class `forest_diversity_result`.
#' @export
forest_diversity <- function(data, by = NULL, taxon = "species_name", plant_category = NULL) {
  x <- .prepare_longform_inventory(data)
  d <- .m1_filter_category(x$records, plant_category)
  by <- .m1_validate_by(d, by)
  .m1_require(d, c(taxon, "count"), "Forest diversity")
  d <- .m1_add_plot_key(d)
  category_col <- .m1_add_category(d)
  if (!is.null(category_col)) d$.plant_category <- ifelse(is.na(d$plant_category), "Unclassified", d$plant_category)

  plot_keys <- c(".plot_key", "plot_id", intersect("boundary_id", names(d)), by, category_col)
  sample_units <- unique(d[plot_keys])
  calc_one <- function(z) {
    z <- z[!is.na(z[[taxon]]) & !is.na(z$count) & z$count > 0, , drop = FALSE]
    if (!nrow(z)) return(data.frame(richness = 0, shannon = 0, simpson = 0, pielou = NA_real_))
    counts <- tapply(z$count, z[[taxon]], sum, na.rm = TRUE)
    counts <- counts[counts > 0]
    S <- length(counts)
    p <- counts / sum(counts)
    H <- -sum(p * log(p))
    sim <- 1 - sum(p^2)
    J <- if (S > 1) H / log(S) else NA_real_
    data.frame(richness = S, shannon = H, simpson = sim, pielou = J)
  }

  pk <- interaction(d[c(".plot_key", category_col)], drop = TRUE, lex.order = TRUE, sep = "\r")
  chunks <- split(d, pk, drop = TRUE)
  rows <- lapply(chunks, function(z) cbind(z[1, plot_keys, drop = FALSE], calc_one(z)))
  plot_data <- do.call(rbind, rows)
  rownames(plot_data) <- NULL

  summary_keys <- c(by, category_col)
  one_summary <- function(z) {
    data.frame(
      n_plots = nrow(z),
      mean_richness = mean(z$richness, na.rm = TRUE),
      sd_richness = if (nrow(z) > 1) stats::sd(z$richness, na.rm = TRUE) else NA_real_,
      mean_shannon = mean(z$shannon, na.rm = TRUE),
      sd_shannon = if (nrow(z) > 1) stats::sd(z$shannon, na.rm = TRUE) else NA_real_,
      mean_simpson = mean(z$simpson, na.rm = TRUE),
      sd_simpson = if (nrow(z) > 1) stats::sd(z$simpson, na.rm = TRUE) else NA_real_,
      mean_pielou = if (all(is.na(z$pielou))) NA_real_ else mean(z$pielou, na.rm = TRUE),
      sd_pielou = if (sum(!is.na(z$pielou)) > 1) stats::sd(z$pielou, na.rm = TRUE) else NA_real_
    )
  }
  result <- .m1_split_apply(plot_data, summary_keys, one_summary)
  result <- .m1_restore_names(result)
  plot_data <- .m1_restore_names(plot_data)

  ans <- list(
    result = result,
    plot_data = plot_data,
    figure_data = plot_data,
    interpretation = "Diversity was calculated at the sampled-plot level. Shannon uses natural logarithms, Simpson is reported as 1 - sum(p^2), and Pielou evenness is Shannon divided by log(richness).",
    settings = list(by = by, taxon = taxon, plant_category = plant_category)
  )
  class(ans) <- "forest_diversity_result"
  ans
}

#' Forest stand-structure analysis
#'
#' Summarizes DBH and height, calculates quadratic mean diameter (QMD), and
#' creates a DBH-class table when DBH is available.
#'
#' @param data A long-form NepInventory dataset or file path.
#' @param by Optional grouping columns.
#' @param plant_category Optional plant-category filter.
#' @param dbh_breaks Optional numeric DBH class boundaries. When omitted,
#'   approximately 5-cm classes are generated from the observed range.
#' @return An object of class `forest_structure_result`.
#' @export
forest_structure <- function(data, by = NULL, plant_category = NULL, dbh_breaks = NULL) {
  x <- .prepare_longform_inventory(data)
  d <- .m1_filter_category(x$records, plant_category)
  by <- .m1_validate_by(d, by)
  d <- .m1_add_plot_key(d)
  category_col <- .m1_add_category(d)
  if (!is.null(category_col)) d$.plant_category <- ifelse(is.na(d$plant_category), "Unclassified", d$plant_category)
  keys <- c(by, category_col)

  has_dbh <- "dbh_cm" %in% names(d) && any(!is.na(d$dbh_cm))
  has_height <- "height_m" %in% names(d) && any(!is.na(d$height_m))
  if (!has_dbh && !has_height) stop("Forest structure requires `dbh_cm` and/or `height_m`.", call. = FALSE)
  if (!"count" %in% names(d)) d$count <- 1

  calc <- function(z) {
    out <- data.frame(n_records = nrow(z))
    if (has_dbh) {
      q <- z[!is.na(z$dbh_cm) & !is.na(z$count) & z$count > 0, , drop = FALSE]
      if (nrow(q)) {
        out$n_stems_dbh <- sum(q$count)
        out$mean_dbh_cm <- weighted.mean(q$dbh_cm, q$count)
        out$median_dbh_cm <- stats::median(rep(q$dbh_cm, pmax(1L, as.integer(round(q$count)))))
        out$qmd_cm <- sqrt(sum(q$count * q$dbh_cm^2) / sum(q$count))
      } else {
        out$n_stems_dbh <- 0; out$mean_dbh_cm <- NA_real_; out$median_dbh_cm <- NA_real_; out$qmd_cm <- NA_real_
      }
    }
    if (has_height) {
      q <- z[!is.na(z$height_m) & !is.na(z$count) & z$count > 0, , drop = FALSE]
      if (nrow(q)) {
        out$n_stems_height <- sum(q$count)
        out$mean_height_m <- weighted.mean(q$height_m, q$count)
        out$min_height_m <- min(q$height_m)
        out$max_height_m <- max(q$height_m)
      } else {
        out$n_stems_height <- 0; out$mean_height_m <- NA_real_; out$min_height_m <- NA_real_; out$max_height_m <- NA_real_
      }
    }
    out
  }
  result <- .m1_split_apply(d, keys, calc)
  result <- .m1_restore_names(result)

  dbh_classes <- NULL
  if (has_dbh) {
    q <- d[!is.na(d$dbh_cm) & !is.na(d$count) & d$count > 0, , drop = FALSE]
    if (nrow(q)) {
      if (is.null(dbh_breaks)) {
        lo <- floor(min(q$dbh_cm) / 5) * 5
        hi <- ceiling(max(q$dbh_cm) / 5) * 5
        if (hi <= lo) hi <- lo + 5
        dbh_breaks <- seq(lo, hi + 5, by = 5)
      }
      if (!is.numeric(dbh_breaks) || length(dbh_breaks) < 2L || any(diff(dbh_breaks) <= 0)) {
        stop("`dbh_breaks` must be a strictly increasing numeric vector.", call. = FALSE)
      }
      q$dbh_class <- cut(q$dbh_cm, breaks = dbh_breaks, include.lowest = TRUE, right = FALSE)
      ckeys <- c(keys, "dbh_class")
      dbh_classes <- stats::aggregate(q$count, q[ckeys], sum, na.rm = TRUE)
      names(dbh_classes)[ncol(dbh_classes)] <- "stem_count"
      dbh_classes <- .m1_restore_names(dbh_classes)
    }
  }

  ans <- list(
    result = result,
    plot_data = d,
    figure_data = dbh_classes,
    dbh_classes = dbh_classes,
    interpretation = "Stand structure summarizes available DBH and height measurements. QMD is the count-weighted quadratic mean diameter; DBH classes use observed stems represented by each record.",
    settings = list(by = by, plant_category = plant_category, dbh_breaks = dbh_breaks)
  )
  class(ans) <- "forest_structure_result"
  ans
}

#' Forest regeneration analysis
#'
#' Summarizes regeneration density by plant category and taxon. Categories are
#' taken exactly as recorded; guideline-based regeneration status belongs to the
#' Nepal Operational Plan module rather than this general ecological function.
#'
#' @inheritParams forest_density
#' @return An object of class `forest_regeneration_result`.
#' @export
forest_regeneration <- function(data, by = NULL, taxon = "species_name", plant_category = NULL) {
  x <- .prepare_longform_inventory(data)
  d <- x$records
  by <- .m1_validate_by(d, by)
  .m1_require(d, c("plant_category", "count", "sample_plot_size_m2", taxon), "Regeneration")
  cats <- unique(d$plant_category[!is.na(d$plant_category)])
  if (!length(cats)) stop("Regeneration analysis requires non-missing `plant_category` values.", call. = FALSE)
  if (is.null(plant_category)) plant_category <- cats

  den <- forest_density(d, by = by, taxon = taxon, plant_category = plant_category)
  ans <- list(
    result = den$result,
    plot_data = den$plot_data,
    figure_data = den$figure_data,
    interpretation = "Regeneration is summarized using the plant categories supplied by the user and standardized to stems per hectare with category-specific sampled support areas. No Nepal guideline status class is imposed in this general ecological module.",
    settings = list(by = by, taxon = taxon, plant_category = plant_category)
  )
  class(ans) <- "forest_regeneration_result"
  ans
}

#' Run Module 1: forest structure, composition and diversity
#'
#' Runs every Module 1 analysis supported by the columns supplied in the
#' one-sheet NepInventory input. Unsupported analyses are skipped with a reason
#' rather than causing the whole module to fail.
#'
#' @param data A long-form NepInventory dataset or file path.
#' @param by Optional grouping columns.
#' @param plant_category Optional plant-category filter.
#' @return An object of class `forest_structure_analysis`.
#' @export
structure_analysis <- function(data, by = NULL, plant_category = NULL) {
  x <- .prepare_longform_inventory(data)
  by <- .m1_validate_by(x$records, by)
  a <- x$availability$analyses
  out <- list()
  skipped <- character(0)

  if (isTRUE(a$density)) out$density <- forest_density(data, by = by, plant_category = plant_category) else skipped["density"] <- "Requires species_name, count and sample_plot_size_m2."
  if (isTRUE(a$frequency)) out$frequency <- forest_frequency(data, by = by, plant_category = plant_category) else skipped["frequency"] <- "Requires species_name."
  if (isTRUE(a$basal_area)) out$basal_area <- forest_basal_area(data, by = by, plant_category = plant_category) else skipped["basal_area"] <- "Requires dbh_cm and sample_plot_size_m2."
  if (isTRUE(a$ivi)) out$ivi <- forest_ivi(data, by = by, plant_category = plant_category) else skipped["ivi"] <- "Requires species_name, count, sample_plot_size_m2 and dbh_cm."
  if (isTRUE(a$diversity)) out$diversity <- forest_diversity(data, by = by, plant_category = plant_category) else skipped["diversity"] <- "Requires species_name and count."
  if (isTRUE(a$stand_structure)) out$stand_structure <- forest_structure(data, by = by, plant_category = plant_category) else skipped["stand_structure"] <- "Requires dbh_cm and/or height_m."
  if (isTRUE(a$regeneration)) out$regeneration <- forest_regeneration(data, by = by, plant_category = plant_category) else skipped["regeneration"] <- "Requires plant_category, species_name, count and sample_plot_size_m2."

  ans <- list(
    analyses = out,
    skipped = skipped,
    availability = x$availability,
    interpretation = sprintf("Module 1 completed %d supported analysis component%s; unsupported components were skipped without failing the workflow.", length(out), if (length(out) == 1L) "" else "s"),
    settings = list(by = by, plant_category = plant_category)
  )
  class(ans) <- "forest_structure_analysis"
  ans
}

# Print methods -------------------------------------------------------------

#' @export
print.forest_frequency_result <- function(x, ...) { cat("NepInventory forest frequency\n-----------------------------\n"); print(x$result, row.names = FALSE); cat("\n", x$interpretation, "\n", sep = ""); invisible(x) }
#' @export
print.forest_basal_area_result <- function(x, ...) { cat("NepInventory basal area and dominance\n-------------------------------------\n"); print(x$result$total, row.names = FALSE); if (!is.null(x$result$by_taxon)) { cat("\nTaxon-specific basal area:\n"); print(x$result$by_taxon, row.names = FALSE) }; cat("\n", x$interpretation, "\n", sep = ""); invisible(x) }
#' @export
print.forest_ivi_result <- function(x, ...) { cat("NepInventory importance value index\n-----------------------------------\n"); print(x$result, row.names = FALSE); cat("\n", x$interpretation, "\n", sep = ""); invisible(x) }
#' @export
print.forest_diversity_result <- function(x, ...) { cat("NepInventory forest diversity\n-----------------------------\n"); print(x$result, row.names = FALSE); cat("\n", x$interpretation, "\n", sep = ""); invisible(x) }
#' @export
print.forest_structure_result <- function(x, ...) { cat("NepInventory stand structure\n----------------------------\n"); print(x$result, row.names = FALSE); if (!is.null(x$dbh_classes)) { cat("\nDBH classes:\n"); print(x$dbh_classes, row.names = FALSE) }; cat("\n", x$interpretation, "\n", sep = ""); invisible(x) }
#' @export
print.forest_regeneration_result <- function(x, ...) { cat("NepInventory regeneration\n-------------------------\n"); print(x$result$total, row.names = FALSE); if (!is.null(x$result$by_taxon)) { cat("\nTaxon-specific regeneration density:\n"); print(x$result$by_taxon, row.names = FALSE) }; cat("\n", x$interpretation, "\n", sep = ""); invisible(x) }
#' @export
print.forest_structure_analysis <- function(x, ...) {
  cat("NepInventory Module 1: Forest Structure, Composition & Diversity\n")
  cat("----------------------------------------------------------------\n")
  cat("Completed:", if (length(x$analyses)) paste(names(x$analyses), collapse = ", ") else "none", "\n")
  if (length(x$skipped)) {
    cat("Skipped:\n")
    for (nm in names(x$skipped)) cat(" - ", nm, ": ", x$skipped[[nm]], "\n", sep = "")
  }
  invisible(x)
}
