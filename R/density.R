#' Estimate stem density from long-form inventory data
#'
#' Calculates plot-level and summarized stem density (stems per hectare) from
#' the standardized long-form NepInventory input. Counts are expanded by the
#' sampled support area stored in `sample_plot_size_m2`.
#'
#' When `plant_category` is available, density is calculated separately by
#' category (for example tree, sapling, seedling) because nested categories may
#' use different sampled areas. Species absent from a sampled plot/category are
#' represented as zero when species-level means are calculated.
#'
#' @param data A data.frame or CSV/Excel file path using the NepInventory
#'   long-form input convention.
#' @param by Optional character vector of grouping columns, such as
#'   `"group_elevation"` or `c("group_elevation", "group_aspect")`.
#' @param taxon Taxonomic column used for the species/taxon breakdown. Defaults
#'   to `"species_name"`; `"genus"` or `"family"` can also be used when present.
#' @param plant_category Optional character vector selecting one or more values
#'   from `plant_category` (for example `"tree"` or `c("seedling", "sapling")`).
#'
#' @return An object of class `forest_density` containing `result`, `plot_data`,
#'   `figure_data`, `interpretation`, and `settings`.
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   plot_id = c("P1", "P1", "P2", "P2"),
#'   species_name = c("Shorea robusta", "Schima wallichii",
#'                    "Shorea robusta", "Schima wallichii"),
#'   count = c(4, 2, 3, 1),
#'   sample_plot_size_m2 = 500
#' )
#' density(dat)
density <- function(data, by = NULL, taxon = "species_name", plant_category = NULL) {
  x <- .prepare_longform_inventory(data)
  d <- x$records

  if (!taxon %in% names(d)) {
    stop(sprintf("Density by `%s` cannot be calculated because that column was not supplied.", taxon), call. = FALSE)
  }
  if (!"sample_plot_size_m2" %in% names(d)) {
    stop("Density cannot be calculated because `sample_plot_size_m2` was not supplied.", call. = FALSE)
  }
  if (!"count" %in% names(d)) {
    stop("Density cannot be calculated because usable counts are unavailable.", call. = FALSE)
  }

  if (!is.null(by)) {
    if (!is.character(by) || anyNA(by) || any(!nzchar(by))) {
      stop("`by` must be NULL or a character vector of column names.", call. = FALSE)
    }
    missing_by <- setdiff(by, names(d))
    if (length(missing_by)) {
      stop(sprintf("Grouping column(s) not found: %s.", paste(missing_by, collapse = ", ")), call. = FALSE)
    }
  }

  if (!is.null(plant_category)) {
    if (!"plant_category" %in% names(d)) {
      stop("`plant_category` filtering was requested, but the column was not supplied.", call. = FALSE)
    }
    d <- d[!is.na(d$plant_category) & d$plant_category %in% plant_category, , drop = FALSE]
    if (!nrow(d)) stop("No records matched the requested `plant_category` value(s).", call. = FALSE)
  }

  usable <- !is.na(d$count) & !is.na(d$sample_plot_size_m2)
  if (!any(usable)) {
    stop("Density cannot be calculated because no records contain both `count` and `sample_plot_size_m2`.", call. = FALSE)
  }
  if (any(!usable)) {
    warning(sprintf("Excluded %d row(s) lacking `count` or `sample_plot_size_m2` from density calculations.", sum(!usable)), call. = FALSE)
  }
  d <- d[usable, , drop = FALSE]

  # Internal plot key permits plot IDs to repeat across separate boundaries.
  if ("boundary_id" %in% names(d) && any(!is.na(d$boundary_id))) {
    b <- ifelse(is.na(d$boundary_id), "<no_boundary>", d$boundary_id)
    d$.plot_key <- paste(b, d$plot_id, sep = "::")
  } else {
    d$.plot_key <- d$plot_id
  }

  category_col <- NULL
  if ("plant_category" %in% names(d) && any(!is.na(d$plant_category))) {
    d$.density_category <- ifelse(is.na(d$plant_category), "Unclassified", d$plant_category)
    category_col <- ".density_category"
  }

  # Nested sampling requires one sampled support area per plot/category. The
  # area is repeated across tree/species rows but is counted only once.
  support_keys <- c(".plot_key", by, category_col)
  support_key <- interaction(d[support_keys], drop = TRUE, lex.order = TRUE, sep = "\r")
  support_split <- split(d$sample_plot_size_m2, support_key, drop = TRUE)
  inconsistent <- vapply(support_split, function(z) length(unique(z[!is.na(z)])) > 1L, logical(1))
  if (any(inconsistent)) {
    stop(
      "`sample_plot_size_m2` must be constant within each plot and plant category used for a density calculation.",
      call. = FALSE
    )
  }

  # Retain every sampled plot/category, including explicit zero-observation rows.
  plot_base_cols <- unique(c(".plot_key", "plot_id", intersect("boundary_id", names(d)), by, category_col, "sample_plot_size_m2"))
  plot_base <- d[!duplicated(support_key), plot_base_cols, drop = FALSE]

  # Positive-count records without the requested taxon can contribute to total
  # density but cannot contribute to the taxon-specific breakdown.
  named <- !is.na(d[[taxon]]) & nzchar(trimws(as.character(d[[taxon]])))
  if (any(d$count > 0 & !named)) {
    warning(
      sprintf("Some positive-count records lack `%s`; total density includes them, but the taxon breakdown does not.", taxon),
      call. = FALSE
    )
  }

  # Total density by sampled plot/category.
  total_keys <- c(".plot_key", by, category_col)
  total_count <- aggregate(d$count, d[total_keys], sum, na.rm = TRUE)
  names(total_count)[ncol(total_count)] <- "count"
  total_plot <- merge(plot_base, total_count, by = total_keys, all.x = TRUE, sort = FALSE)
  total_plot$count[is.na(total_plot$count)] <- 0
  total_plot$density_ha <- total_plot$count / total_plot$sample_plot_size_m2 * 10000

  # Taxon-specific density. Build the full taxon x sampled-plot frame so absent
  # taxa contribute zeros to mean plot-level density.
  taxa <- sort(unique(as.character(d[[taxon]][named])))
  taxon_plot <- NULL
  if (length(taxa)) {
    pieces <- vector("list", length(taxa))
    for (i in seq_along(taxa)) {
      tmp <- plot_base
      tmp[[taxon]] <- taxa[i]
      pieces[[i]] <- tmp
    }
    taxon_frame <- do.call(rbind, pieces)

    dn <- d[named, , drop = FALSE]
    taxon_keys <- c(".plot_key", by, category_col, taxon)
    taxon_count <- aggregate(dn$count, dn[taxon_keys], sum, na.rm = TRUE)
    names(taxon_count)[ncol(taxon_count)] <- "count"
    taxon_plot <- merge(taxon_frame, taxon_count, by = taxon_keys, all.x = TRUE, sort = FALSE)
    taxon_plot$count[is.na(taxon_plot$count)] <- 0
    taxon_plot$density_ha <- taxon_plot$count / taxon_plot$sample_plot_size_m2 * 10000
  }

  summary_keys <- c(by, category_col)
  summarize_density <- function(z, keys, include_taxon = FALSE) {
    if (include_taxon) keys <- c(keys, taxon)
    if (!length(keys)) {
      n <- nrow(z)
      m <- mean(z$density_ha)
      s <- if (n > 1L) stats::sd(z$density_ha) else NA_real_
      out <- data.frame(
        n_plots = n,
        mean_density_ha = m,
        sd_density_ha = s,
        se_density_ha = if (n > 1L) s / sqrt(n) else NA_real_,
        cv_pct = if (n > 1L && is.finite(m) && m != 0) abs(s / m) * 100 else NA_real_,
        stringsAsFactors = FALSE
      )
      return(out)
    }

    split_key <- interaction(z[keys], drop = TRUE, lex.order = TRUE, sep = "\r")
    chunks <- split(z, split_key, drop = TRUE)
    rows <- lapply(chunks, function(q) {
      n <- nrow(q)
      m <- mean(q$density_ha)
      s <- if (n > 1L) stats::sd(q$density_ha) else NA_real_
      base <- q[1, keys, drop = FALSE]
      cbind(
        base,
        data.frame(
          n_plots = n,
          mean_density_ha = m,
          sd_density_ha = s,
          se_density_ha = if (n > 1L) s / sqrt(n) else NA_real_,
          cv_pct = if (n > 1L && is.finite(m) && m != 0) abs(s / m) * 100 else NA_real_,
          stringsAsFactors = FALSE
        )
      )
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }

  total_summary <- summarize_density(total_plot, summary_keys, FALSE)
  total_summary$level <- "total"

  taxon_summary <- NULL
  if (!is.null(taxon_plot) && nrow(taxon_plot)) {
    taxon_summary <- summarize_density(taxon_plot, summary_keys, TRUE)
    taxon_summary$level <- taxon
  }

  # Restore the public category name in outputs.
  restore_category <- function(z) {
    if (!is.null(z) && ".density_category" %in% names(z)) {
      names(z)[names(z) == ".density_category"] <- "plant_category"
    }
    z
  }
  total_summary <- restore_category(total_summary)
  taxon_summary <- restore_category(taxon_summary)
  total_plot <- restore_category(total_plot)
  taxon_plot <- restore_category(taxon_plot)

  result <- list(total = total_summary, by_taxon = taxon_summary)

  figure_data <- taxon_summary
  if (!is.null(figure_data) && nrow(figure_data)) {
    keep <- unique(c(by, if ("plant_category" %in% names(figure_data)) "plant_category", taxon, "mean_density_ha"))
    figure_data <- figure_data[keep]
  }

  nplots <- length(unique(d$.plot_key))
  interpretation <- sprintf(
    "Density was estimated from %d sampled plot%s by expanding observed counts to stems per hectare using `sample_plot_size_m2`. Taxon-level mean densities include zero abundance in sampled plots where a taxon was absent.",
    nplots, if (nplots == 1L) "" else "s"
  )

  out <- list(
    result = result,
    plot_data = list(total = total_plot, by_taxon = taxon_plot),
    figure_data = figure_data,
    interpretation = interpretation,
    settings = list(by = by, taxon = taxon, plant_category = plant_category)
  )
  class(out) <- "forest_density"
  out
}

#' @export
print.forest_density <- function(x, ...) {
  cat("NepInventory density analysis\n")
  cat("-----------------------------\n")
  print(x$result$total, row.names = FALSE)
  if (!is.null(x$result$by_taxon)) {
    cat("\nTaxon-specific density:\n")
    print(x$result$by_taxon, row.names = FALSE)
  }
  cat("\n", x$interpretation, "\n", sep = "")
  invisible(x)
}
