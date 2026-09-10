# Internal helpers for Module 1: forest structure, composition and diversity.

.m1_validate_by <- function(data, by) {
  if (is.null(by)) return(character(0))
  if (!is.character(by) || anyNA(by) || any(!nzchar(by))) {
    stop("`by` must be NULL or a character vector of column names.", call. = FALSE)
  }
  missing_by <- setdiff(by, names(data))
  if (length(missing_by)) {
    stop(sprintf("Grouping column(s) not found: %s.", paste(missing_by, collapse = ", ")), call. = FALSE)
  }
  by
}

.m1_filter_category <- function(data, plant_category = NULL) {
  if (is.null(plant_category)) return(data)
  if (!"plant_category" %in% names(data)) {
    stop("`plant_category` filtering was requested, but that column was not supplied.", call. = FALSE)
  }
  keep <- !is.na(data$plant_category) & data$plant_category %in% plant_category
  out <- data[keep, , drop = FALSE]
  if (!nrow(out)) stop("No records matched the requested `plant_category` value(s).", call. = FALSE)
  out
}

.m1_add_plot_key <- function(data) {
  if ("boundary_id" %in% names(data) && any(!is.na(data$boundary_id))) {
    b <- ifelse(is.na(data$boundary_id), "<no_boundary>", data$boundary_id)
    data$.plot_key <- paste(b, data$plot_id, sep = "::")
  } else {
    data$.plot_key <- data$plot_id
  }
  data
}

.m1_add_category <- function(data) {
  if ("plant_category" %in% names(data) && any(!is.na(data$plant_category))) {
    data$.plant_category <- ifelse(is.na(data$plant_category), "Unclassified", data$plant_category)
    ".plant_category"
  } else {
    NULL
  }
}

.m1_restore_names <- function(x) {
  if (is.null(x)) return(NULL)
  names(x)[names(x) == ".plant_category"] <- "plant_category"
  x
}

.m1_support_table <- function(data, by = character(0), category_col = NULL) {
  keys <- c(".plot_key", by, category_col)
  skey <- interaction(data[keys], drop = TRUE, lex.order = TRUE, sep = "\r")
  vals <- split(data$sample_plot_size_m2, skey, drop = TRUE)
  bad <- vapply(vals, function(z) {
    z <- unique(z[!is.na(z)])
    length(z) > 1L
  }, logical(1))
  if (any(bad)) {
    stop("`sample_plot_size_m2` must be constant within each plot/category used in an analysis.", call. = FALSE)
  }
  cols <- unique(c(".plot_key", "plot_id", intersect("boundary_id", names(data)), by,
                   category_col, "sample_plot_size_m2"))
  data[!duplicated(skey), cols, drop = FALSE]
}

.m1_summary_numeric <- function(data, value, keys = character(0), prefix = NULL) {
  one <- function(z) {
    n <- nrow(z)
    v <- z[[value]]
    m <- mean(v, na.rm = TRUE)
    s <- if (sum(is.finite(v)) > 1L) stats::sd(v, na.rm = TRUE) else NA_real_
    data.frame(
      n_plots = length(unique(z$.plot_key)),
      mean = m,
      sd = s,
      se = if (is.finite(s) && n > 0) s / sqrt(sum(is.finite(v))) else NA_real_,
      cv_pct = if (is.finite(s) && is.finite(m) && m != 0) abs(s / m) * 100 else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  if (!length(keys)) {
    out <- one(data)
  } else {
    k <- interaction(data[keys], drop = TRUE, lex.order = TRUE, sep = "\r")
    chunks <- split(data, k, drop = TRUE)
    rows <- lapply(chunks, function(z) cbind(z[1, keys, drop = FALSE], one(z)))
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
  }

  if (!is.null(prefix)) {
    names(out)[names(out) == "mean"] <- paste0("mean_", prefix)
    names(out)[names(out) == "sd"] <- paste0("sd_", prefix)
    names(out)[names(out) == "se"] <- paste0("se_", prefix)
  }
  out
}

.m1_require <- function(data, cols, analysis) {
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    stop(sprintf("%s cannot be calculated because column(s) %s were not supplied.",
                 analysis, paste(sprintf("`%s`", missing), collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

.m1_split_apply <- function(data, keys, FUN) {
  if (!length(keys)) return(FUN(data))
  k <- interaction(data[keys], drop = TRUE, lex.order = TRUE, sep = "\r")
  chunks <- split(data, k, drop = TRUE)
  rows <- lapply(chunks, function(z) cbind(z[1, keys, drop = FALSE], FUN(z)))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
