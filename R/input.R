#' Read a standardized inventory input file
#'
#' Internal helper for `assess_inventory()`. CSV files are read with base R;
#' Excel files use the optional `readxl` package.
#'
#' @keywords internal
.read_inventory_file <- function(path) {
  if (length(path) != 1L || !is.character(path) || is.na(path) || !nzchar(path)) {
    stop("`data` must be a data.frame or a single CSV/Excel file path.", call. = FALSE)
  }
  if (!file.exists(path)) stop(sprintf("Inventory file not found: %s", path), call. = FALSE)

  ext <- tolower(tools::file_ext(path))
  out <- switch(
    ext,
    csv = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Excel input requires the `readxl` package. Install it with install.packages(\"readxl\").", call. = FALSE)
      }
      readxl::read_excel(path)
    },
    xls = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Excel input requires the `readxl` package. Install it with install.packages(\"readxl\").", call. = FALSE)
      }
      readxl::read_excel(path)
    },
    stop("Unsupported inventory file type. Use a .csv, .xlsx, or .xls file.", call. = FALSE)
  )

  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Extract a single constant metadata value from an inventory table
#'
#' @keywords internal
.extract_inventory_metadata <- function(data, name) {
  if (!name %in% names(data)) {
    stop(sprintf("Column `%s` is required when it is not supplied to `assess_inventory()`.", name), call. = FALSE)
  }
  vals <- data[[name]]
  if (!is.numeric(vals)) {
    vals <- suppressWarnings(as.numeric(vals))
  }
  vals <- vals[is.finite(vals)]
  vals <- unique(vals)
  if (length(vals) != 1L) {
    stop(sprintf("Column `%s` must contain exactly one constant value across all plots.", name), call. = FALSE)
  }
  vals[1]
}

#' Extract a single constant sampling-design value from an inventory table
#'
#' @keywords internal
.extract_inventory_design <- function(data) {
  if (!"design" %in% names(data)) {
    stop("Column `design` is required when it is not supplied to `assess_inventory()`.", call. = FALSE)
  }
  vals <- trimws(as.character(data$design))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  vals <- unique(vals)
  if (length(vals) != 1L) {
    stop("Column `design` must contain exactly one constant value across all plots.", call. = FALSE)
  }
  match.arg(vals[1], c("srs", "systematic"))
}
