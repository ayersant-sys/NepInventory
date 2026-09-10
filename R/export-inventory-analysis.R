#' Export NepInventory analysis to Excel
#'
#' Creates a predictable multi-sheet Excel workbook from a
#' `forest_structure_analysis` object. A raw long-form dataset may also be
#' supplied; in that case `structure_analysis()` is run first.
#'
#' The workbook keeps the analytical result tables, exact figure-source data,
#' deterministic results text, skipped-analysis reasons, and available Module 1
#' figures together in one file. Sheets remain present even when an analysis is
#' unavailable, in which case the sheet explains why it was not generated.
#'
#' @param x A `forest_structure_analysis` object or a long-form NepInventory
#'   dataset accepted by `structure_analysis()`.
#' @param file Output `.xlsx` file path.
#' @param overwrite Logical; overwrite an existing file? Default `TRUE`.
#' @param by Optional grouping columns used only when `x` is raw data.
#' @param plant_category Optional plant-category filter used only when `x` is raw
#'   data.
#' @param include_figures Logical; include available Module 1 figures in their
#'   corresponding sheets. Default `TRUE`.
#' @return Invisibly returns the normalized output path.
#' @export
export_inventory_analysis <- function(
  x,
  file = "NepInventory_results.xlsx",
  overwrite = TRUE,
  by = NULL,
  plant_category = NULL,
  include_figures = TRUE
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Excel export requires the optional package `openxlsx`. Install it with install.packages(\"openxlsx\").",
      call. = FALSE
    )
  }

  if (!inherits(x, "forest_structure_analysis")) {
    x <- structure_analysis(x, by = by, plant_category = plant_category)
  }

  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("`file` must be one non-empty file path.", call. = FALSE)
  }
  if (!grepl("\\.xlsx$", file, ignore.case = TRUE)) file <- paste0(file, ".xlsx")
  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  if (file.exists(file) && !isTRUE(overwrite)) {
    stop(sprintf("File already exists: %s", file), call. = FALSE)
  }

  wb <- openxlsx::createWorkbook(creator = "NepInventory")

  header_style <- openxlsx::createStyle(
    fontColour = "#FFFFFF", fgFill = "#17324D", textDecoration = "bold",
    halign = "center", valign = "center", border = "Bottom"
  )
  section_style <- openxlsx::createStyle(
    fontColour = "#FFFFFF", fgFill = "#2F6B4F", textDecoration = "bold"
  )
  note_style <- openxlsx::createStyle(
    fgFill = "#EAF3EE", wrapText = TRUE, valign = "top"
  )
  title_style <- openxlsx::createStyle(
    fontSize = 16, textDecoration = "bold", fontColour = "#17324D"
  )

  add_sheet <- function(name) {
    openxlsx::addWorksheet(wb, name, gridLines = FALSE)
    openxlsx::setColWidths(wb, name, cols = 1:20, widths = "auto")
  }

  write_title <- function(sheet, title, subtitle = NULL) {
    openxlsx::writeData(wb, sheet, title, startRow = 1, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, title_style, rows = 1, cols = 1, stack = TRUE)
    if (!is.null(subtitle)) {
      openxlsx::writeData(wb, sheet, subtitle, startRow = 2, startCol = 1, colNames = FALSE)
      openxlsx::addStyle(wb, sheet, note_style, rows = 2, cols = 1, stack = TRUE)
    }
  }

  safe_df <- function(z) {
    if (is.null(z)) return(NULL)
    if (is.data.frame(z)) return(z)
    NULL
  }

  write_df <- function(sheet, z, start_row = 4, section = NULL) {
    if (is.null(z) || !is.data.frame(z)) return(start_row)
    if (!is.null(section)) {
      openxlsx::writeData(wb, sheet, section, startRow = start_row, startCol = 1, colNames = FALSE)
      openxlsx::addStyle(wb, sheet, section_style, rows = start_row, cols = 1, stack = TRUE)
      start_row <- start_row + 1L
    }
    openxlsx::writeData(wb, sheet, z, startRow = start_row, startCol = 1,
                        withFilter = FALSE, keepNA = TRUE, na.string = "NA")
    if (ncol(z)) {
      openxlsx::addStyle(wb, sheet, header_style, rows = start_row,
                         cols = seq_len(ncol(z)), gridExpand = TRUE, stack = TRUE)
      openxlsx::freezePane(wb, sheet, firstActiveRow = start_row + 1L)
    }
    start_row + nrow(z) + 3L
  }

  write_message <- function(sheet, text, start_row = 4) {
    openxlsx::writeData(wb, sheet, text, startRow = start_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, note_style, rows = start_row, cols = 1, stack = TRUE)
    invisible(start_row + 2L)
  }

  component_reason <- function(name) {
    if (name %in% names(x$skipped)) return(unname(x$skipped[[name]]))
    "This analysis was not generated from the supplied inputs."
  }

  # README -----------------------------------------------------------------
  add_sheet("README")
  write_title("README", "NepInventory Results Workbook",
              "Adaptive Module 1 export: only supported analyses are calculated, while the workbook keeps a predictable sheet structure.")
  info <- data.frame(
    item = c("Package", "Module", "Generated", "Completed analyses", "Skipped analyses", "Figures included"),
    value = c(
      "NepInventory",
      "Module 1 — Forest Structure, Composition & Diversity",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      if (length(x$analyses)) paste(names(x$analyses), collapse = ", ") else "None",
      if (length(x$skipped)) paste(names(x$skipped), collapse = ", ") else "None",
      if (isTRUE(include_figures)) "Yes, when figure specifications are available" else "No"
    ),
    stringsAsFactors = FALSE
  )
  write_df("README", info, start_row = 4, section = "Workbook summary")
  write_message("README",
    "Per-hectare results use the sampled support area supplied in sample_plot_size_m2. Figure-source data are retained so plots can be reproduced outside NepInventory.",
    start_row = 14)

  # Input summary -----------------------------------------------------------
  add_sheet("Input_Summary")
  write_title("Input_Summary", "Detected Input Availability")
  av <- x$availability
  av_names <- setdiff(names(av), "analyses")
  av_rows <- lapply(av_names, function(nm) {
    val <- av[[nm]]
    if (is.list(val)) val <- paste(unlist(val), collapse = ", ")
    if (length(val) > 1L) val <- paste(val, collapse = ", ")
    if (!length(val)) val <- ""
    data.frame(item = nm, value = as.character(val), stringsAsFactors = FALSE)
  })
  av_df <- if (length(av_rows)) do.call(rbind, av_rows) else data.frame(item=character(), value=character())
  rnext <- write_df("Input_Summary", av_df, start_row = 4, section = "Detected variables")
  if (is.list(av$analyses)) {
    a_df <- data.frame(
      analysis = names(av$analyses),
      available = vapply(av$analyses, function(v) paste(v, collapse = ", "), character(1)),
      stringsAsFactors = FALSE
    )
    write_df("Input_Summary", a_df, start_row = rnext, section = "Detected analysis availability")
  }

  # Fixed analysis sheets ---------------------------------------------------
  sheet_map <- list(
    Composition = "composition",
    Density = "density",
    Frequency = "frequency",
    Basal_Area = "basal_area",
    IVI = "ivi",
    Diversity = "diversity",
    Stand_Structure = "stand_structure",
    Regeneration = "regeneration"
  )

  for (sheet in names(sheet_map)) {
    comp <- sheet_map[[sheet]]
    add_sheet(sheet)
    write_title(sheet, gsub("_", " ", sheet, fixed = TRUE))
    if (!comp %in% names(x$analyses)) {
      write_message(sheet, paste0("Not generated: ", component_reason(comp)))
      next
    }

    a <- x$analyses[[comp]]
    row <- 4L
    if (is.data.frame(a$result)) {
      row <- write_df(sheet, a$result, row, "Summary")
    } else if (is.list(a$result)) {
      for (nm in names(a$result)) {
        if (is.data.frame(a$result[[nm]])) {
          row <- write_df(sheet, a$result[[nm]], row, gsub("_", " ", nm, fixed = TRUE))
        }
      }
    }
    if (identical(comp, "stand_structure") && is.data.frame(a$dbh_classes)) {
      row <- write_df(sheet, a$dbh_classes, row, "DBH classes")
    }
    if (length(a$interpretation) && nzchar(a$interpretation[1])) {
      write_message(sheet, paste0("Interpretation: ", a$interpretation[1]), row)
    }
  }

  # Separate DBH class sheet for predictable navigation --------------------
  add_sheet("DBH_Classes")
  write_title("DBH_Classes", "DBH Class Structure")
  dbh <- NULL
  if ("stand_structure" %in% names(x$analyses)) dbh <- x$analyses$stand_structure$dbh_classes
  if (is.data.frame(dbh)) write_df("DBH_Classes", dbh, 4, "DBH class counts") else
    write_message("DBH_Classes", "Not generated: usable DBH class data were not available.")

  # Figure data -------------------------------------------------------------
  add_sheet("Figure_Data")
  write_title("Figure_Data", "Exact Figure-Source Data",
              "These are the numeric data used by NepInventory's current Module 1 plotting layer.")
  row <- 4L
  if (length(x$figure_data)) {
    for (nm in names(x$figure_data)) {
      z <- x$figure_data[[nm]]
      if (is.data.frame(z)) {
        row <- write_df("Figure_Data", z, row, paste0("Figure data — ", nm))
      } else if (is.list(z)) {
        for (sub in names(z)) {
          if (is.data.frame(z[[sub]])) row <- write_df("Figure_Data", z[[sub]], row, paste0("Figure data — ", nm, " / ", sub))
        }
      }
    }
  } else {
    write_message("Figure_Data", "No figure-source data were generated.")
  }

  # Results text ------------------------------------------------------------
  add_sheet("Results_Text")
  write_title("Results_Text", "Deterministic Results Text")
  rt <- x$results_text
  if (length(rt)) {
    rt_df <- data.frame(section = seq_along(rt), text = unname(rt), stringsAsFactors = FALSE)
    write_df("Results_Text", rt_df, 4, "Generated reporting text")
  } else {
    write_message("Results_Text", "No results text was generated.")
  }

  # Skipped analyses --------------------------------------------------------
  add_sheet("Skipped_Analyses")
  write_title("Skipped_Analyses", "Skipped or Unsupported Analyses")
  if (length(x$skipped)) {
    sk <- data.frame(analysis = names(x$skipped), reason = unname(x$skipped), stringsAsFactors = FALSE)
    write_df("Skipped_Analyses", sk, 4, "Skipped analyses")
  } else {
    write_message("Skipped_Analyses", "None. All Module 1 components supported by the supplied data were generated.")
  }

  # Insert available figures into corresponding sheets ---------------------
  if (isTRUE(include_figures) && length(x$figure_specs)) {
    fig_sheet <- c(
      composition = "Composition", density = "Density", frequency = "Frequency",
      basal_area = "Basal_Area", ivi = "IVI", dbh_structure = "DBH_Classes"
    )
    for (nm in intersect(names(x$figure_specs), names(fig_sheet))) {
      tf <- tempfile(fileext = ".png")
      ok <- tryCatch({
        grDevices::png(tf, width = 1100, height = 760, res = 130)
        plot(x, which = nm)
        grDevices::dev.off()
        TRUE
      }, error = function(e) {
        while (grDevices::dev.cur() > 1L) try(grDevices::dev.off(), silent = TRUE)
        FALSE
      })
      if (isTRUE(ok) && file.exists(tf)) {
        openxlsx::insertImage(wb, fig_sheet[[nm]], tf, startRow = 4, startCol = 12,
                              width = 7.2, height = 5.0, units = "in")
      }
      unlink(tf)
    }
  }

  # General workbook formatting --------------------------------------------
  for (sheet in names(wb)) {
    openxlsx::setColWidths(wb, sheet, cols = 1:10, widths = "auto")
    openxlsx::setColWidths(wb, sheet, cols = 1, widths = 22)
  }

  openxlsx::saveWorkbook(wb, file, overwrite = overwrite)
  invisible(file)
}
