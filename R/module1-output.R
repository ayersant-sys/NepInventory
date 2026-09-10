# Internal output helpers for Module 1.

.m1_collect_tables <- function(analyses) {
  tables <- list()

  add_table <- function(name, value) {
    if (is.data.frame(value)) tables[[name]] <<- value
  }

  for (nm in names(analyses)) {
    a <- analyses[[nm]]

    if (!is.null(a$result)) {
      if (is.data.frame(a$result)) {
        add_table(paste0(nm, "_summary"), a$result)
      } else if (is.list(a$result)) {
        for (sub in names(a$result)) {
          add_table(paste0(nm, "_", sub), a$result[[sub]])
        }
      }
    }

    if (!is.null(a$plot_data)) {
      if (is.data.frame(a$plot_data)) {
        add_table(paste0(nm, "_plot_data"), a$plot_data)
      } else if (is.list(a$plot_data)) {
        for (sub in names(a$plot_data)) {
          add_table(paste0(nm, "_plot_data_", sub), a$plot_data[[sub]])
        }
      }
    }

    if (!is.null(a$dbh_classes) && is.data.frame(a$dbh_classes)) {
      add_table(paste0(nm, "_dbh_classes"), a$dbh_classes)
    }
  }

  tables
}

.m1_collect_figure_data <- function(analyses) {
  out <- list()
  for (nm in names(analyses)) {
    z <- analyses[[nm]]$figure_data
    if (!is.null(z)) out[[nm]] <- z
  }
  out
}

.m1_results_text <- function(analyses, skipped = character(0)) {
  txt <- character(0)
  for (nm in names(analyses)) {
    z <- analyses[[nm]]$interpretation
    if (length(z) && !is.na(z[1]) && nzchar(z[1])) {
      label <- gsub("_", " ", nm, fixed = TRUE)
      txt <- c(txt, paste0(tools::toTitleCase(label), ": ", z[1]))
    }
  }

  if (length(skipped)) {
    skip_txt <- paste(
      paste0(gsub("_", " ", names(skipped), fixed = TRUE), " (", unname(skipped), ")"),
      collapse = "; "
    )
    txt <- c(txt, paste0("Not generated: ", skip_txt, "."))
  }

  txt
}

.m1_figure_specs <- function(analyses) {
  specs <- list()

  pick_taxon <- function(z) {
    candidates <- c("species_name", "genus", "family")
    candidates[candidates %in% names(z)][1]
  }

  if (!is.null(analyses$composition$figure_data)) {
    z <- analyses$composition$figure_data
    taxon <- pick_taxon(z)
    if (length(taxon) && "relative_abundance_pct" %in% names(z)) {
      specs$composition <- list(type = "bar", data = z, x = taxon,
                                y = "relative_abundance_pct",
                                xlab = "Taxon", ylab = "Relative abundance (%)",
                                title = "Forest composition")
    }
  }

  if (!is.null(analyses$density$figure_data)) {
    z <- analyses$density$figure_data
    taxon <- pick_taxon(z)
    if (length(taxon) && "mean_density_ha" %in% names(z)) {
      specs$density <- list(type = "bar", data = z, x = taxon,
                            y = "mean_density_ha",
                            xlab = "Taxon", ylab = "Density (stems/ha)",
                            title = "Forest density")
    }
  }

  if (!is.null(analyses$frequency$figure_data)) {
    z <- analyses$frequency$figure_data
    taxon <- pick_taxon(z)
    if (length(taxon) && "frequency_pct" %in% names(z)) {
      specs$frequency <- list(type = "bar", data = z, x = taxon,
                              y = "frequency_pct",
                              xlab = "Taxon", ylab = "Frequency (%)",
                              title = "Forest frequency")
    }
  }

  if (!is.null(analyses$basal_area$figure_data)) {
    z <- analyses$basal_area$figure_data
    taxon <- pick_taxon(z)
    if (length(taxon) && "mean_basal_area_ha" %in% names(z)) {
      specs$basal_area <- list(type = "bar", data = z, x = taxon,
                               y = "mean_basal_area_ha",
                               xlab = "Taxon", ylab = "Basal area (m2/ha)",
                               title = "Basal area")
    }
  }

  if (!is.null(analyses$ivi$figure_data)) {
    z <- analyses$ivi$figure_data
    taxon <- pick_taxon(z)
    if (length(taxon) && "ivi" %in% names(z)) {
      specs$ivi <- list(type = "bar", data = z, x = taxon,
                        y = "ivi", xlab = "Taxon", ylab = "IVI",
                        title = "Importance value index")
    }
  }

  if (!is.null(analyses$stand_structure$dbh_classes)) {
    z <- analyses$stand_structure$dbh_classes
    class_col <- c("dbh_class", "DBH_class", "class")[c("dbh_class", "DBH_class", "class") %in% names(z)][1]
    count_col <- c("count", "n", "stems")[c("count", "n", "stems") %in% names(z)][1]
    if (length(class_col) && length(count_col)) {
      specs$dbh_structure <- list(type = "bar", data = z, x = class_col,
                                  y = count_col, xlab = "DBH class",
                                  ylab = "Count", title = "DBH structure")
    }
  }

  specs
}

#' Plot a Module 1 analysis result
#'
#' Draws one of the automatically prepared Module 1 figures using base R.
#' The exact source data used for every available figure are retained in
#' `x$figure_data` and the plotting specification in `x$figure_specs`.
#'
#' @param x A `forest_structure_analysis` object.
#' @param which Name of the figure to draw. If omitted, the first available
#'   figure is used.
#' @param ... Additional arguments passed to `barplot()`.
#' @return Invisibly returns the figure specification used.
#' @export
plot.forest_structure_analysis <- function(x, which = NULL, ...) {
  specs <- x$figure_specs
  if (!length(specs)) stop("No Module 1 figures are available for this dataset.", call. = FALSE)

  if (is.null(which)) which <- names(specs)[1]
  if (!which %in% names(specs)) {
    stop(sprintf("Unknown figure `%s`. Available figures: %s.",
                 which, paste(names(specs), collapse = ", ")), call. = FALSE)
  }

  sp <- specs[[which]]
  z <- sp$data
  if (!identical(sp$type, "bar")) stop("Unsupported Module 1 figure type.", call. = FALSE)

  labels <- as.character(z[[sp$x]])
  values <- suppressWarnings(as.numeric(z[[sp$y]]))
  keep <- is.finite(values) & !is.na(labels)
  if (!any(keep)) stop("The selected figure has no plottable values.", call. = FALSE)

  graphics::barplot(
    height = values[keep],
    names.arg = labels[keep],
    las = 2,
    ylab = sp$ylab,
    xlab = sp$xlab,
    main = sp$title,
    ...
  )

  invisible(sp)
}
