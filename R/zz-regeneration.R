# Refined regeneration semantics loaded after the original Module 1 definitions.
# The public function remains forest_regeneration(); this definition prevents
# mature-tree categories from being labelled regeneration by default.

.regeneration_default_categories <- function(x) {
  if (!"plant_category" %in% names(x)) return(character(0))
  raw <- unique(as.character(x$plant_category[!is.na(x$plant_category)]))
  if (!length(raw)) return(character(0))

  key <- tolower(trimws(raw))
  # Conservative defaults only. Users can always override explicitly with
  # plant_category = ... for local or study-specific classifications.
  accepted <- c(
    "seedling", "seedlings",
    "sapling", "saplings",
    "regeneration", "regen",
    "seedling/sapling", "seedlings/saplings"
  )
  raw[key %in% accepted]
}

forest_regeneration <- function(data, by = NULL, taxon = "species_name", plant_category = NULL) {
  x <- .prepare_longform_inventory(data)
  d <- x$records
  by <- .m1_validate_by(d, by)
  .m1_require(d, c("plant_category", "count", "sample_plot_size_m2", taxon), "Regeneration")

  cats <- unique(as.character(d$plant_category[!is.na(d$plant_category)]))
  if (!length(cats)) {
    stop("Regeneration analysis requires non-missing `plant_category` values.", call. = FALSE)
  }

  if (is.null(plant_category)) {
    plant_category <- .regeneration_default_categories(d)
    if (!length(plant_category)) {
      stop(
        paste0(
          "No default regeneration categories were recognized. ",
          "Specify them explicitly with `plant_category = ...` (for example, ",
          "`c(\"seedling\", \"sapling\")`)."
        ),
        call. = FALSE
      )
    }
  } else {
    missing_cats <- setdiff(plant_category, cats)
    if (length(missing_cats)) {
      stop(
        sprintf(
          "Unknown regeneration categor%s: %s.",
          if (length(missing_cats) == 1L) "y" else "ies",
          paste(missing_cats, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  den <- forest_density(d, by = by, taxon = taxon, plant_category = plant_category)
  ans <- list(
    result = den$result,
    plot_data = den$plot_data,
    figure_data = den$figure_data,
    interpretation = paste0(
      "Regeneration is summarized from the selected regeneration categories and standardized ",
      "to stems per hectare using category-specific sampled support areas. When `plant_category` ",
      "is omitted, NepInventory conservatively recognizes common seedling/sapling labels only; ",
      "other classification systems should be supplied explicitly. No Nepal guideline status ",
      "class is imposed in this general ecological module."
    ),
    settings = list(by = by, taxon = taxon, plant_category = plant_category)
  )
  class(ans) <- "forest_regeneration_result"
  ans
}
