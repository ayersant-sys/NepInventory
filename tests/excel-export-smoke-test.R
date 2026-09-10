if (requireNamespace("openxlsx", quietly = TRUE)) {
  demo <- data.frame(
    plot_id = rep(c("P1", "P2", "P3"), each = 4),
    species_name = c(
      "Shorea robusta", "Schima wallichii", "Shorea robusta", "Schima wallichii",
      "Shorea robusta", "Shorea robusta", "Shorea robusta", "Schima wallichii",
      "Schima wallichii", "Shorea robusta", "Shorea robusta", NA
    ),
    plant_category = rep(c("tree", "tree", "seedling", "seedling"), 3),
    count = c(1,1,18,6, 1,1,12,9, 1,1,20,0),
    sample_plot_size_m2 = rep(c(500,500,25,25), 3),
    dbh_cm = c(32,24,NA,NA, 38,29,NA,NA, 27,35,NA,NA),
    height_m = c(23,18,NA,NA, 26,21,NA,NA, 20,24,NA,NA)
  )

  res <- structure_analysis(demo)

  stopifnot(
    inherits(res, "forest_structure_analysis"),
    identical(unique(res$analyses$regeneration$result$total$plant_category), "seedling")
  )

  outfile <- tempfile(fileext = ".xlsx")
  export_inventory_analysis(res, outfile)
  stopifnot(file.exists(outfile), file.info(outfile)$size > 0)

  sheets <- openxlsx::getSheetNames(outfile)
  expected <- c(
    "README", "Input_Summary", "Composition", "Density", "Frequency",
    "Basal_Area", "IVI", "Diversity", "Stand_Structure", "Regeneration",
    "DBH_Classes", "Figure_Data", "Results_Text", "Skipped_Analyses"
  )
  stopifnot(all(expected %in% sheets))
  unlink(outfile)
}
