library(NepInventory)

m1 <- data.frame(
  plot_id = c("P1","P1","P2","P2"),
  species_name = c("Shorea robusta","Schima wallichii","Shorea robusta","Schima wallichii"),
  genus = c("Shorea","Schima","Shorea","Schima"),
  family = c("Dipterocarpaceae","Theaceae","Dipterocarpaceae","Theaceae"),
  plant_category = c("tree","tree","tree","tree"),
  count = c(4,2,3,1),
  sample_plot_size_m2 = 500,
  dbh_cm = c(30,20,25,15),
  height_m = c(18,14,16,12),
  group_elevation = c("Low","Low","High","High"),
  stringsAsFactors = FALSE
)

# Frequency
f <- forest_frequency(m1)
stopifnot(inherits(f, "forest_frequency_result"))
stopifnot(all(abs(f$result$frequency_pct - 100) < 1e-10))
stopifnot(abs(sum(f$result$relative_frequency_pct) - 100) < 1e-10)

# Basal area and dominance
b <- forest_basal_area(m1)
stopifnot(inherits(b, "forest_basal_area_result"))
stopifnot(all(b$result$total$mean_basal_area_ha > 0))
stopifnot(abs(sum(b$result$by_taxon$relative_dominance_pct) - 100) < 1e-8)

# IVI should sum to 300 across taxa for an ungrouped tree layer.
i <- forest_ivi(m1)
stopifnot(inherits(i, "forest_ivi_result"))
stopifnot(abs(sum(i$result$ivi) - 300) < 1e-8)

# Diversity
v <- forest_diversity(m1)
stopifnot(inherits(v, "forest_diversity_result"))
stopifnot(abs(v$result$mean_richness - 2) < 1e-10)
stopifnot(v$result$mean_shannon > 0)
stopifnot(v$result$mean_simpson > 0)

# Stand structure
s <- forest_structure(m1)
stopifnot(inherits(s, "forest_structure_result"))
stopifnot(s$result$qmd_cm > 0)
stopifnot(!is.null(s$dbh_classes))

# Regeneration with nested subplot support.
regen <- data.frame(
  plot_id = c("P1","P1","P2","P2"),
  species_name = c("Shorea robusta","Schima wallichii","Shorea robusta","Schima wallichii"),
  plant_category = c("seedling","seedling","seedling","seedling"),
  count = c(20,5,10,5),
  sample_plot_size_m2 = 25,
  stringsAsFactors = FALSE
)
r <- forest_regeneration(regen)
stopifnot(inherits(r, "forest_regeneration_result"))
stopifnot(r$result$total$mean_density_ha > 0)

# Master Module 1 runner should complete all supported components.
a <- structure_analysis(m1)
stopifnot(inherits(a, "forest_structure_analysis"))
stopifnot(all(c("density","frequency","basal_area","ivi","diversity","stand_structure","regeneration") %in% names(a$analyses)))

# Generic group_* columns can be used without hard-coded meanings.
g <- forest_density(m1, by = "group_elevation")
stopifnot("group_elevation" %in% names(g$result$total))
