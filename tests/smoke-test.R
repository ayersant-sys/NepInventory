library(NepInventory)

example_inventory <- data.frame(
  plot_id = paste0("P", 1:20),
  density_ha = c(520,610,480,570,630,540,590,510,650,560,
                 600,490,575,620,530,605,555,640,500,585),
  basal_area_ha = c(22,29,20,25,31,23,27,21,34,26,
                    28,19,25,30,22,29,24,32,20,27),
  volume_ha = c(145,210,130,175,230,155,195,140,250,180,
                205,125,170,225,150,215,165,240,135,190),
  biomass_ha = c(110,165,95,135,185,115,150,105,200,140,
                 160,90,130,180,112,170,125,195,100,145)
)

x <- assess_inventory(
  example_inventory,
  forest_area_ha = 200,
  plot_area_m2 = 500,
  variables = c("density_ha", "basal_area_ha", "volume_ha", "biomass_ha"),
  precision_targets = c(5, 10, 15),
  guideline_intensity_pct = 0.05,
  design = "systematic"
)

stopifnot(!"achieved_precision_pct" %in% names(x$summary))
stopifnot(all(c("status_5pct", "status_10pct", "status_15pct") %in% names(x$summary)))
stopifnot(isTRUE(x$inventory$guideline_met))

expected <- data.frame(
  variable = c("density_ha", "basal_area_ha", "volume_ha", "biomass_ha"),
  status_5pct = c("MET", "NOT MET", "NOT MET", "NOT MET"),
  status_10pct = c("MET", "MET", "MET", "NOT MET"),
  status_15pct = c("MET", "MET", "MET", "MET"),
  stringsAsFactors = FALSE
)

observed <- x$summary[, names(expected)]
stopifnot(identical(observed, expected))

stopifnot(identical(x$summary$n_required_5pct, c(16L, 47L, 72L, 95L)))
stopifnot(identical(x$summary$n_required_10pct, c(6L, 14L, 20L, 26L)))
stopifnot(identical(x$summary$n_required_15pct, c(4L, 8L, 11L, 13L)))
