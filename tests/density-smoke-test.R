library(NepInventory)

# Basic species density: plot-level expansion followed by mean across plots.
dat <- data.frame(
  plot_id = c("P1", "P1", "P2", "P2"),
  species_name = c("Shorea robusta", "Schima wallichii",
                   "Shorea robusta", "Schima wallichii"),
  count = c(4, 2, 3, 1),
  sample_plot_size_m2 = 500,
  stringsAsFactors = FALSE
)

x <- density(dat)
stopifnot(inherits(x, "forest_density"))
stopifnot(abs(x$result$total$mean_density_ha - 100) < 1e-10)

sp <- x$result$by_taxon
sal <- sp[sp$species_name == "Shorea robusta", ]
schima <- sp[sp$species_name == "Schima wallichii", ]
stopifnot(abs(sal$mean_density_ha - 70) < 1e-10)
stopifnot(abs(schima$mean_density_ha - 30) < 1e-10)

# A taxon absent from a sampled plot must contribute zero to its across-plot mean.
dat_zero <- data.frame(
  plot_id = c("P1", "P2"),
  species_name = c("Shorea robusta", NA),
  count = c(5, 0),
  sample_plot_size_m2 = c(500, 500),
  stringsAsFactors = FALSE
)

z <- density(dat_zero)
zsal <- z$result$by_taxon[z$result$by_taxon$species_name == "Shorea robusta", ]
stopifnot(abs(zsal$mean_density_ha - 50) < 1e-10)

# Nested categories use their own sampled support and remain separate.
dat_nested <- data.frame(
  plot_id = c("P1", "P1", "P2", "P2"),
  species_name = c("Shorea robusta", "Shorea robusta", "Shorea robusta", "Shorea robusta"),
  plant_category = c("tree", "seedling", "tree", "seedling"),
  count = c(5, 20, 4, 10),
  sample_plot_size_m2 = c(500, 25, 500, 25),
  stringsAsFactors = FALSE
)

n <- density(dat_nested)
nt <- n$result$total
stopifnot(abs(nt$mean_density_ha[nt$plant_category == "tree"] - 90) < 1e-10)
stopifnot(abs(nt$mean_density_ha[nt$plant_category == "seedling"] - 6000) < 1e-10)
