# NepInventory 0.1.2

`NepInventory` is a prototype R package for **post-inventory assessment** of
forest inventory sampling intensity and statistical precision using already-calculated **plot-level forest attributes**.

Version 0.1 is intentionally narrow:

- one row per fixed plot;
- equal-area fixed plots;
- plot area supplied in square metres;
- forest area supplied in hectares;
- SRS variance formulas;
- systematic/fishnet inventories may be assessed using the same formulas as an
  explicit approximation;
- Nepal Community Forest reference sampling-intensity benchmark (default 0.05%; user-changeable);
- explicit MET / NOT MET status and required plot counts for 5%, 10%, and 15% relative-error targets by default;
- precision-versus-sample-size curve;
- no nested subplots, combined stratified estimators, or spatial variance model
  yet.

## Example input

```r
plots <- data.frame(
  plot_id = sprintf("P%02d", 1:12),
  density_ha = c(520, 610, 470, 690, 560, 590, 510, 630, 550, 600, 490, 650),
  basal_area_ha = c(24.5, 30.1, 20.8, 34.2, 27.0, 28.5, 23.4, 31.8, 26.1, 29.7, 22.0, 33.0),
  volume_ha = c(165, 205, 132, 242, 181, 195, 151, 221, 174, 201, 143, 231),
  biomass_ha = c(128, 158, 103, 188, 140, 151, 117, 171, 135, 156, 111, 179)
)

res <- assess_inventory(
  data = plots,
  forest_area_ha = 250,
  plot_area_m2 = 500,
  variables = c("density_ha", "basal_area_ha", "volume_ha", "biomass_ha"),
  confidence = 0.95,
  precision_targets = c(5, 10, 15),
  guideline_intensity_pct = 0.05,
  design = "systematic"
)

print(res)
res$inventory
res$summary
plot_precision_curve(res, "volume_ha")
```

## Main outputs

For every selected plot-level attribute, the package reports:

- number of usable plots;
- mean;
- standard deviation;
- coefficient of variation (%);
- standard error;
- confidence interval;
- relative sampling error (%);
- plots required for each requested precision target;
- additional plots required relative to the current inventory.

The inventory-level section reports current sampled area, observed sampling
intensity, the reference benchmark, the number of plots implied by that
benchmark, and whether the benchmark is met. The attribute-level section also
reports whether each requested statistical precision target is met.

## Important interpretation

The package answers a **post-inventory** question: given the variability among
plots already measured, how precise are the current forest-attribute estimates,
and approximately how many plots would be needed for alternative precision
targets?

Required plot counts assume the coefficient of variation observed in the current
inventory remains representative. They are planning estimates, not guarantees.

## Interpretation principle

Guideline-based sampling intensity and statistical precision are reported separately. Meeting the sampling-intensity reference does not guarantee that every forest attribute meets the selected 5%, 10%, or 15% relative-error target.
