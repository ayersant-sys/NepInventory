# NepInventory 0.2.0

`NepInventory` is an R package for **post-inventory assessment** of forest inventory sampling intensity and statistical precision using already-calculated **plot-level forest attributes**.

## Quick start with CSV or Excel

The simplest workflow is to prepare one inventory file with one row per fixed plot. Include the inventory metadata as constant columns across plots:

- `plot_id`
- `forest_area_ha`
- `plot_area_m2`
- `design` (`srs` or `systematic`)
- one or more numeric plot-level forest attributes such as `density_ha`, `basal_area_ha`, `volume_ha`, and `biomass_ha`

Then run:

```r
library(NepInventory)

result <- assess_inventory("inventory.csv")
result
result$summary
plot_precision_curve(result, "volume_ha")
```

Excel files (`.xlsx` and `.xls`) are also supported:

```r
result <- assess_inventory("inventory.xlsx")
```

Excel input requires the optional `readxl` package. Install it with `install.packages("readxl")` if needed.

The package automatically reads forest area, plot area, and sampling design from the file when those arguments are not supplied. The sampled area and sampling intensity are calculated from the number of plots and fixed plot area; sampling area does not need to be entered separately.

## Example input

```text
plot_id,forest_area_ha,plot_area_m2,design,density_ha,basal_area_ha,volume_ha,biomass_ha
P1,200,500,systematic,520,22,145,110
P2,200,500,systematic,610,29,210,165
P3,200,500,systematic,480,20,130,95
P4,200,500,systematic,570,25,175,135
```

Each row represents one plot. `forest_area_ha`, `plot_area_m2`, and `design` should contain one constant value across the inventory file. When `variables = NULL`, the package automatically assesses the remaining numeric plot-level attributes.

## Standard R workflow

The core `assess_inventory()` function also accepts an existing data frame and explicit arguments:

```r
res <- assess_inventory(
  data = plots,
  forest_area_ha = 250,
  plot_area_m2 = 500,
  variables = c("density_ha", "basal_area_ha", "volume_ha", "biomass_ha"),
  confidence = 0.95,
  precision_targets = c(5, 10, 15),
  guideline_intensity_pct = 0.5,
  design = "systematic"
)
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

The inventory-level section reports current sampled area, observed sampling intensity, the reference benchmark, the number of plots implied by that benchmark, and whether the benchmark is met. The attribute-level section also reports whether each requested statistical precision target is met.

The package also provides a **precision-versus-sample-size curve**, showing how expected relative sampling error changes as the number of plots increases.

## Important interpretation

The package answers a **post-inventory** question: given the variability among plots already measured, how precise are the current forest-attribute estimates, and approximately how many plots would be needed for alternative precision targets?

Required plot counts assume the coefficient of variation observed in the current inventory remains representative. They are planning estimates, not guarantees. The default 0.5% sampling-intensity value represents the general Nepal Community Forest reference. Users should supply a different benchmark where another guideline category or inventory requirement applies.

## Interpretation principle

Guideline-based sampling intensity and statistical precision are reported separately. Meeting the sampling-intensity reference does not guarantee that every forest attribute meets the selected 5%, 10%, or 15% relative-error target.

## Scope

The current release focuses on equal-area fixed plots and SRS variance calculations. Systematic/fishnet inventories are assessed using the SRS variance formula as an explicit approximation. Nested plots, variable-area sampling, combined stratified estimators, and spatial variance models are not implemented yet.
