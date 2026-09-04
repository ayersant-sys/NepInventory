# NepInventory 0.2.0

- Added direct input of `.csv`, `.xlsx`, and `.xls` inventory files to `assess_inventory()`.
- Forest area and fixed plot area can now be supplied as constant `forest_area_ha` and `plot_area_m2` columns in the input file.
- Sampling design can now be supplied as a constant `design` column (`srs` or `systematic`) in the input file.
- When `variables = NULL`, metadata columns are excluded automatically and remaining numeric plot-level attributes are assessed.
- CSV input uses base R; Excel input uses the optional `readxl` package.

# NepInventory 0.1.3

- Corrected the default Nepal Community Forest sampling-intensity benchmark from 0.05% to 0.5%.
- Statistical precision calculations are unchanged.

# NepInventory 0.1.2

- Removed the package-defined `achieved_precision_pct` metric.
- Added explicit `MET` / `NOT MET` status for every user-selected relative-error target.
- Updated printed output to show guideline-based sampling intensity separately from attribute-specific statistical precision.
- Clarified equal-area fixed-plot scope and systematic/fishnet SRS-variance approximation.
- Updated README version and terminology.
- Added a base-R smoke test for the core numerical example.
