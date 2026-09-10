\name{export_inventory_analysis}
\alias{export_inventory_analysis}
\title{Export NepInventory analysis to Excel}
\usage{
export_inventory_analysis(
  x,
  file = "NepInventory_results.xlsx",
  overwrite = TRUE,
  by = NULL,
  plant_category = NULL,
  include_figures = TRUE
)
}
\arguments{
\item{x}{A \code{forest_structure_analysis} object or a long-form NepInventory dataset accepted by \code{structure_analysis()}.}
\item{file}{Output \code{.xlsx} file path.}
\item{overwrite}{Logical; overwrite an existing file? Default \code{TRUE}.}
\item{by}{Optional grouping columns used only when \code{x} is raw data.}
\item{plant_category}{Optional plant-category filter used only when \code{x} is raw data.}
\item{include_figures}{Logical; include available Module 1 figures in corresponding workbook sheets. Default \code{TRUE}.}
}
\value{
Invisibly returns the normalized output file path.
}
\description{
Creates a predictable multi-sheet Excel workbook from a Module 1 analysis. The workbook retains result tables, figure-source data, deterministic results text, skipped-analysis reasons, and available figures. Sheets remain present when an analysis is unsupported and explain why the output was not generated.
}
\examples{
\dontrun{
res <- structure_analysis(my_inventory)
export_inventory_analysis(res, "NepInventory_results.xlsx")
}
}
