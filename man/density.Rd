% Generated manually to match the roxygen documentation in R/density.R
\name{density}
\alias{density}
\title{Estimate stem density from long-form inventory data}
\usage{
density(data, by = NULL, taxon = "species_name", plant_category = NULL)
}
\arguments{
\item{data}{A data.frame or CSV/Excel file path using the NepInventory long-form input convention.}
\item{by}{Optional character vector of grouping columns, such as \code{"group_elevation"} or \code{c("group_elevation", "group_aspect")}.}
\item{taxon}{Taxonomic column used for the breakdown. Defaults to \code{"species_name"}; \code{"genus"} or \code{"family"} can also be used when present.}
\item{plant_category}{Optional character vector selecting one or more values from \code{plant_category}, for example \code{"tree"} or \code{c("seedling", "sapling")}.}
}
\value{
An object of class \code{forest_density} containing \code{result}, \code{plot_data}, \code{figure_data}, \code{interpretation}, and \code{settings}.
}
\description{
Calculates plot-level and summarized stem density (stems per hectare) from the standardized long-form NepInventory input. Counts are expanded by the sampled support area stored in \code{sample_plot_size_m2}.
}
\details{
When \code{plant_category} is available, density is calculated separately by category because nested categories may use different sampled areas. Taxa absent from a sampled plot/category are represented as zero when taxon-level means are calculated.
}
\examples{
dat <- data.frame(
  plot_id = c("P1", "P1", "P2", "P2"),
  species_name = c("Shorea robusta", "Schima wallichii",
                   "Shorea robusta", "Schima wallichii"),
  count = c(4, 2, 3, 1),
  sample_plot_size_m2 = 500
)
density(dat)
}
