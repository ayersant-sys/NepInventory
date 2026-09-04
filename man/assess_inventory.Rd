\name{assess_inventory}
\alias{assess_inventory}

\title{Assess forest inventory sampling intensity and precision}

\usage{
assess_inventory(
  data,
  forest_area_ha,
  plot_area_m2,
  variables = NULL,
  confidence = 0.95,
  precision_targets = c(5, 10, 15),
  guideline_intensity_pct = 0.5,
  design = c("srs", "systematic"),
  fpc = FALSE
)
}

\arguments{
  \item{data}{
    A data frame containing one row per equal-area fixed plot and one or more
    numeric plot-level forest attributes.
  }

  \item{forest_area_ha}{
    Total forest area, in hectares.
  }

  \item{plot_area_m2}{
    Area of one fixed plot, in square metres. All plots must have the same area.
  }

  \item{variables}{
    Character vector naming the numeric plot-level attributes to assess.
    If \code{NULL}, suitable numeric variables are selected automatically.
  }

  \item{confidence}{
    Confidence level used for confidence intervals, relative sampling error,
    and required sample-size calculations. Default is \code{0.95}.
  }

  \item{precision_targets}{
    Numeric vector of desired relative sampling-error targets, expressed as
    percentages. Default is \code{c(5, 10, 15)}.
  }

 \item{guideline_intensity_pct}{
  Sampling-intensity benchmark, expressed as a percentage of total forest
  area. Default is \code{0.5}, representing the general Nepal Community
  Forest reference. The benchmark may be changed by the user where a
  different guideline value applies.
}

  \item{design}{
    Sampling design used for the assessment. Supported values are
    \code{"srs"} and \code{"systematic"}. For systematic sampling, precision
    is estimated using the simple-random-sampling variance estimator as a
    practical approximation.
  }

  \item{fpc}{
    Logical. If \code{TRUE}, apply a finite-population correction based on the
    number of possible equal-area plots in the forest. Default is \code{FALSE}.
  }
}

\description{
Performs a post-inventory assessment of equal-area fixed plots using
plot-level forest attributes. Guideline-based sampling intensity and
attribute-specific statistical precision are reported separately.
}

\value{
An object containing the inventory settings, sampling-intensity assessment,
attribute-specific statistical summaries, precision status for each target,
and required sample sizes.
}