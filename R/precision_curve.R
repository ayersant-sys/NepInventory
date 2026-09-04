#' Build a precision-versus-sample-size curve
#'
#' @param x A `forest_adequacy` object.
#' @param variable One variable name assessed in `x`.
#' @param n_min Minimum sample size shown, default 5.
#' @param n_max Maximum sample size. If NULL, chosen automatically.
#' @return A data.frame with sample size and expected relative error percent.
#' @export
precision_curve <- function(x, variable, n_min = 5L, n_max = NULL) {
  if (!inherits(x, "forest_adequacy")) stop("`x` must be a forest_adequacy object.", call. = FALSE)
  if (length(variable) != 1L || !variable %in% x$variables) {
    stop("`variable` must be one assessed variable name.", call. = FALSE)
  }

  vals <- x$data[[variable]]
  vals <- vals[is.finite(vals)]
  if (length(vals) < 2L) stop("At least two finite observations are needed.", call. = FALSE)
  m <- mean(vals)
  if (abs(m) < .Machine$double.eps) stop("Precision curve is undefined when the mean is zero.", call. = FALSE)
  cv_ratio <- stats::sd(vals) / abs(m)

  req_cols <- grep("^n_required_", names(x$summary), value = TRUE)
  reqs <- unlist(x$summary[x$summary$variable == variable, req_cols, drop = TRUE])
  reqs <- reqs[is.finite(reqs)]
  current_n <- nrow(x$data)

  if (is.null(n_max)) {
    n_max <- max(c(current_n * 2L, reqs, 30), na.rm = TRUE)
    if (x$fpc) n_max <- min(n_max, x$finite_frame_N)
  }
  n_min <- max(2L, as.integer(n_min))
  n_max <- as.integer(n_max)
  if (n_max < n_min) stop("`n_max` must be at least `n_min`.", call. = FALSE)

  alpha <- 1 - x$confidence
  ns <- seq.int(n_min, n_max)
  errs <- vapply(ns, function(n) {
    tcrit <- stats::qt(1 - alpha / 2, df = n - 1)
    N <- if (x$fpc) x$finite_frame_N else Inf
    100 * tcrit * cv_ratio / sqrt(n) * .fpc_factor(n, N, x$fpc)
  }, numeric(1))

  data.frame(
    variable = variable,
    n_plots = ns,
    relative_error_pct = errs,
    current_n = current_n,
    stringsAsFactors = FALSE
  )
}

#' Plot a precision curve
#'
#' @param x A `forest_adequacy` object.
#' @param variable One variable name assessed in `x`.
#' @param n_min Minimum sample size shown. Default 5.
#' @param n_max Maximum sample size shown.
#' @param targets Optional relative-error target lines in percent. Defaults to
#'   the targets stored in `x`.
#' @param show_labels Logical; label target intersections and current sample size.
#' @param ... Additional graphical arguments passed to `plot`.
#' @return Invisibly returns the curve data.
#' @export
plot_precision_curve <- function(x, variable, n_min = 5L, n_max = NULL,
                                 targets = x$precision_targets,
                                 show_labels = TRUE, ...) {
  d <- precision_curve(x, variable, n_min = n_min, n_max = n_max)
  targets <- sort(unique(targets))

  # Focus the plotting window on the range useful for inventory decisions.
  current_error <- d$relative_error_pct[d$n_plots == unique(d$current_n)]
  current_error <- if (length(current_error)) current_error[1] else NA_real_
  y_top <- max(c(targets * 1.35, current_error * 1.25, 20), na.rm = TRUE)
  y_top <- min(y_top, max(d$relative_error_pct, na.rm = TRUE))

  pretty <- .pretty_variable(variable)
  graphics::plot(
    d$n_plots, d$relative_error_pct,
    type = "l",
    xlab = "Number of plots",
    ylab = "Expected relative sampling error (%)",
    main = paste("Sampling precision curve -", pretty),
    ylim = c(0, y_top),
    ...
  )

  current_n <- unique(d$current_n)
  graphics::abline(v = current_n, lty = 2)

  if (length(targets)) {
    graphics::abline(h = targets, lty = 3)

    sm <- x$summary[x$summary$variable == variable, , drop = FALSE]
    for (target in targets) {
      nm <- paste0("n_required_", gsub("\\.", "_", as.character(target)), "pct")
      n_req <- if (nm %in% names(sm)) sm[[nm]][1] else NA_integer_
      if (is.finite(n_req) && n_req >= min(d$n_plots) && n_req <= max(d$n_plots)) {
        graphics::abline(v = n_req, lty = 3)
        if (isTRUE(show_labels)) {
          graphics::text(n_req, target, labels = paste0(target, "% -> n=", n_req),
                         pos = 2, cex = 0.8, xpd = NA)
        }
      }
    }
  }

  if (isTRUE(show_labels)) {
    graphics::mtext(paste0("Current n = ", current_n), side = 3, line = 0.2, adj = 1, cex = 0.8)
  }

  invisible(d)
}
