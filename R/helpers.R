.validate_scalar_positive <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) || x <= 0) {
    stop(sprintf("`%s` must be one finite positive number.", name), call. = FALSE)
  }
}

.validate_pct <- function(x, name, lower_open = TRUE, upper = 100) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("`%s` must contain finite numeric values.", name), call. = FALSE)
  }
  
  bad_low <- if (lower_open) x <= 0 else x < 0
  
  if (any(bad_low | x >= upper)) {
    stop(sprintf("`%s` must be greater than 0 and less than %s.", name, upper), call. = FALSE)
  }
}

.fpc_factor <- function(n, N, use_fpc) {
  if (!use_fpc) return(1)
  if (n >= N) return(0)
  
  sqrt((N - n) / (N - 1))
}

.required_n_one <- function(cv_ratio,
                            target_pct,
                            confidence,
                            N = Inf,
                            use_fpc = FALSE) {
  if (!is.finite(cv_ratio) || cv_ratio < 0) return(NA_integer_)
  if (cv_ratio == 0) return(2L)
  
  target <- target_pct / 100
  alpha <- 1 - confidence
  max_n <- if (is.finite(N)) as.integer(N) else 100000L
  
  for (n in 2:max_n) {
    tcrit <- stats::qt(1 - alpha / 2, df = n - 1)
    fpc <- .fpc_factor(n, N, use_fpc)
    
    rel_error <- tcrit * cv_ratio / sqrt(n) * fpc
    
    if (is.finite(rel_error) && rel_error <= target) {
      return(as.integer(n))
    }
  }
  
  NA_integer_
}

.metric_summary <- function(x,
                            confidence,
                            N,
                            use_fpc,
                            precision_targets) {
  x <- x[is.finite(x)]
  n <- length(x)
  
  if (n < 2L) {
    return(list(
      n = n,
      mean = if (n == 1L) x else NA_real_,
      sd = NA_real_,
      cv_pct = NA_real_,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      relative_error_pct = NA_real_,
      required = stats::setNames(
        rep(NA_integer_, length(precision_targets)),
        precision_targets
      )
    ))
  }
  
  m <- mean(x)
  s <- stats::sd(x)
  se <- s / sqrt(n)
  
  fpc <- .fpc_factor(n, N, use_fpc)
  se_adj <- se * fpc
  
  alpha <- 1 - confidence
  tcrit <- stats::qt(1 - alpha / 2, df = n - 1)
  
  moe <- tcrit * se_adj
  ci_low <- m - moe
  ci_high <- m + moe
  
  if (abs(m) < .Machine$double.eps) {
    cv_pct <- NA_real_
    re_pct <- NA_real_
    
    req <- stats::setNames(
      rep(NA_integer_, length(precision_targets)),
      precision_targets
    )
  } else {
    cv_ratio <- s / abs(m)
    cv_pct <- 100 * cv_ratio
    re_pct <- 100 * moe / abs(m)
    
    req <- vapply(
      precision_targets,
      function(p) {
        .required_n_one(
          cv_ratio,
          p,
          confidence,
          N = N,
          use_fpc = use_fpc
        )
      },
      integer(1)
    )
    
    names(req) <- precision_targets
  }
  
  list(
    n = n,
    mean = m,
    sd = s,
    cv_pct = cv_pct,
    se = se_adj,
    ci_low = ci_low,
    ci_high = ci_high,
    relative_error_pct = re_pct,
    required = req
  )
}

.pretty_variable <- function(x) {
  out <- gsub("_ha$", "", x)
  out <- gsub("_", " ", out)
  
  tools::toTitleCase(out)
}