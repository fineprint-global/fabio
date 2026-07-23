# ==============================================================================
# 00_value_added_helpers.R
# ==============================================================================

library(data.table)

# ==============================================================================
# 1. ROBUST UNIVARIATE STATISTICS  (pure base R)
# ==============================================================================

#' Scaled median absolute deviation (R default: 1.4826 * raw MAD).
#' Thin wrapper so the constant is named in one place.
scaled_mad <- function(x, na.rm = TRUE) {
  stats::mad(x, constant = 1.4826, na.rm = na.rm)
}

#' Sample skewness (bias-corrected / "consistent" estimator).
#' Used to score IHS transforms in the theta search.
calc_skewness <- function(v) {
  n <- length(v); m <- mean(v); s <- stats::sd(v)
  if (!is.finite(s) || s < .Machine$double.eps * max(1, abs(m))) return(NA_real_)
  (n / ((n - 1) * (n - 2))) * sum(((v - m) / s)^3)
}

#' Sample excess kurtosis (bias-corrected).
calc_ex_kurtosis <- function(v) {
  n <- length(v); m <- mean(v); s <- stats::sd(v)
  if (!is.finite(s) || s < .Machine$double.eps * max(1, abs(m))) return(NA_real_)
  (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3)) * sum(((v - m) / s)^4) -
    3 * (n - 1)^2 / ((n - 2) * (n - 3))
}


# ==============================================================================
# 2. HAMPEL TIME-SERIES FILTER  (pure base R, single unified implementation)
# ==============================================================================
#
# ONE rule, used everywhere (replaces three divergent `hampel_filter()`s that
# disagreed on the meaning of `k`, on the eligibility gates, AND on the scale):
#
#   flag x[i]  iff  | x[i] - rolling_window_median[i] |  >  threshold * series_mad
#
# The SCALE is the series-level MAD for EVERY position (the "series MAD
# everywhere" choice). There is no local-window MAD and no fallback branching:
# the old per-window MAD could collapse to 0 in a near-constant edge window and
# silently pass an extreme value through, which is exactly the failure the
# series-MAD fallback was patched in to fix. Making the series MAD the sole
# scale removes that failure mode by construction — the scale is always a
# single, robust, positive number (MAD has a 50% breakdown point, so one spike
# barely moves it), so an extreme value at the very first/last observation is
# still measured against a sane scale and gets flagged.
#
# Parameters (unambiguous names; `k`/`window`/`min_n`/`z` are all gone):
#   half_window     half-window radius (full window = 2*half_window + 1) for the
#                   ROLLING MEDIAN only.
#   threshold       robust z cutoff.
#   min_obs         whole-series gate: fewer than this many finite obs (or a
#                   degenerate, non-positive series MAD) -> pass through
#                   unchanged. Below ~7 the series MAD is too noisy to judge.
#   min_window_obs  a position needs this many finite window points to get a
#                   rolling median (else it is left unevaluated). Default 1 so
#                   the first/last observation IS checked; for the >= min_obs
#                   series here every window already has >= half_window+1 points.
#
# Caveat (the price paid for uniformity): the scale is global, so a MODEST value
# that is outlying only against a locally-calm stretch of a strongly
# heteroscedastic series will NOT be flagged (it is within threshold*series_mad).
# Extreme outliers — the ones that matter — are flagged regardless of position.
#
# Return fields: values, is_spike, window_median, series_mad, mad_used, hampel_z.
# `mad_used` is the series MAD wherever a rolling median exists (NA otherwise) --
# i.e. the single scale the spike test used.
hampel_filter <- function(x,
                          half_window    = 3L,
                          threshold      = 3,
                          min_obs        = 2L * half_window + 1L,
                          min_window_obs = 1L) {
  n        <- length(x)
  med_win  <- rep(NA_real_, n)
  finite_x <- is.finite(x)
  
  # Single scale for the whole series. NA (-> pass through) when there are too
  # few points or the series is essentially constant (no robust scale exists).
  series_mad <- if (sum(finite_x) >= min_obs) scaled_mad(x[finite_x]) else NA_real_
  
  if (!is.finite(series_mad) || series_mad <= 0) {
    return(list(
      values = x,
      is_spike = rep(FALSE, n),
      window_median = med_win,
      series_mad = series_mad,
      mad_used = rep(NA_real_, n),
      hampel_z = rep(NA_real_, n)
    ))
  }
  
  # Rolling-window MEDIAN (robust local level). The window includes x[i]; with
  # half_window = 3 even an edge window has >= 4 points, so a single spike never
  # drags the median onto itself.
  for (i in seq_len(n)) {
    lo  <- max(1L, i - half_window)
    hi  <- min(n,  i + half_window)
    win <- x[lo:hi]
    win <- win[is.finite(win)]
    if (length(win) >= min_window_obs) med_win[i] <- median(win)
  }
  
  z        <- (x - med_win) / series_mad      # series MAD is the scale everywhere
  is_spike <- is.finite(z) & abs(z) > threshold
  values   <- ifelse(is_spike, med_win, x)
  
  has_med  <- is.finite(med_win)
  list(
    values          = values,
    is_spike        = is_spike,
    window_median   = med_win,
    series_mad      = series_mad,
    mad_used        = ifelse(has_med, series_mad, NA_real_),  # scale actually used per cell
    hampel_z        = z
  )
}

#' Grouped Hampel filter over a per-group year series (data.table).
#'
#' Runs hampel_filter() once per group of `dt` (rows ordered by `year`) on the
#' `value_col` series and returns ONE row per (group, year) with the filtered
#' value in `out_col` plus the standard diagnostic columns
#' (window_median / series_mad / mad_used / hampel_z / is_spike). Only finite
#' `value_col` rows are evaluated, exactly as
#' the inline blocks did. This is a PURE transform — it does not touch `dt`; the
#' caller writes the result back (initialising the target column to the raw
#' value so non-finite rows are preserved) and feeds the table to
#' annotate_hampel_diagnostic() / write_va_diagnostic().
hampel_by_series <- function(dt, value_col, out_col, by_cols,
                             half_window = 3L, threshold = 3) {
  dt[is.finite(get(value_col)), {
    ord <- order(year)
    x   <- get(value_col)[ord]
    h   <- hampel_filter(x, half_window = half_window, threshold = threshold)
    setNames(
      list(year[ord], h$values, h$window_median,
           h$series_mad, h$mad_used, h$hampel_z, h$is_spike),
      c("year", out_col, "window_median", "series_mad",
        "mad_used", "hampel_z", "is_spike"))
  }, by = by_cols]
}


# ==============================================================================
# 3. MAD WINSORIZATION  (pure core + data.table grouped wrapper)
# ==============================================================================

#' Compute a MAD-based winsorization band [lo, hi] for a single vector.
#'
#' The cap is median +/- k * scaled_MAD computed in the chosen
#' space, then back-transformed.
#'
#' @param x        numeric vector.
#' @param k        robust-z cutoff (default 2.5). NB: pass the caller's own
#'                 constant; this does not import the script-level WINSOR_MAD_K.
#' @param min_obs  minimum valid observations to build a band (else NA band).
#' @return list(lo, hi, center, scale, log_space, n_obs). lo/hi are NA when the
#'         group is too small or the scale is degenerate.
mad_winsor_band <- function(x, k = 2.5, min_obs = WINSOR_MIN_OBS) {
  valid <- x[is.finite(x)]
  valid <- valid[valid > 0]
  n_obs <- length(valid)
  na_band <- function(center = NA_real_, scale = NA_real_, ls = NA)
    list(lo = NA_real_, hi = NA_real_, center = center, scale = scale,
         log_space = ls, n_obs = n_obs)
  
  if (n_obs < min_obs) return(na_band())
  
  raw_obj <- abs(calc_skewness(valid))      + abs(calc_ex_kurtosis(valid))
  log_obj <- abs(calc_skewness(log(valid))) + abs(calc_ex_kurtosis(log(valid)))
  use_log <- is.finite(raw_obj) && is.finite(log_obj) && log_obj < raw_obj
  
  work   <- if (use_log) log(valid) else valid
  center <- median(work)
  scale  <- scaled_mad(work)
  if (!is.finite(scale) || scale == 0) return(na_band(center, scale, use_log))
  
  lo_t <- center - k * scale
  hi_t <- center + k * scale
  list(lo = if (use_log) exp(lo_t) else lo_t,
       hi = if (use_log) exp(hi_t) else hi_t,
       center = center, scale = scale, log_space = use_log, n_obs = n_obs)
}

#' Vector convenience: clip x to its own MAD band.
mad_winsorize <- function(x, k = 2.5, min_obs = WINSOR_MIN_OBS) {
  band <- mad_winsor_band(x, k = k, min_obs = min_obs)
  if (is.na(band$lo)) return(x)
  pmin(pmax(x, band$lo), band$hi)          # element-wise; preserves NA
}

#' data.table grouped winsor stats. Drop-in for 13_2_clean's
#' `compute_winsor_stats(dt, by_cols)`: same returned column names
#' (n_obs, log_space, lo, hi, center, scale), defaults k=2.5 /
#' min_obs=WINSOR_MIN_OBS (8L).
compute_winsor_stats <- function(dt, by_cols, value_col = "price",
                                 k = 2.5, min_obs = WINSOR_MIN_OBS) {
  dt[, {
    b <- mad_winsor_band(get(value_col), k = k, min_obs = min_obs)
    .(n_obs = b$n_obs, log_space = b$log_space,
      lo = b$lo, hi = b$hi, center = b$center, scale = b$scale)
  }, by = by_cols]
}


# ==============================================================================
# 3b. OWN-SERIES MEDIAN GAP-FILL  (shared price-pipeline rung)
# ==============================================================================

#' Per-series own-median and gate decision for the own-series fill rung.
#'
#' Used by 13_2 (main + cbs_override grids) and 13_3 (FAO producer-price side)
#' to fill a missing (series, year) cell with the median of that series' OWN
#' direct observations before falling back to a cross-sectional median. The
#' returned median is a per-series constant, reused for every missing year of
#' the series.
#'
#' @param direct        data.table of direct (post Hampel/winsor) observations;
#'                      one row per (series, year) with a positive `value_col`.
#' @param series_cols   columns identifying a series (e.g. area_code, item_code).
#' @param item_col      single column identifying the item, used for the
#'                      winsor-band lookup.
#' @param value_col     price column name.
#' @param winsor_stats  per-item winsor stats (compute_winsor_stats output,
#'                      keyed by `item_col`, carrying lo/hi). Required when
#'                      PRICE_REQUIRE_WINSOR_BAND is TRUE; an item "has a band"
#'                      iff its lo/hi are finite.
#' @return data.table keyed by `series_cols` with `own_med` (series median) and
#'         `gate_rejected` (TRUE where the series has obs but the band
#'         requirement routes it to the cross-sectional rung). The caller fills
#'         `is.na(price)` cells with `own_med` where `!gate_rejected`.
own_series_median_fill <- function(direct, series_cols, item_col,
                                   value_col = "price", winsor_stats = NULL) {
  d  <- as.data.table(direct)
  d  <- d[is.finite(get(value_col)) & get(value_col) > 0]
  ps <- d[, .(own_med = median(get(value_col))), by = series_cols]
  if (nrow(ps) == 0L) {
    ps[, gate_rejected := logical()]
    return(ps[])
  }
  
  if (PRICE_REQUIRE_WINSOR_BAND) {
    if (is.null(winsor_stats))
      stop("own_series_median_fill(): PRICE_REQUIRE_WINSOR_BAND is TRUE but ",
           "no winsor_stats were supplied.")
    wb <- unique(as.data.table(winsor_stats)[, c(item_col, "lo", "hi"), with = FALSE])
    wb[, has_band := is.finite(lo) & is.finite(hi)]
    ps[wb, has_band := i.has_band, on = item_col]
    ps[is.na(has_band), has_band := FALSE]
  } else {
    ps[, has_band := TRUE]
  }
  
  ps[, gate_rejected := PRICE_REQUIRE_WINSOR_BAND & !has_band]
  ps[, has_band := NULL]
  ps[]
}


# ==============================================================================
# 4. IHS THETA SEARCH + IHS/MAD WINSOR
# ==============================================================================
#
# Used by 14_1 (MRIOTs, stages 4b + 7b), 14_2 (FSDN, stage 8b) and 14_3
# (national_SUTs, stage 8b): each calls fit_ihs_theta() per group, joins the
# theta back, then calls ihs_mad_winsor() per group with its own WINSOR_MAD_K.
# The theta grid / lo / hi were declared identically in all of them.

IHS_THETA_GRID <- 10^seq(-4, 12, by = 0.5)
IHS_THETA_LO   <- min(IHS_THETA_GRID)
IHS_THETA_HI   <- max(IHS_THETA_GRID)

#' Pick the IHS scale theta that makes asinh(x*theta) most Gaussian
#' (minimises |skewness| + |excess kurtosis|).  Returns a 1-row list suitable
#' for use inside `dt[, fit_ihs_theta(x), by = item]`.
#'
#' Degenerate fits (non-finite objective, objective > obj_max, or theta pinned
#' at a grid edge) return optimal_theta = NA, signalling the caller to fall
#' back to a raw (non-IHS) MAD cap.
fit_ihs_theta <- function(x, grid = IHS_THETA_GRID, min_obs = WINSOR_MIN_OBS, obj_max = 10) {
  x <- x[is.finite(x)]
  if (length(x) < min_obs)
    return(list(optimal_theta = NA_real_, objective = NA_real_, n_obs = length(x)))
  scores <- vapply(grid, function(theta) {
    ihs <- asinh(x * theta)
    abs(calc_skewness(ihs)) + abs(calc_ex_kurtosis(ihs))
  }, numeric(1))
  if (!any(is.finite(scores)))
    return(list(optimal_theta = NA_real_, objective = NA_real_, n_obs = length(x)))
  best <- which.min(scores)
  theta <- grid[best]; obj <- scores[best]
  degenerate <- !is.finite(obj) || obj > obj_max ||
    theta == min(grid) || theta == max(grid)
  list(optimal_theta = if (degenerate) NA_real_ else theta,
       objective = obj, n_obs = length(x))
}

#' Winsorize one vector at median +/- k * MAD, optionally in IHS space.
#' Replaces the per-item `[, { ... }, by]` winsor block duplicated in
#' MRIOTs (stage 4b/7b) and national_SUTs (stage 8b). `theta = NA` -> raw space.
#' Returns per-element values plus the cap band and robust z in the cap space.
ihs_mad_winsor <- function(x, theta = NA_real_, k = 2.5) {
  use_ihs <- is.finite(theta)
  vals_ws <- if (use_ihs) asinh(x * theta) else x
  med     <- median(vals_ws, na.rm = TRUE)
  mad_val <- scaled_mad(vals_ws)
  if (is.finite(mad_val) && mad_val > 0) {
    lo_ws <- med - k * mad_val; hi_ws <- med + k * mad_val
    cap_lo <- if (use_ihs) sinh(lo_ws) / theta else lo_ws
    cap_hi <- if (use_ihs) sinh(hi_ws) / theta else hi_ws
    mad_z  <- (vals_ws - med) / mad_val
  } else {
    cap_lo <- -Inf; cap_hi <- Inf; mad_z <- rep(NA_real_, length(x))
  }
  list(values = pmin(pmax(x, cap_lo), cap_hi),
       cap_lower = cap_lo, cap_upper = cap_hi,
       mad_z = mad_z, center = med, scale = mad_val, used_ihs = use_ihs)
}

#' Standard progress message after a grouped `fit_ihs_theta()` pass.
#'
#' `theta_dt` is the table returned by `dt[, fit_ihs_theta(x), by = ...]`
#' (one row per group with optimal_theta / objective / n_obs). Degenerate
#' fits are the rows where optimal_theta is NA — `fit_ihs_theta()` already
#' folds the small-n, bad-objective, and grid-edge cases into NA, so no
#' separate `degenerate` column is needed (a pre-existing one is honoured).
#'
#' @param cell_label  noun used in the message, e.g. "items",
#'                    "(item × strand) cells", "(sector × strand) cells".
va_report_theta <- function(theta_dt, cell_label = "groups", indent = "    ") {
  n_total <- nrow(theta_dt)
  n_ok    <- theta_dt[!is.na(optimal_theta), .N]
  n_bad   <- if ("degenerate" %in% names(theta_dt))
    theta_dt[, sum(degenerate)] else n_total - n_ok
  if (n_ok > 0L) {
    message(sprintf(
      "%s\u03b8 picked for %d / %d %s  (range %g\u2013%g; %d fall back to raw MAD).",
      indent, n_ok, n_total, cell_label,
      theta_dt[!is.na(optimal_theta), min(optimal_theta)],
      theta_dt[!is.na(optimal_theta), max(optimal_theta)],
      n_bad))
  } else {
    message(sprintf(
      "%sNo %s converged to a valid \u03b8; all %d fall back to raw MAD.",
      indent, cell_label, n_total))
  }
  invisible(theta_dt)
}

#' One full per-component IHS+MAD cap pass on (component / denominator), with
#' write-back and per-component diagnostic. (data.table; mutates `dt` by ref.)
#'
#' This is the shared stage-8b loop BODY of 14_2 (FSDN) and 14_3 (national_SUTs):
#'   1. intensity_i = dt[[vc]] / dt[[denom_col]] where both finite and vc != 0
#'   2. eligibility  = finite intensity [AND NOT dt[[exempt_col]] when given]
#'   3. per-item θ via fit_ihs_theta(); per-item cap via ihs_mad_winsor(k)
#'   4. write the capped intensity * denom back into dt[[vc]] on eligible rows
#'   5. build + annotate the per-component winsor diagnostic (sign-flip included)
#'
#' Temp columns (tmp_intensity / tmp_elig / tmp_theta / tmp_intensity_ws) are
#' created and dropped INSIDE this call, so the caller's `dt` is left with only
#' the updated `vc` column. Returns the diagnostic data.table for this component
#' (an empty data.table when no rows are eligible) for the caller to rbindlist().
#'
#' @param exempt_col  name of a logical column in `dt` whose TRUE rows are held
#'                     out of the pool AND never overwritten (national_SUTs'
#'                     `preserve_idx`); NULL for no carve-out (FSDN).
cap_component_by_item <- function(dt, vc, k,
                                  denom_col = "phys_denom",
                                  exempt_col = NULL,
                                  indent = "    ") {
  dt[, tmp_intensity := fifelse(
    is.finite(get(denom_col)) & is.finite(get(vc)) & get(vc) != 0,
    get(vc) / get(denom_col), NA_real_)]
  dt[, tmp_elig := if (is.null(exempt_col)) is.finite(tmp_intensity)
     else is.finite(tmp_intensity) & !get(exempt_col)]
  
  n_eligible <- dt[tmp_elig == TRUE, .N]
  if (is.null(exempt_col)) {
    message(sprintf("%s%d row(s) eligible for the per-item MAD pool.",
                    indent, n_eligible))
  } else {
    n_skip <- dt[is.finite(tmp_intensity) & get(exempt_col) == TRUE, .N]
    message(sprintf("%s%d row(s) eligible; %d preserved-item row(s) exempted.",
                    indent, n_eligible, n_skip))
  }
  
  if (n_eligible == 0L) {
    message(sprintf("%sNo eligible rows \u2014 skipping cap for this component.", indent))
    dt[, c("tmp_intensity", "tmp_elig") := NULL]
    return(data.table())
  }
  
  theta_by_item <- dt[tmp_elig == TRUE, fit_ihs_theta(tmp_intensity),
                      by = .(fabio_item_code)]
  theta_by_item[, degenerate := is.na(optimal_theta)]
  va_report_theta(theta_by_item, "items", indent = indent)
  dt[theta_by_item, tmp_theta := i.optimal_theta, on = "fabio_item_code"]
  
  winsor_one <- dt[tmp_elig == TRUE, {
    w <- ihs_mad_winsor(tmp_intensity, tmp_theta[1L], k = k)
    .(fabio_area_code = fabio_area_code, year = year,
      intensity_pre = tmp_intensity, intensity_post = w$values,
      cap_lower = w$cap_lower, cap_upper = w$cap_upper, mad_z = w$mad_z)
  }, by = .(fabio_item_code)]
  
  dt[winsor_one, tmp_intensity_ws := i.intensity_post,
     on = .(fabio_item_code, year, fabio_area_code)]
  dt[, (vc) := fifelse(
    tmp_elig == TRUE & is.finite(tmp_intensity_ws),
    tmp_intensity_ws * get(denom_col), get(vc))]
  
  diag_one <- merge(
    winsor_one,
    dt[, .(fabio_area_code, fabio_item_code, year, fabio_area, fabio_item)],
    by = c("fabio_area_code", "fabio_item_code", "year"))
  diag_one[theta_by_item, item_theta := i.optimal_theta, on = "fabio_item_code"]
  diag_one[, va_account := vc]
  annotate_winsor_diagnostic(diag_one, pre_col = "intensity_pre",
                             post_col = "intensity_post", add_sign_flip = TRUE)
  
  message(sprintf(
    "%sMAD-capped %d / %d eligible row(s) (%.2f%%); %d crossed zero at the cap.",
    indent, diag_one[winsorized == TRUE, .N], nrow(diag_one),
    if (nrow(diag_one) > 0L) 100 * diag_one[winsorized == TRUE, .N] / nrow(diag_one) else 0,
    diag_one[sign_flipped_at_cap == TRUE, .N]))
  
  dt[, c("tmp_intensity", "tmp_intensity_ws", "tmp_elig", "tmp_theta") := NULL]
  diag_one
}

#' Stage 4b / 7b WINSOR body shared by 14_1's two strands (phys + share).
#'
#' Both strands run the IDENTICAL sequence on their Hampel-filtered intensity:
#'   1. per-(group, va_component) IHS theta via fit_ihs_theta()
#'   2. per-(group, va_component) IHS+MAD cap via ihs_mad_winsor(k)
#'   3. write the capped value back onto dt[[winsor_col]] (by reference)
#'   4. build + annotate + write the per-strand winsor diagnostic
#' They differ ONLY in column / key NAMES and the diagnostic schema, so each of
#' those is a parameter. `group_cols` already INCLUDES va_component (it is the
#' full pooling key); `(group_cols, year, other_key)` must uniquely identify a
#' row. The capped value is carried internally under a fixed temp name so the
#' update-join can use a literal `i.` reference regardless of `winsor_col`.
#'
#' NB: the Hampel stage (4a/7a) is deliberately LEFT INLINE in the caller, not
#' folded in here, because the share strand drops its Hampel buffer years BETWEEN
#' Hampel and winsor while the phys strand does not — keeping the two stages
#' separate keeps that asymmetry visible at the call site.
#'
#' Mutates `dt` by reference (adds `winsor_col` and the temp `theta_col`; the
#' caller drops `theta_col` in its own cleanup, exactly as before). Returns the
#' (invisible) diagnostic data.table.
#'
#' @param dt             strand table (mutated by reference).
#' @param hampel_col     Hampel-filtered intensity column already on `dt`.
#' @param winsor_col     name of the capped column to create on `dt`.
#' @param group_cols     pooling key INCLUDING va_component, e.g.
#'                       c("fabio_item_code", "va_component").
#' @param other_key      remaining key making (group, year, other_key) unique,
#'                       e.g. "fabio_area_code" / "region_code".
#' @param theta_col      name of the per-group theta column to add to `dt`.
#' @param k              MAD multiplier (WINSOR_MAD_K).
#' @param diag_id_cols   columns pulled from `dt` for the diagnostic; must
#'                       include the join keys, the descriptive columns, the raw
#'                       + Hampel intensity, and `theta_col`.
#' @param diag_col_order final column order for the diagnostic CSV.
#' @param out_path       diagnostic CSV path.
#' @param theta_label    cell label for va_report_theta().
#' @param write_label    label for write_va_diagnostic()'s summary line.
#' @param indent         message indent.
cap_strand_by_group <- function(dt, hampel_col, winsor_col,
                                group_cols, other_key, theta_col, k,
                                diag_id_cols, diag_col_order, out_path,
                                theta_label = "(group \u00d7 strand) cells",
                                write_label = "entries",
                                indent      = "    ") {
  WTMP <- "winsor_tmp_val"   # fixed internal name -> literal i.winsor_tmp_val join
  
  # 1. per-(group, va_component) IHS theta on the Hampel-filtered series
  theta_dt <- dt[is.finite(get(hampel_col)),
                 fit_ihs_theta(get(hampel_col)), by = group_cols]
  # fit_ihs_theta() already returns optimal_theta = NA for degenerate fits
  # (n < 8, objective > 10, or theta pinned at a grid edge).
  theta_dt[, degenerate := is.na(optimal_theta)]
  va_report_theta(theta_dt, theta_label, indent = indent)
  dt[theta_dt, (theta_col) := i.optimal_theta, on = group_cols]
  
  # 2. per-(group, va_component) IHS+MAD cap
  ws <- dt[is.finite(get(hampel_col)), {
    w <- ihs_mad_winsor(get(hampel_col), get(theta_col)[1L], k = k)
    setNames(
      list(get(other_key), year, w$values, w$cap_lower, w$cap_upper, w$mad_z),
      c(other_key, "year", WTMP, "cap_lower", "cap_upper", "mad_z"))
  }, by = group_cols]
  
  # 3. write the capped intensity back onto dt
  join_keys <- c(group_cols, "year", other_key)
  dt[ws, (winsor_col) := i.winsor_tmp_val, on = join_keys]
  
  # 4. per-strand diagnostic
  diag <- merge(ws, dt[, ..diag_id_cols], by = join_keys)
  setnames(diag, WTMP, winsor_col)
  annotate_winsor_diagnostic(diag, pre_col = hampel_col, post_col = winsor_col)
  write_va_diagnostic(diag, type = "winsor",
                      sort_first = "va_component",
                      out_path   = out_path,
                      col_order  = diag_col_order,
                      label      = write_label,
                      indent     = indent)
  invisible(diag)
}


# ==============================================================================
# 5. DIAGNOSTIC WRITERS
# ==============================================================================
#
#' `dt` must carry: <key_cols>, price, price_hampel_filtered, window_median,
#' series_mad, hampel_flag.
build_hampel_diagnostic <- function(dt, key_cols, out_path) {
  diag <- dt[, c(mget(key_cols),
                 list(price_pre_hampel  = price,
                      price_post_hampel = price_hampel_filtered,
                      window_median     = window_median,
                      series_mad        = series_mad,
                      is_spike          = hampel_flag))]
  # Under the series-MAD rule the series MAD is the single spike-test scale;
  # NA where the series had no robust scale (degenerate / too short).
  diag[, mad_used := fifelse(is.finite(series_mad) & series_mad > 0,
                             series_mad, NA_real_)]
  diag[, hampel_z := fifelse(
    is.finite(mad_used) & mad_used > 0,
    (price_pre_hampel - window_median) / mad_used, NA_real_)]
  diag[, `:=`(abs_change   = abs(price_pre_hampel - price_post_hampel),
              abs_hampel_z = abs(hampel_z))]
  diag[, direction := fifelse(!is_spike, "none",
                              fifelse(hampel_z > 0, "high", "low"))]
  setcolorder(diag, c(key_cols,
                      "price_pre_hampel", "price_post_hampel",
                      "window_median", "series_mad", "mad_used",
                      "is_spike", "direction", "abs_change", "hampel_z", "abs_hampel_z"))
  setorder(diag, -abs_hampel_z, -abs_change, na.last = TRUE)
  fwrite(diag, out_path)
  invisible(diag)
}

#' `dt` must carry: <key_cols>, price_pre_wins, price, lo, hi, log_space,
#' n_item, mad_z.
build_winsor_diagnostic <- function(dt, key_cols, out_path) {
  diag <- dt[, c(mget(key_cols),
                 list(price_pre  = price_pre_wins,
                      price_post = price,
                      cap_lower  = lo,
                      cap_upper  = hi,
                      log_space  = log_space,
                      n_item     = n_item,
                      mad_z      = mad_z))]
  diag[, winsorized := !is.na(cap_lower) & !is.na(cap_upper) &
         (price_pre < cap_lower | price_pre > cap_upper)]
  diag[, direction := fifelse(!winsorized, "none",
                              fifelse(price_pre > cap_upper, "high", "low"))]
  diag[, `:=`(abs_change = abs(price_pre - price_post), abs_mad_z = abs(mad_z))]
  setcolorder(diag, c(key_cols,
                      "price_pre", "price_post", "cap_lower", "cap_upper", "log_space", "n_item",
                      "winsorized", "direction", "abs_change", "mad_z", "abs_mad_z"))
  setorder(diag, -abs_mad_z, -abs_change, na.last = TRUE)
  fwrite(diag, out_path)
  invisible(diag)
}

#' Annotate a winsor diagnostic table IN PLACE (by reference).
#'
#' `diag` must already carry `cap_lower`, `cap_upper`, `mad_z` plus the
#' pre/post value columns named by `pre_col` / `post_col`. Adds:
#'   winsorized, abs_change, abs_mad_z, direction
#'   [+ sign_flipped_at_cap when add_sign_flip = TRUE]
#' This replaces the `[, := ...]` finisher blocks formerly duplicated in
#' 14_1 (phys + share), 14_2, and 14_3.
annotate_winsor_diagnostic <- function(diag, pre_col, post_col,
                                       add_sign_flip = FALSE) {
  diag[, winsorized := get(pre_col) < cap_lower | get(pre_col) > cap_upper]
  diag[, `:=`(abs_change = abs(get(pre_col) - get(post_col)),
              abs_mad_z  = abs(mad_z))]
  diag[, direction := fifelse(
    !winsorized, "none",
    fifelse(get(pre_col) > cap_upper, "high", "low"))]
  if (add_sign_flip)
    diag[, sign_flipped_at_cap :=
           is.finite(get(pre_col)) & is.finite(get(post_col)) &
           get(pre_col) != 0 & get(post_col) != 0 &
           sign(get(post_col)) != sign(get(pre_col))]
  invisible(diag)
}

#' Annotate a Hampel diagnostic table IN PLACE (by reference).
#'
#' `diag` must already carry `is_spike` and `hampel_z` plus the pre/post
#' columns. Adds: abs_change, abs_hampel_z, direction.
annotate_hampel_diagnostic <- function(diag, pre_col, post_col) {
  diag[, `:=`(abs_change   = abs(get(pre_col) - get(post_col)),
              abs_hampel_z = abs(hampel_z))]
  diag[, direction := fifelse(
    !is_spike, "none",
    fifelse(hampel_z > 0, "high", "low"))]
  invisible(diag)
}

#' Order, write, and (optionally) summarise an annotated VA diagnostic.
#'
#' Shared tail of every VA diagnostic block: setcolorder(col_order), sort by
#' (sort_first asc, abs-z desc, abs_change desc, NAs last), fwrite, and print
#' the standard "<verb> X / N <label> (..%); diagnostic → path" line.
#'
#' @param type        "winsor" (flag col `winsorized`, z col `abs_mad_z`) or
#'                    "hampel" (flag col `is_spike`, z col `abs_hampel_z`).
#' @param sort_first  ascending sort keys placed before the z/abs_change keys
#'                    (e.g. "va_component" / "va_account"); may be empty.
#' @param announce    FALSE to skip the summary message (callers with their
#'                    own richer message, e.g. sign-flip counts).
write_va_diagnostic <- function(diag, col_order, out_path,
                                type = c("winsor", "hampel"),
                                sort_first = character(0),
                                label = "entries",
                                indent = "    ",
                                announce = TRUE) {
  type  <- match.arg(type)
  abs_z <- if (type == "winsor") "abs_mad_z"  else "abs_hampel_z"
  flag  <- if (type == "winsor") "winsorized" else "is_spike"
  verb  <- if (type == "winsor") "MAD-capped" else "Hampel-flagged"
  if (!is.null(col_order)) setcolorder(diag, col_order)
  setorderv(diag, c(sort_first, abs_z, "abs_change"),
            order = c(rep(1L, length(sort_first)), -1L, -1L),
            na.last = TRUE)
  fwrite(diag, out_path)
  if (announce) {
    n_total <- nrow(diag)
    n_flag  <- diag[get(flag) == TRUE, .N]
    message(sprintf(
      "%s%s %d / %d %s (%.2f%%); diagnostic \u2192 %s",
      indent, verb, n_flag, n_total, label,
      if (n_total > 0) 100 * n_flag / n_total else 0, out_path))
  }
  invisible(diag)
}

#' Add the four group-reconciliation mismatch metrics IN PLACE (by reference).
#'
#' `b_col` is the FABIO side, `a_col` the source/base side; diff = b - a and the
#' ratio is b / a (matching both originals).
add_reconciliation_metrics <- function(recon, a_col, b_col,
                                       diff_col  = "diff",
                                       abs_col   = "abs_diff",
                                       ratio_col = "ratio_fabio_over_src",
                                       pct_col   = "pct_mismatch") {
  recon[is.na(get(a_col)), (a_col) := 0]
  recon[is.na(get(b_col)), (b_col) := 0]
  recon[, (diff_col)  := get(b_col) - get(a_col)]
  recon[, (abs_col)   := abs(get(diff_col))]
  recon[, (ratio_col) := fifelse(get(a_col) != 0, get(b_col) / get(a_col), NA_real_)]
  recon[, (pct_col)   := fifelse(
    pmax(abs(get(b_col)), abs(get(a_col))) > 0,
    100 * get(diff_col) / pmax(abs(get(b_col)), abs(get(a_col))),
    NA_real_)]
  invisible(recon)
}


# ==============================================================================
# 6. FABIO TOTAL-VALUES LOADER  (data.table)
# ==============================================================================
#
#
# Default behaviour == national_SUTs / Canada / Brazil: enforce the full SUT
# column set, return list(fv, value_col, output_col).
#   * MRIOTs called a lighter version: pass required_cols = character(0).
#   * FSDN renamed the value/output columns to stable names and returned the dt
#     with label attributes: pass rename_to_stable = TRUE.
prepare_fv <- function(path,
                       required_cols = c(
                         "area_code", "area", "item_code", "item", "year",
                         "row_id", "iso3c", "comm_code", "comm_group", "unit",
                         "price [USD/unit]", "price_source",
                         "price_source_constituents"),
                       drop_extra = c("production [tonnes]",
                                      "total_value_source",
                                      "sua_aggregated_value [USD]",
                                      "sua_aggregated_production [tonnes]"),
                       rename_to_stable = FALSE) {
  if (!file.exists(path))
    stop("FABIOv2 total_values RDS not found at ", path,
         ". Update the path, or run 01_total_value_FABIO_v2 first.")
  
  fv <- as.data.table(readRDS(path))
  
  value_col  <- grep("^total_value \\[",          names(fv), value = TRUE)
  output_col <- grep("^total_product_output \\[", names(fv), value = TRUE)
  if (length(value_col) != 1L)
    stop("Expected exactly one 'total_value [...]' column in ", path,
         "; found: ", paste(value_col, collapse = ", "))
  if (length(output_col) != 1L)
    stop("Expected exactly one 'total_product_output [...]' column in ", path,
         "; found: ", paste(output_col, collapse = ", "))
  
  missing_cols <- setdiff(required_cols, names(fv))
  if (length(missing_cols) > 0L)
    stop("FABIOv2 total_values RDS at ", path,
         " is missing required column(s): ", paste(missing_cols, collapse = ", "))
  
  setnames(fv,
           old = c("area_code", "item_code", "area", "item"),
           new = c("fabio_area_code", "fabio_item_code", "fabio_area", "fabio_item"))
  
  drop_cols <- intersect(drop_extra, names(fv))
  if (length(drop_cols) > 0L) fv[, (drop_cols) := NULL]
  
  fv[, `:=`(fabio_area_code = as.integer(fabio_area_code),
            fabio_item_code = as.integer(fabio_item_code),
            year            = as.integer(year))]
  
  if (rename_to_stable) {
    setnames(fv, value_col,  "total_value")
    setnames(fv, output_col, "total_product_output")
    attr(fv, "value_col_label")  <- value_col
    attr(fv, "output_col_label") <- output_col
    return(fv)
  }
  list(fv = fv, value_col = value_col, output_col = output_col)
}


# ==============================================================================
# 7. FAOSTAT EXCHANGE-RATE LOADER  (data.table)
# ==============================================================================
#
#' Read a FAOSTAT exchange-rate bulk file, filter to one Element Code, and
#' return finite, positive (Area Code, year, rate) cells.
#'
#' Reads the *Normalized* (long) FAOSTAT layout that R/00_1_prep_fao.R now
#' downloads: one row per area / element / currency / months / year, with a
#' single `Value` column -- so no Y<year> melt is needed. Rows are filtered to
#' the requested Element Code (a STRING, e.g. "SLC", in this file) and to
#' annual values (Months Code 7021); the monthly rows sharing the same Element
#' Code must be excluded so they cannot win the first-per-(area, year) pick in
#' faostat_rate_table().
read_faostat_exchange_long <- function(path, element = "SLC") {
  if (!file.exists(path))
    stop("FAOSTAT exchange-rate file not found at:\n  ", path)
  xr <- as.data.table(fread(path))
  for (col in c("Element Code", "Area Code", "Year", "Value"))
    if (!(col %in% names(xr)))
      stop("Exchange-rate file has no '", col, "' column. Found: ",
           paste(names(xr), collapse = ", "))
  xr <- xr[trimws(as.character(`Element Code`)) == element]
  if ("Months Code" %in% names(xr)) xr <- xr[`Months Code` == 7021L]
  if (nrow(xr) == 0L)
    stop("No annual rows with Element Code == '", element, "' in ", path, ".")
  xr[, `:=`(area_code = suppressWarnings(as.integer(`Area Code`)),
            year      = suppressWarnings(as.integer(Year)),
            rate      = suppressWarnings(as.numeric(Value)))]
  xr[is.finite(area_code) & is.finite(year) & is.finite(rate) & rate > 0,
     .(area_code, year, rate)]
}

#' All-areas keyed table (the shape 05's load_lcu_usd returned).
#' First finite-positive value per (area, year).
faostat_rate_table <- function(path, element = "SLC",
                               out_col = "rate_lcu_per_usd") {
  long <- read_faostat_exchange_long(path, element)
  rt <- long[, .(rate = rate[1L]), by = .(area_code, year)]
  setnames(rt, "rate", out_col)
  setnames(rt, "area_code", "fabio_area_code")
  setkey(rt, fabio_area_code, year)
  rt[]
}


# ==============================================================================
# 8. CONCORDANCE LOADERS  (data.table)
# ==============================================================================
#
# The item-concordance loaders in national_SUTs and FSDN are the same shape:
# read CSV, require an ISIC column, filter to one ISIC level with a non-blank
# source code and a non-NA FABIO code, return (source code, source label, FABIO
# code). Only the source COLUMN NAMES differed. This generic version returns
# STANDARD output names (src_item_code / src_item / fabio_item_code); callers
# that want their old names (biosam_item_code, usa_sut_code, ...) add one
# setnames(), or pass out_code / out_item.
load_item_conc <- function(path, isic_level, code_col, item_col,
                           fabio_code_col = "FABIO_item_code",
                           out_code = "src_item_code",
                           out_item = "src_item",
                           keep_code_class_char = TRUE,
                           encoding = "UTF-8") {
  cc <- if (keep_code_class_char) list(character = code_col) else NULL
  ic <- fread(path, encoding = encoding, colClasses = cc)
  if (!"ISIC" %in% names(ic))
    stop("Concordance ", path, " is missing the required 'ISIC' column.")
  req <- c(code_col, item_col, fabio_code_col)
  miss <- setdiff(req, names(ic))
  if (length(miss) > 0L)
    stop("Concordance ", path, " is missing column(s): ", paste(miss, collapse = ", "))
  ic <- ic[
    toupper(trimws(as.character(ISIC))) == toupper(isic_level) &
      !is.na(get(code_col)) & trimws(as.character(get(code_col))) != "" &
      !is.na(get(fabio_code_col)),
    setNames(list(trimws(as.character(get(code_col))),
                  trimws(as.character(get(item_col))),
                  as.integer(get(fabio_code_col))),
             c(out_code, out_item, "fabio_item_code"))
  ]
  unique(ic[!is.na(get(out_code)) & get(out_code) != "" & !is.na(fabio_item_code)])
}

#' Generic area-concordance loader. Returns standard names
#' (region_code, fabio_area_code).
load_area_conc <- function(path, code_col, fabio_col,
                           out_code = "region_code",
                           out_fabio = "fabio_area_code",
                           fabio_as_integer = TRUE) {
  ac <- fread(path)
  miss <- setdiff(c(code_col, fabio_col), names(ac))
  if (length(miss) > 0L)
    stop("Area concordance ", path, " is missing column(s): ",
         paste(miss, collapse = ", "))
  ac <- ac[!is.na(get(code_col)) & trimws(as.character(get(code_col))) != "" &
             !is.na(get(fabio_col)) & trimws(as.character(get(fabio_col))) != ""]
  fabio_out <- if (fabio_as_integer)
    as.integer(ac[[fabio_col]]) else trimws(as.character(ac[[fabio_col]]))
  out <- data.table(code  = trimws(as.character(ac[[code_col]])),
                    fabio = fabio_out)
  setnames(out, c("code", "fabio"), c(out_code, out_fabio))
  unique(out[!is.na(get(out_fabio))])
}


# ==============================================================================
# 9. GENERIC AGGREGATION HELPERS
# ==============================================================================
#
#' Cartesian product of two data.tables (was local to 13_3_FABIO_v2_price_extension).
cross_join <- function(dt_a, dt_b) {
  a <- copy(dt_a)[, .cj_key := 1L]
  b <- copy(dt_b)[, .cj_key := 1L]
  out <- merge(a, b, by = ".cj_key", allow.cartesian = TRUE)
  out[, .cj_key := NULL]
  out
}

#' Weight > 0 weighted mean, falling back to a simple mean, then NA.
wmean_price <- function(price, weight) {
  valid <- !is.na(price) & !is.na(weight) & weight > 0
  if (any(valid)) return(weighted.mean(price[valid], weight[valid]))
  if (any(!is.na(price))) return(mean(price[!is.na(price)]))
  NA_real_
}

#' Mean of the non-NA entries, NA if none.
safe_mean <- function(x) {
  ok <- !is.na(x)
  if (any(ok)) mean(x[ok]) else NA_real_
}

# ==============================================================================
# 10. SHARED CANADA SUT PARSERS (network-free)
# ==============================================================================
#
#' Canonicalise a member label for matching: typographic -> ASCII apostrophe, trimmed.
nrm_label <- function(s) trimws(gsub("\u2019", "'", s))

#' Extract a clean IOIC code from a bracketed classification-code cell e.g. "[MPG111A01]".
clean_ioic_code <- function(x) {
  x <- as.character(x)
  x <- gsub("[\\[\\]]", "", x, perl = TRUE)
  trimws(x)
}

#' Split a cansim `Product` member into (product_full, ioic_code, bare_label).
#' Handles the trailing bracketed IOIC suffix and cansim's duplicate-name
#' "==> Total products [73]" hierarchy disambiguation (numeric brackets -> NA;
#' " ==> ..." tails stripped from the bare label).
split_product_code <- function(x) {
  x     <- as.character(x)
  has   <- grepl("\\[[^][]+\\]\\s*$", x)
  code  <- ifelse(has, trimws(sub(".*\\[([^][]+)\\]\\s*$", "\\1", x)), NA_character_)
  code  <- ifelse(!is.na(code) & !grepl("[A-Za-z]", code), NA_character_, code)
  label <- trimws(sub("\\s*\\[[^][]+\\]\\s*$", "", x))
  label <- trimws(sub("\\s*==>.*$", "", label))
  data.table::data.table(product_full = x, ioic_code = code, bare_label = label)
}

#' Locate product/industry label columns (and any product classification-code column).
detect_sut_columns <- function(dt) {
  nm <- names(dt)
  prod_label <- if ("Product"  %in% nm) "Product"  else
    stop("cansim SUT slice has no 'Product' column; columns: ", paste(nm, collapse = ", "))
  ind_label  <- if ("Industry" %in% nm) "Industry" else NA_character_
  cand <- grep("Classification Code for Product", nm, value = TRUE, fixed = TRUE)
  if (length(cand) == 0L)
    cand <- grep("Product", grep("Code", nm, value = TRUE, ignore.case = TRUE), value = TRUE)
  prod_code <- if (length(cand) >= 1L) cand[1L] else NA_character_
  list(prod_label = prod_label, prod_code = prod_code, ind_label = ind_label)
}

#' From a Supply (Make) numerator slice, derive the long numerator table
#' (industry, industry_raw, sut_item_code, year, sup_val). `industry_raw` is the
#' DB-literal member used to FILTER the denominator pull (cansim normalization
#' rewrites Industry the same way it rewrites Product, so the denominator filter
#' must use the pre-normalization string).
prep_supply_numerators <- function(sup_raw, conc_products_codes,
                                   conc_products_labels, use_years) {
  if (nrow(sup_raw) == 0L)
    stop("Supply (Make) numerator slice is empty \u2014 check that concordance ",
         "codes match the bracketed IOIC codes in product_dimension(con)$ioic_code.")
  cols <- detect_sut_columns(sup_raw)
  if (is.na(cols$ind_label)) stop("Supply slice has no 'Industry' column.")
  prod_src <- if ("product_raw" %in% names(sup_raw))
    sup_raw$product_raw else sup_raw[[cols$prod_label]]
  parsed   <- split_product_code(as.character(prod_src))
  code_col <- if (!is.na(cols$prod_code))
    clean_ioic_code(as.character(sup_raw[[cols$prod_code]])) else NA_character_
  
  ind_raw_src <- if ("industry_raw" %in% names(sup_raw)) {
    as.character(sup_raw$industry_raw)
  } else {
    warning("prep_supply_numerators: numerator slice has no 'industry_raw' column. ",
            "The Make denominator filter will fall back to the normalized label and ",
            "may miss industries \u2014 re-stage the Canada slices by re-running ",
            "R/00_9_prep_value_added.R.", immediate. = TRUE)
    as.character(sup_raw[[cols$ind_label]])
  }
  
  sup <- data.table::data.table(
    year          = as.integer(sup_raw$REF_DATE),
    industry      = trimws(as.character(sup_raw[[cols$ind_label]])),
    industry_raw  = ind_raw_src,
    prod_lab      = parsed$bare_label,
    sut_item_code = data.table::fifelse(!is.na(parsed$ioic_code), parsed$ioic_code, code_col),
    value         = as.numeric(sup_raw$VALUE))
  sup <- sup[year %in% use_years]
  
  if (anyNA(sup$sut_item_code) || any(sup$sut_item_code == "", na.rm = TRUE)) {
    lab2code <- unique(data.table::data.table(prod_lab = trimws(conc_products_labels),
                                              lab_code = conc_products_codes))
    amb <- lab2code[, .N, by = prod_lab][N > 1L, prod_lab]
    if (length(amb) > 0L) {
      warning("Label fallback skipped for ambiguous label(s) mapped to multiple ",
              "concordance codes: ", paste(amb, collapse = " | "),
              ".  Rows for these can only be keyed via their IOIC code.")
      lab2code <- lab2code[!prod_lab %in% amb]
    }
    sup[lab2code, on = "prod_lab",
        sut_item_code := data.table::fifelse(is.na(sut_item_code) | sut_item_code == "",
                                             i.lab_code, sut_item_code)]
  }
  n_dropped <- sup[is.na(sut_item_code) | sut_item_code == "", .N]
  if (n_dropped > 0L)
    warning(sprintf(
      paste0("prep_supply_numerators: dropping %d supply row(s) with no recoverable ",
             "product code (member(s): %s)."),
      n_dropped,
      paste(unique(sup[is.na(sut_item_code) | sut_item_code == "", prod_lab]),
            collapse = " | ")))
  sup <- sup[!is.na(sut_item_code) & sut_item_code != "" & is.finite(value)]
  sup[, .(industry, industry_raw, sut_item_code, year, sup_val = value)]
}



# ==============================================================================
# 11. EXTERNAL VA-BY-ACTIVITY LOADERS  (OECD SUT T1600 / Eurostat NAMA)
# ==============================================================================
#
# Output-schema column names.
BASE_TOTAL_COL <- "value_added [USD]"
STRAND_TO_COL  <- c(
  wages   = "value_added_wages [USD]",
  capital = "value_added_capital [USD]",
  tls     = "value_added_tls [USD]"
)

# FSDN gross-value-added total column (native EUR).
FSDN_TOTAL_COL <- "gross_value_added [EUR]"

# ── Value-added OUTPUT file-name convention (shared writer/reader contract) ───
# The scheme  FABIOv2_<TAG>_value_added[_<SUFFIX>]  is produced by every base/
# source writer (14_1 GLORIA/EXIOBASE, 14_2 FSDN, 14_3 Canada/Brazil SUT) and
# parsed back by the synthesis reader (14_4).
va_va_output_basename <- function(tag, suffix = NULL) {
  if (is.null(suffix) || !nzchar(suffix))
    sprintf("FABIOv2_%s_value_added", tag)
  else
    sprintf("FABIOv2_%s_value_added_%s", tag, suffix)
}

# Matching basename for a per-writer value-added DIAGNOSTIC csv, e.g.
# kind = "va_reconciliation" / "phys_intensity_winsor" / "phys_intensity_hampel".
va_va_diag_basename <- function(tag, kind, suffix)
  sprintf("FABIOv2_%s_%s_%s", tag, kind, suffix)

# COMBINED (post-synthesis) value-added output, written by 14_4 and read back by
# 14_5.  `base_tag` carries its OWN trailing underscore (e.g. "GLORIA_",
# "EXIOBASE_"); `level` is the ISIC level ("A" / "C").
va_combined_output_basename <- function(base_tag, level)
  sprintf("FABIOv2_COMBINED_%svalue_added_ISIC-%s", base_tag, level)

# --- OECD SUT (table T1600) dimensioning -------------------------------------
# OECD SUT activity (2-digit ISIC Rev 4).  A03 = Fishing and aquaculture.  If a
# given country only reports a coarser aggregate (e.g. "A"), it simply yields no
# A03 row and that country keeps the base — see the "no_oecd_sut_cell" bucket.
OECD_SUT_FISHING_ACTIVITY <- "A03"

# Dimension filters isolating the value-added-by-activity block (table T1600,
# PRODUCT "_T", current prices, total economy, valuation not-applicable,
# national currency).  Adjust here if your download uses different codes.
OECD_SUT_FILTERS <- list(
  TABLE_IDENTIFIER = "T1600",
  PRODUCT          = "_T",
  PRICE_BASE       = "V",          # current prices
  SECTOR           = "S1",         # total economy (avoid sub-sector double count)
  VALUATION        = "_Z",         # not applicable (VA is valuation-neutral)
  UNIT_MEASURE     = "XDC"         # national currency (converted via SLC below)
)

# Transaction codes for the four VA strands.  Capital falls back to B2G + B3G
# (a clean sum) when the combined B2A3G is absent; remaining gaps use the GVA
# identity (see header).  D29/D39 are deliberately NOT split here to avoid the
# subsidy-sign ambiguity — D29X39 or the identity supplies tls.
OECD_SUT_TX <- c(total = "B1G", wages = "D1", capital = "B2A3G",
                 capital_os = "B2G", capital_mi = "B3G", tls = "D29X39")

# --- Eurostat NAMA (nama_10_a64) dimensioning --------------------------------
EU_NAMA_TABLE <- "nama_10_a64";    EU_NAMA_UNIT <- "CP_MEUR"
EU_A03        <- "A03"
EU_TOTAL <- "B1G"; EU_LAB <- "D1"; EU_TLS <- "D29X39"

# Eurostat ISO2 geo -> ISO3 (matches the base's iso3c).  EL=Greece, UK=United
# Kingdom are Eurostat's two specials; EFTA/candidate codes included for OECD
# overlap.
EU_ISO2_TO_ISO3 <- c(
  AT="AUT", BE="BEL", BG="BGR", HR="HRV", CY="CYP", CZ="CZE", DK="DNK",
  EE="EST", FI="FIN", FR="FRA", DE="DEU", EL="GRC", GR="GRC", HU="HUN",
  IE="IRL", IT="ITA", LV="LVA", LT="LTU", LU="LUX", MT="MLT", NL="NLD",
  PL="POL", PT="PRT", RO="ROU", SK="SVK", SI="SVN", ES="ESP", SE="SWE",
  UK="GBR", GB="GBR", NO="NOR", IS="ISL", CH="CHE", LI="LIE", TR="TUR")


# ── Helper: OECD SUT activity value added -> USD overlay ─────────────────────
#
# Reads the OECD SUT staged by 00_9, isolates the requested activity's VA-by-activity
# block, builds the three strands in national currency (recovering any single
# gap from the GVA identity), maps ISO3 -> fabio_area_code (crosswalk from the
# combined base), converts to USD via the per-country FAOSTAT rate, and returns
# a table keyed (iso3, fabio_area_code, year) with the three strands + total in
# USD.  Base-independent (it never reads the combined values, only the iso3
# crosswalk, which is identical across bases).  Returns NULL if nothing usable.
#
# Generalized to ANY 2-digit ISIC activity (`activity`, e.g. "A03" fishing or
# "A02" forestry).  It does NOT stamp a FABIO item code — callers add one (the
# fishing wrapper below) or consume it item-free (the forestry reference).

load_oecd_sut_activity <- function(path = VA_OECD_SUT_CSV,
                                   iso3_to_area, lcu_usd,
                                   activity = OECD_SUT_FISHING_ACTIVITY) {
  if (!file.exists(path)) {      # staged by R/00_9_prep_value_added.R
    warning("OECD SUT not staged (", path, ") \u2014 run R/00_9_prep_value_added.R; ",
            "OECD overlay skipped.")
    return(NULL)
  }
  s <- as.data.table(fread(path))
  
  need <- c("REF_AREA", "ACTIVITY", "TRANSACTION", "PRODUCT", "PRICE_BASE",
            "SECTOR", "VALUATION", "UNIT_MEASURE", "TABLE_IDENTIFIER",
            "TIME_PERIOD", "OBS_VALUE", "UNIT_MULT")
  miss <- setdiff(need, names(s))
  if (length(miss) > 0L)
    stop("OECD SUT CSV is missing column(s): ", paste(miss, collapse = ", "),
         ".\nIs this the format=csvfilewithlabels export staged by R/00_9_prep_value_added.R?")
  
  # Core dimension filters (everything in OECD_SUT_FILTERS, plus the activity).
  s <- s[TABLE_IDENTIFIER == OECD_SUT_FILTERS$TABLE_IDENTIFIER &
           PRODUCT      == OECD_SUT_FILTERS$PRODUCT       &
           PRICE_BASE   == OECD_SUT_FILTERS$PRICE_BASE    &
           SECTOR       == OECD_SUT_FILTERS$SECTOR        &
           VALUATION    == OECD_SUT_FILTERS$VALUATION     &
           UNIT_MEASURE == OECD_SUT_FILTERS$UNIT_MEASURE  &
           ACTIVITY     == activity]
  if (nrow(s) == 0L) {
    warning("No OECD SUT rows for activity '", activity,
            "' after filtering — it may not be separately reported, or a filter ",
            "code (", paste(names(OECD_SUT_FILTERS), collapse = "/"),
            ") differs from your download.  Overlay for this activity skipped.")
    return(NULL)
  }
  
  # National-currency absolute value (UNIT_MULT is a power of ten; millions = 6).
  s[, `:=`(value_lcu = suppressWarnings(as.numeric(OBS_VALUE)) *
             10^suppressWarnings(as.integer(UNIT_MULT)),
           year = as.integer(TIME_PERIOD),
           iso3 = trimws(as.character(REF_AREA)))]
  s <- s[is.finite(value_lcu)]
  
  # One value per (iso3, year, TRANSACTION); warn if any filter left duplicates.
  tx_all <- unname(OECD_SUT_TX)
  dup <- s[TRANSACTION %in% tx_all, .N, by = .(iso3, year, TRANSACTION)][N > 1L]
  if (nrow(dup) > 0L)
    warning(sprintf("%d (area, year, transaction) cell(s) had >1 OECD SUT row after ",
                    nrow(dup)),
            "filtering and were summed — check the OECD_SUT_FILTERS dimensions.")
  s <- s[TRANSACTION %in% tx_all,
         .(value_lcu = sum(value_lcu, na.rm = TRUE)),
         by = .(iso3, year, TRANSACTION)]
  w <- dcast(s, iso3 + year ~ TRANSACTION, value.var = "value_lcu")
  
  gettx <- function(dt, code) {
    if (code %in% names(dt)) suppressWarnings(as.numeric(dt[[code]]))
    else rep(NA_real_, nrow(dt))
  }
  
  capB2A3G <- gettx(w, OECD_SUT_TX[["capital"]])
  capB2G   <- gettx(w, OECD_SUT_TX[["capital_os"]])
  capB3G   <- gettx(w, OECD_SUT_TX[["capital_mi"]])
  w[, `:=`(
    wages_lcu   = gettx(w, OECD_SUT_TX[["wages"]]),
    tls_lcu     = gettx(w, OECD_SUT_TX[["tls"]]),
    total_lcu   = gettx(w, OECD_SUT_TX[["total"]]),
    capital_lcu = fcase(is.finite(capB2A3G),                      capB2A3G,
                        is.finite(capB2G) & is.finite(capB3G),    capB2G + capB3G,
                        default = NA_real_))]
  
  # Recover one missing strand from the identity B1G = D1 + B2A3G + D29X39.
  w[!is.finite(capital_lcu) & is.finite(total_lcu) & is.finite(wages_lcu)   & is.finite(tls_lcu),
    capital_lcu := total_lcu - wages_lcu - tls_lcu]
  w[!is.finite(tls_lcu)     & is.finite(total_lcu) & is.finite(wages_lcu)   & is.finite(capital_lcu),
    tls_lcu     := total_lcu - wages_lcu - capital_lcu]
  w[!is.finite(wages_lcu)   & is.finite(total_lcu) & is.finite(capital_lcu) & is.finite(tls_lcu),
    wages_lcu   := total_lcu - capital_lcu - tls_lcu]
  w[!is.finite(total_lcu)   & is.finite(wages_lcu) & is.finite(capital_lcu) & is.finite(tls_lcu),
    total_lcu   := wages_lcu + capital_lcu + tls_lcu]
  
  # Identity residual where all four were published (sanity, not enforced).
  full <- w[is.finite(wages_lcu) & is.finite(capital_lcu) &
              is.finite(tls_lcu) & is.finite(total_lcu)]
  if (nrow(full) > 0L) {
    rr <- full[, abs(total_lcu - (wages_lcu + capital_lcu + tls_lcu)) /
                 pmax(abs(total_lcu), 1)]
    message(sprintf("  OECD SUT GVA identity (%s): max |residual| = %.2e (rel) over %d cell(s).",
                    activity, max(rr, na.rm = TRUE), nrow(full)))
  }
  
  n_pre <- nrow(w)
  w <- w[is.finite(wages_lcu) & is.finite(capital_lcu) &
           is.finite(tls_lcu) & is.finite(total_lcu)]
  if (nrow(w) < n_pre)
    message(sprintf("  %d OECD SUT %s cell(s) dropped for incomplete VA components (kept as base).",
                    n_pre - nrow(w), activity))
  
  # ISO3 -> fabio_area_code (inner join drops OECD aggregates not in the base).
  w <- merge(w, iso3_to_area, by = "iso3")
  if (nrow(w) == 0L) {
    warning("No OECD SUT ", activity, " ISO3 matched the combined base's iso3c — ",
            "overwrite skipped.")
    return(NULL)
  }
  
  # National currency -> USD (USD = national / rate).
  w <- merge(w, lcu_usd, by = c("fabio_area_code", "year"), all.x = TRUE)
  no_rate <- sort(unique(w[!(is.finite(rate_lcu_per_usd) & rate_lcu_per_usd > 0),
                           fabio_area_code]))
  if (length(no_rate) > 0L)
    warning("No FAOSTAT ", VA_FX_ELEMENT_CODE, " rate for fabio_area_code(s): ",
            paste(no_rate, collapse = ", "),
            " — those OECD SUT ", activity, " cells dropped.")
  w <- w[is.finite(rate_lcu_per_usd) & rate_lcu_per_usd > 0]
  if (nrow(w) == 0L) return(NULL)
  
  w[, (STRAND_TO_COL[["wages"]])   := wages_lcu   / rate_lcu_per_usd]
  w[, (STRAND_TO_COL[["capital"]]) := capital_lcu / rate_lcu_per_usd]
  w[, (STRAND_TO_COL[["tls"]])     := tls_lcu     / rate_lcu_per_usd]
  w[, (BASE_TOTAL_COL) := get(STRAND_TO_COL[["wages"]]) +
      get(STRAND_TO_COL[["capital"]]) + get(STRAND_TO_COL[["tls"]])]
  w[, `:=`(year            = as.integer(year),
           fabio_area_code = as.integer(fabio_area_code))]
  
  # Keyed (iso3, fabio_area_code, year) — NO item code stamped here.  `iso3` is
  # retained so item-free consumers (the forestry reference) can key on iso3c.
  keep <- c("iso3", "fabio_area_code", "year", unname(STRAND_TO_COL), BASE_TOTAL_COL)
  ov   <- w[, ..keep]
  setkeyv(ov, c("fabio_area_code", "year"))
  message(sprintf("OECD SUT %s overlay: %d (area, year) cell(s); Sum = %.3e USD.",
                  activity, nrow(ov), ov[, sum(get(BASE_TOTAL_COL), na.rm = TRUE)]))
  ov[]
}


# ── Helper: fetch the Eurostat NAMA source ONCE, in USD ──────────────────────
#
# Returns a data.table keyed (iso3c, year) with the three strands + total in
# USD (wages<-D1, capital<-B1G-D1-D29X39 via the identity, tls<-D29X39,
# total<-B1G).  Shared by BOTH the fallback fill and the fishing diagnostic so
# Eurostat is hit only once per nace.  Returns NULL (with a warning) if
# `eurostat` is missing or the fetch fails — callers then skip gracefully.
#
# Generalized to ANY NACE Rev 2 division (`nace`, e.g. "A03" fishing or "A02"
# forestry).  Return shape: (iso3c, year, wages_usd, capital_usd, tls_usd,
# total_usd) in USD.

load_eurostat_nama_activity <- function(eur_usd, nace = EU_A03,
                                        path = VA_EUROSTAT_NAMA_CSV) {
  if (!file.exists(path)) {
    warning("Eurostat NAMA not staged (", path, ") — no fallback ",
            "fill or diagnostic column.  Run R/00_9_prep_value_added.R to stage it.")
    return(NULL)
  }
  message(sprintf("\nReading staged Eurostat NAMA source (%s, nace %s) ...",
                  EU_NAMA_TABLE, nace))
  
  nama <- tryCatch(as.data.table(fread(path)),
                   error = function(e) {
                     warning("Reading staged Eurostat NAMA failed (", path, "): ",
                             conditionMessage(e)); NULL })
  if (is.null(nama)) {
    message("  No Eurostat NAMA data read (nace ", nace, ").")
    return(NULL)
  }
  
  # NAMA has no B2A3G capital code -> capital comes from B1G - D1 - D29X39.
  if (!"TIME_PERIOD" %in% names(nama) && "time" %in% names(nama))
    setnames(nama, "time", "TIME_PERIOD")
  nama <- nama[na_item %in% c(EU_TOTAL, EU_LAB, EU_TLS) &
                 nace_r2 == nace & unit == EU_NAMA_UNIT & !is.na(values)]
  nama[, `:=`(iso3c = unname(EU_ISO2_TO_ISO3[toupper(trimws(geo))]),
              year  = as.integer(TIME_PERIOD))]
  nama <- nama[!is.na(iso3c)]
  if (nrow(nama) == 0L) {
    message("  No Eurostat NAMA data retrieved (nace ", nace, ").")
    return(NULL)
  }
  
  a <- nama[, .(meur = sum(values, na.rm = TRUE)), by = .(iso3c, year, na_item)]
  a <- merge(a, eur_usd, by = "year", all.x = TRUE)        # EUR millions -> USD
  a[, usd := fifelse(is.finite(rate_eur_per_usd) & rate_eur_per_usd > 0,
                     meur * 1e6 / rate_eur_per_usd, NA_real_)]
  w <- dcast(a, iso3c + year ~ na_item, value.var = "usd")
  g <- function(col) if (col %in% names(w)) w[[col]] else rep(NA_real_, nrow(w))
  tot <- g(EU_TOTAL); lab <- g(EU_LAB); tls <- g(EU_TLS)
  data.table(iso3c = w$iso3c, year = w$year,
             wages_usd = lab, capital_usd = tot - lab - tls,
             tls_usd   = tls, total_usd   = tot)
}

# ==============================================================================
# End of 00_value_added_helpers.R
# ==============================================================================