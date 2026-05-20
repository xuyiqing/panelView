## Project a PanelMatch matched-set object onto a logical T x N sample
## mask that panelview() can render via `sample=`. Reads only the public
## slots / attributes of a `PanelMatch` object --- no PanelMatch package
## modification required.

#' Convert a PanelMatch object to a panelView sample mask
#'
#' Walks a `PanelMatch` object's matched sets and returns a logical
#' T x N matrix marking cells used in estimation, suitable for passing
#' as `sample=` to \code{\link{panelview}}. For every treated event,
#' the treated unit and each matched control (positive weight) are
#' marked TRUE over `[t-lag, t+max(lead)]` --- the lag window covers
#' the matching support and the lead window covers the outcome leads
#' PanelMatch uses on the LHS, so within each event the light-blue
#' pre-period and dark-blue post-onset cells render as one contiguous
#' band.
#'
#' @param pm A `PanelMatch` object (from CRAN package PanelMatch).
#' @param pd The `PanelData` object passed to `PanelMatch()`, or any
#'   data.frame with the same `unit.id` and `time.id` columns. Used
#'   only to define the panel axes (sorted unique units / times).
#' @param qoi One of "att", "atc". If NULL and `pm` has a single
#'   matched-set list, that one is used; if both are present "att"
#'   is preferred.
#' @param include Character vector. Any of "treated" (the treated unit
#'   for each event) and "controls" (matched controls). Default both.
#' @param weight.threshold Numeric. Controls with weight strictly
#'   greater than this are marked. Default 0 keeps any positive weight.
#'
#' @return A logical matrix with rows = sorted unique times (rownames
#'   are character), columns = sorted unique units (colnames are
#'   character). TRUE = cell appears in at least one matched set.
#'
#' @examples
#' \dontrun{
#' library(PanelMatch)
#' data(dem)
#' pd <- PanelData(panel.data = dem, unit.id = "wbcode2",
#'                 time.id = "year", treatment = "dem", outcome = "y")
#' pm <- PanelMatch(panel.data = pd, lag = 4,
#'                  refinement.method = "mahalanobis",
#'                  covs.formula = ~ I(lag(tradewb, 1:4)),
#'                  qoi = "att", lead = 0:3, match.missing = TRUE)
#' mask <- panelmatch_to_sample(pm, pd)
#' panelview(pd, formula = y ~ dem, index = c("wbcode2", "year"),
#'           type = "treat", sample = mask)
#' ## or just:
#' panelview(pd, formula = y ~ dem, index = c("wbcode2", "year"),
#'           type = "treat", sample = pm)
#' }
#' @export
panelmatch_to_sample <- function(pm, pd,
                                  qoi = NULL,
                                  include = c("treated", "controls"),
                                  weight.threshold = 0) {

    if (!inherits(pm, "PanelMatch")) {
        stop("`pm` must be a PanelMatch object.", call. = FALSE)
    }
    include <- match.arg(include, c("treated", "controls"),
                        several.ok = TRUE)

    ## Resolve unit.id / time.id from PanelMatch attributes first,
    ## then PanelData attributes as fallback.
    a_pm <- attributes(pm)
    a_pd <- attributes(pd)
    unit_id <- a_pm$unit.id %||% a_pd$unit.id
    time_id <- a_pm$time.id %||% a_pd$time.id
    if (is.null(unit_id) || is.null(time_id)) {
        stop("Cannot resolve `unit.id` / `time.id` from `pm` or `pd` ",
             "attributes.", call. = FALSE)
    }
    if (!unit_id %in% names(pd) || !time_id %in% names(pd)) {
        stop("`pd` is missing column `", unit_id, "` or `", time_id,
             "`.", call. = FALSE)
    }

    lag.k <- a_pm$lag %||% 0L
    leads <- a_pm$lead %||% 0L
    lead.max <- max(leads, na.rm = TRUE)

    ## Pick the matched-set list.
    ms_names <- names(pm)
    if (is.null(qoi)) {
        if (length(ms_names) == 1L) {
            qoi <- ms_names[1L]
        } else if ("att" %in% ms_names) {
            qoi <- "att"
        } else {
            stop("`pm` carries multiple matched-set lists; specify ",
                 "qoi = 'att' or 'atc'.", call. = FALSE)
        }
    }
    ms <- pm[[qoi]]
    if (is.null(ms)) {
        stop("`pm` does not carry qoi = '", qoi, "'.", call. = FALSE)
    }

    ## Panel axes --- sort to match panelView's internal ordering.
    units <- sort(unique(pd[[unit_id]]))
    times <- sort(unique(pd[[time_id]]))

    ## Mirror panelView's pre-plot drop: a unit whose outcome or
    ## treatment column is entirely NA never reaches the plot, so its
    ## column in the mask is stale and the strict-alignment guard
    ## would fire. Drop those units up front so the returned mask
    ## aligns with whatever panelview() will render.
    outcome   <- a_pm$outcome   %||% a_pd$outcome
    treatment <- a_pm$treatment %||% a_pd$treatment
    drop_cols <- function(col) {
        if (is.null(col) || !col %in% names(pd)) return(character(0))
        ok <- tapply(!is.na(pd[[col]]), pd[[unit_id]], any)
        as.character(names(ok)[!ok])
    }
    dropped <- unique(c(drop_cols(outcome), drop_cols(treatment)))
    if (length(dropped) > 0L) {
        units <- units[!as.character(units) %in% dropped]
    }

    TT <- length(times); N <- length(units)
    out <- matrix(FALSE, nrow = TT, ncol = N,
                  dimnames = list(as.character(times),
                                  as.character(units)))
    if (TT == 0L || N == 0L) return(out)

    row_idx <- stats::setNames(seq_len(TT), as.character(times))
    col_idx <- stats::setNames(seq_len(N),  as.character(units))
    times_num <- suppressWarnings(as.numeric(names(row_idx)))
    times_numeric <- !anyNA(times_num)

    set_keys <- names(ms)
    if (length(set_keys) == 0L) return(out)

    for (k in seq_along(ms)) {
        key <- set_keys[k]
        ## "<treated_unit>.<treated_time>" --- split on the LAST dot
        ## so unit ids that contain a dot still parse correctly.
        dot <- regexpr("\\.[^.]*$", key)
        if (dot < 1L) next
        treated_unit <- substr(key, 1L, dot - 1L)
        treated_time <- as.numeric(substr(key, dot + 1L, nchar(key)))
        if (!is.finite(treated_time)) next

        ## Time window for this event: matching support + outcome
        ## leads. Sequence in the original time scale; subset to
        ## times actually present.
        t_lo <- treated_time - lag.k
        t_hi <- treated_time + lead.max
        if (times_numeric) {
            t_seq <- seq(t_lo, t_hi)
            t_keys <- as.character(t_seq[t_seq %in% times_num])
        } else {
            ## non-numeric times: fall back to all times in the panel
            ## whose character form matches the integer sequence.
            t_keys <- as.character(seq(t_lo, t_hi))
            t_keys <- t_keys[t_keys %in% names(row_idx)]
        }
        if (length(t_keys) == 0L) next
        rows <- row_idx[t_keys]

        ## Units in this set: treated + (optionally) positive-weight controls.
        keep_units <- character(0)
        if ("treated" %in% include) {
            keep_units <- c(keep_units, treated_unit)
        }
        if ("controls" %in% include) {
            ctrls <- as.character(ms[[k]])
            w <- attr(ms[[k]], "weights")
            if (!is.null(w)) {
                wnm <- names(w)
                if (!is.null(wnm) && length(wnm) == length(ctrls)) {
                    ctrls <- wnm[w > weight.threshold]
                } else {
                    ctrls <- ctrls[w > weight.threshold]
                }
            }
            keep_units <- c(keep_units, ctrls)
        }
        keep_units <- unique(keep_units)
        keep_units <- keep_units[keep_units %in% names(col_idx)]
        if (length(keep_units) == 0L) next
        cols <- col_idx[keep_units]

        out[rows, cols] <- TRUE
    }

    out
}

`%||%` <- function(x, y) if (is.null(x)) y else x
