# Reconstruct the (data, formula, index) triple that panelview()
# needs from a fitted panel-estimator object. Supported classes:
#
#   fect, gsynth, tjbal --- carry $Y.dat, $D.dat, $index, $Y, $D
#                           (T x N matrices keyed on (time, unit))
#   cip                 --- carries $Y_mat, $D_mat (N x T), $unit_levels,
#                           $time_levels, $index_names (or implicit)
#
# Returned list always has:
#   data    : long-format data.frame with three columns matching the
#             original (unit, time) names plus Y, D
#   formula : Y ~ D
#   index   : c(unit_name, time_name)
.pv_from_fit <- function(fit, envir = parent.frame()) {

    if (inherits(fit, "fect") ||
        inherits(fit, "gsynth") ||
        inherits(fit, "tjbal") ||
        inherits(fit, "cip")) {

        ## Preferred path 1: the fit object carries the original input
        ## long-format data ($data.long). Use it as-is --- this preserves
        ## the full pre-drop panel including units the estimator removed,
        ## which then render as "Not used" cells under the mask.
        if (!is.null(fit$data.long) && is.data.frame(fit$data.long) &&
            !is.null(fit$index) && length(fit$index) >= 2L &&
            !is.null(fit$Y) && !is.null(fit$D)) {
            unit_name <- fit$index[1L]
            time_name <- fit$index[2L]
            Y_name    <- fit$Y
            D_name    <- fit$D
            return(list(
                data    = fit$data.long,
                formula = stats::as.formula(paste0(Y_name, " ~ ", D_name)),
                index   = c(unit_name, time_name),
                sample  = fit$sample
            ))
        }

        ## Preferred path 2: evaluate fit$call$data in the user's
        ## calling environment to recover the original data.frame. This
        ## restores the FULL pre-drop panel --- units the estimator
        ## dropped (always-treated, insufficient pre-period, etc.) show
        ## up with their original (unit, time, Y, D) values and get
        ## flagged "Not used" by fit$sample. Falls through silently if
        ## the data symbol is not in scope or doesn't yield a usable
        ## data.frame with the expected columns.
        if (!is.null(fit$call) && !is.null(fit$call$data) &&
            !is.null(fit$index) && length(fit$index) >= 2L &&
            !is.null(fit$Y) && !is.null(fit$D)) {
            unit_name <- fit$index[1L]
            time_name <- fit$index[2L]
            Y_name    <- fit$Y
            D_name    <- fit$D
            orig <- tryCatch(eval(fit$call$data, envir = envir),
                             error = function(e) NULL)
            if (is.data.frame(orig) &&
                all(c(unit_name, time_name, Y_name, D_name) %in%
                    names(orig))) {
                return(list(
                    data    = as.data.frame(orig),
                    formula = stats::as.formula(paste0(Y_name, " ~ ",
                                                        D_name)),
                    index   = c(unit_name, time_name),
                    sample  = fit$sample
                ))
            }
        }

        ## Fallback: reconstruct from the post-drop matrices. The mask
        ## is subset to the kept-unit set so the strict alignment guard
        ## passes; dropped units do not appear in the figure.
        Y_mat <- fit$Y.dat
        D_mat <- fit$D.dat
        if (is.null(Y_mat) || is.null(D_mat)) {
            stop("Fitted object does not carry $Y.dat / $D.dat; ",
                 "panelview cannot reconstruct the panel automatically. ",
                 "Pass `data`, `formula`, `index` explicitly.",
                 call. = FALSE)
        }
        TT    <- nrow(Y_mat)
        N     <- ncol(Y_mat)
        ## Y.dat / D.dat may carry no dimnames (fect's case); fall back to
        ## $id (length N) and $rawtime (length T) which fect always sets.
        units <- colnames(Y_mat)
        if (is.null(units) || length(units) != N) units <- fit$id
        if (is.null(units) || length(units) != N) units <- seq_len(N)

        times_raw <- rownames(Y_mat)
        if (is.null(times_raw) || length(times_raw) != TT)
            times_raw <- fit$rawtime
        if (is.null(times_raw) || length(times_raw) != TT)
            times_raw <- seq_len(TT)
        times_num <- suppressWarnings(as.numeric(times_raw))
        times <- if (!anyNA(times_num)) times_num else times_raw

        unit_name <- if (!is.null(fit$index) && length(fit$index) >= 1L)
            fit$index[1L] else "unit"
        time_name <- if (!is.null(fit$index) && length(fit$index) >= 2L)
            fit$index[2L] else "time"
        Y_name <- if (!is.null(fit$Y)) fit$Y else "Y"
        D_name <- if (!is.null(fit$D)) fit$D else "D"

        long <- data.frame(
            ..unit = rep(units, each = TT),
            ..time = rep(times, times = N),
            ..Y    = as.vector(Y_mat),
            ..D    = as.vector(D_mat),
            stringsAsFactors = FALSE
        )
        ## restore original column names
        names(long) <- c(unit_name, time_name, Y_name, D_name)

        ## Restore the original missing-cell pattern. fect (and
        ## relatives) fill in `Y.dat` / `D.dat` after na.rm = TRUE, so
        ## cells that were NA in the source data appear as observed
        ## values in the reconstructed long-form data above. Without
        ## intervention, panelview() would shade those cells in the
        ## active palette (and the sample-mask overlay would relabel
        ## them as "Not used") instead of keeping them as Missing.
        ## fect's `$I` matrix is the observed indicator (1 = observed,
        ## 0 = missing); aligned to `$Y.dat` columns. Use it to set
        ## Y / D back to NA in the reconstructed data.
        I_mat <- fit$I
        if (!is.null(I_mat) && is.matrix(I_mat) &&
            identical(dim(I_mat), dim(Y_mat))) {
            miss <- as.vector(I_mat) == 0
            long[[Y_name]][miss] <- NA
            long[[D_name]][miss] <- NA
        }

        ## panelview's leave.gap = TRUE path drops units that have at
        ## least one all-NA column. Pre-drop those here so the
        ## reconstructed long-form data and the returned $sample have
        ## matching unit sets (otherwise the strict alignment guard
        ## fires after panelview's drop).
        all_na_units <- character(0)
        for (u in unique(long[[unit_name]])) {
            yi <- long[long[[unit_name]] == u, Y_name]
            di <- long[long[[unit_name]] == u, D_name]
            if (anyNA(yi) || anyNA(di)) {
                ## panelview's check is per-row anyNA --- conservatively
                ## drop units with any NA in either Y or D
                if (all(is.na(yi)) || all(is.na(di))) {
                    all_na_units <- c(all_na_units, as.character(u))
                }
            }
        }
        if (length(all_na_units) > 0) {
            long <- long[!as.character(long[[unit_name]]) %in% all_na_units, ,
                         drop = FALSE]
        }

        ## fect's $sample is sized on the pre-drop panel (matches
        ## $obs.missing). The reconstructed long-form data above uses
        ## the post-drop unit set ($id / Y.dat columns) and we just
        ## dropped any further all-NA units. Subset $sample to match.
        sample_out <- fit$sample
        if (!is.null(sample_out)) {
            if (!is.null(colnames(sample_out))) {
                surviving <- setdiff(as.character(units), all_na_units)
                keep <- as.character(colnames(sample_out)) %in% surviving
                sample_out <- sample_out[, keep, drop = FALSE]
            } else if (ncol(sample_out) != N - length(all_na_units)) {
                sample_out <- NULL
            }
        }

        return(list(
            data    = long,
            formula = stats::as.formula(paste0(Y_name, " ~ ", D_name)),
            index   = c(unit_name, time_name),
            sample  = sample_out
        ))
    }

    stop("Unsupported fit class: ", paste(class(fit), collapse = "/"),
         ". Pass `data`, `formula`, `index` explicitly.", call. = FALSE)
}
