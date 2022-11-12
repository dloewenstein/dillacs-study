#' Systolic Stretch Index (SSI)
#'
#' Measuring systolic stretch of both the septum and the lateral wall
#' @param data data.frame of strain values, with n(rows) == n(timeframes) and
#' n(cols) == n(segments)
#' @param time numeric, time index
#' @param time_avo numeric, time of aortic valve opening
#' @param time_avc numeric, time of aortic valve closure
#' @param strain_type character, one of "circumferential", "radial", or
#' "longitudinal" indicating which type of strain is being analyzed
#'
#' @return numeric, Systolic Stretch Index
#' @export
#' @details
#' Made up of Systolic Rebound Stretch of the septum and Systolic Pre-Stretch
#' of the lateral wall.
#'
#' Systolic Rebound Stretch:
#' Systolic stretch following prematurely terminated shortening,
#' measured until aortic valve closure.
#'
#' Systolic Pre-Stretch:
#' Early streching not preceeded by shortening measured until
#' aortic valve opening.
#'
#' Systolic period:
#' Ends at Aortic Valve Closure (AVC)
#'
#' Definitions:
#' Peak shortening is timed and designated as premature if, within
#' the systolic period, any shortening amplitude exceeds the end-systolic
#' value; inversely, peak shortening is considered delayed if the
#' end-systolic value is the highest systolic value and shortening
#' continues beyond aortic valve closure.
#'
#' @examples
strain_ssi <- function(data, time, time_avo, time_avc,
                       septal_segments, lateral_segments,
                       strain_type = c("circumferential", "longitudinal", "radial"),
                       ...) {

    if(
        any(
            is.na(time_avo),
            is.na(time_avc),
            is.null(time_avo),
            is.null(time_avc)
            )
        ) {
            return(NA)
    }


    strain_type <- match.arg(strain_type, choices = c("circumferential", "longitudinal", "radial"))

    SRS_sept <- apply(data[, septal_segments], 2, .calc_rebound_stretch, time, time_avo, time_avc, strain_type)
    SPS_lat  <- apply(data[, lateral_segments], 2, .calc_pre_stretch, time, time_avo, strain_type)

    SRS_sept <- mean(SRS_sept)
    SPS_lat  <- mean(SPS_lat)

    SSI      <- SRS_sept + SPS_lat

}

.calc_rebound_stretch <- function(strain, time, time_avo, time_avc, strain_type, ...) {
    # time_avc -1 since we want before aortic valve closure, not at.
    systolic_strain <- strain[1:(time_avc - 1)]


    # check for preejection shortening
    preejection <- systolic_strain[1:time_avo]

    if (strain_type == "radial")
        preejection_shortening <- preejection > 0
    else
        preejection_shortening <- preejection < 0

    if ( !any(preejection_shortening) ) return(0)

    # positive diff for circ&long means stretch
    # negative diff for rad means stretch

    # identify first valley(circ/long) first peak(radial)
    # sometimes theres stretch immediately after time 0, we don't want to
    # include this in the calculations

    peak <- switch(strain_type,
                   "radial" = (find_peaks(systolic_strain)[1] - 1),
                   "circumferential" = (find_valleys(systolic_strain)[1] - 1),
                   "longitudinal"    = (find_valleys(systolic_strain)[1] - 1)
                   )

    if ( !length(peak) || is.na(peak) ) return(0)

    diff_strain <- diff(systolic_strain[ peak:length(systolic_strain) ])

    # sum stretch
    if (strain_type == "radial")
        # keep only stretch values and invert for correct summation
        stretch <- sum( diff_strain[diff_strain < 0] * -1 )
    else
        stretch <- sum( diff_strain[diff_strain > 0] )
}

.calc_pre_stretch <- function(strain, time, time_avo, strain_type, ...) {

    # time_avo -1 since we want before aortic valve openeing, not at.
    early_strain <- strain[ 1:time_avo ]

    diff_strain <- diff( early_strain )

    # sum stretch
    if (strain_type == "radial")
        # keep only stretch values and invert for correct summation
        stretch <- sum( diff_strain[diff_strain < 0] * -1 )
    else
        stretch <- sum( diff_strain[diff_strain > 0] )
}

find_peaks <- function(x, thresh = 0) {
    pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) <
                     0) + 2
    if (!missing(thresh)) {
        if (sign(thresh) < 0)
            thresh <- -thresh
        pks[x[pks - 1] - zoo::coredata(x[pks]) > thresh]
    }
    else pks
}

find_valleys <- function(x, thresh = 0) {
    pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) >
                     0) + 2
    if (!missing(thresh)) {
        if (sign(thresh) > 0)
            thresh <- -thresh
        pks[x[pks - 1] - zoo::coredata(x[pks]) < thresh]
    }
    else pks
}
