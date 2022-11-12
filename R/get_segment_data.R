
#' Get data from Segment .mat files
#'
#' Extracts image data as well as strain data from .mat files analyzed in
#' medviso Segment V.2
#'
#' @param x A \code{character} file path
#' @importFrom magrittr %>%
#' @return
#' @export
#'
get_segment_data <- function(path){

    matdata <- R.matlab::readMat(path)

    # Get subject id, and acquisition date
    subject_id <-
            tryCatch(
                matdata$info[, , 1]$Name[1],
                error = function(e) {
                    NA
                }
            )

    acq_date <-
            tryCatch(
                matdata$info[, , 1]$AcquisitionDate[1],
                error = function(e) {
                    NA
                }
            )


    #' Standardize names in segment data
    #'
    #' @param array_data \code{array} for which to set names tolower
    #'
    #' @return provided array object with dimnames in lower case
    #'
    standardize_names <- function(array_data) {
        for (dim in seq_len(length(dimnames(array_data)))) {
            standardized_dim_names <- tolower(dimnames(array_data)[[dim]])
            dimnames(array_data)[[dim]] <-
                standardized_dim_names
        }
        return(array_data)
    }

    # Lower case dimnames for subsetting later on
    matdata$setstruct <- standardize_names(matdata$setstruct)

    timeresolved_annotations <- .get_timeresolved_annotations(matdata$setstruct)

    # Some versions of Segment saved image data in matrix form in a 1d-array
    # assert which format
    # class(matdata$setstruct[,,1]) == "list" indicates array form
    # class(matdata$setstruct[,,1]) == "matrix" indicates matrix form

    if (class(matdata$setstruct[,,1]) == "list") {

        extraction_results <- .get_segment_data_array(matdata$setstruct)

    }

    if (class(matdata$setstruct[,,1]) == "matrix") {

        extraction_results <- .get_segment_data_matrix(matdata$setstruct)

    }

    extraction_results <- cbind(
        data_frame(
            image_id = subject_id,
            mri_date = acq_date,
            file_name = basename(path)
        ),
        extraction_results,
        timeresolved_annotations
    )
}

.get_timeresolved_annotations <- function(setstruct, ...) {
    annotations <- do.call(cbind, setstruct["point", , ])
    # Row 1:5, X, Y, T, Z, Label
    # n_col = n image stacks
    # Get the index of stacks containing time resolved points
    length_annotations <- vapply(annotations[3, ], length, numeric(1))
    time_resolved_annotations_logical <- length_annotations > 0
    time_frames <- do.call(c, annotations[3, time_resolved_annotations_logical])
    point_labels <- unlist(annotations[5, time_resolved_annotations_logical])

    # Find (if any) index for AVO and AVC
    avo_ind <- grep(pattern = ".*avo.*", x = point_labels, ignore.case = TRUE)
    avc_ind <- grep(pattern = ".*avc.*", x = point_labels, ignore.case = TRUE)

    # Get corresponding timeframes
    avo_tfind <- time_frames[avo_ind]
    avc_tfind <- time_frames[avc_ind]

    time_points <- data.frame(avo_tfind, avc_tfind)

    if(nrow(time_points))
        return(time_points)
    else
        return(data.frame(avo_tfind = NA, avc_tfind = NA))

}

.get_segment_data_array <- function(array_data, ...){

    # Get nr of image stacks == nr of dimensions in array
    n_stacks <- dim(array_data)[3]

    image_type_and_plane_per_stack <-
        map(seq_len(n_stacks),
            ~ array_data[, , .x][c("imagetype", "imageviewplane")]) %>%
        map_dfr(., data.frame,
                stringsAsFactors = FALSE) %>%
        mutate_all(.funs = funs(tolower(gsub("-", "", .))))

    sax_feature_tracking_stack <-
        which(
            image_type_and_plane_per_stack$imagetype == "feature tracking" &
                image_type_and_plane_per_stack$imageviewplane == "shortaxis"
        )

    try(
        if (!length(sax_feature_tracking_stack) == 1) {
            stop(
                sprintf(
                    "%s has %d feature tracking stacks, can't extract data",
                    (parent.frame())$path,
                    length(sax_feature_tracking_stack)),
                call. = TRUE
            )
        }
    )

    # Get the image data for shortaxis
    shortaxis_info <- array_data[, , sax_feature_tracking_stack]

    #' Safely extraxt `variable` from shortaxis info
    #'
    #' @param variable name of variable
    #'
    #' @return vector
    #'
    try_get_info <- function(variable) {
        list(
            tryCatch(
                shortaxis_info[[variable]][1],
                error = function(e)
                    NA
            )
        )
    }

    # Get time vector for timeaxis
    time_vector <- tryCatch(t(shortaxis_info$timevector), error = function(e) NA)

    # Collect image parameters
    shortaxis_data <- dplyr::data_frame(
        x_size          = try_get_info("xsize"),
        y_size          = try_get_info("ysize"),
        x_res           = try_get_info("resolutionx"),
        y_res           = try_get_info("resolutiony"),
        t_incr          = try_get_info("tincr"),
        t_max           = max(time_vector),
        t_delay         = try_get_info("tdelay"),
        echo_time       = try_get_info("echotime"),
        rep_time        = try_get_info("repetitiontime"),
        inv_time        = try_get_info("inversiontime"),
        flip_angle      = try_get_info("flipangle"),
        slice_thickness = try_get_info("slicethickness"),
        slice_gap       = try_get_info("slicegap"),
        scanner         = try_get_info("scanner"),
        image_pos       = try_get_info("imageposition"),
        image_orient    = try_get_info("imageorientation"),
        x_min           = try_get_info("xmin"),
        y_min           = try_get_info("ymin"),
        heart_rate      = try_get_info("heartrate"),
        edv             = try_get_info("edv"),
        ef              = try_get_info("ef"),
        program_version = try_get_info("programversion")
    )

    # Get data relating to strain analysis
    sax_strain_data <-
        array_data[, , sax_feature_tracking_stack]$straintagging[, , 1]

    # Get characters for location of slices
    saslices <- unlist(sax_strain_data$saslices)
    saslices <-
        match.arg(saslices, c("basal", "mid"), several.ok = TRUE)

    #' Extract strain data from shortaxis view
    #'
    #' @param strain_data \code{array} or \code{data.frame}
    #'
    #' @return list of data.frame or data.frame
    #'
    .extract_strain_data <- function(strain_data) {
        # Check whether strain data is in form array (indicated by dim length 3)
        # if so, split by array index and convert to dataframe
        if (length(dim(strain_data)) == 3) {
            strain_results <- plyr::alply(
                strain_data,
                .fun = as.data.frame,
                stringsAsFactors = FALSE,
                .margins = 3,
                .id = NULL
            )
        }
        if (length(dim(strain_data)) == 2) {
            # Check if dim length 2 (indicating matrix, and single slice data)
            strain_results <-
                list(
                    as.data.frame(strain_data,
                                  stringsAsFactors = FALSE))
        }
        return(strain_results)
    }


    # Extract segmental circumferential strain, all slices
    sax_segmentcirc <- tryCatch(
        .extract_strain_data(sax_strain_data$segmentcirc),
        error = function(e) {
            NA
        }
    )

    # Extract segmental totalstrain, all slices
    sax_segmenttot <- tryCatch(
        .extract_strain_data(sax_strain_data$segmenttot),
        error = function(e) {
            NA
        }
    )

    # This is the order of the segments
    segment_names <- c(
        "anterior",
        "anteroseptal",
        "inferoseptal",
        "inferior",
        "inferolateral",
        "anterolateral"
    )

    # If segmental total strain is missing, assume NULL strain data
    if (all(is.na(sax_segmenttot))) {
        return(shortaxis_data)
    }

    # Add timeaxis to strain data and set column names
    sax_segmenttot <-
        purrr::map(sax_segmenttot, cbind, time_vector) %>%
        purrr::map(setNames, c(segment_names, "time"))

    sax_segmentcirc <-
        purrr::map(sax_segmentcirc, cbind, time_vector) %>%
        purrr::map(setNames, c(segment_names, "time"))

    # Indicate slice level in data.frame name
    names(sax_segmenttot) <-
        paste("sax_segmenttot", saslices, sep = "_")

    names(sax_segmentcirc) <-
        paste("sax_segmentcirc", saslices, sep = "_")

    extracted_dyssynchrony_data <-
            tidyr::nest(
                extract_dyssynchrony(
                    array_data[, , sax_feature_tracking_stack]$straintagging
                ),
                dyssynchrony_data = everything()
            )

    # Nest and rename data
    sax_segmenttot <- purrr::map(sax_segmenttot, tidyr::nest, data = everything())
    sax_segmenttot <- purrr::map2_dfc(
        sax_segmenttot,
        names(sax_segmenttot),
        function(df, name){
            names(df) <- name
            df
        }
    )

    sax_segmentcirc <- purrr::map(sax_segmentcirc, tidyr::nest, data = everything())
    sax_segmentcirc <- purrr::map2_dfc(
        sax_segmentcirc,
        names(sax_segmentcirc),
        function(df, name){
            names(df) <- name
            df
        }
    )
    # Combine data with nested columns for strain data
    strain_data <- dplyr::bind_cols(
        shortaxis_data,
        sax_segmenttot,
        sax_segmentcirc,
        extracted_dyssynchrony_data
    )
}

.get_segment_data_matrix <- function(matrix_data, ...){

    # Get nr of image stacks == nr of dimensions in array
    n_stacks <- dim(matrix_data)[2]

    image_type_and_plane_per_stack <-
        map(seq_len(n_stacks),
            ~ matrix_data[, .x, 1][c("imagetype", "imageviewplane")]) %>%
        map_dfr(., data.frame,
                stringsAsFactors = FALSE) %>%
        mutate_all(.funs = funs(tolower(gsub("-", "", .))))

    sax_feature_tracking_stack <-
        which(
            image_type_and_plane_per_stack$imagetype == "feature tracking" &
                image_type_and_plane_per_stack$imageviewplane == "shortaxis"
        )

    try(
        if (!length(sax_feature_tracking_stack) == 1) {
            stop(
                sprintf(
                    "%s has %d feature tracking stacks, can't extract data",
                    (parent.frame())$path,
                    length(sax_feature_tracking_stack)),
                call. = TRUE
            )
        }
    )

    # Get the image data for shortaxis
    shortaxis_info <- matrix_data[, sax_feature_tracking_stack, 1]

    #' Safely extraxt `variable` from shortaxis info
    #'
    #' @param variable name of variable
    #'
    #' @return vector
    #'
    try_get_info <- function(variable) {
        list(
            tryCatch(
                shortaxis_info[[variable]][1],
                error = function(e)
                    NA
            )
        )
    }

    # Get time vector for timeaxis
    time_vector <- tryCatch(t(shortaxis_info$timevector), error = function(e) NA)

    # Collect image parameters
    shortaxis_data <- dplyr::data_frame(
        x_size          = try_get_info("xsize"),
        y_size          = try_get_info("ysize"),
        x_res           = try_get_info("resolutionx"),
        y_res           = try_get_info("resolutiony"),
        t_incr          = try_get_info("tincr"),
        t_max           = max(time_vector),
        t_delay         = try_get_info("tdelay"),
        echo_time       = try_get_info("echotime"),
        rep_time        = try_get_info("repetitiontime"),
        inv_time        = try_get_info("inversiontime"),
        flip_angle      = try_get_info("flipangle"),
        slice_thickness = try_get_info("slicethickness"),
        slice_gap       = try_get_info("slicegap"),
        scanner         = try_get_info("scanner"),
        image_pos       = try_get_info("imageposition"),
        image_orient    = try_get_info("imageorientation"),
        x_min           = try_get_info("xmin"),
        y_min           = try_get_info("ymin"),
        heart_rate      = try_get_info("heartrate"),
        edv             = try_get_info("edv"),
        ef              = try_get_info("ef"),
        program_version = try_get_info("programversion")
    )

    # Get data relating to strain analysis
    sax_strain_data <-
        matrix_data[, sax_feature_tracking_stack, 1]$straintagging[, , 1]

    # Get characters for location of slices
    saslices <- unlist(sax_strain_data$saslices)
    saslices <-
        match.arg(saslices, c("basal", "mid"), several.ok = TRUE)

    #' Extract strain data from shortaxis view
    #'
    #' @param strain_data \code{array} or \code{data.frame}
    #'
    #' @return list of data.frame or data.frame
    #'
    .extract_strain_data <- function(strain_data) {
        # Check whether strain data is in form array (indicated by dim length 3)
        # if so, split by array index and convert to dataframe
        if (length(dim(strain_data)) == 3) {
            strain_results <- plyr::alply(
                strain_data,
                .fun = as.data.frame,
                stringsAsFactors = FALSE,
                .margins = 3,
                .id = NULL
            )
        }
        if (length(dim(strain_data)) == 2) {
            # Check if dim length 2 (indicating matrix, and single slice data)
            strain_results <- list(
                as.data.frame(
                    strain_data,
                    stringsAsFactors = FALSE
                )
            )
        }
        return(strain_results)
    }


    # Extract segmental circumferential strain, all slices
    sax_segmentcirc <- tryCatch(
        .extract_strain_data(sax_strain_data$segmentcirc),
        error = function(e) {
            NA
        }
    )

    # Extract segmental totalstrain, all slices
    sax_segmenttot <- tryCatch(
        .extract_strain_data(sax_strain_data$segmenttot),
        error = function(e) {
            NA
        }
    )

    # This is the order of the segments
    segment_names <- c(
        "anterior",
        "anteroseptal",
        "inferoseptal",
        "inferior",
        "inferolateral",
        "anterolateral"
    )

    # If segmental total strain is missing, assume NULL strain data
    if (all(is.na(sax_segmenttot))) {
        return(shortaxis_data)
    }

    # Add timeaxis to strain data and set column names
    sax_segmenttot <-
        purrr::map(sax_segmenttot, cbind, time_vector) %>%
        purrr::map(setNames, c(segment_names, "time"))

    sax_segmentcirc <-
        purrr::map(sax_segmentcirc, cbind, time_vector) %>%
        purrr::map(setNames, c(segment_names, "time"))

    # Indicate slice level in data.frame name
    names(sax_segmenttot) <-
        paste("sax_segmenttot", saslices, sep = "_")

    names(sax_segmentcirc) <-
        paste("sax_segmentcirc", saslices, sep = "_")

    extracted_dyssynchrony_data <-
        tidyr::nest(
            extract_dyssynchrony(
                matrix_data[, sax_feature_tracking_stack, 1]$straintagging
            ),
            dyssynchrony_data = everything()
        )


    # Nest and rename data
    sax_segmenttot <- purrr::map(sax_segmenttot, tidyr::nest, data = everything())
    sax_segmenttot <- purrr::map2_dfc(
        sax_segmenttot,
        names(sax_segmenttot),
        function(df, name){
            names(df) <- name
            df
        }
    )

    sax_segmentcirc <- purrr::map(sax_segmentcirc, tidyr::nest, data = everything())
    sax_segmentcirc <- purrr::map2_dfc(
        sax_segmentcirc,
        names(sax_segmentcirc),
        function(df, name){
            names(df) <- name
            df
        }
    )
    # Combine data with nested columns for strain data
    strain_data <- dplyr::bind_cols(
        shortaxis_data,
        sax_segmenttot,
        sax_segmentcirc,
        extracted_dyssynchrony_data
    )
}

#' Extract dyssynchrony data from strain tagging array
#'
#' @param strain_tagging \code{array} with dyssynchrony data
#' @param ...
#'
#' @return \code{data.frame}
#' @export
#'
extract_dyssynchrony <- function(strain_tagging, ...) {

    stopifnot(is.array(strain_tagging))

    n_array <- dim(strain_tagging)[3]

    stopifnot(n_array == 1)

    has_dyssynchrony_data <-
        "dyssynchrony" %in% tolower(names(strain_tagging[,,1]))

    if (!has_dyssynchrony_data) {

        dyssynchrony_data <-
            data.frame(
                basal_anterior      = NA,
                basal_anteroseptal  = NA,
                basal_inferoseptal  = NA,
                basal_inferior      = NA,
                basal_inferolateral = NA,
                basal_anterolateral = NA,
                mid_anterior        = NA,
                mid_anteroseptal    = NA,
                mid_inferoseptal    = NA,
                mid_inferior        = NA,
                mid_inferolateral   = NA,
                mid_anterolateral   = NA,
                apical_anterior     = NA,
                apical_septal       = NA,
                apical_inferior     = NA,
                apical_lateral      = NA
            )
    } else {

        dyssynchrony_extracted <-
            strain_tagging[,,1]$dyssynchrony[,,1]$tfcirc

        which_class <- class(dyssynchrony_extracted)

        stopifnot(which_class %in% c("matrix", "tbl_df", "data.frame"))

        dyssynchrony_data <-
            data.frame(
                basal_anterior      = dyssynchrony_extracted[, 1],
                basal_anteroseptal  = dyssynchrony_extracted[, 2],
                basal_inferoseptal  = dyssynchrony_extracted[, 3],
                basal_inferior      = dyssynchrony_extracted[, 4],
                basal_inferolateral = dyssynchrony_extracted[, 5],
                basal_anterolateral = dyssynchrony_extracted[, 6],
                mid_anterior        = dyssynchrony_extracted[, 7],
                mid_anteroseptal    = dyssynchrony_extracted[, 8],
                mid_inferoseptal    = dyssynchrony_extracted[, 9],
                mid_inferior        = dyssynchrony_extracted[, 10],
                mid_inferolateral   = dyssynchrony_extracted[, 11],
                mid_anterolateral   = dyssynchrony_extracted[, 12],
                apical_anterior     = dyssynchrony_extracted[, 13],
                apical_septal       = dyssynchrony_extracted[, 14],
                apical_inferior     = dyssynchrony_extracted[, 15],
                apical_lateral      = dyssynchrony_extracted[, 16]
            )
    }

    return(dyssynchrony_data)
}
