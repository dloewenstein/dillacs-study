#' Circumferential uniformity ratio estimate
#'
#' Circumferential uniformity ratio estimate for dyssynchrony analysis of
#' cardiac strain.
#'
#' @section Background:
#' Circumferential uniformity ratio estimate (cure) is a measure typically used
#' for analysis of left ventricular (LV) mechanical dyssynchrony. Ranging from 0,
#' meaning complete dyssynchrony, that is, all opposing wall segments in LV move
#' in different directions. And 1 meaning complete synchrony, all walls contract
#' and shortens at the same time. Cure \eqn{\le 0.75} has previously been used to
#' define dyssynchrony.
#'
#' @param data Dataframe, columns containing strainvalues from LV wall segments
#' according to the AHA-17 segment model.
#'
#' @return
#' @export
#'
#' @examples
strain_cure <- function(data, limit=NULL, svd=FALSE, order = NULL) {
    if (sum(is.na(data)) > 0) {
        stop("data contains missing values")
    }

    if(not_null(order)) {
        stopifnot(
            any(
                is.character(order),
                is.numeric(order),
                is.integer(order)
            )
        )
        if(is.character(order)) stopifnot(all(order %in% names(data)))
        if(is.numeric(order) || is.integer(order)) stopifnot(all(order %in% seq_along(data)))

        data <- data[, order]
    }

    if (not_null(limit)) {
        if (is.numeric(limit) && (limit < 1)) {
            limit <- round(nrow(data) * limit)
        } else {
        limit <- as.integer(limit)
        }

        stopifnot(limit < nrow(data))
        data <- data[1:limit, ]
    }

    if (svd) {
        data <- .svd_preproc(data)
        cure_svd <- .calc_cure_svd(data)
        return(cure_svd)
    }

    cure <- vector("numeric", nrow(data))

    for (i in 1:nrow(data)) {
        cure[i] <- .calc_cure(as.numeric(data[i, , drop = FALSE]))
    }
    cure <- mean(cure, na.rm = TRUE)

    return(cure)
}



#' Circumferential uniformity ratio estimate from singular value
#' decomposition
#'
#' Circumferential uniformity ratio estimate based on singular value decomposition
#' of cardiac strain for dyssynchrony analysis.
#'
#' @inheritSection strain_cure Background
#'
#' @inheritParams strain_cure
#'
#' @return
#' @export
#'
#' @examples
strain_cure_svd <- function(data, order){
        if (sum(is.na(data)) > 0) {
                stop("Trying to apply svd to data with NA will\n                                    result in an error.")
        }
        if (!requireNamespace("dplyr", quietly = TRUE)) {
                stop("dplyr needed for this function to work. Please install/load it.",
                     call. = FALSE)
        }

    if(not_null(order)) {
        stopifnot(
            any(
                is.character(order),
                is.numeric(order),
                is.integer(order)
            )
        )
        if(is.character(order)) stopifnot(all(order %in% names(data)))
        if(is.numeric(order) || is.integer(order)) stopifnot(all(order %in% seq_along(data)))

        data <- data[, order]
    }


        old <- options(stringsAsFactors = FALSE)
        cure_data <- data.frame()

        strain <- as.matrix(data)

        strain_svd <- svd(strain, nu = nrow(strain), nv = ncol(strain))

        V <- as.matrix(strain_svd$v[, 1])

        U <- as.matrix(t(strain_svd$u[, 1]))

        D <- strain_svd$d[1]

        svd_matrix <- V %*% U

        rank1 <- (svd_matrix * D)[, 5]

        fourier_frq <- fft(rank1)

        cure_val <- Mod(fourier_frq[1])/sum(Mod(fourier_frq[1]),
                                            Mod(fourier_frq[2]))

        options(old)
        return(cure_val)
}

.svd_preproc <- function(strain) {
        strain <- as.matrix(strain)

        strain_svd <- svd(strain, nu = nrow(strain), nv = ncol(strain))

        V <- as.matrix(strain_svd$v[, 1])

        U <- as.matrix(t(strain_svd$u[, 1]))

        D <- strain_svd$d[1]

        svd_matrix <- V %*% U

        rank1 <- (svd_matrix * D)[, 5]
}

.calc_cure_svd <- function(strain) {
    #Perform fourier transformation
    fourier_frq <- fft(strain)
    # Calculate CURE: f0/(f0 + f1)
    cure <-  (
        Mod(fourier_frq[1])
        /sum(Mod(fourier_frq[1]), Mod(fourier_frq[2]))
        )
}

.calc_cure <- function(strain) {

    fcol = .fftshift(fft(strain))

    fsq  = (fcol * Conj(fcol))

    Nsector = length(fcol)
    idx = (1 + ceiling(Nsector/2))

    #The above equation is what was used previously by John Hopkins and
    #Drew in the DENSE analysis. However, the zero order term should be
    #based on the following index. But this code maintains what others have done up until now.
    #idx = ceil((Nsector + 1) /2);

    e0 = fsq[idx]
    e1 = sum(fsq[idx - 1], fsq[idx + 1])
    #Added Mod as to return the real number
    CURE_val = Mod(1./sqrt(1 + (e1/(e0 + .Machine$double.eps))))
}

not_null <- function(x) {
    !is.null(x)
}

.fftshift <- function(x) {

    # This function is adapted from Matlab.

    if (!is.vector(x))
        stop("Argument 'x' must be a vector.")

    m <- length(x)
    p <- ceiling(m/2)
    x[c((p+1):m, 1:p)]
}
