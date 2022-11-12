formatCons <- function (stats, nam, tr, group.freq, prmsd, sep = "/", formatArgs = NULL,
          round = NULL, prtest, lang = "plain", testUsed = character(0),
          middle.bold = FALSE, outer.size = NULL, msdsize = NULL, brmsd = FALSE,
          pdig = 3, eps = 0.001, footnoteTest = TRUE, prob = c(0.25,
                                                               0.5, 0.75), prN = FALSE, mspecs = markupSpecs)
{
    specs <- mspecs[[lang]]
    spc <- specs$space
    bold <- if (middle.bold)
        specs$bold
    else function(x) x
    lspc <- specs$lspace
    sup <- specs$sup
    br <- specs$br
    plminus <- specs$plminus
    math <- specs$math
    if (lang != "latex" || !length(msdsize))
        msdsize <- function(x) x
    if (!is.function(msdsize)) {
        Msdsize <- msdsize
        msdsize <- function(x) paste0("{\\", Msdsize, " ", x,
                                      "}")
    }
    if (lang != "latex")
        outer.size <- function(x) x
    if (!is.function(outer.size)) {
        Outer.size <- outer.size
        outer.size <- function(x) paste0("{\\", Outer.size, " ",
                                         x, "}")
    }
    nw <- if (lg <- length(group.freq))
        lg
    else 1
    ns <- dimnames(stats)[[2]]
    ns <- ifelse(ns %in% c("Mean", "SD", "N"), "-1", ns)
    ns <- as.numeric(ns)
    l <- 1:length(ns)
    if (length(prob) == 3) {
        qs <- numeric(3)
        for (i in seq_along(qs)) {
            qs[i] <- l[abs(ns - prob[i]) < 0.001]
        }
    }
    else {
        q1 <- l[abs(ns - 0.25) < 0.001]
        med <- l[abs(ns - 0.5) < 0.001]
        q3 <- l[abs(ns - 0.75) < 0.001]
        qs <- c(q1, med, q3)
    }
    qu <- stats[, qs, drop = FALSE]
    if (prmsd)
        qu <- cbind(qu, stats[, c("Mean", "SD"), drop = FALSE])
    if (length(round) && round == "auto") {
        r <- max(abs(stats[, colnames(stats) %nin% c("N", "SD")]),
                 na.rm = TRUE)
        round <- if (r == 0)
            2
        else max(0, min(5, 3 - round(log10(r))))
    }
    if (length(round))
        qu <- round(qu, round)
    ww <- c(list(qu), formatArgs)
    cqu <- do.call("format", ww)
    if (prN)
        cqu <- cbind(cqu, stats[, "N", drop = FALSE])
    cqu[is.na(qu)] <- ""
    if (lang != "plain") {
        st <- character(nrow(cqu))
        names(st) <- dimnames(qu)[[1]]
        for (j in 1:nrow(cqu)) {
            st[j] <- paste0(bold(cqu[j, 2])," [", outer.size(cqu[j, 1]), "-",
                            outer.size(cqu[j, 3]), "]")
            if (prmsd) {
                z <- if (brmsd)
                    paste0(br, msdsize(paste0(cqu[j, 4], spc, plminus,
                                              spc, cqu[j, 5])))
                else paste0(spc, spc, msdsize(paste0("(", cqu[j,
                                                              4], spc, plminus, cqu[j, 5], ")")))
                st[j] <- paste0(st[j], z)
            }
            if (prN)
                st[j] <- paste0(st[j], outer.size(paste0(spc,
                                                         math("N=", cqu[j, ncol(cqu)]))))
        }
    }
    else {
        if (prmsd) {
            st <- apply(cqu, 1, function(x, sep) paste(x[1],
                                                       sep, x[2], sep, x[3], "  ", x[4], "+/-", x[5],
                                                       sep = ""), sep = sep)
        }
        else {
            st <- apply(cqu[, seq(3), drop = FALSE], 1, paste,
                        collapse = sep)
        }
        if (prN) {
            st <- setNames(sprintf("%s  N=%s", st, cqu[, ncol(cqu),
                                                       drop = FALSE]), names(st))
        }
    }
    if (nw == 1)
        yj <- st
    else {
        yj <- rep("", nw)
        names(yj) <- names(group.freq)
        yj[names(st)] <- st
    }
    if (length(tr)) {
        ct <- formatTestStats(tr, prtest = prtest, lang = lang,
                              testUsed = testUsed, pdig = pdig, eps = eps, footnoteTest = footnoteTest,
                              mspecs = mspecs)
        yj <- c(yj, ct)
    }
    matrix(yj, nrow = 1, dimnames = list(nam, names(yj)))
}
