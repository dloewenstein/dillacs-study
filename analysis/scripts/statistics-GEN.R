knitr::opts_chunk$set(echo = TRUE)

# Set paths
PROJECT_DIR <- rprojroot::find_rstudio_root_file()
RAW_DATA_DIR <- file.path(PROJECT_DIR, "analysis", "data", "raw_data")
DERIVED_DATA_DIR <- file.path(PROJECT_DIR, "analysis", "data", "derived_data")
FIG_DIR <- file.path(PROJECT_DIR, "analysis", "figures")

library(pROC)
library(dplyr)
library(rms)
library(boot)
set.seed(28)

if(!interactive()){
    old <- options(pROCProgress = list(name = "none"))
    pdf(NULL)
}

old_opt <- options(list(boot.n = 4000, boot.stratified = TRUE))

dillacs <- readRDS(file.path(DERIVED_DATA_DIR, "dyssynchrony_data.Rds"))

dillacs <- mutate(
    dillacs, 
    lbbb1_control0 = factor(
        lbbb1_control0, 
        levels = c("Control", "LBBB")
    )
)
ddd <- datadist(
    dillacs %>% 
        select(lbbb1_control0, Age, EDVI, LVMI, cure, ssi, Gender)
)
options(datadist = "ddd")

#' Regress dyssynchrony measures on covariates using least squares
#'
#' @param dv Dyssynchrony measure
#' @param level Patient category c("Control", "LBBB")
#'
#' @details Fits covariates Age, EDVI, LVMI using restricted cubic splines,
#'          (with 3 knots, places at quintiles estimated via `datadist`) and 
#'          Gender, as well as interactions between Gender and LVMI, EDVI.
#' @return ols() fit
lm_covar_effects_on_dysindex <- function(dv, level) {
    
    # Create formula
    form <- as.formula(
        paste(
            deparse(substitute(dv)), 
            "~ rcs(Age, 3) + rcs(LVMI, 3)*Gender + rcs(EDVI, 3)*Gender")
    )
    dat <- subset(dillacs, lbbb1_control0 == level)
    # Use eval(bquote()) to get formula nicely printed in model fit print call
    fit <- eval(bquote(ols(.(form), data = dillacs, subset = (lbbb1_control0 == .(level)))))
    fit
}

lm_ssi_0 <- lm_covar_effects_on_dysindex(ssi, "Control")

lm_cure_0 <- lm_covar_effects_on_dysindex(cure, "Control")

plot(Predict(lm_ssi_0), an = anova(lm_ssi_0), pval = TRUE)

plot(Predict(lm_cure_0), an = anova(lm_cure_0), pval = TRUE)

(lm_ssi_anova_0 <- anova(lm_ssi_0))

(lm_cure_anova_0 <- anova(lm_cure_0))

lm_ssi_1 <- lm_covar_effects_on_dysindex(ssi, "LBBB")

lm_cure_1 <- lm_covar_effects_on_dysindex(cure, "LBBB")

plot(Predict(lm_ssi_1), an = anova(lm_ssi_1), pval = TRUE)
plot(Predict(lm_cure_1), an = anova(lm_cure_1), pval = TRUE)

(lm_ssi_anova_1 <- anova(lm_ssi_1))
(lm_cure_anova_1 <- anova(lm_cure_1))

# Use 1-cure as to get odds ratios for LBBB per decrease in cure.
cure_lrm <- lrm(relevel(lbbb1_control0, ref = "Control") ~ I(((1-cure)-mean(1-cure))*10), 
                dillacs, x = TRUE, y = TRUE)

ssi_lrm <- lrm(relevel(lbbb1_control0, ref = "Control") ~ I(ssi-mean(ssi)),
               dillacs, x = TRUE, y = TRUE)

# Perform wald test for nonlinear effects

(cure_rcs_anova <- anova(update(cure_lrm, ~ rcs(cure,3))))
(ssi_rcs_anova  <- anova(update(ssi_lrm, ~ rcs(ssi, 3))))

#' Extract model fit coef confidence intervals
#'
#' @param obj Logistic regression model fit with `lrm()`
#'
#' @return Vector with coef (log odds) and confidence interval
coef.ci.se <- function(obj) {
    res <- matrix(
        nrow     = length(coef(obj)), 
        ncol     = 3, 
        dimnames = list(c(), 
                        c("coef", "2.5%", "97.5%")
        )
    )
    res[,1]       <- coef(obj)
    res[,2:3]     <- confint.default(obj)
    rownames(res) <- names(coef(obj))
    res
}

cure.coef.ci <- coef.ci.se(cure_lrm)
ssi.coef.ci  <- coef.ci.se(ssi_lrm)

# We take care to set direction of values for controls > indicating higher values
# for control. This as to not biase AUC to higher values when bootstrapping.
cure.fit <- pROC::roc(lbbb1_control0 ~ cure,
                      data      = dillacs,
                      family    = "binomial",
                      levels    = c("Control", "LBBB"),
                      direction = ">")

ssi.fit  <- pROC::roc(lbbb1_control0 ~ ssi,
                      data      = dillacs,
                      family    = "binomial",
                      levels    = c("Control", "LBBB"),
                      direction = "<")

resid(cure_lrm, type = "partial", pl = "loess")
resid(ssi_lrm, type  = "partial", pl = "loess")

#' Difference between medians
#'
#' @param formula of the form var ~ grouping_factor
#' @param data data.frame
#' @param ... 
#'
#' @return list with group1 median, group2 median, stats = difference
median.formula <- function(formula, data, ...) {
    if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]), 
                                                                    "term.labels")) != 1L)) 
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if (nlevels(g) != 2L) 
        stop("grouping factor must have exactly 2 levels")
    DATA <- setNames(split(mf[[response]], g), c("x", "y"))
    y <- list(call = DNAME, stats = c(), group1 = c(), group2 = c())
    y$group1 <- median(DATA$x)
    y$group2 <- median(DATA$y)
    y$stats <- y$group1 - y$group2
    y <- setNames(y, c(names(y[c(1,2)]), levels(g)))
    y
}

#' Bootstrap difference between medians
#'
#' @param data data.frame
#' @param indices bootstrap sample index
#' @param formula of the form var ~ grouping_factor
#'
#' @return numeric, difference between medians
median.diff.boot <- function(data, indices, formula) {
    stat <- median(formula, data[indices, ])
    stat$stats
}

# Relevel factor as to get difference median LBBB - Control
# Perform stratified resampling considering study design
ssi_median_diff <- boot(dillacs,
                        median.diff.boot,
                        R = getOption("boot.n"),
                        formula = ssi ~ relevel(lbbb1_control0, "LBBB"), 
                        strata = dillacs$lbbb1_control0)

cure_median_diff <- boot(dillacs,
                         median.diff.boot,
                         R = getOption("boot.n"),
                         formula = cure ~ relevel(lbbb1_control0, "LBBB"), 
                         strata = dillacs$lbbb1_control0)

# Confidence intervals using percentile method
ssi_median_diff_ci  <- boot.ci(ssi_median_diff, type = "perc")
cure_median_diff_ci <- boot.ci(cure_median_diff, type = "perc")

# Get location shift hypothesis results
ssi_wilcox <- wilcox.test(ssi ~ lbbb1_control0, dillacs)
cure_wilcox <- wilcox.test(cure ~ lbbb1_control0, dillacs)

ssi_roc_auc <- auc(ssi.fit)
ssi_roc_ci  <- ci.auc(ssi.fit, method = "bootstrap",
                      boot.n = getOption("boot.n"),
                      boot.stratified = getOption("boot.stratified"))

cure_roc_auc <- auc(cure.fit)
cure_roc_ci  <- ci.auc(cure.fit,
                       method = "bootstrap",
                       boot.n = getOption("boot.n"),
                       boot.stratified = getOption("boot.stratified"))

compare_auc <- roc.test(cure.fit,
                        ssi.fit,
                        method = "bootstrap",
                        paired = TRUE,
                        boot.n = getOption("boot.n"),
                        boot.stratified = getOption("boot.stratified"))


#' Bootstrap sensitivity over unique levels of specificity for ROC plot
#'
#' @param fit pROC::roc() fit
#'
#' @return data.frame with sensitivity confidence intervals
boot_roc_se_ci <- function(fit) {
    
    as.data.frame(
        ci.se(
            fit,
            specificities   = sort(unique(fit$specificities)),
            boot.n          = getOption("boot.n"),
            boot.stratified = getOption("boot.stratified")
        ),
        stringsAsFactors = FALSE
    )
}

ssi_roc_se_ci <- boot_roc_se_ci(ssi.fit)

# Rownames are the unique specificites, add those to data.frame for plotting
ssi_roc_se_ci <- cbind(ssi_roc_se_ci,
                       sp = as.numeric(rownames(ssi_roc_se_ci)),
                       name = "SSI")

cure_roc_se_ci <- boot_roc_se_ci(cure.fit)

cure_roc_se_ci <- cbind(cure_roc_se_ci, 
                        sp = as.numeric(rownames(cure_roc_se_ci)),
                        name = "CURE")

cure_cutoff <- coords(cure.fit, x = "best", best.method = "youden")
ssi_cutoff  <- coords(ssi.fit, x = "best", best.method = "youden")

# To accurately represent the uncertainty in the derivation of these cutoff
# threshold we should bootstrap the derivation procedure

#' Bootstrap threshold confidence interval
#'
#' @param fit pROC::roc() fit
#'
#' @return vector with threshold confidence intervals
boot_youden_ci <- function(fit) {
    pROC::ci.coords(fit,
                    x = "best",
                    best.method = "youden",
                    best.policy = "random",
                    ret = "threshold",
                    conf.level = 0.95,
                    boot.n = 4000,
                    boot.stratified = TRUE)
}

cure_cithresh <- boot_youden_ci(cure.fit)
ssi_cithresh  <- boot_youden_ci(ssi.fit)


#' Boostrap sensitivity and specificity for specific threshold
#'
#' @param fit pROC::roc() fit
#' @param thresh cutoff threshold
#'
#' @return confidence intervals
boot_se_sp_ci <- function(fit, thresh) {
    pROC::ci.coords(fit,
                    x = thresh,
                    ret = c("sens", "spec"),
                    best.policy = "random",
                    conf.level  = 0.95,
                    boot.n      = getOption("boot.n", 2000), 
                    boot.stratified = getOption("boot.stratified", TRUE))
}
cure_cicoords <- boot_se_sp_ci(cure.fit, cure_cutoff[["threshold"]])

ssi_cicoords  <- boot_se_sp_ci(ssi.fit, ssi_cutoff[["threshold"]])

get_prob_cutoff <- function(fit) {
    coords(
        roc(
            response  = fit$y,
            predictor = predict(fit, type = "fitted"),
            direction = "<"
        ),
        x = "best",
        best.method = "youden",
        ret = "thresh",
        transpose = TRUE
    )
}

ssi_cutoff_prob  <- get_prob_cutoff(ssi_lrm)

cure_cutoff_prob <- get_prob_cutoff(cure_lrm)


#' Create confusion matrix
#'
#' @param x numeric, c(TRUE, FALSE) prediction
#' @param y numeric, c(TRUE, FALSE) prediction
#'
#' @return confusion matrix
create_table <- function(x, y) {
    
    vec <- vector("numeric", 4)
    tab <- as.vector(table(x, y))
    vec[1:length(tab)] <- tab
    
    mx <- matrix(vec, nrow = 2, ncol = 2, byrow = TRUE,
                 dimnames = list(c(FALSE, TRUE), c(FALSE, TRUE)))
}

# Index for LBBB as to extract their predictions
lbbb_idx <- which(dillacs$lbbb1_control0=="LBBB")

# Placeholder
lbbb <- data.frame(
    matrix(nrow = length(lbbb_idx), ncol = 2, 
           dimnames = list(c(), c("ssi_pred", "cure_pred"))
    )
)

# Get class predictions
lbbb$ssi_pred  <- predict(ssi_lrm, type = "fitted")[lbbb_idx]  >= ssi_cutoff_prob
lbbb$cure_pred <- predict(cure_lrm, type = "fitted")[lbbb_idx] >= cure_cutoff_prob

# Confusion matrix for test of sensitivity
sens_mx <- create_table(lbbb$ssi_pred, lbbb$cure_pred)

# Placeholder
control <- data.frame(
    matrix(nrow = nrow(dillacs[-lbbb_idx,]), ncol = 2, 
           dimnames = list(c(), c("ssi_pred", "cure_pred"))
    )
)

# Controls have lower probs
control$ssi_pred  <- predict(ssi_lrm, type = "fitted")[-lbbb_idx]  < ssi_cutoff_prob
control$cure_pred <- predict(cure_lrm, type = "fitted")[-lbbb_idx] < cure_cutoff_prob

# Confusion matrix for test of specificity
spec_mx <- create_table(control$ssi_pred, control$cure_pred)

extract_disagreement <- function(x) {
    #         | FALSE | TRUE |
    # FALSE   |   A   |   B  |
    # TRUE    |   c   |   D  |
    
    b <- x["FALSE", "TRUE"]
    c <- x["TRUE", "FALSE"]
    
    c(b = b, c = c)
}

# McNemar exact test
sens_bc   <- extract_disagreement(sens_mx)
sens_pval <- binom.test(sens_bc["b"], sum(sens_bc), 0.5)

spec_bc   <- extract_disagreement(spec_mx)
spec_pval <- binom.test(spec_bc["b"], sum(spec_bc), 0.5)

spearman.ci <- function(var1, var2, group) {
    d <- subset(dillacs, lbbb1_control0 == group)
    
    rho <- eval(bquote(cor.test(d[[.(var1)]], d[[.(var2)]], method = "spearman")))
    
    spearman_boot <- function(data, i, var1, var2) {
        d <- data[i,]
        rho <- cor.test(d[[var1]], d[[var2]], method = "spearman", exact = FALSE)$estimate
    }
    
    tboot <- boot::boot(data = d, spearman_boot, R = getOption("boot.n"), var1 = "ssi", var2 = "QRSDur")
    
    tboot.ci <- boot::boot.ci(tboot, conf = 0.95, type = "perc")$percent
    ci <- tboot.ci[4:5]
    attr(ci, "conf.level") <- tboot.ci[1]
    rho[["conf.int"]] <- ci
    rho
}

lm_qrs_cure_0 <- ols(QRSDur ~ rcs(I((1-cure)*10), 5), dillacs, subset = (lbbb1_control0 == "Control"))
lm_qrs_cure_1 <- ols(formula(lm_qrs_cure_0), dillacs, subset = (lbbb1_control0 == "LBBB"))

lm_qrs_ssi_0 <- ols(QRSDur ~ rcs(ssi, 5), dillacs, subset = (lbbb1_control0 == "Control"))
lm_qrs_ssi_1 <- ols(formula(lm_qrs_ssi_0), dillacs, subset = (lbbb1_control0 == "LBBB"))

dillacs_split <- split(dillacs, dillacs$lbbb1_control0)

qrs_cure_cor_0 <- spearman.ci("cure", "QRSDur", "Control")
qrs_cure_cor_1 <- spearman.ci("cure", "QRSDur", "LBBB")

qrs_ssi_cor_0  <- spearman.ci("ssi", "QRSDur", "Control")
qrs_ssi_cor_1  <- spearman.ci("ssi", "QRSDur", "LBBB")

corplot <- dillacs %>%
    select(lbbb1_control0, cure, ssi, QRSDur) %>% 
    tidyr::pivot_longer(cols = c("cure", "ssi"),
                 names_to = c("measure"),
                 values_to = "value") %>% 
    ggplot(aes(x = value, y = QRSDur)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ measure + lbbb1_control0, scales = "free")

corplot

stats <- list(
    cure = list(
        fit           = cure.fit,
        fit.gof       = resid(cure_lrm, type = "gof"), # le cessie van houwelingen goodness-of-fit
        fit.anova     = cure_rcs_anova,
        model         = cure_lrm,
        coef.ci       = cure.coef.ci,
        modstats      = cure_lrm$stats,
        dys_median_ci = cure_median_diff_ci,
        wilcox        = cure_wilcox,
        cor           = list(LBBB = qrs_cure_cor_1, Control = qrs_cure_cor_0),
        auc           = cure_roc_auc,
        auc_ci        = cure_roc_ci,
        roc_se_ci     = cure_roc_se_ci,
        thresh        = cure_cutoff["threshold"],
        sens          = cure_cutoff["sensitivity"],
        spec          = cure_cutoff["specificity"],
        cithresh      = cure_cithresh$threshold[1, c("2.5%", "97.5%")],
        cisens        = cure_cicoords$sensitivity[1, c("2.5%", "97.5%")],
        cispec        = cure_cicoords$specificity[1, c("2.5%", "97.5%")]
    ),
    ssi = list(
        fit           = ssi.fit,
        fit.gof       = resid(ssi_lrm, type = "gof"),
        fit.anova     = ssi_rcs_anova,
        model         = ssi_lrm,
        modstats      = ssi_lrm$stats,
        coef.ci       = ssi.coef.ci,
        dys_median_ci = ssi_median_diff_ci,
        wilcox        = ssi_wilcox,
        cor           = list(LBBB = qrs_ssi_cor_1, Control = qrs_ssi_cor_0),
        auc           = ssi_roc_auc,
        auc_ci        = ssi_roc_ci,
        roc_se_ci     = ssi_roc_se_ci,
        thresh        = ssi_cutoff["threshold"],
        sens          = ssi_cutoff["sensitivity"],
        spec          = ssi_cutoff["specificity"],
        cithresh      = ssi_cithresh$threshold[1, c("2.5%", "97.5%")],
        cisens        = ssi_cicoords$sensitivity[1, c("2.5%", "97.5%")],
        cispec        = ssi_cicoords$specificity[1, c("2.5%", "97.5%")]
    ),
    compare_auc   = compare_auc,
    sens_xmcnemar = sens_pval$p.value,
    spec_xmcnemar = spec_pval$p.value
)

saveRDS(stats, file.path(DERIVED_DATA_DIR, "statistics.Rds"))
