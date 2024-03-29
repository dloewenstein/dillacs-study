knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(pROC)

# Set paths
PROJECT_DIR <- rprojroot::find_rstudio_root_file()
RAW_DATA_DIR <- file.path(PROJECT_DIR, "analysis", "data", "raw_data")
DERIVED_DATA_DIR <- file.path(PROJECT_DIR, "analysis", "data", "derived_data")
FIG_DIR <- file.path(PROJECT_DIR, "analysis", "figures")

dillacs <- readRDS(file.path(DERIVED_DATA_DIR, "dyssynchrony_data.Rds"))
stats <- readRDS(file.path(DERIVED_DATA_DIR, "statistics.Rds"))
source(file.path(PROJECT_DIR, "R", "geom_signif.R"))

safe_colors <- RColorBrewer::brewer.pal(3, "PuOr")[c(1, 3)]
names(safe_colors) <- c("SSI", "CURE")

safe_colors_boxplot <- safe_colors
names(safe_colors_boxplot) <- c("LBBB", "Normal")
# annotation <- data.frame(dyssynchrony_index = c("CURE", "SSI"),
#                          pval = c(stats$cure$wilcox$p.value, stats$ssi$wilcox$p.value))

pval_form <- function(x) {
    if (x < 0.001) 
        return(paste("p<0.001"))
    else
        paste("p", round(x, 3), sep = "=")
}

pval_form <- Vectorize(pval_form)

ci_form <- function(x) {
    stopifnot(class(x) == "bootci")
    
    # if (any(x[[4L]][c(4,5)] < 0)) {
    # sprintf("%.2f [(%.2f)-(%.2f)]", x$t0, x[[4L]][4],x[[4L]][5])
    # } else {
    # sprintf("%.2f [%.2f-%.2f]", x$t0, x[[4L]][4],x[[4L]][5])
    # }
    sprintf("%.2f [%.2f, %.2f]", x$t0, x[[4L]][4],x[[4L]][5])
}

ci_form <- Vectorize(ci_form)

# pval_annotation <- data.frame(dyssynchrony_index = c("CURE", "SSI"),
#                               x = c(1.5, 1.5),
#                               y = c(1.01, 24.05),
#                               pval = ci_form(list(stats$cure$dys_median_ci,
#                                              stats$ssi$dys_median_ci)),
#                               stringsAsFactors = FALSE)

pval_annotation <- data.frame(dyssynchrony_index = c("CURE", "SSI"),
                              xmin = c(1, 1),
                              xmax = c(2,2),
                              y = c(1.03, 24.08),
                              annotations = c(
                                  pval_form(stats$cure$wilcox$p.value),
                                  pval_form(stats$ssi$wilcox$p.value)
                                  ),
                              stringsAsFactors = FALSE)

p_dys_distr <-
    dillacs %>%
    select(id, lbbb1_control0, cure, ssi) %>%
    group_by(id) %>%
    gather(dyssynchrony_index, value, -lbbb1_control0, -id) %>%
    mutate(dyssynchrony_index = toupper(dyssynchrony_index)) %>% 
    ggplot(
        aes(lbbb1_control0, value,
           fill   = 
               factor(lbbb1_control0,
                      levels = c("LBBB", "Control"),
                      labels = c("LBBB", "Normal")),
           colour = 
               factor(lbbb1_control0, 
                      levels = c("LBBB", "Control"), 
                      labels = c("LBBB", "Normal")
                      )
           )
        ) +
    geom_boxplot(alpha = 0.75, width = 0.6) +
    geom_point(aes(x = as.numeric(lbbb1_control0) - 0.47), 
               alpha = 0.75, 
               position = position_jitter(width = 0.09)) +
    # geom_text(data = pval_annotation, mapping = aes(x=x, y=y, label = pval),
    #           inherit.aes = FALSE,
    #           fontface = "bold") +
    geom_signif(data = pval_annotation,
                mapping = aes(annotations = annotations, xmin = xmin, xmax = xmax, y_position = y, group = dyssynchrony_index),
                inherit.aes = FALSE,
                manual = TRUE) +
    geom_blank(data = data.frame(dyssynchrony_index = c("CURE", "CURE", "SSI", "SSI"),
                                 value = c(0, 1.08, 0, 25.5),
                                 lbbb1_control0 = c("LBBB", "Control", "Control", "LBBB"))) +
    facet_wrap(~dyssynchrony_index, 
               scales = "free",
               strip.position = "left",
               labeller = 
                   as_labeller(
                       c(
                           CURE = "Circumferential uniformity ratio estimate (CURE) [a.u.]", 
                           SSI = "Systolic Stretch Index (SSI) [%]"))) +
    xlab(NULL) +
    scale_fill_manual(values = safe_colors_boxplot) +
    scale_color_manual(values = safe_colors_boxplot) +
    scale_x_discrete(expand = expansion(add = c(0.7, 0))) +
    scale_y_continuous(name = NULL, expand = c(0,0),
                       breaks = function(x){ifelse(x[2]<=2, return(seq(0,1,by=0.25)), return(seq(0, 25, by=5)))}) +
    expand_limits(y = 0) +
    guides(fill = FALSE, colour = FALSE) +
    theme_classic() +
    theme(strip.placement = "outside",
          strip.background = element_blank(),
          axis.text = element_text(face = "bold", size = 12),
          strip.text = element_text(face = "bold", size = 12))
ggsave("dyssynchrony_distribution.png", path = FIG_DIR, dpi = 600)

auc_legend <- data.frame(
    sensitivity = rep(1, 2), 
    specificity = rep(1, 2), 
    name = paste(
        round(c(stats$cure$fit$auc, stats$ssi$fit$auc), 2),
        c(" CURE" , " SSI")
    )
)

get_coords <- function(x) {
    data.frame(thresh = x$thresh,
               thresh_lower = x$cithresh["2.5%"],
               thresh_upper = x$cithresh["97.5%"],
               auc = x$auc[[1]],
               auc_lower = x$auc_ci["2.5%"],
               auc_upper = x$auc_ci["97.5%"],
               sensitivity = x$sens,
               sens_lower  = x$cisens["2.5%"],
               sens_upper  = x$cisens["97.5%"],
               specificity = x$spec,
               spec_lower  = x$cispec["2.5%"],
               spec_upper  = x$cispec["97.5%"])
}

roc_results <- bind_rows(get_coords(stats$ssi), get_coords(stats$cure))
roc_results$dyssynchrony_index <- factor(c("SSI", "CURE"), levels = c("SSI", "CURE"))

format_thresh <- function(thresh, sens, spec) {
    if (thresh < 1)
        paste0(sprintf("Threshold: %.2f", thresh),
               "\n",
               sprintf("Sensitivity: %.0f%%", sens*100),
               "\n",
               sprintf("Specificity: %.0f%%", spec*100)
        )
    else
        paste0(sprintf("Threshold: %.1f", thresh),
               "\n",
               sprintf("Sensitivity: %.0f%%", sens*100),
               "\n",
               sprintf("Specificity: %.0f%%", spec*100)
        )
}

format_thresh <- Vectorize(format_thresh)

text_height <- 0.25
text_size   <- 4.25

p_roc_auc <-
    ggroc( list( SSI = stats$ssi$fit,  CURE = stats$cure$fit ), 
        size = 1.1
    ) +
    annotate("segment", x = 1,  xend = 0,  y = 0,  yend = 1, 
        #color = "darkgrey", 
        linetype = "dashed",
        alpha = 0.2,
    ) +
    geom_point( data = roc_results,  aes( specificity,  sensitivity ),
        shape = 1, size = 4, stroke = 1.1, inherit.aes = FALSE
    ) +
    geom_ribbon( data = rbind(stats$ssi$roc_se_ci, stats$cure$roc_se_ci),
                 aes( x = sp, ymin = `2.5%`, ymax = `97.5%`,
                      group = name, fill = name ),
                 inherit.aes = FALSE, alpha = 0.2 ) +
    geom_text( data = roc_results,  aes( specificity, sensitivity,
                                         label = dyssynchrony_index ),
        inherit.aes = FALSE,
        size = text_size,
        fontface = "bold",
        nudge_x = c(.04, .04),
        nudge_y = c(-.02, -.02),
        hjust = 0) +
    geom_text( data = roc_results,  aes( specificity,  sensitivity,
                                         label = format_thresh(threshold, 
                                                               sensitivity, 
                                                               specificity),
                                         lineheight = 0.8
                                         ),
        inherit.aes = FALSE,
        size = text_size,
        fontface = "bold",
        nudge_x = c(.04, .04),
        nudge_y = c(-.075, -.075),
        hjust = 0
    ) +
    geom_text( data = roc_results,  aes( 0.40,  text_height + 0.03, 
                                         label = dyssynchrony_index ),
        inherit.aes = FALSE,
        size = text_size,
        fontface = "bold",
        nudge_y = c(.03, 0),
        hjust = 0
    ) +
    geom_text( data = roc_results,  aes( 0,  text_height + 0.03,  
                                         label = sprintf( "%.2f [%.2f, %.2f]", 
                                                          auc, 
                                                          auc_lower, 
                                                          auc_upper )
                                         ),
        inherit.aes = FALSE,
        size = text_size,
        fontface = "bold",
        nudge_y = c(.03, 0),
        hjust = 1
    ) +
    annotate(
        "text", 
        x = 0, 
        y = text_height, 
        label = pval_form(stats$compare_auc$p.value),
        hjust = 1,
        size = text_size,
        fontface = "bold"
    ) +
    annotate(
        "text", 
        x = 0, 
        y = text_height + 0.09, 
        label = "AUC [95% CI]", 
        hjust = 1,
        size = text_size,
        fontface = "bold"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_reverse(labels = scales::percent) +
    scale_color_manual(values = safe_colors) +
    scale_fill_manual(values = safe_colors) +
    labs(x = "Specificity", y = "Sensitivity") +
    guides(color = FALSE, fill = FALSE) +
    theme_classic() +
    theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 12)
    )

ggsave("roc_by_dyssynchrony_index.png", path = FIG_DIR, dpi = 600, width = 7, height = 7)


capitalize <- function(text) {
    
    .capitalize <- function(text) {
        text <- strsplit(text, "")
        text[[1]][1] <- toupper(text[[1]][1])
        paste0(unlist(text), collapse = "")
    }
    .capitalize <- Vectorize(.capitalize, USE.NAMES = FALSE)
    text <- .capitalize(text)
}

get_coords_long <- function(x, name_idx) {
    data.frame(dyssynchrony_index = name_idx,
               auc = x$auc[[1]],
               statistical_measure = c("Sensitivity", "Specificity"),
               value = c(x$sens$sensitivity, x$spec$specificity),
               cilower = c(x$cisens["2.5%"], x$cispec["2.5%"]),
               ciupper = c(x$cisens["97.5%"], x$cispec["97.5%"]),
               stringsAsFactors = FALSE)
}

roc_ci <- bind_rows(get_coords_long(stats$ssi, "SSI"), get_coords_long(stats$cure, "CURE"))

perf_measure_pval_annotation <- data_frame(
    statistical_measure = c("Sensitivity", "Specificity"),
    pval = pval_form(c(stats$sens_xmcnemar, stats$spec_xmcnemar))
)

p_perf_measure_cutoff <-
    ggplot(
        roc_ci,
        aes(
            dyssynchrony_index, 
            value, 
            fill = dyssynchrony_index,
            label = scales::percent(round(value, 2))
        )
    ) +
    geom_col() +
    geom_errorbar(
        aes(
            ymin = cilower, 
            ymax = ciupper
        ), 
        width = .2
    ) +
    geom_text(
        aes(
            y = (value + 0.015)
        ), 
        size = 3, 
        fontface = "bold", 
        position = position_nudge(x = -.25)
    ) +
    geom_text(
        data = perf_measure_pval_annotation,
        mapping = aes(
            x = c(1.5, 1.5),
            y = c(1.055, 1.055),
            label = pval,
            fontface = "bold"
        ),
        inherit.aes = FALSE
    ) +
    # annotate(
    #     "text",
    #     x = c(1.5, 1.5),
    #     y = c(1.055, 1.055),
    #     label = pval_form(c(stats$sens_xmcnemar, stats$spec_xmcnemar)),
    #     fontface = "bold"
    # ) +
    scale_y_continuous(
        name = "Percent (%)", 
        labels = scales::percent, 
        expand = expansion(add = c(0, 0.05))
    ) +
    scale_x_discrete(expand = expansion(add = -2)) +
    facet_wrap(
        ~statistical_measure,
        strip.position = "bottom"
    ) +
    labs(x = "") +
    scale_fill_manual(values = safe_colors) +
    guides(fill = FALSE) +
    theme_classic() +
    theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12), 
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12)
    )

ggsave("performance_measure_cutoff.png", path = FIG_DIR, dpi = 600)

corr_plots <- 
    ggplot(
        dillacs %>%
            select(lbbb1_control0, ssi, cure, QRSDur) %>% 
            gather(index, value, -lbbb1_control0, -QRSDur),
        aes(
            QRSDur, 
            value, 
            colour = index
        )
    ) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(lbbb1_control0 ~ index, scales = "free") +
    scale_color_manual(values = safe_colors) +
    guides(color = FALSE) +
    theme_classic() +
    theme(
        axis.text = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12), 
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12)
    )

figs <- list(
    dyssynchrony_distribution = p_dys_distr,
    roc_by_dyssynchrony_index = p_roc_auc,
    performance_measure_cutoff = p_perf_measure_cutoff,
    dysqrs_cor = corr_plots
)

saveRDS(figs, file.path(DERIVED_DATA_DIR, "figs.Rds"))
