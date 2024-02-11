# Generates a figure showing CAPS scores for variants split into groups

library(dplyr)
library(ggplot2)

scores <- read.table(snakemake@input[["scores"]],
  header = TRUE,
  sep = "\t"
)

# Reformatting ######################################################
# TODO: [2023-05-17] double-check that this works correctly
if (!is.null(snakemake@params[["source_labels_set"]])) scores <- filter(scores, source %in% snakemake@params[["source_labels_set"]])
if (!is.null(snakemake@params[["method_labels_set"]])) scores <- filter(scores, method %in% snakemake@params[["method_labels_set"]])

if (!is.null(snakemake@params[["source_labels"]]) || !is.null(snakemake@params[["new_source_labels"]])) {
  if (!is.null(snakemake@params[["source_labels"]]) && !is.null(snakemake@params[["new_source_labels"]])) {
    scores[["source"]] <- factor(scores[["source"]],
      levels = snakemake@params[["source_labels"]],
      labels = snakemake@params[["new_source_labels"]]
    )
  } else {
    stop("Either original or new labels were not provided")
  }
}

if (!is.null(snakemake@params[["method_labels"]]) || !is.null(snakemake@params[["new_method_labels"]])) {
  if (!is.null(snakemake@params[["method_labels"]]) && !is.null(snakemake@params[["new_method_labels"]])) {
    scores[["method"]] <- factor(scores[["method"]],
      levels = snakemake@params[["method_labels"]],
      labels = snakemake@params[["new_method_labels"]]
    )
  } else {
    stop("Either original or new labels were not provided")
  }
}

if (!is.null(snakemake@params[["prediction_labels"]]) || !is.null(snakemake@params[["new_prediction_labels"]])) {
  if (!is.null(snakemake@params[["prediction_labels"]]) && !is.null(snakemake@params[["new_prediction_labels"]])) {
    scores[["prediction"]] <- factor(scores[["prediction"]],
      levels = snakemake@params[["prediction_labels"]],
      labels = snakemake@params[["new_prediction_labels"]]
    )
  } else {
    stop("Either original or new labels were not provided")
  }
}
#####################################################################

pdf(snakemake@output[["plot"]])
ggplot(scores) +
  aes(
    x = factor(!!sym(ifelse(!is.null(snakemake@params[["x_axis"]]), snakemake@params[["x_axis"]], "source"))), y = !!sym(snakemake@params[["score_name"]]),
  ) +
  xlab(snakemake@params[["xlab"]]) +
  ylab(ifelse(snakemake@params[["score_name"]] %in% c("maps", "caps", "caps_pdd"), case_when(
    (snakemake@params[["score_name"]] == "maps") ~ "MAPS",
    (snakemake@params[["score_name"]] == "caps") ~ "CAPS",
    (snakemake@params[["score_name"]] == "caps_pdd") ~ "CAPS-PDD"
  ), stop("Score name error"))) +
  geom_pointrange(
    aes(
      ymin = !!sym(snakemake@params[["lconf"]]),
      ymax = !!sym(snakemake@params[["uconf"]]),
      color = !!sym(ifelse(!is.null(snakemake@params[["color_var"]]), snakemake@params[["color_var"]], "method")),
      #TODO: put back
      # shape = prediction
    ),
    linewidth = 1.8,
    alpha = ifelse(is.null(snakemake@params[["point_alpha"]]), 1, snakemake@params[["point_alpha"]]),
    size = ifelse(is.null(snakemake@params[["point_size"]]), 1.3, snakemake@params[["point_size"]]),
    position = position_dodge(width = ifelse(is.null(snakemake@params[["dodge_width"]]), 0.65, snakemake@params[["dodge_width"]]))
  ) +
  {
    if (!is.null(snakemake@params[["ylim_min"]]) &&
      !is.null(snakemake@params[["ylim_max"]])) {
      ylim(
        snakemake@params[["ylim_min"]],
        snakemake@params[["ylim_max"]]
      )
    }
  } +
  {
    if (!is.null(snakemake@params[["sector_to_highlight"]])) {
      annotate("rect", xmin = snakemake@params[["sector_to_highlight"]] - 0.5, xmax = snakemake@params[["sector_to_highlight"]] + 0.5, ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = 0.4)
    }
  } +
  theme_classic() +
  theme(
    aspect.ratio = ifelse(!is.null(snakemake@params[["aspect_ratio"]]), snakemake@params[["aspect_ratio"]], 1),
    legend.position = ifelse(!is.null(snakemake@params[["legend_position"]]), snakemake@params[["legend_position"]], "bottom"),
    axis.text.x = element_text(
      vjust = snakemake@params[["xlab_vjust"]],
      hjust = snakemake@params[["xlab_hjust"]],
      angle = ifelse(is.null(snakemake@params[["xlab_angle"]]), 0, snakemake@params[["xlab_angle"]])
    ),
    text = element_text(size = ifelse(is.null(snakemake@params[["text_size"]]), 24, snakemake@params[["text_size"]])),
    plot.margin = margin(0, 5.5, 0, 5.5)
  ) +
  scale_color_manual(snakemake@params[["color_legend_title"]],
    values = snakemake@params[["colors"]]
    ) +
    #TODO: put back
  # scale_shape_manual(snakemake@params[["shape_legend_title"]],
  #   values = snakemake@params[["shapes"]]
  # ) +
  {
    if (!is.null(snakemake@params[["gnomAD_missense_level"]])) geom_hline(aes(yintercept = snakemake@params[["gnomAD_missense_level"]]), linetype = "dashed")
  } +
    {
    if (!is.null(snakemake@params[["rotate"]]) && (snakemake@params[["rotate"]] == TRUE)) coord_flip()
  }
dev.off()
