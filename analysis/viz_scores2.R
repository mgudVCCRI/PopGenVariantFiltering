# Generates a figure showing the distribution of "adjusted proportion
# of singletons" scores by variable

library(dplyr)
library(ggplot2)

scores <- read.table(snakemake@input[["scores"]],
  header = TRUE,
  sep = "\t"
)

if (!is.null(snakemake@params[["variable2_sep"]])) {
  scores <- scores %>% mutate(variable2 = stringr::str_split_i(variable, snakemake@params[["variable2_sep"]], 2), variable = stringr::str_split_i(variable, snakemake@params[["variable2_sep"]], 1))
}

if (!is.null(snakemake@params[["variable2_swap"]]) && snakemake@params[["variable2_swap"]] == TRUE) {
  scores <- scores %>%
    mutate(variableX = variable, variable = variable2, variable2 = variableX) %>%
    select(-variableX)
}

if (!is.null(snakemake@params[["xlab_labels_set"]])) scores <- filter(scores, variable_value %in% snakemake@params[["xlab_labels_set"]])

if (!is.null(snakemake@params[["xlab_labels"]]) || !is.null(snakemake@params[["new_xlab_labels"]])) {
  if (!is.null(snakemake@params[["xlab_labels"]]) && !is.null(snakemake@params[["new_xlab_labels"]])) {
    scores[["variable_value"]] <- factor(scores[["variable_value"]],
      levels = snakemake@params[["xlab_labels"]],
      labels = snakemake@params[["new_xlab_labels"]]
    )
  } else {
    stop("Either original or new labels were not provided")
  }
}

if (!is.null(snakemake@params[["color_labels"]]) || !is.null(snakemake@params[["new_color_labels"]])) {
  if (!is.null(snakemake@params[["color_labels"]]) && !is.null(snakemake@params[["new_color_labels"]])) {
    scores[["variable"]] <- factor(scores[["variable"]],
      levels = snakemake@params[["color_labels"]],
      labels = snakemake@params[["new_color_labels"]]
    )
  } else {
    stop("Either original or new labels were not provided")
  }
}

if (!is.null(snakemake@params[["variable2_labels"]]) || !is.null(snakemake@params[["new_variable2_labels"]])) {
  if (!is.null(snakemake@params[["variable2_labels"]]) && !is.null(snakemake@params[["new_variable2_labels"]])) {
    scores[["variable2"]] <- factor(scores[["variable2"]],
      levels = snakemake@params[["variable2_labels"]],
      labels = snakemake@params[["new_variable2_labels"]]
    )
  } else {
    stop("Either original or new labels were not provided")
  }
}

# TODO: make this filtering specific: scores[!is.na(...),]
if (!is.null(snakemake@params[["NA_omit"]]) && snakemake@params[["NA_omit"]]) {
  scores <- na.omit(scores)
}

pdf(snakemake@output[["plot"]])
ggplot(scores) +
  {
    if (snakemake@params[["reorder_xlab_by_score"]]) {
      aes(x = reorder(factor(variable_value), color = variable, !!sym(snakemake@params[["score_name"]])), y = !!sym(snakemake@params[["score_name"]]))
    } else {
      aes(x = factor(variable_value), color = variable, y = !!sym(snakemake@params[["score_name"]]))
    }
  } +
  ylab(ifelse(snakemake@params[["score_name"]] %in% c("maps", "caps", "caps_pdd"), case_when(
    (snakemake@params[["score_name"]] == "maps") ~ "MAPS",
    (snakemake@params[["score_name"]] == "caps") ~ "CAPS",
    (snakemake@params[["score_name"]] == "caps_pdd") ~ "CAPS-PDD"
  ), stop("Score name error"))) +
  {
    if (!is.null(snakemake@params[["variable2_sep"]]) && !is.null(snakemake@params[["variable2_swap"]])) {
      facet_wrap(~variable2)
    }
  } +
  xlab(snakemake@params[["xlab"]]) +
  {
    if (!is.null(snakemake@params[["add_lines"]]) && (snakemake@params[["add_lines"]] == TRUE)) geom_line(alpha = 0.5, linewidth = 1.3, aes(group = variable))
  } +
  geom_pointrange(
    aes(
      ymin = !!sym(snakemake@params[["lconf"]]),
      ymax = !!sym(snakemake@params[["uconf"]])
    ),
    linewidth = 1.8,
    alpha = ifelse(is.null(snakemake@params[["point_alpha"]]), 1, snakemake@params[["point_alpha"]]),
    size = ifelse(is.null(snakemake@params[["point_size"]]), 1.3, snakemake@params[["point_size"]]),
    position = position_dodge(width = ifelse(is.null(snakemake@params[["dodge_width"]]), 0.65, snakemake@params[["dodge_width"]]))
  ) +
  {
    if (!is.null(snakemake@params[["vert_bars"]]) && snakemake@params[["vert_bars"]] == TRUE) {
      geom_vline(linetype = "dotted", xintercept = seq(1.5, 17.5, by = 1.0))
    }
  } +
  {
    if (!is.null(snakemake@params[["xlim_breaks"]])) {
      scale_x_discrete(limits = snakemake@params[["xlim_breaks"]])
    }
  } +
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
      annotate("rect", xmin = snakemake@params[["sector_to_highlight"]] - 0.5, xmax = snakemake@params[["sector_to_highlight"]] + 0.5, ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3)
    }
  } +
  theme_classic() +
  theme(
    aspect.ratio = ifelse(!is.null(snakemake@params[["aspect_ratio"]]), snakemake@params[["aspect_ratio"]], 1),
    text = element_text(size = ifelse(is.null(snakemake@params[["text_size"]]), 18, snakemake@params[["text_size"]])),
    legend.position = ifelse(is.null(snakemake@params[["legend_position"]]), "top", snakemake@params[["legend_position"]]),
    axis.text.x = element_text(
      size = ifelse(is.null(snakemake@params[["xlab_size"]]), 18, snakemake@params[["xlab_size"]]),
      vjust = snakemake@params[["xlab_vjust"]],
      hjust = snakemake@params[["xlab_hjust"]],
      angle = ifelse(is.null(snakemake@params[["xlab_angle"]]), 0, snakemake@params[["xlab_angle"]])
    ),
    plot.margin = margin(0, 5.5, 0, 5.5)
  ) +
  scale_color_manual(snakemake@params[["legend_title"]],
    values = snakemake@params[["colors"]]
  )
dev.off()
