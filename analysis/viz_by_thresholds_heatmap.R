# Generates a figure showing CAPS scores for variants split into groups

# TODO: only works for up to 2 filters maximum

library(dplyr)
library(ggplot2)
library(tidyr)

scores <- read.table(snakemake@input[["scores"]],
  header = TRUE,
  sep = "\t"
)

# Reformatting ######################################################
# TODO: [2023-05-17] double-check that this works correctly
if (!is.null(snakemake@params[["source_labels_set"]])) scores <- filter(scores, source %in% snakemake@params[["source_labels_set"]])
if (!is.null(snakemake@params[["method_labels_set"]])) scores <- filter(scores, method %in% snakemake@params[["method_labels_set"]])

# TODO: this is not being used as of [2023-06-13]
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
# TODO: this is not being used as of [2023-06-13]
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

scores <- select(scores, caps, method) %>%
  separate(method, into = c("tool1", "tool2"), sep = " & ") %>%
  mutate(tool2 = ifelse(is.na(tool2), coalesce(tool2, tool1), tool2))

# Ensure symmetry
scores <- mutate(scores, tool1_copy = tool1, tool1 = tool2, tool2 = tool1_copy) %>%
  select(-tool1_copy) %>%
  bind_rows(scores)

ggplot(scores) +
  {
    if (!is.null(snakemake@params[["labels_order"]])) {
      aes(
        x = factor(tool1, levels = snakemake@params[["labels_order"]]),
        y = factor(tool2, levels = snakemake@params[["labels_order"]]),
        fill = caps
      )
    } else {
      aes(
        x = tool1,
        y = tool2,
        fill = caps
      )
    }
  } +
  xlab(snakemake@params[["xlab"]]) +
  ylab(snakemake@params[["ylab"]]) +
  geom_tile() +
  theme_minimal() +
  scale_fill_gradient(snakemake@params[["legend_title"]], low = "blue", high = "red", na.value = "white") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dev.off()
