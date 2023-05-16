# Per-group statistics for ClinVar

library(dplyr)
library(magrittr)
library(xtable)

x <- read.table(snakemake@input[["In"]], header = TRUE, sep = "\t")

if (snakemake@params[["NA_omit"]]) x <- x[!is.na(x[[snakemake@params[["variable"]]]]), ]

if (!is.null(snakemake@params[["labels_set"]])) x <- filter(x, .data[[snakemake@params[["variable"]]]] %in% snakemake@params[["labels_set"]])

if (!is.null(snakemake@params[["labels"]]) || !is.null(snakemake@params[["new_labels"]])) {
  if (!is.null(snakemake@params[["labels"]]) && !is.null(snakemake@params[["new_labels"]])) {
    x[[snakemake@params[["variable"]]]] <- factor(x[[snakemake@params[["variable"]]]],
      levels = snakemake@params[["labels"]],
      labels = snakemake@params[["new_labels"]]
    )
  } else {
    stop("Either original or new labels were not provided")
  }
}

x <- group_by(x, .data[[snakemake@params[["variable"]]]]) %>%
  summarise(
    `Total` = sum(variant_count),
    `Proportion singletons` = round(sum(singleton_count) / sum(variant_count), 3)
  )

x %>%
  xtable(
    caption = snakemake@params[["caption"]], label = snakemake@params[["label"]],
    align = "ll|rr", display = c("s", "s", "s", "s")
  ) %>%
  print(include.rownames = FALSE) %>%
  write(snakemake@output[["Out"]])
