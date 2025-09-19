# This function joins groups of variants split into bins by creating
# nested bins. Nested bins can be interpreted as top 100%, top 50%, etc.

library(magrittr)
library(dplyr)

# Load groups of variants split into bins
groups <- read.table(snakemake@input[["groups"]], header = TRUE)

# Regroup by variable of interest
groups <- groups %>%
  group_by_at(vars("context", "ref", "alt", "methylation_level", "mu", snakemake@params[["variable"]])) %>%
  summarise(
    singleton_count = sum(singleton_count),
    variant_count = sum(variant_count)
  ) %>%
  ungroup()

# All bins, from 1st to the last observed. If starts from 0, increment
# all bins (that is, if bins were calculated in Python).
if (min(groups[[snakemake@params[["variable"]]]]) == 0) groups <- mutate(groups, across(snakemake@params[["variable"]], ~ . + 1))
bins <- seq(1:max(groups[[snakemake@params[["variable"]]]]))

df <- data.frame()
for (i in length(bins):1) {
  # Go from last bin to first (the higher the score, the higher the bin number)
  groups %>%
    # Only include the current bin and all higher scores
    filter(.data[[snakemake@params[["variable"]]]] %in% bins[i:length(bins)]) %>%
    # Drop the bins column
    group_by(context, ref, alt, methylation_level, mu) %>%
    summarise(
      singleton_count = sum(singleton_count),
      variant_count = sum(variant_count)
    ) %>%
    ungroup() %>%
    # The new table is all combinations of "context", "ref", "alt",
    # "methylation_level" and "mu", where "variant_count" and
    # "singleton_count" includes variants in the current bin and all
    # variants with higher scores
    mutate(rep(bins[i:length(bins)][1], dim(.)[1])) %>%
    as.data.frame() %>%
    rbind(df) -> df
}

colnames(df) <- c(
  "context",
  "ref",
  "alt",
  "methylation_level",
  "mu",
  "singleton_count",
  "variant_count",
  snakemake@params[["variable"]]
)
write.table(df, snakemake@output[["joined_groups"]],
  row.names = FALSE, quote = FALSE, sep = "\t"
)
