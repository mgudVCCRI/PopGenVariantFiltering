library(dplyr)
library(magrittr)
library(stringr)

df <- data.frame()

for (i in 1:length(snakemake@input[["scores"]])) {
  x <- read.table(snakemake@input[["scores"]][i], header = TRUE, sep = "\t")
  df <- rbind(df, x)
}

df <- df %>%
  filter(variable_value == "true") %>%
  select(-variable_value) %>%
  rowwise() %>%
  ######################################################################
  # These three mutate operations extract the name of the tool for
  # which the threshold was set, the source of that threshold and the
  # prediction. For combinations of filters, "method", "source" and
  # "prediction" will contain '&' symbols. For example, "SIFT &
  # PolyPhen" and "source1 & source2" means that the threshold for
  # SIFT was taken from "source1", the threshold for PolyPhen was
  # taken from "source2" and a combination of those two filters was
  # used.
  mutate(method = {
    method <- lapply(unlist(str_split(variable, "meets_"))[-1], function(v) str_split(v, "_")) %>% lapply(function(a) unlist(a)[1:3])
    sapply(seq_along(method[[1]]), function(i) paste(sapply(method, "[[", i), collapse = " & "))[1]
  }) %>%
  mutate(source = {
    source <- lapply(unlist(str_split(variable, "meets_"))[-1], function(v) str_split(v, "_")) %>% lapply(function(a) unlist(a)[1:3])
    sapply(seq_along(source[[1]]), function(i) paste(sapply(source, "[[", i), collapse = " & "))[2]
  }) %>%
  mutate(prediction = {
    prediction <- lapply(unlist(str_split(variable, "meets_"))[-1], function(v) str_split(v, "_")) %>% lapply(function(a) unlist(a)[1:3])
    sapply(seq_along(prediction[[1]]), function(i) paste(sapply(prediction, "[[", i), collapse = " & "))[3]
  }) %>%
  ######################################################################
  select(-variable) %>%
  write.table(file = snakemake@output[["joined_scores"]], quote = FALSE, row.names = FALSE, sep = "\t")
