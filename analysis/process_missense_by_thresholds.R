library(dplyr)
library(magrittr)
library(stringr)

df <- data.frame()

for (i in 1:length(snakemake@input[["scores"]])) {
  x <- read.table(snakemake@input[["scores"]][i], header = TRUE, sep = "\t")
  df <- rbind(df, x)
}

# Since NAs are excluded, there are only two options, "Yes" and "No"
df <- df %>%
  filter(variable_value == "true") %>%
  select(-variable_value) %>%
  rowwise() %>%
  mutate(method = unlist(str_split(variable, "_"))[2], source = unlist(str_split(variable, "_"))[3]) %>%
  select(-variable) %>%
  write.table(file = snakemake@output[["joined_scores"]], quote = FALSE, row.names = FALSE, sep = "\t")
