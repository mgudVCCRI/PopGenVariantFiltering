# This function joins multiple tables with identical structure (same header)

library(dplyr)

df <- data.frame()

for (i in 1:length(snakemake@input[["In"]])) {
  x <- read.table(snakemake@input[["In"]][i], sep = "\t", header = TRUE)
  if (!is.null(snakemake@params[["labels_to_rename_variable_with"]])) {
    x <- mutate(x, variable = snakemake@params[["labels_to_rename_variable_with"]][i])
  }
  df <- rbind(df, x)
}

write.table(df, file = snakemake@output[["Out"]], quote = FALSE, row.names = FALSE, sep = "\t")
