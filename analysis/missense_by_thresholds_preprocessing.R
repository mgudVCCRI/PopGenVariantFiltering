library(dplyr)
library(magrittr)

x <- read.table(snakemake@input[["unprocessed"]], header = TRUE, sep = "\t")

x <- select(x, -matches("^meets_"), snakemake@params[["filters"]])

# Combinations of filters #######################################
# Get all column names that start with "column_"
cols <- grep("^meets_", names(x), value = TRUE)
# Create a list of all possible combinations of column names
combos <- combn(cols, 2, simplify = FALSE)
# Loop through the list of combinations and create new columns
for (i in seq_along(combos)) {
  col1 <- combos[[i]][1]
  col2 <- combos[[i]][2]
  new_col <- paste0(col1, "_", col2)
  x <- x %>% mutate(!!new_col := ifelse(x[[col1]] == "true" & x[[col2]] == "true", "true", "false"))
}
#################################################################

write.table(x, file = snakemake@output[["processed"]], quote = FALSE, row.names = FALSE, sep = "\t")
