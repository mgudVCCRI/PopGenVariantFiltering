library(dplyr)
library(magrittr)

x <- read.table(snakemake@input[["unprocessed"]], header = TRUE, sep = "\t")

# Remove NA filter annotations
if (snakemake@params[["NA_omit"]]) {
  x <- na.omit(x)
}

x <- select(x, -matches("^meets_"), snakemake@params[["filters"]])

# Combinations of filters #######################################
# Get all column names that start with "meets_"
cols <- grep("^meets_", names(x), value = TRUE)
# Create a list of all possible combinations of column names
if (snakemake@params[["max_combinations"]] >= 2) {
  combos <- combn(cols, 2, simplify = FALSE)
  # Loop through the list of combinations and create new columns
  for (i in seq_along(combos)) {
    col1 <- combos[[i]][1]
    col2 <- combos[[i]][2]
    new_col <- paste0(col1, "_", col2)
    x <- x %>% mutate(!!new_col := ifelse(x[[col1]] == "true" & x[[col2]] == "true", "true", "false"))
  }
}
if (snakemake@params[["max_combinations"]] >= 3) {
  combos <- combn(cols, 3, simplify = FALSE)
  # Loop through the list of combinations and create new columns
  for (i in seq_along(combos)) {
    col1 <- combos[[i]][1]
    col2 <- combos[[i]][2]
    col3 <- combos[[i]][3]
    new_col <- paste0(col1, "_", col2, "_", col3)
    x <- x %>% mutate(!!new_col := ifelse(x[[col1]] == "true" & x[[col2]] == "true" & x[[col3]] == "true", "true", "false"))
  }
}
if (snakemake@params[["max_combinations"]] >= 4) {
  combos <- combn(cols, 4, simplify = FALSE)
  # Loop through the list of combinations and create new columns
  for (i in seq_along(combos)) {
    col1 <- combos[[i]][1]
    col2 <- combos[[i]][2]
    col3 <- combos[[i]][3]
    col4 <- combos[[i]][4]
    new_col <- paste0(col1, "_", col2, "_", col3, "_", col4)
    x <- x %>% mutate(!!new_col := ifelse(x[[col1]] == "true" & x[[col2]] == "true" & x[[col3]] == "true" & x[[col4]] == "true", "true", "false"))
  }
}
if (snakemake@params[["max_combinations"]] >= 5) {
  combos <- combn(cols, 5, simplify = FALSE)
  # Loop through the list of combinations and create new columns
  for (i in seq_along(combos)) {
    col1 <- combos[[i]][1]
    col2 <- combos[[i]][2]
    col3 <- combos[[i]][3]
    col4 <- combos[[i]][4]
    col5 <- combos[[i]][5]
    new_col <- paste0(col1, "_", col2, "_", col3, "_", col4, "_", col5)
    x <- x %>% mutate(!!new_col := ifelse(x[[col1]] == "true" & x[[col2]] == "true" & x[[col3]] == "true" & x[[col4]] == "true" & x[[col5]] == "true", "true", "false"))
  }
}
if (snakemake@params[["max_combinations"]] >= 6) {
  combos <- combn(cols, 6, simplify = FALSE)
  # Loop through the list of combinations and create new columns
  for (i in seq_along(combos)) {
    col1 <- combos[[i]][1]
    col2 <- combos[[i]][2]
    col3 <- combos[[i]][3]
    col4 <- combos[[i]][4]
    col5 <- combos[[i]][5]
    col6 <- combos[[i]][6]
    new_col <- paste0(col1, "_", col2, "_", col3, "_", col4, "_", col5, "_", col6)
    x <- x %>% mutate(!!new_col := ifelse(x[[col1]] == "true" & x[[col2]] == "true" & x[[col3]] == "true" & x[[col4]] == "true" & x[[col5]] == "true" & x[[col6]] == "true", "true", "false"))
  }
}
if (snakemake@params[["max_combinations"]] >= 7) {
  combos <- combn(cols, 7, simplify = FALSE)
  # Loop through the list of combinations and create new columns
  for (i in seq_along(combos)) {
    col1 <- combos[[i]][1]
    col2 <- combos[[i]][2]
    col3 <- combos[[i]][3]
    col4 <- combos[[i]][4]
    col5 <- combos[[i]][5]
    col6 <- combos[[i]][6]
    col7 <- combos[[i]][7]
    new_col <- paste0(col1, "_", col2, "_", col3, "_", col4, "_", col5, "_", col6, "_", col7)
    x <- x %>% mutate(!!new_col := ifelse(x[[col1]] == "true" & x[[col2]] == "true" & x[[col3]] == "true" & x[[col4]] == "true" & x[[col5]] == "true" & x[[col6]] == "true" & x[[col7]] == "true", "true", "false"))
  }
}
if (snakemake@params[["max_combinations"]] >= 8) {
  combos <- combn(cols, 8, simplify = FALSE)
  # Loop through the list of combinations and create new columns
  for (i in seq_along(combos)) {
    col1 <- combos[[i]][1]
    col2 <- combos[[i]][2]
    col3 <- combos[[i]][3]
    col4 <- combos[[i]][4]
    col5 <- combos[[i]][5]
    col6 <- combos[[i]][6]
    col7 <- combos[[i]][7]
    col8 <- combos[[i]][8]
    new_col <- paste0(col1, "_", col2, "_", col3, "_", col4, "_", col5, "_", col6, "_", col7, "_", col8)
    x <- x %>% mutate(!!new_col := ifelse(x[[col1]] == "true" & x[[col2]] == "true" & x[[col3]] == "true" & x[[col4]] == "true" & x[[col5]] == "true" & x[[col6]] == "true" & x[[col7]] == "true" & x[[col8]] == "true", "true", "false"))
  }
}
if (snakemake@params[["max_combinations"]] >= 9) {
  combos <- combn(cols, 9, simplify = FALSE)
  # Loop through the list of combinations and create new columns
  for (i in seq_along(combos)) {
    col1 <- combos[[i]][1]
    col2 <- combos[[i]][2]
    col3 <- combos[[i]][3]
    col4 <- combos[[i]][4]
    col5 <- combos[[i]][5]
    col6 <- combos[[i]][6]
    col7 <- combos[[i]][7]
    col8 <- combos[[i]][8]
    col9 <- combos[[i]][9]
    new_col <- paste0(col1, "_", col2, "_", col3, "_", col4, "_", col5, "_", col6, "_", col7, "_", col8, "_", col9)
    x <- x %>% mutate(!!new_col := ifelse(x[[col1]] == "true" & x[[col2]] == "true" & x[[col3]] == "true" & x[[col4]] == "true" & x[[col5]] == "true" & x[[col6]] == "true" & x[[col7]] == "true" & x[[col8]] == "true" & x[[col9]] == "true", "true", "false"))
  }
}
if (snakemake@params[["max_combinations"]] >= 10) {
  stop("Too many combinations!")
}
#################################################################

write.table(x, file = snakemake@output[["processed"]], quote = FALSE, row.names = FALSE, sep = "\t")
