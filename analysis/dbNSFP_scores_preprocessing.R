# Preprocessing of per-variant dbNSFP scores: (1) mirrors (multiplies by -1) scores from user-specified list, (2) removes variants with at least one NA score

library(dplyr)
library(magrittr)

x <- vroom::vroom(snakemake@input[["In"]])

# Adjust (mirror) scores where necessary
x <- mutate(x, across(snakemake@params[["scores_to_adjust"]], ~ . * -1))

# Remove variants with NA scores
if (!is.null(snakemake@params[["scores_to_include"]])) {
  scores <- c(grep("_score$", colnames(x), value = TRUE), grep("_phred$", colnames(x), value = TRUE))
  scores_to_remove <- setdiff(scores, snakemake@params[["scores_to_include"]])
  x <- select(x, -all_of(scores_to_remove))
}
if (snakemake@params[["NA_omit"]]){
    x <- na.omit(x)
}

write.table(x, file = snakemake@output[["Out"]], quote = FALSE, row.names = FALSE, sep = "\t")
