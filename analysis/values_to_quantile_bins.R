# This function takes a list of variants annotated with pathogenicity
# scores as input and converts those annotations to "bins", where each
# bin is based on the observed distribution of the scores. As a
# result, the values in each column become integers, but the number of
# variants remains unchanged.

library(dplyr)
library(magrittr)

x <- vroom::vroom(snakemake@input[["In"]])

if (snakemake@params[["variants_input"]] == TRUE) {
  x <- select(x, -locus, -alleles) %>%
    mutate(variant_count = 1, singleton_count = ifelse(AC == 1, 1, 0))
}

if (!is.null(snakemake@params[["scores_to_include"]])) {
  scores <- c(grep("_score$", colnames(x), value = TRUE), grep("^CADD_", colnames(x), value = TRUE))
  scores_to_remove <- setdiff(scores, snakemake@params[["scores_to_include"]])
  x <- select(x, -all_of(scores_to_remove))
}

x %>%
  # All scores should end with "_score", with the only exception being CADD
  mutate_at(vars(ends_with("_score"), starts_with("CADD_")), ~ {
    if (any(is.na(.))) {
      stop("NAs found in input scores")
    } else {
      quantiles <- quantile(.,
        probs = snakemake@params[["quantiles"]],
      )
      if (length(snakemake@params[["quantiles"]]) != 8) stop("Wrong quantiles")
      case_when(
        (. >= quantiles[8]) ~ 8,
        (. >= quantiles[7]) ~ 7,
        (. >= quantiles[6]) ~ 6,
        (. >= quantiles[5]) ~ 5,
        (. >= quantiles[4]) ~ 4,
        (. >= quantiles[3]) ~ 3,
        (. >= quantiles[2]) ~ 2,
        (. >= quantiles[1]) ~ 1,
        TRUE ~ NA_real_
      )
    }
  }) -> x

x %>%
  # All scores should end with "_score", with the only exception being CADD
  mutate_at(vars(ends_with("_score"), starts_with("CADD_")), ~ {
    if (any(is.na(.))) {
      stop("NAs found in quantile bins")
    } else {.}
  }) -> x

write.table(x, file = snakemake@output[["Out"]], quote = FALSE, row.names = FALSE, sep = "\t")
