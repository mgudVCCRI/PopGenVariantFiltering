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
  scores <- c(grep("_score$", colnames(x), value = TRUE), grep("_phred$", colnames(x), value = TRUE))
  scores_to_remove <- setdiff(scores, snakemake@params[["scores_to_include"]])
  x <- select(x, -all_of(scores_to_remove))
}

# For use with quantile to replace quantiles with identical values
# with NAs (in cases when it's not possible to differentiate between
# very fine quantiles past some point)
replace_duplicates_with_NA <- function(arr) {
    arr[duplicated(arr) | duplicated(arr, fromLast = TRUE)] <- NA
    arr
}

x %>%
  # All scores should end with "_score", with the only exception being CADD ("CADD_phred")
  mutate_at(vars(ends_with("_score"), ends_with("_phred")), ~ {
    if (any(is.na(.))) {
      stop("NAs found in input scores")
    } else {
      cut(.,
        breaks = replace_duplicates_with_NA(quantile(.,
          probs = snakemake@params[["quantiles"]],
        )),
        include.lowest = TRUE,
        # NOTE: remove to have intervals instead of numbers
        labels = FALSE
      )
    }
  }) -> out

# NOTE: this makes the top 100% group centered (one point)
if (!is.null(snakemake@params[["NA_omit"]]) && snakemake@params[["NA_omit"]]) {
  out <- na.omit(out)
}

write.table(out, file = snakemake@output[["Out"]], quote = FALSE, row.names = FALSE, sep = "\t")
