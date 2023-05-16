library(dplyr)
library(magrittr)

#TODO: make this work for general cases!

x <- read.table(snakemake@input[["unprocessed"]], header = TRUE, sep = "\t")

x %>%
  rowwise() %>%
  mutate(
    meets_all_auth = ifelse(meets_sift_auth == "true" & meets_polyphen_auth == "true", "true", "false"),
    meets_all_95sens = ifelse(meets_sift_95sens == "true" & meets_polyphen_95sens == "true", "true", "false")
  ) %>%
  write.table(file = snakemake@output[["processed"]], quote = FALSE, row.names = FALSE, sep = "\t")


# x %>%
#   rowwise() %>%
#   mutate(
#     meets_all_auth = ifelse(meets_cadd_auth == "true" & meets_sift_auth == "true" & meets_polyphen_auth == "true", "true", "false"),
#     meets_all_95sens = ifelse(meets_cadd_95sens == "true" & meets_sift_95sens == "true" & meets_polyphen_95sens == "true", "true", "false")
#   ) %>%
#   write.table(file = snakemake@output[["processed"]], quote = FALSE, row.names = FALSE, sep = "\t")
