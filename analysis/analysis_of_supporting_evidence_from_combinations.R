library(dplyr)
library(magrittr)
library(stringr)

x <- read.table(snakemake@input[["scores"]], header = TRUE, sep = "\t")

x <- x %>%
  mutate(chain_len = str_count(method, "&") + 1) %>%
  mutate(ref_free = is.na(str_match(method, snakemake@params[["reference"]])))

ref_score <- x[x$method == snakemake@params[["reference"]], ]$caps

info <- x %>%
  filter(ref_free == TRUE) %>%
  group_by(chain_len) %>%
  summarise(max_caps = max(caps)) %>%
  mutate(caps_dist_to_ref = ref_score - max_caps) %>%
  mutate(caps_jump_perc = 100 * (lag(caps_dist_to_ref) - caps_dist_to_ref) / lag(caps_dist_to_ref))

sink(snakemake@output[["out"]])
info
sink()
