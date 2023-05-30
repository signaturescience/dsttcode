library(tidyverse)

dat <- tibble::tribble(
         ~RecordAxisCode1, ~RecordAxisCode2, ~RecordAxisCode3, ~RecordAxisCode4, ~RecordAxisCode5, ~id,
                    "W19",           "F191",           "S065",           "T402",           "T519",  1L,
                   "I251",           "F102",           "F179",           "I517",           "T402",  2L,
                    "W19",           "F191",           "S065",               NA,               NA,  3L,
                   "I251",           "F102",           "F179",           "I517",               NA,  4L
         )
dat

opioid_codes <- c("T400", "T401", "T402", "T403", "T404")

dat %>%
  pivot_longer(RecordAxisCode1:RecordAxisCode5) %>%
  group_by(id) %>%
  mutate(opioid = ifelse(any(substring(value,1,4) %in% opioid_codes, na.rm = TRUE), 1, 0)) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  ungroup()

add_opioid_column <- function(df, opioid_codes=c("T400", "T401", "T402", "T403", "T404")) {
  if (! "id" %in% colnames(df)) {"'id' must be a column name in df"}
  dat %>%
    pivot_longer(!id) %>%
    group_by(id) %>%
    mutate(opioid = ifelse(any(substring(value,1,4) %in% opioid_codes, na.rm = TRUE), 1, 0)) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    ungroup()
}

add_opioid_column(dat)
add_opioid_column(dat, opioid_codes = c("W19", "F191", "I251", "F179"))
