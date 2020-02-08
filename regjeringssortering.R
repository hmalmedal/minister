library(tidyverse)

source("factor_no.R")

csvfil <- "regjering.csv"

d <- read_csv(csvfil) %>%
  mutate_at(vars(Navn), factor_no) %>%
  arrange(Sluttdato, Startdato, desc(Navn)) %>%
  arrange(desc(row_number()))

write_csv(d, csvfil)
