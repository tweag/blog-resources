#!/usr/bin/env Rscript

library(tidyverse)

data <-
  read_tsv(
    file      = file("stdin"),
    col_types = "iiiiii"
  ) %>%
  mutate(
    cat.rate            = (size / 1024) / (cat / 10^9),
    topiary.full.rate   = (size / 1024) / (topiary.full / 10^9),
    topiary.noidem.rate = (size / 1024) / (topiary.noidem / 10^9),
    shfmt.rate          = (size / 1024) / (shfmt / 10^9)
  ) %>%
  select(
    # "cat\n(control)"                 = cat.rate,
    "Topiary\n(full)"                  = topiary.full.rate,
    "Topiary\n(w/o Idempotency Check)" = topiary.noidem.rate,
    "shfmt"                            = shfmt.rate
  ) %>%
  pivot_longer(
    cols           = everything(),
    names_to       = "source",
    values_to      = "rate",
    values_drop_na = TRUE
  )

plot <-
  ggplot(data, aes(x = rate, y = source, fill = source)) +
    geom_boxplot() +
    scale_x_continuous(breaks = seq(0, 500, by = 50)) +
    labs(x = "Rate (KiB/s)", y = NULL) +
    guides(fill = "none")

ggsave(
  file   = "output.svg",
  plot   = plot,
  width  = 10,
  height = 5
)
