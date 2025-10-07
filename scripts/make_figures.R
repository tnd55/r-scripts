#!/usr/bin/env Rscript
library(tidyverse)
library(svglite)

# Ensure the figures directory exists
if (!dir.exists("figures")) dir.create("figures")

# Load data
df <- read_csv("data/experiment1.csv")

# Example plot
p1 <- ggplot(df, aes(x = Time, y = Measurement)) +
    geom_line(size = 0.8) +
    theme_minimal(base_size = 10) +
    theme(text = element_text(family = "Arial"))

# Save figure
ggsave("figures/exp1_plot.pdf", plot = p1, width = 5, height = 4)