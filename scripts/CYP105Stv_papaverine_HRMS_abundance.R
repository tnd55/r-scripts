# ===============================================================
# Script: plot_ms_clean.R
# Purpose: Plot Mass Spectrometry data (m/z vs Intensity)
# Author: [Your Name]
# Date: [Date]
# ===============================================================

# ---- Load Packages ----
if (!require(ggplot2, quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)
library(scales)

# ---- 1. Helper Function: Scientific Notation Formatter ----
sci_format <- function(x) {
  formatted <- scientific_format()(x)               # "1.6e+05"
  formatted[x == 0] <- "0"                          # keep exact zeros
  parse(text = gsub("e\\+?", " %*% 10^", formatted))# convert to 1.6 × 10^5
}

# ---- 2. User Input ----
file_path <- "data/CYP105Stv_papaverine.csv"      # update path as needed
output_path <- "figures/CYP105Stv_papaverine_v3.png"

# ---- 3. Read & Validate Data ----
if (!file.exists(file_path)) stop("❌ File not found: ", file_path)

data <- read.csv(file_path, header = TRUE)
if (ncol(data) < 2) stop("❌ CSV must have at least two columns: m/z and intensity")

colnames(data)[1:2] <- c("mz", "intensity")

# ---- 4. Plot Setup ----
p <- ggplot(data, aes(x = mz, y = intensity)) +
  geom_line(color = "black", linewidth = 0.5) +
  labs(
    x = "m/z",
    y = "Abundance"
  ) +
  scale_x_continuous(
    limits = c(300, 370),
    breaks = seq(300, 370, by = 5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 6e6),
    breaks = seq(0, 6e6, by = 2e6),
    labels = sci_format,
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    axis.line       = element_line(color = "black", linewidth = 0.8),
    panel.grid      = element_blank(),
    axis.ticks      = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(5, "pt"),
    axis.text       = element_text(color = "black"),
    plot.margin     = margin(10, 30, 10, 10)
  ) +
  coord_cartesian(clip = "off")

# ---- 5. Display Plot ----
print(p)

# ---- 6. Save Plot ----
ggsave(
  filename = output_path,
  plot = p,
  width = 16, height = 3, dpi = 300
)

message("✅ Plot saved to: ", output_path)
