# ================================================
# Script: plot_ms.R
# Purpose: Plot Mass Spectrometry data (m/z vs Intensity)
# Author: [Your Name]
# Date: [Date]
# ================================================

# ---- Load Required Package ----
# Install ggplot2 if not already installed
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)

# ---- User Input ----
# Path to your CSV file (update this to your actual file path)
file_path <- "data/CYP105Stv_papaverine.csv"

# ---- Read Data ----
# Assumes first column = m/z, second column = relative intensity
data <- read.csv(file_path, header = TRUE)

# ---- Basic Validation ----
if (ncol(data) < 2) stop("Error: CSV must have at least two columns (m/z, intensity).")

# Rename columns for clarity
colnames(data)[1:2] <- c("mz", "intensity")

# ---- Plot ----
p <- ggplot(data, aes(x = mz, y = intensity)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Mass Spectrum",
    x = "m/z (mass-to-charge ratio)",
    y = "Relative Intensity (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# ---- Display Plot ----
print(p)

# ---- Optional: Save Plot ----
# ggsave("mass_spectrum_plot.png", p, width = 8, height = 4, dpi = 300)
