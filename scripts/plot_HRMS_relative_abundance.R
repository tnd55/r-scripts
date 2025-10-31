# ================================================
# Script: plot_ms_relative_zoom.R
# Purpose: Plot Mass Spectrometry data (m/z vs Relative Abundance, sqrt scale, zoomed)
# Author: [Your Name]
# Date: [Date]
# ================================================

# ---- Load Required Package ----
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)

# ---- 1. Scientific-notation formatter (optional, kept for consistency) ----
sci_format <- function(x) {
  formatted <- scales::scientific_format()(x)
  formatted[x == 0] <- "0"
  parse(text = gsub("e\\+?", " %*% 10^", formatted))
}

# ---- 2. User Input ----
file_path <- "data/CYP105Stv_papaverine.csv"  # update path as needed

# ---- 3. Read Data ----
data <- read.csv(file_path, header = TRUE)
if (ncol(data) < 2) stop("Error: CSV must have at least two columns (m/z, intensity).")

# Rename columns for clarity
colnames(data)[1:2] <- c("mz", "intensity")

# ---- 4. Calculate Relative Abundance ----
data$relative_abundance <- (data$intensity / max(data$intensity, na.rm = TRUE)) * 100

# ---- 5. Plot ----
p <- ggplot(data, aes(x = mz, y = relative_abundance)) +
  geom_line(color = "black", linewidth = 0.5) +
  labs(
    x = "m/z",
    y = "Relative Abundance (sqrt scale)"
  ) +
  scale_x_continuous(
    limits = c(300, 370),                     # adjust as needed
    breaks = seq(300, 370, by = 5),
    expand = c(0, 0)
  ) +
  scale_y_sqrt(
    limits = c(0, 100),
    breaks = c(0, 1, 2, 5, 10, 20, 50, 100),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 50)) +           # 🔹 Zoom in vertically to 0–10%
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 0.8),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(5, "pt"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )

# ---- 6. Display Plot ----
print(p)

# ---- 7. Save Plot ----
ggsave("figures/CYP105Stv_papaverine_v2.png",
       p, width = 8, height = 3, dpi = 300)

# ---- 8. Save Updated Data ----
#write.csv(data, "data/papaverine_Noxide_HRMS_with_relative.csv", row.names = FALSE)
