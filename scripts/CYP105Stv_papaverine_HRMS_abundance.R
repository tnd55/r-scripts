# ===============================================================
# Script: plot_ms_optimized.R
# Purpose: High-efficiency Mass Spectrometry Plotting
# ===============================================================

# ---- Load Packages ----
# pacman handles loading and installing automatically
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, scales, data.table)

# ---- 1. Scientific Notation Helper ----
# Simplified using scales::label_scientific or a cleaner regex
sci_labeler <- function(x) {
  ifelse(x == 0, "0", parse(text = gsub("e\\+?", " %*% 10^", scales::scientific(x))))
}

# ---- 2. Core Plotting Function ----
# Abstracting this allows you to loop through multiple files easily
plot_ms_data <- function(df, mz_range = c(320, 370), max_int = 6e6) {
  ggplot(df, aes(x = mz, y = intensity)) +
    geom_line(color = "black", linewidth = 0.5) +
    scale_x_continuous(
      limits = mz_range, 
      breaks = seq(mz_range[1], mz_range[2], by = 5),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_y_continuous(
      limits = c(0, max_int),
      labels = sci_labeler,
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = "m/z", y = "Abundance") +
    theme_classic(base_size = 14) + # Classic removes grid & adds lines automatically
    theme(
      axis.text = element_text(color = "black"),
      axis.line = element_line(linewidth = 0.8),
      plot.margin = margin(10, 20, 10, 10)
    )
}

# ---- 3. Main Execution ----
file_path   <- "data/CYP105Stv_papaverine.csv"
output_path <- "figures/105Stv_PV.png"

if (file.exists(file_path)) {
  # fread is significantly faster than read.csv for large MS datasets
  ms_data <- fread(file_path, select = c(1, 2), col.names = c("mz", "intensity"))
  
  # Generate Plot
  p <- plot_ms_data(ms_data)
  
  print(p)
  # Save with high-res settings
  ggsave(output_path, p, width = 10, height = 7, dpi = 600, bg = "white")
  
  message("✅ Plot saved: ", output_path)
} else {
  stop("❌ File not found.")
}