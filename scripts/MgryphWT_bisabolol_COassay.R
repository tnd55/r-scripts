#######################################################################
##  PACKAGES  ─────────────────────────────────────────────────────────
#######################################################################
library(tidyverse)   # ggplot2, readr, dplyr, …
library(conflicted)  # optional—helps resolve naming clashes
library(patchwork)   # for multi-panel layouts (future use)
library(scales)      # nice axis-label helpers


#######################################################################
##  HELPER FUNCTIONS  ────────────────────────────────────────────────
#######################################################################

# ── 1. Scientific-notation formatter (e.g. 1.6 × 10^5) ──────────────
# sci_format <- function(x) {
#   formatted <- scientific_format()(x)               # "1.6e+05"
#   formatted[x == 0] <- "0"                          # keep exact zero as "0"
#   parse(text = gsub("e\\+?", " %*% 10^", formatted))# convert to 1.6 × 10^5
# }

# ── 2. Publication theme (clean, boxed, no grid)  ───────────────────
theme_pub <- function(base_size   = 14,
                      base_family = "Arial") {
  
  theme_minimal(base_size = base_size,
                base_family = base_family) +
    theme(
      
      # ★ Plot & panel backgrounds (blank = no grey fill)
      panel.background = element_blank(),
      plot.background  = element_blank(),
      
      # ★ Axis box & lines
      axis.line   = element_line(color = "black", linewidth = 1),
      # If you prefer a full rectangle border around the panel, uncomment ↓
      # panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      
      # ★ Tick marks
      axis.ticks        = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(0.25, "cm"),  # tick length
      axis.ticks.direction = "out",          # point ticks outward
      
      # ★ Tick-label text (size & color)
      axis.text.x = element_text(color = "black", size = base_size),
      axis.text.y = element_text(color = "black", size = base_size),
      
      # ★ Remove grid lines for a clean look
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      #  ★ Legend: inside plot, frameless, no title
      legend.position   = c(0.75, 0.85),     # (x, y) in [0,1] plot coords
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.title      = element_blank(),
      legend.text       = element_text(size = base_size),
      
      # ★ Axis titles & plot title
      axis.title = element_text(size = base_size * 1.2),
      plot.title = element_text(size = base_size * 1.4, face = "bold")
    )
}

#######################################################################
##  DATA  ─────────────────────────────────────────────────────────────
#######################################################################
# Read the raw chromatogram data
raw <- read_csv("data/MgryphWT_bisabolol_COassay.csv")
raw2 <- read_csv("data/MgryphQE_bisabolol_COassay.csv")

## DATA CLEANING
clean_data <- raw %>%
  pivot_longer(
    cols = -wavelength, 
    names_to = "type", 
    values_to = "absorbance"
  )
## cleaning second set of data
clean_data2 <- raw2 %>%
  pivot_longer(
    cols = -wavelength, 
    names_to = "type", 
    values_to = "absorbance"
  )
#######################################################################
##  PLOT  ────────────────────────────────────────────────────────────
#######################################################################
final <- ggplot(clean_data, aes(wavelength, absorbance, colour = type, group = type)) +
  geom_line(linewidth = 1) +                                   # data line
  labs(x = "Wavelength (nm)", y = "Absorbance") +          # axis titles
  scale_x_continuous(                                          # X-axis
    limits = c(350, 500),
    expand = c(0, 0)                                           # no padding
  ) +
  scale_y_continuous(                                          # Y-axis
    limits  = c(0, 0.8),
    breaks  = seq(0, 0.8, by = 0.2),                      # nice ticks
    expand  = c(0, 0),
    #labels  = sci_format                                       
  ) +
  theme_pub()                                                  # apply theme

#plotting second set of data
final2 <- ggplot(clean_data2, aes(wavelength, absorbance, colour = type, group = type)) +
  geom_line(linewidth = 1) +                                   # data line
  labs(x = "Wavelength (nm)", y = "Absorbance") +          # axis titles
  scale_x_continuous(                                          # X-axis
    limits = c(350, 500),
    expand = c(0, 0)                                           # no padding
  ) +
  scale_y_continuous(                                          # Y-axis
    limits  = c(0, 0.8),
    breaks  = seq(0, 0.8, by = 0.2),                      # nice ticks
    expand  = c(0, 0),
    #labels  = sci_format                                       
  ) +
  theme_pub() 


#######################################################################
##  SAVE / VIEW  ─────────────────────────────────────────────────────
#######################################################################
#print(final2)  # View in RStudio

## combining both plots into a panel
# combined<- final + final2 + 
#   plot_layout(ncol = 2)
#   # theme(legend.position = "bottom")
# print(combined)

# TO EXPORT IN PDF FORMAT
# Uncomment to save publication-quality PDF
# dir.create("output", showWarnings = FALSE) ###>>> create output folder if it doesn't exist
# cairo_pdf("figures/test.pdf", width = 5, height = 5)
# print(final)
# dev.off()

# TO EXPORT IN TIFF/PNG FORMAT
# tiff("figures/test.jpg", units="in", width=5, height=5, res=600)
# print(final)
# dev.off()

# To export using ggsave function
#ggsave("figures/test3.jpg", plot = final, width = 6, height = 6, units = "in", dpi = 600)

