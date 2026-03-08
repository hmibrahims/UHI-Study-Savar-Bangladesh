# =========================================================
# Springer-style figure: Extreme Amplification Index (EAI)
# EAI = SUHI_p75_P90_C - Mean_SUHI_p75_C
# Output: 600 dpi PNG
# =========================================================

# Packages
library(readr)
library(dplyr)
library(ggplot2)

# ---- Path settings ----
infile <- "/Users/ibrahimkhalil/WorkSpace/Projects & Work/UHI-Study-Savar-Bangladesh/Results/RQ3_Outputs/Table_RQ3_SUHI_TimeSeries_Tidy.csv"
outdir <- "/Users/ibrahimkhalil/WorkSpace/Projects & Work/UHI-Study-Savar-Bangladesh/Results/RQ3_Outputs"

# ---- Read data ----
df <- read_csv(infile, show_col_types = FALSE)

# ---- Clean + compute EAI ----
# Ensure correct order (chronological) using year_block order in your file
df <- df %>%
  mutate(
    year_label = factor(year_label, levels = year_label),
    EAI_P90_minus_Mean = SUHI_p75_P90_C - Mean_SUHI_p75_C
  )

# ---- Springer-style theme ----
springer_theme <- theme_classic(base_size = 11) +
  theme(
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4)
  )

# ---- Plot (line + points; clean & journal-friendly) ----
p_eai <- ggplot(df, aes(x = year_label, y = EAI_P90_minus_Mean, group = 1)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.4) +
  labs(
    title = "Extreme Amplification of SUHI Hotspots (P90 − Mean)",
    subtitle = "EAI indicates how strongly extremes depart from the mean SUHI in each time block",
    x = "Time block",
    y = expression("Extreme Amplification Index, EAI ("*degree*C*") = P90 − Mean")
  ) +
  springer_theme

# ---- Export (600 dpi) ----
outfile <- file.path(outdir, "Figure_RQ3_RQ4_EAI_P90_minus_Mean_600dpi.png")
ggsave(outfile, p_eai, width = 180, height = 110, units = "mm", dpi = 600)

