# =========================================================
# Springer-style figure: Scatter plot (Mean SUHI vs P90 SUHI)
# Output: 600 dpi PNG
# =========================================================

library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)

# ---- Path settings ----
infile <- "/Users/ibrahimkhalil/WorkSpace/Projects & Work/UHI-Study-Savar-Bangladesh/Results/RQ3_Outputs/Table_RQ3_SUHI_TimeSeries_Tidy.csv"
outdir <- "/Users/ibrahimkhalil/WorkSpace/Projects & Work/UHI-Study-Savar-Bangladesh/Results/RQ3_Outputs"

# ---- Read data ----
df <- read_csv(infile, show_col_types = FALSE)

# ---- Fit linear model for annotation ----
fit <- lm(SUHI_p75_P90_C ~ Mean_SUHI_p75_C, data = df)
fit_sum <- summary(fit)

slope <- coef(fit)[2]
intercept <- coef(fit)[1]
r2 <- fit_sum$r.squared
pval <- fit_sum$coefficients[2, 4]

eq_text <- sprintf("P90 = %.2f + %.2f × Mean\nR² = %.2f, p = %.3f",
                   intercept, slope, r2, pval)

springer_theme <- theme_classic(base_size = 11, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5   # center title
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5   # center subtitle
    ),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10),
    axis.line  = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4)
  )

# ---- Plot ----
p_scatter <- ggplot(df, aes(x = Mean_SUHI_p75_C, y = SUHI_p75_P90_C)) +
  geom_point(size = 2.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "black") +
  geom_text_repel(aes(label = year_label), size = 3.2, max.overlaps = Inf) +
  
  annotate(
    "text",
    x = Inf, y = Inf,
    label = eq_text,
    hjust = 1.05, vjust = 1.3,
    size = 3.3
  ) +
  
  labs(
    title = "Scaling Between Mean and Extreme SUHI (P90) Intensity Across Time",
    subtitle = "Regression-based assessment of linear versus nonlinear SUHI amplification",
    x = expression("Mean SUHI (p75) ("*degree*C*")"),
    y = expression("SUHI P90 (p75) ("*degree*C*")")
  ) +
  
  coord_cartesian(clip = "off") +
  springer_theme



# ---- Export (600 dpi) ----
outfile <- file.path(outdir, "Figure_RQ3_RQ4_Scatter_MeanSUHI_vs_P90SUHI_600dpi.png")
ggsave(outfile, p_scatter, width = 180, height = 120, units = "mm", dpi = 600)

